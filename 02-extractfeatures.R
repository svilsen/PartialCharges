if (extract_features) {
    ##
    voltage_feature_functions <- list(
        Average = ~ r_mean(.x, p = 0.025, na.rm = TRUE), 
        SD = ~ r_sd(.x, p = 0.025, na.rm = TRUE),
        Skewness = ~ r_skewness(.x, p = 0.025, na.rm = TRUE), 
        Kurtosis = ~ r_kurtosis(.x, p = 0.025, na.rm = TRUE),
        MAD = ~ r_mad(.x, p = 0.025),
        MaxDelta = ~ max(abs(diff(.x)), na.rm = TRUE),
        FE = ~ fuzzy_entropy(.x[!is.na(.x)])
    )
    
    current_feature_functions <- voltage_feature_functions[-length(voltage_feature_functions)]
    
    ##
    charging_lower_limit <- c(3600, 2500)
    
    min_start_voltage <- c(3.3, 3.40)
    max_end_voltage <- c(3.4, 3.50)
    
    ## 
    profile_dirs <- list.dirs(data_dir, recursive = FALSE)
    for (i in seq_along(profile_dirs)) {
        #
        profile_name_i <- str_remove(profile_dirs[i], "Data/")
        capacity_i <- readRDS(paste0(files_dir, "/", tolower(profile_name_i), "_capacity", ".Rds"))
        
        #
        cells_i <- list.dirs(profile_dirs[i], recursive = FALSE) 
        for (j in seq_along(cells_i)) {
            #
            cells_ij <- list.dirs(cells_i[j], recursive = FALSE)
            cells_ij <- cells_ij[-str_detect(cells_ij, "00")]
            
            #
            capacity_ij <- capacity_i |> filter(Cell == as.numeric(str_split(cells_i[j], "Cell")[[1]][2]))
            capacity_ij_0 <- capacity_ij |> filter(Round == 0)
            
            ## Determine expected data size 
            size_ij <- suppressWarnings(sapply(seq_along(cells_ij), function(k) {
                if (file.exists(paste0(cells_ij[k], "/Ageing.csv"))) {
                    cells_ijk <- read_csv(paste0(cells_ij[k], "/Ageing.csv"), show_col_types = FALSE, progress = FALSE)
                    return(dim(cells_ijk)[1])
                }
                else {
                    return(0)
                }
            }))
            
            size_ij <- tibble(
                Round = as.numeric(sapply(str_split(cells_ij, "Round"), function(xx) xx[2])), 
                Size = size_ij, 
                `E(Size)` = median(size_ij, na.rm = TRUE), 
                Ignore = Size / `E(Size)` < 0.85
            ) 
            
            ## Determining FEC
            fec_ij <- suppressWarnings(sapply(seq_along(cells_ij), function(k) {
                if (file.exists(paste0(cells_ij[k], "/Ageing.csv"))) {
                    cells_ijk <- read_csv(paste0(cells_ij[k], "/Ageing.csv"), show_col_types = FALSE, progress = FALSE)
                    if (profile_name_i == "Forklifts") {
                        part_time_ijk <- cells_ijk |> filter(Part == 1) |> select(Time) |> max()
                        cells_ijk <- cells_ijk |> mutate(Time = Time + part_time_ijk * (Part - 1))
                    }
                    
                    fec_ijk <- sum(diff(c(0, cells_ijk$Time)) * abs(cells_ijk$Current) / 3600) / (2 * capacity_ij_0$Capacity)
                    
                    if (profile_name_i == "Forklifts") {
                        if (length(unique(cells_ijk$Part)) > 1) {
                            return(fec_ijk)
                        }
                        else {
                            return(NA)
                        }
                    }
                    else {
                        return(fec_ijk)
                    }
                }
                else {
                    return(NA)
                }
            }))
            
            fec_ij <- tibble(Round = as.numeric(sapply(str_split(cells_ij, "Round"), function(xx) xx[2])), FEC = fec_ij) |> 
                left_join(size_ij |> select(Round, Ignore), by = "Round") |> 
                mutate(
                    FEC = ifelse(Ignore, NA, FEC),
                    FEC = zoo::na.approx(FEC),
                    FEC = cumsum(FEC)
                ) |> 
                select(-Ignore)
            
            ## Extracting features
            for (k in seq_along(cells_ij)) {
                cat("Profile:", i, "/", length(profile_dirs), ":: Cell:", j, "/", length(cells_i), ":: Round:", k, "/", length(cells_ij), "\n")
                
                ## 
                filename_ijk <- paste0(cells_ij[k], "/Ageing.csv")
                if (!file.exists(filename_ijk)) {
                    next
                }
                
                ##
                cell_ijk <- str_split(cells_i[j], "Cell")[[1]][2]
                round_ijk <- str_split(cells_ij[k], paste0("Cell", cell_ijk, "/"))[[1]][2]
                feat_name_ijk <- paste0(files_dir, "/", profile_name_i, "/Cell", cell_ijk, "/", round_ijk, ".Rds")
                
                if (file.exists(feat_name_ijk)) {
                    next
                }
                
                ## Checks if there is "enough" information in the file
                size_ijk <- size_ij |> filter(Round == as.numeric(str_remove(round_ijk, "Round")))
                if (size_ijk$Ignore) {
                    next
                }
                
                ##
                file_ijk <- read_csv(filename_ijk, show_col_types = FALSE, progress = FALSE)
                if (profile_name_i == "Forklifts") {
                    part_time_ijk <- file_ijk |> filter(Part == 1) |> select(Time) |> max()
                    file_ijk <- file_ijk |> mutate(Time = Time + part_time_ijk * (Part - 1))
                }
                
                ## 
                capacity_ijk <- capacity_ij |> filter(Round >= unique(file_ijk$Round) - 2, Round <= unique(file_ijk$Round))
                fec_ijk <- fec_ij |> filter(Round == as.numeric(str_remove(round_ijk, "Round")))
                
                ## 
                feat_ijk <- feature_extraction(
                    ageing_file = file_ijk, 
                    capacity_file = capacity_ijk, 
                    capacity_0 = capacity_ij_0$Capacity,
                    fec = fec_ijk$FEC - fec_ij$FEC[1],
                    fec_0 = fec_ij$FEC[1], 
                    voltage_feature_functions = voltage_feature_functions, 
                    current_feature_functions = current_feature_functions, 
                    charging_lower_limit = charging_lower_limit[i], 
                    min_start_voltage = min_start_voltage[i], 
                    max_end_voltage = max_end_voltage[i], 
                    epsilon = 20, 
                    nr_cores = 4,
                    trace = 25
                )
                
                feat_ijk <- feat_ijk |> mutate(Profile = profile_name_i, FEC_C = fec_ijk$FEC) |> select(Profile, Cell:SOHW, FEC_C, SOH:D0)
                
                ## Used to create temporary storage location
                saveRDS(feat_ijk, file = feat_name_ijk)
            }
        }
    }
    
    ## Collect information
    cells <- list.files(files_dir, pattern = "Round", recursive = TRUE, full.names = TRUE)
    partial_charging_intervals <- vector("list", length(cells))
    for(i in seq_along(cells)) {
        partial_charging_intervals[[i]] <- readRDS(cells[i])
    }
    
    partial_charging_intervals <- partial_charging_intervals |> 
        bind_rows()
    
    ## Re-grouping/re-make charge cycle index
    partial_charging_intervals <- partial_charging_intervals |> 
        mutate(PC = paste(Profile, Cell, sep = "_")) |> 
        (\(x) split(x, x$PC))()
    
    partial_charging_intervals <- lapply(partial_charging_intervals, function(xx) {
        xx_grp <- xx |> 
            distinct(Cell, Round, StartTime) |> 
            arrange(StartTime) 
        
        j <- 1
        change_limit <- ifelse(unique(xx$Profile) == "Forklifts", charging_lower_limit[1], charging_lower_limit[2])
        current_value <- xx_grp$StartTime[1]
        grouping <- rep(NA, length(xx_grp$StartTime))
        for (i in seq_along(grouping)) {
            if ((xx_grp$StartTime[i] - current_value) > change_limit) {
                current_value <- xx_grp$StartTime[i]
                j <- j + 1
            }
            
            grouping[i] <- j 
        }
        
        xx_grp <- xx_grp |> mutate(Group = grouping)
        
        xx <- xx |> left_join(xx_grp, by = c("Cell", "Round", "StartTime"))
        return(xx)
    }) |> bind_rows()
    
    saveRDS(partial_charging_intervals, file = paste0(files_dir, "/partial_charging_intervals.Rds"))
}
