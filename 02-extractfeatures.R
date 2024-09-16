# Extracting the features for all profiles, cells, and rounds placed in the "Data" folder.
if (extract_features) {
    ## The descriptive features extracted from the voltage
    voltage_feature_functions <- list(
        Average = ~ r_mean(.x, p = 0.025, na.rm = TRUE), 
        SD = ~ r_sd(.x, p = 0.025, na.rm = TRUE),
        Skewness = ~ r_skewness(.x, p = 0.025, na.rm = TRUE), 
        Kurtosis = ~ r_kurtosis(.x, p = 0.025, na.rm = TRUE),
        MAD = ~ r_mad(.x, p = 0.025),
        MaxDelta = ~ max(abs(diff(.x)), na.rm = TRUE),
        FE = ~ fuzzy_entropy(.x[!is.na(.x)])
    )
    
    ## The descriptive features extracted from the current
    current_feature_functions <- voltage_feature_functions[-length(voltage_feature_functions)]
    
    ## A lower limit on the charges features are extracted from for each profile.
    charging_lower_limit <- c(3600, 2500)
    
    ## A lower and upper limit voltage interval for each profile.
    min_start_voltage <- c(3.3, 3.40)
    max_end_voltage <- c(3.4, 3.50)
    
    ## Identifying, and looping over, each profile directory.
    profile_dirs <- list.dirs(data_dir, recursive = FALSE)
    for (i in seq_along(profile_dirs)) {
        ## Identifying, loading extracted capacity, and looping over each cell of profile 'i'.
        profile_name_i <- str_remove(profile_dirs[i], "Data/")
        capacity_i <- readRDS(paste0(files_dir, "/", tolower(profile_name_i), "_capacity", ".Rds"))
        
        cells_i <- list.dirs(profile_dirs[i], recursive = FALSE) 
        for (j in seq_along(cells_i)) {
            ## Identifying each round of cell 'j' of profile 'i'.
            cells_ij <- list.dirs(cells_i[j], recursive = FALSE)
            cells_ij <- cells_ij[-str_detect(cells_ij, "00")]
            
            ## Identifying correct capacity for cell 'j' of profile 'i'.
            capacity_ij <- capacity_i |> filter(Cell == as.numeric(str_split(cells_i[j], "Cell")[[1]][2]))
            capacity_ij_0 <- capacity_ij |> filter(Round == 0)
            
            ## Determine expected data size of a 'complete' round.
            size_ij <- suppressWarnings(sapply(seq_along(cells_ij), function(k) {
                if (file.exists(paste0(cells_ij[k], "/Ageing.csv"))) {
                    cells_ijk <- read_csv(paste0(cells_ij[k], "/Ageing.csv"), show_col_types = FALSE, progress = FALSE)
                    return(dim(cells_ijk)[1])
                }
                else {
                    return(0)
                }
            }))
            
            ## Limiting the rounds with a large number of missing measurements.
            size_ij <- tibble(
                Round = as.numeric(sapply(str_split(cells_ij, "Round"), function(xx) xx[2])), 
                Size = size_ij, 
                `E(Size)` = median(size_ij, na.rm = TRUE), 
                Ignore = Size / `E(Size)` < 0.85
            ) 
            
            ## Determine FEC of all rounds. 
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
            
            ## Linearly interpolating FEC of missing rounds.
            fec_ij <- tibble(Round = as.numeric(sapply(str_split(cells_ij, "Round"), function(xx) xx[2])), FEC = fec_ij) |> 
                left_join(size_ij |> select(Round, Ignore), by = "Round") |> 
                mutate(
                    FEC = ifelse(Ignore, NA, FEC),
                    FEC = zoo::na.approx(FEC),
                    FEC = cumsum(FEC)
                ) |> 
                select(-Ignore)
            
            ## Extracting features when looping over each round of cell 'j' and profile 'i'.
            for (k in seq_along(cells_ij)) {
                ## Trace showing the progress through all files.
                cat("Profile:", i, "/", length(profile_dirs), ":: Cell:", j, "/", length(cells_i), ":: Round:", k, "/", length(cells_ij), "\n")
                
                ## Checking if 'Ageing' file exists for the round 'k', cell 'j', and profile 'i'.
                filename_ijk <- paste0(cells_ij[k], "/Ageing.csv")
                if (!file.exists(filename_ijk)) {
                    next
                }
                
                ## Checking if features have already been extracted from round 'k', cell 'j', and profile 'i'.
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
                
                ## Loading the 'Ageing' file of round 'k', cell 'j', and profile 'i'.
                file_ijk <- read_csv(filename_ijk, show_col_types = FALSE, progress = FALSE)
                
                ## Because the forklift profile was applied in two parts, the time index is reset in between, which needs to be corrected.
                if (profile_name_i == "Forklifts") {
                    part_time_ijk <- file_ijk |> filter(Part == 1) |> select(Time) |> max()
                    file_ijk <- file_ijk |> mutate(Time = Time + part_time_ijk * (Part - 1))
                }
                
                ## The capacity and FEC of round 'k', cell 'j', and profile 'i'.
                capacity_ijk <- capacity_ij |> filter(Round >= unique(file_ijk$Round) - 2, Round <= unique(file_ijk$Round))
                fec_ijk <- fec_ij |> filter(Round == as.numeric(str_remove(round_ijk, "Round")))
                
                ## Extracts features of round 'k', cell 'j', and profile 'i'.
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
                
                ## Saving the features extracted from round 'k', cell 'j', and profile 'i'.
                saveRDS(feat_ijk, file = feat_name_ijk)
            }
        }
    }
    
    ## Collecting information
    cells <- list.files(files_dir, pattern = "Round", recursive = TRUE, full.names = TRUE)
    partial_charging_intervals <- vector("list", length(cells))
    for(i in seq_along(cells)) {
        partial_charging_intervals[[i]] <- readRDS(cells[i])
    }
    
    partial_charging_intervals <- partial_charging_intervals |> bind_rows()
    
    ## Re-grouping/re-making charge cycle index, ensuring that it is consistent.
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
    
    ## Saving the complete data-set, includes features, FEC, and SOH.
    saveRDS(partial_charging_intervals, file = paste0(files_dir, "/partial_charging_intervals.Rds"))
}
