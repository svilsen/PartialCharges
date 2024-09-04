if (extract_capacity) {
    profile_search_limits <- c(10000, 3000)
    profile_dirs <- list.dirs(data_dir, recursive = FALSE)
    for (i in seq_along(profile_dirs)) {
        profile_name_i <- str_remove(profile_dirs[i], "Data/")
        cells_i <- list.dirs(profile_dirs[i], recursive = FALSE) 
        
        capacity_i <- vector("list", length(cells_i))
        for (j in seq_along(capacity_i)) {
            cells_ij <- list.dirs(cells_i[j], recursive = FALSE)
            
            capacity_ij <- vector("list", length(cells_ij))
            for (k in seq_along(cells_ij)) {
                filename_ijk <- paste0(cells_ij[k], "/RPT.csv")
                if (!file.exists(filename_ijk)) {
                    next
                }
                
                file_ijk <- read_csv(filename_ijk, show_col_types = FALSE)
                cap_ijk <- capacity_extract(file_ijk, L = profile_search_limits[i])
                
                capacity_ij[[k]] <- cap_ijk |> mutate(Profile = profile_name_i) |> select(Profile, Cell:Capacity)
            }
            
            capacity_i[[j]] <- capacity_ij |> bind_rows()
        }
        
        capacity_i <- capacity_i |> bind_rows()
        saveRDS(capacity_i, file = paste0(files_dir, "/", tolower(profile_name_i), "_capacity", ".Rds"))
    }

    ## Extract complete charge curves
    rounds <- list.dirs(paste(data_dir, "Forklifts", "Cell1", sep = "/"), recursive = FALSE) 
    charge <- vector("list", length(rounds))
    for (k in seq_along(rounds)) {
        filename_k <- paste0(rounds[k], "/RPT.csv")
        if (!file.exists(filename_k)) {
            next
        }
        
        file_k <- read_csv(filename_k, show_col_types = FALSE)
        charge_k <- charge_curve_extract(file_k, L = 10000)
        
        charge[[k]] <- charge_k |> 
            group_by(Cell, Round) |> 
            mutate(Time = Time - Time[1]) |> 
            ungroup()
    }
    
    charge <- charge |> 
        bind_rows() 
    
    saveRDS(charge, file = "Files/forklifts_charge.Rds")
}
