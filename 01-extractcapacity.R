# Extracting the capacities of all profiles, cells, and rounds placed in the "Data" folder.
if (extract_capacity) {
    ## The lower limits of (dis)charge time for each of the two profiles.
    profile_search_limits <- c(10000, 3000)
    
    ## Identifying, and looping over, each profile directory.
    profile_dirs <- list.dirs(data_dir, recursive = FALSE)
    for (i in seq_along(profile_dirs)) {
        ## Identifying and looping over each cell of profile 'i'.
        profile_name_i <- str_remove(profile_dirs[i], "Data/")
        cells_i <- list.dirs(profile_dirs[i], recursive = FALSE) 
        
        capacity_i <- vector("list", length(cells_i))
        for (j in seq_along(capacity_i)) {
            ## Identifying and looping over each round of cell 'j' of profile 'i'.
            cells_ij <- list.dirs(cells_i[j], recursive = FALSE)
            
            capacity_ij <- vector("list", length(cells_ij))
            for (k in seq_along(cells_ij)) {
                ## Checking if 'RPT' file exists for the round 'k', cell 'j', and profile 'i'.
                filename_ijk <- paste0(cells_ij[k], "/RPT.csv")
                if (!file.exists(filename_ijk)) {
                    next
                }
                
                ## Loading and extracting the capacity from the RPT for the round 'k', cell 'j', and profile 'i'.
                file_ijk <- read_csv(filename_ijk, show_col_types = FALSE)
                cap_ijk <- capacity_extract(file_ijk, L = profile_search_limits[i])
                
                ## Storing the extracted capacity measurements of round 'k'.
                capacity_ij[[k]] <- cap_ijk |> mutate(Profile = profile_name_i) |> select(Profile, Cell:Capacity)
            }
            
            ## Collecting the extracted capacity measurements of cell 'j'.
            capacity_i[[j]] <- capacity_ij |> bind_rows()
        }
        
        ## Collecting, and saving, the extracted capacity measurements of profile 'i'.
        capacity_i <- capacity_i |> bind_rows()
        saveRDS(capacity_i, file = paste0(files_dir, "/", tolower(profile_name_i), "_capacity", ".Rds"))
    }
}

# Extract complete charge curves for cell 1 of the forklift profile (needed for a figure).
if (extract_charge) {
    ## Identifying, and looping over, each round.
    rounds <- list.dirs(paste(data_dir, "Forklifts", "Cell1", sep = "/"), recursive = FALSE) 
    charge <- vector("list", length(rounds))
    for (k in seq_along(rounds)) {
        ## Checking if RPT exists for the round 'k'.
        filename_k <- paste0(rounds[k], "/RPT.csv")
        if (!file.exists(filename_k)) {
            next
        }
        
        ## Loading and extracting the charge curves from the RPT of round 'k'.
        file_k <- read_csv(filename_k, show_col_types = FALSE)
        charge_k <- charge_curve_extract(file_k, L = 10000)
        
        ## Storing the extracted charge curves of round 'k'.
        charge[[k]] <- charge_k |> 
            group_by(Cell, Round) |> 
            mutate(Time = Time - Time[1]) |> 
            ungroup()
    }
    
    ## Collecting, and saving, the extracted charge curves.
    charge <- charge |> bind_rows() 
    saveRDS(charge, file = "Files/forklifts_charge.Rds")
}