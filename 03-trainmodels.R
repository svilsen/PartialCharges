#### Features ----
window_capacity_features <- c(
    "StartVoltage",
    "DeltaVoltage",
    "Voltage_Average",
    "Voltage_SD",
    "Voltage_Skewness",
    "Voltage_Kurtosis",
    "Voltage_MAD",
    "Voltage_MaxDelta", 
    "Voltage_FE", 
    "Current_Average",
    "Current_SD",
    "Current_Skewness",
    "Current_Kurtosis",
    "Current_MAD",
    "FEC",
    "SOHW", 
    "Ri",
    "Temperature"
)

window_prior_capacity_features <- c(
    "C_AH", 
    "C_Delta_Time",
    "C_Start_Voltage", 
    "C_Delta_Voltage",
    "C_Voltage_Average",
    "C_Voltage_SD",
    "C_Voltage_Skewness",
    "C_Voltage_Kurtosis",
    "C_Voltage_MAD",
    "C_Voltage_MaxDelta",
    "C_Current_Average",
    "C_Current_SD",
    "C_Current_Skewness",
    "C_Current_Kurtosis",
    "C_Current_MAD"
)

window_prior_features <- c(
    "DC_AH", 
    "DC_Voltage_Average",
    "DC_Voltage_SD",
    "DC_Voltage_Skewness",
    "DC_Voltage_Kurtosis",
    "DC_Voltage_MAD",
    "DC_Voltage_MaxDelta", 
    "DC_Current_Average",
    "DC_Current_SD",
    "DC_Current_Skewness",
    "DC_Current_Kurtosis",
    "DC_Current_MAD",
    "DC_Current_MaxDelta", 
    "Relaxation_Time"
)

#### Create training-set ----
if (create_trainingset) {
    # Loading and formatting data 
    partial_charging <- readRDS("Files/partial_charging_intervals.Rds") |> 
        mutate(
            Cell = factor(Cell),
            Relaxation_Distance = ifelse(is.infinite(Relaxation_Distance), 0, Relaxation_Distance),
            Ri = ifelse(is.infinite(Ri), 0, Ri),
            Data = NA
        ) |> 
        group_by(Profile, Cell, Round) |> 
        mutate(
            RoundPLUS = Round + (StartTime + DeltaTime) / (60 * 60 * 24 * 7 + 180000),
            RoundDELTA = RoundPLUS - Round,
            RT = round(RoundDELTA * 10) / 10, 
            Temperature_C = median(Temperature)
        ) |>
        ungroup() |> 
        select(
            Profile, 
            Data, 
            Cell, 
            Round, 
            FEC_C, 
            Temperature_C,
            Group, 
            RoundPLUS, 
            RoundDELTA,
            FEC,
            StartTime,
            DeltaTime, 
            StartVoltage, 
            DeltaVoltage, 
            starts_with("Voltage"), 
            starts_with("Current"), 
            SOHW,
            Ri,
            starts_with("C_"), 
            starts_with("DC_"), 
            Relaxation_Distance,
            Relaxation_Time,
            Temperature, 
            minVoltage, 
            maxVoltage, 
            SOH0,
            SOH,
            D0, 
            D
        ) |>
        filter(SOHW < 200, SOHW > 1) |> 
        arrange(Profile, Cell, Round) 
    
    ## Creating training / validation split
    training_fraction <- 0.7
    profile_cell <- unique(paste(partial_charging$Profile, partial_charging$Cell, sep = "_"))
    for (i in seq_along(profile_cell)) {
        #
        profile_cell_i <- str_split(profile_cell[i], "_")[[1]]
        
        #
        partial_charging_x <- partial_charging |> filter(Profile == profile_cell_i[1], Cell == profile_cell_i[2]) # 
        partial_charging_unique_x <- unique(partial_charging_x$Round)
        partial_charging_tr_x <- sort(sample(partial_charging_unique_x, floor(training_fraction * length(partial_charging_unique_x))))
        
        #
        data_x <- rep("Validation", dim(partial_charging_x)[1])
        data_x[which(partial_charging_x$Round %in% partial_charging_tr_x)] <- "Training"
        
        partial_charging$Data[which((partial_charging$Profile == profile_cell_i[1]) & (partial_charging$Cell == profile_cell_i[2]))] <- data_x
    }
    
    ## Fixing missing features -- happens when the voltage windows starts immidiately after discharge.
    partial_charging[!complete.cases(partial_charging),] <- partial_charging[!complete.cases(partial_charging),] |> 
        mutate(
            C_AH = 0, 
            C_Delta_Time = 0, 
            C_Start_Voltage = StartVoltage,
            C_Delta_Voltage = 0,
            C_Voltage_Average = StartVoltage, 
            C_Voltage_SD = 0, 
            C_Voltage_Skewness = 0, 
            C_Voltage_Kurtosis = 0, 
            C_Voltage_MAD = 0, 
            C_Voltage_MaxDelta = 0, 
            C_Current_Average = 0, 
            C_Current_SD = 0, 
            C_Current_Skewness = 0, 
            C_Current_Kurtosis = 0, 
            C_Current_MAD = 0,    
            C_Temperature = Temperature
        )
    
    # Standardising features based on training-set
    partial_charging_std <- partial_charging |> 
        mutate_at(
            vars(all_of(window_capacity_features)),
            ~standardise_features_reference(., ref = Data == "Training")
        ) |> 
        mutate_at(
            vars(all_of(window_prior_capacity_features)),
            ~standardise_features_reference(., ref = Data == "Training")
        ) |>
        mutate_at(
            vars(all_of(window_prior_features)),
            ~standardise_features_reference(., ref = Data == "Training")
        ) |>
        filter(!is.na(SOHW)) |> 
        filter(if_all(starts_with("Voltage_"), ~ abs(.x) < 20)) |> 
        filter(!is.na(DC_Current_Average)) |>
        arrange(Profile, Cell, Round, Group) |> 
        filter(!is.na(SOH)) |> 
        mutate(
            DeltaTime = (DeltaTime - min(DeltaTime)) / (max(DeltaTime) - min(DeltaTime))
        ) |> 
        select(Data, Profile, Cell, Round, RoundPLUS, FEC_C, Temperature_C, Group, all_of(window_capacity_features), all_of(window_prior_capacity_features), all_of(window_prior_features), SOH0, D0)
    
    saveRDS(partial_charging_std, file = paste0(files_dir, "/partial_charging_intervals_training_random.Rds"))
} 
if (file.exists(paste0(files_dir, "/partial_charging_intervals_training_random.Rds"))) {
    partial_charging_std <- readRDS(paste0(files_dir, "/partial_charging_intervals_training_random.Rds"))
}

#### MLR ----
if (train_mlr_models) {
    #### MLR ----
    cat("MLR:\n")
    data_train <- partial_charging_std |> filter(Data == "Training")
    
    ## Without any prior knowledge
    cat("\tWithout any prior knowledge.\n")
    m_simple <- stepAIC(
        lm(D0 ~ ., data = data_train |> select(D0, all_of(window_capacity_features))), 
        k = 5, 
        direction = "both", 
        scope = list(lower = D0 ~ FEC + StartVoltage + DeltaVoltage + Temperature, upper = D0 ~ .*.), 
        trace = FALSE
    )
    
    p_data_simple <- partial_charging_std |>
        mutate(
            DHAT = predict(m_simple, newdata = partial_charging_std)
        ) |>
        group_by(Profile, Data, Cell, Round) |>
        mutate(
            RoundDIFF = RoundPLUS - RoundPLUS[1]
        ) |>
        summarise(
            FEC = max(FEC_C),
            Temperature = round(median(Temperature_C)),
            `Average weights` =  mean(DHAT),
            `Exponential weights` = sum(exp(RoundDIFF - max(RoundDIFF)) * DHAT / sum(exp(RoundDIFF - max(RoundDIFF)))),
            `Linear weights` = sum(RoundDIFF * DHAT / sum(RoundDIFF)),
            D = mean(D0), 
            .groups = "drop"
        ) |>
        pivot_longer(cols = c("Average weights", "Exponential weights", "Linear weights"), names_to = "TYPE", values_to = "DHAT") |>
        mutate(
            Prior = "Without prior information"
        )
    
    saveRDS(m_simple, file = paste0(files_dir, "/Models/MLR/without_prior_knowledge.Rds"))
    
    ## Without prior charge knowledge
    cat("\tWithout prior charge knowledge.\n")
    m_no_charge <- stepAIC(
        lm(D0 ~ ., data = data_train |> select(D0, all_of(window_capacity_features), all_of(window_prior_features))), 
        k = 5, 
        direction = "both", 
        scope = list(lower = D0 ~ FEC + StartVoltage + DeltaVoltage + Temperature, upper = D0 ~ .*.), 
        trace = FALSE
    )
    
    p_data_no_charge <- partial_charging_std |>
        mutate(
            DHAT = predict(m_no_charge, newdata = partial_charging_std)
        ) |>
        group_by(Profile, Data, Cell, Round) |>
        mutate(
            RoundDIFF = RoundPLUS - RoundPLUS[1]
        ) |>
        summarise(
            FEC = max(FEC_C),
            Temperature = round(median(Temperature_C)),
            `Average weights` =  mean(DHAT),
            `Exponential weights` = sum(exp(RoundDIFF - max(RoundDIFF)) * DHAT / sum(exp(RoundDIFF - max(RoundDIFF)))),
            `Linear weights` = sum(RoundDIFF * DHAT / sum(RoundDIFF)),
            D = mean(D0), 
            .groups = "drop"
        ) |>
        pivot_longer(cols = c("Average weights", "Exponential weights", "Linear weights"), names_to = "TYPE", values_to = "DHAT") |>
        mutate(
            Prior = "Without prior charge information"
        ) 
    
    saveRDS(m_no_charge, file = paste0(files_dir, "/Models/MLR/without_prior_charge_knowledge.Rds"))
    
    ## Without prior discharge knowledge
    cat("\tWithout prior discharge knowledge.\n")
    m_no_dcharge <- stepAIC(
        lm(D0 ~ ., data = data_train |> select(D0, all_of(window_capacity_features), all_of(window_prior_capacity_features))),
        k = 5, 
        direction = "both", 
        scope = list(lower = D0 ~ FEC + StartVoltage + DeltaVoltage + Temperature, upper = D0 ~ .*.), 
        trace = FALSE
    )
    
    p_data_no_dcharge <- partial_charging_std |>
        mutate(
            DHAT = predict(m_no_dcharge, newdata = partial_charging_std)
        ) |>
        group_by(Profile, Data, Cell, Round) |>
        mutate(
            RoundDIFF = RoundPLUS - RoundPLUS[1]
        ) |>
        summarise(
            FEC = max(FEC_C),
            Temperature = round(median(Temperature_C)),
            `Average weights` =  mean(DHAT),
            `Exponential weights` = sum(exp(RoundDIFF - max(RoundDIFF)) * DHAT / sum(exp(RoundDIFF - max(RoundDIFF)))),
            `Linear weights` = sum(RoundDIFF * DHAT / sum(RoundDIFF)),
            D = mean(D0), 
            .groups = "drop"
        ) |>
        pivot_longer(cols = c("Average weights", "Exponential weights", "Linear weights"), names_to = "TYPE", values_to = "DHAT") |>
        mutate(
            Prior = "Without prior discharge information"
        )
    
    saveRDS(m_no_dcharge, file = paste0(files_dir, "/Models/MLR/without_prior_discharge_knowledge.Rds"))
    
    ## With all prior knowledge
    cat("\tWith all prior knowledge.\n")
    m_total <- stepAIC(
        lm(D0 ~ ., data = data_train |> select(D0, all_of(window_capacity_features), all_of(window_prior_capacity_features), all_of(window_prior_features))),
        k = 5, 
        direction = "both", 
        scope = list(lower = D0 ~ FEC + StartVoltage + DeltaVoltage + Temperature, upper = D0 ~ .*.), 
        trace = FALSE
    )
    
    p_data_total <- partial_charging_std |>
        mutate(
            DHAT = predict(m_total, newdata = partial_charging_std)
        ) |>
        group_by(Profile, Data, Cell, Round) |>
        mutate(
            RoundDIFF = RoundPLUS - RoundPLUS[1]
        ) |>
        summarise(
            FEC = max(FEC_C),
            Temperature = round(median(Temperature_C)),
            `Average weights` =  mean(DHAT),
            `Exponential weights` = sum(exp(RoundDIFF - max(RoundDIFF)) * DHAT / sum(exp(RoundDIFF - max(RoundDIFF)))),
            `Linear weights` = sum(RoundDIFF * DHAT / sum(RoundDIFF)),
            D = mean(D0), 
            .groups = "drop"
        ) |>
        pivot_longer(cols = c("Average weights", "Exponential weights", "Linear weights"), names_to = "TYPE", values_to = "DHAT") |>
        mutate(
            Prior = "With prior information"
        )
    
    saveRDS(m_total, file = paste0(files_dir, "/Models/MLR/with_all_prior_knowledge.Rds"))
    
    ##
    prior_levels <- c("Without prior information", "Without prior charge information", "Without prior discharge information", "With prior information")
    mlr_error_tibble <- p_data_simple |>
        bind_rows(p_data_no_charge) |>
        bind_rows(p_data_no_dcharge) |>
        bind_rows(p_data_total) |>
        mutate(
            Prior = factor(Prior, levels = prior_levels), 
            Model = "MLR"
        )
    
    saveRDS(mlr_error_tibble, file = paste0(files_dir, "/mlr_model_tibble.Rds"))
} 

#### SVR ----
if (train_svr_models) {
    #### SVR ----
    cat("SVR:\n")
    data_train <- partial_charging_std |> filter(Data == "Training")
    
    grid <-  expand.grid(
        C = c(0.1, 1, 10), 
        sigma = c(0.001, 0.01, 0.1)
    )
    
    epsilon <- c(0.001, 0.01, 0.1)
    
    fit_control <- caret::trainControl(
        method = "CV",
        number = 5,
        verboseIter = FALSE
    )
    
    ## Without any prior knowledge
    cat("\tWithout any prior knowledge.\n")
    svr_simple <- vector("list", length(epsilon))
    for (i in seq_along(epsilon)) {
        cat(i, "/", length(epsilon), " ")
        svr_simple_i <- caret::train(
            D0 ~ .*., 
            data = data_train |> select(D0, all_of(window_capacity_features)),
            method = 'svmRadial',
            type = "eps-svr",
            epsilon = epsilon[i],
            scaled = FALSE,
            tuneGrid = grid,
            trControl = fit_control
        )
        
        svr_simple[[i]] <- svr_simple_i
    }
    cat("\n")
    
    svr_simple <- svr_simple[[which.max(sapply(svr_simple, function(x) max(x$results$Rsquared)))]]
    m_simple <- svr_simple$finalModel
    
    p_data_simple <- partial_charging_std |>
        mutate(
            DHAT = predict(m_simple, newdata = partial_charging_std |> select(all_of(window_capacity_features)) |> (\(x) model.matrix(~ -1 + .*., data = x))())[, 1]
        ) |>
        group_by(Profile, Data, Cell, Round) |>
        mutate(
            RoundDIFF = RoundPLUS - RoundPLUS[1]
        ) |>
        summarise(
            FEC = max(FEC_C),
            Temperature = round(median(Temperature_C)),
            `Average weights` =  mean(DHAT),
            `Exponential weights` = sum(exp(RoundDIFF - max(RoundDIFF)) * DHAT / sum(exp(RoundDIFF - max(RoundDIFF)))),
            `Linear weights` = sum(RoundDIFF * DHAT / sum(RoundDIFF)),
            D = mean(D0), 
            .groups = "drop"
        ) |>
        pivot_longer(cols = c("Average weights", "Exponential weights", "Linear weights"), names_to = "TYPE", values_to = "DHAT") |>
        mutate(
            Prior = "Without prior information"
        )
    
    saveRDS(m_simple, file = paste0(files_dir, "/Models/SVR/without_prior_knowledge.Rds"))
    
    rm(m_simple, svr_simple_i, svr_simple)
    
    ## Without prior charge knowledge
    cat("\tWithout prior charge knowledge.\n")
    svr_no_charge <- vector("list", length(epsilon))
    for (i in seq_along(epsilon)) {
        cat(i, "/", length(epsilon), " ")
        svr_no_charge_i <- caret::train(
            D0 ~ .*., 
            data = data_train |> select(D0, all_of(window_capacity_features), all_of(window_prior_features)),
            method = 'svmRadial',
            type = "eps-svr",
            epsilon = epsilon[i],
            scaled = FALSE,
            tuneGrid = grid,
            trControl = fit_control
        )
        
        svr_no_charge[[i]] <- svr_no_charge_i
    }
    cat("\n")
    
    
    svr_no_charge <- svr_no_charge[[which.max(sapply(svr_no_charge, function(x) max(x$results$Rsquared)))]]
    m_no_charge <- svr_no_charge$finalModel
    
    p_data_no_charge <- partial_charging_std |>
        mutate(
            DHAT = predict(m_no_charge, newdata = partial_charging_std |> select(all_of(window_capacity_features), all_of(window_prior_features)) |> (\(x) model.matrix(~ -1 + .*., data = x))())[, 1]
        ) |>
        group_by(Profile, Data, Cell, Round) |>
        mutate(
            RoundDIFF = RoundPLUS - RoundPLUS[1]
        ) |>
        summarise(
            FEC = max(FEC_C),
            Temperature = round(median(Temperature_C)),
            `Average weights` =  mean(DHAT),
            `Exponential weights` = sum(exp(RoundDIFF - max(RoundDIFF)) * DHAT / sum(exp(RoundDIFF - max(RoundDIFF)))),
            `Linear weights` = sum(RoundDIFF * DHAT / sum(RoundDIFF)),
            D = mean(D0), 
            .groups = "drop"
        ) |>
        pivot_longer(cols = c("Average weights", "Exponential weights", "Linear weights"), names_to = "TYPE", values_to = "DHAT") |>
        mutate(
            Prior = "Without prior charge information"
        )
    
    saveRDS(m_no_charge, file = paste0(files_dir, "/Models/SVR/without_prior_charge_knowledge.Rds"))
    
    rm(m_no_charge, svr_no_charge_i, svr_no_charge)
    
    ## Without prior discharge knowledge
    cat("\tWithout prior discharge knowledge.\n")
    svr_no_dcharge <- vector("list", length(epsilon))
    for (i in seq_along(epsilon)) {
        cat(i, "/", length(epsilon), " ")
        svr_no_dcharge_i <- caret::train(
            D0 ~ .*., 
            data = data_train |> select(D0, all_of(window_capacity_features), all_of(window_prior_capacity_features)),
            method = 'svmRadial',
            type = "eps-svr",
            epsilon = epsilon[i],
            scaled = FALSE,
            tuneGrid = grid,
            trControl = fit_control
        )
        
        svr_no_dcharge[[i]] <- svr_no_dcharge_i
    }
    cat("\n")
    
    
    svr_no_dcharge <- svr_no_dcharge[[which.max(sapply(svr_no_dcharge, function(x) max(x$results$Rsquared)))]]
    m_no_dcharge <- svr_no_dcharge$finalModel
    
    p_data_no_dcharge <- partial_charging_std |>
        mutate(
            DHAT = predict(m_no_dcharge, newdata = partial_charging_std |> select(all_of(window_capacity_features), all_of(window_prior_capacity_features)) |> (\(x) model.matrix(~ -1 + .*., data = x))())[, 1]
        ) |>
        group_by(Profile, Data, Cell, Round) |>
        mutate(
            RoundDIFF = RoundPLUS - RoundPLUS[1]
        ) |>
        summarise(
            FEC = max(FEC_C),
            Temperature = round(median(Temperature_C)),
            `Average weights` =  mean(DHAT),
            `Exponential weights` = sum(exp(RoundDIFF - max(RoundDIFF)) * DHAT / sum(exp(RoundDIFF - max(RoundDIFF)))),
            `Linear weights` = sum(RoundDIFF * DHAT / sum(RoundDIFF)),
            D = mean(D0), 
            .groups = "drop"
        ) |>
        pivot_longer(cols = c("Average weights", "Exponential weights", "Linear weights"), names_to = "TYPE", values_to = "DHAT") |>
        mutate(
            Prior = "Without prior discharge information"
        )
    
    saveRDS(m_no_dcharge, file = paste0(files_dir, "/Models/SVR/without_prior_discharge_knowledge.Rds"))
    
    rm(m_no_dcharge, svr_no_dcharge_i, svr_no_dcharge)
    
    ## With all prior knowledge
    cat("\tWith all prior knowledge.\n")
    svr_total <- vector("list", length(epsilon))
    for (i in seq_along(epsilon)) {
        cat(i, "/", length(epsilon), " ")
        svr_total_i <- caret::train(
            D0 ~ .*., 
            data = data_train |> select(D0, all_of(window_capacity_features), all_of(window_prior_capacity_features), all_of(window_prior_features)),
            method = 'svmRadial',
            type = "eps-svr",
            epsilon = epsilon[i],
            scaled = FALSE,
            tuneGrid = grid,
            trControl = fit_control
        )
        
        svr_total[[i]] <- svr_total_i
    }
    cat("\n")
    
    
    svr_total <- svr_total[[which.max(sapply(svr_total, function(x) max(x$results$Rsquared)))]]
    m_total <- svr_total$finalModel
    
    p_data_total <- partial_charging_std |>
        mutate(
            DHAT = predict(m_total, newdata = partial_charging_std |> select(all_of(window_capacity_features), all_of(window_prior_capacity_features), all_of(window_prior_features)) |> (\(x) model.matrix(~ -1 + .*., data = x))())[, 1]
        ) |>
        group_by(Profile, Data, Cell, Round) |>
        mutate(
            RoundDIFF = RoundPLUS - RoundPLUS[1]
        ) |>
        summarise(
            FEC = max(FEC_C),
            Temperature = round(median(Temperature_C)),
            `Average weights` =  mean(DHAT),
            `Exponential weights` = sum(exp(RoundDIFF - max(RoundDIFF)) * DHAT / sum(exp(RoundDIFF - max(RoundDIFF)))),
            `Linear weights` = sum(RoundDIFF * DHAT / sum(RoundDIFF)),
            D = mean(D0), 
            .groups = "drop"
        ) |>
        pivot_longer(cols = c("Average weights", "Exponential weights", "Linear weights"), names_to = "TYPE", values_to = "DHAT") |>
        mutate(
            Prior = "With prior information"
        )
    
    saveRDS(m_total, file = paste0(files_dir, "/Models/SVR/with_all_prior_knowledge.Rds"))
    
    rm(m_total, svr_total_i, svr_total)
    
    #### Error tibble ---- 
    prior_levels <- c("Without prior information", "Without prior charge information", "Without prior discharge information", "With prior information")
    svr_error_tibble <- p_data_simple |>
        bind_rows(p_data_no_charge) |>
        bind_rows(p_data_no_dcharge) |>
        bind_rows(p_data_total) |>
        mutate(
            Prior = factor(Prior, levels = prior_levels), 
            Model = "SVR"
        )
    
    saveRDS(svr_error_tibble, file = paste0(files_dir, "/svr_model_tibble.Rds"))
    
}

#### RF ----
if (train_rf_models) {
    #### RF ----
    cat("RF:\n")
    data_train <- partial_charging_std |> filter(Data == "Training")
    
    num.trees = c(500, 1000, 2500)
    grid <-  expand.grid(
        mtry = c(3, 4), 
        min.node.size = c(3, 5, 10),
        splitrule = c("extratrees", "variance")
    )
    
    fit_control <- caret::trainControl(
        method = "CV",
        number = 5,
        verboseIter = FALSE
    )
    
    ## Without any prior knowledge
    cat("\tWithout any prior knowledge.\n")
    rf_simple <- vector("list", length(num.trees))
    for (i in seq_along(num.trees)) {
        rf_simple_i <- caret::train(
            D0 ~ .*., 
            data = data_train |> select(D0, all_of(window_capacity_features)),
            method = "ranger", 
            tuneGrid = grid,
            num.trees = num.trees[i],
            classification = FALSE,
            trControl = fit_control
        )
        
        rf_simple[[i]] <- rf_simple_i
    }
    
    rf_simple <- rf_simple[[which.max(sapply(rf_simple, function(x) max(x$results$Rsquared)))]]
    m_simple <- ranger(
        D0 ~ .*., 
        data = data_train |> select(D0, all_of(window_capacity_features)), 
        num.trees = rf_simple$finalModel$num.trees, 
        mtry = rf_simple$finalModel$mtry, 
        min.node.size = rf_simple$finalModel$min.node.size,
        splitrule = rf_simple$finalModel$splitrule
    )
    
    p_data_simple <- partial_charging_std |>
        mutate(
            DHAT = predict(m_simple, data = partial_charging_std |> (\(x) model.matrix(~ -1 + .*., data = x))())$predictions
        ) |>
        group_by(Profile, Data, Cell, Round) |>
        mutate(
            RoundDIFF = RoundPLUS - RoundPLUS[1]
        ) |>
        summarise(
            FEC = max(FEC_C),
            Temperature = round(median(Temperature_C)),
            `Average weights` =  mean(DHAT),
            `Exponential weights` = sum(exp(RoundDIFF - max(RoundDIFF)) * DHAT / sum(exp(RoundDIFF - max(RoundDIFF)))),
            `Linear weights` = sum(RoundDIFF * DHAT / sum(RoundDIFF)),
            D = mean(D0), 
            .groups = "drop"
        ) |>
        pivot_longer(cols = c("Average weights", "Exponential weights", "Linear weights"), names_to = "TYPE", values_to = "DHAT") |>
        mutate(
            Prior = "Without prior information"
        )
    
    saveRDS(m_simple, file = paste0(files_dir, "/Models/RF/without_prior_knowledge.Rds"))
    
    rm(m_simple, rf_simple_i, rf_simple)
    
    ## Without prior charge knowledge
    cat("\tWithout prior charge knowledge.\n")
    rf_no_charge <- vector("list", length(num.trees))
    for (i in seq_along(num.trees)) {
        rf_no_charge_i <- caret::train(
            D0 ~ .*., 
            data = data_train |> select(D0, all_of(window_capacity_features), all_of(window_prior_features)),
            method = "ranger", 
            tuneGrid = grid,
            num.trees = num.trees[i],
            classification = FALSE,
            trControl = fit_control
        )
        
        rf_no_charge[[i]] <- rf_no_charge_i
    }
    
    rf_no_charge <- rf_no_charge[[which.max(sapply(rf_no_charge, function(x) max(x$results$Rsquared)))]]
    m_no_charge <- ranger(
        D0 ~ .*., 
        data = data_train |> select(D0, all_of(window_capacity_features), all_of(window_prior_features)),
        num.trees = rf_no_charge$finalModel$num.trees, 
        mtry = rf_no_charge$finalModel$mtry, 
        min.node.size = rf_no_charge$finalModel$min.node.size,
        splitrule = rf_no_charge$finalModel$splitrule
    )
    
    p_data_no_charge <- partial_charging_std |>
        mutate(
            DHAT = predict(m_no_charge, data = partial_charging_std |> (\(x) model.matrix(~ -1 + .*., data = x))())$predictions
        ) |>
        group_by(Profile, Data, Cell, Round) |>
        mutate(
            RoundDIFF = RoundPLUS - RoundPLUS[1]
        ) |>
        summarise(
            FEC = max(FEC_C),
            Temperature = round(median(Temperature_C)),
            `Average weights` =  mean(DHAT),
            `Exponential weights` = sum(exp(RoundDIFF - max(RoundDIFF)) * DHAT / sum(exp(RoundDIFF - max(RoundDIFF)))),
            `Linear weights` = sum(RoundDIFF * DHAT / sum(RoundDIFF)),
            D = mean(D0), 
            .groups = "drop"
        ) |>
        pivot_longer(cols = c("Average weights", "Exponential weights", "Linear weights"), names_to = "TYPE", values_to = "DHAT") |>
        mutate(
            Prior = "Without prior charge information"
        )
    
    saveRDS(m_no_charge, file = paste0(files_dir, "/Models/RF/without_prior_charge_knowledge.Rds"))
    
    rm(m_no_charge, rf_no_charge_i, rf_no_charge)
    
    ## Without prior discharge knowledge
    cat("\tWithout prior discharge knowledge.\n")
    rf_no_dcharge <- vector("list", length(num.trees))
    for (i in seq_along(num.trees)) {
        rf_no_dcharge_i <- caret::train(
            D0 ~ .*., 
            data = data_train |> select(D0, all_of(window_capacity_features), all_of(window_prior_capacity_features)),
            method = "ranger", 
            tuneGrid = grid,
            num.trees = num.trees[i],
            classification = FALSE,
            trControl = fit_control
        )
        
        rf_no_dcharge[[i]] <- rf_no_dcharge_i
    }
    
    rf_no_dcharge <- rf_no_dcharge[[which.max(sapply(rf_no_dcharge, function(x) max(x$results$Rsquared)))]]
    m_no_dcharge <- ranger(
        D0 ~ .*., 
        data = data_train |> select(D0, all_of(window_capacity_features), all_of(window_prior_capacity_features)),
        num.trees = rf_no_dcharge$finalModel$num.trees, 
        mtry = rf_no_dcharge$finalModel$mtry, 
        min.node.size = rf_no_dcharge$finalModel$min.node.size,
        splitrule = rf_no_dcharge$finalModel$splitrule
    )
    
    p_data_no_dcharge <- partial_charging_std |>
        mutate(
            DHAT = predict(m_no_dcharge, data = partial_charging_std |> (\(x) model.matrix(~ -1 + .*., data = x))())$predictions
        ) |>
        group_by(Profile, Data, Cell, Round) |>
        mutate(
            RoundDIFF = RoundPLUS - RoundPLUS[1]
        ) |>
        summarise(
            FEC = max(FEC_C),
            Temperature = round(median(Temperature_C)),
            `Average weights` =  mean(DHAT),
            `Exponential weights` = sum(exp(RoundDIFF - max(RoundDIFF)) * DHAT / sum(exp(RoundDIFF - max(RoundDIFF)))),
            `Linear weights` = sum(RoundDIFF * DHAT / sum(RoundDIFF)),
            D = mean(D0), 
            .groups = "drop"
        ) |>
        pivot_longer(cols = c("Average weights", "Exponential weights", "Linear weights"), names_to = "TYPE", values_to = "DHAT") |>
        mutate(
            Prior = "Without prior discharge information"
        )
    
    saveRDS(m_no_dcharge, file = paste0(files_dir, "/Models/RF/without_prior_discharge_knowledge.Rds"))
    
    rm(m_no_dcharge, rf_no_dcharge_i, rf_no_dcharge)
    
    ## With all prior knowledge
    cat("\tWith all prior knowledge.\n")
    rf_total <- vector("list", length(num.trees))
    for (i in seq_along(num.trees)) {
        rf_total_i <- caret::train(
            D0 ~ .*., 
            data = data_train |> select(D0, all_of(window_capacity_features), all_of(window_prior_capacity_features), all_of(window_prior_features)),
            method = "ranger", 
            tuneGrid = grid,
            num.trees = num.trees[i],
            classification = FALSE,
            trControl = fit_control
        )
        
        rf_total[[i]] <- rf_total_i
    }
    
    rf_total <- rf_total[[which.max(sapply(rf_total, function(x) max(x$results$Rsquared)))]]
    m_total <- ranger(
        D0 ~ .*., 
        data = data_train |> select(D0, all_of(window_capacity_features), all_of(window_prior_capacity_features), all_of(window_prior_features)),
        num.trees = rf_total$finalModel$num.trees, 
        mtry = rf_total$finalModel$mtry, 
        min.node.size = rf_total$finalModel$min.node.size,
        splitrule = rf_total$finalModel$splitrule
    )
    
    p_data_total <- partial_charging_std |>
        mutate(
            DHAT = predict(m_total, data = partial_charging_std |> (\(x) model.matrix(~ -1 + .*., data = x))())$predictions
        ) |>
        group_by(Profile, Data, Cell, Round) |>
        mutate(
            RoundDIFF = RoundPLUS - RoundPLUS[1]
        ) |>
        summarise(
            FEC = max(FEC_C),
            Temperature = round(median(Temperature_C)),
            `Average weights` =  mean(DHAT),
            `Exponential weights` = sum(exp(RoundDIFF - max(RoundDIFF)) * DHAT / sum(exp(RoundDIFF - max(RoundDIFF)))),
            `Linear weights` = sum(RoundDIFF * DHAT / sum(RoundDIFF)),
            D = mean(D0), 
            .groups = "drop"
        ) |>
        pivot_longer(cols = c("Average weights", "Exponential weights", "Linear weights"), names_to = "TYPE", values_to = "DHAT") |>
        mutate(
            Prior = "With prior information"
        )
    
    saveRDS(m_total, file = paste0(files_dir, "/Models/RF/with_all_prior_knowledge.Rds"))
    
    rm(m_total, rf_total_i, rf_total)
    
    #### Error tibble ---- 
    prior_levels <- c("Without prior information", "Without prior charge information", "Without prior discharge information", "With prior information")
    rf_error_tibble <- p_data_simple |>
        bind_rows(p_data_no_charge) |>
        bind_rows(p_data_no_dcharge) |>
        bind_rows(p_data_total) |>
        mutate(
            Prior = factor(Prior, levels = prior_levels), 
            Model = "RF"
        )
    
    saveRDS(rf_error_tibble, file = paste0(files_dir, "/rf_model_tibble.Rds"))
}

#### NN ----
if (train_nn_models) {
    #### NN ----
    cat("NN:\n")
    data_train <- partial_charging_std |> filter(Data == "Training")
    
    b_size <- 1000 * round((dim(data_train)[1] / 20) / 1000)
    
    ## Without any prior knowledge
    cat("\tWithout any prior knowledge.\n")
    X_train <- data_train |> select(all_of(window_capacity_features)) |> (\(x) model.matrix(~ -1 + .*., data = x))() |> as.matrix()
    y_train <- data_train |> select(D0) |> as.matrix()
    
    m_simple <- train_nn_model(
        X_train = X_train, y_train = y_train, batch_size = b_size, verbose = 0
    )
    
    p_data_simple <- partial_charging_std |>
        mutate(
            DHAT = predict(m_simple, x = partial_charging_std |> select(all_of(window_capacity_features)) |> (\(x) model.matrix(~ -1 + .*., data = x))()  |> as.matrix(), verbose = 0)[, 1]
        ) |>
        group_by(Profile, Data, Cell, Round) |>
        mutate(
            RoundDIFF = RoundPLUS - RoundPLUS[1]
        ) |>
        summarise(
            FEC = max(FEC_C),
            Temperature = round(median(Temperature_C)),
            `Average weights` =  mean(DHAT),
            `Exponential weights` = sum(exp(RoundDIFF - max(RoundDIFF)) * DHAT / sum(exp(RoundDIFF - max(RoundDIFF)))),
            `Linear weights` = sum(RoundDIFF * DHAT / sum(RoundDIFF)),
            D = mean(D0), 
            .groups = "drop"
        ) |>
        pivot_longer(cols = c("Average weights", "Exponential weights", "Linear weights"), names_to = "TYPE", values_to = "DHAT") |>
        mutate(
            Prior = "Without prior information"
        )
    
    save_model_tf(m_simple, filepath = paste0(files_dir, "/Models/NN/without_prior_knowledge"))
    
    rm(m_simple)
    
    ## Without prior charge knowledge
    cat("\tWithout prior charge knowledge.\n")
    X_train <- data_train |> select(all_of(window_capacity_features), all_of(window_prior_features)) |> (\(x) model.matrix(~ -1 + .*., data = x))()  |> as.matrix()
    y_train <- data_train |> select(D0) |> as.matrix()
    
    m_no_charge <- train_nn_model(
        X_train = X_train, y_train = y_train, batch_size = b_size, verbose = 0
    )
    
    p_data_no_charge <- partial_charging_std |>
        mutate(
            DHAT = predict(m_no_charge, x = partial_charging_std |> select(all_of(window_capacity_features), all_of(window_prior_features)) |> (\(x) model.matrix(~ -1 + .*., data = x))()  |> as.matrix(), verbose = 0)[, 1]
        ) |>
        group_by(Profile, Data, Cell, Round) |>
        mutate(
            RoundDIFF = RoundPLUS - RoundPLUS[1]
        ) |>
        summarise(
            FEC = max(FEC_C),
            Temperature = round(median(Temperature_C)),
            `Average weights` =  mean(DHAT),
            `Exponential weights` = sum(exp(RoundDIFF - max(RoundDIFF)) * DHAT / sum(exp(RoundDIFF - max(RoundDIFF)))),
            `Linear weights` = sum(RoundDIFF * DHAT / sum(RoundDIFF)),
            D = mean(D0), 
            .groups = "drop"
        ) |>
        pivot_longer(cols = c("Average weights", "Exponential weights", "Linear weights"), names_to = "TYPE", values_to = "DHAT") |>
        mutate(
            Prior = "Without prior charge information"
        )
    
    save_model_tf(m_no_charge, filepath = paste0(files_dir, "/Models/NN/without_prior_charge_knowledge"))
    rm(m_no_charge)
    
    ## Without prior discharge knowledge
    cat("\tWithout prior discharge knowledge.\n")
    X_train <- data_train |> select(all_of(window_capacity_features), all_of(window_prior_capacity_features)) |> (\(x) model.matrix(~ -1 + .*., data = x))()  |> as.matrix()
    y_train <- data_train |> select(D0) |> as.matrix()
    
    m_no_dcharge <- train_nn_model(
        X_train = X_train, y_train = y_train, batch_size = b_size, verbose = 0
    )
    
    p_data_no_dcharge <- partial_charging_std |>
        mutate(
            DHAT = predict(m_no_dcharge, x = partial_charging_std |> select(all_of(window_capacity_features), all_of(window_prior_capacity_features)) |> (\(x) model.matrix(~ -1 + .*., data = x))()  |> as.matrix(), verbose = 0)[, 1]
        ) |>
        group_by(Profile, Data, Cell, Round) |>
        mutate(
            RoundDIFF = RoundPLUS - RoundPLUS[1]
        ) |>
        summarise(
            FEC = max(FEC_C),
            Temperature = round(median(Temperature_C)),
            `Average weights` =  mean(DHAT),
            `Exponential weights` = sum(exp(RoundDIFF - max(RoundDIFF)) * DHAT / sum(exp(RoundDIFF - max(RoundDIFF)))),
            `Linear weights` = sum(RoundDIFF * DHAT / sum(RoundDIFF)),
            D = mean(D0), 
            .groups = "drop"
        )|>
        pivot_longer(cols = c("Average weights", "Exponential weights", "Linear weights"), names_to = "TYPE", values_to = "DHAT") |>
        mutate(
            Prior = "Without prior discharge information"
        )
    
    save_model_tf(m_no_dcharge, filepath = paste0(files_dir, "/Models/NN/without_prior_discharge_knowledge"))
    
    rm(m_no_dcharge)
    
    ## With all prior knowledge
    cat("\tWith all prior knowledge.\n")
    X_train <- data_train |> select(all_of(window_capacity_features), all_of(window_prior_capacity_features), all_of(window_prior_features)) |> (\(x) model.matrix(~ -1 + .*., data = x))()  |> as.matrix()
    y_train <- data_train |> select(D0) |> as.matrix()
    
    m_total <- train_nn_model(
        X_train = X_train, y_train = y_train, batch_size = 2*b_size, verbose = 0
    )
    
    p_data_total <- partial_charging_std |>
        mutate(
            DHAT = predict(m_total, x = partial_charging_std |> select(all_of(window_capacity_features), all_of(window_prior_capacity_features), all_of(window_prior_features)) |> (\(x) model.matrix(~ -1 + .*., data = x))()  |> as.matrix(), verbose = 0)[, 1]
        ) |>
        group_by(Profile, Data, Cell, Round) |>
        mutate(
            RoundDIFF = RoundPLUS - RoundPLUS[1]
        ) |>
        summarise(
            FEC = max(FEC_C),
            Temperature = round(median(Temperature_C)),
            `Average weights` =  mean(DHAT),
            `Exponential weights` = sum(exp(RoundDIFF - max(RoundDIFF)) * DHAT / sum(exp(RoundDIFF - max(RoundDIFF)))),
            `Linear weights` = sum(RoundDIFF * DHAT / sum(RoundDIFF)),
            D = mean(D0), 
            .groups = "drop"
        ) |>
        pivot_longer(cols = c("Average weights", "Exponential weights", "Linear weights"), names_to = "TYPE", values_to = "DHAT") |>
        mutate(
            Prior = "With prior information"
        )
    
    save_model_tf(m_total, filepath = paste0(files_dir, "/Models/NN/with_all_prior_knowledge"))
    
    rm(m_total)
    
    #### Error tibble ---- 
    prior_levels <- c("Without prior information", "Without prior charge information", "Without prior discharge information", "With prior information")
    nn_error_tibble <- p_data_simple |>
        bind_rows(p_data_no_charge) |>
        bind_rows(p_data_no_dcharge) |>
        bind_rows(p_data_total) |>
        mutate(
            Prior = factor(Prior, levels = prior_levels), 
            Model = "NN"
        )
    
    saveRDS(nn_error_tibble, file = paste0(files_dir, "/nn_model_tibble.Rds"))
}
