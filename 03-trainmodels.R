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
        scope = list(lower = D0 ~ StartVoltage + DeltaVoltage + Temperature, upper = D0 ~ .*.), 
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
        scope = list(lower = D0 ~ StartVoltage + DeltaVoltage + Temperature, upper = D0 ~ .*.), 
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
        scope = list(lower = D0 ~ StartVoltage + DeltaVoltage + Temperature, upper = D0 ~ .*.), 
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
        scope = list(lower = D0 ~ StartVoltage + DeltaVoltage + Temperature, upper = D0 ~ .*.), 
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
    
    ## Grid of hyperparameters optimised by the 'caret' package.
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
    
    ## Grid of hyperparameters optimised by the 'caret' package.
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
    m_simple <- rf_simple$finalModel
    
    p_data_simple <- partial_charging_std |>
        mutate(
            DHAT = predict(m_simple, data = partial_charging_std |> select(all_of(window_capacity_features)) |> (\(x) model.matrix(~ -1 + .*., data = x))())$predictions
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
    m_no_charge <- rf_no_charge$finalModel
    
    p_data_no_charge <- partial_charging_std |>
        mutate(
            DHAT = predict(m_no_charge, data = partial_charging_std |> select(all_of(window_capacity_features), all_of(window_prior_features)) |> (\(x) model.matrix(~ -1 + .*., data = x))())$predictions
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
    m_no_dcharge <- rf_no_dcharge$finalModel
    
    p_data_no_dcharge <- partial_charging_std |>
        mutate(
            DHAT = predict(m_no_dcharge, data = partial_charging_std |> select(all_of(window_capacity_features), all_of(window_prior_capacity_features)) |> (\(x) model.matrix(~ -1 + .*., data = x))())$predictions
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
    m_total <- rf_total$finalModel
    
    p_data_total <- partial_charging_std |>
        mutate(
            DHAT = predict(m_total, data = partial_charging_std |> select(all_of(window_capacity_features), all_of(window_prior_capacity_features), all_of(window_prior_features)) |> (\(x) model.matrix(~ -1 + .*., data = x))())$predictions
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
    
    ## Batch-size
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
