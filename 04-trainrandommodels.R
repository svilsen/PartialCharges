#### MLR ----
if (trainrandom_mlr_models) {
    #### MLR ----
    cat("MLR:\n")
    data_train <- partial_charging_std |> 
        filter(Data == "Training")
    
    data_train_split <- data_train |> 
        mutate(PC = paste0(Profile, "_", Cell)) |> 
        (\(x) split(x, x$PC))()
    
    trace <- 10
    number_of_bootstraps <- 25
    number_of_charges <- 50
    
    ## Without any prior knowledge
    cat("\tWithout any prior knowledge.\n")
    m_simple_formula <- readRDS(paste0(files_dir, "/Models/MLR/without_prior_knowledge.Rds"))$call$formula
    
    p_data_simple <- vector("list", number_of_charges) 
    for (i in seq_len(number_of_charges)) {
        p_data_simple_i <- vector("list", number_of_bootstraps) 
        for (j in seq_len(number_of_bootstraps)) {
            if ((trace > 1) & ((j == 1) || (j == number_of_bootstraps) || (j %% trace == 0))) {
                cat("\t\tLimit:", i, "/", number_of_charges, ":: Sample:", j, "/", number_of_bootstraps, "\n")
            }
            
            data_train_ij <- data_train_split |> 
                lapply(function(x) {
                    do.call("rbind", lapply(split(x, x$Round), function(xx) {
                        xx[sample(dim(xx)[1], i, replace = TRUE), ]
                    }))
                }) |> 
                (\(x) do.call("rbind", x))()
            
            m_simple_ij <- lm(formula = m_simple_formula, data = data_train_ij)
            p_data_simple_i[[j]] <- partial_charging_std |>
                mutate(
                    DHAT = predict(m_simple_ij, newdata = partial_charging_std)
                ) |>
                group_by(Profile, Data, Cell, Round) |>
                summarise(
                    FEC = max(FEC_C),
                    Temperature = round(median(Temperature_C)),
                    DHAT =  mean(DHAT),
                    D = mean(D0), 
                    .groups = "drop"
                ) |>
                mutate(
                    Limit = i,
                    Model = j,
                    Prior = "Without prior information"
                )
        }
        
        p_data_simple[[i]] <- p_data_simple_i |> bind_rows()
    }
    
    p_data_simple <- p_data_simple |> bind_rows()
    saveRDS(p_data_simple, file = paste0(files_dir, "/RandomModels/MLR/without_prior_knowledge.Rds"))
    
    ## Without prior charge knowledge
    cat("\tWithout prior charge knowledge.\n")
    m_no_charge_formula <- readRDS(paste0(files_dir, "/Models/MLR/without_prior_charge_knowledge.Rds"))$call$formula
    
    p_data_no_charge <- vector("list", number_of_charges) 
    for (i in seq_len(number_of_charges)) {
        p_data_no_charge_i <- vector("list", number_of_bootstraps) 
        for (j in seq_len(number_of_bootstraps)) {
            if ((trace > 1) & ((j == 1) || (j == number_of_bootstraps) || (j %% trace == 0))) {
                cat("\t\tLimit:", i, "/", number_of_charges, ":: Sample:", j, "/", number_of_bootstraps, "\n")
            }
            
            data_train_ij <- data_train_split |> 
                lapply(function(x) {
                    do.call("rbind", lapply(split(x, x$Round), function(xx) {
                        xx[sample(dim(xx)[1], i, replace = TRUE), ]
                    }))
                }) |> 
                (\(x) do.call("rbind", x))()
            
            m_no_charge_ij <- lm(formula = m_no_charge_formula, data = data_train_ij)
            p_data_no_charge_i[[j]] <- partial_charging_std |>
                mutate(
                    DHAT = predict(m_no_charge_ij, newdata = partial_charging_std)
                ) |>
                group_by(Profile, Data, Cell, Round) |>
                summarise(
                    FEC = max(FEC_C),
                    Temperature = round(median(Temperature_C)),
                    DHAT =  mean(DHAT),
                    D = mean(D0), 
                    .groups = "drop"
                ) |>
                mutate(
                    Limit = i,
                    Model = j,
                    Prior = "Without prior charge information"
                )
        }
        
        p_data_no_charge[[i]] <- p_data_no_charge_i |> bind_rows()
    }
    
    p_data_no_charge <- p_data_no_charge |> bind_rows()
    saveRDS(p_data_no_charge, file = paste0(files_dir, "/RandomModels/MLR/without_prior_charge_knowledge.Rds"))
    
    ## Without prior discharge knowledge
    cat("\tWithout prior discharge knowledge.\n")
    m_no_dcharge_formula <- readRDS(paste0(files_dir, "/Models/MLR/without_prior_discharge_knowledge.Rds"))$call$formula
    
    p_data_no_dcharge <- vector("list", number_of_charges) 
    for (i in seq_len(number_of_charges)) {
        p_data_no_dcharge_i <- vector("list", number_of_bootstraps) 
        for (j in seq_len(number_of_bootstraps)) {
            if ((trace > 1) & ((j == 1) || (j == number_of_bootstraps) || (j %% trace == 0))) {
                cat("\t\tLimit:", i, "/", number_of_charges, ":: Sample:", j, "/", number_of_bootstraps, "\n")
            }
            
            data_train_ij <- data_train_split |> 
                lapply(function(x) {
                    do.call("rbind", lapply(split(x, x$Round), function(xx) {
                        xx[sample(dim(xx)[1], i, replace = TRUE), ]
                    }))
                }) |> 
                (\(x) do.call("rbind", x))()
            
            m_no_dcharge_ij <- lm(formula = m_no_dcharge_formula, data = data_train_ij)
            p_data_no_dcharge_i[[j]] <- partial_charging_std |>
                mutate(
                    DHAT = predict(m_no_dcharge_ij, newdata = partial_charging_std)
                ) |>
                group_by(Profile, Data, Cell, Round) |>
                summarise(
                    FEC = max(FEC_C),
                    Temperature = round(median(Temperature_C)),
                    DHAT =  mean(DHAT),
                    D = mean(D0), 
                    .groups = "drop"
                ) |>
                mutate(
                    Limit = i,
                    Model = j,
                    Prior = "Without prior discharge information"
                )
        }
        
        p_data_no_dcharge[[i]] <- p_data_no_dcharge_i |> bind_rows()
    }
    
    p_data_no_dcharge <- p_data_no_dcharge |> bind_rows()
    saveRDS(p_data_no_dcharge, file = paste0(files_dir, "/RandomModels/MLR/without_prior_discharge_knowledge.Rds"))
    
    ## With all prior knowledge
    cat("\tWith all prior knowledge.\n")
    m_total_formula <- readRDS(paste0(files_dir, "/Models/MLR/with_all_prior_knowledge.Rds"))$call$formula
    
    p_data_total <- vector("list", number_of_charges) 
    for (i in seq_len(number_of_charges)) {
        p_data_total_i <- vector("list", number_of_bootstraps) 
        for (j in seq_len(number_of_bootstraps)) {
            if ((trace > 1) & ((j == 1) || (j == number_of_bootstraps) || (j %% trace == 0))) {
                cat("\t\tLimit:", i, "/", number_of_charges, ":: Sample:", j, "/", number_of_bootstraps, "\n")
            }
            
            data_train_ij <- data_train_split |> 
                lapply(function(x) {
                    do.call("rbind", lapply(split(x, x$Round), function(xx) {
                        xx[sample(dim(xx)[1], i, replace = TRUE), ]
                    }))
                }) |> 
                (\(x) do.call("rbind", x))()
            
            m_total_ij <- lm(formula = m_total_formula, data = data_train_ij)
            p_data_total_i[[j]] <- partial_charging_std |>
                mutate(
                    DHAT = predict(m_total_ij, newdata = partial_charging_std)
                ) |>
                group_by(Profile, Data, Cell, Round) |>
                summarise(
                    FEC = max(FEC_C),
                    Temperature = round(median(Temperature_C)),
                    DHAT =  mean(DHAT),
                    D = mean(D0), 
                    .groups = "drop"
                ) |>
                mutate(
                    Limit = i,
                    Model = j,
                    Prior = "With prior information"
                )
        }
        
        p_data_total[[i]] <- p_data_total_i |> bind_rows()
    }
    
    p_data_total <- p_data_total |> bind_rows()
    saveRDS(p_data_total, file = paste0(files_dir, "/RandomModels/MLR/with_all_prior_knowledge.Rds"))
    
    ##
    p_data_simple <- readRDS(paste0(files_dir, "/RandomModels/MLR/without_prior_knowledge.Rds"))
    p_data_no_charge <- readRDS(paste0(files_dir, "/RandomModels/MLR/without_prior_charge_knowledge.Rds"))
    p_data_no_dcharge <- readRDS(paste0(files_dir, "/RandomModels/MLR/without_prior_discharge_knowledge.Rds"))
    p_data_total <- readRDS(paste0(files_dir, "/RandomModels/MLR/with_all_prior_knowledge.Rds"))
    
    prior_levels <- c("Without prior information", "Without prior charge information", "Without prior discharge information", "With prior information")
    mlr_error_tibble <- p_data_simple |>
        bind_rows(p_data_no_charge) |>
        bind_rows(p_data_no_dcharge) |>
        bind_rows(p_data_total) |>
        mutate(
            Prior = factor(Prior, levels = prior_levels), 
            ML = "MLR"
        ) |> 
        select(ML, Limit, Model, Prior, Data, Profile, Cell:D)
    
    saveRDS(mlr_error_tibble, file = paste0(files_dir, "/mlr_randommodel_tibble.Rds"))
    
} 

#### SVR ----
if (trainrandom_svr_models) {
    #### SVR ----
    cat("SVR:\n")
    data_train <- partial_charging_std |> 
        filter(Data == "Training")
    
    data_train_split <- data_train |> 
        mutate(PC = paste0(Profile, "_", Cell)) |> 
        (\(x) split(x, x$PC))()
    
    trace <- 10
    number_of_bootstraps <- 25
    number_of_charges <- 50
    
    ## Without any prior knowledge
    cat("\tWithout any prior knowledge.\n")
    svr_simple_opt <- readRDS(paste0(files_dir, "/Models/SVR/without_prior_knowledge.Rds"))
    C <- svr_simple_opt@param$C
    epsilon <- svr_simple_opt@param$epsilon
    
    p_data_simple <- vector("list", number_of_charges) 
    for (i in seq_len(number_of_charges)) {
        p_data_simple_i <- vector("list", number_of_bootstraps) 
        for (j in seq_len(number_of_bootstraps)) {
            if ((trace > 1) & ((j == 1) || (j == number_of_bootstraps) || (j %% trace == 0))) {
                cat("\t\tLimit:", i, "/", number_of_charges, ":: Sample:", j, "/", number_of_bootstraps, "\n")
            }
            
            data_train_ij <- data_train_split |> 
                lapply(function(x) {
                    do.call("rbind", lapply(split(x, x$Round), function(xx) {
                        xx[sample(dim(xx)[1], i, replace = TRUE), ]
                    }))
                }) |> 
                (\(x) do.call("rbind", x))() |> 
                select(D0, all_of(window_capacity_features))
            
            m_simple_ij <- ksvm(
                D0 ~ -1 + .*., 
                data = data_train_ij,
                kernel = "rbfdot",
                type = "eps-svr",
                epsilon = epsilon, 
                C = C,
                scaled = FALSE
            )
            
            p_data_simple_i[[j]] <- partial_charging_std |>
                mutate(
                    DHAT = predict(m_simple_ij, newdata = partial_charging_std |> select(all_of(window_capacity_features)) |> (\(x) model.matrix(~ -1 + .*., data = x))())[, 1]
                ) |>
                group_by(Profile, Data, Cell, Round) |>
                summarise(
                    FEC = max(FEC_C),
                    Temperature = round(median(Temperature_C)),
                    DHAT =  mean(DHAT),
                    D = mean(D0), 
                    .groups = "drop"
                ) |>
                mutate(
                    Limit = i,
                    Model = j,
                    Prior = "Without prior information"
                )
        }
        
        p_data_simple[[i]] <- p_data_simple_i |> bind_rows()
    }
    
    p_data_simple <- p_data_simple |> bind_rows()
    saveRDS(p_data_simple, file = paste0(files_dir, "/RandomModels/SVR/without_prior_knowledge.Rds"))
    rm(svr_simple_opt, m_simple_ij)
    
    ## Without prior charge knowledge
    cat("\tWithout prior charge knowledge.\n")
    svr_no_charge_opt <- readRDS(paste0(files_dir, "/Models/SVR/without_prior_charge_knowledge.Rds"))
    C <- svr_no_charge_opt@param$C
    epsilon <- svr_no_charge_opt@param$epsilon
    
    p_data_no_charge <- vector("list", number_of_charges) 
    for (i in seq_len(number_of_charges)) {
        p_data_no_charge_i <- vector("list", number_of_bootstraps) 
        for (j in seq_len(number_of_bootstraps)) {
            if ((trace > 1) & ((j == 1) || (j == number_of_bootstraps) || (j %% trace == 0))) {
                cat("\t\tLimit:", i, "/", number_of_charges, ":: Sample:", j, "/", number_of_bootstraps, "\n")
            }
            
            data_train_ij <- data_train_split |> 
                lapply(function(x) {
                    do.call("rbind", lapply(split(x, x$Round), function(xx) {
                        xx[sample(dim(xx)[1], i, replace = TRUE), ]
                    }))
                }) |> 
                (\(x) do.call("rbind", x))() |> 
                select(D0, all_of(window_capacity_features), all_of(window_prior_features))
            
            m_no_charge_ij <- ksvm(
                D0 ~ -1 + .*., 
                data = data_train_ij,
                kernel = "rbfdot",
                type = "eps-svr",
                epsilon = epsilon, 
                C = C,
                scaled = FALSE
            )
            
            p_data_no_charge_i[[j]] <- partial_charging_std |>
                mutate(
                    DHAT = predict(m_no_charge_ij, newdata = partial_charging_std |> select(all_of(window_capacity_features), all_of(window_prior_features)) |> (\(x) model.matrix(~ -1 + .*., data = x))())[, 1]
                ) |>
                group_by(Profile, Data, Cell, Round) |>
                summarise(
                    FEC = max(FEC_C),
                    Temperature = round(median(Temperature_C)),
                    DHAT =  mean(DHAT),
                    D = mean(D0), 
                    .groups = "drop"
                ) |>
                mutate(
                    Limit = i,
                    Model = j,
                    Prior = "Without prior charge information"
                )
        }
        
        p_data_no_charge[[i]] <- p_data_no_charge_i |> bind_rows()
    }
    
    p_data_no_charge <- p_data_no_charge |> bind_rows()
    saveRDS(p_data_no_charge, file = paste0(files_dir, "/RandomModels/SVR/without_prior_charge_knowledge.Rds"))
    rm(svr_no_charge_opt, m_no_charge_ij)
    
    ## Without prior discharge knowledge
    cat("\tWithout prior discharge knowledge.\n")
    svr_no_dcharge_opt <- readRDS(paste0(files_dir, "/Models/SVR/without_prior_discharge_knowledge.Rds"))
    C <- svr_no_dcharge_opt@param$C
    epsilon <- svr_no_dcharge_opt@param$epsilon
    
    p_data_no_dcharge <- vector("list", number_of_charges) 
    for (i in seq_len(number_of_charges)) {
        p_data_no_dcharge_i <- vector("list", number_of_bootstraps) 
        for (j in seq_len(number_of_bootstraps)) {
            if ((trace > 1) & ((j == 1) || (j == number_of_bootstraps) || (j %% trace == 0))) {
                cat("\t\tLimit:", i, "/", number_of_charges, ":: Sample:", j, "/", number_of_bootstraps, "\n")
            }
            
            data_train_ij <- data_train_split |> 
                lapply(function(x) {
                    do.call("rbind", lapply(split(x, x$Round), function(xx) {
                        xx[sample(dim(xx)[1], i, replace = TRUE), ]
                    }))
                }) |> 
                (\(x) do.call("rbind", x))() |> 
                select(D0, all_of(window_capacity_features), all_of(window_prior_capacity_features))
            
            m_no_dcharge_ij <- ksvm(
                D0 ~ -1 + .*., 
                data = data_train_ij,
                kernel = "rbfdot",
                type = "eps-svr",
                epsilon = epsilon, 
                C = C,
                scaled = FALSE
            )
                
            p_data_no_dcharge_i[[j]] <- partial_charging_std |>
                mutate(
                    DHAT = predict(m_no_dcharge_ij, newdata = partial_charging_std |> select(all_of(window_capacity_features), all_of(window_prior_capacity_features)) |> (\(x) model.matrix(~ -1 + .*., data = x))())[, 1]
                ) |>
                group_by(Profile, Data, Cell, Round) |>
                summarise(
                    FEC = max(FEC_C),
                    Temperature = round(median(Temperature_C)),
                    DHAT =  mean(DHAT),
                    D = mean(D0), 
                    .groups = "drop"
                ) |>
                mutate(
                    Limit = i,
                    Model = j,
                    Prior = "Without prior discharge information"
                )
        }
        
        p_data_no_dcharge[[i]] <- p_data_no_dcharge_i |> bind_rows()
    }
    
    p_data_no_dcharge <- p_data_no_dcharge |> bind_rows()
    saveRDS(p_data_no_dcharge, file = paste0(files_dir, "/RandomModels/SVR/without_prior_discharge_knowledge.Rds"))
    
    rm(svr_no_dcharge_opt, m_no_dcharge_ij)
    
    ## With all prior knowledge
    cat("\tWith all prior knowledge.\n")
    svr_total_opt <- readRDS(paste0(files_dir, "/Models/SVR/with_all_prior_knowledge.Rds"))
    C <- svr_total_opt@param$C
    epsilon <- svr_total_opt@param$epsilon
    
    p_data_total <- vector("list", number_of_charges) 
    for (i in seq_len(number_of_charges)) {
        p_data_total_i <- vector("list", number_of_bootstraps) 
        for (j in seq_len(number_of_bootstraps)) {
            if ((trace > 1) & ((j == 1) || (j == number_of_bootstraps) || (j %% trace == 0))) {
                cat("\t\tLimit:", i, "/", number_of_charges, ":: Sample:", j, "/", number_of_bootstraps, "\n")
            }
            
            data_train_ij <- data_train_split |> 
                lapply(function(x) {
                    do.call("rbind", lapply(split(x, x$Round), function(xx) {
                        xx[sample(dim(xx)[1], i, replace = TRUE), ]
                    }))
                }) |> 
                (\(x) do.call("rbind", x))() |> 
                select(D0, all_of(window_capacity_features), all_of(window_prior_capacity_features), all_of(window_prior_features))
            
            m_total_ij <- ksvm(
                D0 ~ -1 + .*., 
                data = data_train_ij,
                kernel = "rbfdot",
                type = "eps-svr",
                epsilon = epsilon, 
                C = C,
                scaled = FALSE
            )
            
            p_data_total_i[[j]] <- partial_charging_std |>
                mutate(
                    DHAT = predict(m_total_ij, newdata = partial_charging_std |> select(all_of(window_capacity_features), all_of(window_prior_capacity_features), all_of(window_prior_features)) |> (\(x) model.matrix(~ -1 + .*., data = x))())[, 1]
                ) |>
                group_by(Profile, Data, Cell, Round) |>
                summarise(
                    FEC = max(FEC_C),
                    Temperature = round(median(Temperature_C)),
                    DHAT =  mean(DHAT),
                    D = mean(D0), 
                    .groups = "drop"
                ) |>
                mutate(
                    Limit = i,
                    Model = j,
                    Prior = "With prior information"
                )
        }
        
        p_data_total[[i]] <- p_data_total_i |> bind_rows()
    }
    
    p_data_total <- p_data_total |> bind_rows()
    saveRDS(p_data_total, file = paste0(files_dir, "/RandomModels/SVR/with_all_prior_knowledge.Rds"))
    rm(svr_total_opt, m_total_ij)
    
    #### Error tibble ---- 
    p_data_simple <- readRDS(paste0(files_dir, "/RandomModels/SVR/without_prior_knowledge.Rds"))
    p_data_no_charge <- readRDS(paste0(files_dir, "/RandomModels/SVR/without_prior_charge_knowledge.Rds"))
    p_data_no_dcharge <- readRDS(paste0(files_dir, "/RandomModels/SVR/without_prior_discharge_knowledge.Rds"))
    p_data_total <- readRDS(paste0(files_dir, "/RandomModels/SVR/with_all_prior_knowledge.Rds"))
    
    prior_levels <- c("Without prior information", "Without prior charge information", "Without prior discharge information", "With prior information")
    svr_error_tibble <- p_data_simple |>
        bind_rows(p_data_no_charge) |>
        bind_rows(p_data_no_dcharge) |>
        bind_rows(p_data_total) |>
        mutate(
            Prior = factor(Prior, levels = prior_levels), 
            ML = "SVR"
        ) |> 
        select(ML, Limit, Model, Prior, Data, Profile, Cell:D)
    
    saveRDS(svr_error_tibble, file = paste0(files_dir, "/svr_randommodel_tibble.Rds"))
    
}

#### RF ----
if (trainrandom_rf_models) {
    #### RF ----
    cat("RF:\n")
    data_train <- partial_charging_std |> 
        filter(Data == "Training")
    
    data_train_split <- data_train |> 
        mutate(PC = paste0(Profile, "_", Cell)) |> 
        (\(x) split(x, x$PC))()
    
    trace <- 10
    number_of_bootstraps <- 25
    number_of_charges <- 50
    
    ## Without any prior knowledge
    cat("\tWithout any prior knowledge.\n")
    rf_simple_opt <- readRDS(paste0(files_dir, "/Models/RF/without_prior_knowledge.Rds"))
    num.trees <- rf_simple_opt$num.trees
    mtry <- rf_simple_opt$mtry
    min.node.size <- rf_simple_opt$min.node.size
    splitrule <- rf_simple_opt$splitrule
    
    p_data_simple <- vector("list", number_of_charges) 
    for (i in seq_len(number_of_charges)) {
        p_data_simple_i <- vector("list", number_of_bootstraps) 
        for (j in seq_len(number_of_bootstraps)) {
            if ((trace > 1) & ((j == 1) || (j == number_of_bootstraps) || (j %% trace == 0))) {
                cat("\t\tLimit:", i, "/", number_of_charges, ":: Sample:", j, "/", number_of_bootstraps, "\n")
            }
            
            data_train_ij <- data_train_split |> 
                lapply(function(x) {
                    do.call("rbind", lapply(split(x, x$Round), function(xx) {
                        xx[sample(dim(xx)[1], i, replace = TRUE), ]
                    }))
                }) |> 
                (\(x) do.call("rbind", x))() |> 
                select(D0, all_of(window_capacity_features))
            
            m_simple_ij <- ranger(
                D0 ~ .*., 
                data = data_train_ij, 
                num.trees = num.trees, 
                mtry = mtry, 
                min.node.size = min.node.size, 
                splitrule = splitrule, 
                classification = FALSE
            )
            
            p_data_simple_i[[j]] <- partial_charging_std |>
                mutate(
                    DHAT = predict(m_simple_ij, data = partial_charging_std |> select(all_of(window_capacity_features)) |> (\(x) model.matrix(~ -1 + .*., data = x))())$predictions
                ) |>
                group_by(Profile, Data, Cell, Round) |>
                summarise(
                    FEC = max(FEC_C),
                    Temperature = round(median(Temperature_C)),
                    DHAT =  mean(DHAT),
                    D = mean(D0), 
                    .groups = "drop"
                ) |>
                mutate(
                    Limit = i,
                    Model = j,
                    Prior = "Without prior information"
                )
        }
        
        p_data_simple[[i]] <- p_data_simple_i |> bind_rows()
    }
    
    p_data_simple <- p_data_simple |> bind_rows()
    saveRDS(p_data_simple, file = paste0(files_dir, "/RandomModels/RF/without_prior_knowledge.Rds"))
    rm(rf_simple_opt, m_simple_ij)
    
    ## Without prior charge knowledge
    cat("\tWithout prior charge knowledge.\n")
    rf_no_charge_opt <- readRDS(paste0(files_dir, "/Models/RF/without_prior_charge_knowledge.Rds"))
    num.trees <- rf_no_charge_opt$num.trees
    mtry <- rf_no_charge_opt$mtry
    min.node.size <- rf_no_charge_opt$min.node.size
    splitrule <- rf_no_charge_opt$splitrule
    
    p_data_no_charge <- vector("list", number_of_charges) 
    for (i in seq_len(number_of_charges)) {
        p_data_no_charge_i <- vector("list", number_of_bootstraps) 
        for (j in seq_len(number_of_bootstraps)) {
            if ((trace > 1) & ((j == 1) || (j == number_of_bootstraps) || (j %% trace == 0))) {
                cat("\t\tLimit:", i, "/", number_of_charges, ":: Sample:", j, "/", number_of_bootstraps, "\n")
            }
            
            data_train_ij <- data_train_split |> 
                lapply(function(x) {
                    do.call("rbind", lapply(split(x, x$Round), function(xx) {
                        xx[sample(dim(xx)[1], i, replace = TRUE), ]
                    }))
                }) |> 
                (\(x) do.call("rbind", x))() |> 
                select(D0, all_of(window_capacity_features), all_of(window_prior_features))
            
            m_no_charge_ij <- ranger(
                D0 ~ .*., 
                data = data_train_ij, 
                num.trees = num.trees, 
                mtry = mtry, 
                min.node.size = min.node.size, 
                splitrule = splitrule, 
                classification = FALSE
            )
            
            p_data_no_charge_i[[j]] <- partial_charging_std |>
                mutate(
                    DHAT = predict(m_no_charge_ij, data = partial_charging_std |> select(all_of(window_capacity_features), all_of(window_prior_features)) |> (\(x) model.matrix(~ -1 + .*., data = x))())$predictions
                ) |>
                group_by(Profile, Data, Cell, Round) |>
                summarise(
                    FEC = max(FEC_C),
                    Temperature = round(median(Temperature_C)),
                    DHAT =  mean(DHAT),
                    D = mean(D0), 
                    .groups = "drop"
                ) |>
                mutate(
                    Limit = i,
                    Model = j,
                    Prior = "Without prior charge information"
                )
        }
        
        p_data_no_charge[[i]] <- p_data_no_charge_i |> bind_rows()
    }
    
    p_data_no_charge <- p_data_no_charge |> bind_rows()
    saveRDS(p_data_no_charge, file = paste0(files_dir, "/RandomModels/RF/without_prior_charge_knowledge.Rds"))
    rm(rf_no_charge_opt, m_no_charge_ij)
    
    ## Without prior discharge knowledge
    cat("\tWithout prior discharge knowledge.\n")
    rf_no_dcharge_opt <- readRDS(paste0(files_dir, "/Models/RF/without_prior_discharge_knowledge.Rds"))
    num.trees <- rf_no_dcharge_opt$num.trees
    mtry <- rf_no_dcharge_opt$mtry
    min.node.size <- rf_no_dcharge_opt$min.node.size
    splitrule <- rf_no_dcharge_opt$splitrule
    
    p_data_no_dcharge <- vector("list", number_of_charges) 
    for (i in seq_len(number_of_charges)) {
        p_data_no_dcharge_i <- vector("list", number_of_bootstraps) 
        for (j in seq_len(number_of_bootstraps)) {
            if ((trace > 1) & ((j == 1) || (j == number_of_bootstraps) || (j %% trace == 0))) {
                cat("\t\tLimit:", i, "/", number_of_charges, ":: Sample:", j, "/", number_of_bootstraps, "\n")
            }
            
            data_train_ij <- data_train_split |> 
                lapply(function(x) {
                    do.call("rbind", lapply(split(x, x$Round), function(xx) {
                        xx[sample(dim(xx)[1], i, replace = TRUE), ]
                    }))
                }) |> 
                (\(x) do.call("rbind", x))() |> 
                select(D0, all_of(window_capacity_features), all_of(window_prior_capacity_features))
            
            m_no_dcharge_ij <- ranger(
                D0 ~ .*., 
                data = data_train_ij, 
                num.trees = num.trees, 
                mtry = mtry, 
                min.node.size = min.node.size, 
                splitrule = splitrule, 
                classification = FALSE
            )
            
            p_data_no_dcharge_i[[j]] <- partial_charging_std |>
                mutate(
                    DHAT = predict(m_no_dcharge_ij, data = partial_charging_std |> select(all_of(window_capacity_features), all_of(window_prior_capacity_features)) |> (\(x) model.matrix(~ -1 + .*., data = x))())$predictions
                ) |>
                group_by(Profile, Data, Cell, Round) |>
                summarise(
                    FEC = max(FEC_C),
                    Temperature = round(median(Temperature_C)),
                    DHAT =  mean(DHAT),
                    D = mean(D0), 
                    .groups = "drop"
                ) |>
                mutate(
                    Limit = i,
                    Model = j,
                    Prior = "Without prior discharge information"
                )
        }
        
        p_data_no_dcharge[[i]] <- p_data_no_dcharge_i |> bind_rows()
    }
    
    p_data_no_dcharge <- p_data_no_dcharge |> bind_rows()
    saveRDS(p_data_no_dcharge, file = paste0(files_dir, "/RandomModels/RF/without_prior_discharge_knowledge.Rds"))
    rm(rf_total_opt, m_no_dcharge_ij)
    
    ## With all prior knowledge
    cat("\tWith all prior knowledge.\n")
    rf_total_opt <- readRDS(paste0(files_dir, "/Models/RF/with_all_prior_knowledge.Rds"))
    num.trees <- rf_total_opt$num.trees
    mtry <- rf_total_opt$mtry
    min.node.size <- rf_total_opt$min.node.size
    splitrule <- rf_total_opt$splitrule
    
    p_data_total <- vector("list", number_of_charges) 
    for (i in seq_len(number_of_charges)) {
        p_data_total_i <- vector("list", number_of_bootstraps) 
        for (j in seq_len(number_of_bootstraps)) {
            if ((trace > 1) & ((j == 1) || (j == number_of_bootstraps) || (j %% trace == 0))) {
                cat("\t\tLimit:", i, "/", number_of_charges, ":: Sample:", j, "/", number_of_bootstraps, "\n")
            }
            
            data_train_ij <- data_train_split |> 
                lapply(function(x) {
                    do.call("rbind", lapply(split(x, x$Round), function(xx) {
                        xx[sample(dim(xx)[1], i, replace = TRUE), ]
                    }))
                }) |> 
                (\(x) do.call("rbind", x))() |> 
                select(D0, all_of(window_capacity_features), all_of(window_prior_capacity_features), all_of(window_prior_features))
            
            m_total_ij <- ranger(
                D0 ~ .*., 
                data = data_train_ij, 
                num.trees = num.trees, 
                mtry = mtry, 
                min.node.size = min.node.size, 
                splitrule = splitrule, 
                classification = FALSE
            )
            
            p_data_total_i[[j]] <- partial_charging_std |>
                mutate(
                    DHAT = predict(m_total_ij, data = partial_charging_std |> select(all_of(window_capacity_features), all_of(window_prior_capacity_features), all_of(window_prior_features)) |> (\(x) model.matrix(~ -1 + .*., data = x))())$predictions
                ) |>
                group_by(Profile, Data, Cell, Round) |>
                summarise(
                    FEC = max(FEC_C),
                    Temperature = round(median(Temperature_C)),
                    DHAT =  mean(DHAT),
                    D = mean(D0), 
                    .groups = "drop"
                ) |>
                mutate(
                    Limit = i,
                    Model = j,
                    Prior = "With prior information"
                )
        }
        
        p_data_total[[i]] <- p_data_total_i |> bind_rows()
    }
    
    p_data_total <- p_data_total |> bind_rows()
    saveRDS(p_data_total, file = paste0(files_dir, "/RandomModels/RF/with_all_prior_knowledge.Rds"))
    rm(rf_total_opt, m_total_ij)
    
    #### Error tibble ---- 
    p_data_simple <- readRDS(paste0(files_dir, "/RandomModels/RF/without_prior_knowledge.Rds"))
    p_data_no_charge <- readRDS(paste0(files_dir, "/RandomModels/RF/without_prior_charge_knowledge.Rds"))
    p_data_no_dcharge <- readRDS(paste0(files_dir, "/RandomModels/RF/without_prior_discharge_knowledge.Rds"))
    p_data_total <- readRDS(paste0(files_dir, "/RandomModels/RF/with_all_prior_knowledge.Rds"))
    
    prior_levels <- c("Without prior information", "Without prior charge information", "Without prior discharge information", "With prior information")
    rf_error_tibble <- p_data_simple |>
        bind_rows(p_data_no_charge) |>
        bind_rows(p_data_no_dcharge) |>
        bind_rows(p_data_total) |>
        mutate(
            Prior = factor(Prior, levels = prior_levels), 
            ML = "RF"
        ) |> 
        select(ML, Limit, Model, Prior, Data, Profile, Cell:D)
    
    saveRDS(rf_error_tibble, file = paste0(files_dir, "/rf_randommodel_tibble.Rds"))
    
}

#### NN ----
if (trainrandom_nn_models) {
    #### NN ----
    cat("NN:\n")
    data_train <- partial_charging_std |> 
        filter(Data == "Training")
    
    data_train_split <- data_train |> 
        mutate(PC = paste0(Profile, "_", Cell)) |> 
        (\(x) split(x, x$PC))()
    
    trace <- 10
    number_of_bootstraps <- 25
    number_of_charges <- 50
    
    # Batch-size
    b_size <- 2 * 1000 * round((dim(data_train)[1] / 20) / 1000)
    
    ## Without any prior knowledge
    cat("\tWithout any prior knowledge.\n")
    p_data_simple <- vector("list", number_of_charges) 
    for (i in seq_len(number_of_charges)) {
        p_data_simple_i <- vector("list", number_of_bootstraps) 
        for (j in seq_len(number_of_bootstraps)) {
            if ((trace > 1) & ((j == 1) || (j == number_of_bootstraps) || (j %% trace == 0))) {
                cat("\t\tLimit:", i, "/", number_of_charges, ":: Sample:", j, "/", number_of_bootstraps, "\n")
            }
            
            data_train_ij <- data_train_split |> 
                lapply(function(x) {
                    do.call("rbind", lapply(split(x, x$Round), function(xx) {
                        xx[sample(dim(xx)[1], i, replace = TRUE), ]
                    }))
                }) |> 
                (\(x) do.call("rbind", x))() |> 
                select(D0, all_of(window_capacity_features))
            
            X_train_ij <- data_train_ij |> select(-D0) |> (\(x) model.matrix(~ -1 + .*., data = x))() |> as.matrix()
            y_train_ij <- data_train_ij |> select(D0) |> as.matrix()
            
            m_simple_ij <- train_nn_model(
                X_train = X_train_ij, y_train = y_train_ij, batch_size = b_size, verbose = 0
            )
            
            p_data_simple_i[[j]] <- partial_charging_std |>
                mutate(
                    DHAT = predict(m_simple_ij, x = partial_charging_std |> select(all_of(window_capacity_features)) |> (\(x) model.matrix(~ -1 + .*., data = x))()  |> as.matrix(), verbose = 0)[, 1]
                ) |>
                group_by(Profile, Data, Cell, Round) |>
                summarise(
                    FEC = max(FEC_C),
                    Temperature = round(median(Temperature_C)),
                    DHAT =  mean(DHAT),
                    D = mean(D0), 
                    .groups = "drop"
                ) |>
                mutate(
                    Limit = i,
                    Model = j,
                    Prior = "Without prior information"
                )
        }
        
        p_data_simple[[i]] <- p_data_simple_i |> bind_rows()
    }
    
    p_data_simple <- p_data_simple |> bind_rows()
    saveRDS(p_data_simple, file = paste0(files_dir, "/RandomModels/NN/without_prior_knowledge"))
    rm(m_simple)
    
    ## Without prior charge knowledge
    cat("\tWithout prior charge knowledge.\n")
    p_data_no_charge <- vector("list", number_of_charges) 
    for (i in seq_len(number_of_charges)) {
        p_data_no_charge_i <- vector("list", number_of_bootstraps) 
        for (j in seq_len(number_of_bootstraps)) {
            if ((trace > 1) & ((j == 1) || (j == number_of_bootstraps) || (j %% trace == 0))) {
                cat("\t\tLimit:", i, "/", number_of_charges, ":: Sample:", j, "/", number_of_bootstraps, "\n")
            }
            
            data_train_ij <- data_train_split |> 
                lapply(function(x) {
                    do.call("rbind", lapply(split(x, x$Round), function(xx) {
                        xx[sample(dim(xx)[1], i, replace = TRUE), ]
                    }))
                }) |> 
                (\(x) do.call("rbind", x))() |> 
                select(D0, all_of(window_capacity_features), all_of(window_prior_features))
            
            X_train_ij <- data_train_ij |> select(-D0) |> (\(x) model.matrix(~ -1 + .*., data = x))()  |> as.matrix()
            y_train_ij <- data_train_ij |> select(D0) |> as.matrix()
            
            m_no_charge_ij <- train_nn_model(
                X_train = X_train_ij, y_train = y_train_ij, batch_size = b_size, verbose = 0
            )
            
            p_data_no_charge_i[[j]] <- partial_charging_std |>
                mutate(
                    DHAT = predict(m_no_charge_ij, x = partial_charging_std |> select(all_of(window_capacity_features), all_of(window_prior_features)) |> (\(x) model.matrix(~ -1 + .*., data = x))())[,1]
                ) |>
                group_by(Profile, Data, Cell, Round) |>
                summarise(
                    FEC = max(FEC_C),
                    Temperature = round(median(Temperature_C)),
                    DHAT =  mean(DHAT),
                    D = mean(D0), 
                    .groups = "drop"
                ) |>
                mutate(
                    Limit = i,
                    Model = j,
                    Prior = "Without prior charge information"
                )
        }
        
        p_data_no_charge[[i]] <- p_data_no_charge_i |> bind_rows()
    }
    
    p_data_no_charge <- p_data_no_charge |> bind_rows()
    saveRDS(p_data_no_charge, file = paste0(files_dir, "/RandomModels/NN/without_prior_charge_knowledge"))
    rm(m_no_charge)
    
    ## Without prior discharge knowledge
    cat("\tWithout prior discharge knowledge.\n")
    p_data_no_dcharge <- vector("list", number_of_charges) 
    for (i in seq_len(number_of_charges)) {
        p_data_no_dcharge_i <- vector("list", number_of_bootstraps) 
        for (j in seq_len(number_of_bootstraps)) {
            if ((trace > 1) & ((j == 1) || (j == number_of_bootstraps) || (j %% trace == 0))) {
                cat("\t\tLimit:", i, "/", number_of_charges, ":: Sample:", j, "/", number_of_bootstraps, "\n")
            }
            
            data_train_ij <- data_train_split |> 
                lapply(function(x) {
                    do.call("rbind", lapply(split(x, x$Round), function(xx) {
                        xx[sample(dim(xx)[1], i, replace = TRUE), ]
                    }))
                }) |> 
                (\(x) do.call("rbind", x))() |> 
                select(D0, all_of(window_capacity_features), all_of(window_prior_capacity_features))
            
            X_train_ij <- data_train_ij |> select(-D0) |> (\(x) model.matrix(~ -1 + .*., data = x))()  |> as.matrix()
            y_train_ij <- data_train_ij |> select(D0) |> as.matrix()
            
            m_no_dcharge_ij <- train_nn_model(
                X_train = X_train_ij, y_train = y_train_ij, batch_size = b_size, verbose = 0
            )
            
            p_data_no_dcharge_i[[j]] <- partial_charging_std |>
                mutate(
                    DHAT = predict(m_no_dcharge_ij, x = partial_charging_std |> select(all_of(window_capacity_features), all_of(window_prior_capacity_features)) |> (\(x) model.matrix(~ -1 + .*., data = x))())[,1]
                ) |>
                group_by(Profile, Data, Cell, Round) |>
                summarise(
                    FEC = max(FEC_C),
                    Temperature = round(median(Temperature_C)),
                    DHAT =  mean(DHAT),
                    D = mean(D0), 
                    .groups = "drop"
                ) |>
                mutate(
                    Limit = i,
                    Model = j,
                    Prior = "Without prior discharge information"
                )
        }
        
        p_data_no_dcharge[[i]] <- p_data_no_dcharge_i |> bind_rows()
    }
    
    p_data_no_dcharge <- p_data_no_dcharge |> bind_rows()
    saveRDS(p_data_no_dcharge, file = paste0(files_dir, "/RandomModels/NN/without_prior_discharge_knowledge"))
    rm(m_no_dcharge)
    
    ## With all prior knowledge
    cat("\tWith all prior knowledge.\n")
    p_data_total <- vector("list", number_of_charges) 
    for (i in seq_len(number_of_charges)) {
        p_data_total_i <- vector("list", number_of_bootstraps) 
        for (j in seq_len(number_of_bootstraps)) {
            if ((trace > 1) & ((j == 1) || (j == number_of_bootstraps) || (j %% trace == 0))) {
                cat("\t\tLimit:", i, "/", number_of_charges, ":: Sample:", j, "/", number_of_bootstraps, "\n")
            }
            
            data_train_ij <- data_train_split |> 
                lapply(function(x) {
                    do.call("rbind", lapply(split(x, x$Round), function(xx) {
                        xx[sample(dim(xx)[1], i, replace = TRUE), ]
                    }))
                }) |> 
                (\(x) do.call("rbind", x))() |> 
                select(D0, all_of(window_capacity_features), all_of(window_prior_capacity_features), all_of(window_prior_features))
            
            X_train_ij <- data_train_ij |> select(-D0) |> (\(x) model.matrix(~ -1 + .*., data = x))()  |> as.matrix()
            y_train_ij <- data_train_ij |> select(D0) |> as.matrix()
            
            m_total_ij <- train_nn_model(
                X_train = X_train_ij, y_train = y_train_ij, batch_size = b_size, verbose = 0
            )
            
            p_data_total_i[[j]] <- partial_charging_std |>
                mutate(
                    DHAT = predict(m_total_ij, x = partial_charging_std |> select(all_of(window_capacity_features), all_of(window_prior_capacity_features), all_of(window_prior_features)) |> (\(x) model.matrix(~ -1 + .*., data = x))())[,1]
                ) |>
                group_by(Profile, Data, Cell, Round) |>
                summarise(
                    FEC = max(FEC_C),
                    Temperature = round(median(Temperature_C)),
                    DHAT =  mean(DHAT),
                    D = mean(D0), 
                    .groups = "drop"
                ) |>
                mutate(
                    Limit = i,
                    Model = j,
                    Prior = "With prior information"
                )
        }
        
        p_data_total[[i]] <- p_data_total_i |> bind_rows()
    }
    
    p_data_total <- p_data_total |> bind_rows()
    saveRDS(p_data_total, file = paste0(files_dir, "/RandomModels/NN/with_all_prior_knowledge.Rds"))
    rm(m_total)
    
    #### Error tibble ---- 
    p_data_simple <- readRDS(paste0(files_dir, "/RandomModels/NN/without_prior_knowledge.Rds"))
    p_data_no_charge <- readRDS(paste0(files_dir, "/RandomModels/NN/without_prior_charge_knowledge.Rds"))
    p_data_no_dcharge <- readRDS(paste0(files_dir, "/RandomModels/NN/without_prior_discharge_knowledge.Rds"))
    p_data_total <- readRDS(paste0(files_dir, "/RandomModels/NN/with_all_prior_knowledge.Rds"))
    
    prior_levels <- c("Without prior information", "Without prior charge information", "Without prior discharge information", "With prior information")
    nn_error_tibble <- p_data_simple |>
        bind_rows(p_data_no_charge) |>
        bind_rows(p_data_no_dcharge) |>
        bind_rows(p_data_total) |>
        mutate(
            Prior = factor(Prior, levels = prior_levels), 
            ML = "NN"
        )
    
    saveRDS(nn_error_tibble, file = paste0(files_dir, "/nn_randommodel_tibble.Rds"))
    
}
