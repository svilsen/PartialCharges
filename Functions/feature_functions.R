## Restricted descriptive statistics 
r_mean <- function(x, p, na.rm = TRUE) {
    if (na.rm) {
        x <- x[!is.na(x)]
    }
    
    N <- length(x)
    R <- floor(p * N)
    O <- order(x)
    K <- O[seq(1 + R, N - R)]
    M <- mean(x[K])
    return(M)
}

r_sd <- function(x, p, na.rm = TRUE) {
    if (na.rm) {
        x <- x[!is.na(x)]
    }
    
    N <- length(x)
    R <- floor(p * N)
    O <- order(x)
    K <- O[seq(1 + R, N - R)]
    M <- mean(x[K])
    S <- sum((x[K] - M)^2) / (N - 2 * R - 1)
    
    return(S)
}

r_skewness <- function(x, p, na.rm = TRUE) {
    if (na.rm) {
        x <- x[!is.na(x)]
    }
    
    N <- length(x)
    R <- floor(p * N)
    O <- order(x)
    K <- O[seq(1 + R, N - R)]
    S <- skewness(x[K], na.rm = na.rm)
    
    return(S)
}

r_kurtosis <- function(x, p, na.rm = TRUE) {
    if (na.rm) {
        x <- x[!is.na(x)]
    }
    
    N <- length(x)
    R <- floor(p * N)
    O <- order(x)
    K <- O[seq(1 + R, N - R)]
    S <- kurtosis(x[K], na.rm = na.rm)
    
    return(S)
}

r_mad <- function(x, p, na.rm = TRUE) {
    R <- r_mean(abs(x - r_mean(x, p, na.rm)), p, na.rm)
    return(R)
}

## Sample entropy
sourceCpp(code='
        #include <Rcpp.h>
        
        // [[Rcpp::export]]
        double sample_entropy_cpp(std::vector<double> X, const double m, const double r) {
            const int N = X.size();
            double A = 0.0;
            double B = 0.0;
            for (int i = 0; i < (N - m); i++) {
                double A_i = 0.0;
                double B_i = 0.0;
                for (int j = 0; j < (N - m); j++) {
                    if (i != j) {
                        double d_m = 0.0;
                        double d_m_1 = 0.0;
                        for (int k = 0; k < (m + 1); k++) {
                            double d = std::abs(X[i + k] - X[j + k]);
                            if (k < m) {
                                if (d > d_m) {
                                    d_m = d;
                                }                            
                            }
                            else {
                                d_m_1 = d_m;
                                if (d > d_m_1) {
                                    d_m_1 = d;
                                }
                            }
                        }
                        
                        double A_ij = 0.0;
                        if (d_m_1 < r) {
                            A_ij = 1.0;
                        }
                        
                        double B_ij = 0.0;
                        if (d_m < r) {
                            B_ij = 1.0;
                        }
                        
                        A_i += A_ij;
                        B_i += B_ij;
                    }
                }
                
                A += A_i / (N - m);
                B += B_i / (N - m);
            }
            
            double s = -std::log(A / B);
            return s;
        }'
)

sample_entropy <- function(X, m = NULL, r = NULL, normalise = FALSE) {
    ## 
    if (normalise) {
        X <- (X - mean(X)) / sd(X)
    }
    
    if (is.null(m)) {
        m <- 2
    }
    
    sigma_X = sd(X)
    if (is.null(r)) {
        r <- 0.2 * sigma_X
    }
    
    s_entropy <- sample_entropy_cpp(X, m, r)
    return(s_entropy)
}

## Fuzzy entropy
sourceCpp(code='
        #include <Rcpp.h>
        
        double mu(const double d, const double r) {
            double dr = d / r; 
            return std::exp(-std::log(2) * dr * dr);
        }
        
        // [[Rcpp::export]]
        double fuzzy_entropy_cpp(std::vector<double> X, const double m, const double r) {
            const int N = X.size();
            double A = 0.0;
            double B = 0.0;
            for (int i = 0; i < (N - m); i++) {
                double A_i = 0.0;
                double B_i = 0.0;
                for (int j = 0; j < (N - m); j++) {
                    if (i != j) {
                        double Xi_m = 0.0;
                        double Xi_m_1 = 0.0;
                        
                        double Xj_m = 0.0;
                        double Xj_m_1 = 0.0;
                        for (int k = 0; k < (m + 1); k++) {
                            if (k < m) {
                                Xi_m += X[i + k] / m;
                                Xj_m += X[j + k] / m;
                                
                                Xi_m_1 += X[i + k] / (m + 1);
                                Xj_m_1 += X[j + k] / (m + 1);
                            }
                            else {
                                Xi_m_1 += X[i + k] / (m + 1);
                                Xj_m_1 += X[j + k] / (m + 1);
                            }
                        }
                    
                        double d_m = 0.0;
                        double d_m_1 = 0.0;
                        for (int k = 0; k < (m + 1); k++) {
                            double d_k_m = std::abs((X[i + k] - Xi_m) - (X[j + k] - Xj_m));
                            double d_k_m_1 = std::abs((X[i + k] - Xi_m_1) - (X[j + k] - Xj_m_1));
                            if (k < m) {
                                if (d_k_m > d_m) {
                                    d_m = d_k_m;
                                }
                                
                                if (d_k_m_1 > d_m_1) {
                                    d_m_1 = d_k_m_1;
                                }
                            }
                            else {
                                if (d_k_m_1 > d_m_1) {
                                    d_m_1 = d_k_m_1;
                                }
                            }
                        }
                        
                        double A_ij = mu(d_m_1, r);
                        double B_ij = mu(d_m, r);
                        
                        A_i += A_ij;
                        B_i += B_ij;
                    }
                }
                
                A += A_i / (N - m);
                B += B_i / (N - m);
            }
            
            double s = -std::log(A / B);
            return s;
        }'
)

fuzzy_entropy <- function(X, m = NULL, r = NULL, normalise = FALSE) {
    ## 
    if (normalise) {
        X <- (X - mean(X)) / sd(X)
    }
    
    if (is.null(m)) {
        m <- 2
    }
    
    sigma_X = sd(X)
    if (is.null(r)) {
        r <- 0.2 * sigma_X
    }
    
    f_entropy <- fuzzy_entropy_cpp(X, m, r)
    return(f_entropy)
}

## Time correction
sourceCpp(code='
        // [[Rcpp::depends(RcppArmadillo)]]
        #include <RcppArmadillo.h>
        
        // [[Rcpp::export]]
        arma::mat correct_time(const arma::colvec & time, const arma::colvec & current, const arma::colvec & voltage, const arma::colvec & temperature, const arma::colvec & fec) 
        {
            unsigned int S_total = 1;   
            unsigned int S = current.size();
            for (unsigned int s = 1; s < S; s++)
            {
                double new_time = std::ceil(time[s] - time[s - 1]);
                if (new_time < 1) 
                    new_time = 1;
                
                unsigned int new_time_ = static_cast<unsigned int>(new_time);
                S_total += new_time_;
            }
            
            arma::mat M(S_total, 5);
            M(0, 0) = 1;
            M(0, 1) = current[0];
            M(0, 2) = voltage[0];
            M(0, 3) = temperature[0];
            M(0, 4) = fec[0];
            
            unsigned int S_total_2 = 1;   
            for (unsigned int s = 1; s < S; s++) 
            {
                double new_time = std::ceil(time[s] - time[s - 1]);
                if (new_time < 1) 
                    new_time = 1;
                
                unsigned int S_s = static_cast<unsigned int>(new_time);
                for (unsigned int s_s = 0; s_s < S_s; s_s++) 
                {
                    M(S_total_2 + s_s, 0) = S_total_2 + s_s + 1;
                    M(S_total_2 + s_s, 1) = current[s];
                    M(S_total_2 + s_s, 2) = voltage[s];
                    M(S_total_2 + s_s, 3) = temperature[s];
                    
                    if (s_s == 0) {
                        M(S_total_2 + s_s, 4) = fec[s];
                    }
                    else {
                        M(S_total_2 + s_s, 4) = NA_REAL;
                    }
                }
                
                S_total_2 += S_s;
            }
            
            return M;
        }'
)

## Extract features 
feature_extraction <- function(
        ageing_file, 
        capacity_file, 
        capacity_0,
        fec,
        fec_0,
        voltage_feature_functions, 
        current_feature_functions, 
        charging_lower_limit, 
        min_start_voltage, 
        max_end_voltage, 
        epsilon = 20, 
        nr_cores = 1, 
        trace = 10
) {
    ## Calculate FEC
    ageing_file <- ageing_file |> 
        mutate(
            FEC = cumsum(diff(c(0, Time)) * abs(Current) / 3600) / (2 * capacity_0) + fec
        )
    
    ## Corrects time to be second-by-second
    ageing_time <- correct_time(
        time = ageing_file$Time,
        current = ageing_file$Current, 
        voltage = ageing_file$Voltage,
        temperature = ageing_file$Temperature,
        fec = ageing_file$FEC
    ) |> 
        structure(.Dimnames = list(NULL, c("Time", "Current", "Voltage", "Temperature", "FEC"))) |> 
        as_tibble()
    
    if (is.na(ageing_time$FEC[length(ageing_time$FEC)])) {
        ageing_time$FEC[length(ageing_time$FEC)] <- fec + fec_0
    }
    
    
    ageing_time <- ageing_time |> 
        mutate(FEC = zoo::na.approx(FEC))
    
    ageing_file <- ageing_file |> 
        distinct(Cell, Round) |> 
        mutate(VAL = list(ageing_time)) |> 
        unnest(VAL)
    
    ## Finds partial charges longer than 'charging_lower_limit' seconds
    total_time <- max(ageing_file$Time)
    ageing_file_split <- ageing_file |> 
        filter(Current >= 0) |> 
        mutate(DT = cumsum(abs(diff(c(-epsilon, Time))) > epsilon)) |> 
        group_by(DT) |> 
        mutate(
            N = n(), 
            SOH0 = capacity_file$Capacity[2],
            SOH = capacity_file$Capacity[1] - min(Time) / total_time * (capacity_file$Capacity[1] - capacity_file$Capacity[2]), 
            D0 = 1 - SOH0 / capacity_0,
            D = 1 - SOH / capacity_0,
            TimeStart = Time[1],
            TIMEOLD = Time,
            Time = Time - Time[1] + 1, 
            AH = cumsum(Current / 3600)
        ) |> 
        ungroup() |> 
        filter(N > charging_lower_limit) |> 
        arrange(TimeStart) |> 
        mutate(
            ChargeCycle = as.numeric(as.factor(TimeStart))
        ) |>
        (\(x) split(x, x$ChargeCycle))()
    
    ## 
    start_times <- unname(c(0, sapply(ageing_file_split, function(x) unique(x$TimeStart))))
    partial_charging_intervals <- parallel::mclapply(seq_along(ageing_file_split), function(k) {
        if (trace > 0) {
            if ((k == 1) || (k == length(ageing_file_split)) || ((k %% trace) == 0)) {
                cat("\t\tCharge cycle:", k, "/", length(ageing_file_split), "\n") 
            }
        }
        
        ## Extracting at cycle 'k'
        ageing_file_split_k <- ageing_file_split[[k]]
        
        ## Extracting features from prior usage
        prior_usage_k <- ageing_file |> 
            filter(Time >= start_times[k], Time < start_times[k + 1]) |> 
            mutate(
                ZeroCurrent = abs(Current) < 1e-6
            ) 
        
        rle_prior <- rle(prior_usage_k$ZeroCurrent)
        if (any(rle_prior$values)) {
            rle_start <- c(1, cumsum(rle_prior$lengths))[max(which(rle_prior$values))]
            prior_usage_k <- prior_usage_k[seq(min(rle_start + 1, dim(prior_usage_k)[1]), dim(prior_usage_k)[1]), ]
            
            prior_relaxation_k <- prior_usage_k |> 
                group_by(Cell, Round) |> 
                summarise(
                    Relaxation_Time = sum(ZeroCurrent), 
                    Relaxation_Distance = max(Time) - ifelse(sum(ZeroCurrent) > 0, max(Time[ZeroCurrent]), 0),
                    .groups = "drop"
                )
        } else {
            prior_relaxation_k <- prior_usage_k |> 
                distinct(Cell, Round) |> 
                mutate(Relaxation_Time = 0, Relaxation_Distance = sum(rle_prior$lengths))
        }
        
        prior_usage_k_intermediate <- prior_usage_k |>
            group_by(Cell, Round) |> 
            filter(!ZeroCurrent) |> 
            summarise(
                dplyr::across("Voltage", voltage_feature_functions[-length(voltage_feature_functions)]),
                dplyr::across("Current", current_feature_functions[-length(voltage_feature_functions)]),
                AH = sum(abs(Current) / 3600),
                .groups = "drop"
            ) 
        
        if (dim(prior_usage_k |> filter(!ZeroCurrent))[1] == 0) {
            prior_usage_k <- (prior_usage_k |> select(Cell, Round))[1, ] |> 
                left_join(prior_usage_k_intermediate, by = c("Cell", "Round"))
        } else {
            prior_usage_k <- prior_usage_k_intermediate
        }
        
        colnames(prior_usage_k)[-c(1, 2)] <- paste0("DC_", colnames(prior_usage_k)[-c(1, 2)])
        prior_usage_k <- prior_usage_k |> left_join(prior_relaxation_k, by = c("Cell", "Round"))
        
        ## Determine largest window within the interval [min_start_voltage, max_end_voltage]
        ageing_file_split_k_start <- which(ageing_file_split_k$Voltage >= min_start_voltage)[1]
        ageing_file_split_k_end <- rev(which(ageing_file_split_k$Voltage <= max_end_voltage))[1]
        
        ageing_file_split_k_total <- ageing_file_split_k[seq(ageing_file_split_k_start, ageing_file_split_k_end), ]
        
        ## Partial charges
        res_k <- ageing_file_split_k_total[dim(ageing_file_split_k_total)[1], ] |> 
            mutate(
                DeltaTime = Time - ageing_file_split_k_total$Time[1],
                StartVoltage = ageing_file_split_k_total$Voltage[1], 
                EndVoltage = Voltage,
                DeltaVoltage = EndVoltage - StartVoltage, 
                SOHW = AH - ageing_file_split_k_total$AH[1], 
                minVoltage = min(ageing_file_split_k$Voltage), 
                maxVoltage = max(ageing_file_split_k$Voltage),
                SOHW = max(ageing_file_split_k$AH)
            ) |> 
            dplyr::select(Cell, Round, ChargeCycle, StartTime = TimeStart, DeltaTime, StartVoltage, EndVoltage, DeltaVoltage, minVoltage, maxVoltage, SOHW, SOH, SOH0, D, D0)
        
        ## Descriptive features
        desc_k <- ageing_file_split_k_total |> 
            summarise(
                FEC = FEC[1],
                dplyr::across("Voltage", voltage_feature_functions),
                dplyr::across("Current", current_feature_functions),
                Temperature = mean(Temperature),
                .groups = "drop"
            )
        
        res_k <- res_k |> bind_cols(desc_k)
        
        ## Transient
        tran_k <- ageing_file_split_k_total |> 
            summarise(
                Ri = (Voltage[n()] - Voltage[1]) / Current[1],
                .groups = "drop"
            )
        
        res_k <- res_k |> bind_cols(tran_k)
        
        ## Previous usage in charge-cycle
        if ((ageing_file_split_k_start - 1) > 0) {
            ignored_partial_k <- ageing_file_split_k[seq(1, ageing_file_split_k_start - 1), ] |> 
                summarise(
                    C_AH = AH[n()] - AH[1],
                    C_Delta_Time = Time[n()] - Time[1],
                    C_Start_Voltage = Voltage[1],
                    C_Delta_Voltage = Voltage[n()] - Voltage[1],
                    .groups = "drop"
                )
            
            ignored_desc_k <- ageing_file_split_k[seq(1, ageing_file_split_k_start - 1), ] |> 
                summarise(
                    dplyr::across("Voltage", voltage_feature_functions),
                    dplyr::across("Current", current_feature_functions),
                    Temperature = mean(Temperature),
                    .groups = "drop"
                )
            
            colnames(ignored_desc_k) <- paste0("C_", colnames(ignored_desc_k))
            
            ignored_k <- ignored_partial_k |> bind_cols(ignored_desc_k)
            res_k <- res_k |> bind_cols(ignored_k)
        }
        
        ##
        res_k <- res_k |> 
            select(
                Cell:maxVoltage, 
                FEC,
                starts_with("Current"), 
                starts_with("Voltage"), 
                Temperature,
                starts_with("C_"),
                Ri,
                SOHW, 
                SOH, 
                SOH0,
                D, 
                D0
            )
        
        if (dim(res_k)[1] > 0) {
            res_k <- res_k |> 
                left_join(prior_usage_k, by = c("Cell", "Round")) |> 
                select(
                    Cell:maxVoltage, 
                    FEC, 
                    starts_with("Current"), 
                    starts_with("Voltage"), 
                    Temperature,
                    starts_with("C_"),
                    Ri,
                    starts_with("DC_"),
                    Relaxation_Time, 
                    Relaxation_Distance,
                    SOHW, 
                    SOH, 
                    SOH0,
                    D, 
                    D0
                ) 
        } else {
            res_k <- NULL
        }
        
        return(res_k)
    }, mc.cores = nr_cores)
    
    partial_charging_intervals <- partial_charging_intervals |> bind_rows()
    return(partial_charging_intervals)
}

