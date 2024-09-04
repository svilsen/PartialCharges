#### Creating figures of raw data ----
if (make_figures) {
    ## Degradation
    partial_charging_intervals <- readRDS(file = paste0(files_dir, "/partial_charging_intervals.Rds"))
    
    degradation_plots <- partial_charging_intervals |> 
        group_by(Profile, Cell, Round) |> 
        summarise(FEC = max(FEC), D0 = min(D0), .groups = "drop") |> 
        bind_rows(tibble(Profile = c(rep("Forklifts", 3), rep("WLTC", 2)), Cell = c(seq(1, 3), seq(1, 2)), Round = 0, FEC = 0, D0 = 0))
    
    fec_lim <- 50 * ceiling(c(min(degradation_plots$FEC), max(degradation_plots$FEC)) / 50)
    deg_lim <- ceiling(100 * c(min(degradation_plots$D0), max(degradation_plots$D0)))
    
    # Forklift
    pdf("Figures/forklifts_degradation.pdf", width = 8, height = 5)
    degradation_plots |> 
        filter(Profile == "Forklifts") |> 
        mutate(
            Temperature = case_when(Cell == 1 ~ 45, Cell == 2 ~ 40, Cell == 3 ~ 35),
            Cell = factor(Cell), 
            TC = paste0("Cell: ", Cell, ", Temperature: ", Temperature)
        ) |> 
        ggplot(aes(x = FEC, y = 100 * D0, colour = TC, group = TC)) + 
        geom_point(size = 2) + 
        geom_line(linewidth = 1) + 
        labs(x = "FEC", y = "Capacity fade [%]", colour = "") + 
        lims(x = fec_lim, y = deg_lim) + 
        theme_bw(base_size = 25) + 
        theme(legend.position = c(0.7, 0.3))
    dev.off()
    
    # WLTC
    pdf("Figures/wltc_degradation.pdf", width = 8, height = 5)
    degradation_plots |> 
        filter(Profile == "WLTC") |> 
        mutate(
            Temperature = 35,
            Cell = factor(Cell), 
            TC = paste0("Cell: ", Cell, ", Temperature: ", Temperature)
        ) |> 
        ggplot(aes(x = FEC, y = 100 * D0, colour = TC, group = TC)) + 
        geom_point(size = 2) + 
        geom_line(linewidth = 1) + 
        labs(x = "FEC", y = "Capacity fade [%]", colour = "") + 
        lims(x = fec_lim, y = deg_lim) + 
        theme_bw(base_size = 25) + 
        theme(legend.position = c(0.3, 0.8))
    dev.off()
    
    ## Current / voltage
    # Forklifts
    forklift_plots <- read_csv(paste0(data_dir, "/Forklifts/Cell1/Round01/Ageing.csv"), show_col_types = FALSE, progress = FALSE) |> 
        filter(Part == 1, Time > 5600, Time < 51300)
    
    forklift_plots <- correct_time(
        time = forklift_plots$Time,
        current = forklift_plots$Current, 
        voltage = forklift_plots$Voltage,
        temperature = forklift_plots$Temperature,
        fec = forklift_plots$Current
    ) |> 
        structure(.Dimnames = list(NULL, c("Time", "Current", "Voltage", "Temperature", "FEC"))) |> 
        as_tibble()
    
    pdf("Figures/forklifts_current.pdf", width = 8, height = 5)
    forklift_plots |> 
        ggplot(aes(x = Time, y = Current)) + 
        geom_line() + 
        labs(x = "Time [s]", y = "Current [A]") + 
        theme_bw(base_size = 25) + 
        theme(legend.position = "none")
    dev.off()
    
    pdf("Figures/forklifts_voltage.pdf", width = 8, height = 5)
    forklift_plots |> 
        ggplot(aes(x = Time, y = Voltage)) + 
        geom_line() + 
        labs(x = "Time [s]", y = "Voltage [V]") + 
        theme_bw(base_size = 25) + 
        theme(legend.position = "none")
    dev.off()
    
    # WLTC
    wltc_plots <- read_csv(paste0(data_dir, "/WLTC/Cell1/Round01/Ageing.csv"), show_col_types = FALSE, progress = FALSE) |> 
        filter(Part == 1, Time > 10000, Time < 55000)
    
    pdf("Figures/wltc_current.pdf", width = 8, height = 5)
    wltc_plots |> 
        ggplot(aes(x = Time, y = Current)) + 
        geom_line() + 
        labs(x = "Time [s]", y = "Current [A]") + 
        theme_bw(base_size = 25) + 
        theme(legend.position = "none")
    dev.off()
    
    pdf("Figures/wltc_voltage.pdf", width = 8, height = 5)
    wltc_plots |> 
        ggplot(aes(x = Time, y = Voltage)) + 
        geom_line() + 
        labs(x = "Time [s]", y = "Voltage [V]") + 
        theme_bw(base_size = 25) + 
        theme(legend.position = "none")
    dev.off()
    
    ## Charge curves / degradation
    rounds_keep <- sort((degradation_plots |> filter(Profile == "Forklifts", Cell == 1))$Round)
    charge <- readRDS("Files/forklifts_charge.Rds") |> 
        filter(Round %in% rounds_keep) |> 
        filter(Voltage < 3.651)
    
    pdf("Figures/forklifts_charge_curve.pdf", width = 8, height = 5)
    charge |> 
        filter(Round != 40) |> 
        ggplot(aes(x = Time, y = Voltage, colour = Round, group = Round)) + 
        geom_line(linewidth = 0.8) + 
        scale_colour_viridis_c() + 
        theme_bw(base_size = 25) +
        labs(x = "Time [s]", y = "Voltage [V]", colour = "") + 
        guides(colour = guide_colorbar(barwidth = 15)) + 
        theme(legend.position = "top")
    dev.off()
    
    pdf("Figures/forklifts_charge_curve_zoom.pdf", width = 8, height = 5)
    charge |> 
        filter(Round != 40) |> 
        filter(Voltage > 3.5, Voltage < 3.65) |> 
        ggplot(aes(x = Time, y = Voltage, colour = Round, group = Round)) + 
        geom_line(linewidth = 0.8) + 
        scale_colour_viridis_c() + 
        theme_bw(base_size = 25) +
        labs(x = "Time [s]", y = "Voltage [V]", colour = "") + 
        guides(colour = guide_colorbar(barwidth = 15)) + 
        theme(legend.position = "top")
    dev.off()
}

#### Creating figures of features ----
if (make_figures) {
    partial_charging_std <- readRDS(paste0(files_dir, "/partial_charging_intervals_training_random.Rds"))
    
    c_within_vi_order <- c(
        "AH", "Current_Average", "Current_SD", "Current_Skewness", "Current_Kurtosis", "Current_MAD",  
        "StartVoltage", "DeltaVoltage", "Voltage_Average", "Voltage_SD", "Voltage_Skewness", "Voltage_Kurtosis", "Voltage_MAD", "Voltage_MaxDelta", "Voltage_FE", 
        "Temperature",
        "D0"
    )
    
    c_before_vi_order <- c(
        "C_Delta_Time", "C_AH", "C_Current_Average", "C_Current_SD", "C_Current_Skewness", "C_Current_Kurtosis", "C_Current_MAD", 
        "C_Start_Voltage", "C_Delta_Voltage", "C_Voltage_Average", "C_Voltage_SD", "C_Voltage_Skewness", "C_Voltage_Kurtosis", "C_Voltage_MAD", "C_Voltage_MaxDelta", 
        "D0"
    )
    
    dc_order <- c(
        "DC_AH", "DC_Current_Average", "DC_Current_SD", "DC_Current_Skewness", "DC_Current_Kurtosis", "DC_Current_MAD", "DC_Current_MaxDelta", 
        "DC_Voltage_Average", "DC_Voltage_SD", "DC_Voltage_Skewness", "DC_Voltage_Kurtosis", "DC_Voltage_MAD", "DC_Voltage_MaxDelta", 
        "D0" 
    )
    
    #### Correlation plots ----
    # Features extracted during charging, but within the voltage interval
    correlation_c_within_vi <- partial_charging_std |> 
        filter(Data == "Training") |> 
        select(AH = FEC, StartVoltage, DeltaVoltage, starts_with("Voltage_"), starts_with("Current_"), Temperature, D0) |> 
        cor(method = "spearman")

    correlation_c_within_vi <- correlation_c_within_vi[c_within_vi_order, c_within_vi_order]
    
    colnames(correlation_c_within_vi) <- str_replace_all(colnames(correlation_c_within_vi), "_", " ") |> 
        str_replace_all("MaxDelta", "Largest difference") |> 
        str_replace_all("FE", "Fuzzy entropy") |> 
        str_replace_all("AH", "FEC") |> 
        str_replace_all("D0", "Capacity fade") 
    
    rownames(correlation_c_within_vi) <- str_replace_all(rownames(correlation_c_within_vi), "_", " ") |> 
        str_replace_all("MaxDelta", "Largest difference") |> 
        str_replace_all("FE", "Fuzzy entropy") |> 
        str_replace_all("AH", "FEC") |> 
        str_replace_all("D0", "Capacity fade") 
    
    pdf("Figures/correlation_c_within_vi.pdf", height = 10, width = 10)
    correlation_c_within_vi |> 
        ggcorrplot(type = "lower", show.diag = TRUE, ggtheme = theme_minimal(base_size = 12), outline.col = "white", lab = TRUE, digits = 2) + 
        scale_fill_viridis_c(name = "Spearman\ncorrelation", limit = c(-1.0, 1.0)) + 
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5), legend.position = "right", legend.text = element_text(size = 11), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    dev.off()
    
    # Features extracted during charging, but before the voltage interval
    correlation_c_before_vi <- partial_charging_std |> 
        filter(Data == "Training") |> 
        select(starts_with("C_"), D0) |> 
        cor(method = "spearman")
    
    correlation_c_before_vi <- correlation_c_before_vi[c_before_vi_order, c_before_vi_order]
    
    colnames(correlation_c_before_vi) <- str_replace_all(colnames(correlation_c_before_vi), "_", " ") |> 
        str_replace_all("MaxDelta", "Largest difference") |> 
        str_replace_all("FE", "Fuzzy entropy") |> 
        str_replace_all("D0", "Capacity fade") 
    
    rownames(correlation_c_before_vi) <- str_replace_all(rownames(correlation_c_before_vi), "_", " ") |> 
        str_replace_all("MaxDelta", "Largest difference") |> 
        str_replace_all("FE", "Fuzzy entropy") |> 
        str_replace_all("D0", "Capacity fade") 
    
    pdf("Figures/correlation_c_before_vi.pdf", height = 10, width = 10)
    correlation_c_before_vi |> 
        ggcorrplot(type = "lower", show.diag = TRUE, ggtheme = theme_minimal(base_size = 12), outline.col = "white", lab = TRUE, digits = 2) + 
        scale_fill_viridis_c(name = "Spearman\ncorrelation", limit = c(-1.0, 1.0)) + 
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5), legend.position = "right", legend.text = element_text(size = 11), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    dev.off()
    
    # Features extracted during discharging
    correlation_dc <- partial_charging_std |> 
        filter(Data == "Training") |> 
        select(starts_with("DC_"), D0) |>
        cor(method = "spearman")
    
    correlation_dc <- correlation_dc[dc_order, dc_order]
    
    colnames(correlation_dc) <- str_replace_all(colnames(correlation_dc), "_", " ") |> 
        str_replace_all("MaxDelta", "Largest difference") |> 
        str_replace_all("FE", "Fuzzy entropy") |> 
        str_replace_all("D0", "Capacity fade") 
    
    rownames(correlation_dc) <- str_replace_all(rownames(correlation_dc), "_", " ") |> 
        str_replace_all("MaxDelta", "Largest difference") |> 
        str_replace_all("FE", "Fuzzy entropy") |> 
        str_replace_all("D0", "Capacity fade") 
    
    pdf("Figures/correlation_dc.pdf", height = 10, width = 10)
    correlation_dc |> 
        ggcorrplot(type = "lower", show.diag = TRUE, ggtheme = theme_minimal(base_size = 12), outline.col = "white", lab = TRUE, digits = 2) + 
        scale_fill_viridis_c(name = "Spearman\ncorrelation", limit = c(-1.0, 1.0)) + 
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5), legend.position = "right", legend.text = element_text(size = 11), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    dev.off()
    
    #### MIC plots ----
    # Features extracted during charging, but within the voltage interval
    mine_c_within_vi <- partial_charging_std |> 
        filter(Data == "Training") |> 
        select(AH = FEC, StartVoltage, DeltaVoltage, starts_with("Voltage_"), starts_with("Current_"), Temperature, D0) |> 
        mine(n.cores = 7) |> 
        (\(x) x$MIC)()
    
    mine_c_within_vi <- mine_c_within_vi[c_within_vi_order, c_within_vi_order]
    
    colnames(mine_c_within_vi) <- str_replace_all(colnames(mine_c_within_vi), "_", " ") |> 
        str_replace_all("MaxDelta", "Largest difference") |> 
        str_replace_all("FE", "Fuzzy entropy") |> 
        str_replace_all("AH", "FEC") |> 
        str_replace_all("D0", "Capacity fade") 
    
    rownames(mine_c_within_vi) <- str_replace_all(rownames(mine_c_within_vi), "_", " ") |> 
        str_replace_all("MaxDelta", "Largest difference") |> 
        str_replace_all("FE", "Fuzzy entropy") |> 
        str_replace_all("AH", "FEC") |> 
        str_replace_all("D0", "Capacity fade") 
    
    pdf("Figures/mic_c_within_vi.pdf", height = 10, width = 10)
    mine_c_within_vi |> 
        ggcorrplot(type = "upper", show.diag = TRUE, ggtheme = theme_minimal(base_size = 12), outline.col = "white", lab = TRUE, digits = 2) + 
        scale_fill_gradient2(name = "MIC", limit = c(0.0, 1.0), low = "blue", high =  "red", mid = "white", midpoint = 0.0) + 
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5), legend.position = "right", legend.text = element_text(size = 11), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    dev.off()
    
    # Features extracted during charging, but before the voltage interval
    mine_c_before_vi <- partial_charging_std |> 
        filter(Data == "Training") |> 
        select(starts_with("C_"), D0) |> 
        mine(n.cores = 7) |> 
        (\(x) x$MIC)()
    
    mine_c_before_vi <- mine_c_before_vi[c_before_vi_order, c_before_vi_order]
    
    colnames(mine_c_before_vi) <- str_replace_all(colnames(mine_c_before_vi), "_", " ") |> 
        str_replace_all("MaxDelta", "Largest difference") |> 
        str_replace_all("FE", "Fuzzy entropy") |> 
        str_replace_all("D0", "Capacity fade") 
    
    rownames(mine_c_before_vi) <- str_replace_all(rownames(mine_c_before_vi), "_", " ") |> 
        str_replace_all("MaxDelta", "Largest difference") |> 
        str_replace_all("FE", "Fuzzy entropy") |> 
        str_replace_all("D0", "Capacity fade") 
    
    pdf("Figures/mic_c_before_vi.pdf", height = 10, width = 10)
    mine_c_before_vi |> 
        ggcorrplot(type = "upper", show.diag = TRUE, ggtheme = theme_minimal(base_size = 12), outline.col = "white", lab = TRUE, digits = 2) + 
        scale_fill_gradient2(name = "MIC", limit = c(0.0, 1.0), low = "blue", high =  "red", mid = "white", midpoint = 0.0) + 
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5), legend.position = "right", legend.text = element_text(size = 11), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    dev.off()
    
    # Features extracted during discharging
    mine_dc <- partial_charging_std |> 
        filter(Data == "Training") |> 
        select(starts_with("DC_"), D0) |> 
        mine(n.cores = 7) |> 
        (\(x) x$MIC)()
    
    mine_dc <- mine_dc[dc_order, dc_order]
    
    colnames(mine_dc) <- str_replace_all(colnames(mine_dc), "_", " ") |> 
        str_replace_all("MaxDelta", "Largest difference") |> 
        str_replace_all("FE", "Fuzzy entropy") |> 
        str_replace_all("D0", "Capacity fade") 
    
    rownames(mine_dc) <- str_replace_all(rownames(mine_dc), "_", " ") |> 
        str_replace_all("MaxDelta", "Largest difference") |> 
        str_replace_all("FE", "Fuzzy entropy") |> 
        str_replace_all("D0", "Capacity fade") 
    
    pdf("Figures/mic_dc.pdf", height = 10, width = 10)
    mine_dc |> 
        ggcorrplot(type = "upper", show.diag = TRUE, ggtheme = theme_minimal(base_size = 12), outline.col = "white", lab = TRUE, digits = 2) + 
        scale_fill_gradient2(name = "MIC", limit = c(0.0, 1.0), low = "blue", high =  "red", mid = "white", midpoint = 0.0) + 
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5), legend.position = "right", legend.text = element_text(size = 11), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    dev.off()
    
}

#### Creating figures of model predictions ----
if (make_figures) {
    #### ----
    prior_levels <- c("Without prior information", "Without prior charge information", "Without prior discharge information", "With prior information")
    type_levels <- c("Average weights", "Linear weights", "Exponential weights")
    
    mlr_error_tibble <- readRDS("Files/mlr_model_tibble.Rds") |> 
        mutate(TYPE = factor(TYPE, levels = type_levels), Prior = factor(Prior, levels = prior_levels))
    rf_error_tibble <- readRDS("Files/rf_model_tibble.Rds")  |> 
        mutate(TYPE = factor(TYPE, levels = type_levels), Prior = factor(Prior, levels = prior_levels))
    svr_error_tibble <- readRDS("Files/svr_model_tibble.Rds")  |> 
        mutate(TYPE = factor(TYPE, levels = type_levels), Prior = factor(Prior, levels = prior_levels))
    nn_error_tibble <- readRDS("Files/nn_model_tibble.Rds")  |> 
        mutate(TYPE = factor(TYPE, levels = type_levels), Prior = factor(Prior, levels = prior_levels))
    
    total_error_tibble <- mlr_error_tibble |> 
        bind_rows(rf_error_tibble) |> 
        bind_rows(svr_error_tibble) |> 
        bind_rows(nn_error_tibble) |> 
        mutate(
            Model = factor(Model, levels = c("MLR", "SVR", "RF", "NN")),
            Temperature = case_when(
                Profile == "Forklifts" & Cell == 1 ~ 45, 
                Profile == "Forklifts" & Cell == 2 ~ 40, 
                Profile == "Forklifts" & Cell == 3 ~ 35, 
                Profile == "WLTC" & Cell == 1 ~ 35, 
                Profile == "WLTC" & Cell == 2 ~ 35, 
            )
        )
    
    
    avg_error_tibble <- total_error_tibble |> 
        filter(TYPE == "Average weights") |> 
        mutate(PC = paste0(Profile, ", Cell: ", Cell, ", T: ", Temperature))
    
    avg_error_tibble_colors <- c(
        "Forklifts, Cell: 1, T: 45" = "#f8766d", "Forklifts, Cell: 2, T: 40" = "#00ba38", "Forklifts, Cell: 3, T: 35" = "#619cff", "WLTC, Cell: 1, T: 35" = "#f48a1a", "WLTC, Cell: 2, T: 35" = "#c400be"
    )
    
    pdf("Figures/comparing_methods.pdf", height = 12, width = 20)
    avg_error_tibble |> 
        ggplot(aes(x = FEC)) + 
        geom_line(aes(y = 100 * DHAT, group = PC), colour = "black", linewidth = 2) +
        geom_line(aes(y = 100 * DHAT, colour = PC), linewidth = 0.8) +
        geom_point(aes(y = 100 * D, shape = Data, colour = PC), colour = "black", size = 3) + 
        geom_point(aes(y = 100 * D, shape = Data, colour = PC), size = 2) + 
        geom_label(
            data = avg_error_tibble |> 
                group_by(Prior, Model) |> 
                filter(Data == "Validation", !is.nan(DHAT), !is.na(DHAT)) |> 
                summarise(
                    MAE = paste(round(100 * mean(abs((DHAT - D))), 2)),
                    MAE = paste("MAE:", ifelse(nchar(MAE) < 4, paste0(MAE, "0"), MAE), "[%]"), 
                    .groups = "drop"
                ) |> 
                mutate(Prior = factor(Prior, levels = prior_levels)),
            aes(x = 1500, y = 100*0.025, label = MAE),
            label.padding = unit(0.5, "lines"),
            label.size = 1,
            size = 5
        ) + 
        geom_hline(yintercept = 100 * 0.2, linetype = "dashed") + 
        annotate("text", x = 2, y = 100 * 0.21, label = "EOL") + 
        facet_grid(Model ~ Prior) + 
        scale_shape_manual(values = c(16, 17)) + 
        scale_colour_manual(values = avg_error_tibble_colors) + 
        labs(x = "FEC", y = "Capacity fade [%]", colour = "") + 
        lims(y = c(0.0, 25)) + 
        theme_bw(base_size = 20) + 
        theme(legend.position = "top")
    dev.off()
    
    pdf("Figures/comparing_methods_one_to_one.pdf", height = 12, width = 20)
    avg_error_tibble |> 
        ggplot(aes(x = 100*D)) + 
        geom_point(aes(y = 100*DHAT, shape = Data, colour = PC), colour = "black", size = 3) + 
        geom_point(aes(y = 100*DHAT, shape = Data, colour = PC), size = 2) + 
        geom_abline(linetype = "dashed") +
        geom_label(
            data = avg_error_tibble |> 
                group_by(Prior, Model) |> 
                filter(Data == "Validation", !is.nan(DHAT), !is.na(DHAT)) |> 
                summarise(
                    MAE = paste(round(100 * mean(abs((DHAT - D))), 2)),
                    MAE = paste("MAE:", ifelse(nchar(MAE) < 4, paste0(MAE, "0"), MAE), "[%]"), 
                    .groups = "drop"
                ) |> 
                mutate(Prior = factor(Prior, levels = prior_levels)),
            aes(x = 100*0.18, y = 100*0.035, label = MAE),
            label.padding = unit(0.5, "lines"),
            label.size = 1,
            size = 5
        ) + 
        facet_grid(Model ~ Prior) + 
        scale_shape_manual(values = c(16, 17)) + 
        scale_colour_manual(values = avg_error_tibble_colors) + 
        labs(x = "Measured capacity fade [%]", y = "Predicted capacity fade [%]", colour = "") + 
        theme_bw(base_size = 20) + 
        theme(legend.position = "top")
    dev.off()
    
    pdf("Figures/comparing_methods_difference.pdf", height = 12, width = 20)
    avg_error_tibble |> 
        ggplot(aes(x = 100 * D)) + 
        geom_point(aes(y = 100 * (D - DHAT), shape = Data, colour = PC), colour = "black", size = 3) + 
        geom_point(aes(y = 100 * (D - DHAT), shape = Data, colour = PC), size = 2) + 
        geom_hline(yintercept = 0, linetype = "dashed") +
        geom_label(
            data = avg_error_tibble |> 
                group_by(Prior, Model) |> 
                filter(Data == "Validation", !is.nan(DHAT), !is.na(DHAT)) |> 
                summarise(
                    MAE = paste(round(100 * mean(abs((DHAT - D))), 2)),
                    MAE = paste("MAE:", ifelse(nchar(MAE) < 4, paste0(MAE, "0"), MAE), "[%]"), 
                    .groups = "drop"
                ) |> 
                mutate(Prior = factor(Prior, levels = prior_levels)),
            aes(x = 100 * 0.20, y = -100 * 0.025, label = MAE),
            label.padding = unit(0.5, "lines"),
            label.size = 1,
            size = 5
        ) + 
        facet_grid(Model ~ Prior) + 
        scale_shape_manual(values = c(16, 17)) + 
        scale_colour_manual(values = avg_error_tibble_colors) + 
        labs(x = "Measured capacity fade [%]", y = "Prediction error [%]", colour = "") + 
        lims(x = c(0, 25), y = c(-4.5, 2.5)) + 
        theme_bw(base_size = 20) + 
        theme(legend.position = "top")
    dev.off()
    
    pdf("Figures/comparing_methods_density.pdf", height = 12, width = 20)
    total_error_tibble |> 
        mutate(Error = 100 * (D - DHAT)) |> 
        drop_na() |> 
        filter(Data == "Validation") |> 
        ggplot(aes(x = Error)) + 
        geom_density(aes(group = Model, colour = Model), linewidth = 1) + 
        geom_vline(xintercept = 0, linetype = "dashed") + 
        facet_grid(TYPE ~ Prior) + 
        labs(x = "Prediction error [%]", y = "Density", colour = "", fill = "") + 
        lims(x = c(-1.5, 1.5)) + 
        theme_bw(base_size = 20) + 
        theme(legend.position = "top")
    dev.off()
    
    total_error_table <- total_error_tibble |> 
        group_by(Profile, Model, Prior, TYPE) |> 
        filter(Data == "Validation", !is.nan(DHAT), !is.na(DHAT)) |> 
        summarise(
            RMSE = round(100 * sqrt(mean(abs((DHAT - D)^2))), 2), 
            MAE = round(100 * mean(abs((DHAT - D))), 2), 
            .groups = "drop"
        ) |> 
        mutate(Prior = factor(Prior, levels = prior_levels)) |> 
        pivot_wider(names_from = c("TYPE"), values_from = c("RMSE", "MAE")) |> 
        arrange(Profile, Prior, Model) |> 
        select(Profile, Prior, Model, contains("Average"), contains("Linear"), contains("Exponential"))
    
    print(xtable::xtable(total_error_table |> filter(Profile == "Forklifts") |> select(-Profile, -Prior)), include.rownames = FALSE)
    print(xtable::xtable(total_error_table |> filter(Profile == "WLTC") |> select(-Profile, -Prior)), include.rownames = FALSE)
}

#### Creating variable importance tibble for each model ----
if (create_vi) {
    partial_charging_std <- readRDS(paste0(files_dir, "/partial_charging_intervals_training_random.Rds"))
    
    models <- c("MLR", "SVR", "RF", "NN") 
    model_levels <- c("MLR", "SVR", "RF", "NN") 
    
    priors <- c("without_prior_knowledge", "without_prior_charge_knowledge", "without_prior_discharge_knowledge", "with_all_prior_knowledge")
    prior_levels <- c("Without prior information", "Without prior charge information", "Without prior discharge information", "With prior information")
    
    #### ----
    for (i in seq_along(model_levels)) {
        cat("Model:", model_levels[i], paste0("(", i, " / ", length(models), ")"), "\n")
        
        ## Specify model predict function
        if (models[i] == "RF") {
            predict_function <- function(object, newdata) {
                predict(object, data = newdata |> (\(x) model.matrix(~ -1 + .*., data = x))())$predictions
            }
        } else if (models[i] == "SVR") {
            predict_function <- function(object, newdata) {
                predict(object, newdata = newdata |> (\(x) model.matrix(~ -1 + .*., data = x))())[, 1]
            }
        } else if (models[i] == "NN") {
            predict_function <- function(object, newdata) {
                x = newdata |> (\(x) model.matrix(~ -1 + .*., data = x))() |> as.matrix()
                predict(object, x = x, verbose = 0)[, 1]
            }
        } else {
            predict_function <- function(object, newdata) {
                predict(object, newdata = newdata)
            }
        }
        
        variable_importance_i <- vector("list", length(prior_levels)) 
        for (j in seq_along(variable_importance_i)) {
            cat("\tPrior:", prior_levels[j], paste0("(", j, " / ", length(priors), ")"), "\n")
            
            ## Specify feature-set
            if (priors[j] == "without_prior_knowledge") {
                features_ij <- window_capacity_features
            } else if (priors[j] == "without_prior_charge_knowledge") {
                features_ij <- c(window_capacity_features, window_prior_features)
            } else if (priors[j] == "without_prior_discharge_knowledge") {
                features_ij <- c(window_capacity_features, window_prior_capacity_features)
            } else {
                features_ij <- c(window_capacity_features, window_prior_capacity_features, window_prior_features)
            }
            
            train_ij <- partial_charging_std |> 
                filter(Data == "Training") |> 
                select(D0, all_of(features_ij))
            
            ## Load model
            if (models[i] != "NN") {
                model_ij <- readRDS(paste0("Files/Models/", models[i], "/", priors[j], ".Rds"))
            } else {
                model_ij <- load_model_tf(paste0("Files/Models/", models[i], "/", priors[j]))
            }
            
            ## Simulate variable importance by permutation
            vi_ij <- vi(
                model_ij, 
                method = "permute", 
                feature_names = features_ij, 
                target = "D0", 
                metric = "rsq",
                pred_wrapper = predict_function, 
                train = train_ij,
                nsim = 25
            ) 
            
            ##
            variable_importance_i[[j]] <- vi_ij |> 
                mutate(
                    Model = model_levels[i], 
                    Prior = prior_levels[j]
                ) |> 
                select(Model, Prior, Variable, Importance)
        }
        
        variable_importance_i <- variable_importance_i |> bind_rows()
        saveRDS(variable_importance_i, paste0(files_dir, "/variable_importance_", tolower(models[i]), ".Rds"))
        
    }
    
    ##
    variable_importance <- vector("list", length(model_levels)) 
    for (i in seq_along(models)) {
        variable_importance_i <- readRDS(paste0(files_dir, "/variable_importance_", tolower(models[i]), ".Rds"))
        variable_importance[[i]] <- variable_importance_i
    }
    
    variable_importance <- variable_importance |> 
        bind_rows() |> 
        mutate(
            Model = factor(Model, levels = models), 
            Prior = factor(Prior, levels = prior_levels)
        )
    
    saveRDS(variable_importance, paste0(files_dir, "/variable_importance.Rds"))
}

#### Creating figures for variable importance ----
if (make_figures) {
    variable_importance <- readRDS(paste0(files_dir, "/variable_importance.Rds")) |> 
        mutate(
            Variable = str_replace_all(Variable, "_", " "), 
            Variable = factor(Variable, levels = rev(str_replace_all(c(window_capacity_features[c(16, 10:14, 1:9, 17, 15, 18)], window_prior_capacity_features[c(2, 1, 11:15, 3:10)], window_prior_features[c(1, 8:13, 2:7, 14)]), "_", " ")))
        )
    
    #### ----
    pdf("Figures/comparing_methods_vi_nn.pdf", height = 10, width = 15)
    variable_importance |> 
        filter(Model == "NN") |> 
        ggplot(aes(x = Variable, y = Importance)) + 
        geom_col(aes(fill = Importance)) + 
        facet_grid( ~ Prior) + 
        scale_fill_viridis_c() + 
        labs(x = "", y = "Importance score") + 
        lims(y = c(0, 1)) + 
        theme(legend.position = "none", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
        coord_flip()
    dev.off()
    
    pdf("Figures/comparing_methods_vi.pdf", height = 20, width = 15)
    variable_importance |> 
        filter(Model != "NN") |> 
        ggplot(aes(x = Variable, y = Importance)) + 
        geom_col(aes(fill = Importance)) + 
        facet_grid(Model ~ Prior) + 
        scale_fill_viridis_c() + 
        labs(x = "", y = "Importance score") + 
        lims(y = c(0, 1)) + 
        theme(legend.position = "none", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
        coord_flip()
    dev.off()
    
}




