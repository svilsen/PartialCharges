#' @title Capacity extraction
#' 
#' @description Function extracting the capacity of a reference performance test.
#' 
#' @param rpt A \link[tibble]{tibble} (or \link{data.frame}) containing the RPT current and voltage measurements.
#' @param L The minimum (dis)charging time required for it to count as a reference measurement.
#' 
#' @return A \link[tibble]{tibble} containing the capacity measurements.
capacity_extract <- function(rpt, L = 3000) {
    #
    dch <- rle(rpt$Current < 0)
    cap <- rpt |> 
        mutate(
            Group = rep(1:length(dch$lengths), times = dch$lengths),
        )  |> 
        filter(
            abs(Current) > 0
        ) %>% 
        group_by(Cell, Round, Group) |>  
        mutate(
            N = n()
        ) |>  
        filter(
            N > L
        ) |> 
        summarise(
            N = mean(N),
            Current = mean(Current),
            Capacity = sum(diff(Time) * Current) / 3600,
            .groups = "drop"
        ) |> 
        filter(Current > 0) |> 
        group_by(Cell, Round) |> 
        summarise(
            Capacity = mean(Capacity), 
            .groups = "drop"
        ) 
        
    return(cap)
}

#' @title Charge curve extraction 
#' 
#' @description Function extracting the charge curve of a reference performance test.
#' 
#' @param rpt A \link[tibble]{tibble} (or \link{data.frame}) containing the RPT current and voltage measurements.
#' @param L The minimum (dis)charging time required for it to count as a reference measurement.
#' 
#' @return A \link[tibble]{tibble} containing the charge curves.
charge_curve_extract <- function(rpt, L = 10000) {
    dch <- rle(rpt$Current < 0)
    charge_curve <- rpt |> 
        mutate(
            Group = rep(1:length(dch$lengths), times = dch$lengths),
        )  |> 
        filter(
            Current > 0
        ) %>% 
        group_by(Cell, Round, Group) |>  
        mutate(
            N = n()
        ) |>  
        filter(
            N > L
        ) |> 
        ungroup() |> 
        filter(
            Group == min(Group)
        )
    
    return(charge_curve)
}
