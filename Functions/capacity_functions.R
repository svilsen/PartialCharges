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
