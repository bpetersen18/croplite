calc_gdd <- function(tmax, tmin, tbase = 6){

  if (any(tmin > tmax)){
    stop("Error: Minimum temperature must be less than maximum temperature")
  }

  gdd <- ((tmax + tmin) / 2) - tbase

  gdd[gdd < 0] <- 0

  return(gdd)
}

calc_epsilon_i <- function(lai, k = 0.68){

  epsilon_i <- 1 - exp(-k * lai)

  if (any(epsilon_i > 1) | any(epsilon_i < 0)){
    stop("Error: Light interception efficiency must be between 0 and 1")
  }

  return(epsilon_i)
}

calc_start_gs <- function(date, tmin, t_threshold = -2, spring_months = 1:5){

  if (length(spring_months) == 0){
    stop("Error: spring_months must be a vector of month numbers")
  }

  if (length(date) != length(tmin)){
    stop("Error: date and tmin must be the same length")
  }

  start_gs <- data.frame(date = date, tmin = tmin) %>%
    dplyr::mutate(month = lubridate::month(date)) %>%
    dplyr::filter(month %in% spring_months) %>%
    dplyr::filter(tmin <= t_threshold) %>%
    dplyr::filter(date == max(date)) %>%
    dplyr::pull(date)

  return(start_gs)
}

calc_end_gs <- function(date, tmin, t_threshold = -2, fall_months = 8:12){

  if (length(fall_months) == 0){
    stop("Error: fall_months must be a vector of month numbers")
  }

  if (length(date) != length(tmin)){
    stop("Error: date and tmin must be the same length")
  }

  end_gs <- data.frame(date = date, tmin = tmin) %>%
    dplyr::mutate(month = lubridate::month(date)) %>%
    dplyr::filter(month %in% fall_months) %>%
    dplyr::filter(tmin <= t_threshold) %>%
    dplyr::filter(date == min(date)) %>%
    dplyr::pull(date)

  return(end_gs)
}

calc_lai <- function(accumulated_gdd, x = 0.006, max_lai = 5){

  lai <- x * accumulated_gdd

  lai[lai > max_lai] <- max_lai

  return(lai)
}

calc_growth_rate <- function(lai, par, rue = 1.91){

  epsilon_i <- calc_epsilon_i(lai)

  growth_rate <- rue * epsilon_i * par

  return(growth_rate)
}

model_yield_potential <- function(date, par, tmin, tmax, params = list(x = 0.006,
                                                              max_lai = 5,
                                                              k = 0.68,
                                                              tbase = 6,
                                                              t_threshold = -2,
                                                              spring_months = 1:5,
                                                              fall_months = 8:12,
                                                              rue = 1.91)){

  start_gs <- calc_start_gs(date, tmin, params$t_threshold, params$spring_months)
  end_gs <- calc_end_gs(date, tmin, params$t_threshold, params$fall_months)

  growing_season <- data.frame(date = date, tmin = tmin, tmax = tmax, par = par) %>%
    dplyr::filter(date >= start_gs & date <= end_gs) %>%
    dplyr::mutate(gdd = calc_gdd(tmax, tmin, params$tbase),
           accumulated_gdd = cumsum(gdd),
           lai = calc_lai(accumulated_gdd, params$x, params$max_lai),
           growth_rate = calc_growth_rate(lai, par, params$rue)) %>%
    dplyr::select(date, gdd, accumulated_gdd, lai, growth_rate)

  return(growing_season)

}





