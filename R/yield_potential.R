#' Model yield potential of a crop.
#'
#' The `model_yield_potential()` calculates the yield potential of a crop
#' given a full year's worth of daily weather data.
#'
#' @param date A vector of dates. Must be the same length as `tmin`, `tmax`, and `par`.
#' @param par A vector of daily photosynthetically active radiation (PAR) values in \eqn{\mathrm{MJ \cdot m^{-2} \cdot day^{-1}}}.
#' @param tmin A vector of daily minimum temperature values in \eqn{^\circ \mathrm{C}}.
#' @param tmax A vector of daily maximum temperature values in \eqn{^\circ \mathrm{C}}.
#' @param params A list of parameters that control the model. The list must contain the following elements:
#'  \itemize{
#'  \item{x}{ - The slope of the LAI curve. Default is 0.006.}
#'  \item{max_lai}{ - The maximum LAI value. Default is 5}
#'  \item{k}{ - The extinction coefficient. Default is 0.68.}
#'  \item{tbase}{ - The base temperature for calculating GDD. Default is 6 \eqn{^\circ \mathrm{C}}.}
#'  \item{t_threshold}{ - The temperature threshold for determining the start and end of the growing season. Default is -2 \eqn{^\circ \mathrm{C}}.}
#'  \item{spring_months}{ - A vector of integers representing the months of the year that are considered spring. Default is 1:5.}
#'  \item{fall_months}{ - A vector of integers representing the months of the year that are considered fall. Default is 8:12.}
#'  \item{rue}{ - The radiation use efficiency. Default is 1.91 \eqn{\mathrm{g \cdot MJ^{-1}}}.}
#'  }
#'
#' @returns A data frame with the following columns:
#' \itemize{
#' \item{date}{ - The date of the observation.}
#' \item{gdd}{ - The growing degree days.}
#' \item{accumulated_gdd}{ - The accumulated growing degree days.}
#' \item{lai}{ - The leaf area index.}
#' \item{growth_rate}{ - The growth rate.}
#' }
#'
#' @examples
#'
#' @importFrom dplyr filter mutate select
#' @export
model_yield_potential <- function(date, par, tmin, tmax, params = list(x = 0.006,
                                                                       max_lai = 5,
                                                                       k = 0.68,
                                                                       tbase = 6,
                                                                       t_threshold = -2,
                                                                       spring_months = 1:5,
                                                                       fall_months = 8:12,
                                                                       rue = 1.91)){

  start_gs <- croplite::calc_start_gs(date, tmin, params$t_threshold, params$spring_months)
  end_gs <- croplite::calc_end_gs(date, tmin, params$t_threshold, params$fall_months)

  growing_season <- data.frame(date = date, tmin = tmin, tmax = tmax, par = par) |>
    dplyr::filter(date >= start_gs & date <= end_gs) |>
    dplyr::mutate(gdd = croplite::calc_gdd(tmax, tmin, params$tbase),
                  accumulated_gdd = cumsum(gdd),
                  lai = croplite::calc_lai(accumulated_gdd, params$x, params$max_lai),
                  growth_rate = croplite::calc_growth_rate(lai, par, params$rue)) |>
    dplyr::select(date, gdd, accumulated_gdd, lai, growth_rate)

  return(growing_season)
}

#' @noRd
calc_gdd <- function(tmax, tmin, tbase = 6){

  if (any(tmin > tmax)){
    stop("Error: Minimum temperature must be less than maximum temperature")
  }

  gdd <- ((tmax + tmin) / 2) - tbase

  gdd[gdd < 0] <- 0

  return(gdd)
}

#' @noRd
calc_epsilon_i <- function(lai, k = 0.68){

  epsilon_i <- 1 - exp(-k * lai)

  if (any(epsilon_i > 1) | any(epsilon_i < 0)){
    stop("Error: Light interception efficiency must be between 0 and 1")
  }

  return(epsilon_i)
}

#' @noRd
#' @importFrom dplyr mutate filter pull
calc_start_gs <- function(date, tmin, t_threshold = -2, spring_months = 1:5){

  if (length(spring_months) == 0){
    stop("Error: spring_months must be a vector of month numbers")
  }

  if (length(date) != length(tmin)){
    stop("Error: date and tmin must be the same length")
  }

  start_gs <- data.frame(date = date, tmin = tmin) |>
    dplyr::mutate(month = lubridate::month(date)) |>
    dplyr::filter(month %in% spring_months) |>
    dplyr::filter(tmin <= t_threshold) |>
    dplyr::filter(date == max(date)) |>
    dplyr::pull(date)

  return(start_gs)
}

#' @noRd
#' @importFrom dplyr mutate filter pull
calc_end_gs <- function(date, tmin, t_threshold = -2, fall_months = 8:12){

  if (length(fall_months) == 0){
    stop("Error: fall_months must be a vector of month numbers")
  }

  if (length(date) != length(tmin)){
    stop("Error: date and tmin must be the same length")
  }

  end_gs <- data.frame(date = date, tmin = tmin) |>
    dplyr::mutate(month = lubridate::month(date)) |>
    dplyr::filter(month %in% fall_months) |>
    dplyr::filter(tmin <= t_threshold) |>
    dplyr::filter(date == min(date)) |>
    dplyr::pull(date)

  return(end_gs)
}

#' @noRd
calc_lai <- function(accumulated_gdd, x = 0.006, max_lai = 5){

  lai <- x * accumulated_gdd

  lai[lai > max_lai] <- max_lai

  return(lai)
}

#' @noRd
calc_growth_rate <- function(lai, par, rue = 1.91){

  epsilon_i <- calc_epsilon_i(lai)

  growth_rate <- rue * epsilon_i * par

  return(growth_rate)
}





