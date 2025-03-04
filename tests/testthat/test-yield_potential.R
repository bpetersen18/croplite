test_that("Can calculate the light intercecption efficiency",{

  lai <- 0:3

  expected_epsilon_i <- c(0, 0.4934, 0.7433, 0.8700)

  epsilon_i <- round(calc_epsilon_i(lai, k = 0.68), digits = 4)

  expect_equal(epsilon_i, expected_epsilon_i)

  lai <- -1

  expect_error(calc_epsilon_i(lai, k = 0.68), "Error: Light interception efficiency must be between 0 and 1")
})

test_that("Can calculate the growing degree days given the daily maximum and minimum temperature", {
  tmax <- c(0, 10, 20, 30)
  tmin <- c(-10, 0, 10, 20)

  expected_gdd <- c(0, 0, 9, 19)

  gdd <- calc_gdd(tmax, tmin, tbase = 6)

  expect_equal(gdd, expected_gdd)

  tmax <- 20
  tmin <- 30

  expect_error(calc_gdd(tmax, tmin, tbase = 6), "Error: Minimum temperature must be less than maximum temperature")
})

test_that("Can determine the start and end of the growing season", {
  # Create a data frame with daily temperature data
  daily_temp <- data.frame(
    date = seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by = "days"),
    tmin = sin(seq(pi+1, 3*pi+1, length.out = 366)) * 20 + 0
  )

  start_gs <- calc_start_gs(daily_temp$date, daily_temp$tmin, t_threshold = -2, spring_months = 1:5)
  end_gs <- calc_end_gs(daily_temp$date, daily_temp$tmin, t_threshold = -2, fall_months = 8:12)

  expect_equal(start_gs, as.Date("2020-04-28"))
  expect_equal(end_gs, as.Date("2020-11-09"))

  # Test for errors
  tmin <- c(10, 20, 30)
  date <- c(as.Date("2020-01-01"), as.Date("2020-01-03"))

  expect_error(calc_start_gs(date, tmin, t_threshold = -2, spring_months = 1:5), "Error: date and tmin must be the same length")
  expect_error(calc_end_gs(date, tmin, t_threshold = -2, fall_months = 8:12), "Error: date and tmin must be the same length")
})

test_that("Can calculate the LAI given the accumulated gdd", {
  accumulated_gdd <- seq(0, 1000, by = 500)

  expected_lai <- c(0, 3, 5)

  lai <- calc_lai(accumulated_gdd, x = 0.006, max_lai = 5)

  expect_equal(lai, expected_lai)
})

test_that("Can calculate the daily growth rate given the lai and par", {
  lai <- c(0, 3, 5)
  par <- rep(1000, 3)

  expected_growth_rate <- c(0, 1661.6, 1846.3)

  growth_rate <- round(calc_growth_rate(lai, par, rue = 1.91), digits = 1)

  expect_equal(growth_rate, expected_growth_rate)
})

test_that("Can model crop growth given a date, daily par, daily minimum temperature, daily maximum temperature", {
  daily_weather <- data.frame(
    date = seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by = "days"),
    tmin = sin(seq(pi+1, 3*pi+1, length.out = 366)) * 20 + 0,
    tmax = sin(seq(pi+1, 3*pi+1, length.out = 366)) * 20 + 20,
    par = rep(5, 366)
  )

  peak_biomass <- round(sum(model_growth(daily_weather$date, daily_weather$par, daily_weather$tmin, daily_weather$tmax)$growth_rate), digits = 1)

  expected_biomass <- 1556.4

  expect_equal(peak_biomass, expected_biomass)

})

