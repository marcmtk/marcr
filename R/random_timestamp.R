#' Random timestamp
#'
#' generate a random string timestamp of the format: 2016-02-29T22:53:28.835
#'
#' @param n number of random timestamps
#' @param start_date a character string of length 1 detailing the start date in the format YYYY-MM-DD
#' @param day_range number of days after start_date to generate
#'
#' @return
#' @export
#'
#' @examples
#'
#' random_timestamp(5)
random_timestamp <- function(n, start_date = "2016-01-01", day_range = 365) {
  start_date <- as.Date(start_date)
  days_after <- sample(seq(0, day_range), size = n, replace = TRUE)
  days <- start_date + days_after

  hour <- sample(0:23, size = n, replace = TRUE)
  minute <- sample(0:59, size = n, replace = TRUE)
  second <- sample(0:59, size = n, replace = TRUE)
  milisecond <- sample(0:999, size = n, replace = TRUE)
  times <- sprintf("%02i:%02i:%02i.%03i", hour, minute, second, milisecond)
  paste(days, times, sep = "T")
}
