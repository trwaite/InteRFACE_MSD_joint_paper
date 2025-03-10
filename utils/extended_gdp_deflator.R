#' Calculate a gross domestic product (GDP) implicit price deflator between two years.
#'
#' The GDP deflator is a measure of price inflation with respect to a
#' specific base year; it allows us to back out the effects of inflation when we
#' compare prices over time.  This function calculates a deflator given a base
#' year (the year to convert from) and a conversion year (the year to convert
#' to).  To use the deflator, multiply prices in base-year dollars by the deflator; the
#' result will be prices in the converted dollar year.
#'
#' @param year Year to convert TO.
#' @param base_year Year to convert FROM.
#' @return GDP Deflator.  Multiply to convert FROM \code{base_year} dollars TO
#' \code{year} dollars.
#' @source U.S. Bureau of Economic Analysis, Gross domestic product (implicit
#' price deflator) [A191RD3A086NBEA], retrieved from FRED, Federal Reserve Bank
#' of St. Louis; https://fred.stlouisfed.org/series/A191RD3A086NBEA, April 12,
#' 2017
#' @author BBL
#' @export
#' @examples
#' gdp_bil_1990USD <- c(4770, 4779, 4937)
#' gdp_bil_2010USD <- gdp_bil_1990USD * gdp_deflator(2010, base_year = 1990)
extended_gdp_deflator <- function(year, base_year) {
  # This time series is the BEA "A191RD3A086NBEA" product
  # Downloaded April 13, 2017 from https://fred.stlouisfed.org/series/A191RD3A086NBEA
  gdp_years <- 1929:2019
  gdp_data <- read.csv("utils/gdp_deflator.csv")
  gdp_years <- lubridate::year(lubridate::mdy(gdp_data$DATE))
  gdp <- gdp_data$A191RD3A086NBEA
  names(gdp) <- gdp_years

  assertthat::assert_that(all(year %in% gdp_years))
  assertthat::assert_that(all(base_year %in% gdp_years))

  as.vector(unlist(gdp[as.character(year)] / gdp[as.character(base_year)]))
}
