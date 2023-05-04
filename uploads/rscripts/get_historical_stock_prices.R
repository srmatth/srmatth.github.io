#' Get Historical Prices
#'
#' A function to get the historical prices for a character vector of tickers
#'
#' @param tickers the tickers of the stocks that you want to retrieve historical prices for
#'
#' @return a data frame containing historical price information
#' @export
get_historical_prices <- function(tickers) {
  purrr::map_dfr(
    .x = toupper(tickers),
    .f = ~{
      tryCatch({
        logger::log_info("Downloading price data for {.x}")
        url <- stringr::str_c(
          "https://query1.finance.yahoo.com/v7/finance/download/",
          .x,
          "?period1=1&period2=",
          round(as.numeric(Sys.time())) - (86400 * 4),
          "&interval=1d&events=history"
        )
        readr::read_csv(url, col_types = "Ddddddd") %>%
          dplyr::select(-`Adj Close`) %>%
          magrittr::set_colnames(c("date", "open", "high", "low", "close", "volume")) %>%
          dplyr::mutate(ticker = .x)
      },
      error = function(e) {
        d <- data.frame(
          date = NA,
          open = NA,
          high = NA,
          low = NA,
          close = NA,
          volumne = NA,
          ticker = NA
        ) %>%
          dplyr::filter(!is.na(date))
        logger::log_error("Failed to download Price Data for {.x}")
        return(d)
      }
      )
    }
  )
}
