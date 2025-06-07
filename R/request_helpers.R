#' Create a base NCAA stats request
#'
#' Adds consistent headers, timeout, and future throttling.
#' @param url The full URL to request
#' @return A httr2 request object
#' @noRd
ncaa_request <- function(url) {
  httr2::request(url) |>
    httr2::req_headers(
      `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/114.0.0.0 Safari/537.36",
      Accept = "text/html,application/xhtml+xml"
    ) |>
    httr2::req_timeout(10)
  # Add req_retry() or req_cache() here if needed later
}

#' Throttled Request with Retry
#'
#' Logs, delays, and retries NCAA requests safely.
#'
#' @param req httr2 request
#' @param min_delay minimum seconds between requests
#' @param max_delay maximum seconds between requests
#' @param max_retries how many times to retry after failure
#' @param verbose logical; whether to print logs
#' @return httr2 response object
#' @noRd
perform_throttled <- function(req, min_delay = 0.5, max_delay = 1.5, max_retries = 3, verbose = TRUE) {
  delay <- runif(1, min_delay, max_delay)
  if (verbose) message(glue::glue("Requesting {req$url} (wait {round(delay, 2)}s)..."))
  Sys.sleep(delay)

  attempt <- 1
  repeat {
    tryCatch({
      return(httr2::req_perform(req))
    }, error = function(e) {
      if (attempt >= max_retries) stop("Request failed after ", max_retries, " attempts: ", e$message)
      wait <- 2^attempt  # exponential backoff
      message(glue::glue("Retry {attempt} failed. Waiting {wait}s..."))
      Sys.sleep(wait)
      attempt <<- attempt + 1
    })
  }
}
