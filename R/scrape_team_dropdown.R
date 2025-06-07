#' Get NCAA Baseball Teams
#'
#' Scrapes the org_id and school name for all institutions sponsoring baseball.
#'
#' @return A tibble with columns `org_id` and `school_name`
#' @export
get_ncaa_baseball_teams <- function() {
  url <- "https://stats.ncaa.org/teams/history"

  # Use a browser-like User-Agent to avoid 403
  ua <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/114.0.0.0 Safari/537.36"

  Sys.sleep(runif(1, 0.5, 1.5))  # polite delay

  page <- ncaa_request(url) |>
    perform_throttled() |>
    httr2::resp_body_html()

  dropdown <- page |>
    rvest::html_element("#org_id_select") |>
    rvest::html_elements("option")

  tibble::tibble(
    org_id = dropdown |> rvest::html_attr("value") |> as.integer(),
    school_name = dropdown |> rvest::html_text(trim = TRUE)
  ) |>
    dplyr::filter(!is.na(org_id))
}
