#' Get All Seasons for a Team
#'
#' Scrapes all available seasons for a given team/org_id
#' from the team history page.
#'
#' @param org_id Integer NCAA org ID (e.g., 694 for Tennessee)
#' @param sport_code Character sport code, default is "MBA" (Baseball)
#' @return A tibble with one row per season and columns:
#'   - season_id
#'   - org_id
#'   - season (e.g. "2024-25")
#'   - head_coach
#'   - division
#'   - conference
#'   - wins, losses, ties
#'   - wl_pct
#'   - notes
#' @export
get_team_seasons <- function(org_id, sport_code = "MBA") {
  base_url <- "https://stats.ncaa.org/teams/history"
  query <- list(
    utf8 = "✓",
    org_id = org_id,
    sport_code = sport_code,
    commit = "Search"
  )

  req <- ncaa_request(base_url) |>
    httr2::req_url_query(!!!query)

  page <- perform_throttled(req) |>
    httr2::resp_body_html()

  rows <- page |>
    rvest::html_element("table") |>
    rvest::html_elements("tr")

  rows <- rows[-1]  # skip header

  purrr::map_dfr(rows, function(row) {
    cols <- row |> rvest::html_elements("td")
    link <- cols[[1]] |> rvest::html_element("a")

    tibble::tibble(
      season_id = link |> rvest::html_attr("href") |> stringr::str_extract("[0-9]+$") |> as.integer(),
      org_id = org_id,
      season = link |> rvest::html_text(trim = TRUE),
      head_coach = cols[[2]] |> rvest::html_text(trim = TRUE),
      division = cols[[3]] |> rvest::html_text(trim = TRUE),
      conference = cols[[4]] |> rvest::html_text(trim = TRUE),
      wins = cols[[5]] |> rvest::html_text(trim = TRUE) |> as.integer(),
      losses = cols[[6]] |> rvest::html_text(trim = TRUE) |> as.integer(),
      ties = cols[[7]] |> rvest::html_text(trim = TRUE) |> as.integer(),
      wl_pct = cols[[8]] |> rvest::html_text(trim = TRUE) |> as.numeric(),
      notes = cols[[9]] |> rvest::html_text(trim = TRUE)
    )
  }) |>
    dplyr::filter(!is.na(season_id))
}
