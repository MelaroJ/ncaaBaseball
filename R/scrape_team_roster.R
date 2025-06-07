#' Get Roster for a Team-Season
#'
#' Scrapes the roster table for a given team-season.
#'
#' @param season_id Integer season ID
#' @return A tibble with one row per player and columns:
#'   - number, name, player_url, class, position, height, bats, throws, hometown, highschool
#' @export
get_roster <- function(season_id) {
  url <- glue::glue("https://stats.ncaa.org/teams/{season_id}/roster")

  page <- ncaa_request(url) |>
    perform_throttled() |>
    httr2::resp_body_html()

  rows <- page |>
    rvest::html_element("table") |>
    rvest::html_elements("tr") |>
    tail(-1)  # skip header

  purrr::map_dfr(rows, function(row) {
    cols <- row |> rvest::html_elements("td")
    if (length(cols) < 10) return(NULL)

    name_node <- cols[[4]] |> rvest::html_element("a")
    player_href <- if (!is.na(name_node)) rvest::html_attr(name_node, "href") else NA_character_
    player_url <- if (!is.na(player_href)) glue::glue("https://stats.ncaa.org{player_href}") else NA_character_

    tibble::tibble(
      gp = rvest::html_text(cols[[1]], trim = TRUE) |> as.integer(),
      gs = rvest::html_text(cols[[2]], trim = TRUE) |> as.integer(),
      number = rvest::html_text(cols[[3]], trim = TRUE),
      name = rvest::html_text(cols[[4]], trim = TRUE),
      player_url = player_url,
      class = rvest::html_text(cols[[5]], trim = TRUE),
      position = rvest::html_text(cols[[6]], trim = TRUE),
      height = rvest::html_text(cols[[7]], trim = TRUE),
      bats = rvest::html_text(cols[[8]], trim = TRUE),
      throws = rvest::html_text(cols[[9]], trim = TRUE),
      hometown = rvest::html_text(cols[[10]], trim = TRUE),
      highschool = if (length(cols) >= 11) rvest::html_text(cols[[11]], trim = TRUE) else NA_character_
    )
  })
}
