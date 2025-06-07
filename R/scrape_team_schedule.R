#' Get Game Schedule for a Team-Season
#'
#' Scrapes the schedule/results table from a team-season page.
#'
#' @param season_id Integer season page ID (from get_team_seasons)
#' @return A tibble with one row per game and columns:
#'   - game_date: Date
#'   - opponent_name: string
#'   - opponent_id: integer
#'   - result: string
#'   - team_score: integer
#'   - opp_score: integer
#'   - box_score_url: string
#'   - attendance: integer
#' @export
get_schedule <- function(season_id) {
  url <- glue::glue("https://stats.ncaa.org/teams/{season_id}")

  page <- ncaa_request(url) |>
    perform_throttled() |>
    httr2::resp_body_html()

  rows <- page |>
    rvest::html_elements("tr.underline_rows")

  purrr::map_dfr(rows, function(row) {
    cols <- row |> rvest::html_elements("td")

    # Skip non-schedule rows like table headers (e.g., "Batting Average")
    if (length(cols) < 4 || grepl("Batting Average", rvest::html_text(cols[[1]], trim = TRUE))) {
      return(NULL)
    }

    raw_date <- rvest::html_text(cols[[1]], trim = TRUE)
    clean_date <- raw_date |>
      stringr::str_remove("\\(\\d+\\)") |>  # remove doubleheader suffix
      stringr::str_remove("\\s+\\d{1,2}:\\d{2}\\s*(AM|PM|am|pm)?") |>  # remove time if present
      trimws()
    parsed_date <- suppressWarnings(lubridate::mdy(clean_date))

    if (is.na(parsed_date)) {
      return(NULL)
    }

    opponent_node <- cols[[2]] |> rvest::html_element("a")
    opponent_name <- if (!is.na(opponent_node)) rvest::html_text(opponent_node, trim = TRUE) else NA_character_
    opponent_href <- if (!is.na(opponent_node)) rvest::html_attr(opponent_node, "href") else NA_character_
    opponent_id <- stringr::str_extract(opponent_href, "[0-9]+$") |> as.integer()

    result_node <- if (length(cols) >= 3) rvest::html_element(cols[[3]], "a") else NULL
    result_text <- if (!is.null(result_node)) rvest::html_text(result_node, trim = TRUE) else NA_character_
    box_score_url <- if (!is.null(result_node)) rvest::html_attr(result_node, "href") else NA_character_

    scores <- if (!is.na(result_text)) stringr::str_extract_all(result_text, "\\d+")[[1]] |> as.integer() else integer(0)
    team_score <- if (length(scores) >= 2) scores[1] else NA_integer_
    opp_score <- if (length(scores) >= 2) scores[2] else NA_integer_

    attendance <- if (length(cols) >= 4) {
      cols[[4]] |> rvest::html_text(trim = TRUE) |>
        stringr::str_remove_all(",") |> as.integer()
    } else NA_integer_

    tibble::tibble(
      game_date = parsed_date,
      opponent_name = opponent_name,
      opponent_id = opponent_id,
      result = result_text,
      team_score = team_score,
      opp_score = opp_score,
      box_score_url = if (!is.na(box_score_url)) glue::glue("https://stats.ncaa.org{box_score_url}") else NA_character_,
      attendance = attendance
    )
  })
}
