#' Get Season-to-Date Team Statistics
#'
#' Scrapes the full team statistics table and prints the date label.
#'
#' @param season_id Integer season ID
#' @param stat_type One of "hitting", "pitching", or "fielding"
#' @return A tibble with one row per player and all season stat columns (depending on stat_type)
#' @export
get_team_stats <- function(season_id, stat_type = c("hitting", "pitching", "fielding")) {
  stat_type <- match.arg(stat_type)
  base_url <- glue::glue("https://stats.ncaa.org/teams/{season_id}/season_to_date_stats")
  base_page <- ncaa_request(base_url) |> perform_throttled() |> httr2::resp_body_html()

  stat_links <- base_page |> rvest::html_elements("#stats_div .nav-link")
  stat_hrefs <- stat_links |> rvest::html_attr("href")
  stat_texts <- stat_links |> rvest::html_text(trim = TRUE) |> tolower()

  stat_map <- rlang::set_names(
    stringr::str_extract(stat_hrefs, "(?<=year_stat_category_id=)\\d+"),
    stat_texts
  )

  if (stat_type == "hitting" && is.na(stat_map[["hitting"]])) {
    url <- base_url
  } else {
    if (!(stat_type %in% names(stat_map))) {
      stop("Invalid stat_type or stat_type not found on page.")
    }
    stat_id <- stat_map[[stat_type]]
    url <- glue::glue("{base_url}?year_stat_category_id={stat_id}")
  }

  if (stat_type == "hitting" && is.na(stat_map[["hitting"]])) {
    page <- base_page
  } else {
    if (!(stat_type %in% names(stat_map))) {
      stop("Invalid stat_type or stat_type not found on page.")
    }
    stat_id <- stat_map[[stat_type]]
    url <- glue::glue("{base_url}?year_stat_category_id={stat_id}")
    page <- ncaa_request(url) |>
      perform_throttled() |>
      httr2::resp_body_html()
  }

  # Extract and print update notice
  stat_notice <- page |>
    rvest::html_element("#stats_div .card-header") |>
    rvest::html_text(trim = TRUE)

  message("\n\u2139\ufe0f  ", stat_notice, "\n")

  header <- page |>
    rvest::html_element("thead") |>
    rvest::html_elements("th") |>
    rvest::html_text(trim = TRUE) |>
    make.names(unique = TRUE)

  rows <- page |>
    rvest::html_element("table") |>
    rvest::html_elements("tr") |>
    purrr::keep(~ {
      id <- rvest::html_attr(.x, "id")
      !is.na(id) && stringr::str_starts(id, "player_")
    })  # skip header

  purrr::map_dfr(rows, function(row) {
    cols <- row |> rvest::html_elements("td")
    if (length(cols) < 5) return(NULL)

    val1 <- rvest::html_text(cols[[1]], trim = TRUE)
    if (!grepl("^\\d+$", val1)) return(NULL)  # skip non-player rows

    vals <- purrr::map_chr(cols, rvest::html_text, trim = TRUE)
    df <- tibble::as_tibble_row(vals, .name_repair = "minimal")
    names(df) <- header[seq_along(df)] |> toupper()
    names(df)[names(df) == "X."] <- "NUM"
    df
  })
}
