test_that("get_team_seasons() returns expected structure", {
  seasons <- get_team_seasons(694)  # Tennessee

  expect_s3_class(seasons, "tbl_df")
  expect_named(seasons, c(
    "season_id", "org_id", "season", "head_coach", "division",
    "conference", "wins", "losses", "ties", "wl_pct", "notes"
  ))
  expect_true(all(seasons$org_id == 694))
  expect_true(all(!is.na(seasons$season) & nchar(seasons$season) >= 4))
  expect_gt(nrow(seasons), 5)
})
