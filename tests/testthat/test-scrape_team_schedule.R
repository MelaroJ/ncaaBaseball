test_that("get_schedule() returns expected structure", {
  schedule <- get_schedule(596721)  # Tennessee 2024-2025

  expect_s3_class(schedule, "tbl_df")
  expect_named(schedule, c(
    "game_date", "opponent_name", "opponent_id", "result",
    "team_score", "opp_score", "box_score_url", "attendance"
  ))

  expect_true(lubridate::is.Date(schedule$game_date))
  expect_true(is.character(schedule$opponent_name))
  expect_true(is.integer(schedule$opponent_id))
  expect_true(is.character(schedule$result))
  expect_true(is.integer(schedule$team_score))
  expect_true(is.integer(schedule$opp_score))
  expect_true(is.character(schedule$box_score_url))
  expect_true(is.integer(schedule$attendance))

  expect_gt(nrow(schedule), 5)
})
