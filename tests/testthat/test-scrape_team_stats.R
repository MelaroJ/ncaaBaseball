test_that("get_team_stats() returns expected structure for hitting", {
  stats <- get_team_stats(596721, stat_type = "hitting")

  expect_s3_class(stats, "tbl_df")
  expect_gt(nrow(stats), 5)
  expect_true("NUM" %in% names(stats))
  expect_true("PLAYER" %in% names(stats))
})

test_that("get_team_stats() works for pitching and fielding", {
  pitching <- get_team_stats(596721, stat_type = "pitching")
  fielding <- get_team_stats(596721, stat_type = "fielding")

  expect_s3_class(pitching, "tbl_df")
  expect_s3_class(fielding, "tbl_df")
  expect_gt(nrow(pitching), 2)
  expect_gt(nrow(fielding), 2)
})
