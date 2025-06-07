test_that("get_ncaa_baseball_teams() returns expected structure", {
  teams <- get_ncaa_baseball_teams()

  expect_s3_class(teams, "tbl_df")
  expect_named(teams, c("org_id", "school_name"))
  expect_true(is.integer(teams$org_id))
  expect_true(is.character(teams$school_name))
  expect_gt(nrow(teams), 200)  # Expecting at least 200 teams
  expect_true(any(grepl("Tennessee", teams$school_name)))
})
