test_that("get_roster() returns expected structure", {
  roster <- get_roster(596721)

  expect_s3_class(roster, "tbl_df")
  expect_named(roster, c(
    "gp", "gs", "number", "name", "player_url", "class",
    "position", "height", "bats", "throws", "hometown", "highschool"
  ))

  expect_true(is.character(roster$name))
  expect_true(is.character(roster$player_url))
  expect_true(is.character(roster$class))
  expect_true(is.character(roster$position))
  expect_true(is.character(roster$height))
  expect_true(is.character(roster$bats))
  expect_true(is.character(roster$throws))
  expect_true(is.character(roster$hometown))
  expect_true(is.character(roster$highschool))
  expect_true(is.integer(roster$gp))
  expect_true(is.integer(roster$gs))

  expect_gt(nrow(roster), 10)
})
