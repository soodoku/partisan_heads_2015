survey <- read_survey()
respondents <- read_derived("respondents.csv")
guesses <- read_derived("guesses.csv")

test_that("sample flow matches the documented counts", {
  flow <- sample_flow(survey)
  expect_equal(flow$n, c(1868L, 359L, 343L))
  expect_equal(nrow(respondents), 343L)
  expect_false(anyDuplicated(respondents$respondent_id) > 0)
  expect_equal(nrow(guesses), 343L * 2L * 8L)
})

test_that("guesses agree with the legacy recoded columns after case folding", {
  # The 2014 recode script built *r columns from the same blocks; it is an
  # independent implementation of the block selection.
  legacy <- c(
    class = "rich", religion = "rel", race = "race", sex = "sex", age = "age",
    education = "educ", employment = "employ", union = "union"
  )
  d <- survey[match(guesses$respondent_id, survey$uniqid), ]
  column <- paste0(ifelse(guesses$target == "Democrat", "dem", "rep"), legacy[guesses$attribute], "r")
  legacy_value <- d[cbind(seq_len(nrow(d)), match(column, names(d)))]
  legacy_value[legacy_value %in% c("", "NA")] <- NA
  expect_equal(tolower(guesses$guess), tolower(legacy_value))
})

test_that("order assignment matches the Qualtrics block randomizer", {
  raw <- utils::read.csv(data_path("survey_2013.csv"), stringsAsFactors = FALSE)
  d <- raw[match(respondents$respondent_id, raw$uniqid), ]
  expect_equal(respondents$dem_first, d$demfirst %in% 1)
  expect_equal(sum(respondents$dem_first), 187L)
})

test_that("every guess is an offered category", {
  ok <- mapply(function(a, k) is.na(k) || k %in% attribute_levels[[a]], guesses$attribute, guesses$guess)
  expect_true(all(ok))
  expect_equal(sum(is.na(guesses$guess)), 2L)
})
