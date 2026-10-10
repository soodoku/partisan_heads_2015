guesses <- read_derived("guesses.csv")
benchmarks <- read_derived("benchmarks.csv")
truth <- truth_table(benchmarks)

test_that("conditional logit matches a hand-coded likelihood", {
  d <- choice_data(filter(guesses, relation == "Out-party"), truth)
  fit <- fit_choice(d)
  negll <- function(b) {
    u <- b[1] * d$log_share + b[2] * d$log_lr
    -sum(u[d$chosen == 1]) + sum(tapply(exp(u), d$stratum, function(x) log(sum(x))))
  }
  opt <- stats::optim(c(0.5, 0), negll, method = "BFGS")
  expect_equal(unname(fit$estimate), opt$par, tolerance = 1e-3)
})

test_that("per-cell gaps equal differences in raw shares", {
  scored <- score_guesses(guesses, truth)
  gap <- diagnostic_gap(scored, truth)$by_cell
  expect_equal(gap$gap, gap$out_party - gap$in_party)
})

test_that("reported tables match a fresh computation", {
  scored <- score_guesses(guesses, truth)
  acc <- accuracy_summary(scored)
  stored <- read_table("accuracy_by_attribute.csv")
  expect_equal(acc$chose_mode, stored$chose_mode)
  gap <- read_table("diagnostic_gap_pooled.csv")
  expect_equal(diagnostic_gap(scored, truth)$pooled$gap, gap$gap)
})
