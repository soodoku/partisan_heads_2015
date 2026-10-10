source("scripts/00_config.R")

survey <- read_survey()
respondents <- readr::read_csv(data_path("respondents.csv"), show_col_types = FALSE)
guesses <- readr::read_csv(data_path("guesses.csv"), show_col_types = FALSE)
benchmarks <- readr::read_csv(data_path("benchmarks.csv"), show_col_types = FALSE)

truth <- truth_table(benchmarks)
scored <- score_guesses(guesses, truth)
relations <- c("In-party", "Out-party", "Independent")

# Benchmark comparison: CCES and GSS shares under every coding variant.
readr::write_csv(benchmarks, table_path("benchmarks_long.csv"))
readr::write_csv(alternatives_table(truth), table_path("truth.csv"))

guess_shares <- scored |>
  count(target, relation, attribute, guess) |>
  group_by(target, relation, attribute) |>
  mutate(share = n / sum(n), total = sum(n), se = sqrt(share * (1 - share) / total)) |>
  ungroup() |>
  complete(nesting(target, relation, attribute, total), guess, fill = list(n = 0L, share = 0, se = 0)) |>
  filter(mapply(function(a, k) k %in% attribute_levels[[a]], attribute, guess))
readr::write_csv(guess_shares, table_path("guess_shares.csv"))

# 1. Accuracy.
readr::write_csv(accuracy_summary(scored), table_path("accuracy_by_attribute.csv"))
readr::write_csv(pooled_accuracy(scored), table_path("accuracy_pooled.csv"))

# 2. Stereotyping: conditional logit (bootstrap intervals are added at the end).
readr::write_csv(conflict_cells(scored, truth), table_path("conflict_cells.csv"))
gap <- diagnostic_gap(scored, truth)
readr::write_csv(gap$by_cell, table_path("diagnostic_gap_by_cell.csv"))
readr::write_csv(gap$pooled, table_path("diagnostic_gap_pooled.csv"))
gap_gss <- diagnostic_gap(scored, truth_table(benchmarks, other_source = "GSS"))
readr::write_csv(gap_gss$pooled, table_path("diagnostic_gap_pooled_gss.csv"))

# Sensitivity of the choice model to benchmark source, coding and dropped attributes.
sensitivity_fit <- function(truth_s, label, g = guesses) {
  bind_rows(lapply(c("In-party", "Out-party"), function(rel) {
    fit_choice(choice_data(filter(g, relation == rel), truth_s)) |>
      mutate(relation = rel, analysis = label)
  }))
}
sensitivity <- bind_rows(
  sensitivity_fit(truth, "Primary"),
  sensitivity_fit(truth_table(benchmarks, other_source = "GSS"), "GSS for every attribute"),
  sensitivity_fit(truth_table(benchmarks, class_source = "CCES"), "CCES income for class"),
  bind_rows(lapply(setdiff(benchmark_specs$spec, "primary"), function(s) {
    sensitivity_fit(truth_table(benchmarks, spec_name = s), paste("Variant:", s))
  })),
  bind_rows(lapply(names(attribute_levels), function(a) {
    sensitivity_fit(truth, paste("Without", a), filter(guesses, attribute != a))
  })),
  sensitivity_fit(truth, "Without Democratic union item", filter(
    guesses, !(attribute == "union" & target == "Democrat")
  ))
)
readr::write_csv(sensitivity, table_path("choice_sensitivity.csv"))

# Rival explanation: projecting one's own characteristics.
readr::write_csv(projection_effects(scored, survey, respondents), table_path("projection.csv"))

# 3. Randomised order.
order <- order_effects(scored)
readr::write_csv(order$pooled, table_path("order_pooled.csv"))
readr::write_csv(order$by_attribute, table_path("order_by_attribute.csv"))
readr::write_csv(order_balance(respondents), table_path("order_balance.csv"))

# 4. Perceived out-party extremity.
extremity <- extremity_models(scored, respondents)
readr::write_csv(extremity$coefficients, table_path("extremity_models.csv"))
readr::write_csv(
  extremity$data |>
    summarise(
      n = n(), mean_extremity = mean(out_extremity), sd_extremity = sd(out_extremity),
      mean_diagnostic = mean(out_diagnostic), sd_diagnostic = sd(out_diagnostic),
      recall_scored = sum(!is.na(out_recall_extremity))
    ),
  table_path("extremity_descriptives.csv")
)

readr::write_csv(scored, data_path("scored_guesses.csv"))

# Slowest step last: refit the choice model on joint respondent/benchmark resamples.
choice <- bind_rows(lapply(relations, function(rel) {
  fit_choice(choice_data(filter(guesses, relation == rel), truth)) |>
    left_join(choice_bootstrap(guesses, truth, rel), by = "term") |>
    mutate(relation = rel)
}))
readr::write_csv(choice, table_path("choice_model.csv"))
