source("scripts/00_config.R")
library(ggplot2)

read_tab <- function(name) readr::read_csv(table_path(paste0(name, ".csv")), show_col_types = FALSE)

truth <- read_tab("truth")
benchmarks <- read_tab("benchmarks_long")
guess_shares <- read_tab("guess_shares")
accuracy <- read_tab("accuracy_pooled")
choice <- read_tab("choice_model")
sensitivity <- read_tab("choice_sensitivity")
gap_cells <- read_tab("diagnostic_gap_by_cell")
gap_pooled <- read_tab("diagnostic_gap_pooled")
gap_pooled_gss <- read_tab("diagnostic_gap_pooled_gss")
order_pooled <- read_tab("order_pooled")
order_attr <- read_tab("order_by_attribute")
order_bal <- read_tab("order_balance")
extremity <- read_tab("extremity_models")
extremity_desc <- read_tab("extremity_descriptives")
projection <- read_tab("projection")
flow <- read_tab("sample_flow")
respondents <- readr::read_csv(data_path("respondents.csv"), show_col_types = FALSE)

category_order <- unlist(lapply(attribute_levels, rev), use.names = FALSE)
attribute_order <- unname(attribute_labels)
relation_labels <- c("In-party" = "Co-partisans' guesses", "Out-party" = "Opposing partisans' guesses")

# Figure 1: guesses against population shares, every category.
fig1_data <- guess_shares |>
  filter(relation %in% names(relation_labels)) |>
  mutate(series = relation_labels[relation], lo = pmax(share - 1.96 * se, 0), hi = pmin(share + 1.96 * se, 1))
fig1_truth <- truth |>
  transmute(target, attribute, guess = category, share, series = "Population share (2012)")
prep_axes <- function(d) {
  d |>
    mutate(
      attribute = factor(attribute_labels[attribute], levels = attribute_order),
      guess = factor(guess, levels = unique(category_order)),
      target = paste("Guesses about a", target)
    )
}
series_levels <- c("Population share (2012)", unname(relation_labels))
series_colours <- setNames(c(colours[["truth"]], colours[["in_party"]], colours[["out_party"]]), series_levels)
fig1 <- ggplot(
  bind_rows(prep_axes(fig1_data), prep_axes(fig1_truth)),
  aes(x = share, y = guess, colour = factor(series, series_levels), shape = factor(series, series_levels))
) +
  geom_linerange(
    data = function(d) filter(d, !is.na(lo)), aes(xmin = lo, xmax = hi),
    position = position_dodge(width = 0.6), linewidth = 0.4, show.legend = FALSE
  ) +
  geom_point(data = function(d) filter(d, !is.na(lo)), position = position_dodge(width = 0.6), size = 1.4) +
  geom_point(data = function(d) filter(d, is.na(lo)), size = 4) +
  facet_grid(attribute ~ target, scales = "free_y", space = "free_y") +
  scale_x_continuous(
    labels = function(x) paste0(100 * x, "%"), limits = c(0, 1), breaks = seq(0, 0.75, 0.25),
    expand = expansion(mult = c(0.02, 0.04))
  ) +
  scale_colour_manual(values = series_colours, breaks = series_levels) +
  scale_shape_manual(values = setNames(c(124, 16, 16), series_levels), breaks = series_levels) +
  labs(x = "Share choosing the category; black ticks mark the population share", y = NULL) +
  theme_paper() +
  theme(strip.text.y = element_text(angle = 0, hjust = 0), panel.spacing.x = unit(1.2, "lines"))
save_figure(fig1, "guesses_vs_truth", width = 6.5, height = 8.2)

# Figure 2: opposing minus co-partisan share choosing the most diagnostic category.
fig2_data <- gap_cells |>
  mutate(
    label = sprintf("%s: %s (%.1f times as common)", target, tolower(diagnostic), lr),
    label = gsub("black or african-american", "Black", label)
  ) |>
  arrange(lr) |>
  mutate(label = factor(label, levels = label))
fig2 <- ggplot(fig2_data, aes(x = 100 * gap, y = label)) +
  geom_vline(xintercept = 0, colour = "grey50", linewidth = 0.3) +
  geom_linerange(aes(xmin = 100 * lo, xmax = 100 * hi), colour = colours[["out_party"]], linewidth = 0.5) +
  geom_point(colour = colours[["out_party"]], size = 1.8) +
  labs(x = "Difference (percentage points)", y = NULL) +
  theme_paper()
save_figure(fig2, "diagnostic_gap", width = 6.5, height = 3.2)

# Tables.
acc <- accuracy |>
  filter(relation != "Independent") |>
  arrange(target, relation) |>
  transmute(
    `Guesses about` = target,
    `Guessers` = ifelse(relation == "In-party", "Co-partisans", "Opposing partisans"),
    `Chose modal category (\\%)` = paste(pct(chose_mode), interval(chose_mode_lo, chose_mode_hi, 100, 0)),
    `Correct by chance (\\%)` = paste(pct(hit_rate), interval(hit_rate_lo, hit_rate_hi, 100, 0)),
    `Proportional guessing (\\%)` = pct(probability_matching),
    `Always the mode (\\%)` = pct(best_possible),
    `$n$` = n
  )
names(acc) <- gsub("\\\\%", "%", names(acc))
write_tabular(acc, table_path("accuracy.tex"), "llccccr")

choice_rows <- choice |>
  mutate(relation = factor(relation, c("In-party", "Out-party", "Independent"))) |>
  arrange(relation) |>
  select(relation, term, estimate, lo, hi, n_respondents) |>
  pivot_wider(names_from = term, values_from = c(estimate, lo, hi)) |>
  transmute(
    Guessers = c("In-party" = "Co-partisans", "Out-party" = "Opposing partisans", "Independent" = "Independents")[
      as.character(relation)
    ],
    `Weight on population share` = paste(num(estimate_log_share), interval(lo_log_share, hi_log_share)),
    `Weight on diagnosticity` = paste(num(estimate_log_lr), interval(lo_log_lr, hi_log_lr)),
    `Respondents` = n_respondents
  )
write_tabular(choice_rows, table_path("choice.tex"), "lccr")

sens_labels <- c(
  "Primary" = "Primary benchmarks",
  "GSS for every attribute" = "GSS for every attribute",
  "Variant: leaners" = "Leaners counted as partisans",
  "Variant: degree" = "College = four-year degree",
  "Variant: all_adults" = "Unemployed = everyone not working",
  "Without race" = "Race omitted",
  "Without union" = "Union membership omitted",
  "Without Democratic union item" = "Democratic union item omitted"
)
sens <- sensitivity |>
  filter(term == "log_lr", analysis %in% names(sens_labels)) |>
  select(analysis, relation, estimate, se) |>
  mutate(value = paste0(num(estimate), " (", num(se), ")")) |>
  select(-estimate, -se) |>
  pivot_wider(names_from = relation, values_from = value) |>
  mutate(analysis = factor(analysis, names(sens_labels))) |>
  arrange(analysis) |>
  transmute(
    Specification = sens_labels[as.character(analysis)], `Co-partisans` = `In-party`,
    `Opposing partisans` = `Out-party`
  )
write_tabular(sens, table_path("choice_sensitivity.tex"), "lcc")

order_tab <- bind_rows(
  order_pooled |>
    filter(outcome == "diagnostic") |>
    transmute(
      Outcome = "Most diagnostic categories chosen (of 8)", `Guesses about` = target,
      `Asked first` = num(mean_first), `Difference if asked second` = paste(num(estimate), interval(lo, hi))
    ),
  order_attr |>
    filter(attribute == "union") |>
    transmute(
      Outcome = "Chose union member (proportion)", `Guesses about` = target,
      `Asked first` = num(ifelse(target == "Democrat", mean_first, 1 - mean_first)),
      `Difference if asked second` = paste(
        num(ifelse(target == "Democrat", estimate, -estimate)),
        interval(
          ifelse(target == "Democrat", estimate - 1.96 * se, -estimate - 1.96 * se),
          ifelse(target == "Democrat", estimate + 1.96 * se, -estimate + 1.96 * se)
        )
      )
    )
)
write_tabular(order_tab, table_path("order.tex"), "llcc")

term_labels <- c(
  out_diagnostic = "Most diagnostic categories chosen for the opposing party (0--8)",
  out_recall_extremity = "Mean CF-score of opposing leaders recalled (toward their pole)",
  strength = "Strength of partisanship (1--3)",
  partyRepublican = "Republican respondent",
  `(Intercept)` = "Constant"
)
ext <- extremity |>
  mutate(cell = paste(num(estimate), interval(lo, hi))) |>
  select(model, term, cell) |>
  pivot_wider(names_from = model, values_from = cell, values_fill = "") |>
  mutate(term = factor(term, names(term_labels))) |>
  arrange(term) |>
  transmute(` ` = term_labels[as.character(term)], `(1)` = stereotypes, `(2)` = recall, `(3)` = both)
ext <- bind_rows(ext, tibble(
  ` ` = "Respondents",
  `(1)` = as.character(extremity$n[extremity$model == "stereotypes"][1]),
  `(2)` = as.character(extremity$n[extremity$model == "recall"][1]),
  `(3)` = as.character(extremity$n[extremity$model == "both"][1])
))
write_tabular(ext, table_path("extremity.tex"), "p{6.2cm}ccc")

bench <- benchmarks |>
  filter(spec == "primary") |>
  select(source, party, attribute, category, share) |>
  pivot_wider(names_from = c(source, party), values_from = share) |>
  mutate(
    attribute = factor(attribute, names(attribute_levels)),
    order = match(category, unlist(attribute_levels))
  ) |>
  arrange(attribute, order)
bench_tab <- bench |>
  transmute(
    Category = category,
    `CCES Dem.` = pct(CCES_Democrat, 1), `GSS Dem.` = pct(GSS_Democrat, 1),
    `CCES Rep.` = pct(CCES_Republican, 1), `GSS Rep.` = pct(GSS_Republican, 1)
  )
bench_groups <- setNames(
  as.list(unname(attribute_labels[as.character(unique(bench$attribute))])),
  as.character(match(unique(bench$attribute), bench$attribute))
)
write_tabular(bench_tab, table_path("benchmarks.tex"), "lcccc", groups = bench_groups)

proj <- projection |>
  mutate(attribute = factor(attribute_labels[attribute], attribute_order)) |>
  arrange(attribute, relation) |>
  transmute(
    Characteristic = as.character(attribute),
    Guessers = ifelse(relation == "In-party", "Co-partisans", "Opposing partisans"),
    `Difference (proportion)` = paste(num(estimate), interval(lo, hi)), `$n$` = n
  )
write_tabular(proj, table_path("projection.tex"), "llcr")

# Manuscript numbers.
# Value of `col` in the rows of `df` matching every name = value pair.
pick <- function(df, col, ...) {
  cond <- list(...)
  keep <- Reduce(`&`, Map(function(k, v) df[[k]] == v, names(cond), cond))
  df[[col]][keep]
}
share_of <- function(target_, attribute_, category_, relation_) {
  pick(guess_shares, "share", target = target_, attribute = attribute_, guess = category_, relation = relation_)
}
truth_of <- function(target_, attribute_, category_, source_ = NULL) {
  if (is.null(source_)) {
    return(pick(truth, "share", target = target_, attribute = attribute_, category = category_))
  }
  pick(benchmarks, "share",
    spec = "primary", source = source_, party = target_, attribute = attribute_,
    category = category_
  )
}
bench_n <- function(source_, party_) {
  pick(benchmarks, "n", spec = "primary", source = source_, party = party_, attribute = "sex")[1]
}
coef_of <- function(rel, term_, col = "estimate") pick(choice, col, relation = rel, term = term_)
sens_of <- function(analysis_, rel) {
  pick(sensitivity, "estimate", analysis = analysis_, relation = rel, term = "log_lr")
}
gap_of <- function(sample_, col = "gap", tab = gap_pooled) pick(tab, col, sample = sample_)
acc_of <- function(target_, rel, col) pick(accuracy, col, target = target_, relation = rel)
cell_of <- function(target_, attribute_, col) pick(gap_cells, col, target = target_, attribute = attribute_)
ext_of <- function(model_, term_, col = "estimate") pick(extremity, col, model = model_, term = term_)
proj_of <- function(attribute_, rel) pick(projection, "estimate", attribute = attribute_, relation = rel)
order_union <- order_attr[order_attr$target == "Democrat" & order_attr$attribute == "union", ]
order_dem <- order_pooled[order_pooled$target == "Democrat" & order_pooled$outcome == "diagnostic", ]
order_rep <- order_pooled[order_pooled$target == "Republican" & order_pooled$outcome == "diagnostic", ]

macros <- list(
  nRecords = format(flow$n[1], big.mark = ","),
  nEligible = flow$n[2],
  nAnalysis = flow$n[3],
  nDem = sum(respondents$party == "Democrat"),
  nRep = sum(respondents$party == "Republican"),
  nInd = sum(respondents$party == "Independent"),
  nDemFirst = sum(respondents$dem_first),
  nRepFirst = sum(!respondents$dem_first),
  nCcesDem = format(bench_n("CCES", "Democrat"), big.mark = ","),
  nCcesRep = format(bench_n("CCES", "Republican"), big.mark = ","),
  nGssDem = bench_n("GSS", "Democrat"),
  nGssRep = bench_n("GSS", "Republican"),
  richOut = pct(share_of("Republican", "class", "Rich", "Out-party")),
  richIn = pct(share_of("Republican", "class", "Rich", "In-party")),
  richTruth = pct(truth_of("Republican", "class", "Rich"), 1),
  manRepOut = pct(share_of("Republican", "sex", "Man", "Out-party")),
  manRepIn = pct(share_of("Republican", "sex", "Man", "In-party")),
  unionOut = pct(share_of("Democrat", "union", "Union member", "Out-party")),
  unionIn = pct(share_of("Democrat", "union", "Union member", "In-party")),
  unionTruth = pct(truth_of("Democrat", "union", "Union member")),
  atheistIn = pct(share_of("Democrat", "religion", "Atheist or agnostic", "In-party")),
  atheistTruth = pct(truth_of("Democrat", "religion", "Atheist or agnostic")),
  atheistGss = pct(truth_of("Democrat", "religion", "Atheist or agnostic", "GSS")),
  blackOut = pct(share_of("Democrat", "race", "Black or African-American", "Out-party")),
  blackIn = pct(share_of("Democrat", "race", "Black or African-American", "In-party")),
  blackTruth = pct(truth_of("Democrat", "race", "Black or African-American")),
  blackGss = pct(truth_of("Democrat", "race", "Black or African-American", "GSS")),
  blackLr = num(cell_of("Democrat", "race", "lr"), 0),
  oldDemOut = pct(share_of("Democrat", "age", "50+", "Out-party")),
  oldDemIn = pct(share_of("Democrat", "age", "50+", "In-party")),
  oldDemTruth = pct(truth_of("Democrat", "age", "50+")),
  oldRepOut = pct(share_of("Republican", "age", "50+", "Out-party")),
  oldRepIn = pct(share_of("Republican", "age", "50+", "In-party")),
  oldRepTruth = pct(truth_of("Republican", "age", "50+")),
  unemployedOut = pct(share_of("Democrat", "employment", "Unemployed", "Out-party")),
  unemployedIn = pct(share_of("Democrat", "employment", "Unemployed", "In-party")),
  unemployedCces = pct(truth_of("Democrat", "employment", "Unemployed", "CCES")),
  unemployedGss = pct(truth_of("Democrat", "employment", "Unemployed", "GSS")),
  noCollegeOut = pct(share_of("Democrat", "education", "Never went to college", "Out-party")),
  noCollegeIn = pct(share_of("Democrat", "education", "Never went to college", "In-party")),
  noCollegeTruth = pct(truth_of("Democrat", "education", "Never went to college")),
  hitDemIn = pct(acc_of("Democrat", "In-party", "hit_rate")),
  hitDemOut = pct(acc_of("Democrat", "Out-party", "hit_rate")),
  hitRepIn = pct(acc_of("Republican", "In-party", "hit_rate")),
  hitRepOut = pct(acc_of("Republican", "Out-party", "hit_rate")),
  bestDem = pct(acc_of("Democrat", "In-party", "best_possible")),
  matchDem = pct(acc_of("Democrat", "In-party", "probability_matching")),
  thetaIn = num(coef_of("In-party", "log_lr")),
  thetaOut = num(coef_of("Out-party", "log_lr")),
  thetaOutLo = num(coef_of("Out-party", "log_lr", "lo")),
  thetaOutHi = num(coef_of("Out-party", "log_lr", "hi")),
  thetaOutDouble = num(2^coef_of("Out-party", "log_lr")),
  thetaInNoRace = num(sens_of("Without race", "In-party")),
  thetaOutNoRace = num(sens_of("Without race", "Out-party")),
  gap = pct(gap_of("All cells"), 1),
  gapLo = pct(gap_of("All cells", "lo"), 1),
  gapHi = pct(gap_of("All cells", "hi"), 1),
  gapParty = pct(gap_of("Adjusting for respondent party"), 1),
  gapPartyLo = pct(gap_of("Adjusting for respondent party", "lo"), 1),
  gapPartyHi = pct(gap_of("Adjusting for respondent party", "hi"), 1),
  gapNoRace = pct(gap_of("Without race"), 1),
  gapNoUnion = pct(gap_of("Without Democratic union item"), 1),
  gapGss = pct(gap_of("All cells", tab = gap_pooled_gss), 1),
  gapGssLo = pct(gap_of("All cells", "lo", gap_pooled_gss), 1),
  gapGssHi = pct(gap_of("All cells", "hi", gap_pooled_gss), 1),
  gapCells = gap_of("All cells", "cells"),
  gapCellsGss = gap_of("All cells", "cells", gap_pooled_gss),
  projAgeIn = num(proj_of("age", "In-party")),
  projAgeOut = num(proj_of("age", "Out-party")),
  projSexIn = num(proj_of("sex", "In-party")),
  projSexOut = num(proj_of("sex", "Out-party")),
  orderUnionFirst = pct(order_union$mean_first),
  orderUnionSecond = pct(order_union$mean_first + order_union$estimate),
  orderUnionHolm = num(order_union$p_holm, 3),
  orderShareDemFirst = pct(order_bal$democrat[order_bal$dem_first]),
  orderShareDemSecond = pct(order_bal$democrat[!order_bal$dem_first]),
  orderChisqP = num(stats::chisq.test(table(respondents$dem_first, respondents$party))$p.value),
  nOrderTests = nrow(order_attr),
  extN = extremity_desc$n,
  extMean = num(extremity_desc$mean_extremity),
  extSd = num(extremity_desc$sd_extremity),
  extStereo = num(ext_of("both", "out_diagnostic"), 3),
  extStereoLo = num(ext_of("both", "out_diagnostic", "lo"), 3),
  extStereoHi = num(ext_of("both", "out_diagnostic", "hi"), 3),
  extRecall = num(ext_of("both", "out_recall_extremity")),
  extRecallLo = num(ext_of("both", "out_recall_extremity", "lo")),
  extRecallHi = num(ext_of("both", "out_recall_extremity", "hi")),
  extStereoRange = num(8 * ext_of("both", "out_diagnostic"), 2),
  bootReps = format(bootstrap_reps, big.mark = ",")
)
write_macros(macros, table_path("macros.tex"))
