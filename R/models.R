# Estimators. Every function takes the long guess table (one row per respondent x
# target party x attribute) and a truth table of population shares.

# Category-level shares for both parties side by side, with the likelihood ratio
# Pr(category | target) / Pr(category | other party).
alternatives_table <- function(truth) {
  wide <- truth |>
    select(party, attribute, category, share) |>
    pivot_wider(names_from = party, values_from = share)
  stopifnot(all(wide$Democrat > 0), all(wide$Republican > 0))
  bind_rows(
    wide |> transmute(target = "Democrat", attribute, category, share = Democrat, other = Republican),
    wide |> transmute(target = "Republican", attribute, category, share = Republican, other = Democrat)
  ) |>
    mutate(log_share = log(share), log_lr = log(share / other)) |>
    group_by(target, attribute) |>
    mutate(
      is_mode = share == max(share),
      is_most_diagnostic = log_lr == max(log_lr)
    ) |>
    ungroup()
}

score_guesses <- function(guesses, truth) {
  alt <- alternatives_table(truth)
  guesses |>
    filter(!is.na(guess)) |>
    left_join(
      alt |> select(target, attribute, guess = category, guess_share = share, is_mode, is_most_diagnostic),
      by = c("target", "attribute", "guess"),
      relationship = "many-to-one"
    ) |>
    left_join(
      alt |>
        group_by(target, attribute) |>
        summarise(mode_share = max(share), matching_share = sum(share^2), .groups = "drop"),
      by = c("target", "attribute")
    )
}

# Accuracy: how often a guess would match a randomly drawn partisan. Picking the
# modal category every time maximises this; drawing guesses in proportion to the
# true shares gives sum(share^2).
accuracy_summary <- function(scored) {
  scored |>
    group_by(target, attribute, relation) |>
    summarise(
      n = n(),
      chose_mode = mean(is_mode),
      hit_rate = mean(guess_share),
      best_possible = first(mode_share),
      probability_matching = first(matching_share),
      .groups = "drop"
    )
}

# Pooled accuracy by target and relation, averaging over the eight attributes within
# each respondent, with a respondent bootstrap.
pooled_accuracy <- function(scored, reps = bootstrap_reps, seed = bootstrap_seed) {
  per_respondent <- scored |>
    group_by(respondent_id, target, relation) |>
    summarise(
      chose_mode = mean(is_mode), hit_rate = mean(guess_share),
      best_possible = mean(mode_share), probability_matching = mean(matching_share), .groups = "drop"
    )
  point <- per_respondent |>
    group_by(target, relation) |>
    summarise(across(c(chose_mode, hit_rate, best_possible, probability_matching), mean), n = n(), .groups = "drop")
  set.seed(seed)
  ids <- unique(per_respondent$respondent_id)
  draws <- replicate(reps,
    {
      pick <- tibble(respondent_id = sample(ids, replace = TRUE))
      per_respondent |>
        inner_join(pick, by = "respondent_id", relationship = "many-to-many") |>
        group_by(target, relation) |>
        summarise(chose_mode = mean(chose_mode), hit_rate = mean(hit_rate), .groups = "drop")
    },
    simplify = FALSE
  )
  bounds <- bind_rows(draws) |>
    group_by(target, relation) |>
    summarise(
      chose_mode_lo = quantile(chose_mode, 0.025), chose_mode_hi = quantile(chose_mode, 0.975),
      hit_rate_lo = quantile(hit_rate, 0.025), hit_rate_hi = quantile(hit_rate, 0.975), .groups = "drop"
    )
  left_join(point, bounds, by = c("target", "relation"))
}

# Conditional logit over the offered categories. Choice probability is proportional
# to share^gamma * LR^theta. Probability matching is gamma = 1, theta = 0; always
# picking the mode is gamma -> infinity, theta = 0; the Bordalo et al. (2016)
# stereotype model is theta > 0.
choice_data <- function(guesses, truth) {
  alt <- alternatives_table(truth)
  guesses |>
    filter(!is.na(guess)) |>
    mutate(stratum = paste(respondent_id, target, attribute)) |>
    inner_join(
      alt |> select(target, attribute, category, log_share, log_lr),
      by = c("target", "attribute"),
      relationship = "many-to-many"
    ) |>
    mutate(chosen = as.integer(category == guess))
}

fit_choice <- function(data) {
  stopifnot(all(tapply(data$chosen, data$stratum, sum) == 1))
  fit <- survival::clogit(
    chosen ~ log_share + log_lr + strata(stratum),
    data = data, cluster = respondent_id, method = "efron"
  )
  s <- summary(fit)$coefficients
  tibble(
    term = rownames(s), estimate = s[, "coef"], se = s[, "robust se"],
    n_choices = length(unique(data$stratum)), n_respondents = length(unique(data$respondent_id))
  )
}

# Conditional-logit coefficients are refit on respondent resamples with benchmark
# shares redrawn from their sampling distribution, so the interval reflects both
# the MTurk sample and the benchmark survey.
choice_bootstrap <- function(guesses, benchmarks_spec, relation_filter, reps = bootstrap_reps,
                             seed = bootstrap_seed) {
  set.seed(seed)
  ids <- unique(guesses$respondent_id[guesses$relation %in% relation_filter])
  draws <- vector("list", reps)
  for (r in seq_len(reps)) {
    truth_r <- benchmarks_spec |>
      group_by(party, attribute) |>
      mutate(share = {
        g <- stats::rgamma(n(), shape = pmax(share * n_eff, 1e-3))
        g / sum(g)
      }) |>
      ungroup() |>
      mutate(share = pmax(share, 1e-4))
    pick <- tibble(respondent_id = sample(ids, replace = TRUE)) |>
      mutate(copy = row_number())
    g <- guesses |>
      filter(relation %in% relation_filter) |>
      inner_join(pick, by = "respondent_id", relationship = "many-to-many") |>
      mutate(respondent_id = paste(respondent_id, copy))
    draws[[r]] <- fit_choice(choice_data(g, truth_r)) |>
      select(term, estimate) |>
      mutate(rep = r)
  }
  bind_rows(draws) |>
    group_by(term) |>
    summarise(lo = quantile(estimate, 0.025), hi = quantile(estimate, 0.975), .groups = "drop")
}

# Share of guesses landing on the most diagnostic category where it differs from the
# modal one: the cleanest contrast between accuracy and stereotyping.
conflict_cells <- function(scored, truth) {
  alt <- alternatives_table(truth)
  cells <- alt |>
    group_by(target, attribute) |>
    summarise(
      mode = category[is_mode][1], diagnostic = category[is_most_diagnostic][1],
      true_mode_share = share[is_mode][1], true_diagnostic_share = share[is_most_diagnostic][1],
      lr = exp(max(log_lr)), .groups = "drop"
    ) |>
    filter(mode != diagnostic)
  scored |>
    inner_join(cells, by = c("target", "attribute")) |>
    group_by(target, attribute, relation, mode, diagnostic, true_mode_share, true_diagnostic_share, lr) |>
    summarise(
      n = n(), chose_mode = mean(guess == mode), chose_diagnostic = mean(guess == diagnostic),
      .groups = "drop"
    )
}

# Projection: do respondents guess their own characteristics? Compared within
# attribute across respondents who differ on that characteristic.
projection_effects <- function(scored, survey, respondents) {
  d <- survey[match(respondents$respondent_id, survey$uniqid), ]
  own <- tibble(
    respondent_id = respondents$respondent_id,
    sex = case_when(d$gender == "Male" ~ "Man", d$gender == "Female" ~ "Woman"),
    # Own age was asked as 18-29 / 30-49 / 50+; 30-49 straddles two guess bins.
    age = case_when(d$age == "18-29" ~ "18-35", d$age == "50+" ~ "50+"),
    education = case_when(
      d$educ == "High school degree or equivalent" ~ "Never went to college",
      d$educ != "" ~ "College educated"
    ),
    race = case_when(
      d$race == "White/Caucasian" & d$hispanic == "FALSE" ~ "White",
      d$race == "Black/African American" ~ "Black or African-American",
      d$race == "Asian" ~ "Asian"
    )
  ) |>
    pivot_longer(-respondent_id, names_to = "attribute", values_to = "own")
  scored |>
    filter(relation != "Independent") |>
    inner_join(own, by = c("respondent_id", "attribute")) |>
    filter(!is.na(own)) |>
    tidyr::crossing(category = unique(unlist(attribute_levels[c("sex", "age", "education", "race")]))) |>
    filter(mapply(function(a, k) k %in% attribute_levels[[a]], attribute, category)) |>
    mutate(guessed = as.integer(guess == category), is_own = as.integer(own == category)) |>
    group_by(attribute, relation) |>
    group_modify(function(df, key) {
      fit <- estimatr::lm_robust(guessed ~ is_own + factor(category),
        data = df, clusters = respondent_id,
        se_type = "CR2"
      )
      tibble(
        estimate = fit$coefficients[["is_own"]], se = fit$std.error[["is_own"]],
        lo = fit$conf.low[["is_own"]], hi = fit$conf.high[["is_own"]], n = length(unique(df$respondent_id))
      )
    }) |>
    ungroup()
}

# Randomised order: whether the target was asked about first or second.
order_effects <- function(scored) {
  per_target <- scored |>
    group_by(respondent_id, target, asked_second) |>
    summarise(diagnostic = sum(is_most_diagnostic), modal = sum(is_mode), .groups = "drop")
  pooled <- per_target |>
    group_by(target) |>
    group_modify(function(df, key) {
      bind_rows(lapply(c("diagnostic", "modal"), function(y) {
        fit <- estimatr::lm_robust(stats::reformulate("asked_second", y), data = df, se_type = "HC2")
        tibble(
          outcome = y, estimate = fit$coefficients[["asked_secondTRUE"]],
          se = fit$std.error[["asked_secondTRUE"]], lo = fit$conf.low[["asked_secondTRUE"]],
          hi = fit$conf.high[["asked_secondTRUE"]], p = fit$p.value[["asked_secondTRUE"]],
          mean_first = mean(df[[y]][!df$asked_second]), n = nrow(df)
        )
      }))
    }) |>
    ungroup()
  by_attribute <- scored |>
    group_by(target, attribute) |>
    group_modify(function(df, key) {
      fit <- estimatr::lm_robust(as.numeric(is_most_diagnostic) ~ asked_second, data = df, se_type = "HC2")
      tibble(
        estimate = fit$coefficients[["asked_secondTRUE"]], se = fit$std.error[["asked_secondTRUE"]],
        p = fit$p.value[["asked_secondTRUE"]], mean_first = mean(df$is_most_diagnostic[!df$asked_second]),
        n = nrow(df)
      )
    }) |>
    ungroup() |>
    mutate(p_holm = stats::p.adjust(p, "holm"))
  list(pooled = pooled, by_attribute = by_attribute)
}

order_balance <- function(respondents) {
  respondents |>
    mutate(democrat = party == "Democrat", republican = party == "Republican") |>
    group_by(dem_first) |>
    summarise(
      n = n(), democrat = mean(democrat), republican = mean(republican),
      strength = mean(strength), ideology = mean(ideology, na.rm = TRUE), .groups = "drop"
    )
}

# Association between stereotyped out-party guesses, the extremity of out-party
# leaders that come to mind, and how extreme the out-party is perceived to be.
extremity_models <- function(scored, respondents) {
  counts <- scored |>
    filter(relation == "Out-party") |>
    group_by(respondent_id) |>
    summarise(out_diagnostic = sum(is_most_diagnostic), out_answered = n(), .groups = "drop")
  d <- respondents |>
    filter(party %in% c("Democrat", "Republican")) |>
    inner_join(counts, by = "respondent_id") |>
    filter(out_answered == length(attribute_levels), !is.na(out_extremity))
  specs <- list(
    stereotypes = out_extremity ~ out_diagnostic + strength + party,
    recall = out_extremity ~ out_recall_extremity + strength + party,
    both = out_extremity ~ out_diagnostic + out_recall_extremity + strength + party
  )
  fits <- lapply(names(specs), function(nm) {
    fit <- estimatr::lm_robust(specs[[nm]], data = d, se_type = "HC2")
    tibble(
      model = nm, term = names(fit$coefficients), estimate = fit$coefficients, se = fit$std.error,
      lo = fit$conf.low, hi = fit$conf.high, p = fit$p.value, n = fit$nobs
    )
  })
  list(data = d, coefficients = bind_rows(fits))
}

# Out-party minus in-party share choosing the most diagnostic category, in cells where
# that category is not the modal one. Respondents answer each cell once, so the
# pooled estimate clusters by respondent and compares within cell.
diagnostic_gap <- function(scored, truth) {
  alt <- alternatives_table(truth)
  cells <- alt |>
    filter(is_most_diagnostic, !is_mode) |>
    select(target, attribute, diagnostic = category, true_share = share, lr = log_lr) |>
    mutate(lr = exp(lr))
  d <- scored |>
    filter(relation %in% c("In-party", "Out-party")) |>
    inner_join(cells, by = c("target", "attribute")) |>
    mutate(
      chose = as.numeric(guess == diagnostic),
      out = as.numeric(relation == "Out-party"),
      cell = paste(target, attribute)
    )
  by_cell <- d |>
    group_by(target, attribute, diagnostic, true_share, lr) |>
    group_modify(function(df, key) {
      fit <- estimatr::lm_robust(chose ~ out, data = df, se_type = "HC2")
      tibble(
        in_party = mean(df$chose[df$out == 0]), out_party = mean(df$chose[df$out == 1]),
        gap = fit$coefficients[["out"]], lo = fit$conf.low[["out"]], hi = fit$conf.high[["out"]],
        n_in = sum(df$out == 0), n_out = sum(df$out == 1)
      )
    }) |>
    ungroup()
  pooled_fit <- function(df, formula = chose ~ out + factor(cell)) {
    fit <- estimatr::lm_robust(formula, data = df, clusters = respondent_id, se_type = "CR2")
    tibble(
      gap = fit$coefficients[["out"]], lo = fit$conf.low[["out"]], hi = fit$conf.high[["out"]],
      cells = length(unique(df$cell)), n = length(unique(df$respondent_id))
    )
  }
  pooled <- bind_rows(
    pooled_fit(d) |> mutate(sample = "All cells"),
    pooled_fit(filter(d, attribute != "race")) |> mutate(sample = "Without race"),
    pooled_fit(filter(d, !(attribute == "union" & target == "Democrat"))) |>
      mutate(sample = "Without Democratic union item"),
    pooled_fit(d, chose ~ out + party + factor(cell)) |> mutate(sample = "Adjusting for respondent party")
  )
  list(by_cell = by_cell, pooled = pooled)
}
