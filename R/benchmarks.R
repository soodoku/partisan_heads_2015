# Population benchmarks for the eight attributes respondents guessed. The CCES and GSS
# recodes are written separately so that an error in one cannot reproduce in the other.
# Category labels match the answer options in docs/instrument.pdf.

attribute_levels <- list(
  class = c("Poor", "Middle class", "Rich"),
  religion = c(
    "Born-again/Evangelical Christian", "Other Protestant Christian", "Catholic",
    "Jewish", "Atheist or agnostic", "Something else"
  ),
  race = c("White", "Black or African-American", "Asian"),
  sex = c("Man", "Woman"),
  age = c("18-35", "36-50", "50+"),
  education = c("College educated", "Never went to college"),
  employment = c("Employed", "Unemployed"),
  union = c("Union member", "Not a union member")
)

attribute_labels <- c(
  class = "Class", religion = "Religion", race = "Race", sex = "Sex", age = "Age",
  education = "Education", employment = "Employment", union = "Union membership"
)

age_bin <- function(age) {
  cut(age, c(17, 35, 50, Inf), labels = attribute_levels$age)
}

# Each recode returns one row per respondent with party, weight and the eight
# attributes; NA marks a respondent outside an attribute's denominator.
recode_cces <- function(cces, leaners = FALSE, variant = "primary") {
  num <- function(x) suppressWarnings(as.numeric(x))
  d <- cces |> mutate(across(everything(), num))
  party <- if (leaners) {
    case_when(d$pid7 %in% 1:3 ~ "Democrat", d$pid7 %in% 5:7 ~ "Republican")
  } else {
    case_when(d$pid3 == 1 ~ "Democrat", d$pid3 == 2 ~ "Republican")
  }
  born_again_set <- if (variant == "born_again_catholic") c(1, 2) else 1
  tibble(
    party = party,
    weight = d$V103,
    class = case_when(
      d$faminc %in% 1:2 ~ "Poor",
      d$faminc %in% c(14:16, 18) ~ "Rich",
      d$faminc %in% c(3:13, 31) ~ "Middle class"
    ),
    religion = case_when(
      d$religpew %in% born_again_set & d$pew_bornagain == 1 ~ "Born-again/Evangelical Christian",
      d$religpew == 1 & d$pew_bornagain == 2 ~ "Other Protestant Christian",
      d$religpew == 2 ~ "Catholic",
      d$religpew == 5 ~ "Jewish",
      d$religpew %in% 9:10 ~ "Atheist or agnostic",
      d$religpew %in% c(3, 4, 6, 7, 8, 11, 12) ~ "Something else"
    ),
    race = case_when(
      d$race == 1 ~ "White",
      d$race == 3 & variant == "hispanic_white" ~ "White",
      d$race == 2 ~ "Black or African-American",
      d$race == 4 ~ "Asian"
    ),
    sex = case_when(d$gender == 1 ~ "Man", d$gender == 2 ~ "Woman"),
    age = as.character(age_bin(ifelse(d$birthyr > 1900 & d$birthyr < 2000, 2012 - d$birthyr, NA))),
    education = if (variant == "degree") {
      case_when(d$educ %in% 5:6 ~ "College educated", d$educ %in% 1:2 ~ "Never went to college")
    } else {
      case_when(d$educ %in% 3:6 ~ "College educated", d$educ %in% 1:2 ~ "Never went to college")
    },
    employment = if (variant == "all_adults") {
      case_when(d$employ %in% 1:2 ~ "Employed", d$employ %in% 3:9 ~ "Unemployed")
    } else {
      case_when(d$employ %in% 1:2 ~ "Employed", d$employ %in% 3:4 ~ "Unemployed")
    },
    union = case_when(d$union == 1 ~ "Union member", d$union %in% 2:3 ~ "Not a union member")
  ) |>
    filter(!is.na(party))
}

recode_gss <- function(gss, leaners = FALSE, variant = "primary") {
  party <- if (leaners) {
    case_when(gss$partyid %in% 0:2 ~ "Democrat", gss$partyid %in% 4:6 ~ "Republican")
  } else {
    case_when(gss$partyid %in% 0:1 ~ "Democrat", gss$partyid %in% 5:6 ~ "Republican")
  }
  protestant <- gss$relig %in% c(1, 11, 13)
  tibble(
    party = party,
    weight = gss$wtssall,
    class = case_when(
      gss$class == 1 ~ "Poor",
      gss$class %in% 2:3 ~ "Middle class",
      gss$class == 4 ~ "Rich"
    ),
    religion = case_when(
      is.na(gss$god) ~ NA_character_,
      protestant & gss$reborn == 1 ~ "Born-again/Evangelical Christian",
      gss$relig == 2 & gss$reborn == 1 & variant == "born_again_catholic" ~ "Born-again/Evangelical Christian",
      protestant & gss$reborn == 2 ~ "Other Protestant Christian",
      gss$relig == 2 ~ "Catholic",
      gss$relig == 3 ~ "Jewish",
      gss$relig == 4 & gss$god %in% 1:2 ~ "Atheist or agnostic",
      gss$relig == 4 & gss$god %in% 3:6 ~ "Something else",
      gss$relig %in% c(5:10, 12) ~ "Something else"
    ),
    race = case_when(
      gss$racecen1 == 1 ~ "White",
      gss$racecen1 == 16 & variant == "hispanic_white" ~ "White",
      gss$racecen1 == 2 ~ "Black or African-American",
      gss$racecen1 %in% 4:10 ~ "Asian"
    ),
    sex = case_when(gss$sex == 1 ~ "Man", gss$sex == 2 ~ "Woman"),
    age = as.character(age_bin(gss$age)),
    education = if (variant == "degree") {
      case_when(
        gss$degree %in% 3:4 ~ "College educated",
        gss$degree %in% 0:1 & gss$educ <= 12 ~ "Never went to college"
      )
    } else {
      case_when(gss$educ >= 13 ~ "College educated", gss$educ <= 12 ~ "Never went to college")
    },
    employment = if (variant == "all_adults") {
      case_when(gss$wrkstat %in% 1:3 ~ "Employed", gss$wrkstat %in% 4:8 ~ "Unemployed")
    } else {
      case_when(gss$wrkstat %in% 1:3 ~ "Employed", gss$wrkstat == 4 ~ "Unemployed")
    },
    union = case_when(gss$union %in% c(1, 3) ~ "Union member", gss$union %in% c(2, 4) ~ "Not a union member")
  ) |>
    filter(!is.na(party), !is.na(weight))
}

# Weighted shares with Kish effective sample sizes for the standard errors.
weighted_shares <- function(recoded) {
  recoded |>
    pivot_longer(all_of(names(attribute_levels)), names_to = "attribute", values_to = "category") |>
    filter(!is.na(category)) |>
    group_by(party, attribute) |>
    mutate(n_eff = sum(weight)^2 / sum(weight^2), n = n()) |>
    group_by(party, attribute, category, n, n_eff) |>
    summarise(w = sum(weight), .groups = "drop") |>
    group_by(party, attribute) |>
    mutate(share = w / sum(w)) |>
    ungroup() |>
    complete(nesting(party, attribute, n, n_eff), category, fill = list(w = 0, share = 0)) |>
    filter(is_offered_category(attribute, category)) |>
    mutate(se = sqrt(share * (1 - share) / n_eff)) |>
    select(party, attribute, category, share, se, n, n_eff)
}

is_offered_category <- function(attribute, category) {
  mapply(function(a, k) k %in% attribute_levels[[a]], attribute, category)
}

benchmark_specs <- tibble::tribble(
  ~spec, ~leaners, ~variant,
  "primary", FALSE, "primary",
  "leaners", TRUE, "primary",
  "degree", FALSE, "degree",
  "all_adults", FALSE, "all_adults",
  "born_again_catholic", FALSE, "born_again_catholic",
  "hispanic_white", FALSE, "hispanic_white"
)

build_benchmarks <- function(cces, gss) {
  rows <- lapply(seq_len(nrow(benchmark_specs)), function(i) {
    s <- benchmark_specs[i, ]
    bind_rows(
      weighted_shares(recode_cces(cces, s$leaners, s$variant)) |> mutate(source = "CCES"),
      weighted_shares(recode_gss(gss, s$leaners, s$variant)) |> mutate(source = "GSS")
    ) |>
      mutate(spec = s$spec)
  })
  bind_rows(rows) |>
    relocate(source, spec)
}

# Truth used in the main analysis: CCES for every attribute except class, which the
# CCES does not ask; class comes from the GSS subjective-class question.
truth_table <- function(benchmarks, spec_name = "primary", class_source = "GSS", other_source = "CCES") {
  benchmarks |>
    filter(spec == spec_name) |>
    filter((attribute == "class" & source == class_source) | (attribute != "class" & source == other_source)) |>
    select(party, attribute, category, share, se, n_eff)
}
