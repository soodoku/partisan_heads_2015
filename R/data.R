# Survey preparation. The Qualtrics export stores each guess in one of two blocks:
# columns without a suffix come from the block that asked about the Democrat first,
# columns ending in ".1" from the block that asked about the Republican first.

guess_stems <- c(
  rich = "class", rel = "religion", race = "race", sex = "sex", age = "age",
  educ = "education", employ = "employment", union = "union"
)

# Answer options differ in capitalisation between blocks ("College Educated" vs
# "College educated"); everything is mapped onto the labels in attribute_levels.
canonical_guess <- function(x) {
  x <- trimws(x)
  x[x == ""] <- NA
  lower <- tolower(x)
  lookup <- unlist(attribute_levels, use.names = FALSE)
  out <- lookup[match(lower, tolower(lookup))]
  stopifnot(identical(is.na(out), is.na(x)))
  out
}

ideology_scale <- c(
  "Very liberal" = 1, "Somewhat liberal" = 2, "Lean liberal" = 3, "Strictly moderate" = 4,
  "Lean conservative" = 5, "Somewhat conservative" = 6, "Very conservative" = 7
)

read_survey <- function(path = data_path("survey_2013.csv")) {
  utils::read.csv(path, stringsAsFactors = FALSE)
}

# Sample flow: eligible (passed the party quota screen), then assigned to a
# prototype block. Every eligible worker ID is unique, so no duplicate rule applies.
sample_flow <- function(survey) {
  eligible <- survey$allowed %in% 1
  assigned <- eligible & (survey$demfirst %in% 1 | survey$repfirst %in% 1)
  tibble(
    step = c("Survey records", "Passed the party quota screen", "Assigned to a prototype block"),
    n = c(nrow(survey), sum(eligible), sum(assigned))
  )
}

prepare_respondents <- function(survey) {
  stopifnot(!anyDuplicated(survey$uniqid))
  d <- survey |>
    filter(allowed %in% 1, demfirst %in% 1 | repfirst %in% 1)
  stopifnot(!anyDuplicated(d$worker_index), all(xor(d$demfirst %in% 1, d$repfirst %in% 1)))
  branch <- dplyr::coalesce(
    dplyr::na_if(d$lib_branch, ""), dplyr::na_if(d$con_branch, ""), dplyr::na_if(d$mod_branch, "")
  )
  tibble(
    respondent_id = d$uniqid,
    party = d$pid3,
    pid7 = as.numeric(d$pid7),
    dem_first = d$demfirst %in% 1,
    ideology = unname(ideology_scale[branch]),
    dem_placement = as.numeric(d$rdlibcon_1),
    rep_placement = as.numeric(d$rdlibcon_2)
  ) |>
    mutate(
      strength = abs(pid7 - 4),
      out_party = case_when(party == "Democrat" ~ "Republican", party == "Republican" ~ "Democrat"),
      out_placement = case_when(party == "Democrat" ~ rep_placement, party == "Republican" ~ dem_placement),
      out_extremity = abs(out_placement - 4),
      perceived_gap = rep_placement - dem_placement
    )
}

prepare_guesses <- function(survey, respondents) {
  d <- survey[match(respondents$respondent_id, survey$uniqid), ]
  rows <- list()
  for (target in c("dem", "rep")) {
    for (stem in names(guess_stems)) {
      dem_block <- d[[paste0(target, stem)]]
      rep_block <- d[[paste0(target, stem, ".1")]]
      rows[[length(rows) + 1]] <- tibble(
        respondent_id = respondents$respondent_id,
        target = if (target == "dem") "Democrat" else "Republican",
        attribute = unname(guess_stems[stem]),
        guess = canonical_guess(ifelse(respondents$dem_first, dem_block, rep_block)),
        other_block = canonical_guess(ifelse(respondents$dem_first, rep_block, dem_block))
      )
    }
  }
  out <- bind_rows(rows) |>
    left_join(select(respondents, respondent_id, party, dem_first), by = "respondent_id") |>
    mutate(
      relation = case_when(
        party == "Independent" ~ "Independent",
        party == target ~ "In-party",
        .default = "Out-party"
      ),
      asked_second = (target == "Republican") == dem_first
    )
  # A respondent sees only one block, so the other block must be empty.
  stopifnot(all(is.na(out$other_block)))
  out |>
    select(-other_block)
}

# Out-party leaders named in free recall, scored with the static CF-scores used in
# extreme_recall; positive values point toward the out-party's ideological pole.
recall_extremity <- function(survey, respondents, crosswalk, scores) {
  normalize <- function(x) tolower(gsub("\\s+", " ", trimws(x)))
  crosswalk <- crosswalk |>
    filter(response_type == "free_recall") |>
    mutate(response = normalize(response), name = normalize(name))
  stopifnot(!anyDuplicated(select(crosswalk, recall_party, response)))
  scores <- scores |>
    mutate(name = normalize(name)) |>
    distinct()
  stopifnot(!anyDuplicated(scores$name))
  d <- survey[match(respondents$respondent_id, survey$uniqid), ]
  mentions <- bind_rows(lapply(c("dem", "rep"), function(p) {
    bind_rows(lapply(1:3, function(i) {
      tibble(
        respondent_id = respondents$respondent_id, recall_party = p,
        response = normalize(d[[paste0(p, "_ex_", i)]])
      )
    }))
  })) |>
    filter(response != "") |>
    left_join(select(crosswalk, recall_party, response, name), by = c("recall_party", "response")) |>
    left_join(scores, by = "name") |>
    mutate(direction = if_else(recall_party == "dem", -cfscore, cfscore))
  mentions |>
    left_join(select(respondents, respondent_id, party), by = "respondent_id") |>
    filter(
      (party == "Democrat" & recall_party == "rep") | (party == "Republican" & recall_party == "dem")
    ) |>
    group_by(respondent_id) |>
    summarise(
      out_recall_named = n(),
      out_recall_scored = sum(!is.na(direction)),
      out_recall_extremity = if (any(!is.na(direction))) mean(direction, na.rm = TRUE) else NA_real_,
      .groups = "drop"
    )
}
