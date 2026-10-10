tex <- readLines(project_file("ms", "main.tex"))
macros <- readLines(table_path("macros.tex"))
defined <- sub("^\\\\newcommand\\{\\\\([A-Za-z]+)\\}.*$", "\\1", macros)

test_that("every macro used in the manuscript is generated", {
  body <- paste(tex[!grepl("^\\\\(usepackage|newcommand|input|documentclass)", tex)], collapse = "\n")
  used <- unique(regmatches(body, gregexpr("\\\\[A-Za-z]+", body))[[1]])
  used <- sub("^\\\\", "", used)
  # Generated macros are camelCase; LaTeX commands in the manuscript are lowercase.
  candidates <- setdiff(used[grepl("[a-z][A-Z]", used) | used == "gap"], "TableStyle")
  expect_setequal(setdiff(candidates, defined), character(0))
  expect_setequal(setdiff(defined, used), character(0))
})

test_that("manuscript prose contains no hand-typed decimals", {
  body <- tex[!grepl("^\\\\", tex)]
  decimals <- unlist(regmatches(body, gregexpr("[^\\\\{A-Za-z][0-9]+\\.[0-9]+", body)))
  expect_length(decimals, 0)
})
