suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(survival)
})

project_root <- rprojroot::find_root(rprojroot::has_file("DESCRIPTION"))
project_file <- function(...) file.path(project_root, ...)
data_path <- function(...) project_file("data", ...)
table_path <- function(...) project_file("tabs", ...)
figure_path <- function(...) project_file("figs", ...)

sha256_file <- function(path) digest::digest(file = path, algo = "sha256")

for (f in list.files(project_file("R"), pattern = "\\.R$", full.names = TRUE)) source(f)

bootstrap_reps <- 1000L
bootstrap_seed <- 20131113L
