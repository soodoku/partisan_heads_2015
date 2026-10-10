source(file.path(rprojroot::find_root(rprojroot::has_file("DESCRIPTION")), "scripts", "00_config.R"))

read_derived <- function(name) readr::read_csv(data_path(name), show_col_types = FALSE)
read_table <- function(name) readr::read_csv(table_path(name), show_col_types = FALSE)
