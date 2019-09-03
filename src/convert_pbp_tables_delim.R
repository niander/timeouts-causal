projroot <- rprojroot::find_root(rprojroot::is_rstudio_project)

library(fs)
library(purrr)
library(readr)

tables.path <- path(projroot, "data", "pbp")
seasons.path <- dir_ls(tables.path, regexp = "\\-old", invert = TRUE)

seasons.path %>% walk(function(season.path) {
  files <- dir_ls(season.path)
  tables <- files %>%
    map(~ read_delim(.x, delim = " ", col_types = "cccccc"))
  tables %>% 
    iwalk(~ write_csv(.x, .y, na = ""))
})


## Equality Test
files <- dir_ls(seasons.path[1])
tables <- map(files, ~ read_delim(.x, delim = " ", col_types = "cccccc"))
tables %>% iwalk(~ write_csv(.x, path(projroot, "test_new_tables", basename(.y)), na = ""))

old.tables <- tables
tables <- map(path(projroot, "test_new_tables", basename(files)), ~ read_csv(.x, col_types = "cccccc"))
names(old.tables) <- NULL

all.equal(old.tables, tables)
