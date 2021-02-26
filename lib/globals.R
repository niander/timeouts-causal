# Add any project specific configuration here.

add.config(
  pbp_folder = "data/pbp",
  all_seasons = c(2015, 2016, 2017,2018),
  seasons = c(2015, 2016, 2017, 2018),
  deltas = c(2, 4, 6),
  results_folder = "data/res",
  clean_global_vars = TRUE,
  matching_seed = 222,
  apply.override = TRUE
)

all.seasons = c(2015, 2016, 2017, 2018)
exclude.season = all.seasons[!all.seasons %in% config$seasons] 
new.data_ignore <- paste(config$data_ignore, paste0("/\\.", exclude.season, "$/", collapse = ","), sep = ",")

#config$.override.config$data_ignore = new.data_ignore

#add.config(data_ignore = config$data_ignore,
#           apply.override = TRUE)

config$data_ignore <- new.data_ignore
