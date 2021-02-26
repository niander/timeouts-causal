library(ProjectTemplate)
load.project(munging = FALSE, cache_loading = FALSE)

source("src/results_functions.R")

methods <- c("nobal", "maha", "propensity.nocalip")
methods_last5min <- str_c(methods, ".last5min")
methods_but5min <- str_c(methods, ".but5min")
all_methods <- c(methods, methods_last5min, methods_but5min)
JoinAllMatchesAndDeltaWithData(years = c(2017), methods = all_methods, data = res.timeout.effect.data)

all.mat <- env_get_list(global_env(), nms = str_c("mat", all_methods, sep = ".")) %>%
  modify(~ drop_na(., match.id)) %>%
  set_names(all_methods) %>%
  bind_rows(.id = "method") %>%
  mutate(method = fct_relevel(method, !!!all_methods))

tef.data <- res.timeout.effect.data
ClearWorkspaceWithLoadedDatasets()

all.permut <- LoadObject(folder = path("reports", "data"), name = "all.permutations.10m.rds")
last5min.permut <- LoadObject(folder = path("reports", "data"), name = "last5min.permutations.10m.rds")
but5min.permut <- LoadObject(folder = path("reports", "data"), name = "but5min.permutations.10m.rds")

BrewTableAllResults <- function(tab, suffix.file = NULL, themethods = methods) {
  tab %<>% mutate_if(is.numeric, round, digits = 3)
  deltas <- config$deltas
  tab.methods <- themethods %>%
    as_list() %>%
    set_names(themethods) %>%
    list_modify(unmatch = "Before Matching", nobal = "No-balance", 
                maha = "Mahalanobis", propensity.nocalip = "Propensity",
                propensity = "Propensity with caliper") %>%
    squash_chr() %>%
    extract(themethods)
  
  brew(file=path(rprojroot::find_root(rprojroot::is_rstudio_project), "reports", "all.results.brew"),
       output =  path("reports", str_c("latex.results.new", suffix.file, "txt", sep = ".")))
}

results.table <- all.permut %>%
  GetPValues(get.ci = T, filter.home = F, separate.effects = F)

results.table %<>%
  pivot_wider(names_from = match.poss, values_from = c(obs.diff, ci.min, ci.max, p_value), names_sep = "_")

BrewTableAllResults(results.table)


last5min.table <- last5min.permut %>%
  mutate(method = str_remove(method, ".last5min")) %>%
  GetPValues(get.ci = T, filter.home = F, separate.effects = F) %>%
  pivot_wider(names_from = match.poss, values_from = c(obs.diff, ci.min, ci.max, p_value), names_sep = "_")

BrewTableAllResults(last5min.table, suffix.file = "last5min")


but5min.table <- but5min.permut %>%
  mutate(method = str_remove(method, ".but5min")) %>%
  GetPValues(get.ci = T, filter.home = F, separate.effects = F) %>%
  pivot_wider(names_from = match.poss, values_from = c(obs.diff, ci.min, ci.max, p_value), names_sep = "_")

BrewTableAllResults(but5min.table, suffix.file = "but5min")
