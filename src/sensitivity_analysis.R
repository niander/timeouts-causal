library(ProjectTemplate)
load.project(munging = FALSE, cache_loading = FALSE)

source("src/results_functions.R")

library(sensitivitymult)

methods =  c("nobal", "maha", "propensity.nocalip")
JoinAllMatchesAndDeltaWithData(years = c(2017), methods = methods, data = res.timeout.effect.data)

all.mat <- env_get_list(global_env(), nms = str_c("mat", methods, sep = ".")) %>%
  modify(~ drop_na(., match.id)) %>%
  set_names(methods) %>%
  bind_rows(.id = "method") %>%
  mutate(method = fct_relevel(method, !!!methods))

tef.data <- res.timeout.effect.data
ClearWorkspaceWithLoadedDatasets()

new_sens <- all.mat %>%
  filter(delta == 4) %>%
  {
    df <- .
    ldply(lst(1,1.5,2), function(gamma) {
      ddply(df, .(method, match.poss, delta), function(d) {
        a <- d %$% 
          senmCI(infl.score, A, match.id, inner = 0, trim = Inf, gamma = gamma, alpha = 0.01, twosided = T, TonT = T)
        tibble(p = list(a$PointEstimates), ci = list(a$ConfidenceInterval))
      }, .progress = "text")
    }, .id = "gamma") %>%
      as.tibble()
  } %>%
  mutate_at(vars(p, ci), ~ modify_depth(., 1, ~ set_names(., c("min", "max")))) %>%
  unnest_wider(p, names_sep = ".") %>%
  unnest_wider(ci, names_sep = ".")

sens_table <- sens %>%
  select(-p.min, -p.max) %>%
  pivot_wider(names_from = "match.poss",
              values_from = c("ci.min", "ci.max"),
              names_sep = "_") %>%
  mutate_if(is.numeric, round, digits = 3) %>%
  arrange(gamma, method)

sens_table

library(brew)

BrewTable <- function(tab) {
  gammas <- c(1,1.5,2,3)
  methods <- c("nobal", "maha", "propensity.nocalip")
  brew(file=path(rprojroot::find_root(rprojroot::is_rstudio_project), "src", "sensitivity.brew"),
       output =  path("reports", str_c("latex.sensitivity", "txt", sep = ".")))
  
}

BrewTable(sens_table)



# all.mat %>% 
#   filter(method == "propensity.nocalip") %>%
#   filter(delta == 4, season == 2017, match.poss == "home") %$%
#   senmCI(infl.score, A, match.id, trim = Inf, gamma = 10, twosided = T, TonT = T, alpha = 0.01)
#   #senm(infl.score, A, match.id, trim = Inf, gamma = 1, alternative = "less", TonT = T, )
# 
# mat.propensity.nocalip %>% filter(delta == 6, season == 2017, match.poss == "home") %>%
#   drop_na(match.id) %>%
#   RunInferPermutation()# %>%
#   #RIPListToDataFrame()
# 
# tdata <- mat.propensity.nocalip %>% filter(delta == 6, season == 2017, match.poss == "home") %>%
#   mutate_at(vars(A), factor) %>%
#   specify(infl.score ~ A)
# 
# tdata %>% calculate(stat = "diff in means", order = c("1", "0"))
# 
# tdata %>% group_by(A) %>% summarise(mean = mean(infl.score))