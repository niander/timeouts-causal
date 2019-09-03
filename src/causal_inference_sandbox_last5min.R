library(ProjectTemplate)
invisible(rlang::locally(source("src/causal_inference.R", local = T)))

methods <- c(str_c(c("nobal", "maha", "propensity", "propensity.nocalip"), ".last5min"))
JoinAllMatchesAndDeltaWithData(methods = methods,
                               data = res.timeout.effect.data)
all.mat.2017 <- env_get_list(global_env(), nms = str_c("mat", methods, "2017", sep = ".")) %>%
  modify(~ drop_na(., match.id)) %>%
  set_names(methods) %>%
  bind_rows(.id = "method") %>%
  mutate(method = fct_relevel(method, !!!methods))

source("src/results_functions.R")

#########

all.mat.2017 %>%
  filter(str_detect(method, "last5min")) %>%
  mutate(infl.score = if_else(match.poss == "away", -infl.score, infl.score)) %>%
  filter(season %in% c(2016, 2017), delta == 4) %>%
  PlotLines() %>%
  AddLegend(labels = c("nobal", "maha", "prop"))

all.mat.2017 %>%
  filter(str_detect(method, "last5min")) %>%
  mutate(infl.score = if_else(match.poss == "away", -infl.score, infl.score)) %>%
  filter(season %in% c(2017, 2018), delta == 4) %>%
  PlotLines() %>%
  AddLegend(labels = c("nobal", "maha", "prop"))

all.mat.2017 %>%
  filter(str_detect(method, "last5min")) %>%
  mutate(infl.score = if_else(match.poss == "away", -infl.score, infl.score)) %>%
  filter(season %in% c(2018), delta == 4) %>%
  PlotLines() %>%
  AddLegend(labels = c("nobal", "maha", "prop"))

all.mat.2017 %>%
  filter(str_detect(method, "last5min")) %>%
  mutate(infl.score = if_else(match.poss == "away", -infl.score, infl.score)) %>%
  filter(season %in% c(2015, 2016, 2017, 2018), delta == 4) %>%
  PlotLines() %>%
  AddLegend(labels = c("nobal", "maha", "prop"))

##########

last5min.data <- all.mat.2017 %>%
  filter(str_detect(method, "last5min")) %>%
  filter(season %in% c(2015, 2016, 2017, 2018)) %>%
  mutate(infl.score = if_else(match.poss == "away", -infl.score, infl.score)) 

last5min.data %>%
  ggplot(aes(infl.score, colour = treatment, fill = treatment)) +
  geom_density(adjust = 2.5, size = 0.8, alpha = 0.15) +
  facet_wrap(delta ~ method, scales = "free")

last5min.data %>%
  dlply(.(method, delta), RunInferPermutation,
        .parallel = T) %>%
  RIPListToDataFrame() %>%
  GetPValues(filter.home = FALSE)

PrintTableOnesUnify(mat.propensity.nocalip.last5min.2017 %>% drop_na(match.id) %>% filter(season == 2015))
PrintTableOnesUnify(mat.propensity.nocalip.last5min.2017 %>% drop_na(match.id) %>% filter(season == 2016)) 
PrintTableOnesUnify(mat.propensity.nocalip.last5min.2017 %>% drop_na(match.id) %>% filter(season == 2017))
PrintTableOnesUnify(mat.propensity.nocalip.last5min.2017 %>% drop_na(match.id) %>% filter(season == 2018))
PrintTableOnesUnify(mat.propensity.nocalip.last5min.2017 %>% drop_na(match.id))
PrintTableOnesUnify(mat.propensity.nocalip.last5min.2017 %>% drop_na(match.id) %>% filter(season %in% c(2015,2016,2017)))

last5min.data <- all.mat.2017 %>%
  filter(str_detect(method, "last5min")) %>%
  filter(season %in% c(2015, 2016, 2017)) %>%
  mutate(infl.score = if_else(match.poss == "away", -infl.score, infl.score)) 

last5min.data %>%
  ggplot(aes(infl.score, colour = treatment, fill = treatment)) +
  geom_density(adjust = 1, size = 0.8, alpha = 0.15) +
  facet_grid(method ~ delta, scales = "free")

last5min.data %>%
  dlply(.(method, delta, match.poss), RunInferPermutation,
        .parallel = T) %>%
  RIPListToDataFrame() %>%
  GetPValues(get.ci = F, filter.home = F, separate.effects = T)

# last5min.data <- all.mat.2017 %>%
#   filter(method == "propensity.nocalip.last5min") %>%
#   filter(season %in% c(2015, 2016, 2017, 2018)) %>%
#   mutate(infl.score = if_else(match.poss == "away", -infl.score, infl.score)) 

# last5min.onlyvalid.data <- last5min.data %>%
#   group_by(delta, match.poss, match.id) %>%
#   summarise(toremove = ifelse(abs(poss.id[[1]] - poss.id[[2]]) < (2 * delta[[1]] + 1), TRUE, FALSE)) %>%
#   ungroup() %>%
#   filter(toremove) %>%
#   select(-toremove) %>%
#   anti_join(last5min.data, .)
# 
# PrintTableOnesUnify(last5min.onlyvalid.data)

last5min.onlyvalid.data %>%
  ggplot(aes(infl.score, colour = treatment, fill = treatment)) +
  geom_density(adjust = 1, size = 0.8, alpha = 0.15) +
  facet_grid(method ~ delta, scales = "free")

last5min.onlyvalid.data %>%
  dlply(.(method, delta), RunInferPermutation,
        .parallel = T) %>%
  RIPListToDataFrame() %>%
  GetPValues(filter.home = FALSE)
