library(ProjectTemplate)
invisible(rlang::locally(source("src/causal_inference.R", local = T)))
source("src/results_functions.R")

#methods <- c("nobal", "maha", "maha.replace", "maha.noquarter", "propensity", 
#             "propensity.strat", "propensity.nocalip")
methods <- c("nobal", "maha", "propensity.nocalip", "propensity")
JoinAllMatchesAndDeltaWithData("2017",
                               methods = methods,
                               data = res.timeout.effect.data)
all.mat.2017 <- env_get_list(global_env(), nms = str_c("mat", methods, "2017", sep = ".")) %>%
  modify(~ drop_na(., match.id)) %>%
  set_names(methods) %>%
  bind_rows(.id = "method") %>%
  mutate(method = fct_relevel(method, !!!methods))



all.mat.data.valid <- all.mat.2017 %>%
  group_by(method, delta, season, match.poss, match.id) %>%
  summarise(toremove = ifelse(abs(poss.id[[1]] - poss.id[[2]]) < (2 * delta[[1]] + 1), TRUE, FALSE)) %>%
  ungroup() %>%
  filter(toremove) %>%
  select(-toremove) %>%
  anti_join(all.mat.2017, .)

all.mat.data.valid %>%
  mutate(infl.score = if_else(match.poss == "away", -infl.score, infl.score)) %>%
  dlply(.(method, delta, match.poss), RunInferPermutation, 
        .parallel = T) %>%
  RIPListToDataFrame() %>%
  GetPValues(get.ci = F, filter.home = F) %>%
  nest(obs.diff, p_value, .key = "dn") %>%
  spread(match.poss, dn) %>%
  unnest(.sep = ".") %>%
  print(n=1000)

all.mat.2017 %>%
  mutate(infl.score = if_else(match.poss == "away", -infl.score, infl.score)) %>%
  dlply(.(method, delta, match.poss), RunInferPermutation, 
        .parallel = T) %>%
  RIPListToDataFrame() %>%
  GetPValues(get.ci = F, filter.home = F) %>%
  nest(obs.diff, p_value, .key = "dn") %>%
  spread(match.poss, dn) %>%
  unnest(.sep = ".") %>%
  print(n=1000)

all.mat.data.valid %>%
  mutate(infl.score = if_else(match.poss == "away", -infl.score, infl.score)) %>%
  ggplot(aes(infl.score, colour = treatment, fill = treatment)) +
  geom_density(adjust = 3, size = 0.8, alpha = 0.15) +
  facet_grid(method ~ delta, scales = "free")

all.mat.2017 %>%
  mutate(infl.score = if_else(match.poss == "away", -infl.score, infl.score)) %>%
  ggplot(aes(infl.score, colour = treatment, fill = treatment)) +
  geom_density(adjust = 3, size = 0.8, alpha = 0.15) +
  facet_grid(method ~ delta, scales = "free")


walk(methods, ~ PrintTableOnesUnify(all.mat.data.valid %>% filter(method == .x)))

walk(methods, ~ PrintTableOnesUnify(all.mat.2017 %>% filter(method == .x)))


all.mat.data.valid %>%
  filter(delta == 4) %>%
  PlotLines() %>%
  AddLegend()

all.mat.2017 %>%
  filter(delta == 4) %>%
  PlotLines() %>%
  AddLegend()

#############################

all.mat.2017 %>%
  filter(method == "propensity", delta == 4, match.poss == "home")

res <- all.mat.2017 %>%
  filter(method == "propensity", delta == 4, match.poss == "home") %>%
  select(treatment, infl.score, match.id) %>%
  mutate(match.id = factor(match.id)) %>%
  spread(treatment, infl.score) %>%
  mutate(effect = timeout - control)

hist(res$effect)

res %>%
  summarise(Mean = mean(effect),
         Sd = sd(effect),
         mean_sd = sqrt(2192)*(Mean/Sd))


res <- all.mat.2017 %>%
  filter(method == "propensity", delta == 2, match.poss == "home") %>%
  select(treatment, infl.score, match.id) %>%
  mutate(match.id = factor(match.id)) %>%
  spread(treatment, infl.score) %>%
  mutate(effect = timeout - control)

hist(res$effect)

res %>%
  summarise(Mean = mean(effect),
            Sd = sd(effect),
            mean_sd = sqrt(n())*(Mean/Sd))


res <- all.mat.2017 %>%
  filter(method == "propensity", delta == 6, match.poss == "home") %>%
  select(treatment, infl.score, match.id) %>%
  mutate(match.id = factor(match.id)) %>%
  spread(treatment, infl.score) %>%
  mutate(effect = timeout - control)

hist(res$effect)

res %>%
  summarise(Mean = mean(effect),
            Sd = sd(effect),
            mean_sd = sqrt(n())*(Mean/Sd))

# all.mat.2017 %>%
#   mutate(infl.score = if_else(match.poss == "away", -infl.score, infl.score)) %>%
#   select(method, delta, treatment, match.poss, match.id, infl.score) %>%
#   spread(treatment, infl.score) %>%
#   mutate(effect = timeout - control) %>%
#   d_ply(.(method, delta), function(df) {
#     df %>%
#       write_csv(path = str_c("reports/tables/", str_c(df$method[[1]], df$delta[[1]], "csv", sep = ".")))
#   })

###############

all.mat.2017 %>%
  group_by(method, delta, game.id, poss.id) %>%
  summarise(n = n()) %>%
  filter(n > 1) %>%
  group_by(method, delta) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  summarise(sum = sum(n))
  

#################

repeateds <- all.mat.2017 %>%
  group_by(method, delta, game.id, poss.id) %>%
  summarise(n = n()) %>%
  filter(n > 1) %>%
  select(-n)

controls.stay <- all.mat.2017 %>%
  filter(A == 0) %>%
  distinct(method, delta, game.id, poss.id, .keep_all = T) %>%
  select(method, delta, game.id, match.poss, match.id)

nodup.mat.2017 <- all.mat.2017 %>%
  semi_join(controls.stay)

nodup.mat.2017 %>%
  mutate(infl.score = if_else(match.poss == "away", -infl.score, infl.score)) %>%
  dlply(.(method, delta, match.poss), RunInferPermutation, 
        .parallel = T) %>%
  RIPListToDataFrame() %>%
  GetPValues(get.ci = F, filter.home = F, separate.effects = T) %>%
  print(n=1000)


nodup.mat.2017 %>%
  mutate(infl.score = if_else(match.poss == "away", -infl.score, infl.score)) %>%
  dlply(.(method, delta), RunInferPermutation, 
        .parallel = T) %>%
  RIPListToDataFrame() %>%
  GetPValues(get.ci = F, filter.home = F) %>%
  print(n=1000)

all.mat.2017 %>%
  mutate(infl.score = if_else(match.poss == "away", -infl.score, infl.score)) %>%
  dlply(.(method, delta), RunInferPermutation, 
        .parallel = T) %>%
  RIPListToDataFrame() %>%
  GetPValues(get.ci = F, filter.home = F) %>%
  print(n=1000)  

all.mat.2017 %>%
  mutate(infl.score = if_else(match.poss == "away", -infl.score, infl.score)) %>%
  filter(method == "propensity") %>%
  d_ply(.(delta), function(df) {
    cat("==============================\ndelta=", df$delta[[1]])
    df %<>% mutate_at(vars(A, match.id, match.poss), factor)
    #median_test(infl.score ~ A | match.id, data = df, distribution = approximate(B=1000, parallel = "multicore", ncpus = 4)) %>%
    #wilcox_test(infl.score ~ A, data = df, distribution = approximate(B=5000, parallel = "multicore", ncpus = 4)) %>%
    oneway_test(infl.score ~ A, data = df, distribution = "asymptotic") %>% #approximate(B=5000, parallel = "multicore", ncpus = 4)) %>%
      print()
  })

#########

controls.stay <- all.mat.2017 %>%
  mutate(pts = if_else(match.poss == "away", -pts, pts)) %>%
  filter(A == 0) %>%
  filter(pts < 0) %>%
  select(method, delta, game.id, match.poss, match.id)

noa.mat.2017 <- all.mat.2017 %>%
  semi_join(controls.stay)

noa.mat.2017 %>%
  mutate(infl.score = if_else(match.poss == "away", -infl.score, infl.score)) %>%
  dlply(.(method, delta, match.poss), RunInferPermutation, 
        .parallel = T) %>%
  RIPListToDataFrame() %>%
  GetPValues(get.ci = F, filter.home = F, separate.effects = T) %>%
  add_column(size = group_by(noa.mat.2017, method, delta) %>% summarise(n=n()) %>% pull()) %>%
  print(n=1000)

noa.mat.2017 %>%
  mutate(infl.score = if_else(match.poss == "away", -infl.score, infl.score)) %>%
  dlply(.(method, delta), RunInferPermutation, 
        .parallel = T) %>%
  RIPListToDataFrame() %>%
  GetPValues(get.ci = F, filter.home = F) %>%
  add_column(size = group_by(noa.mat.2017, method, delta) %>% summarise(n=n()) %>% pull()) %>%
  print(n=1000)

noa.mat.2017 %>%
  mutate(infl.score = if_else(match.poss == "away", -infl.score, infl.score)) %>%
  ggplot(aes(infl.score, colour = treatment, fill = treatment)) +
  geom_density(adjust = 0.5, size = 0.8, alpha = 0.15) +
  facet_grid(method ~ delta, scales = "free")

############

controls.stay <- all.mat.2017 %>%
  filter(A == 1) %>%
  filter(quarter <= 4) %>%
  select(method, delta, game.id, match.poss, match.id)

noot.mat.2017 <- all.mat.2017 %>%
  semi_join(controls.stay)

noot.mat.2017 %>%
  mutate(infl.score = if_else(match.poss == "away", -infl.score, infl.score)) %>%
  dlply(.(method, delta, match.poss), RunInferPermutation, 
        .parallel = T) %>%
  RIPListToDataFrame() %>%
  GetPValues(get.ci = F, filter.home = F, separate.effects = T) %>%
  add_column(size = group_by(noa.mat.2017, method, delta) %>% summarise(n=n()) %>% pull()) %>%
  print(n=1000)

noot.mat.2017 %>%
  mutate(infl.score = if_else(match.poss == "away", -infl.score, infl.score)) %>%
  dlply(.(method, delta), RunInferPermutation, 
        .parallel = T) %>%
  RIPListToDataFrame() %>%
  GetPValues(get.ci = F, filter.home = F) %>%
  add_column(size = group_by(noa.mat.2017, method, delta) %>% summarise(n=n()) %>% pull()) %>%
  print(n=1000)

noot.mat.2017 %>%
  mutate(infl.score = if_else(match.poss == "away", -infl.score, infl.score)) %>%
  ggplot(aes(infl.score, colour = treatment, fill = treatment)) +
  geom_density(adjust = 0.5, size = 0.8, alpha = 0.15) +
  facet_grid(method ~ delta, scales = "free")
