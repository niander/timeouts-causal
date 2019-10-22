library(ProjectTemplate)
invisible(rlang::locally(source("src/causal_inference.R", local = T)))

methods <- c("nobal", "maha", "maha.replace", "maha.noquarter", "propensity", 
            "propensity.strat", "propensity.nocalip")
JoinAllMatchesAndDeltaWithData(methods = methods,
                               data = res.timeout.effect.data)
all.mat.2017 <- env_get_list(global_env(), nms = str_c("mat", methods, "2017", sep = ".")) %>%
  modify(~ drop_na(., match.id)) %>%
  set_names(methods) %>%
  bind_rows(.id = "method") %>%
  mutate(method = fct_relevel(method, !!!methods))

####################
# Table Ones
source("src/results_functions.R")

PrintTableOnes(mat.propensity.strat.2017 %>% drop_na(match.id))
PrintTableOnes(mat.propensity.2017 %>% drop_na(match.id))
PrintTableOnes(mat.propensity.nocalip.2017 %>% drop_na(match.id))
PrintTableOnes(mat.maha.2017 %>% drop_na(match.id))
PrintTableOnes(mat.maha.noquarter.2017 %>% drop_na(match.id))

PrintTableOnesUnify(mat.maha.last5min.2017 %>% drop_na(match.id))
PrintTableOnesUnify(mat.propensity.nocalip.last5min.2017 %>% drop_na(match.id))

PrintTableOnesUnify(mat.propensity.nocalip.last5min.2017 %>% drop_na(match.id) %>% filter(season == 2016))
PrintTableOnesUnify(mat.propensity.nocalip.last5min.2017 %>% drop_na(match.id) %>% filter(season == 2017))
PrintTableOnesUnify(mat.propensity.nocalip.last5min.2017 %>% drop_na(match.id) %>% filter(season == 2018))



########################
# Covariate distributions plot
source("src/results_functions.R")

all.mat.2017 %>%
  filter(match.poss == "home", delta == 2) %>%
  PlotLines() %>%
  AddLegend()


# Causal Effect Plot
plot.data <- all.methods.data %>%
  drop_na(match.id) %>%
  filter(match.poss == "home")

theplot <- function(df, title) {
  plt <- ggplot(df, aes(infl.score, color = treatment, fill = treatment)) +
    geom_vline(xintercept = 0) +
    geom_density(alpha = 0.15, adjust = 3.5, size=0.6) +
    labs(title = title)
    return(plt + facet_grid(~ delta))
}

plot.data %>%
  theplot("Matched Distributions") +
  facet_grid(method ~ delta) +
  theme_gray()


################
library(twang)
data <- res.propensity.2017 %>%
    filter(delta == 4, match.poss == "home")
ps.lalonde <- ps(A ~ seconds + quarter + margin,
                 data = select(data, A, seconds, quarter, margin) %>% as.data.frame(),
                 n.trees=3000,
                 interaction.depth=2,
                 shrinkage=0.01,
                 perm.test.iters=0,
                 stop.method=c("es.mean","ks.max"),
                 estimand = "ATT",
                 verbose=TRUE)

datares <- res.maha.2017 %>%
  filter(delta == 4, match.poss == "home") %>%
  drop_na(match.id)

data <- mat.maha.2017 %>%
  filter(delta == 4, match.poss == "home") %>%
  drop_na(match.id)

mean(filter(data, A==1)$infl.score) - mean(filter(data, A==0)$infl.score)

data %>% group_by(team) %>%
  do(eff = mean(filter(., A==1)$infl.score) - mean(filter(., A==0)$infl.score),
     size = nrow(.)/2) %>%
  unnest(eff) %>%
  unnest(size)
                                           
ps.data <- ps(A ~ seconds + quarter + margin,
              data = select(data, A, seconds, quarter, margin) %>% as.data.frame(),
              n.trees = 2000,
              interaction.depth = 3,
              shrinkage=0.02,
              perm.test.iters=0,
              stop.method=c("es.mean","ks.max"),
              estimand = "ATT",
              verbose=TRUE)
plot(ps.data, plots=3)

library(survey)

data$w <- get.weights(ps.data, stop.method="es.mean")

design.ps <- svydesign(ids=~1, weights=~w, data=data)
glm1 <- svyglm(infl.score ~ A, design=design.ps)
summary(glm1)

weighted.mean(filter(data, A==1)$infl.score, filter(data, A==1)$w) - weighted.mean(filter(data, A==0)$infl.score, filter(data, A==0)$w)

res <- data %>% group_by(team) %>%
  do(eff = mean(filter(., A==1)$infl.score) - mean(filter(., A==0)$infl.score),
     eff.w = weighted.mean(filter(., A==1)$infl.score, filter(., A==1)$w) - weighted.mean(filter(., A==0)$infl.score, filter(., A==0)$w),
     size = nrow(.)/2) %>%
  unnest(eff) %>%
  unnest(eff.w) %>%
  unnest(size)

res <- mat.propensity.2017 %>%
  filter(delta == 4, match.poss == "home") %>%
  drop_na(match.id) %>%
  group_by(team) %>%
  do(eff.prop = mean(filter(., A==1)$infl.score) - mean(filter(., A==0)$infl.score)) %>%
  unnest(eff.prop) %>%
  left_join(res, ., by="team")

res %>%
  summarise(eff = weighted.mean(eff, size),
            eff.w = weighted.mean(eff.w, size),
            eff.prop = weighted.mean(eff.prop, size),
            size = sum(size))

print(res, n=30)

crossing(delta = c(2,4,6), type = c("home", "away")) %>%
  bind_cols(., 
            map2_dfr(.$delta, .$type, function(d, t) {
              map_dfc(list(nobal = mat.nobal.2017,
                           maha = mat.maha.2017, 
                           mahar = mat.maha.replace.2017, 
                           prop = mat.propensity.2017,
                           prop.strat = mat.propensity.strat.2017), function(df) {
                             df %>%
                               filter(delta == d, match.poss == t) %>%
                               drop_na(match.id) %>%
                               summarise(eff.treat = weighted.mean(infl.score, A == 1),
                                         eff.contr = weighted.mean(infl.score, A == 0),
                                         eff = eff.treat - eff.contr) %>%
                               pull(eff)
                           })
            }))


plt <- bind_rows(maha = mat.maha.2017, mahar = mat.maha.replace.2017, prop = mat.propensity.2017, .id = "method") %>%
  filter(match.poss == "home") %>%
  drop_na(match.id) %>%
  ggplot(aes(infl.score, color = treatment, fill = treatment)) +
  facet_wrap(delta ~method, scales = "free_y") +
  geom_vline(xintercept = 0) +
  geom_bar(alpha = 0.15, size = 0.6, position = "identity")
  #geom_density(alpha = 0.15, adjust = 2, size=0.6)
plot(plt)

# Investigating the distribution of the control in maha mathcing for delta=2
plt.data <- mat.maha.2017 %>%
  filter(match.poss == "home", delta == 2) %>%
  drop_na(match.id) %>%
  filter(A == 0) %>%
  mutate(is.zero = near(infl.score, 0.0))
ggplot(plt.data, aes(quarter)) +
  facet_wrap(~delta) +
  geom_bar(size = 0.6)
ggplot(plt.data, aes(seconds)) +
  facet_wrap(~delta) +
  geom_histogram()
ggplot(plt.data, aes(slope.before)) +
  facet_wrap(~delta) +
  geom_histogram()

# Investigating the distribution of the control that did not match in propensity

aux.prop <- mat.propensity.2017 %>%
  filter(match.poss == "home", is.na(match.id), A == 0)
aux.maha <- mat.maha.2017 %>%
  filter(match.poss == "home", is.na(match.id), A == 0)
plt.data <- anti_join(aux.prop, aux.maha, by = c("delta", "season", "game.id", "poss.id"))
ggplot(plt.data, aes(quarter)) +
  facet_wrap(~delta) +
  geom_bar(size = 0.6)
ggplot(plt.data, aes(seconds)) +
  facet_wrap(~delta) +
  geom_histogram()
ggplot(plt.data, aes(slope.before)) +
  facet_wrap(~delta) +
  geom_histogram()

# KS test for balance in delta == 2
plt.data <- mat.maha.2017 %>%
  filter(match.poss == "home", delta == 2) %>%
  drop_na(match.id) 
plt.data %$%
  ks.test(seconds[which(A==0)], seconds[which(A==1)], alternative = "greater")
plt.data %$%
  ks.test(quarter[which(A==0)], quarter[which(A==1)], alternative = "greater")

plt.data %>%
  ggplot(aes(seconds)) +
  stat_ecdf(aes(color = treatment), pad = T)
plt.data %>%
  ggplot(aes(margin)) +
  stat_ecdf(aes(color = treatment), pad = T)

plt.data %>%
  normal_test(seconds ~ as.factor(treatment), ., distribution = approximate(B = 10000))

mat.propensity.2017 %>%
  filter(match.poss == "home", delta == 6) %>%
  drop_na(match.id) %>%
  normal_test(seconds ~ as.factor(treatment), ., distribution = approximate(B = 10000))

plot.data %>%
  filter(A == 1) %>%
  left_join(filter(plot.data, A == 0), by = "match.id", suffix = c(".t", ".c")) %>%
  ggplot(aes(x = sort(seconds.c), y = sort(seconds.t))) +
  geom_point()


plt.data %$%
  qqplot(seconds[which(A==0)], seconds[which(A==1)])
plt.data %$%
  qqline(seconds[which(A==1)])

##########################
# ECDF of distributions
plt.data <- bind_rows(nobal = mat.nobal.2017,
                      maha = mat.maha.2017, 
                      prop = mat.propensity.2017,
                      .id = "method") %>%
  filter(match.poss == "home", delta == 2) %>%
  drop_na(match.id)

plts <- plt.data %>%
  group_by(method) %>%
  by_slice(function(df) {
    ggplot(df, aes(seconds)) +
      stat_ecdf(aes(color = treatment))
  })
plts$.out %>% walk2(plts$method, ~ plot(.x + labs(title = .y)))


###################################
# Some causal effect tests
# test.data <- bind_rows(nobal = mat.nobal.2017,
#                        maha = mat.maha.2017, 
#                        prop = mat.propensity.2017,
#                        .id = "method") %>%
#   filter(match.poss == "home", delta == 2) %>%
#   drop_na(match.id)

library(coin)

test.data <- all.mat.2017 %>%
  filter(match.poss == "home", delta == 4)

test.data %>%
  filter(method == "propensity.nocalip") %>%
  oneway_test(infl.score ~ factor(treatment), ., distribution = "asymptotic")

test.data %>%
  filter(method == "maha") %$%
  t.test(infl.score[which(A==0)], infl.score[which(A==1)])

oneway.test.res <- test.data %>%
  filter(method == "maha") %>%
  oneway_test(infl.score ~ factor(treatment), ., distribution = "asymptotic")

supp = support(oneway.test.res)
dens = dperm(oneway.test.res, x = supp)

ggplot(tibble(dens = dens, supp = supp), aes(supp)) +
  geom_density(adjust = 0.1)

plot(supp, dens)

ggplot(data.frame(x=0), aes(x=x)) +
  stat_function(fun = partial(dperm, object = oneway.test.res)) + xlim(-5,5)


matchit.out <- res.maha.replace.delta2.2017$m.out$home
plot(matchit.out)
matchit.out <- res.maha.replace.delta4.2017$m.out$home
plot(matchit.out)
matchit.out <- res.maha.replace.delta6.2017$m.out$home
plot(matchit.out)


res.maha.replace.delta6.2017$mat.data %>% 
  drop_na(match.id) %>% 
  group_by(strat) %>%
  summarise(n.treat = sum(which(A==1)),
            n.contr = sum(which(A==0)),
            n = n())

res.maha.replace.delta6.2017$mat.data %>%
  drop_na(match.id) %>%
  group_by(match.id, match.poss) %>%
  do(unique.strat = unique(.$strat)) %>%
  unnest(unique.strat)

# library(SVMMatch)
# data <- res.propensity.2017 %>%
#   filter(delta == 4, match.poss == "home") %>%
#   drop_na(match.id)
# 
# svm.match <- svmmatch(treat = data$A,
#                       X = select(data, quarter, seconds, margin) %>% 
#                         mutate_if(is.numeric, as.numeric) %>%
#                         as.matrix(),
#                       dv = data$infl.score)

#library(CausalGAM)

data <- res.maha.2017 %>% filter(delta == 4) %>% filter(match.poss == "home")
#dist.struct <- build.dist.struct(data$A, as.data.frame(select(data, quarter, seconds, margin)), data$strat, calip.option = "none")
m.out <- matchit(A ~ quarter + seconds + margin, data = as.data.frame(select(data, A, strat, quarter, seconds, margin)),
                 method = "nearest", distance = "mahalanobis", exact = "strat", 
                 discard = "both", caliper = 0.2)


mat.propensity.delta2.2017 <- mat.propensity.2017 %>% filter(delta == 2)
mat.propensity.delta2.2017$mat.data %>%
  ggplot(aes(x = ps.score, y = margin, color = as.factor(A))) +
  geom_smooth(method = "loess", se = F) +
  geom_point(alpha = 0.2, size = 1.3)

mat.propensity.delta2.2017$data %>%
  ggplot(aes(x = ps.score, y = seconds, color = as.factor(A))) +
  #geom_smooth(method = "loess", se = F) +
  geom_point(alpha = 0.2, size = 1.3)

library(foreach)
library(iterators)
foreach(df = iter(ll(mat.propensity.delta2.2017$data, mat.propensity.delta2.2017$mat.data)), .verbose = T)

foreach(df = iter(ll(mat.propensity.delta2.2017$data, mat.propensity.delta2.2017$mat.data)), .verbose = T) %do% {
  print(ggplot(df, aes(x = ps.score, y = seconds, color = as.factor(A))) + geom_smooth(method = "loess", se = F))
  NULL
}


data <- JoinMatchesWithData(mat.propensity.delta2.2017$data, timeout.effect.data, vars.from.treatment = NULL)
mat.data <- JoinMatchesWithData(mat.propensity.delta2.2017$mat.data, timeout.effect.data)

xvars = .(slope.before, quarter, seconds, margin)
tab.one <- CreateTableOne(vars = as.character(xvars), strata = c("A", "poss"), data = data, test = F)
tab.one
tab.one <- CreateTableOne(vars = as.character(xvars), strata = c("A", "poss"), data = mat.data, test = F)
tab.one

###################
data <- mat.maha.2017 %>% 
  filter(delta == 2, match.poss == "home") %>%
  drop_na(match.id)

ids.score.zero <- data %>%
  filter(A == 0, infl.score == 0) %>%
  sample_n(20) %>%
  select(game.id, poss.id)

ids.score.zero %>%
  left_join(`2017.new.games.df`, by = c("game.id", "poss.id")) %>%
  arrange(game.id, poss.id) %>%
  print(n=nrow(.))
  

ids.score.zero %>%
  bind_rows(., mutate(., poss.id = poss.id - 1), mutate(., poss.id = poss.id - 2), mutate(., poss.id = poss.id + 1), mutate(., poss.id = poss.id + 2)) %>%
  left_join(`2017.new.games.df`, by = c("game.id", "poss.id")) %>%
  arrange(game.id, poss.id) %>%
  print(n=nrow(.))

ids.score.zero %>%
  bind_rows(., mutate(., poss.id = poss.id - 1), mutate(., poss.id = poss.id - 2), mutate(., poss.id = poss.id + 1), mutate(., poss.id = poss.id + 2)) %>%
  left_join(`2017.games.perposs`, by = c("game.id", "poss.id")) %>%
  arrange(game.id, poss.id) %>%
  print(n=nrow(.))

##########

plt.data <- timeout.effect.data %>% 
  filter(season == "2017", treatment == "timeout") %>% 
  mutate(slope.before = if_else(poss == "away", -slope.before, slope.before)) %>%
  group_by(delta, slope.before) %>% 
  summarise(n=n())

ggplot(plt.data, aes(x = slope.before, y = n, group = factor(delta), colour = factor(delta))) +
  geom_path() +
  geom_point() +
  geom_vline(xintercept = 0) +
  scale_x_discrete(limits = sort(plt.data$slope.before), labels = sort(round(plt.data$slope.before, 2))) +
  theme(axis.text.x = element_text(angle = -45))

###########

#Treatment group covariate comparison

JoinAllMatchesAndDeltaWithData(c("2017"), c("nobal", "maha", "propensity"))
mat.nobal.2017 <- JoinMatchesWithData(res.nobal.2017, res.timeout.effect.data)
mat.maha.2017 <- JoinMatchesWithData(res.maha.2017, res.timeout.effect.data)
mat.propensity.2017 <- JoinMatchesWithData(res.propensity.2017, res.timeout.effect.data)

xvars = .(slope.before, quarter, seconds, margin)
plot.data <- bind_rows(unmatch = mat.nobal.2017,
                       nobal = mat.nobal.2017 %>% drop_na(match.id),
                       maha = mat.maha.2017 %>% drop_na(match.id),
                       prop = mat.propensity.2017 %>% drop_na(match.id),
                       .id = "method") %>%
  group_by(method, delta, match.poss) %>%
  do(tableone = CreateTableOne(vars = as.character(xvars), strata = "A", data = ., test = F) %$% 
       `ContTable`$`1` %>%
       as_tibble(rownames = "covar")) %>%
  unnest(tableone)

#plot.data %<>%
#  gather(key = "var", value = "smd", slope.before, quarter, seconds, margin)

plot.data %>% filter(covar != 'seconds') %>%
  ggplot(aes(x = method)) +
  facet_grid(delta ~ match.poss) +
  geom_point(aes(y=mean, color = covar)) +
  geom_line(aes(y=mean, color = covar, group = covar)) +
  scale_x_discrete(limits = c("unmatch", "nobal", "maha", "prop")) +
  geom_hline(yintercept = 0.1, linetype = 2, size = 0.2) +
  theme_gray()


# mat.data.2017 %>% 
#   group_by(delta, team, poss, A) %>% 
#   summarise_at(vars(infl.score), mean) %>% 
#   group_by(delta, team, poss) %>% 
#   mutate(effect = infl.score - lag(infl.score)) %>%
#   summarise(effect = last(effect)) %>% ungroup() %>%
#   mutate(effect = if_else(poss == "away", -effect, effect)) %>%
#   arrange(delta, poss, desc(effect)) %T>% print(n=2000) %>%
#   spread(delta, effect, sep= ".")
#   
#   
# CalculateTimeoutEffect <- function(matched.data, subset = T) {
#   tef.data <- matched.data %>% drop_na(match.id) %>%
#     filter(subset) %>%
#     mutate(timeout.effect = if_else(A == 1, infl.score, -infl.score)) %>%
#     group_by(match.id) %>%
#     summarise_at(vars(timeout.effect), sum)
#   
#   tef.data %>%
#     left_join(filter(matched.data, A==1), by = "match.id")
# }
# 
# PerformPermTest <- function(mat.data) {
#   mat.data %>%
#     group_by(delta, team, poss) %>%
#     do(perm.test = oneway_test(infl.score ~ as.factor(A), ., distribution = approximate(B = 10000)))
# }
# 
# # aux %>% mutate(pvalue = pvalue(perm.test) %>% as.numeric())
# 
# AnalysisByTeam <- function(tef.data) {
#   ggplot(tef.data, aes(timeout.effect)) +
#     facet_wrap(~team) +
#     geom_density(adjust = 1.5) +
#     geom_vline(xintercept = 0)
# }
# 
# ###
# 
# PlotTables <- function(plot.data, strat = "treatment", xvars = .(slope.before, quarter, seconds, margin)) {
#   xvars = as.character(xvars)
#   table1 <- CreateTableOne(xvars, strat, plot.data, test = FALSE)
#   print(table1, smd=T)
# }
# 
# PlotGraph <- function(plot.data, effect.var) {
#   effect.var <- ensym(effect.var)
#   ggplot(plot.data, aes(!!effect.var, color = as.factor(A))) +
#     geom_density(adjust=2.5) +
#     geom_vline(xintercept = 0)
# }
# 
# OneWayTest <- function(plot.data) {
#   oneway_test(diff ~ as.factor(A), data = plot.data,
#               distribution = approximate(B = 10000))
# }
# 
# SeparateTypeTimeout <- function(data, in.caller.env = F) {
#   data.var.name <- quo_name(enquo(data))
#   
#   if (data %>% has_name("match.poss")) return(data)
#   
#   new.data <- lst(home = data, away = data) %>%
#     imap(function(d, t) {
#       filter(d, treatment == "timeout", poss == !!t) %>%
#         pull(match.id) %>%
#         { filter(d, match.id %in% .)}
#     })
#   
#   if (in.caller.env) {
#     cenv <- caller_env()
#     new.data %>%
#       iwalk(~ env_bind(cenv, !!str_glue(data.var.name, ".", .y) := .x))
#     return(data)
#   } else {
#     return(bind_rows(new.data, .id = "match.poss"))
#   }
# }
# 
# RunExperiments <- function(matched.data) {
#   matched.data %<>% SeparateTypeTimeout()
#   
#   print(matched.data %$% by(., match.poss, PlotTables))
#   
#   print(matched.data %$% by(., match.poss, PlotGraph))
#   
#   print(matched.data %$% by(., match.poss, OneWayTest))
# }



# ToCompare <- function(matched.data, complete.data) {
#   md.name <- quo_name(enquo(matched.data))
#   cd.name <- quo_name(enquo(complete.data))
#   
#   r <- bind_rows(filter(matched.data, type == "control"), filter(complete.data, type == "timeout"))
#   return(r)
# }
# plot.data <- bind_rows(old = old.mat.prop.away, new = new.mat.prop.away, .id = "new")
# plot.data <- bind_rows(old = ToCompare(old.mat.prop.away, old.data %>% filter(poss == "away")), 
#                        new = ToCompare(new.mat.prop.away, new.data %>% filter(poss == "away")), 
#                        .id = "new")
# 
# plot.data <- bind_rows(nnew = nnew.mat.nb.away, new = new.mat.nb.away, .id = "new")
# 
# ggplot(plot.data, aes(slope.before)) +
#   facet_grid(type~new) +
#   geom_bar()
# ggplot(plot.data, aes(seconds)) +
#   facet_grid(type~new) +
#   geom_histogram(binwidth = 30)
# ggplot(plot.data, aes(quarter)) +
#   facet_grid(type~new) +
#   geom_bar()
# ggplot(plot.data, aes(margin)) +
#   facet_grid(type~new) +
#   geom_bar()
# 
# 
# 
# new.data %>% filter(type == "timeout", not(strat %in% old.data$strat))
# old.data %>% filter(type == "timeout", not(strat %in% new.data$strat))
# new.data %>% filter(type == "timeout", not(strat %in% old.data$strat)) %>% pull(game.id) %>%
#   { filter(old.data, type == "timeout", game.id %in% .) }

###
# PlotPosterGraph <- function(matched.data) {
#   matched.data %<>% SeparateTypeTimeout() %>% filter(match.poss == "home")
#   
#   matched.data %>% mutate(delta = as.factor(delta)) %>%
#     ggplot(aes(diff, fill = delta, colour = delta, linetype = type)) +
#     geom_density(alpha=0.15, adjust = 2.5, size = 1) + 
#     geom_vline(xintercept = 0) + 
#     labs(fill = "Delta",
#          colour = "Delta",
#          linetype = "Grupo") + 
#     scale_linetype_manual(values = c("3333", "solid"),
#                           breaks = c("control", "timeout"),
#                           labels = c("Controle", "Real")) +
#     labs(x = "Efeito Timeout",
#          y = "Densidade")
# }
# 
# PlotPosterGraph(mat.unbalanced)
# ggsave("graphs/plot1.png", device = "png")

###
# run_old <- function(data, d = 4) {
#   # PrepareToCausalInference
#   data <- data %>% 
#     mutate(A = type) %>%
#     mutate(A = relevel(A, "control")) %>%
#     mutate(A = as.integer(A)-1) %>%
#     select(A, delta, game.id = game, poss.id = point, diff, everything()) %>%
#     mutate_if(is.numeric, funs(round(., 4))) %>%
#     select(-team, -rk, -win.p, -win.p.diff, -opponent, -opp.rk, -opp.win.p, -rk.diff) %>%
#     mutate(strat = paste(delta, game.id, slope.before, sep = ".")) %>%
#     mutate(poss = case_when(kind == "TIMEOUT-GUEST" ~ 'away',
#                             kind == "TIMEOUT-HOME" ~ 'home',
#                             T ~ 'none')) %>%
#     select(A, delta, game.id, poss.id, type, poss, slope.before, slope.after, diff, quarter, seconds, margin.score, strat) %>%
#     mutate(delta = as.integer(as.character(delta)))
#   
#   data %<>%
#     filter(delta == d)
# }