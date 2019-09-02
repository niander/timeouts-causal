p_load(ProjectTemplate)
#load.project(munging = FALSE, cache_loading = FALSE)
load.project()

p_unload(MASS)
library(MASS, pos = "package:base")
p_load(rcbalance)
p_load(optmatch)
p_load(gbm)
p_load(doParallel)
registerDoParallel(cores = detectCores())
on.exit(stopImplicitCluster())
p_load(coin)
p_load(tableone)
p_load(MatchIt)
#p_load(Matching)

RemoveOverlapingPossibleMatches <- function(unmat.data, dist.struct) {
  unmat.data %<>% 
    group_by(A) %>%
    mutate(m.id = row_number()) %>%
    ungroup()
  
  unmat.data.t <- filter(unmat.data, A == 1) %>%
    select(delta, poss.id, strat.id, m.id)
  unmat.data.c <- filter(unmat.data, A == 0) %>%
    select(delta, poss.id, strat.id, m.id)
  
  valid.possible.matches <- left_join(unmat.data.t, unmat.data.c, by = c("delta", "strat.id"), suffix = c(".t", ".c")) %>%
    mutate(poss.id.diff = abs(poss.id.t - poss.id.c),
           valid = if_else(poss.id.diff < (2 * delta + 1), FALSE, TRUE)) %>%
    select(-poss.id.diff, -delta) %>%
    group_by(m.id.t) %>%
    summarise_at(vars(m.id.c, valid), lst) %>%
    ungroup() %>%
    rowwise() %>%
    mutate(valid.m.id.c = lst(extract(m.id.c, valid))) %>%
    ungroup() %>%
    select(-m.id.c, -valid) %>%
    arrange(m.id.t)
  
  new.dist.struct <- dist.struct %>%
    imodify(~ extract(.x, names(.x) %in% ( pull(valid.possible.matches[.y,], valid.m.id.c) %>% squash_int() ) ))
  
  return(new.dist.struct)
}

NoBalanceMatching <- function(data, ...) {
  inform("No balance matching")
  inform("Creating distance structure")
  dist.struct <- build.dist.struct(z = data$A,
                                   X = rnorm(nrow(data), mean=1,sd=0.01),
                                   exact = data$strat.id,
                                   calip.option = "none",
                                   verbose = TRUE)
  
  inform("Removing Overlaping Possible Matches")
  dist.struct <- RemoveOverlapingPossibleMatches(data, dist.struct)
  
  inform("Initiating the matching")
  m.out <- rcbalance(dist.struct,
                     exclude.treated = T,
                     tol = 1e-3,
                     ...)
  return(m.out)
}

MahalanobisMatching <- function(data, 
                                covars,
                                near.exact = NULL, 
                                replace = FALSE,
                                ...) {
  inform("Maha matching")
  if(replace) {
    
    inform("Initiating the matching with replacement")
    m.out <- matchit(formula = new_formula(quote(A), parse_expr(str_flatten(as.character(covars), collapse = " + "))),
                     data = select(data, A, strat.id, !!!covars) %>% as.data.frame(),
                     exact = "strat.id",
                     method = "nearest",
                     distance = "mahalanobis",
                     discard = "both",
                     replace = T)
    
    m.out$matches <- m.out$match.matrix
    rownames(m.out$matches) <- as.integer(rownames(m.out$matches)) - sum(m.out$treat == 0)
  } else{
    inform("Creating distance structure")
    dist.struct <- build.dist.struct(z = data$A,
                                     X = select(data, !!!covars) %>% as.data.frame(),
                                     exact = data$strat.id,
                                     calip.option = "none",
                                     verbose = TRUE)
    
    inform("Removing Overlaping Possible Matches")
    dist.struct <- RemoveOverlapingPossibleMatches(data, dist.struct)
    
    data %<>% as.data.frame()
    # l1 <- c("seconds")
    # l2 <- c(l1, "margin")
    # l3 <- c(l2, "quarter")
    inform("Initiating the matching")
    m.out <- rcbalance(dist.struct,
                       near.exact = if (is.null(near.exact)) NULL else as.character(near.exact),
                       treated.info = filter(data, A == 1),
                       control.info = filter(data, A == 0),
                       exclude.treated = T,
                       tol = 1e-3,
                       ...)
  }
  
  return(m.out)
}

GeneratePropesityScores <- function(data, covars = exprs(seconds, quarter, margin), ...) {
  inform("Generating Propensity Scores")
  gbm.data <- data %>% 
    mutate(id = row_number()) %>%
    sample_frac() %>% 
    select(id, A, !!!covars) %>%
    as.data.frame()
  ps.model <- gbm(formula = new_formula(quote(A), parse_expr(str_flatten(as.character(covars), collapse = " + "))),
                  data = select(gbm.data, -id), 
                  distribution = "bernoulli",
                  #weights = rep(1, nrow(gbm.data)),
                  #weights = ave(gbm.data$A, as.factor(gbm.data$A), FUN = function(x) length(x)),
                  n.trees = 2000, 
                  interaction.depth = 2, 
                  n.minobsinnode = 10, 
                  shrinkage = 0.05, 
                  bag.fraction = 0.5, 
                  train.fraction = 0.85,
                  cv.folds = 5,
                  verbose = T, 
                  keep.data = T,
                  n.cores = parallel::detectCores())
  best.iter <- gbm.perf(ps.model, method="cv")
  ps.score <- predict(ps.model, n.trees = best.iter, type="response")
  ps.score <- ps.score[order(gbm.data$id)]
  
  return(ps.score)
}

PropensityMatching <- function(data,
                               ps.scores,
                               caliper = 0.2,
                               calip.option = "user",
                               ...) {
  dist.struct <- build.dist.struct(z = data$A,
                                   X = ps.scores,
                                   exact = data$strat.id,
                                   calip.option = calip.option,
                                   calip.cov = ps.scores,
                                   caliper = caliper,
                                   verbose = TRUE)
  
  inform("Removing Overlaping Possible Matches")
  dist.struct <- RemoveOverlapingPossibleMatches(data, dist.struct)
  
  inform("Initiating the matching")
  m.out <- rcbalance(dist.struct,
                     exclude.treated = TRUE,
                     tol = 1e-3,
                     ...)
  
  return(m.out)
}

MatchSamples_ <- function(data,
                         covars,
                         method,
                         ps.config,
                         ...) {
  data %<>% mutate(id = row_number())
  
  no.control.id <- data %>%
    filter(A == 1) %>%
    anti_join(filter(data, A == 0), by = "strat") %>%
    pull(id)
  
  data %<>%
    mutate(has.control = not(id %in% no.control.id)) %>%
    select(-id)
  
  unmat.data <- data %>%
    filter(has.control) %>%
    arrange(A, delta, game.id, poss.id) %>%
    mutate(strat.id = group_indices(., strat))
  
  inform("Ready to start the matching")
  
  old.random.state <- .Random.seed
  set.seed(222)
  
  ps.scores <- na_dbl
  m.out <- switch(method,
                  nobal = NoBalanceMatching(unmat.data, ...),
                  maha = MahalanobisMatching(unmat.data, covars, ...),
                  propensity = {
                    ps.scores <- GeneratePropesityScores(unmat.data, covars, ps.config)
                    PropensityMatching(unmat.data, ps.scores = ps.scores, ...)
                  }
  )
  
  .Random.seed <- old.random.state
  
  unmat.data %<>% 
    select(-strat.id) %>%
    mutate(ps.scores = ps.scores)
  
  matches <- m.out$matches
  matches.df <- tibble(treatment.id = as.numeric(rownames(matches)), 
                       control.id = as.numeric(matches),
                       match.id = if (nrow(matches) == 0) numeric(0) else 1:nrow(matches))
  unmat.data %<>% 
    group_by(A) %>%
    mutate(t.id = row_number()) %>%
    ungroup()
  
  ctrl.mat.data <- unmat.data %>%
    filter(A == 0) %>%
    left_join(matches.df, by = c("t.id" = "control.id")) %>%
    select(-t.id, -treatment.id)
  
  treat.mat.data <- unmat.data %>%
    filter(A == 1) %>%
    left_join(matches.df, by = c("t.id" = "treatment.id")) %>%
    select(-t.id, -control.id)
  
  mat.data <- bind_rows(ctrl.mat.data, 
                        treat.mat.data,
                        filter(data, !has.control))
  
  mat.data %<>%
    arrange(match.id, A)
  
  # matches.treat <- tibble(treatment.id = 1:nrow(filter(data, A==1))) %>%
  #   left_join(matches.df, by = "treatment.id") %>% 
  #   select(-control.id)
  # 
  # matches.contr <- tibble(control.id = 1:nrow(filter(data, A==0))) %>%
  #   left_join(matches.df, by = "control.id") %>% 
  #   select(-treatment.id)
  # 
  # mat.data <- bind_rows(filter(m.out$data, A==0) %>%
  #                        mutate(match.id = matches.contr$match.id),
  #                      filter(m.out$data, A==1) %>%
  #                        mutate(match.id = matches.treat$match.id))
  # mat.data %<>%
  #   drop_na(match.id) %>%
  #   arrange(match.id)
  
  res <- lst(m.out = m.out,
             mat.data = mat.data)
  
  return(res)
}

MatchSamples <- function(data,
                         data.name = as_name(ensym(data)),
                         covars = exprs(seconds, quarter, margin),
                         method = c("nobal", "maha", "propensity"),
                         ps.config = NULL,
                         cache.suffix.name = NULL,
                         break.by.poss = F,
                         force.cache = F,
                         ...) {
  method.name <- arg_match(method)
  cache.suffix.name <- str_c(cache.suffix.name, data.name, sep = ".")
  cache.file.name <- str_c(method.name, cache.suffix.name, sep = ".")
  cache.name <- str_c(path_file(config$results_folder), cache.file.name, sep = ".")
  call.name <- match.call()
  inform("===============")
  inform("Matching")
  inform(str_c("Method:", method.name, "; cache.suffix.name:", cache.suffix.name, sep = " "))
  
  if (force.cache || !env_has(nms = cache.name, inherit = T)) {
    if(break.by.poss) {
      controls <- data %>%
        filter(A == 1) %>%
        group_by(poss) %>%
        by_slice(~ semi_join(x = filter(data, A==0) %>% select(-poss), 
                             ., by = "strat"), .collate = "rows")
      
      new.data <- bind_rows(filter(data, A == 1), controls) %>%
        arrange(A, delta, season, game.id, poss.id)
      
      res <- new.data %>% 
        group_by(poss) %>%
        by_slice(function(df, ...) MatchSamples_(df, covars, method, ps.config, ...), ..., .collate = "list") %$%
        set_names(.out, poss) %>%
        transpose()
      
      res$mat.data %<>% bind_rows(.id = "poss") %>%
        select(-poss, everything()) %>%
        rename(match.poss = poss)
    } else {
      res <- MatchSamples_(data, covars, method, ps.config, ...)
    }
    res %<>% update_list(method.name = method.name,
                         cache.suffix.name = cache.suffix.name,
                         call = call.name)
    SaveObject(res, config$results_folder, cache.file.name)
    env_bind(global_env(), !!cache.name := res)
    
    inform("Matching completed")
    #return(res)
  } else {
    inform("Matching already cached")
  }
}

PrepareData <- function(data) {
  data %<>%
    mutate(A = if_else(treatment == 'control', 0L, 1L)) %>%
    mutate_if(is_double, funs(round(., 4))) %>%
    mutate(strat = paste(delta, game.id, slope.before, sep = ".")) %>%
    select(A, delta, season, game.id, poss.id, slope.before, slope.after, infl.score, quarter, seconds, margin, strat, poss)
  
  return(data)
}

#LoadObjects(config$results_folder, env_to_save = global_env())

###########################

# data <- PrepareData(res.timeout.effect.data) %>%
#   filter(season == 2017)
# 
# controls <- data %>%
#   filter(A == 1) %>%
#   group_by(poss) %>%
#   by_slice(~ semi_join(x = filter(data, A==0) %>% select(-poss), 
#                        ., by = "strat"), .collate = "rows")
# 
# new.data <- bind_rows(filter(data, A == 1), controls) %>%
#   arrange(A, delta, season, game.id, poss.id)
# 
# 
# res.away <- new.data %>% filter(A == 0 | (A == 1 & poss == "away"))
# res.home <- new.data %>% filter(A == 0 | (A == 1 & poss == "home"))
# 
# data <- res.home
# 
# data %<>% mutate(id = row_number())
# 
# no.control.id <- data %>%
#   filter(A == 1) %>%
#   anti_join(filter(data, A == 0), by = "strat") %>%
#   pull(id)
# 
# data %<>%
#   mutate(has.control = not(id %in% no.control.id)) %>%
#   select(-id)
# 
# unmat.data <- data %>%
#   filter(has.control) %>%
#   arrange(A, delta, game.id, poss.id) %>%
#   mutate(strat.id = group_indices(., strat))
# 
# covars = exprs(seconds, quarter, margin)
# dist.struct <- build.dist.struct(z = unmat.data$A,
#                                  X = select(unmat.data, !!!covars) %>% as.data.frame(),
#                                  exact = unmat.data$strat.id,
#                                  calip.option = "none",
#                                  verbose = TRUE)
# 
# res <- new.data %>% 
#   group_by(poss) %>%
#   by_slice(function(df, ...) MatchSamples_(df, covars, method, ps.config, ...), ..., .collate = "list") %$%
#   set_names(.out, poss) %>%
#   transpose()
# 
# res$mat.data %<>% bind_rows(.id = "poss") %>%
#   select(-poss, everything()) %>%
#   rename(match.poss = poss)
# 

############################

RunAllMatches <- function(data) {
  data.name <- as_name(ensym(data))
  MatchSamples(data, data.name = data.name, method = "nobal", break.by.poss = T)
  MatchSamples(data, data.name = data.name, method = "maha", break.by.poss = T)
  #MatchSamples(data, data.name = data.name, method = "propensity", break.by.poss = T)

  # MatchSamples(data, data.name = data.name, method = "maha", break.by.poss = T,
  #              cache.suffix.name = "replace",
  #              replace = T)

  MatchSamples(data, data.name = data.name, method = "propensity", break.by.poss = T,
               cache.suffix.name = "nocalip",
               calip.option = "none")

  # MatchSamples(data, data.name = data.name, method = "maha", break.by.poss = T,
  #              covars = exprs(seconds, margin),
  #              cache.suffix.name = "noquarter")
}

data.2017 <- PrepareData(res.timeout.effect.data) %>%
  filter(season == 2017)

delta2.2017 <- data.2017 %>% filter(delta == 2)
RunAllMatches(delta2.2017)

delta4.2017 <- data.2017 %>% filter(delta == 4)
RunAllMatches(delta4.2017)

delta6.2017 <- data.2017 %>% filter(delta == 6)
RunAllMatches(delta6.2017)

#####

# data.2018 <- PrepareData(res.timeout.effect.data) %>%
#   filter(season == 2018)
# 
# delta2.2018 <- data.2018 %>% filter(delta == 2)
# RunAllMatches(delta2.2018)
# 
# delta4.2018 <- data.2018 %>% filter(delta == 4)
# RunAllMatches(delta4.2018)
# 
# delta6.2018 <- data.2018 %>% filter(delta == 6)
# RunAllMatches(delta6.2018)

#####

# data.2016 <- PrepareData(res.timeout.effect.data) %>%
#   filter(season == 2016)
# 
# delta2.2016 <- data.2016 %>% filter(delta == 2)
# RunAllMatches(delta2.2016)
# 
# delta4.2016 <- data.2016 %>% filter(delta == 4)
# RunAllMatches(delta4.2016)
# 
# delta6.2016 <- data.2016 %>% filter(delta == 6)
# RunAllMatches(delta6.2016)

#####

# data.2015 <- PrepareData(res.timeout.effect.data) %>%
#   filter(season == 2015)
# 
# delta2.2015 <- data.2015 %>% filter(delta == 2)
# RunAllMatches(delta2.2015)
# 
# delta4.2015 <- data.2015 %>% filter(delta == 4)
# RunAllMatches(delta4.2015)
# 
# delta6.2015 <- data.2015 %>% filter(delta == 6)
# RunAllMatches(delta6.2015)


#######################

# data <- PrepareData(res.timeout.effect.data) %>%
#   filter(season %in% c("2015", "2016", "2017", "2018"))
# delta2 <- data %>% filter(delta == 2)
# delta4 <- data %>% filter(delta == 4)
# delta6 <- data %>% filter(delta == 6)
# 
# 
# cenv <- current_env()
# lst(delta2, delta4, delta6) %>%
#   iwalk(~ .x %>% 
#           filter(quarter == 4, seconds >= 420) %>% # Last 5 min
#           { env_bind(cenv, !!str_c("last5min", .y, sep = ".") := .) })
# 
# RunAllMatches(last5min.delta2)
# RunAllMatches(last5min.delta4)
# RunAllMatches(last5min.delta6)

JoinMatchesWithData <- function(mat.data, data,
                                vars.from.treatment = as.character(.(poss, team, opponent))) {
  mat.data %<>%
    left_join(data, by = c("delta", "season", "game.id", "poss.id")) %>% 
    select(-ends_with(".y")) %>%
    rename_at(vars(ends_with(".x")), funs(str_remove(., ".x")))
  
  if (!is.null(vars.from.treatment)) {
    treats.vars <- mat.data %>%
      filter(A == 1, !is.na(match.id)) %>%
      arrange(match.id) %>%
      select(!!!vars.from.treatment)
    
    control.matched.id <- mat.data %>%
      mutate(id = row_number()) %>%
      filter(A == 0, !is.na(match.id)) %>%
      arrange(match.id) %>%
      pull(id)
    
    mat.data[control.matched.id, vars.from.treatment] <- treats.vars
    # mat.data %<>%
    #   group_by(match.id) %>%
    #   summarise_at(vars(!!!vars.from.treatment), funs(extract2(., which(A == 1)))) %>%
    #   right_join(mat.data, by = "match.id") %>%
    #   rename_at(vars(ends_with(".x")), funs(str_remove(., ".x"))) %>%
    #   select(-ends_with(".y"))
  }
  
  return(mat.data)
}

JoinDeltaMatData <- function(years = c("2017"),
                             methods = c("nobal", "propensity"),
                             deltas = config$deltas) {
  
  crossing(methods, years) %$%
    walk2(methods, years, function(m, y) {
      env_bind(global_env(),
               !!str_c("res", m, sep = ".") :=
                 crossing(dd = str_c("delta", deltas), yy = years) %$%
                 map2_dfr(dd, yy, function(dd, yy) {
                   objname <- str_c("res", m, dd, yy, sep = ".")
                   if (!env_has(global_env(), nms = objname)) {
                     inform(str_c(objname, "not found in global env", sep = " "))
                     objname <- str_c("res", m, dd, sep = ".")
                     if (env_has(global_env(), nms = objname)) {
                       inform(str_c("However,", objname, "found in global env", sep = " "))
                       return(env_get(global_env(), objname)$mat.data)
                     } else {
                       inform(str_c(objname, "not found in global env", sep = " "))
                       return(zap())
                     }
                   } else {
                     return(env_get(global_env(), objname)$mat.data)
                   }
                 })
      )
    })
}

JoinAllMatchesAndDeltaWithData <- function(years = c("2017"), 
                                           methods = c("nobal", "propensity"),
                                           deltas = config$deltas,
                                           data, ...) {
  JoinDeltaMatData(years, methods, deltas)
  
  walk(methods, function(m) {
    nm <- str_c("res", m, sep = ".")
    if(env_has(global_env(), nm)) {
      mat.data <- JoinMatchesWithData(env_get(global_env(), nm), data, ...)
      env_bind(global_env(), !!str_c("mat", m, sep = ".") := mat.data)
    } else {
      inform(str_glue("{nm} was not found in global env"))
    }
  })
}

env_bind(global_env(), JoinMatchesWithData = JoinMatchesWithData, 
         JoinDeltaMatData = JoinDeltaMatData, 
         JoinAllMatchesAndDeltaWithData = JoinAllMatchesAndDeltaWithData)

# JoinUnmatchesWithData <- function(unmat.data, data,
#                                   vars.from.treatment = as.character(.(poss, team, opponent))) {
#   unmat.data %<>%
#     left_join(data, by = c("delta", "season", "game.id", "poss.id")) %>% 
#     select(-ends_with(".y")) %>%
#     rename_at(vars(ends_with(".x")), funs(str_remove(., ".x")))
#   
#   if (!is.null(vars.from.treatment)) {
#     unmat.data %<>%
#       group_by(match.id) %>%
#       summarise_at(vars(!!!vars.from.treatment), funs(extract2(., which(A == 1)))) %>%
#       right_join(mat.data, by = "match.id") %>%
#       rename_at(vars(ends_with(".x")), funs(str_remove(., ".x"))) %>%
#       select(-ends_with(".y"))
#   }
#   
#   return(mat.data)
# }

# AddPsScoreToMatData <- function(mat) {
#   mat$data %<>% add_column(ps.score = mat$ps.score)
#   df <- mat$data %>% select(game.id, poss.id, ps.score)
#   
#   mat$mat.data %<>% left_join(df, by = c("game.id", "poss.id"))
#   return(mat)
# }

