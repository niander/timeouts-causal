PbpStrMatch <- function(data, pattern) {
  str_detect(paste(data$away, data$home), regex(pattern, ignore_case = T))
}

games.df <- config$seasons %>%
  map_dfr(function(season) {
    name.cache <- str_c("games.df", season, sep=".")
    name.depend <- str_c("games.list", season, sep=".")
    
    scoped_bindings(glist = env_get(global_env(), name.depend),
                    .env = global_env())
    cache(name.cache, {
      games <- glist %>%
        bind_rows(.id = "season.game.id") %>%
        separate(season.game.id, c("game.id", "season"))
      
      games %<>% mutate(subs = PbpStrMatch(., "enters the game"),
                        foul = PbpStrMatch(., "foul by"),
                        shooting.foul = PbpStrMatch(., "shooting foul by"),
                        personal.foul = PbpStrMatch(., "personal foul by"),
                        violation = PbpStrMatch(., "violation by"),
                        turnover = PbpStrMatch(., "turnover by"),
                        off.rebound = PbpStrMatch(., "offensive rebound by"),
                        def.rebound = PbpStrMatch(., "defensive rebound by"),
                        timeout = PbpStrMatch(., "timeout"),
                        official.timeout = PbpStrMatch(., "official timeout"),
                        endquarter = PbpStrMatch(., "end of \\w+ quarter"),
                        shot = PbpStrMatch(., "(makes|misses) (\\w|-)+ shot"),
                        free.throw = PbpStrMatch(., "free throw"))
      vars.kind <- select_vars(colnames(games), subs:free.throw)
      games %<>% mutate(kind = "other")
      for(col in vars.kind) {
        games$kind = ifelse(pull(games[,col]), col, games$kind)
      }
      games %<>% select(-vars.kind)
      
      # Add pbp row id
      games %<>% group_by(season, game.id) %>%
        mutate(pbp.id = 1:n()) %>%
        ungroup() %>%
        select(1:match('game.id', names(.)), pbp.id, everything())
      
      games %<>% mutate(poss = if_else(!xor(is.na(away), is.na(home)), na_int, 0L),
                        poss = if_else(!is.na(home) & !is.na(poss), 1L, poss))
      # consider personal foul on other side
      games %<>% mutate(poss = if_else(kind == "personal.foul", as.integer(not(poss)), poss))
      # ignore the table side for subs, violation, official timeouts and other fouls
      games %<>% mutate(poss = if_else(kind %in% exprs(subs, violation, foul, official.timeout), na_int, poss))
      
      # replace NA for possession
      games %<>% 
        mutate(game.id.duplicate = game.id) %>%
        group_by(season, game.id) %>%
        by_slice(function(game) {
          lead.n <- 1
          while(is.na(game$poss[1])) {
            game %<>% within({
              poss[1] <- poss[lead.n+1]
            })
            lead.n <- lead.n + 1
          }
          
          count <- 0
          sbs <- with(game, which(not(kind %in% exprs(timeout, official.timeout, endquarter))))
          while(anyNA(game$poss[sbs]) && count < length(sbs)) {
            #game %<>% mutate(poss = if_else(is.na(poss), lag(poss), poss))
            game[sbs,] <- game[sbs,] %>% mutate(poss = if_else(is.na(poss), lag(poss), poss))
            count <- count + 1
          }
          if (count >= length(sbs)) {
            stop(game$game.id.duplicate, ": error while updating poss")
          }
          
          return(game)
        }) %>%
        unnest(.out) %>%
        select(-game.id.duplicate)
      
      # handle timeouts in the middle of free throws
      games %<>% 
        mutate(timeout = kind == "timeout" | kind == "official.timeout") %>%
        group_by(season, game.id, quarter, time) %>%
        mutate(new.ingroup.order = if_else(timeout,
                                           1:n() + n(),
                                           1:n())) %>%
        ungroup() %>%
        arrange(season, game.id, quarter, desc(time), new.ingroup.order) %>%
        select(-new.ingroup.order, -timeout)
      
      # Create possessions id
      # timeouts, official timeouts and endquartes as a separate ball possession
      games %<>% 
        group_by(season, game.id) %>%
        mutate(old.poss = poss,
               poss = as.character(poss),
               poss = case_when(kind == "timeout" ~ str_c("tim", 1:n()),
                                kind == "official.timeout" ~ str_c("off.t", 1:n()),
                                kind == "endquarter" ~ str_c("eq", 1:n()),
                                T ~ poss),
               poss.id = rep(1:length(rle(poss)$lengths), rle(poss)$lengths),
               poss = old.poss) %>%
        select(-old.poss) %>%
        ungroup()
      
      # make poss more readble
      games %<>% mutate(poss = case_when(poss == 1 ~ 'home',
                                         poss == 0 ~ 'away',
                                         T ~ na_chr))
      
      games
    }, c(name.depend))
    
    env_get(global_env(), name.cache)
  })

rm(PbpStrMatch)

# cache(name.cache, {
#   info(logger, str_c("caching ", name.cache))
#   
#   p_load(doParallel)
#   cl <- makeForkCluster(detectCores())
#   #registerDoParallel(cl)
#   #registerDoParallel(cores = detectCores())
#   on.exit(stopImplicitCluster())
#   
#   games.list <- env_get(global_env(), name.depend)
#   
#   games.df.list <- games.list %>%
#   {tibble(game = ., season.game.name = names(.), index = 1:length(.))} %>%
#     mlply(function(game, season.game.name, index) {
#       game <- flatten_dfr(game)
#       Pbp_match <- function(data, pattern)
#         str_detect(paste(data$away, data$home), regex(pattern, ignore_case = T))
#       
#       game %<>% mutate(subs = Pbp_match(., "enters the game"),
#                        foul = Pbp_match(., "foul by"),
#                        shooting.foul = Pbp_match(., "shooting foul by"),
#                        personal.foul = Pbp_match(., "personal foul by"),
#                        violation = Pbp_match(., "violation by"),
#                        turnover = Pbp_match(., "turnover by"),
#                        off.rebound = Pbp_match(., "offensive rebound by"),
#                        def.rebound = Pbp_match(., "defensive rebound by"),
#                        timeout = Pbp_match(., "timeout"),
#                        official.timeout = Pbp_match(., "official timeout"),
#                        endquarter = Pbp_match(., "end of \\w+ quarter"),
#                        shot = Pbp_match(., "(makes|misses) (\\w|-)+ shot"),
#                        free.throw = Pbp_match(., "free throw"))
#       vars.kind <- select_vars(colnames(game), subs:free.throw)
#       game %<>% mutate(kind = "other")
#       for(col in vars.kind) {
#         game$kind = ifelse(pull(game[,col]), col, game$kind)
#       }
#       game %<>% select(-vars.kind)
#       
#       # Add pbp row id
#       game %<>% mutate(pbp.id = 1:n()) %>%
#         select(pbp.id, everything())
#       
#       game %<>% mutate(poss = if_else(!xor(is.na(away), is.na(home)), na_int, 0L),
#                        poss = if_else(!is.na(home) & !is.na(poss), 1L, poss))
#       # consider personal foul on other side
#       game %<>% mutate(poss = if_else(kind == "personal.foul", as.integer(not(poss)), poss))
#       # ignore the table side for subs, violation, official timeouts and other fouls
#       game %<>% mutate(poss = if_else(kind %in% exprs(subs, violation, foul, official.timeout), na_int, poss))
#       
#       # replace NA for possession before
#       lead.n <- 1
#       while(is.na(game$poss[1])) {
#         game$poss[1] <- game$poss[lead.n+1]
#         lead.n <- lead.n + 1
#       }
#       count <- 0
#       sbs <- with(game, which(not(kind %in% exprs(timeout, official.timeout, endquarter))))
#       while(anyNA(game$poss[sbs]) && count < length(sbs)) {
#         #game %<>% mutate(poss = if_else(is.na(poss), lag(poss), poss))
#         game[sbs,] <- game[sbs,] %>% mutate(poss = if_else(is.na(poss), lag(poss), poss))
#         count <- count + 1
#       }
#       if (count >= length(sbs)) {
#         stop(season.game.name, ": error while updating poss")
#       }
#       
#       # handle timeouts in the middle of free throws
#       game %<>% mutate(timeout = kind == "timeout" | kind == "official.timeout") %>%
#         slice_rows(c("quarter", "time")) %>%
#         by_slice(function(df) {
#           df$new.ingroup.order = 1L
#           df[df$timeout, ] %<>% mutate(new.ingroup.order = seq.int(to=nrow(df), length.out=sum(timeout)))
#           df[!df$timeout, ] %<>% mutate(new.ingroup.order = seq_len(sum(!timeout)))
#           return(df)
#         }, .collate = "rows") %>%
#         unslice() %>%
#         arrange(quarter, desc(time), new.ingroup.order) %>%
#         select(-new.ingroup.order, -timeout)
#       
#       # is.all.true <- game %>%
#       #   mutate(timeout = kind == "timeout") %>%
#       #   group_by(quarter, time) %>%
#       #   mutate(group_num = 1:n(),
#       #          is.last = case_when(timeout & group_num == n() ~ T, 
#       #                              timeout ~ F,
#       #                              T ~ na_lgl)) %>%
#       #   pull(is.last) %>%
#       #   all(na.rm = T)
#       # 
#       # if(!is.all.true) {
#       #   stop(season.game.name, ": not all timeouts in correct position")
#       # }
#       
#       # Create possessions id
#       # timeouts, official timeouts and endquartes as a separate ball possession
#       game %<>%
#         mutate(old.poss = poss,
#                poss = as.character(poss),
#                poss = case_when(kind == "timeout" ~ str_c("tim", 1:nrow(.)),
#                                 kind == "official.timeout" ~ str_c("off.t", 1:nrow(.)),
#                                 kind == "endquarter" ~ str_c("eq", 1:nrow(.)),
#                                 T ~ poss),
#                poss.id = rep(1:length(rle(poss)$lengths), rle(poss)$lengths),
#                poss = old.poss) %>%
#         select(-old.poss)
#       
#       # # make poss a more readble
#       game %<>% mutate(poss = case_when(poss == 1 ~ 'home',
#                                         poss == 0 ~ 'away',
#                                         T ~ na_chr))
#       
#       info(logger, paste("finished", season.game.name, "of id", index))
#       return(game)
#     }, .parallel = TRUE)
#   
#   games.df <- games.df.list %>%
#     set_names(names(env_get(global_env(), name.depend))) %>%
#     bind_rows(.id = "season.game.id") %>%
#     separate(season.game.id, c("season", "game.id"))
#   games.df
# }, c(name.depend))
