timeout.effect.data <- config$seasons %>%
  map_dfr(function(season) {
    name.cache <- str_c("timeout.effect.data", season, sep=".")
    name.depend <- str_c(c("timeouts.inflscore", "contr.inflscore"), season, sep=".")
    
    scoped_bindings(timeouts.inflscore = env_get(global_env(), name.depend[1]),
                    contr.inflscore = env_get(global_env(), name.depend[2]),
                    .env = global_env())
    
    cache(name.cache, {
      # Add name of the team
      tefdata <- bind_rows(timeout = timeouts.inflscore, control = contr.inflscore, .id = "treatment")
      tefdata %<>%
        left_join(games.teams, by = c("season", "game.id")) %>%
        select(1:match("game.id", colnames(.)), away, home, names(tefdata)) %>%
        mutate(team = case_when(treatment != 'timeout' ~ na_chr,
                                poss == 'home' ~ home, 
                                poss == 'away' ~ away,
                                T ~ na_chr),
               opponent = case_when(treatment != 'timeout' ~ na_chr,
                                    poss == 'home' ~ away,
                                    poss == 'away' ~ home,
                                    T ~ na_chr))
      # select(!!!1:match("game", colnames(.)), team, opponent, everything()) %>%
      # select(-guest,-home)
      
      # Get teams Win.Percentage and Rank location (before playoffs)
      # rk.and.win.p <- st.table %>% 
      #   select(team = Pbp.Name, win.p = Win.Percentage, rk = Rk)
      # tef.data %<>% left_join(rk.and.win.p, by = c("opponent" = "team")) %>%
      #   select(!!!1:match("opponent", colnames(.)), opp.rk = rk, opp.win.p = win.p, everything()) %>%
      #   left_join(rk.and.win.p, by = "team") %>%
      #   select(!!!1:match("team", colnames(.)), rk, win.p, everything())
      
      # Generate rank and win.p difference
      # tef.data %<>% 
      #   mutate(rk.diff = rk - opp.rk, win.p.diff = win.p - opp.win.p) %>%
      #   select(!!!1:match("opp.win.p", colnames(.)), rk.diff, win.p.diff, everything())
      
      # Add seconds from time
      tefdata %<>%
        add_column(seconds = na_int, .after = 'time') %>%
        mutate(seconds = as.integer(seconds(time)),
               seconds = if_else(quarter <= 4, 12L*60L - seconds, 5L*60L - seconds))
      
      tefdata %<>%
        select(-away, -home, -kind, -time, -timeout.poss) %>%
        mutate(poss = if_else(treatment == "timeout", poss, na_chr))
      
      tefdata
    }, c(name.depend))
    
    env_get(global_env(), name.cache)
})

SaveObject(timeout.effect.data, config$results_folder)
