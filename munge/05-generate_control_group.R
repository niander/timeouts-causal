
### Functions

only_valids <- . %>% filter(valid.infl.score) %>% 
  select(-valid.infl.score) %>%
  drop_na(infl.score)

timeouts_only <- . %>%
  filter(kind == "timeout") %>%
  only_valids()

others_only <- . %>%
  filter(not(kind %in% c("timeout", "official.timeout", "endquarter"))) %>%
  only_valids()

### Script

contr.inflscore <- config$seasons %>%
  map_dfr(function(season) {
    name.cache <- str_c("contr.inflscore", season, sep=".")
    name.depend <- str_c("games.inflscore", season, sep=".")
    
    scoped_bindings(name.depend = name.depend,
                    .env = global_env())
    cache(name.cache, {
      threshold <- sqrt(.Machine$double.eps)
      
      ginflscore <- env_get(global_env(), name.depend)
      timeouts.insc <- timeouts_only(ginflscore)
      others.insc <- others_only(ginflscore)
      
      contr.inflscore <- timeouts.insc %>%
        left_join(others.insc, by = c("season", "game.id", "delta"), suffix = c(".t", ".c")) %>%
        mutate(diff.slope.before = abs(slope.before.t - slope.before.c)) %>%
        filter(diff.slope.before <= threshold) %>%
        select(season, game.id, delta, ends_with(".c"), timeout.poss = poss.id.t) %>%
        rename_all(~ str_remove(., ".c$")) %>%
        nest(timeout.poss, .key = 'timeout.poss') %>%
        mutate(timeout.poss = map(timeout.poss, flatten_int))
    }, c(name.depend))
    
    env_get(global_env(), name.cache)
  })

walk(config$seasons, function(season) {
  ginflscore <- env_get(global_env(), str_c("games.inflscore", season, sep="."))
  bname <- str_c("timeouts.inflscore", season, sep=".")
  env_bind(global_env(), !!bname := timeouts_only(ginflscore))
})

rm(only_valids, timeouts_only, others_only)


