env_bind(global_env(), config.deltas = config$deltas)

games.inflscore <- config$seasons %>%
  map_dfr(function(season) {
    name.cache <- str_c("games.inflscore", season, sep=".")
    name.depend <- str_c("games.perposs", season, sep=".")
    
    scoped_bindings(name.depend = name.depend,
                    .env = global_env())
    cache(name.cache, {
      CalculateInflectionScore <- function(df, delta) {
        df %<>%
          mutate(pts.delta.before = lag(margin, delta) - lag(pts, delta),
                 pts.delta.after = lead(margin, delta),
                 slope.before = (margin - pts - pts.delta.before) / delta,
                 slope.after = (pts.delta.after - margin) / delta,
                 infl.score = slope.after - slope.before) %>%
          select(-pts.delta.before, -pts.delta.after)
        
        df %<>%
          mutate(kinds.before = map(delta:1, ~ lag(kind, .)) %>% transpose() %>% modify(chr),
                 kinds.after = map(delta:1, ~ lead(kind, .)) %>% transpose() %>% modify(chr),
                 valid.infl.score = map_lgl(map2(kinds.before, kinds.after, combine), 
                                            ~ !any(. %in% c("timeout", "official.timeout", "endquarter")))) %>%
          select(-kinds.before, -kinds.after)
        
        df %<>% mutate(delta = as.integer(delta))
        return(df)
      }
      gpp <- env_get(global_env(), name.depend)
      gpp %>%
        group_by(season, game.id) %>%
        do(map_dfr(config.deltas, function(delta) CalculateInflectionScore(., delta = delta))) %>%
        ungroup()
    }, c(name.depend, 'config.deltas'))
    
    env_get(global_env(), name.cache)
  })

env_unbind(global_env(), 'config.deltas')