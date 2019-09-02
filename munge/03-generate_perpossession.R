games.perposs <- config$seasons %>%
  map_dfr(function(season) {
    name.cache <- str_c("games.perposs", season, sep=".")
    name.depend <- str_c("games.df", season, sep=".")
    
    scoped_bindings(name.depend = name.depend,
                    .env = global_env())
    
    cache(name.cache, {
      gdf <- env_get(global_env(), name.depend)
      gdf %>%
        group_by(season, game.id, poss.id) %>%
        summarize(quarter = first(quarter),
                  time = last(time),
                  pts = sum(pts),
                  margin = last(margin),
                  kind = last(kind),
                  poss = last(poss)) %>%
        ungroup()
    }, c(name.depend))

    env_get(global_env(), name.cache)
  })
