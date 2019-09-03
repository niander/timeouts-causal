
### Functions

ReadPbpRawData <- function(folder.path) {
  pbp.data.list <- list.files(folder.path, full.names = T) %>%
    llply(function(x) {
      game <- ReadTable(fullpath = x, delim = ",", col_types = cols(time = col_character(),
                                                                    pts.home = col_integer(),
                                                                    pts.away = col_integer()))
      game %<>% mutate(time = str_replace(time, "^(\\d):", "0\\1:")) %>% 
        type_convert(col_types = cols(time = col_time("%M:%OS")))
    })
  names(pbp.data.list) <- list.files(folder.path) %>% 
    gsub("\\.txt", "", .)
  return(pbp.data.list)
}

GenerateGamesPbpFromRawData <- function(pbp.data.list) {
  games <- pbp.data.list %>% 
    llply(function(game) CleanAndGenerateGamePbp(game), .progress = "text")
  
  weird.games.name <- games %>%
    llply(function(g) filter(g, weird.score)) %>%
    keep(~ nrow(.) > 0) %>%
    names()
  
  games[weird.games.name] %<>% llply(function(game) {
    which.weird.score <- which(game$weird.score)
    game[which.weird.score,] %<>%
      mutate(pts.away.text = as.integer(str_match(away, "makes (\\d)")[,2]),
             pts.home.text = as.integer(str_match(home, "makes (\\d)")[,2]),
             pts.away = if_else(pts.away.text != pts.away, pts.away.text, pts.away, pts.away),
             pts.home = if_else(pts.home.text != pts.home, pts.home.text, pts.home, pts.home)) %>%
      select(-pts.away.text, -pts.home.text)
    
    game %<>% CleanAndGenerateGamePbp()
    return(game)
  })
  
  games %<>% map(~ select(., -score, -pts.away, -pts.home, -weird.score))

  return(games)
}

CleanAndGenerateGamePbp <- function(game) {
  game %<>% rename(away = colnames(game)[2],
                   home = colnames(game)[6])
  
  game %<>% mutate(score.away = cumsum(pts.away %>% ifelse(is.na(.), 0, .)),
                   score.home = cumsum(pts.home %>% ifelse(is.na(.), 0, .)),
                   pts = pts.home %>% ifelse(is.na(.), 0, .) - pts.away %>% ifelse(is.na(.), 0, .),
                   margin = score.home - score.away)
  
  game %<>% mutate(totest = paste(score.away, score.home, sep = "-"),
                   weird.score = totest != score) %>%
    select(-totest)
  
  # create quarter variable
  game %<>% filter(!grepl("start of \\w+ (quarter|overtime)", paste(away, home), ignore.case = T)) %>%
    mutate(endofquarter = grepl("end of \\w+ (quarter|overtime)", paste(away, home), ignore.case = T),
           quarter = cumsum(as.integer(endofquarter)) + 1,
           quarter = if_else(endofquarter, quarter - 1, quarter),
           quarter = as.integer(quarter)) %>%
    select(-endofquarter)
  
  return(game)
}

### Script

games.list <- config$seasons %>% 
  map(function(season) {
    name.cache <- str_c("games.list", season, sep=".")
    
    if (!env_has(global_env(), name.cache)) {
      pbp.data.list <- ReadPbpRawData(file.path(config$pbp_folder, season))
      to.cache <- GenerateGamesPbpFromRawData(pbp.data.list) %>%
        set_names(str_c(names(.), season, sep = "."))
      
      env_bind(global_env(), !!name.cache := to.cache)
      cache(name.cache)
    }
    
    env_get(global_env(), name.cache)
  }) %>%
  flatten()

games.teams <- config$seasons %>%
  map_dfr(function(season) {
    name.cache <- str_c("games.teams", season, sep=".")
    name.depend <- str_c("games.list", season, sep=".")
    
    scoped_bindings(season = season,
                    .env = global_env())
    
    cache(name.cache, {
      ReadPbpRawData(file.path(config$pbp_folder, season)) %>%
        map_dfr(~ tibble(away = names(.)[2], home = names(.)[6]), .id = "game.id") %>%
        add_column(season = as.character(season), .before = "game.id")
    }, c(name.depend))
    
    env_get(global_env(), name.cache)
  })

rm(ReadPbpRawData, GenerateGamesPbpFromRawData, CleanAndGenerateGamePbp)


