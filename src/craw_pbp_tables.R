library(ProjectTemplate)
load.project(cache_loading = FALSE, munging = FALSE)

#library(XML)
#library(RCurl)
#xmldata <- getURL(url)
#oldtable <- readHTMLTable(xmldata, as.data.frame = TRUE)[[1]]

library(rvest)
CrawlPbpTable <- function(url) {
  table <- url %>% read_html() %>%
    html_node("#pbp") %>%
    html_table(fill = TRUE)
  teams.name <- c(table[1,2], table[1,6])
  names(table) <- c("time",teams.name[1],"pts.away","score","pts.home",teams.name[2])
  
  # Remove headers
  #table <- table[!grepl("score", table[,4], ignore.case = T),]
  table <- table[grepl("\\d+:\\d+\\.", table[,1]),]
  
  # coloca vazio nas celulas vazias lidas com ? ou somente espaço
  # A cédula vazia em tabelas HTML vem com o código unicode \x00A0
  table %<>% mutate_at(vars(2:6), ~ gsub("^(\\?|\\x{00A0})$", "", ., perl = T))
  
  # Clean some columns
  table %<>% mutate_at(vars(pts.home, pts.away), ~ inset(., !grepl("^\\+", .), "") %>% gsub("\\+", "", .))
  table %<>% mutate_at(vars(score), ~ inset(., !grepl("\\d+-\\d+", .), ""))
  
  # Removing weird rows with NA on description
  table %<>% drop_na(teams.name[1], teams.name[2])
  
  return(table)
}


season <- "2017"
pbptable.path <- file.path(config$pbp_folder, season)

info(logger, str_glue("Craw pbp tables from season {season}"))

pbpurls = c()
allgamecodes = c()
for(month in tolower(c(month.name[10:12], month.name[1:6]))) {
  url <- paste0("https://www.basketball-reference.com/leagues/NBA_", season, "_games-", month, ".html")
  
  gamecodes <- url %>% read_html() %>%
    html_node("#schedule") %>%
    html_nodes(xpath = '//th[@data-stat="date_game" and @csk]') %>%
    html_attr("csk")
  
  allgamecodes = c(allgamecodes, gamecodes)
  pbpurls = c(pbpurls, paste0("https://www.basketball-reference.com/boxscores/pbp/", gamecodes, ".html"))
}

library(doParallel)
#cl <- makeForkCluster(detectCores())
#registerDoParallel(cl)
registerDoParallel(cores=detectCores())
on.exit(stopImplicitCluster())

m_ply(bind_cols(url=pbpurls, gamecode=allgamecodes), 
      function(url, gamecode) {
        info(logger, str_glue("crawling {url}"))
        tab <- CrawlPbpTable(url)
        SaveTable(tab, pbptable.path, paste0(gamecode, ".txt"))
        info(logger, str_glue("{gamecode} finished"))
      }, .parallel = TRUE)

#stopCluster(cl)
