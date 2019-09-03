library(ProjectTemplate)
load.project()

CrawlStandingsTable <- function(url = "http://www.basketball-reference.com/leagues/NBA_2015_standings.html") {
  require(rvest)
  table = url %>% read_html() %>%
    html_nodes('#all_expanded_standings') %>%
    html_nodes(xpath = 'comment()') %>%
    html_text() %>%
    read_html() %>%
    html_node('table') %>%
    html_table()
  names(table)[1:3] <- table[1,1:3]
  names(table)[4:24] <- paste(names(table)[4:24], table[1,4:24], sep = ".")
  table <- table[-1,]
  table <- table %>% as.tibble() %>%
    mutate(Rk = as.numeric(Rk)) %>%
    arrange(Rk)
  return(table)
}

SaveTable(CrawlStandingsTable(), 'data/standings_table', 'standings.txt')

