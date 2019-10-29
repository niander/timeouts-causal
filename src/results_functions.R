PrintTableOnes <- function(df, xvars = .(slope.before, quarter, seconds, margin), just_home = T) {
  tabones <- list()
  i <- 1
  for(dta in config$deltas) {
    print(str_glue("===========================\ndelta: {dta}"))
    data <- df %>% filter(delta == dta)
    ddply(data, .(match.poss), function(d) {
      if(just_home) {
        if(d$match.poss[1] != "home") return()
      }
      tab.one <- CreateTableOne(vars = as.character(xvars), strata = c("treatment"), data = d, test = F)
      print(str_glue("poss:  {d$match.poss[1]}"))
      l <- print(tab.one, noSpaces = T, smd = T)
      #tabones <<- list_merge(tabones, !!str_c(dta, d$match.poss[1]) := as.matrix(l))
      tabones[[i]] <<- l
      i <<- i + 1
      return(NULL)
    })
  }
  return(invisible(tabones))
}

PrintTableOnesUnify <- function(df, xvars = .(slope.before, quarter, seconds, margin)) {
  df %<>%
    mutate_at(vars(slope.before, slope.after, infl.score), funs(if_else(match.poss == "away", -1 * ., .)))
  tabones <- list()
  i <- 1
  for(dta in config$deltas) {
    print(str_glue("===========================\ndelta: {dta}"))
    d <- df %>% filter(delta == dta)
    tab.one <- CreateTableOne(vars = as.character(xvars), strata = c("treatment"), data = d, test = F)
    l <- print(tab.one, noSpaces = T, smd = T)
    #tabones <<- list_merge(tabones, !!str_c(dta, d$match.poss[1]) := as.matrix(l))
    tabones[[i]] <- l
    i <- i + 1
  }
  return(invisible(tabones))
}

library(cowplot)

PlotLines <- function(data) {
  plt.lines <- dlply(data, .(method), function(dtf) {
    dtf %<>%
      mutate(treatment = factor(treatment))
    
    plt1 <- dtf %>%
      ggplot(aes(seconds, fill = treatment, color = treatment)) +
      geom_histogram(aes(y = stat(count / sum(count))), 
                     binwidth = 10, position = "identity", alpha = 0.15, size = 0.6) +
      scale_y_continuous(name = "prop", labels = function(x) paste0(x*100,"%")) +
      scale_color_viridis_d("Treatment Group",
                            #labels = c(expression(A[t]==0), expression(A[t]==1)),
                            labels = c("Control", "Timeouts"),
                            end = 0.5, aesthetics = c("colour", "fill"))
    
    plt2 <- dtf %>% 
      mutate(quarter = if_else(quarter > 4, "OT", as.character(quarter))) %>%
      ggplot(aes(quarter, fill = treatment, color = treatment)) +
      geom_bar(aes(y = stat(count/sum(count))), 
               position = "identity", alpha = 0.15, size = 0.6) +
      scale_y_continuous(name = "prop", labels = function(x) paste0(x*100,"%")) +
      scale_color_viridis_d(end = 0.5, aesthetics = c("colour", "fill"))
    
    plt3 <- dtf %>%
      ggplot(aes(margin, fill = treatment, color = treatment)) +
      geom_histogram(aes(y = stat(count / sum(count))),
                     binwidth = 2, position = "identity", alpha = 0.15, size = 0.6) +
      scale_y_continuous(name = "prop", labels = function(x) paste0(x*100,"%")) +
      scale_x_continuous(breaks = c(-30, 0, 30)) +
      scale_color_viridis_d(end = 0.5, aesthetics = c("colour", "fill"))
    
    cmb <- plot_grid(plt1 + theme(plot.margin = unit(c(6,0,6,0), "pt"),
                                  legend.position = "none"),
                     plt2 + theme(axis.title.y = element_blank(), 
                                  plot.margin = unit(c(6,0,6,0), "pt"),
                                  legend.position = "none"),
                     plt3 + theme(axis.title.y = element_blank(), 
                                  plot.margin = unit(c(6,0,6,0), "pt"),
                                  legend.position = "none"),
                     align = 'h',
                     nrow = 1,
                     rel_widths = c(1.8,1,1))
    #method.name <- df$method[[1]]
    plt1 <- dtf %>%
      ggplot(aes(seconds, fill = treatment, color = treatment)) +
      geom_histogram(aes(y = stat(count / sum(count))), 
                     binwidth = 10, position = "identity", alpha = 0.15, size = 1) +
      scale_y_continuous(name = "prop", labels = function(x) paste0(x*100,"%")) +
      scale_color_viridis_d("Treatment Group",
                            #labels = c(expression(A[t]==0), expression(A[t]==1)),
                            labels = c("Control", "Timeouts"),
                            end = 0.5, aesthetics = c("colour", "fill"))
    return(lst(cmb, plt1))
  })
}

AddLegend <- function(plts, labels = c("No-balance", "Mahalanobis", "Propensity")) {
  plotlist <- map(plts, "cmb")
  plt <- plot_grid(plotlist = plotlist, 
                   ncol = 1,
                   labels = labels,
                   label_x = 0.1,
                   hjust = 0,
                   label_size = 11) %>%
    plot_grid(get_legend(plts[[1]]$plt1 + theme(legend.position = "bottom",
                                                legend.justification = "center")),
              ncol = 1, 
              rel_heights = c(1,.08))
  return(plt)
}

# Remeber to drop any NA on match.id
RunInferPermutation <- function(data) {
  data.sp <- data %>%
    mutate_at(vars(treatment), factor) %>%
    specify(infl.score ~ treatment)
  
  ####
  old.random.state <- .Random.seed
  set.seed(222)
  
  nulldist.diff <- data.sp %>%
    hypothesize(null = "independence") %>%
    generate(reps = 10000, type = "permute") %>%
    calculate(stat = "diff in means", order = c("timeout", "control"))
  
  .Random.seed <- old.random.state
  ###
  
  obs.diff <- data.sp %>%
    calculate(stat = "diff in means", order = c("timeout", "control"))
  
  return(tibble(nulldist.diff = list(nulldist.diff), 
                obs.diff = pull(obs.diff)))
}

RIPListToDataFrame <- function(l) {
  labels <- l %@% 
    "split_labels" %>%
    as_tibble()
  res <- l %>%
    enframe(name = "run", value = "results") %>%
    unnest(results) %>%
    select(-run)
  bind_cols(labels, res)
}

GetPValues <- function(df, get.ci = TRUE, filter.home = TRUE, separate.effects = FALSE) {
  aux.df <- df %>%
    rowwise()
  
  if(get.ci) {
    aux.df %<>%
      do(bind_cols(get_ci(.$nulldist.diff),
                   get_pvalue(.$nulldist.diff, .$obs.diff, "both")))
  } else {
    aux.df %<>%
      do(bind_cols(get_pvalue(.$nulldist.diff, .$obs.diff, "both")))
  }
  
  aux.df %<>%
    ungroup() %>%
    bind_cols(df, .) %>%
    select(-nulldist.diff)
  
  if(filter.home) {
    aux.df %<>% filter(match.poss == "home")
  } else {
    if(separate.effects) {
      aux.df %<>%
        complete(method, delta, match.poss) %>%
        nest(obs.diff, p_value, .key = "dn") %>%
        spread(match.poss, dn) %>%
        unnest(.sep = ".")
    }
  }
  return(aux.df)
}