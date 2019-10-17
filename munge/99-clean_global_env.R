if(config$clean_global_vars) {
  to.remove.suffix <- as.character(exprs(games.list, games.teams, games.df,
                                         games.perposs, games.inflscore, timeouts.inflscore,
                                         contr.inflscore, timeout.effect.data))
  to.remove <- paste(rep(to.remove.suffix, each = length(config$seasons)),
                     config$seasons,
                     sep = ".")
  
  env_unbind(global_env(), to.remove)
}
