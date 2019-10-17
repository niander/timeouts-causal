projroot <- rprojroot::find_root(rprojroot::is_rstudio_project)

SaveTable <- function(table, folder, name, ...) {
  if(!dir.exists(folder))
    dir.create(file.path(projroot, folder), recursive = T)
  write_delim(table, file.path(folder, name), ...)
}

ReadTable <- function(folder, name, fullpath = NULL, delim = " ", header = T, col_types = cols(), ...) {
  fullpath <- ifelse(is.null(fullpath), file.path(projroot, folder, name), fullpath)
  tab <- read_delim(fullpath, delim, col_names = header, col_types = col_types, ...)
  #attr(tab, "spec") <- NULL
  return(tab)
}

SaveObject <- function(obj, folder, name = NULL, fullpath = NULL, ...) {
  if(!is.null(fullpath)) {
    folder <- path_dir(fullpath)
    name <- path_file(fullpath)
  } else {
    folder <- path(projroot, folder)
    name <- if_else(is.null(name), expr_name(enexpr(obj)), name) %>%
      str_c(".rds")
  }
  dir_create(folder)
  saveRDS(obj, path(folder, name), ...)
}

LoadObject <- function(folder, name, fullpath = NULL, ...) {
  fullpath <- ifelse(is.null(fullpath), file.path(projroot, folder, name), fullpath)
  readRDS(fullpath, ...)
}

LoadObjects <- function(folder, which = NULL, env_to_save = NULL, ...) {
  paths <- dir_ls(folder, regexp = which) %>% path_filter(glob = "*.rds")
  
  objs <- paths %>%
    set_names(path_ext_remove(path_file(.))) %>%
    map(~ LoadObject(fullpath = .x, ...))
  
  if(!is.null(env_to_save)) {
    env_bind(env_to_save, !!!objs)
  } else {
    return(objs)
  }
}

LoadObjectList <- function(folder, name, df = F, col.name = NULL, ...) {
  obj <- LoadObject(folder, name, ...)
  if(df)
    return(bind_rows(obj, .id = col.name))
  else
    return(obj)
}
