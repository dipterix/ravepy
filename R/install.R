#' @export
CONDAENV_NAME <- "ravepy-conda-env"

install_root <- function(){
  if(Sys.info()["sysname"] == "Darwin"){
    path <- path.expand("~/Library/r-ravepy")
  } else {
    root <- normalizePath(rappdirs::user_data_dir(), winslash = "/",
                          mustWork = FALSE)
    path <- file.path(root, "r-ravepy")
  }
  getOption("ravepy.install_root", path)
}

#' @export
conda_path <- function(){
  file.path(install_root(), "miniconda")
}

#' @export
env_path <- function(){
  file.path(install_root(), CONDAENV_NAME)
}

mat_pyver <- function(mat_ver){
  list(
    '2014b' = c('2.7', '3.3'),
    '2015a' = c('2.7', '3.3', '3.4'),
    '2015b' = c('2.7', '3.3', '3.4'),
    '2016a' = c('2.7', '3.3', '3.4'),
    '2016b' = c('2.7', '3.3', '3.4', '3.5'),
    '2017a' = c('2.7', '3.4', '3.5'),
    '2017b' = c('2.7', '3.4', '3.5', '3.6'),
    '2018a' = c('2.7', '3.5', '3.6'),
    '2018b' = c('2.7', '3.5', '3.6'),
    '2019a' = c('2.7', '3.5', '3.6', '3.7'),
    '2019b' = c('2.7', '3.6', '3.7'),
    '2020a' = c('2.7', '3.6', '3.7'),
    '2020b' = c('2.7', '3.6', '3.7', '3.8')
  )[[mat_ver]]
}

#' @export
configure_matlab <- function(matlab, python_ver = 'auto'){

  # TODO: must configure python first

  # matlab <- '/Applications/MATLAB_R2020b.app'
  matlab <- matlab[[1]]
  mat_engine_path <- file.path(matlab, "extern/engines/python/")
  py_path <- reticulate::conda_python(env_path())

  if(python_ver == 'auto'){
    # check matlab version

    try({
      s <- readLines(file.path(mat_engine_path, 'setup.py'))
      s <- stringr::str_trim(s)
      s <- s[startsWith(s, "version")]
      if(length(s)){

        s <- s[[length(s)]]
        s <- stringr::str_to_lower(s)
        mat_ver <- stringr::str_extract(s, "20[0-9]{2}[abcdefgh]")
        compatible_ver <- mat_pyver(mat_ver)


        if(length(compatible_ver)){
          # check installed python version
          ver <- system2(py_path, "-V", stdout = TRUE, stderr = TRUE)
          ver <- stringr::str_match(ver, "([23]\\.[0-9]+)\\.[0-9]+")
          ver <- ver[[2]]

          if(!ver %in% compatible_ver) {
            python_ver <- compatible_ver[[length(compatible_ver)]]
            message(sprintf("Current python version is `%s`, but matlab engine requires python version to be one of the followings: %s. Re-install python %s", ver, paste(compatible_ver, collapse = ', '), python_ver))
          }
        }


      }

    })

  }

  if(python_ver != 'auto'){
    add_packages(NULL, python_ver = python_ver)
  }


  setwd2(mat_engine_path)


  build_dir <- file.path(install_root(), "matlab-engine-build")
  dir.create(build_dir)
  build_dir <- normalizePath(build_dir)
  system2(py_path, c(
    "setup.py",
    "build",
    sprintf('--build-base="%s"', build_dir),
    "install"
  ), wait = TRUE)
}



#' @export
configure <- function(python_ver = "auto",
                      packages = NULL,
                      matlab = NULL,
                      update = TRUE, force = FALSE){

  old_path <- Sys.getenv('RETICULATE_MINICONDA_PATH')
  on.exit({
    Sys.setenv("RETICULATE_MINICONDA_PATH" = old_path)
  }, add = TRUE, after = TRUE)
  Sys.setenv("RETICULATE_MINICONDA_PATH" = conda_path())

  # TODO: check if conda bin exists
  path <- conda_path()
  if(force || update || !dir.exists(path)){
    # reticulate::install_miniconda(path = path, update = update, force = force)
    install_conda(path = path, update = update, force = force)
  }

  # create virtual env
  if(force || update || !CONDAENV_NAME %in% reticulate::conda_list()[['name']]){
    reticulate::conda_create(env_path())
  }

  # check matlab
  if(length(matlab)){
    configure_matlab(matlab, python_ver = python_ver)
  }

  if(!length(matlab) || length(packages)) {
    if(python_ver == 'auto'){
      python_ver <- NULL
    }
    add_packages(packages, python_ver)
  }
}

#' @export
remove_conda <- function(){
  conda_path <- conda_path()
  if(dir.exists(conda_path)){
    unlink(conda_path, recursive = TRUE, force = TRUE)
    return(invisible(TRUE))
  }
  return(invisible(FALSE))
}

#' @export
add_packages <- function(packages = NULL, python_ver = NULL) {
  old_path <- Sys.getenv('RETICULATE_MINICONDA_PATH')
  on.exit({
    Sys.setenv("RETICULATE_MINICONDA_PATH" = old_path)
  }, add = TRUE, after = TRUE)
  Sys.setenv("RETICULATE_MINICONDA_PATH" = conda_path())


  # install packages
  packages <- unique(c('numpy', 'h5py', 'scipy', 'matplotlib', "ipython", packages))
  reticulate::conda_install(env_path(), packages = packages,
                            python_version = python_ver)
}

#' @export
ensure_ravepy <- function(){
  old_path <- Sys.getenv('RETICULATE_MINICONDA_PATH')
  on.exit({
    Sys.setenv("RETICULATE_MINICONDA_PATH" = old_path)
  }, add = TRUE, after = TRUE)
  Sys.setenv("RETICULATE_MINICONDA_PATH" = conda_path())
  reticulate::use_condaenv(CONDAENV_NAME, required = TRUE)
  reticulate::py_config()
}

#' @export
matlab_engine <- function(){
  old_path <- Sys.getenv('RETICULATE_MINICONDA_PATH')
  on.exit({
    Sys.setenv("RETICULATE_MINICONDA_PATH" = old_path)
  }, add = TRUE, after = TRUE)
  Sys.setenv("RETICULATE_MINICONDA_PATH" = conda_path())
  reticulate::use_condaenv(CONDAENV_NAME, required = TRUE)

  if(reticulate::py_module_available("matlab.engine")){
    matlab <- reticulate::import('matlab.engine')
    return(invisible(matlab))
    # try({
    #   eng <- matlab$start_matlab(matlab_param)
    # })
  }
  return(invisible())
  # eng$biliear(matrix(rnorm(10), nrow = 1), matrix(rnorm(10), nrow = 1), 0.5)
  # eng$biliear(rnorm(10), rnorm(10), 0.5)

}


#' @export
call_matlab <- function(fun, ..., .options = getOption("ravepy.matlab_opt", "-nodesktop -nojvm"), .debug = getOption("ravepy.debug", FALSE)){

  matlab <- matlab_engine()
  if(is.null(matlab)){
    stop("Matlab engine not configured. Please run `configure_matlab(matlab_root)` to set up matlab")
  }


  existing_engines <- getOption("ravepy.matlab_engine", NULL)
  if(is.null(existing_engines)){
    existing_engines <- fastqueue2()
    options("ravepy.matlab_engine" = existing_engines)
  }

  suc <- FALSE

  if(.debug){
    message("Existing engine: ", existing_engines$size())
  }
  if(existing_engines$size()){
    same_opt <- vapply(as.list(existing_engines), function(item){
      if(!is.environment(item)){ return(FALSE) }
      isTRUE(item$options == .options)
    }, FALSE)

    if(any(same_opt)){
      idx <- which(same_opt)[[1]]
      if(idx > 1){
        burned <- existing_engines$mremove(n = idx - 1, missing = NA)
        for(item in burned){
          if(is.environment(item)){
            existing_engines$add(item)
          }
        }
      }
      item <- existing_engines$remove()
      suc <- tryCatch({
        force(item$engine$workspace)
        TRUE
      }, error = function(e){
        # engine is invalid, quit
        item$engine$quit()
        FALSE
      })
    }
  }
  if(!suc){
    if(.debug){
      message("Creating new matlab engine with options: ", .options)
    }
    item <- new.env(parent = emptyenv())
    engine <- matlab$start_matlab(.options)
    item$engine <- engine
    item$options <- .options
    reg.finalizer(item, function(item){

      if(getOption("ravepy.debug", FALSE)){
        message("Removing a matlab instance.")
      }
      try({item$engine$quit()}, silent = TRUE)
    }, onexit = TRUE)
  } else {
    if(.debug){
      message("Using existing idle engine")
    }
  }
  on.exit({
    tryCatch({
      force(item$engine$workspace)

      if(.debug){
        message("Engine is still alive, keep it for future use")
      }
      existing_engines$add(item)
    }, error = function(e){
      if(.debug) {
        message("Engine is not alive, removing from the list.")
      }
      item$engine$quit()
    })
  })
  if(.debug) {
    message("Executing matlab call")
  }
  engine <- item$engine
  res <- engine[[fun]](...)

  return(res)
}

# reticulate::repl_python()
#
#
#
# reticulate::py_config()
#
#
#
# reticulate::miniconda_update()
# reticulate::conda_install('RAVEPy', packages = 'matlab')
