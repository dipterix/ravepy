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


#' @export
configure_matlab <- function(matlab){
  # matlab <- '/Applications/MATLAB_R2020b.app'
  matlab <- matlab[[1]]
  mat_engine_path <- file.path(matlab, "extern/engines/python/")
  setwd2(mat_engine_path)

  py_path <- reticulate::conda_python(env_path())
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
configure <- function(python_ver = "3.8",
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
    reticulate::install_miniconda(path = path, update = update, force = force)
  }

  # create virtual env
  if(force || update || !CONDAENV_NAME %in% reticulate::conda_list()[['name']]){
    reticulate::conda_create(env_path())
  }

  add_packages(packages, python_ver = python_ver)

  # check matlab
  if(length(matlab)){
    configure_matlab(matlab)
  }


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
call_matlab <- function(fun, ..., .options = getOption("ravepy.matlab_opt", "-nodesktop -nojvm")){

  matlab <- matlab_engine()
  if(is.null(matlab)){
    stop("Matlab engine not configured. Please run `configure_matlab(matlab_root)` to set up matlab")
  }


  existing_engines <- getOption("ravepy.matlab_engine", NULL)
  if(is.null(existing_engines)){
    existing_engines <- dipsaus::fastqueue2()
    options("ravepy.matlab_engine" = existing_engines)
  }

  suc <- FALSE
  if(existing_engines$size()){
    engine <- existing_engines$remove()
    suc <- tryCatch({
      force(engine$workspace)
      TRUE
    }, error = function(e){
      # engine is invalid, quit
      engine$quit()
      FALSE
    })
  }
  if(!suc){
    engine <- matlab$start_matlab(.options)
  }
  on.exit({
    tryCatch({
      force(engine$workspace)
      existing_engines$add(engine)
    }, error = function(e){
      # engine is invalid, quit
      engine$quit()
    })
  })

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
