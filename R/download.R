
miniconda_installer_url <- function (version = "3") {
  base <- "https://repo.anaconda.com/miniconda"
  rver <- R.version
  if( isTRUE(rver$arch == "aarch64") ){
    arch <- "armv71"
    if( startsWith(rver$os, 'darwin') ){
      return("https://github.com/conda-forge/miniforge/releases/latest/download/Miniforge3-MacOSX-arm64.sh")
    }
  } else {
    arch <- ifelse(.Machine$sizeof.pointer == 8, "x86_64", "x86")
  }
  version <- as.character(version)
  name <- if (is_windows())
    sprintf("Miniconda%s-latest-Windows-%s.exe", version,
            arch)
  else if (is_osx())
    sprintf("Miniconda%s-latest-MacOSX-%s.sh", version, arch)
  else if (is_linux())
    sprintf("Miniconda%s-latest-Linux-%s.sh", version, arch)
  else stopf("unsupported platform %s", shQuote(Sys.info()[["sysname"]]))
  file.path(base, name)
}

install_conda <- function(path, update = TRUE, force = FALSE){
  ns <- asNamespace('reticulate')
  ns$check_forbidden_install("Miniconda")
  if (grepl(" ", path, fixed = TRUE))
    stop("cannot install Miniconda into a path containing spaces")
  ns$install_miniconda_preflight(path, force)
  url <- miniconda_installer_url()
  installer <- ns$miniconda_installer_download(url)
  message("* Installing Miniconda -- please wait a moment ...")
  ns$miniconda_installer_run(installer, path)
  ok <- ns$miniconda_exists(path) && ns$miniconda_test(path)
  if (!ok)
    stopf("Miniconda installation failed [unknown reason]")
  if (update)
    ns$miniconda_update(path)
  conda <- ns$miniconda_conda(path)
  python <- ns$miniconda_python_package()
  ns$messagef("* Miniconda has been successfully installed at %s.",
           shQuote(path))
  path
}
