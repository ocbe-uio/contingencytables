# This file contains miscellaneous functions that are not to be used by a package user.

#' @title Prints welcome message on package load
#' @description Prints package version number and welcome message on package load
#' @param libname library location. See `?base::.onAttach` for details
#' @param pkgname package name. See `?base::.onAttach` for details
.onAttach <- function(libname, pkgname) {
  version <- read.dcf(
    file   = system.file("DESCRIPTION", package = pkgname),
    fields = "Version"
  )
  packageStartupMessage(
    "Welcome to ", paste(pkgname, version), ".\n",
    "Please run ?chapX or list_functions(x) to see the functions related to ",
    "chapter X."
  )
}
