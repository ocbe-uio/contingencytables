# This file contains miscellaneous functions that are not to be used by a package user.

#' @title Prints welcome message on package load
#' @param libname no idea, but will break devtools::document() if removed
#' @param pkgname name of the package
#' @description Prints package version number and welcome message on package load
.onAttach <- function(libname, pkgname) {
	version <- read.dcf(
		file   = system.file("DESCRIPTION", package = pkgname),
		fields = "Version")
	packageStartupMessage(
		"Welcome to ", paste(pkgname, version), ".\n",
		"Please load functions from chapter X by running chapter(X)."
	)
}