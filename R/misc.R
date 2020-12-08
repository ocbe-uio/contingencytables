# This file contains miscellaneous functions that are not to be used by a package user.

#' @title Prints welcome message on package load
#' @description Prints package version number and welcome message on package load
#' @param libname library location. See `?base::.onAttach` for details
#' @param pkgname package name. See `?base::.onAttach` for details
.onAttach <- function(libname, pkgname) {
	version <- read.dcf(
		file   = system.file("DESCRIPTION", package = pkgname),
		fields = "Version")
	packageStartupMessage(
		"Welcome to ", paste(pkgname, version), ".\n",
		"Please load functions from chapter X by running load_chapter(X)."
	)
}

#' @title Package script
#' @description Ad-hoc packages a function script
#' @param filename name of the file
#' @param saveOutput if `TRUE`, `filename` is overwritten. Defaults to `FALSE`
#' @return text converted to R, printed to screen or replacing input file
#' @note This function is used to expedite conversion of the original scripts
#' into a package format
#' @author Waldir Leoncio
#' @importFrom utils write.table
reformatScript <- function(filename, saveOutput = FALSE) {

	# ======================================================== #
	# Verification                                             #
	# ======================================================== #
	if (!file.exists(filename)) stop("File not found")

	# ======================================================== #
	# Reading file into R                                      #
	# ======================================================== #
	txt <- readLines(filename)

	# ======================================================== #
	# Converting code                                          #
	# ======================================================== #

	# Function documentation --------------------------------- #
	fun_name <- gsub("(.+)\\s+=\\s+function\\(.+", "\\1", txt[1]) # FIXME: it's not always the 1st line (e.g. Wald_CI_CC_1x2). Create a fun_line first
	chap_line <- which(grepl(".+Chapter (\\d{1,2}) .+", txt))
	chap_num <- sub(".+Chapter (\\d{1,2}) .+", "\\1", txt[chap_line])
	for (cl in seq_len(chap_line)) {
		txt[chap_line - cl] <- gsub("#", "#' @description", txt[chap_line - cl])
	}
	txt <- gsub("\\s*#$", "", txt)

	# Function arguments, general ---------------------------- #
	txt <- gsub("# Input arguments", "", txt)
	txt <- gsub("# ---------------", "", txt)
	txt <- gsub("#\\s{2,4}X", "X", txt)

	# Function arguments, specific --------------------------- #
	txt <- gsub(
		pattern = "X = (\\d+); n = (\\d+).+# Example:",
		replacement = paste0(fun_name, "(X=\\1, n=\\2) #"),
		x = txt
	)
	txt <- gsub(
		pattern = "X = (\\d+); n = (\\d+); pi0 = 0.(\\d+)\\s+# Example:",
		replacement = paste0(fun_name, "(X=\\1, n=\\2, pi0=0.\\3) #"),
		x = txt
	)
	txt <- gsub(".*#\\s([^(E|H)]{,15})(:| =)(.+[^;])$", "#' @param \\1\\3", txt)
	txt <- gsub(
		pattern = paste0(fun_name, "(\\(.+\\)) # (.+)"),
		replacement = paste0("#' # \\2\n#' ", fun_name, "\\1"),
		x = txt
	)
	txt <- gsub(
		pattern = "\\s+if\\s*\\(.+(n|pi0)\\)\\) \\{",
		replacement = paste0("#' @examples load_chapter(", chap_num,")"),
		txt
	)
	txt <- gsub("\\s+#' (.+)", "#' \\1", txt)

	# TODO: after idenfifying the documentation block, isolate it and move to beginning of the character vector

	# Function code, general ---------------------------------- #
	txt <- gsub("printresults=T)", "printresults=TRUE)", txt)
	txt <- gsub("quote=F)", "quote=FALSE)", txt)
	txt <- gsub("(\\S)\\+(\\S)", "\\1 + \\2", txt)
	txt <- gsub("(\\W)\\S\\-\\S(\\W)", "\\1 - \\2", txt)
	txt <- gsub("(\\S)\\*(\\S)", "\\1 * \\2", txt)
	txt <- gsub("(\\S)\\/(\\S)", "\\1 / \\2", txt)

	# Assignment operator ------------------------------------ #
	txt <- gsub("^(\\s{4,8})(\\w+)\\s=\\s", "\\1\\2 <- ", txt)
	txt <- gsub("^(\\s{4,8})names\\((\\w+)\\)\\s=\\s", "\\1names(\\2) <- ", txt)
	txt <- gsub("^(.+)\\s=\\sfunction", "\\1 <- function", txt)

	# Indentation -------------------------------------------- #
	txt <- gsub("^\\s{8}", "\t\t", txt) # indentation level 2
	txt <- gsub("^\\s{4}", "\t", txt) # indentation level 1


	# ======================================================== #
	# Returning converted code                                 #
	# ======================================================== #
	if (!saveOutput) {
		return(cat(txt, sep="\n"))
	} else {
		return(
			write.table(
				x         = txt,
				file      = filename,
				quote     = FALSE,
				row.names = FALSE,
				col.names = FALSE
			)
		)
	}
}
# TODO: import diff functionality from rBAPS (extract as separate function 1st)