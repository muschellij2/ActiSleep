#' This code was written by Alice Cleynen, Guillem Rigaill, and Michel Koskas
#' as part of the Segmentor3IsBack package, which is no longer in CRAN. It has
#' been imported into the ActiSleep package to ensure this package's longevity.
#'
#' @title Announce Segmentor3IsBack has Loaded
#'
#' @description Print out a statement to the console when Segmentor3IsBack is
#' successfully loaded
#'
#' @param lib FILLER TEXT
#' @param pkg FILLER TEXT
#'
.onAttach <- function(lib, pkg) {
    packageStartupMessage("Segmentor3IsBack v2.0 Loaded \n")
}


