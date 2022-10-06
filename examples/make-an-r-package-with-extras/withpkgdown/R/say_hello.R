# Generated from create-withpkgdown.Rmd: do not edit by hand

#' Say hello to someone
#' 
#' @param name Name of a person
#' @param exclamation Whether to include an exclamation mark
#' @export 
say_hello <- function(name, exclamation = TRUE) {
  paste0("Hello ", name, ifelse(exclamation, "!", "."))
}

#' Say hi to someone
#' 
#' @param name Name of a person
#' @param exclamation Whether to include an exclamation mark
#' @export 
say_hi <- function(name, exclamation = TRUE) {
  paste0("Hi ", name, ifelse(exclamation, "!", "."))
}
