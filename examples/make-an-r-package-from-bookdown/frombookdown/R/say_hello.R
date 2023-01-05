# Generated from _main.Rmd: do not edit by hand

#' Say hello to someone
#' 
#' @param name Name of a person
#' @param exclamation Whether to include an exclamation mark
#' @export 
say_hello <- function(name, exclamation = TRUE) {
  paste0("Hello ", name, ifelse(exclamation, "!", "."))
}
