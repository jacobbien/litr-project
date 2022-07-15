# Generated from create-rF2C.Rmd: do not edit by hand

#' Convert a Fahrenheit temperature to Celsius
#' @param temp A temperature measurement on the Fahrenheit temperature scale
#' @export 
F2C <- function(temp){
        if(!is.numeric(temp)) stop('Please provide a numeric argument')
        (temp-32) * 5/9
}
