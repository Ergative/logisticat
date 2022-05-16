#' Set the alpha value of a color.
#'
#' @param color Color specified as a name or hex string.
#' @param alpha Value in range \[0,1\] for alpha.
#'
#' @keywords internal
#'
#' @return A hex string representing the color with the chosen alpha.
with.alpha <- Vectorize(function(color, alpha=1) {
  a <- clamp(floor(alpha * 255), 0, 255)
  rgb <- grDevices::col2rgb(color, F)
  paste0("#", byte2hex(rgb[1]), byte2hex(rgb[2]), byte2hex(rgb[3]), byte2hex(a))
}, vectorize.args = "color")


#' Constrain a numeric value within a range.
#'
#' @param value Value to constrain.
#' @param min.value Minimum, inclusive.
#' @param max.value Maximum, inclusive.
#'
#' @keywords internal
#'
#' @return The closest number in the range \[min.value,max.value\] to value.
clamp <- function(value, min.value, max.value) {
  max(min.value, min(max.value, value))
}


#' Get a two-character hex string representation of a number.
#'
#' @param byte Integer value in range \[0,255\]
#'
#' @keywords internal
#'
#' @return String representation of byte.
byte2hex <- function(byte) {
  paste0(nybble2hex(bitwShiftR(byte, 4)), nybble2hex(bitwAnd(byte, 15)))
}


#' Get a hex string representation of a nybble
#'
#' @param nybble Integer value in range \[0,15\]
#'
#' @keywords internal
#'
#' @return String representation of nybble.
nybble2hex <- function(nybble) {
  nybble.names = c("0", "1", "2", "3", "4", "5", "6", "7",
                   "8", "9", "a", "b", "c", "d", "e", "f")
  nybble.names[[nybble + 1]] # 1-indexed.

}


#' Get a vector from a data frame column.
#'
#' @param data Data frame.
#' @param column Column to extract - can be used with \{\{\}\}.
#'
#' @keywords internal
#'
#' @return Vector of column contents.
column.to.vector <- function(data, column) {
  column.name <- as_name(rlang::ensym(column))
  data[[column.name]]
}


#' Get a list a row vectors from a data frame.
#'
#' @param data Data frame.
#'
#' @keywords internal
#'
#' @return A list, with one vector for each row.
list.df.rows <- function(data){
  # See https://stackoverflow.com/a/14370455/4104189
  lapply(stats::setNames(split(data, seq(nrow(data))), rownames(data)),
         unlist) # Unlist so that we have vector elements instead of tibbles.

}


log.caller <- function(){
  caller <- sys.call(which=1)
  print (caller)
}
