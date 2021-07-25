#' Prints out cumhit object
#'
#' @param object A [cumhist][cumhist-class()] object
#' @param ... Unused
#' @return Nothing, console output only.
#'
#' @export
#'
#' @examples
#' #' \donttest{
#' br_fit <- fit_cumhist(br_singleblock, state="State", duration="Duration", fixed="Time")
#' br_fit
#' }

print.cumhist <- function(object, ...){
  cat('Call: ')
  print(object$Call)
  cat('\n')

  cat(glue::glue('Family: {object$family}\n\n'))
}

