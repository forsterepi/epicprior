#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL

#' Assignment pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%<>\%}} for details.
#'
#' @name %<>%
#' @keywords internal
#' @importFrom magrittr %<>%
#' @usage lhs \%<>\% rhs
#' @param lhs An object which serves both as the initial value and as target.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `lhs <- rhs(lhs)`.
#' @noRd
NULL

#' Default value for NULL
#'
#' @name %||%
#' @keywords internal
#' @importFrom rlang %||%
#' @usage x \%||\% y
#' @param x If x is NULL, will return y; otherwise returns x.
#' @param y If x is NULL, will return y; otherwise returns x.
#' @return If x is NULL, will return y; otherwise returns x.
#' @noRd
NULL

# Avoid the devtools::check() NOTE that the . used in piping is a non-declaree
# variable
utils::globalVariables(".")

# devtools::check() does not realize that packages bslib, DT are used in the
# shinyapp code
# Strategy suggested in R packages (2e), 11.4.1.1 How to not use a package in
# Imports
ignore_unused_imports <- function() {
  bslib::version_default()
}
