#' Use Nicknames in ggplot2 Plots
#'
#' This function provides a convenient way to apply human readable labels to
#' ggplot2 plots, by first registering them using \code{\link{nn_register}} and
#' then applying them using this function.
#'
#' This makes it easy to specify nice names once and use them across a project.
#'
#' @param dict The dictionary name to use for nickname lookups (optional).
#'
#' @seealso \code{\link{labs_map}} for direct remapping
#'
#' @examples
#' library(ggplot2)
#'
#' # Register nicknames
#' nn_register(c(
#'   "mpg" = "Miles per Gallon",
#'   "hp" = "Horsepower",
#'   "factor(cyl)" = "Number of\nCylinders"
#' ))
#'
#' # Create plot and apply nickname labels
#' ggplot(mtcars, aes(x = mpg, y = hp, color = factor(cyl))) +
#'   geom_point() +
#'   labs_nn()
#'
#' @export
labs_nn <- function(dict = "default") {
  env <- .get_nn_env(dict)
  labs_map(names = env)
}
