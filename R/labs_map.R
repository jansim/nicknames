#' Map Names to Aesthetic Labels in ggplot2
#'
#' This function automatically maps variable names to labels based on a plot's
#' aesthetic mapping. It provides a convenient way to set multiple labels at
#' once by matching the names provided to aes / used by default in ggplot with
#' a prespecified list.
#'
#' This is particularly useful when you have a consistent naming scheme for
#' variables and want to apply human-readable labels without manually specifying
#' each aesthetic.
#'
#' @param names A named list or vector where names are variable names and values
#'   are the desired labels for those variables in the plot.
#'
#' @return An object of class "labs_map" that can be added to a ggplot object
#'   using the + operator. When added, it will automatically apply appropriate
#'   labels based on the plot's aesthetic mappings.
#'
#'
#' @examples
#' library(ggplot2)
#'
#' # Create plot and apply labels
#' ggplot(mtcars, aes(x = mpg, y = hp, color = factor(cyl))) +
#'   geom_point() +
#'   labs_map(c(
#'     "mpg" = "Miles per Gallon",
#'     "hp" = "Horsepower",
#'     "factor(cyl)" = "Number of\nCylinders"
#'   ))
#'
#' @seealso \code{\link[ggplot2]{labs}} for manual label setting
#'
#' @export
labs_map <- function(names) {
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    # Register S3 Method for ggplot_add
    registerS3method("ggplot_add", "labs_map", ggplot_add.labs_map, envir = asNamespace("ggplot2"))
  } else {
    stop("Please install ggplot2 before running labs_map.")
  }

  # Create a simple list with class that ggplot2 can handle
  structure(
    list(names = names),
    class = "labs_map"
  )
}

# Method that will handle the actual logic, arguments get forwarded
ggplot_add.labs_map <- function(object, plot, object_name) {
  # Get the aesthetic mappings from the plot
  aes_mapping <- plot$mapping

  labs_args <- list()
  # Retrieve correct names for all aesthetics
  for (var_name in names(object$names)) {
    for (aes_name in names(aes_mapping)) {
      if (!is.null(aes_mapping[[aes_name]]) && rlang::as_label(aes_mapping[[aes_name]]) == var_name) {
        labs_args[[aes_name]] <- object$names[[var_name]]
      }
    }
  }

  # Apply the labels to the plot
  plot + do.call(ggplot2::labs, labs_args)
}
