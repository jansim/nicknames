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
#'     "cyl" = "Number of\nCylinders"
#'   ))
#'
#' # Even though names (e.g. cyl) are extracted, exact matches take priority
#' ggplot(mtcars, aes(x = mpg, y = hp, color = factor(cyl))) +
#'   geom_point() +
#'   labs_map(c(
#'     "mpg" = "Miles per Gallon",
#'     "hp" = "Horsepower",
#'     "cyl" = "Number of\nCylinders",
#'     "factor(cyl)" = "Number of\nCylinders (factor)"
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

get_candidate_names <- function(expr) {
  full_expr <- rlang::as_label(expr)
  var_names <- all.vars(rlang::quo_get_expr(expr))

  # Only use extracted varnames if there's only one variable involved
  if (length(var_names) == 1) {
    return(c(full_expr, var_names))
  } else {
    return(c(full_expr))
  }
}

# Method that will handle the actual logic, arguments get forwarded
ggplot_add.labs_map <- function(object, plot, object_name) {
  # Get the aesthetic mappings from the plot
  aes_mapping <- plot$mapping

  labs_args <- list()

  for (aes_name in names(aes_mapping)) {
    if (!is.null(aes_mapping[[aes_name]])) {
      aes_expr <- aes_mapping[[aes_name]]

      # Get all candidate names (full expression + variable names)
      candidate_names <- get_candidate_names(aes_expr)

      # Find first match in order of preference
      matching_name <- intersect(candidate_names, names(object$names))
      if (length(matching_name) > 0) {
        labs_args[[aes_name]] <- object$names[[matching_name[1]]]
      }
    }
  }

  # Apply the labels to the plot
  plot + do.call(ggplot2::labs, labs_args)
}
