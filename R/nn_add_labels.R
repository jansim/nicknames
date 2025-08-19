#' Add Labels to Dataframe Columns via Nicknames
#'
#' This function takes a dataframe and adds labels to columns that have
#' registered nicknames. The labels are stored as the "label" attribute
#' for each matching column and are visible in e.g. the Rstudio data viewer.
#'
#' @param df A dataframe to add labels to
#' @param dict The dictionary name to use for nickname lookups (defaults to "default")
#'
#' @return The dataframe with label attributes added to matching columns
#'
#' @examples
#' # Register some nickname mappings
#' nn_register(c(
#'   "mpg" = "Miles per Gallon",
#'   "hp" = "Horsepower",
#'   "cyl" = "Number of Cylinders"
#' ))
#'
#' # Add labels to mtcars dataframe
#' labeled_mtcars <- nn_add_labels(mtcars)
#'
#' # If you're using Rstudio, run View(labeled_mtcars)
#' # or check the labels manually:
#' attr(labeled_mtcars$mpg, "label") # "Miles per Gallon"
#' attr(labeled_mtcars$hp, "label") # "Horsepower"
#'
#' @export
nn_add_labels <- function(df, dict = "default") {
  if (!is.data.frame(df)) {
    stop("Input must be a dataframe")
  }

  env <- .get_nn_env(dict)

  # Get column names
  col_names <- colnames(df)

  # For each column, check if there's a nickname mapping and add label
  for (col_name in col_names) {
    if (exists(col_name, envir = env, inherits = FALSE)) {
      attr(df[[col_name]], "label") <- env[[col_name]]
    }
  }

  return(df)
}
