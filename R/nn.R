# Package-level environment to store different dictionaries
.nn_envs <- new.env(parent = emptyenv())

# Get or create a dictionary environment
.get_nn_env <- function(dict) {
  if (!exists(dict, envir = .nn_envs, inherits = FALSE)) {
    .nn_envs[[dict]] <- new.env(parent = emptyenv())
  }
  return(.nn_envs[[dict]])
}

#' Nickname registration and lookup
#'
#' Register and look up nickname mappings.
#'
#' @param mappings A named vector where names are original values and values are nicknames
#' @param x The value to look up, this can be a dataframe or character vector.
#' @param dict The dictionary name to use (defaults to "default").
#'
#' @return
#' \code{nn_register()} returns nothing.
#' \code{nn()} returns the nickname if one is registered, otherwise the original value.
#'
#' @examples
#' # Register some nickname mappings
#' nn_register(c(
#'   "Jennifer" = "Jen",
#'   "Robert" = "Bob",
#'   "Elizabeth" = "Liz"
#' ))
#'
#' # Look up nicknames
#' nn("Jennifer")  # Returns "Jen"
#' nn("Robert")    # Returns "Bob"
#' nn("John")      # Returns "John" (no mapping registered)
#'
#' # Use different dictionaries
#' nn_register(c("Jennifer" = "Jenny"), dict = "alt")
#' nn("Jennifer")              # Returns "Jen" (from default dict)
#' nn("Jennifer", dict = "alt") # Returns "Jenny" (from alt dict)
#'
#' @export
nn_register <- function(mappings, dict = "default") {
  env <- .get_nn_env(dict)

  # Check if mappings have names
  if (is.null(names(mappings)) && length(mappings > 0)) {
    warning("Mappings are unnamed and will be ignored. Please provide named mappings e.g. c(\"A\" = \"a\").")
    return(invisible())
  }
  # Check for unnamed elements and warn if present
  unnamed_indices <- which(names(mappings) == "" | is.na(names(mappings)))
  if (length(unnamed_indices) > 0) {
    warning("Found ", length(unnamed_indices), " unnamed mapping(s) which will be ignored.")
    # Remove unnamed elements
    mappings <- mappings[-unnamed_indices]
  }

  # Register the named mappings
  if (length(mappings) > 0) {
    for (name in names(mappings)) {
      env[[name]] <- mappings[[name]]
    }
  }
}

#' @rdname nn_register
#' @export
nn <- function(x, dict = "default") {
  UseMethod("nn")
}

# Default renaming function, taking in single values or vectors
#' @rdname nn_register
#' @export
nn.default <- function(x, dict = "default") {
  env <- .get_nn_env(dict)

  sapply(x, function(item) {
    if (exists(item, envir = env, inherits = FALSE)) env[[item]] else item
  }, USE.NAMES = FALSE)
}

# Dataframe variant running over colnames
#' @rdname nn_register
#' @export
nn.data.frame <- function(x, dict = "default") {
  new_colnames <- nn.default(colnames(x), dict = dict)
  colnames(x) <- new_colnames
  return(x)
}
