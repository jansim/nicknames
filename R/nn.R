# Package-level environment to store different dictionaries
.nn_envs <- new.env(parent = emptyenv())

# Get or create a dictionary. Each dictionary is an environment holding
# - mappings: an environment with exact name -> nickname mappings
# - patterns: a list of wildcard rules (in registration order)
# - default: an optional fallback renaming function
.get_nn_dict <- function(dict) {
  if (!exists(dict, envir = .nn_envs, inherits = FALSE)) {
    d <- new.env(parent = emptyenv())
    d$mappings <- new.env(parent = emptyenv())
    d$patterns <- list()
    d$default <- NULL
    .nn_envs[[dict]] <- d
  }
  return(.nn_envs[[dict]])
}

# Get the environment of exact mappings for a dictionary
.get_nn_env <- function(dict) {
  .get_nn_dict(dict)$mappings
}

# Convert a wildcard pattern (where * matches any sequence of characters)
# into an anchored regular expression
.wildcard_to_regex <- function(pattern) {
  escaped <- gsub("([][{}()+?.\\\\^$|])", "\\\\\\1", pattern)
  paste0("^", gsub("*", ".*", escaped, fixed = TRUE), "$")
}

# Resolve a registered value: functions are called with the original name
.resolve_nn_value <- function(value, item) {
  if (is.function(value)) {
    value <- value(item)
    if (length(value) != 1) {
      stop("Renaming functions must return a single value, but returned ",
           length(value), " values for \"", item, "\".")
    }
  }
  value
}

# Look up a single name in a dictionary.
# Exact mappings take priority over wildcard patterns (checked in registration
# order), which take priority over the default renaming function.
# Returns NULL if there is no match (when use_default = FALSE or no default).
.nn_lookup <- function(item, dict = "default", use_default = TRUE) {
  d <- .get_nn_dict(dict)
  item <- as.character(item)

  if (is.na(item)) {
    return(NULL)
  }

  if (exists(item, envir = d$mappings, inherits = FALSE)) {
    return(.resolve_nn_value(d$mappings[[item]], item))
  }

  for (rule in d$patterns) {
    if (grepl(rule$regex, item)) {
      return(.resolve_nn_value(rule$value, item))
    }
  }

  if (use_default && !is.null(d$default)) {
    return(.resolve_nn_value(d$default, item))
  }

  return(NULL)
}

#' Nickname registration and lookup
#'
#' Register and look up nickname mappings.
#'
#' Nicknames can be registered in three ways, which are checked in the
#' following order during lookup:
#'
#' 1. **Exact mappings**, e.g. `c("mpg" = "Miles per Gallon")`.
#' 2. **Wildcard mappings**, where `*` in the name matches any sequence of
#'    characters, e.g. `c("score_*" = "Score")`. Patterns are checked in the
#'    order they were registered. Set `wildcard = FALSE` to treat `*`
#'    literally.
#' 3. A **default** renaming function, used for all values without a mapping.
#'
#' Instead of a string, the value of a mapping can also be a function. It is
#' called with the original value and should return the nickname. Use a
#' `list()` to combine functions and strings, e.g.
#' `list("score_*" = function(x) toupper(x), "mpg" = "Miles per Gallon")`.
#'
#' @param mappings A named vector or list where names are original values and
#'   values are nicknames or functions which take the original value and
#'   return the nickname.
#' @param x The value to look up, this can be a dataframe or character vector.
#' @param dict The dictionary name to use (defaults to "default").
#' @param default A function used to rename all values for which no mapping
#'   exists, e.g. `default = function(x) gsub("_", " ", x)`. Pass `NULL` to
#'   remove a previously registered default. If not specified, the current
#'   default is left unchanged.
#' @param wildcard Whether a `*` in the names of `mappings` should be treated
#'   as a wildcard matching any sequence of characters (defaults to `TRUE`).
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
#' # Use wildcards and renaming functions
#' nn_register(list(
#'   "score_*" = function(x) paste("Score", sub("score_", "", x))
#' ), dict = "fun")
#' nn(c("score_math", "score_art"), dict = "fun") # "Score math" "Score art"
#'
#' # Use a default renaming function for everything else
#' nn_register(default = function(x) gsub("_", " ", x), dict = "fun")
#' nn("first_name", dict = "fun") # Returns "first name"
#'
#' @export
nn_register <- function(mappings = NULL, dict = "default", default, wildcard = TRUE) {
  d <- .get_nn_dict(dict)

  if (!missing(default)) {
    if (!is.null(default) && !is.function(default)) {
      stop("`default` must be a function or NULL.")
    }
    d$default <- default
  }

  # Check if mappings have names
  if (is.null(names(mappings)) && length(mappings) > 0) {
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
      value <- mappings[[name]]
      if (wildcard && grepl("*", name, fixed = TRUE)) {
        rule <- list(pattern = name, regex = .wildcard_to_regex(name), value = value)
        existing <- vapply(d$patterns, function(r) identical(r$pattern, name), logical(1))
        if (any(existing)) {
          # Update an existing pattern in place, keeping its priority
          d$patterns[[which(existing)[1]]] <- rule
        } else {
          d$patterns[[length(d$patterns) + 1]] <- rule
        }
      } else {
        d$mappings[[name]] <- value
      }
    }
  }

  invisible()
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
  sapply(x, function(item) {
    value <- .nn_lookup(item, dict = dict)
    if (is.null(value)) item else value
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
