
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
      if (!is.null(aes_mapping[[aes_name]]) && rlang::as_name(aes_mapping[[aes_name]]) == var_name) {
        labs_args[[aes_name]] <- object$names[[var_name]]
      }
    }
  }

  # Apply the labels to the plot
  plot + do.call(ggplot2::labs, labs_args)
}
