local_clear_nn_envs <- function(env = parent.frame()) {
  withr::defer({
    rm(list=ls(envir = .nn_envs), envir = .nn_envs)
  }, env)
}

test_that("labs_nn works with x and y", {
  local_clear_nn_envs()

  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point()

  nn_register(c(
    wt = "Weight (1000 lbs)",
    mpg = "Miles/(US) gallon"
  ))

  p <- p +
    labs_nn()

  built_plot <- ggplot2::ggplot_build(p)
  expect_equal(p$labels$x, "Weight (1000 lbs)")
  expect_equal(p$labels$y, "Miles/(US) gallon")
})

test_that("labs_nn supports wildcards, functions and defaults", {
  local_clear_nn_envs()

  nn_register(
    list("w*" = toupper, cyl = "Cylinders"),
    default = function(x) paste("Default:", x)
  )

  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, color = factor(cyl))) +
    ggplot2::geom_point() +
    labs_nn()

  expect_equal(p$labels$x, "WT")
  # The default is applied to the full name
  expect_equal(p$labels$y, "Default: mpg")
  # Extracted variable names still take priority over the default
  expect_equal(p$labels$colour, "Cylinders")
})

test_that("labs_nn uses the requested dictionary", {
  local_clear_nn_envs()

  nn_register(c(wt = "Weight"), dict = "plots")

  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point() +
    labs_nn(dict = "plots")

  expect_equal(p$labels$x, "Weight")
})
