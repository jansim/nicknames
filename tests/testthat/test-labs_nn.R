test_that("labs_nn works with x and y", {
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
