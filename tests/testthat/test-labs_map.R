test_that("labs_map works with x and y", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point()

  p <- p +
    labs_map(c(
      wt = "Weight (1000 lbs)",
      mpg = "Miles/(US) gallon"
    ))

  built_plot <- ggplot2::ggplot_build(p)
  expect_equal(p$labels$x, "Weight (1000 lbs)")
  expect_equal(p$labels$y, "Miles/(US) gallon")
})

test_that("labs_map works with nothing matching", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point()

  p <- p +
    labs_map(c(
      not_wt = "Weight (1000 lbs)",
      not_mpg = "Miles/(US) gallon"
    ))

  built_plot <- ggplot2::ggplot_build(p)
  expect_equal(p$labels$x, "wt")
  expect_equal(p$labels$y, "mpg")
})


test_that("labs_map works with empty names", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point()

  p <- p +
    labs_map(c())

  built_plot <- ggplot2::ggplot_build(p)
  expect_equal(p$labels$x, "wt")
  expect_equal(p$labels$y, "mpg")
})

test_that("labs_map works with color and size aesthetics", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, color = cyl, size = hp)) +
    ggplot2::geom_point()

  p <- p +
    labs_map(c(
      wt = "Weight (1000 lbs)",
      mpg = "Miles/(US) gallon",
      cyl = "Number of cylinders",
      hp = "Horsepower"
    ))

  built_plot <- ggplot2::ggplot_build(p)
  expect_equal(p$labels$x, "Weight (1000 lbs)")
  expect_equal(p$labels$y, "Miles/(US) gallon")
  expect_equal(p$labels$colour, "Number of cylinders")
  expect_equal(p$labels$size, "Horsepower")
})

test_that("labs_map works with fill and alpha aesthetics", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, fill = factor(vs), alpha = qsec)) +
    ggplot2::geom_point(shape = 21, size = 3)

  p <- p +
    labs_map(c(
      wt = "Weight (1000 lbs)",
      mpg = "Miles per gallon",
      "factor(vs)" = "Engine shape",
      qsec = "Quarter mile time"
    ))

  built_plot <- ggplot2::ggplot_build(p)
  expect_equal(p$labels$x, "Weight (1000 lbs)")
  expect_equal(p$labels$y, "Miles per gallon")
  expect_equal(p$labels$fill, "Engine shape")
  expect_equal(p$labels$alpha, "Quarter mile time")
})

test_that("labs_map works with shape and linetype aesthetics", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, shape = factor(gear), linetype = factor(am))) +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_line()

  p <- p +
    labs_map(c(
      wt = "Weight (1000 lbs)",
      mpg = "Miles per gallon",
      "factor(gear)" = "Number of gears",
      "factor(am)" = "Transmission type"
    ))

  built_plot <- ggplot2::ggplot_build(p)
  expect_equal(p$labels$x, "Weight (1000 lbs)")
  expect_equal(p$labels$y, "Miles per gallon")
  expect_equal(p$labels$shape, "Number of gears")
  expect_equal(p$labels$linetype, "Transmission type")
})

test_that("labs_map works with stroke and group aesthetics", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, stroke = disp, group = factor(carb))) +
    ggplot2::geom_point(shape = 21, size = 3) +
    ggplot2::geom_line()

  p <- p +
    labs_map(c(
      wt = "Weight (1000 lbs)",
      mpg = "Miles per gallon",
      disp = "Engine displacement",
      "factor(carb)" = "Number of carburetors"
    ))

  built_plot <- ggplot2::ggplot_build(p)
  expect_equal(p$labels$x, "Weight (1000 lbs)")
  expect_equal(p$labels$y, "Miles per gallon")
  expect_equal(p$labels$stroke, "Engine displacement")
  expect_equal(p$labels$group, "Number of carburetors")
})

test_that("labs_map works with multiple aesthetics including facets", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, color = hp, size = drat)) +
    ggplot2::geom_point() +
    ggplot2::facet_grid(vs ~ am)

  p <- p +
    labs_map(c(
      wt = "Weight (1000 lbs)",
      mpg = "Miles per gallon",
      hp = "Gross horsepower",
      drat = "Rear axle ratio",
      vs = "Engine (0 = V-shaped, 1 = straight)",
      am = "Transmission (0 = automatic, 1 = manual)"
    ))

  built_plot <- ggplot2::ggplot_build(p)
  expect_equal(p$labels$x, "Weight (1000 lbs)")
  expect_equal(p$labels$y, "Miles per gallon")
  expect_equal(p$labels$colour, "Gross horsepower")
  expect_equal(p$labels$size, "Rear axle ratio")
})

test_that("labs_map works with partial mapping and preserves unmapped aesthetics", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, color = cyl, size = hp, shape = factor(gear))) +
    ggplot2::geom_point()

  p <- p +
    labs_map(c(
      wt = "Weight (1000 lbs)",
      mpg = "Miles per gallon",
      cyl = "Number of cylinders"
      # hp and gear are intentionally not mapped
    ))

  built_plot <- ggplot2::ggplot_build(p)
  expect_equal(p$labels$x, "Weight (1000 lbs)")
  expect_equal(p$labels$y, "Miles per gallon")
  expect_equal(p$labels$colour, "Number of cylinders")
  # These should remain as original variable names
  expect_equal(p$labels$size, "hp")
  expect_equal(p$labels$shape, "factor(gear)")
})

test_that("labs_map works with a list", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point()

  p <- p +
    labs_map(list(
      wt = "Weight (1000 lbs)",
      mpg = "Miles/(US) gallon"
    ))

  built_plot <- ggplot2::ggplot_build(p)
  expect_equal(p$labels$x, "Weight (1000 lbs)")
  expect_equal(p$labels$y, "Miles/(US) gallon")
})

test_that("labs_map works with an environment (for internal use only)", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point()

  env <- new.env(parent = emptyenv())
  env$wt <- "Weight (1000 lbs)"
  env$mpg <- "Miles/(US) gallon"

  p <- p +
    labs_map(env)

  built_plot <- ggplot2::ggplot_build(p)
  expect_equal(p$labels$x, "Weight (1000 lbs)")
  expect_equal(p$labels$y, "Miles/(US) gallon")
})

test_that("labs_map strips function calls", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, fill = factor(vs))) +
    ggplot2::geom_point(shape = 21, size = 3)

  # Even loose values are matched
  p <- p +
    labs_map(c(
      wt = "Weight (1000 lbs)",
      mpg = "Miles per gallon",
      vs = "Engine shape"
    ))

  expect_equal(p$labels$x, "Weight (1000 lbs)")
  expect_equal(p$labels$y, "Miles per gallon")
  expect_equal(p$labels$fill, "Engine shape")

  # An exact match with the function call takes priority
  p2 <- p +
    labs_map(c(
      wt = "Weight (1000 lbs)",
      mpg = "Miles per gallon",
      vs = "Engine shape",
      "factor(vs)" = "Engine shape (factorized)"
    ))

  expect_equal(p2$labels$x, "Weight (1000 lbs)")
  expect_equal(p2$labels$y, "Miles per gallon")
  expect_equal(p2$labels$fill, "Engine shape (factorized)")
})

test_that("labs_map works with composite columns", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt+mpg, mpg)) +
    ggplot2::geom_point()

  p <- p +
    labs_map(c(
      wt = "Weight (1000 lbs)",
      mpg = "Miles/(US) gallon"
    ))

  built_plot <- ggplot2::ggplot_build(p)
  expect_equal(p$labels$x, "wt + mpg")
  expect_equal(p$labels$y, "Miles/(US) gallon")
})
