local_clear_nn_envs <- function(env = parent.frame()) {
  withr::defer({
    rm(list=ls(envir = .nn_envs), envir = .nn_envs)
  }, env)
}

test_that("nn_add_labels works with data.frame column names", {
  local_clear_nn_envs()

  # Create test data frame
  df <- data.frame(
    first_name = c("Alice", "Bob"),
    last_name = c("Smith", "Jones"),
    age = c(30, 25)
  )

  # Register column name mappings
  nn_register(c(
    first_name = "First Name",
    last_name = "Last Name",
    age = "Age (years)"
  ))

  result <- nn_add_labels(df)

  # Check that it's still a data.frame
  expect_s3_class(result, "data.frame")

  # Check that labels have been set
  expect_equal(attr(result$first_name, "label"), "First Name")
  expect_equal(attr(result$last_name, "label"), "Last Name")
  expect_equal(attr(result$age, "label"), "Age (years)")

  # Check that column names are unchanged
  expected_names <- c("first_name", "last_name", "age")
  expect_equal(names(result), expected_names)
})

test_that("nn_add_labels preserves data.frame attributes and row names", {
  local_clear_nn_envs()

  # Create data frame with row names and custom attributes
  df <- data.frame(
    var1 = c("A", "B", "C"),
    var2 = c(1, 2, 3),
    row.names = c("row1", "row2", "row3")
  )
  attr(df, "custom_attr") <- "test_value"
  attr(df, "class") <- c("custom_class", "data.frame")

  # Register mappings
  nn_register(c(var1 = "Variable 1", var2 = "Variable 2"))

  result <- nn_add_labels(df)

  # Check that row names are preserved
  expect_equal(rownames(result), c("row1", "row2", "row3"))

  # Check that custom attributes are preserved
  expect_equal(attr(result, "custom_attr"), "test_value")

  # Check that class is preserved (but may include additional classes)
  expect_true("data.frame" %in% class(result))
})

test_that("nn_add_labels works with an empty data frame", {
  local_clear_nn_envs()

  # Register some mappings
  nn_register(c(a = "Alpha", b = "Beta"))

  # Test with empty data frame
  df_empty <- data.frame()
  result_df <- nn_add_labels(df_empty)
  expect_s3_class(result_df, "data.frame")
  expect_equal(nrow(result_df), 0)
  expect_equal(ncol(result_df), 0)
})

test_that("nn_add_labels with data.frame handles duplicate column names after labeling", {
  local_clear_nn_envs()

  # Create data frame
  df <- data.frame(
    col1 = c(1, 2, 3),
    col2 = c(4, 5, 6),
    col3 = c(7, 8, 9)
  )

  # Register mappings that create duplicate labels
  nn_register(c(
    col1 = "Same Label",
    col2 = "Same Label",
    col3 = "Unique Label"
  ))

  result <- nn_add_labels(df)

  # Check that labels are applied even if duplicated
  expect_equal(attr(result$col1, "label"), "Same Label")
  expect_equal(attr(result$col2, "label"), "Same Label")
  expect_equal(attr(result$col3, "label"), "Unique Label")

  # Check that data is preserved
  col1 <- c(1, 2, 3)
  col2 <- c(4, 5, 6)
  col3 <- c(7, 8, 9)
  attr(col1, "label") <- "Same Label"
  attr(col2, "label") <- "Same Label"
  attr(col3, "label") <- "Unique Label"
  expect_equal(result[[1]], col1)
  expect_equal(result[[2]], col2)
  expect_equal(result[[3]], col3)
})
