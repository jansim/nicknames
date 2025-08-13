local_clear_nn_envs <- function(env = parent.frame()) {
  withr::defer({
    rm(list=ls(envir = .nn_envs), envir = .nn_envs)
  }, env)
}

test_that("nn and nn_register works", {
  local_clear_nn_envs()

  nn_register(c(
    a = "b",
    A = "B"
  ))

  expect_equal(nn("a"), "b")
  expect_equal(nn("A"), "B")
  expect_equal(nn("c"), "c")

  nn_register(c(
    c = "d"
  ))

  # c gets updated, everything else stays the same
  expect_equal(nn("a"), "b")
  expect_equal(nn("A"), "B")
  expect_equal(nn("c"), "d")
  expect_equal(nn("d"), "d")
})


test_that("local_clear_nn_envs cleans up between tests", {
  local_clear_nn_envs()

  expect_equal(nn("a"), "a")
  expect_equal(nn("A"), "A")
})

test_that("nn works with vectors", {
  local_clear_nn_envs()

  nn_register(c(
    a = "b",
    A = "B"
  ))

  expect_equal(nn(c("a", "A")), c("b", "B"))

  nn_register(c(
    c = "d"
  ))

  expect_equal(nn(c("a", "A", "c", "d")), c("b", "B", "d", "d"))
})

test_that("multiple dictionaries work independently", {
  local_clear_nn_envs()

  # Register mappings in different dictionaries
  nn_register(c(name1 = "nick1", name2 = "nick2"), dict = "dict1")
  nn_register(c(name1 = "different1", name3 = "nick3"), dict = "dict2")

  # Check lookups in different dictionaries
  expect_equal(nn("name1", dict = "dict1"), "nick1")
  expect_equal(nn("name1", dict = "dict2"), "different1")
  expect_equal(nn("name2", dict = "dict1"), "nick2")
  expect_equal(nn("name2", dict = "dict2"), "name2")  # no mapping in dict2
  expect_equal(nn("name3", dict = "dict1"), "name3")  # no mapping in dict1
  expect_equal(nn("name3", dict = "dict2"), "nick3")
})

test_that("default dictionary works", {
  local_clear_nn_envs()

  nn_register(c(default_name = "default_nick"))

  # Should work with explicit default dict
  expect_equal(nn("default_name", dict = "default"), "default_nick")
  # Should work without specifying dict (uses default)
  expect_equal(nn("default_name"), "default_nick")
})

test_that("edge cases work correctly", {
  local_clear_nn_envs()

  # Empty mappings
  nn_register(c())
  expect_equal(nn("anything"), "anything")

  # Unnamed vector (should be ignored)
  expect_warning(nn_register(c("a", "b")))
  expect_equal(nn("a"), "a")  # no mapping created

  # Mixed named/unnamed (only named should be registered)
  expect_warning(nn_register(c(named = "value", "unnamed")))
  expect_equal(nn("named"), "value")
  expect_equal(nn("unnamed"), "unnamed")  # no mapping
})

test_that("numeric and other types work as names and values", {
  local_clear_nn_envs()

  nn_register(c("1" = "one", "2" = "two"))
  expect_equal(nn("1"), "one")
  expect_equal(nn("2"), "two")
  expect_equal(nn("3"), "3")  # no mapping
})

test_that("overwriting mappings works", {
  local_clear_nn_envs()

  nn_register(c(name = "original"), dict = "overwrite_test")
  expect_equal(nn("name", dict = "overwrite_test"), "original")

  # Overwrite with new value
  nn_register(c(name = "updated"), dict = "overwrite_test")
  expect_equal(nn("name", dict = "overwrite_test"), "updated")
})

test_that("special characters in names and values work", {
  local_clear_nn_envs()

  nn_register(c(
    "name with spaces" = "nickname with spaces",
    "name.with.dots" = "nick.with.dots",
    "name_with_underscores" = "nick_with_underscores"
  ), dict = "special_chars")

  expect_equal(nn("name with spaces", dict = "special_chars"), "nickname with spaces")
  expect_equal(nn("name.with.dots", dict = "special_chars"), "nick.with.dots")
  expect_equal(nn("name_with_underscores", dict = "special_chars"), "nick_with_underscores")
})

test_that("nn works with data.frame column names", {
  local_clear_nn_envs()

  # Create a test data frame
  df <- data.frame(
    first_name = c("Alice", "Bob"),
    last_name = c("Smith", "Jones"),
    age = c(30, 25)
  )

  # Register column name mappings
  nn_register(c(
    first_name = "fname",
    last_name = "lname"
  ))

  # Apply nn to the data frame
  result_df <- nn(df)

  # Check that column names are renamed
  expect_equal(colnames(result_df), c("fname", "lname", "age"))
  # Check that data is preserved
  expect_equal(result_df$fname, c("Alice", "Bob"))
  expect_equal(result_df$lname, c("Smith", "Jones"))
  expect_equal(result_df$age, c(30, 25))
})

test_that("nn with data.frame works with different dictionaries", {
  local_clear_nn_envs()

  # Create a test data frame
  df <- data.frame(
    name = c("John", "Jane"),
    value = c(10, 20)
  )

  # Register mappings in different dictionaries
  nn_register(c(name = "full_name", value = "amount"), dict = "verbose")
  nn_register(c(name = "n", value = "v"), dict = "short")

  # Apply nn with different dictionaries
  verbose_df <- nn(df, dict = "verbose")
  short_df <- nn(df, dict = "short")

  # Check verbose dictionary results
  expect_equal(colnames(verbose_df), c("full_name", "amount"))
  expect_equal(verbose_df$full_name, c("John", "Jane"))

  # Check short dictionary results
  expect_equal(colnames(short_df), c("n", "v"))
  expect_equal(short_df$n, c("John", "Jane"))
})

test_that("nn with data.frame handles unmapped columns correctly", {
  local_clear_nn_envs()

  # Create a test data frame with more columns than mappings
  df <- data.frame(
    col1 = c(1, 2),
    col2 = c(3, 4),
    col3 = c(5, 6),
    col4 = c(7, 8)
  )

  # Register mappings for only some columns
  nn_register(c(
    col1 = "first",
    col3 = "third"
  ))

  # Apply nn to the data frame
  result_df <- nn(df)

  # Check that mapped columns are renamed and unmapped columns stay the same
  expect_equal(colnames(result_df), c("first", "col2", "third", "col4"))
  # Check that all data is preserved
  expect_equal(result_df$first, c(1, 2))
  expect_equal(result_df$col2, c(3, 4))
  expect_equal(result_df$third, c(5, 6))
  expect_equal(result_df$col4, c(7, 8))
})

test_that("nn with empty data.frame works correctly", {
  local_clear_nn_envs()

  # Create empty data frame
  df <- data.frame()

  # Register some mappings (shouldn't affect empty df)
  nn_register(c(col1 = "first", col2 = "second"))

  # Apply nn to empty data frame
  result_df <- nn(df)

  # Should still be empty
  expect_equal(nrow(result_df), 0)
  expect_equal(ncol(result_df), 0)
})

test_that("nn with data.frame preserves row names and attributes", {
  local_clear_nn_envs()

  # Create data frame with row names and custom attributes
  df <- data.frame(
    old_name = c("A", "B", "C"),
    other_col = c(1, 2, 3),
    row.names = c("row1", "row2", "row3")
  )
  attr(df, "custom_attr") <- "test_value"

  # Register mapping
  nn_register(c(old_name = "new_name"))

  # Apply nn
  result_df <- nn(df)

  # Check that column names are updated
  expect_equal(colnames(result_df), c("new_name", "other_col"))

  # Check that row names are preserved
  expect_equal(rownames(result_df), c("row1", "row2", "row3"))

  # Check that data is preserved
  expect_equal(result_df$new_name, c("A", "B", "C"))
  expect_equal(result_df$other_col, c(1, 2, 3))

  # Check that custom attributes are preserved
  expect_equal(attr(result_df, "custom_attr"), "test_value")
})

test_that("nn with data.frame handles duplicate column names after renaming", {
  local_clear_nn_envs()

  # Create data frame
  df <- data.frame(
    col1 = c(1, 2, 3),
    col2 = c(4, 5, 6),
    col3 = c(7, 8, 9)
  )

  # Register mappings that will create duplicate column names
  nn_register(c(
    col1 = "renamed",
    col2 = "renamed",  # duplicate
    col3 = "unique"
  ))

  # Apply nn
  result_df <- nn(df)
  expect_equal(colnames(result_df), c("renamed", "renamed", "unique"))

  # Check that data is still preserved
  expect_equal(result_df[[1]], c(1, 2, 3))  # First column data
  expect_equal(result_df[[2]], c(4, 5, 6))  # Second column data
  expect_equal(result_df[[3]], c(7, 8, 9))  # Third column data
})
