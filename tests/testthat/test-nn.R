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
