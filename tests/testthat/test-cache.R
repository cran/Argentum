test_that("cache keys are stable and value dependent", {
  expect_equal(arg_cache_key("a", "b"), arg_cache_key("a", "b"))
  expect_false(identical(arg_cache_key("a", "b"), arg_cache_key("a", "c")))
  expect_match(arg_cache_key("a"), "^[a-f0-9]{16}\\.rds$")
})

test_that("a cached value is computed once and reused", {
  local_temp_cache()
  calls <- 0L
  compute <- function() {
    calls <<- calls + 1L
    "value"
  }

  expect_equal(arg_cached("t.rds", compute), "value")
  expect_equal(arg_cached("t.rds", compute), "value")
  expect_equal(calls, 1L)
})

test_that("refresh bypasses the cache", {
  local_temp_cache()
  calls <- 0L
  compute <- function() {
    calls <<- calls + 1L
    calls
  }

  arg_cached("t.rds", compute)
  arg_cached("t.rds", compute, refresh = TRUE)
  expect_equal(calls, 2L)
})

test_that("a stale entry is recomputed", {
  local_temp_cache()
  calls <- 0L
  compute <- function() {
    calls <<- calls + 1L
    calls
  }

  arg_cached("t.rds", compute)
  argentum_cache_clear("memory")            # force the disk path
  arg_cached("t.rds", compute, ttl = -1)    # everything on disk is stale
  expect_equal(calls, 2L)
})
