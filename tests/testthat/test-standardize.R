test_that("standardized variable has mean ~0", {
  x <- rnorm(100000, mean = 8)
  y <- standardize(x)
  expect_equal(mean(y), 0, tolerance = 1e-6)
})

test_that("standardized variable has sd ~1", {
  x <- rnorm(100000, mean = 8)
  y <- standardize(x)
  expect_equal(sd(y), 1, tolerance = 1e-6)
})

