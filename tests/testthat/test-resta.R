test_that("resta returns correct results", {
  expect_equal(resta(2, 3), -1)
  expect_equal(resta(c(2, 2), c(3, 3)), c(-1, -1))
})

test_that("non-numeric arguments give error", {
  expect_error(resta("p", 2))
})


test_that("numeric arguments of different length give error", {
  expect_error(resta(c(3, 5), 2))
})


test_that("negative results give warning when warn.negative is TRUE", {
  expect_warning(resta(2, 5, warn.negative = TRUE))
})
