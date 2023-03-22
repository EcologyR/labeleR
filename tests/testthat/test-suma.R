
test_that("suma returns the right result", {
  expect_equal(suma(2, 3), 5)
  expect_equal(suma(c(2, 2), c(3, 4)), c(5, 6))
})


