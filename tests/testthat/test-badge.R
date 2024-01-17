
data <- data.frame(Names = c("Pippin", "Frodo"),
                   Affil = c("Home", NA))
path <- tempdir()

test_that("PDF file is created in the provided path", {

  skip_on_cran()
  skip_on_ci()

  create_badge(data, path = path, name.column = "Names", affiliation.column = "Affil")
  expect_true(file.exists(file.path(path, "Badges.pdf")))

})






