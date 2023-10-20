data <- data.frame(Field1 = c("Bombus", "Amata"),
                   Field2 = c("Apidae", "Erebidae"),
                   Field3 = c("05/09/92", "03/04/95"),
                   Field4 = c("England", "Northern Ireland"),
                   Field5 = c("Wasp-moth hybrid", "Produces fluid")
)

path <- tempdir()

test_that("PDF file is created in the provided path", {

  skip_on_cran()
  skip_on_ci()

  create_collection_label(data, path = path,
                          field1.column = "Field1",
                          field2.column = "Field2",
                          field3.column = "Field3",
                          field4.column = "Field4",
                          field5.column = "Field5")

  expect_true(file.exists(file.path(path, "Collection_label.pdf")))

})
