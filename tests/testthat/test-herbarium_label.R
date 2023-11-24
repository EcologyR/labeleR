

data <- data.frame("ID"=                  "1",
                   "Collector"=           "Person1",
                   "Collection_number"=   "11.2",
                   "Assistants"=          "Person2, Person3",
                   "Family"=              "Canellae",
                   "Taxon"=               "Trorgssionis sp.1",
                   "Author"=              "L.",
                   "det"=                 "Person1",
                   "Det_date"=            "1/1/2001",
                   "life_form"=           "shrub",
                   "Observations"=        "none",
                   "Height"=              "1m",
                   "Location"=            "AAAAA",
                   "Area_description"=    "AAAAA",
                   "Elevation"=           "314 m.a.s.l.",
                   "Date"=                "2/2/2002",
                   "Latitude"=            "1º2'3''W",
                   "Longitude"=           "1º2'3''N",
                   "QR_code" =            "https://powo.science.kew.org/results?q=Trorgssionis"
                   )
data2 <- data
data2$Collector <- c("Person1&Person2")

path <- tempdir()
filename <- "labeleR_test"

test_that("PDF certificates are created", {

  skip_on_ci()
  skip_on_cran()

  ## Spanish
  create_herbarium_label(data = data, path = path, filename =  filename,
                         title ="example herbarium label" ,
                         subtitle = "is labeleR working correctly?",
                         family.column = "Family",
                         taxon.column = "Taxon",
                         author.column = "Author",
                         det.column = "det",
                         date.det.column = "Det_date",
                         location.column = "Location",
                         area.description.column = "Area_description",
                         latitude.column = "Latitude",
                         longitude.column = "Longitude",
                         elevation.column = "Elevation",
                         field1.column = "life_form",
                         field2.column = "Observations",
                         field3.column = "Height",
                         collector.column = "Collector",
                         collection.column = "Collection_number",
                         assistants.column = "Assistants",
                         date.column = "Date"    )

  expect_true(file.exists(file.path(path, paste0(filename,".pdf"))))

})


test_that("PDF certificates are created with an '&' ", {

  skip_on_ci()
  skip_on_cran()

  ## Spanish
  create_herbarium_label(data = data2, path = path, filename =  filename,
                         title ="example herbarium label" ,
                         subtitle = "is labeleR working correctly?",
                         family.column = "Family",
                         taxon.column = "Taxon",
                         author.column = "Author",
                         det.column = "det",
                         date.det.column = "Det_date",
                         location.column = "Location",
                         area.description.column = "Area_description",
                         latitude.column = "Latitude",
                         longitude.column = "Longitude",
                         elevation.column = "Elevation",
                         field1.column = "life_form",
                         field2.column = "Observations",
                         field3.column = "Height",
                         collector.column = "Collector",
                         collection.column = "Collection_number",
                         assistants.column = "Assistants",
                         date.column = "Date"    )

  expect_true(file.exists(file.path(path, paste0(filename,".pdf"))))

})


data <- data.frame("ID"=                  "1",
                   "Collector"=           "Person1",
                   "Collection_number"=   "11.2",
                   "Assistants"=          "Person2, Person3",
                   "Family"=              "Canellae",
                   "Taxon"=               "Trorgssionis sp.1",
                   "Author"=              "L.",
                   "det"=                 "Person1",
                   "Det_date"=            "1/1/2001",
                   "life_form"=           "shrub",
                   "Observations"=        "Lorem ipsum dolor sit amet, consectetur adipiscing elit,
                   sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam,
                   quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
                   Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur.
                   Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.",
                   "Height"=              "1m",
                   "Location"=            "AAAAA",
                   "Area_description"=    "AAAAA",
                   "Elevation"=           "314 m.a.s.l.",
                   "Date"=                "2/2/2002",
                   "Latitude"=            "1º2'3''W",
                   "Longitude"=           "1º2'3''N",
                   "QR_code" =            "https://powo.science.kew.org/results?q=Trorgssionis"
)

path <- tempdir()
filename <- "labeleR_test"

test_that("PDF certificates are created", {

  skip_on_ci()
  skip_on_cran()

  ## Spanish
  expect_message(
    create_herbarium_label(data = data, path = path, filename =  filename,
                         title ="example herbarium label" ,
                         subtitle = "is labeleR working correctly?",
                         family.column = "Family",
                         taxon.column = "Taxon",
                         author.column = "Author",
                         det.column = "det",
                         date.det.column = "Det_date",
                         location.column = "Location",
                         area.description.column = "Area_description",
                         latitude.column = "Latitude",
                         longitude.column = "Longitude",
                         elevation.column = "Elevation",
                         field1.column = "life_form",
                         field2.column = "Observations",
                         field3.column = "Height",
                         collector.column = "Collector",
                         collection.column = "Collection_number",
                         assistants.column = "Assistants",
                         date.column = "Date",
                         qr = "QR_code"
                           )



  )

})
