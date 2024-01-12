

data <- data.frame(Names = c("Pippin", "Merry"),
                   Affiliation = c("A", "B"),
                   CommType = c("oral communication", "poster"),
                   Title = c("Title 1", "Title 2"),
                   Date = c("02/06/2016", "2016-04-03")
                   )

path <- tempdir()

test_that("PDF certificates are created", {

  skip_on_ci()
  skip_on_cran()

  ## Spanish
  create_participation_certificate(data = data,
                                   path = path,
                                   language = "Spanish",
                                   name.column = "Names",
                                   affiliation.column = "Affiliation",
                                   comm.type.column = "CommType",
                                   title.column = "Title",
                                   date.column = "Date",
                                   type = "Adventure",
                                   event = "Going to Mordor",
                                   signer = "Gandalf",
                                   signer.role = "Head"
  )

  expect_true(file.exists(file.path(path, "Participacion_Pippin_02-06-2016.pdf")))
  expect_true(file.exists(file.path(path, "Participacion_Merry_2016-04-03.pdf")))


  ## English
  create_participation_certificate(data = data,
                                   path = path,
                                   language = "English",
                                   name.column = "Names",
                                   affiliation.column = "Affiliation",
                                   comm.type.column = "CommType",
                                   title.column = "Title",
                                   date.column = "Date",
                                   type = "Adventure",
                                   event = "Going to Mordor",
                                   signer = "Gandalf",
                                   signer.role = "Head"
  )

  expect_true(file.exists(file.path(path, "Participation_Pippin_02-06-2016.pdf")))
  expect_true(file.exists(file.path(path, "Participation_Merry_2016-04-03.pdf")))


})


test_that("Rmd file is present when keep.files = TRUE", {

  skip_on_ci()
  skip_on_cran()

  create_participation_certificate(data = data,
                                   path = path,
                                   language = "English",
                                   name.column = "Names",
                                   affiliation.column = "Affiliation",
                                   comm.type.column = "CommType",
                                   title.column = "Title",
                                   date.column = "Date",
                                   type = "Adventure",
                                   event = "Going to Mordor",
                                   signer = "Gandalf",
                                   signer.role = "Head",
                                   keep.files = TRUE
  )

  expect_true(file.exists(file.path(path, "participation.Rmd")))

})


