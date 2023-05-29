
data <- data.frame(Names = c("Pippin", "Merry"))

path <- tempdir()

test_that("PDF certificates are created", {

  skip_on_ci()
  skip_on_cran()

  ## Spanish
  create_certificate_attendance(data = data, path = path, type = "Adventure",
                                title = "Going to Mordor",
                                hours = "1000", name.column = "Names",
                                language = "Spanish", signer = "Gandalf",
                                date = "10/07/3064"
  )

  expect_true(file.exists(file.path(path, "Asistencia_Pippin.pdf")))
  expect_true(file.exists(file.path(path, "Asistencia_Merry.pdf")))


  ## English
  create_certificate_attendance(data = data, path = path, type = "Adventure",
                                title = "Going to Mordor",
                                hours = "1000", name.column = "Names",
                                language = "English", signer = "Gandalf",
                                date = "10/07/3064"
  )

  expect_true(file.exists(file.path(path, "Attendance_Pippin.pdf")))
  expect_true(file.exists(file.path(path, "Attendance_Merry.pdf")))


})


test_that("Rmd file is present when keep.files = TRUE", {

  skip_on_ci()
  skip_on_cran()

  create_certificate_attendance(data = data, path = path, type = "Adventure",
                                title = "Going to Mordor",
                                organiser = "The Fellowship of The Ring",
                                hours = "1000", name.column = "Names",
                                language = "English", signer = "Gandalf",
                                speaker = "Sauron", date = "10/07/3064",
                                keep.files = TRUE
  )

  expect_true(file.exists(file.path(path, "attendance.Rmd")))

})

