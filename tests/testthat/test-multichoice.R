
data <- data.frame(question = c("why?", "where?", "when?", "who?"),
                   opt1.ok = c("A", "B", "C","D"),
                   opt2 = c("1", NA, "3", "4"))




path <- tempdir()

file.copy(
  system.file("rmarkdown/pictures/Hogwarts_logo.png", package = "labeleR"),
  to = paste0(path, "/image.png")
    )

data_image <- data
data_image$image <- paste0(path, "/image.png")
test_that("PDF file is not created if no seeds are provided", {

  skip_on_cran()
  skip_on_ci()



  expect_error(
    create_multichoice(data,
                       path = path,
                       question.column = "question",
                       option1.column = "opt1.ok",
                       option2.column = "opt2"
    )

    ,regexp = "Please set at least one seed to start the randomization process.")

})



test_that("PDF file is created in the provided path", {

  skip_on_cran()
  skip_on_ci()

  create_multichoice(data,
                     path = path,
                     question.column = "question",
                     option1.column = "opt1.ok",
                     option2.column = "opt2",
                     seeds = 1

                     )

  expect_true(file.exists(file.path(path, "Exam.pdf")))

})

test_that("PDF file is created in the provided path when using image", {

  skip_on_cran()
  skip_on_ci()

  create_multichoice(data_image,
                     path = path,
                     question.column = "question",
                     option1.column = "opt1.ok",
                     option2.column = "opt2",
                     seeds = 1,
                     image.column = "image"
   )



  expect_true(file.exists(file.path(path, "Exam.pdf")))

})






