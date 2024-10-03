

data <- data.frame(
  "title" = c("Abstract book title 1", "Abstract book title 1"),
  "aut"   = c("First Author (1,2,3); Second Author (2,3)", "First Author2 (1,2,*); Last Author (2,3)"),
  "affil" = c("UNIV1; Univ2; Department 3 ", "Dpt1 ; Dpt 2 ; Univ 3; correpondicng author"),
  "text"  = c("abcdefghijkl mnopqrstuvwxyz abcdefghijklmnopqr stuvwxyzab cdefg hij
              kl m no pqrstuvwxyzab cdefghijklmnop qrstuvwxyza
             bcdefghijklmn opqrstuvwxyz",
             "1234567891 011121314151 6171819202122232425
             26272829303 1323334353637 3839404 14243444 54647
             48495051525354555657585960616263646566676869707172
             737475767778 798081828 3848586878 88990919293949596979
             899100101102103104105106107108109110111112113114115116117
             118119120 1211221231241251261271 281291301311321331341
             351361371381391 4014114214 3144145146147148149150"))



path <- tempdir()
filename <- "labeleR_test"

test_that("Abstract book is created", {

  skip_on_ci()
  skip_on_cran()

  ## Spanish
  create_abstractbook(data = data,
                      path = path,
                      filename =  filename,
                      title.column = "title",
                      authors.column = "aut",
                      affiliation.column = "affil",
                      text.column = "text"
                         )

  expect_true(file.exists(file.path(path, paste0(filename,".pdf"))))

})


test_that("book created with an '&' ", {

  skip_on_ci()
  skip_on_cran()

  data2 <- data
  data2[1, "text"] <- "abcdefgh&ijkl mn&opqrstuv&wxyz abcdefghijklmnopqr"
  ## Spanish
  create_abstractbook(data = data2,
                      path = path,
                      filename =  filename,
                      title.column = "title",
                      authors.column = "aut",
                      affiliation.column = "affil",
                      text.column = "text"
  )

  expect_true(file.exists(file.path(path, paste0(filename,".pdf"))))

})


test_that("Fails if missing brackets ", {

  skip_on_ci()
  skip_on_cran()

  data3 <- data
  data3[1, "aut"] <- "First Author 1,2,3); Second Author (2,3)"

expect_error(

  create_abstractbook(data = data3,
                      path = path,
                      filename =  filename,
                      title.column = "title",
                      authors.column = "aut",
                      affiliation.column = "affil",
                      text.column = "text"
  )
  ,regexp = "Unmatched closing bracket"

)

data4 <- data
data4[1, "aut"] <- "First Author (1,2,3); Second Author (2,3"

expect_error(

  create_abstractbook(data = data4,
                      path = path,
                      filename =  filename,
                      title.column = "title",
                      authors.column = "aut",
                      affiliation.column = "affil",
                      text.column = "text"
  )
  ,regexp = "Unmatched opening bracket"

)

})

test_that("Fails if number of affiliations != ton affiliation numbers", {

  skip_on_ci()
  skip_on_cran()

  data5 <- data
  data5[1, "aut"] <- "First Author (1,2,3,4,5); Second Author (2,3)"

  expect_error(

    create_abstractbook(data = data5,
                        path = path,
                        filename =  filename,
                        title.column = "title",
                        authors.column = "aut",
                        affiliation.column = "affil",
                        text.column = "text"
    )
    ,regexp = "There must be the same amount of affiliations as affiliation names"

  )


})
