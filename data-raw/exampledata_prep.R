exampledata <- data.frame(
  x = runif(30),
  y = runif(30)
)

usethis::use_data(exampledata, overwrite = TRUE)
