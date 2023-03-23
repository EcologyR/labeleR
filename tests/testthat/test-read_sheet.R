
test_that("Check if URL is wrong", {
  expect_error(read_sheet(url='https://docs.google.com/spreadsvUNzFk/edit#gid=0', "Certificado", "no"))
})

test_that("Check if selection values are OK", {
expect_error(read_sheet(url='https://docs.google.com/spreadsheets/d/1uwhi7IROqDcdMEEld7yuyyAoSfuuJUbK2HVlHvUNzFk/edit#gid=0', "Certificado", "1"))
})
