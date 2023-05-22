# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/tests.html
# * https://testthat.r-lib.org/reference/test_package.html#special-files

library(testthat)
library(labeleR)

test_check("labeleR")


# read_sheet.R TESTS
testthat::expect_s3_class(
  object = labeleR::read_sheet(url="1Q005BDM0XyUNq5XzGWuvdzgZVMc4KhbYadVzi77h3Xw"),
  "data.frame"
)
#
# testthat::expect_message(
#   labeleR::read_sheet(url="1Q005BDM0XyUNq5XzGWuvdzgZVMc4KhbYadVzi77h3Xw")
# )





# participation TESTS
data <- data.frame("Names"=c("Pippin", "Merry", "Frodo", "Samwise"),
                   "Famnames"=c("Took", "Brandybuck", "Baggins", "Gamgee"),
                   "date"="10/07/3064",
                   "type"="Adventure",
                   "title"= paste0("From the Shire to Mordor, Chapter ",1:4))


path="output"
if(file.exists(path)){  unlink(path, recursive = T, force = T)}

create_certificate_participation(data=data, path="output", "s", type = "Adventure",
                                   organiser = "Rivendel Elf Association", hours = 2, signer = "Gandalf", name.column = "Names",
                                   affiliation.column = "Famnames", comm.type.column = "type", date.column ="date", title.column = "title"     )


  testthat::expect_equal(length(list.files(path=path, pattern = ".pdf")),nrow(data))

  # participation TESTS
  data=read_sheet("1Q005BDM0XyUNq5XzGWuvdzgZVMc4KhbYadVzi77h3Xw")

  path="LabeleR_output"
  if(file.exists(path)){  unlink(path, recursive = T, force = T)}

  create_herbarium_label(
    data=data,
    path = path,
    title="Magical flora of the British Isles",
    subtitle="Project: Eliminating plant blindness in Hogwarts students",
    qr = "QR_code",
    family.column="Family",
    taxon.column="Taxon",
    author.column="Author",
    det.column="det",
    date.det.column="Det_date",
    location.column="Location",
    area.description.column="Area_description",
    latitude.column="Latitude",
    longitude.column="Longitude",
    elevation.column="Elevation",
    field1.column="life_form",
    field2.column="Observations",
    field3.column="Height",
    collector.column="Collector",
    collection.column="Collection_number",
    assistants.column="Assistants",
    date.column="Date"
    )
  testthat::expect_equal(length(list.files(path=path, pattern = ".pdf")),1)

  # collection label TESTS
  data <- data.frame(
        "Family" =paste0("Family" ,1:5),
        "species"=paste0("Species",1:5),
        "QR_code"=paste0("12345",  1:5),
        "f1"     =paste0("field1-",1:5),
        "f2"     =paste0("field2-",1:5),
        "f3"     =paste0("field3-",1:5))

  path="LabeleR_output"
  if(file.exists(path)){  unlink(path, recursive = T, force = T)}
  create_collection_label(data = data, path = path, qr = "QR_code",
                          field1.column = "f1", field3.column = "f2",
                          field5.column = "f3",
                          logo = system.file("rmarkdown/pictures/Hogwarts_BnW.png", package = "labeleR"))

  testthat::expect_equal(length(list.files(path=path, pattern = ".pdf")),1)

  # tiny label TESTS
  data <- data.frame(
    "Family" =paste0("Family" ,1:36),
    "species"=paste0("Species",1:36),
    "QR_code"=paste0("12345",  1:36),
    "f1"     =paste0("field1-",1:36),
    "f2"     =paste0("field2-",1:36),
    "f3"     =paste0("field3-",1:36),
    "f4"     =paste0("field4-",1:36),
    "f5"     =paste0("field5-",1:36))

  path="LabeleR_output"
  if(file.exists(path)){  unlink(path, recursive = T, force = T)}
  create_tinylabel(data = data, path = path, qr = "QR_code",
                          field1.column = "f1", field3.column = "f3",
                          field5.column = "f5", field2.column = "f2",
                          field4.column = "f4")

  testthat::expect_equal(length(list.files(path=path, pattern = ".pdf")),1)
