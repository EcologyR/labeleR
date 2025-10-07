#' Create herbarium labels
#'
#' Create herbarium labels (4 labels per DIN-A4 page)
#'
#' @param data a data frame. Each row contains the information by species that will appear in the label.
#' @param path Character. Path to folder where the PDF file will be saved.
#' @param filename Character. Filename of the pdf. If NULL, default is "Herbarium".
#' @param title Main title at the top of the labels. Can be blank if set to NULL.
#' @param subtitle Subtitle at the bottom of the labels. Can be blank if set to NULL.
#' @param qr String. Free text or column of `data` that specifies the link for the QR code.
#'          If the specified value of `qr` is not a column name of `data`,
#'          all the QRs will be equal, pointing to the same link.
#' @param family.column Character (optional). Name of the column in `data` storing the family of the taxon.
#' @param taxon.column Character (optional). Name of the column in `data` storing the taxon name.
#' @param author.column Character (optional). Name of the column in `data` storing the taxon author.
#' @param det.column Character (optional). Name of the column in `data` storing the determiner of the voucher.
#' @param date.det.column Character (optional). Name of the column in `data` storing the date when the voucher
#' was determined.
#' @param location.column Character (optional). Name of the column in `data` storing where the voucher
#' was collected.
#' @param area.description.column Character (optional). Name of the column in `data` storing the description
#' of the area.
#' @param latitude.column Character (optional). Name of the column in `data` storing the latitude
#' where the specimen was collected.
#' @param longitude.column Character (optional). Name of the column in `data` storing the longitude
#' where the specimen was collected.
#' @param elevation.column Character (optional). Name of the column in `data` storing the elevation
#' where the specimen was collected.
#' @param field1.column Character (optional). Name of the column in `data` storing the first free text to
#' appear at the top of the label.
#' @param field2.column Character (optional). Name of the column in `data` storing the second free text to
#' appear below field1.
#' @param field3.column Character (optional). Name of the column in `data` storing the second free text to
#' appear below field2.
#' @param collector.column Character (optional). Name of the column in `data` storing the name of the collector.
#' @param collection.column Character (optional). Name of the column in `data` storing the voucher's collection
#' number.
#' @param assistants.column Character (optional). Name of the column in `data` storing the names of the
#' collector's assistants.
#' @param date.column Character (optional). Name of the column in `data` storing the date when the specimen
#' was collected.
#' @inheritParams create_badge
#'
#' @inherit create_badge details
#'
#' @return A pdf file with four herbarium labels per page within an 'output' folder.
#'
#' @export
#'
#' @author Ignacio Ramos-Gutierrez, Julia G. de Aledo, Francisco Rodriguez-Sanchez
#'
#' @examplesIf interactive()
#'
#' create_herbarium_label (
#'   data = herbarium.table,
#'   path = "labeleR_output",
#'   title = "Magical flora of the British Isles",
#'   subtitle = "Project: Eliminating plant blindness in Hogwarts students",
#'   qr = "QR_code",
#'   family.column ="Family",
#'   taxon.column = "Taxon",
#'   author.column = "Author",
#'   det.column = "det",
#'   date.det.column = "Det_date",
#'   location.column = "Location",
#'   latitude.column = "Latitude",
#'   longitude.column = "Longitude",
#'   elevation.column = "Elevation",
#'   field1.column = "life_form",
#'   field3.column = "Height",
#'   collector.column = "Collector",
#'   collection.column = "Collection_number",
#'   assistants.column = "Assistants",
#'   date.column = "Date",
#'   font = "libertinus"
#' )

create_herbarium_label <- function(data = data,
                                   path = NULL,
                                   filename = NULL,
                                   title = NULL,
                                   subtitle = NULL,
                                   qr = NULL,
                                   family.column = NULL,
                                   taxon.column = NULL,
                                   author.column = NULL,
                                   det.column = NULL,
                                   date.det.column = NULL,
                                   location.column = NULL,
                                   area.description.column = NULL,
                                   latitude.column = NULL,
                                   longitude.column = NULL,
                                   elevation.column = NULL,
                                   field1.column = NULL,
                                   field2.column = NULL,
                                   field3.column = NULL,
                                   collector.column = NULL,
                                   collection.column = NULL,
                                   assistants.column = NULL,
                                   date.column = NULL,
                                   font = NULL,
                                   keep.files = FALSE,
                                   template = NULL
){

  ## Check arguments

  if (is.null(data)) {
    stop("Please provide a data.frame or tibble.")
  }
  if ((!(all(class(data) == "data.frame"))) & any(class(data) == "data.frame")) {data <- as.data.frame(data)}
  if (!inherits(data, "data.frame")) {stop("The 'data' object must be a data frame.")}
  data <- fill_NAs_df(data)

  if (is.null(path)) {stop("A folder path must be specified.")}
  if (!file.exists(path)) {
    message("The specified folder does not exist. Creating folder")
    dir.create(path)
  }

  if (is.null(filename)) {
    message("No file name provided")
    filename <- "Herbarium"
  }

  if(is.null(font)){
    font <- ""
  }else{
    font <- as.character(font)
    if(length(font)!= 1){
      stop("Font length should be 1")
    }
  }


  if (any(apply(data, 1, nchar) > 150)) {
    message("Too long texts may give undesired results. Please consider shortening long fields.")
  }


  if (is.null(title)) {
    message("No title provided")
    title <- ""
  }

  if (is.null(subtitle)) {
    message("No subtitle provided")
    subtitle <- ""
  }


  ## QR code

  if (!is.null(qr)) {
    stopifnot(is.character(qr))
    # If qr is not a column in data, use same qr for all items
    if (!(qr %in% colnames(data))) {
      data$qr <- qr   # recycling to all rows in data
      qr <- "qr"    # used later for selecting column
    }
  }
  if (is.null(qr)) {
    message("No qr provided")
    data$qr <- ""
    qr <- "qr"    # used later for selecting column
  }




  if (!is.null(family.column)) {
    check_column_in_df(data, family.column)
    data[,family.column] <- toupper(data[,family.column])
  }

  if (!is.null(area.description.column)) {
    check_column_in_df(data, area.description.column)
    data[,area.description.column] <- as.character(data[,area.description.column])
  }



  ## Check columns are in data or create empty characters if NULL

  family.column           <- check_column_or_create_empty_char(data, family.column)
  taxon.column            <- check_column_or_create_empty_char(data, taxon.column)
  author.column           <- check_column_or_create_empty_char(data, author.column)
  det.column              <- check_column_or_create_empty_char(data, det.column)
  date.det.column         <- check_column_or_create_empty_char(data, date.det.column)
  location.column         <- check_column_or_create_empty_char(data, location.column)
  area.description.column <- check_column_or_create_empty_char(data, area.description.column)
  latitude.column         <- check_column_or_create_empty_char(data, latitude.column)
  longitude.column        <- check_column_or_create_empty_char(data, longitude.column)
  location.column         <- check_column_or_create_empty_char(data, location.column)
  elevation.column        <- check_column_or_create_empty_char(data, elevation.column)
  field1.column           <- check_column_or_create_empty_char(data, field1.column)
  field2.column           <- check_column_or_create_empty_char(data, field2.column)
  field3.column           <- check_column_or_create_empty_char(data, field3.column)
  collector.column        <- check_column_or_create_empty_char(data, collector.column)
  collection.column       <- check_column_or_create_empty_char(data, collection.column)
  assistants.column       <- check_column_or_create_empty_char(data, assistants.column)
  date.column             <- check_column_or_create_empty_char(data, date.column)

  arguments <- c(family.column           ,
                 taxon.column            ,
                 author.column           ,
                 det.column              ,
                 date.det.column         ,
                 location.column         ,
                 area.description.column ,
                 latitude.column         ,
                 longitude.column        ,
                 elevation.column        ,
                 field1.column           ,
                 field2.column           ,
                 field3.column           ,
                 collector.column        ,
                 collection.column       ,
                 assistants.column       ,
                 date.column             )
  arguments <- arguments[arguments != ""]

  data <- check_latex_columns(data, arguments)





  ## Keep intermediate files? If no, using tempdir for intermediate files
  if (!isTRUE(keep.files)) {
    folder <- tempdir()
  } else {
    folder <- path  # all files will remain there
  }



  #### Defining Rmd template to use ####

  if (is.null(template)) { # use pkg default
    file.copy(
      from = system.file("rmarkdown/templates/herbarium/skeleton/skeleton.Rmd", package = "labeleR"),
      to = file.path(folder, "herbarium.Rmd"),
      overwrite = TRUE
    )
  }

  if (!is.null(template)) {
    stopifnot(file.exists(template))
    if (template != file.path(folder, "herbarium.Rmd")) {
      file.copy(
        from = template,
        to = file.path(folder, "herbarium.Rmd"),
        overwrite = TRUE
      )
    }
  }



  ## Render
  output_file <- paste0(filename,'.pdf')
  data <- as.data.frame(data)  ## to exploit drop = TRUE when selecting cols below
  bl.char <- rep("~", times = nrow(data))

  rmarkdown::render(
    input = file.path(folder, "herbarium.Rmd"),
    output_dir = path,
    output_file = output_file,
    params = list(
      qr.i = data[, qr],
      title              = if (title                   == "") {bl.char} else {title},
      subtitle           = if (subtitle                == "") {bl.char} else {subtitle},
      family.i           = if (family.column           == "") {bl.char} else {data[,family.column]},
      taxon.i            = if (taxon.column            == "") {bl.char} else {data[,taxon.column]},
      author.i           = if (author.column           == "") {bl.char} else {data[,author.column]},
      det.i              = if (det.column              == "") {bl.char} else {data[,det.column]},
      date.det.i         = if (date.det.column         == "") {bl.char} else {data[,date.det.column]},
      location.i         = if (location.column         == "") {bl.char} else {data[,location.column]},
      area.description.i = if (area.description.column == "") {bl.char} else {data[,area.description.column]},
      latitude.i         = if (latitude.column         == "") {bl.char} else {data[,latitude.column]},
      longitude.i        = if (longitude.column        == "") {bl.char} else {data[,longitude.column]},
      elevation.i        = if (elevation.column        == "") {bl.char} else {data[,elevation.column]},
      field1.i           = if (field1.column           == "") {bl.char} else {data[,field1.column]},
      field2.i           = if (field2.column           == "") {bl.char} else {data[,field2.column]},
      field3.i           = if (field3.column           == "") {bl.char} else {data[,field3.column]},
      collector.i        = if (collector.column        == "") {bl.char} else {data[,collector.column]},
      collection.i       = if (collection.column       == "") {bl.char} else {data[,collection.column]},
      assistants.i       = if (assistants.column       == "") {bl.char} else {data[,assistants.column]},
      date.i             = if (date.column             == "") {bl.char} else {data[,date.column]},
      font = font
    )
  )

}

