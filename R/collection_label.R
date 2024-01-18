#' Create collection labels
#'
#' Create collection labels (8 labels per DIN-A4 page)
#'
#' @param data a data frame. Each row contains the information by species that will appear in the label.
#' @param path Character. Path to folder where the PDF file will be saved.
#' @param filename Character. Filename of the pdf. If NULL, default is "Collection_label".
#' @param qr String. Free text or column of `data` that specifies the link for the QR code.
#'          If the specified value of `qr` is not a column name of `data`,
#'          all the QRs will be equal, pointing to the same link.
#' @param field1.column Character (optional). Name of the column in `data` storing the first free text to
#' appear at the top of the label.
#' @param field2.column Character (optional). Name of the column in `data` storing the second free text to
#' appear below field1.
#' @param field3.column Character (optional). Name of the column in `data` storing the third free text to
#' appear below field2.
#' @param field4.column Character (optional). Name of the column in `data` storing the fourth free text to
#' appear below field3.
#' @param field5.column Character (optional). Name of the column in `data` storing the fifth free text to
#' appear below field4.
#' @param logo Character (optional) Path to a PNG image to be located in the label bottom.
#' @param bgcolor HTML color for label background. Default is D0ECC1
#' @param textcolor HTML color for label text. Default is 1E3F20
#' @inheritParams create_badge
#'
#' @return A PDF file named "Collection_label.pdf" is saved on disk, in the folder defined
#' by `path`. If `keep.files = TRUE`, an RMarkdown and PNG logo files will also
#' appear in the same folder.
#'
#' @export
#'
#' @author Ignacio Ramos-Gutierrez, Julia G. de Aledo, Francisco Rodriguez-Sanchez
#'
#' @examplesIf interactive()
#'
#' create_collection_label(
#'   data = collection.table,
#'   path = "labeleR_output",
#'   qr = "QR_code",
#'   field1.column = "field1",
#'   field2.column = "field2",
#'   field3.column = "field3",
#'   field4.column = "field6",
#'   field5.column = "field7"
#' )

create_collection_label <- function(data = NULL,
                                    path = NULL,
                                    filename = NULL,
                                    qr = NULL,
                                    field1.column = NULL,
                                    field2.column = NULL,
                                    field3.column = NULL,
                                    field4.column = NULL,
                                    field5.column = NULL,
                                    logo = NULL,
                                    bgcolor = "D0ECC1",
                                    textcolor = "1E3F20",
                                    keep.files = FALSE,
                                    template = NULL) {


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
    filename <- "Collection_label"
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


  ## Check field columns are in data or create empty characters if NULL

  field1.column <- check_column_or_create_empty_char(data, field1.column)
  field2.column <- check_column_or_create_empty_char(data, field2.column)
  field3.column <- check_column_or_create_empty_char(data, field3.column)
  field4.column <- check_column_or_create_empty_char(data, field4.column)
  field5.column <- check_column_or_create_empty_char(data, field5.column)

  arguments <- c(field1.column,field2.column,field3.column,field4.column,field5.column)
  arguments <- arguments[arguments!=""]

  data <- check_latex_columns(data, arguments)






  ## Keep intermediate files? If no, using tempdir for intermediate files
  if (!isTRUE(keep.files)) {
    folder <- tempdir()
  } else {
    folder <- path  # all files will remain there
  }


  ## Defining Rmd template to use
  if (is.null(template)) {
    # use pkg default
    file.copy(
      from = system.file("rmarkdown/templates/collection_label/skeleton/skeleton.Rmd", package = "labeleR"),
      to = file.path(folder, "collection_label.Rmd"),
      overwrite = TRUE
    )
  } else {
    stopifnot(file.exists(template))
    file.copy(
      from = template,
      to = file.path(folder, "collection_label.Rmd"),
      overwrite = TRUE
    )
  }

  ## Logos

  use_image(logo, name = "logo", folder = folder)

  ## Render
  output_file <- paste0(filename,'.pdf')
  data <- as.data.frame(data)  ## to exploit drop = TRUE when selecting cols below
  bl.char <- rep("~", times = nrow(data))
  rmarkdown::render(
    input = file.path(folder, "collection_label.Rmd"),
    output_dir = path,
    output_file = output_file,
    params = list(
      qr.i = data[, qr],
      field1.i = if (field1.column == "") {bl.char} else {data[,field1.column]},
      field2.i = if (field2.column == "") {bl.char} else {data[,field2.column]},
      field3.i = if (field3.column == "") {bl.char} else {data[,field3.column]},
      field4.i = if (field4.column == "") {bl.char} else {data[,field4.column]},
      field5.i = if (field5.column == "") {bl.char} else {data[,field5.column]},
      bgcolor = bgcolor,
      textcolor = textcolor
    )
  )

}
