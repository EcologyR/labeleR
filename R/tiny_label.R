#' Create tiny labels
#'
#' Create tiny labels (16 per DIN-A4 page)
#'
#' @param data a data frame including information of a species
#' @param path Character. Path to folder where the PDF file will be saved.
#' @param filename Character. Filename of the pdf. If NULL, default is "Tiny_label".
#' @param qr String. Free text or column of \code{data} that specifies the link where to create the QR code.
#'          If the specified value is not a column name of \code{data}, all the QRs will be equal,
#'          and will output the specified \code{qr}.
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
#'
#' @return A PDF file named "Tiny_label.pdf" is saved on disk, in the folder defined
#' by `path`. If `keep.files = TRUE`, an Rmarkdown file will also appear in the same folder.
#'
#' @export
#'
#' @author  Ignacio Ramos-Gutiérrez, Julia G. de Aledo, Francisco Rodríguez-Sánchez
#'
#' @examplesIf interactive()
#' create_tinylabel(
#' data = tiny.table,
#' qr = "QR_code",
#' path = "LabeleR_output",
#' field1.column = "campo1",
#' field2.column = "campo2",
#' field3.column = "campo3",
#' field4.column = "campo4",
#' field5.column = "campo5"
#' )
#'

create_tinylabel <- function(data = NULL,
                             qr = NULL,
                             path = NULL,
                             filename = NULL,
                             field1.column = NULL,
                             field2.column = NULL,
                             field3.column = NULL,
                             field4.column = NULL,
                             field5.column = NULL,
                             keep.files = FALSE,
                             template = NULL) {

## Check arguments

if (is.null(data)) {
  stop("Please provide a data.frame or tibble.")
  }

if (!inherits(data, "data.frame")) {stop("The 'data' object must be a data frame.")}

if (is.null(path)) {stop("A folder path must be specified.")}
if (!file.exists(path)) {
message("The specified folder does not exist. Creating folder")
dir.create(path)
}

  if (is.null(filename)) {
    message("No file name provided")
    filename <- "Tiny_label"
  }

if (is.null(qr)) {
message("No qr provided")
qr <- ""
}

if (!(qr %in% colnames(data))) {
  data$qr <- qr
  qr <- "qr"
  data[,qr]<- as.character (data[,qr])
}


  if (is.null(field1.column)) {
    field1.column <- ""
  }
  check_column_in_df(data, field1.column)

if (is.null(field2.column)) {
field2.column <- ""
}
check_column_in_df(data, field2.column)

if (is.null(field3.column)) {
field3.column <- ""
}
check_column_in_df(data, field3.column)

if (is.null(field4.column)) {
field4.column <- ""
}
check_column_in_df(data, field4.column)

if (is.null(field5.column)) {
field5.column <- ""
}
check_column_in_df(data, field5.column)

if (any(apply(data, 1, nchar)>150)) {
  message("Warning: cells containing too long texts may alter the result.
          Please consider shortening the content of your cells.")
  }

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
    from = system.file("rmarkdown/templates/tiny_label/skeleton/skeleton.Rmd", package = "labeleR"),
    to = file.path(folder, "tiny_label.Rmd"),
    overwrite = TRUE
  )
} else {
  stopifnot(file.exists(template))
  file.copy(
    from = template,
    to = file.path(folder, "tiny_label.Rmd"),
    overwrite = TRUE
  )
}


## Render
output_file <- paste0(filename,'.pdf')
data <- as.data.frame(data)## to exploit drop = TRUE when selecting cols below
bl.char <- rep("~", times=nrow(data))

rmarkdown::render(
  input = file.path(folder, "tiny_label.Rmd"),
  output_dir = path,
  output_file = output_file,
  params = list(
    qr.i = data[,qr],
    field1.i = if(field1.column==""){bl.char}else{data[,field1.column]},
    field2.i = if(field2.column==""){bl.char}else{data[,field2.column]},
    field3.i = if(field3.column==""){bl.char}else{data[,field3.column]},
    field4.i = if(field4.column==""){bl.char}else{data[,field4.column]},
    field5.i = if(field5.column==""){bl.char}else{data[,field5.column]}
    )
  )

}
