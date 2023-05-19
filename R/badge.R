#' Create badges (8 per DIN-A4 page)
#'
#' @param data a data frame including names and (optionally) affiliations.
#' @param path Character. Path to folder where the PDF file will be saved.
#' @param event Character. Title of the event.
#' @param name.column Character. Name of the column in `data` storing participants' name.
#' @param affiliation.column Character (optional). Name of the column in `data`
#' storing participants' affiliation.
#' @param lpic Character (optional) Path to a PNG image to be located in the badge top-left.
#' @param rpic Character (optional) Path to a PNG image to be located in the badge top-right.
#'
#' @return A PDF file named "Badges.pdf" is saved on disk.
#'
#' @export
#'
#' @author Julia G. de Aledo, Ignacio Ramos-Gutierrez
#'
#' @examplesIf interactive()
#' data <- read_sheet(url = 'https://docs.google.com/spreadsheets/d
#'         /16smXdP-Ehwu1cEmJTbJI1DerIpUrOcD7H5Ni6z9B07M/edit#gid=0')
#' create_badge(
#' data = data,
#' path = "labeleR_output",
#' event = "INTERNATIONAL CONFERENCE OF MUGGLEOLOGY",
#' name.column = "List",
#' affiliation.column = "Affiliation",
#' lpic = NULL,
#' rpic = NULL)

create_badge <- function(data = NULL,
                                 path = NULL,
                                 event = NULL,
                                 name.column = NULL,
                                 affiliation.column = NULL,
                                 lpic = NULL,
                                 rpic = NULL
) {

  if (is.null(data)) {
    stop("A data.frame must be provided. To import from Google Sheets use function 'read_sheet()'")
  }

  if (!inherits(data, "data.frame")) {stop("The 'data' object must be a data frame.")}

  if (is.null(path)) {stop("A folder path must be specified.")}
  if (!file.exists(path)) {
    message("The specified folder does not exist. Creating folder")
    dir.create(path)
  }

  if (is.null(event)) {
    message("No event provided")
    event <- ""
  }

  if (!(name.column) %in% colnames(data)) {
    stop("Column '", name.column,
         " is not a column of your 'data' object. Please select from \n",
         paste0("-", colnames(data), sep = "\n"))
  }

  if (is.null(affiliation.column)) {
    affiliation.column <- ""
  }

  if (!(affiliation.column) %in% c(colnames(data),"")) {
    stop("Column '", affiliation.column ,
         "' is not a column of your 'data' object. Please select from \n",
         paste0("-", colnames(data), sep = "\n"))
  }

  # Create tempdir to store all intermediate files
  tmp <- tempdir()


  if (!is.null(lpic)) {
    if (!file.exists(lpic)) {
      stop(lpic, " file not found")
    } else {
      file.copy(from = lpic, to = file.path(tmp, "lpic.png"), overwrite = TRUE)
    }
  }

  if (!is.null(rpic)) {
    if (!file.exists(rpic)) {
      stop(rpic, " file not found")
    } else {
      file.copy(from = rpic, to = file.path(tmp, "rpic.png"), overwrite = TRUE)
    }
  }

  if (is.null(lpic)) {
    grDevices::png(file.path(tmp, "lpic.png"), 150, 150, "px")
    graphics::plot.new()
    grDevices::dev.off()
  }

  if (is.null(rpic)) {
    grDevices::png(file.path(tmp, "rpic.png"), 150, 150, "px")
    graphics::plot.new()
    grDevices::dev.off()
  }


  tmpl_file <- system.file("rmarkdown/templates/badge/skeleton/skeleton.Rmd", package = "labeleR")
  file.copy(tmpl_file, file.path(tmp, "badge.Rmd"), overwrite = TRUE)
  tmpl_file   <- file.path(tmp, "badge.Rmd")
  output_file <- 'Badges.pdf'  # allow user to choose name of the pdf file?


  rmarkdown::render(
    tmpl_file,
    output_dir = path,
    output_file = output_file,
    params = list(
      event        = event,
      names        = data[, name.column],
      affiliations = data[, affiliation.column]
    )
  )

}


