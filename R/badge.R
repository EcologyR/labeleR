#' Create badges
#'
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
#' @param keep.files Logical. Keep the Rmarkdown template and associated files
#' in the output folder? Default is FALSE.
#' @param template Character (optional) Rmarkdown template to use. If not provided,
#' using the default template included in `labeleR`.
#'
#' @return A PDF file named "Badges.pdf" is saved on disk, in the folder defined
#' by `path`. If `keep.files = TRUE`, an Rmarkdown and PNG logo files will also
#' appear in the same folder.
#'
#' @export
#'
#' @author Julia G. de Aledo, Ignacio Ramos-Gutierrez, Francisco Rodríguez-Sánchez
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
                         rpic = NULL,
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

  if (is.null(event)) {
    message("No event provided")
    event <- ""
  }

  check_column_in_df(data, name.column)
  if (!is.null(affiliation.column)) check_column_in_df(data, affiliation.column)

  if (is.null(affiliation.column)) {
    affiliation.column <- ""
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
      from = system.file("rmarkdown/templates/badge/skeleton/skeleton.Rmd", package = "labeleR"),
      to = file.path(folder, "badge.Rmd"),
      overwrite = TRUE
    )
  } else {
    stopifnot(file.exists(template))
    file.copy(
      from = template,
      to = file.path(folder, "badge.Rmd"),
      overwrite = TRUE
    )
  }


  ## Logos

  use_image(lpic, name = "lpic", folder = folder)
  use_image(rpic, name = "rpic", folder = folder)



  ## Render
  data <- as.data.frame(data)  ## to exploit drop = TRUE when selecting cols below
  rmarkdown::render(
    input = file.path(folder, "badge.Rmd"),
    output_dir = path,
    output_file = "Badges.pdf",
    params = list(
      event        = event,
      names        = data[, name.column],
      affiliations = if (affiliation.column == "") {"~"} else {data[, affiliation.column]}
    )
  )

}



