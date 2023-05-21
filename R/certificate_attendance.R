#' Create certificate of attendance
#'
#' @param data a data frame containing attendees' names (in `name.column`)
#' @param path Character. Path to folder where the PDF certificates will be saved.
#' @param language Character. Select 'English' or 'Spanish'.
#' @param name.column Character. Name of the column in `data` storing attendees' name.
#' @param type Character (optional). Type of event (conference, workshop, seminar...)
#' @param title Character. Title of the event
#' @param date Date of the event
#' @param hours Number of hours the event has lasted
#' @param freetext Character (optional). Free text to insert between the
#' event title and date. Can include LaTeX commands (see examples).
#' @param signer Character. Person who signs the certificate
#' @param signer.role Character. Signer's role or position
#' @param signature.pic Character (optional) Path to a PNG image to appear in
#' the bottom, above signer's name.
#' @param lpic Character (optional) Path to a PNG image to appear in the top-left.
#' @param rpic Character (optional) Path to a PNG image to appear in the top-right.
#' @param keep.files Logical. Keep the Rmarkdown template and associated files
#' in the output folder? Default is FALSE.
#' @param template Character (optional) Rmarkdown template to use. If not provided,
#' using the default template included in `labeleR`.
#'
#' @return PDF certificates are saved on disk, in the folder defined
#' by `path`. If `keep.files = TRUE`, the Rmarkdown template and PNG logo files
#' will also appear in the same folder.
#'
#' @export
#'
#' @author Julia G. de Aledo, Ignacio Ramos-Gutierrez, Francisco Rodríguez-Sánchez
#'
#' @examplesIf interactive()
#' data <- data.frame(Names = c("Joe", "Mary"))
#'
#' create_certificate_attendance(data,
#' path = "labeleR_output",
#' language = "English",
#' name.column = "Names",
#' type = "class",
#' title = "Potions Class",
#' date = "01/01/2021",
#' hours = 200,
#' freetext = "organised by {\\bf Hogwarts School year 1992-1993}",
#' signer = "A.P.W.B. Dumbledore",
#' signer.role = "School Headmaster",
#' lpic = NULL,
#' rpic = NULL,
#' signature.pic = NULL,
#' )


create_certificate_attendance <- function(
    data = NULL,
    path = NULL,
    language = c("English", "Spanish"),
    name.column = NULL,
    type = "",
    title = "",
    date = "",
    hours = "",
    freetext = "",
    signer = "",
    signer.role = "",
    signature.pic = NULL,
    lpic = NULL,
    rpic = NULL,
    keep.files = FALSE,
    template = NULL) {

  language <- match.arg(language)

  ## Check arguments

  if (is.null(data)) {
    stop("A data.frame must be provided. To import from Google Sheets use function 'read_sheet()'")
  }

  if (!inherits(data, "data.frame")) {stop("The 'data' object must be a data frame.")}

  if (is.null(path)) {stop("A folder path must be specified.")}
  if (!file.exists(path)) {
    message("The specified folder does not exist. Creating folder")
    dir.create(path)
  }

  stopifnot(is.character(name.column))
  if (!(name.column) %in% colnames(data)) {
    stop("Column '", name.column ,
         " is not a column of your 'data' object. Please select from \n",
         paste0("-", colnames(df), sep = "\n"))
  }

  stopifnot(is.character(type))
  stopifnot(is.character(title))
  stopifnot(is.character(freetext))
  stopifnot(is.character(signer))
  stopifnot(is.character(signer.role))

  if (!(is.character(date))) {
    date <- as.character(date)
  }

  if (!(is.character(hours))) {
    hours <- as.character(hours)
  }


  #### End of argument checks ####


  ## Keep intermediate files? If no, using tempdir for intermediate files
  if (!isTRUE(keep.files)) {
    folder <- tempdir()
  } else {
    folder <- path  # all files will remain there
  }


  #### Defining Rmd template to use ####

  if (is.null(template)) { # use pkg default

    if (language == "English") {
      file.copy(
        from = system.file("rmarkdown/templates/attendance_EN/skeleton/skeleton.Rmd", package = "labeleR"),
        to = file.path(folder, "attendance.Rmd"),
        overwrite = TRUE
      )
    }

    if (language == "Spanish") {
      file.copy(
        from = system.file("rmarkdown/templates/attendance_ES/skeleton/skeleton.Rmd", package = "labeleR"),
        to = file.path(folder, "attendance.Rmd"),
        overwrite = TRUE
      )
    }
  }

  if (!is.null(template)) {
    stopifnot(file.exists(template))
    if (template != file.path(folder, "attendance.Rmd")) {
      file.copy(
        from = template,
        to = file.path(folder, "attendance.Rmd"),
        overwrite = TRUE
      )
    }
  }


  #### Logos ####

  ## If logos provided
  if (!is.null(lpic)) {
    if (!file.exists(lpic)) {
      stop(lpic, " file not found")
    } else {
      file.copy(from = lpic, to = file.path(folder, "lpic.png"), overwrite = TRUE)
    }
  }

  if (!is.null(rpic)) {
    if (!file.exists(rpic)) {
      stop(rpic, " file not found")
    } else {
      file.copy(from = rpic, to = file.path(folder, "rpic.png"), overwrite = TRUE)
    }
  }

  if (!is.null(signature.pic)) {
    if (!file.exists(signature.pic)) {
      stop(signature.pic, " file not found")
    } else {
      file.copy(from = signature.pic, to = file.path(folder, "spic.png"), overwrite = TRUE)
    }
  }


  ## If logos not provided
  if (is.null(lpic)) {
    grDevices::png(file.path(folder, "lpic.png"), 150, 150, "px")
    graphics::plot.new()
    grDevices::dev.off()
  }

  if (is.null(rpic)) {
    grDevices::png(file.path(folder, "rpic.png"), 150, 150, "px")
    graphics::plot.new()
    grDevices::dev.off()
  }

  if (is.null(signature.pic)) {
    grDevices::png(file.path(folder, "spic.png"), 150, 150, "px")
    graphics::plot.new()
    grDevices::dev.off()
  }



  #### Render #####

  for (i in 1:nrow(data)) {

    if (language == "English") {out.name <- "Attendance"}
    if (language == "Spanish") {out.name <- "Asistencia"}

    out.name <- paste0(out.name, "_", data[i, name.column])
    output_file <- paste0(out.name, '.pdf')

    bl.char <- "~"

    rmarkdown::render(
      input = file.path(folder, "attendance.Rmd"),
      output_dir = path,
      output_file = output_file,
      params = list(
        name.column.i   = if (name.column     == "") {bl.char} else {data[i, name.column]},
        type            = if (type            == "") {bl.char} else {type},
        title           = if (title           == "") {bl.char} else {title},
        freetext        = if (freetext        == "") {bl.char} else {freetext},
        date            = if (date            == "") {bl.char} else {date},
        hours           = if (hours           == "") {bl.char} else {hours},
        signer          = if (signer          == "") {bl.char} else {signer},
        signer.role     = if (signer.role     == "") {bl.char} else {signer.role}
      )
    )

  }

}
