#' Create certificate of participation
#'
#' Create certificate of participation  (1 certificate per DIN-A4 page)
#'
#' @param data A data frame containing participants' names and contributions.
#' @param path path Character. Path to folder where the PDF certificates will be saved.
#' @param filename Character. Filename of the pdf. If NULL, default is "Participation" for English, "Participacion" for Spanish".
#' @param language Character. Select 'English' or 'Spanish'.
#' @param name.column Character. Name of the column in `data` storing participants' name.
#' @param affiliation.column Character (optional). Name of the column in `data`
#' storing participants' affiliation
#' @param comm.type.column Character. Name of the column in `data` reporting
#' participation type (e.g. poster, oral communication, etc)
#' @param title.column Character. Name of the column in `data` storing the title of the
#' contribution.
#' @param coauthor.column Name of the column in `data` storing the names of the coauthors to the contribution.
#' @param date.column Character. Name of the column in `data` storing dates of
#' participation.
#' @param email.column Character. Name of the column in `data` storing attendees' email address
#' to automatically send them their certificates.
#' @param email.info Object created using [configure_email()] function.
#' @param type Character (optional). Type of event (conference, workshop, seminar...)
#' @param event Character. Title of the event
#' @param freetext Character (optional). Free text to insert before the date.
#' Can include LaTeX commands (see examples).
#' @param signer Character. Person who signs the certificate
#' @param signer.role Character. Signer's role or position
#' @param signature.pic Character (optional) Path to a PNG image to appear in
#' the bottom, above signer's name.
#' @param lpic Character (optional) Path to a PNG image to appear in the top-left.
#' @param rpic Character (optional) Path to a PNG image to appear in the top-right.
#' @param keep.files Logical. Keep the RMarkdown template and associated files
#' in the output folder? Default is FALSE.
#' @param template Character (optional) RMarkdown template to use. If not provided,
#' using the default template included in `labeleR`.
#' @inheritParams create_badge
#'
#' @details
#' **font**
#' Not all fonts are able to be used. Consider only those which are stated to be 'Part of TeX Live', and have OTF and TT available. Additionally, fonts whos 'Usage'
#' differs from `\normalfont`, `\itshape` and `\bfseries` usually fail during installation and/or rendering.
#' Several fonts tried and working are:
#' - libertinus
#' - accanthis
#' - Alegreya
#' - algolrevived
#' - almendra
#' - antpolt
#' - Archivo
#' - Baskervaldx
#' - bitter
#' - tgbonum
#' - caladea
#' - librecaslon
#' - tgchorus
#' - cyklop
#' - forum
#' - imfellEnglish
#' - LobsterTwo
#' - quattrocento
#'
#' @return PDF certificates are saved on disk, in the folder defined
#' by `path`. If `keep.files = TRUE`, the RMarkdown template and PNG logo files
#' will also appear in the same folder.
#'
#' @export
#'
#' @author Ignacio Ramos-Gutierrez, Julia G. de Aledo, Francisco Rodriguez-Sanchez
#'
#' @examplesIf interactive()
#
#' create_participation_certificate(
#'   data = participation.table,
#'   path = "labeleR_output",
#'   language = "Spanish",
#'   name.column = "Name",
#'   affiliation.column = "House",
#'   comm.type.column = "Comm.type",
#'   title.column = "Title",
#'   date.column = "Date",
#'   type = "online seminar",
#'   event = "Hogwarts School of Witchcraft and Wizardry",
#'   freetext = "which lasted 2 hours",
#'   signer = "A.P.W.B. Dumbledore",
#'   signer.role = "School Headmaster",
#'   lpic = NULL,
#'   rpic = NULL,
#'   signature.pic = NULL,
#'   font = "libertinus"
#' )

create_participation_certificate <- function(
    data = NULL,
    path = NULL,
    filename = NULL,
    language = c("English", "Spanish"),
    name.column = NULL,
    affiliation.column = "",
    comm.type.column = "",
    title.column = "",
    coauthor.column = "",
    date.column = "",
    email.column = NULL,
    email.info = NULL,
    type = "",
    event = "",
    freetext = "",
    signer = "",
    signer.role = "",
    signature.pic = NULL,
    lpic = NULL,
    rpic = NULL,
    font = NULL,
    keep.files = FALSE,
    template = NULL
){

  language <- match.arg(language, c("English", "Spanish"))

  ## Check arguments

  if (is.null(data)) {
    stop("A data.frame must be provided.")
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
    if (language == "English") {filename <- "Participation"}
    if (language == "Spanish") {filename <- "Participacion"}
  }

  if(is.null(font)){
    font <- ""
  }else{
    font <- as.character(font)
    if(length(font)!= 1){
      stop("Font length should be 1")
    }
  }

  sendmail <- sendmail_setup(email.column, email.info)


  check_column_in_df(data, name.column)

  if (affiliation.column != "") {
    check_column_in_df(data, affiliation.column)
    data[,affiliation.column]<- check_latex(data, affiliation.column)
  }

  if (comm.type.column != "") {
  check_column_in_df(data, comm.type.column)
  data[,comm.type.column]<- check_latex(data, comm.type.column)
  }

  if (coauthor.column != "") {
    check_column_in_df(data, coauthor.column)
    data[,coauthor.column]<- check_latex(data, coauthor.column)
  }

  if (date.column != "") {
    check_column_in_df(data, date.column)
    data[,date.column]<- check_latex(data, date.column)
  }


  check_column_in_df(data, title.column)



  if(!is.null(email.column)){
    check_column_in_df(data, email.column)
  }

  arguments <- c(name.column, title.column) #these are the compulsory arguments
  arguments <- arguments[arguments!=""]

  data <- check_latex_columns(data, arguments)

  stopifnot(is.character(type))
  stopifnot(is.character(event))
  stopifnot(is.character(freetext))
  stopifnot(is.character(signer))
  stopifnot(is.character(signer.role))

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
        from = system.file("rmarkdown/templates/participation_EN/skeleton/skeleton.Rmd", package = "labeleR"),
        to = file.path(folder, "participation.Rmd"),
        overwrite = TRUE
      )
    }

    if (language == "Spanish") {
      file.copy(
        from = system.file("rmarkdown/templates/participation_ES/skeleton/skeleton.Rmd", package = "labeleR"),
        to = file.path(folder, "participation.Rmd"),
        overwrite = TRUE
      )
    }
  }

  if (!is.null(template)) {
    stopifnot(file.exists(template))
    if (template != file.path(folder, "participation.Rmd")) {
      file.copy(
        from = template,
        to = file.path(folder, "participation.Rmd"),
        overwrite = TRUE
      )
    }
  }


  #### Logos ####

  use_image(lpic, name = "lpic", folder = folder)
  use_image(rpic, name = "rpic", folder = folder)
  use_image(signature.pic, name = "spic", folder = folder)



  #### Render #####

  data <- as.data.frame(data) ## to exploit drop = TRUE when selecting cols below

  for (i in 1:nrow(data)) {
    out.name <- filename
    out.name <- paste0(out.name, "_", data[i, name.column])
    out.name <- check_file_name(out.name, ".pdf", path)

    output_file <- paste0(out.name,'.pdf')

    bl.char <- "~"

    rmarkdown::render(
      input = file.path(folder, "participation.Rmd"),
      output_dir = path,
      output_file = output_file,
      params = list(
        name.i        = if (name.column        == "") {bl.char} else {data[i, name.column]},
        affiliation.i = if (affiliation.column == "") {bl.char} else {data[i, affiliation.column]},
        type.i        = if (type               == "") {bl.char} else {type},
        event.i       = if (event              == "") {bl.char} else {event},
        comm.type.i   = if (comm.type.column   == "") {bl.char} else {data[i, comm.type.column]},
        title.i       = if (title.column       == "") {bl.char} else {data[i, title.column]},
        coauthor.i    = if (coauthor.column    == "") {bl.char} else {data[i, coauthor.column]},
        date.i        = if (date.column        == "") {bl.char} else {data[i, date.column]},
        freetext.i    = if (freetext           == "") {bl.char} else {freetext},
        signer.i      = if (signer             == "") {bl.char} else {signer},
        signer.role.i = if (signer.role        == "") {bl.char} else {signer.role},
        font = font
      )
    )

      if(isTRUE(sendmail)){
        send_mail(data = data,
                 row = i,
                 email.info = email.info,
                 name.column  =  name.column ,
                 email.column =  email.column,
                 attachment = paste0(path, "/", output_file) )
      }
    }
  }



