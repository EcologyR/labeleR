#' Create certificate of attendance
#'
#' @param data a data frame  to create attendance certificates.
#' @param language Select english or spanish
#' @param type Type of event (conference, workshop, seminar...)
#' @param title Title of the event
#' @param organiser Name of the organizing entity
#' @param hours Number of hours the event has lasted
#' @param signer Person who credits the certificate
#' @param signer.position Position of the \code{signer}
#' @param date Date of the event
#' @param speaker Name of the speaker of the event
#' @param lpic PNG object. File route of the top-left image. Can be blank if set to NULL.
#' @param rpic PNG object. File route of the top-right image. Can be blank if set to NULL.
#' @param signature.pic PNG object. File route of a signature image. Can be blank if set to NULL.
#' @param name.column Column name of the Google Sheet column which specifies the attendee's name.
#'
#' @return An 'output' folder with the PDF documents inside
#'
#' @export
#'
#' @author Julia G. de Aledo, Ignacio Ramos-Gutierrez
#'
#' @examples
#' create_certificate_attendance(
#' data= read_sheet('https://docs.google.com/spreadsheets/d/1inkk3_oNvvt8ajdK4wOkSgPoUyE8JzENrZgSTFJEFBw/edit#gid=0'),
#' language="en",
#' type="class",
#' title="Potions Class",
#' organiser="Hogwarts School year 1992-1993",
#' signer="A.P.W.B. Dumbledore",
#' signer.position="School Headmaster",
#' hours=200,
#' date="01/01/2021",
#' speaker="Severus Snape",
#' rpic=NULL,
#' lpic=NULL,
#' signature.pic=NULL,
#' name.column="List_assistants"
#' )

create_certificate_attendance <- function(
    data=NULL,
    language =c("spanish", "english"),
    type=NULL,
    title=NULL,
    organiser=NULL,
    hours=NULL,
    signer=NULL,
    signer.position=NULL,
    date=NULL,
    speaker=NULL,
    lpic=NULL,
    rpic=NULL,
    signature.pic = NULL,
    name.column=NULL){

  if(!dir.exists("tmp")){dir.create("tmp")}

  if (language%in%c("sp", "s")){language<- "spanish"}
  if (language%in%c("en", "e")){language<- "english"}
  match.arg(language, c("spanish", "english"),F)

  if(is.null(data)){
    stop(" a 'data' data.frame must be provided.
         To import from Google Sheets use function 'read_sheet()'")
  }
  if(is.null(type)){
    stop("A type of event (conference, workshop, seminar...) must be specfied")
  }
  if(is.null(organiser)){
    stop("An organiser must be specfied")
  }
  if(is.null(title)){
    stop("A title must be specfied")
  }
  if(is.null(signer)){
    stop("An signer name must be specfied")
  }
  if(is.null(signer.position)){
    signer.position <- ""
  }
  if(is.null(hours)){
    stop("A number of hours name must be specfied")
  }
  if(!(is.character(hours))) {
    hours <- as.character(hours)
  }



  if(is.null(lpic))         {
    png("tmp/blank.png", 150, 150, "px")
    plot.new()
    dev.off()
    lpic <- "tmp/blank.png"
  }
  if(is.null(rpic))         {
    png("tmp/blank.png", 150, 150, "px")
    plot.new()
    dev.off()
    rpic <- "tmp/blank.png"
  }
  if(is.null(signature.pic)){
    png("tmp/blank.png", 150, 150, "px")
    plot.new()
    dev.off()
    signature.pic <- "tmp/blank.png"
  }

  file.copy(lpic, "tmp/lpic.png")#create files to call them lpic@rpic to make it homogeneous
  file.copy(rpic, "tmp/rpic.png")#create files to call them lpic@rpic to make it homogeneous
  file.copy(signature.pic, "tmp/spic.png")#create files to call them lpic@rpic to make it homogeneous


  df <- data

  if(!(name.column)%in%colnames(df)){
    stop("Column '", name.column ,
         "' is not a column of your data frame. Please select from \n",
         paste0("-", colnames(df), sep="\n"))
  }


  # load either pdf or word certificate template
  if(language == "english"){
    tmpl_file   <- system.file("rmarkdown/templates/attendance_EN/sketelon/skeleton.Rmd", package="labeleR")
    }
  if(language == "spanish"){
    tmpl_file   <- system.file("rmarkdown/templates/attendance_ES/sketelon/skeleton.Rmd", package="labeleR")
    }

  file.copy(tmpl_file, "tmp/attendance.Rmd", overwrite = T)#create files to call them lpic@rpic to make it homogeneous

  tmpl_file   <- "tmp/attendance.Rmd"

  for(i in 1:nrow(df)){

    if(language == "english"){out.name <- "Attendance"}
    if(language == "spanish"){out.name <- "Asistencia"}

    out.name <- paste0(out.name, "_", df[i,name.column])
    output_file <- paste0(out.name,'.pdf')

    rmarkdown::render(
      tmpl_file,
      output_dir = "tmp",
      output_file = output_file,
      params = list(
        type            = type,
        organiser       = organiser,
        hours           = hours,
        signer          = signer,
        signer.position = signer.position,
        name.column.i   = df[i,name.column],
        speaker         = speaker,
        title           = title,
        date            = date
      )
    )

    if(!dir.exists("output")){dir.create("output")}
    file.copy(paste0("tmp/",output_file), paste0("output/",output_file), overwrite=T)#create files to call them lpic@rpic to make it homogeneous
  }

  unlink("tmp", recursive = T, force = T)

}
