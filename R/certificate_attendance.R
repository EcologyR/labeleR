#' Create certificate of attendance
#'
#' @param language Select english or spanish
#' @param url Specify a valid Google Sheets URL
#' @param select.column Column of the Google Sheets that specifies which rows must be selected
#' @param select.value Value of \code{select.column} that specifies which rows must be selected
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
#'url <- 'https://docs.google.com/spreadsheets/d/1inkk3_oNvvt8ajdK4wOkSgPoUyE8JzENrZgSTFJEFBw/edit#gid=0',
#'language="en",
#'type="class",
#'title="Potions Class, Hogwarts School year 1992-1993",
#'signer="A.P.W.B. Dumbledore",
#'hours=1250,
#'signer.position="School Headmaster",
#'rpic="templates/Hogwarts_logo.png",
#'lpic=NULL,
#'signature.pic="templates/firma.png" ,
#'name.column="List_assistants",
#'speaker="Severus Snape",
#'date="01/01/2021")

create_certificate_attendance <- function(
    language =c("spanish", "english"),
    url=NULL,
    select.column=NULL,
    select.value=NULL,
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


  if(!dir.exists("temp")){dir.create("temp"); rm.templ <- F}else{rm.templ <- T}

  if (language%in%c("sp", "s")){language<- "spanish"}
  if (language%in%c("en", "e")){language<- "english"}
  match.arg(language, c("spanish", "english"),F)

  if(is.null(url)){stop("A valid Google Sheets URL must be specfied")}
  if(is.null(type)){stop("A type of event (conference, workshop, seminar...) must be specfied")}
  if(is.null(title)){stop("A title must be specified")}
  if(is.null(signer)){stop("An signer name must be specfied")}
  if(is.null(signer.position)){signer.position <- ""}
  if(is.null(hours)){stop("A number of hours name must be specfied")}
  if(is.null(date)){stop("A date must be specfied")}
  if(is.null(speaker)){stop("A speaker must be specfied")}

  if(!(is.character(hours))) {hours <- as.character(hours)}
  erase.lpic <- F; erase.rpic<-F; erase.spic<-F
  if(is.null(lpic)){png("temp/blank.png", 150, 150, "px");dev.off(); lpic <- "temp/blank.png";erase.lpic<-T}
  if(is.null(rpic)){png("temp/blank.png", 150, 150, "px");dev.off(); rpic <- "temp/blank.png";erase.rpic<-T}
  if(is.null(signature.pic)){png("temp/blank.png", 150, 150, "px");dev.off(); spic <- "temp/blank.png";erase.spic<-T}

  file.copy(lpic, "temp/lpic.png")#create files to call them lpic@rpic to make it homogeneous
  file.copy(rpic, "temp/rpic.png")#create files to call them lpic@rpic to make it homogeneous
  file.copy(signature.pic, "temp/spic.png")#create files to call them lpic@rpic to make it homogeneous

   df <- read_sheet(url, select.column, select.value )

  if(!(name.column)%in%colnames(df)){stop("Column '", name.column , "' is not a column of ypur Google Sheets document. Please select from \n", paste0("-", colnames(df), sep="\n"))}


  # load either pdf or word certificate template
  if(language == "english"){template <- readr::read_file("templates/assistance_EN.Rmd")}
  if(language == "spanish"){template <- readr::read_file("templates/assistance_ES.Rmd")}


  template_cert <- template %>%
    stringr::str_replace_all("<<ACTO>>", type) %>%
    stringr::str_replace_all("<<GRUPO>>", title) %>%
    stringr::str_replace_all("<<FIRMANTE>>", signer) %>%
    stringr::str_replace_all("<<PUESTO>>", signer.position)%>%
    stringr::str_replace_all("<<HORAS>>", hours)%>%
    stringr::str_replace_all("<<Fecha>>", date)%>%
    stringr::str_replace_all("<<NOMBRE PONENTE>>", speaker)

  if(!dir.exists("output")){dir.create("output")}

  for (i in 1:nrow(df)){

    #replace the placeholder words in the template with the student information


    personal_cert <- template_cert %>%
      stringr::str_replace_all("<<Asistente>>", df[i, name.column])


    #generate an output file name based on student name
    out_filename = df[i,name.column]
    out_file_pdf = paste0("output/attendance_",out_filename, '.pdf')

    #save customized Rmd to a temporary file
    readr::write_file(personal_cert, "tmp.Rmd")

    #create the certificates using R markdown.
    #it will detect the ending of the output file and use the right format
    rmarkdown::render("tmp.Rmd", output_file = out_file_pdf)

    #temporary Rmd file can be deleted.
    file.remove("tmp.Rmd")

  }

  file.remove("temp/lpic.png")
  file.remove("temp/rpic.png")
  file.remove("temp/spic.png")
  if(erase.lpic){file.remove("temp/blank.png")}
  if(erase.rpic & file.exists("temp/blank.png")){file.remove("temp/blank.png")}
  if(erase.spic & file.exists("temp/blank.png")){file.remove("temp/blank.png")}

  if(rm.templ){unlink("temp", recursive = T, force = T)}
}



