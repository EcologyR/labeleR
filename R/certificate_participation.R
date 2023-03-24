

#' Create certificate of participation
#'
#' @param language Select english or spanish
#' @param url Specify a valid Google Sheets URL
#' @param select.column Column of the Google Sheets that specifies which rows must be selected
#' @param select.value Value of \code{select.column} that specifies which rows must be selected
#' @param type Type of event (conference, workshop, seminar...)
#' @param organiser Name of the organizing entity
#' @param hours Number of hours the event has lasted
#' @param signer Person who credits the certificate
#' @param signer.position Position of the \code{signer}
#' @param lpic PNG object. File route of the top-left image. Can be blank if set to NULL.
#' @param rpic PNG object. File route of the top-right image. Can be blank if set to NULL.
#' @param signature.pic PNG object. File route of a signature image. Can be blank if set to NULL.
#' @param name.column Column name of the Google Sheet column which specifies the particpant name.
#' @param affiliation.column Column name of the Google Sheet column which specifies the particpant affiliation
#' @param date.column Column name of the Google Sheet column which specifies the date.
#' @param title.column Column name of the Google Sheet column which specifies the communication title.
#' @param comm.type.column Column name of the Google Sheet column which specifies the communication type (oral, poster, online...)
#'
#' @return An 'output' folder with the PDF documents inside
#'
#' @export
#'
#' @author Julia G. de Aledo, Ignacio Ramos-Gutierrez
#'
#' @examples
#' create_certificate_participation(
#' language ="en",
#' url="https://docs.google.com/spreadsheets/u/1/d/11No4aLvta2qxGhkxD7W6HfNfGmO1wpCIDvyRKFF-_gM/edit?usp=drive_web&ouid=106603768357414088091",
#' select.column=NULL,
#' select.value=NULL,
#' type="online seminar",
#' organiser="Hogwarts School of Witchcraft and Wizardry",
#' hours=2,
#' signer="A.P.W.B. Dumbledore",
#' signer.position="School Headmaster",
#' lpic="templates/Hogwarts_logo.png",
#' rpic=NULL,
#' signature.pic = "templates/firma.png",
#' name.column="Name",
#' affiliation.column="House",
#' date.column="Date",
#' title.column="Title",
#' comm.type.column = "Comm.type")
#'
create_certificate_participation <- function(
    language =c("spanish", "english"),
    url=NULL,
    select.column=NULL,
    select.value=NULL,
    type=NULL,
    organiser=NULL,
    hours=NULL,
    signer=NULL,
    signer.position=NULL,
    lpic=NULL,
    rpic=NULL,
    signature.pic = NULL,
    name.column=NULL,
    affiliation.column=NULL,
    date.column=NULL,
    title.column=NULL,
    comm.type.column = NULL){

  if(!dir.exists("temp")){dir.create("temp")}

  if (language%in%c("sp", "s")){language<- "spanish"}
  if (language%in%c("en", "e")){language<- "english"}
  match.arg(language, c("spanish", "english"),F)

  if(is.null(url)){stop("A valid Google Sheets URL must be specfied")}
  if(is.null(type)){stop("A type of event (conference, workshop, seminar...) must be specfied")}
  if(is.null(organiser)){stop("An organiser must be specfied")}
  if(is.null(signer)){stop("An signer name must be specfied")}
  if(is.null(signer.position)){signer.position <- ""}
  if(is.null(hours)){stop("A number of hours name must be specfied")}
  if(!(is.character(hours))) {hours <- as.character(hours)}
  erase.lpic <- F; erase.rpic<-F; erase.spic<-F
  if(is.null(lpic)){png("temp/blank.png", 150, 150, "px");dev.off(); lpic <- "temp/blank.png";erase.lpic<-T}
  if(is.null(rpic)){png("temp/blank.png", 150, 150, "px");dev.off(); rpic <- "temp/blank.png";erase.rpic<-T}
  if(is.null(signature.pic)){png("temp/blank.png", 150, 150, "px");dev.off(); spic <- "temp/blank.png";erase.spic<-T}

  file.copy(lpic, "temp/lpic.png")#create files to call them lpic@rpic to make it homogeneous
  file.copy(rpic, "temp/rpic.png")#create files to call them lpic@rpic to make it homogeneous
  file.copy(signature.pic, "temp/spic.png")#create files to call them lpic@rpic to make it homogeneous


df <- read_sheet(url, select.column, select.value )

if(!(name.column)%in%colnames(df)){stop("Column '", name.column , "' is not a column of your Google Sheets document. Please select from \n", paste0("-", colnames(df), sep="\n"))}
if(!(date.column)%in%colnames(df)){stop("Column '", date.column , "' is not a column of your Google Sheets document. Please select from \n", paste0("-", colnames(df), sep="\n"))}
if(!(title.column)%in%colnames(df)){stop("Column '", title.column , "' is not a column of your Google Sheets document. Please select from \n", paste0("-", colnames(df), sep="\n"))}
if(!(comm.type.column)%in%colnames(df)){stop("Column '", comm.type.column , "' is not a column of your Google Sheets document. Please select from \n", paste0("-", colnames(df), sep="\n"))}
if(!(affiliation.column)%in%colnames(df)){stop("Column '", affiliation.column , "' is not a column of your Google Sheets document. Please select from \n", paste0("-", colnames(df), sep="\n"))}


# load either pdf or word certificate template
if(language == "english"){template <- readr::read_file("templates/participation_EN.Rmd")}
if(language == "spanish"){template <- readr::read_file("templates/participation_ES.Rmd")}


template_cert <- template %>%
  stringr::str_replace_all("<<ACTO>>", type) %>%
  stringr::str_replace_all("<<GRUPO>>", organiser) %>%
  stringr::str_replace_all("<<FIRMANTE>>", signer) %>%
  stringr::str_replace_all("<<PUESTO>>", signer.position)%>%
  stringr::str_replace_all("<<HORAS>>", hours)

if(!dir.exists("output")){dir.create("output")}

for (i in 1:nrow(df)){

  #replace the placeholder words in the template with the student information


  personal_cert <- template_cert %>%
    stringr::str_replace_all("<<Ponente>>", df[i, name.column]) %>%
    stringr::str_replace_all("<<Fecha>>", df[i, date.column])%>%
    stringr::str_replace_all("<<Titulo>>", df[i, title.column])%>%
    stringr::str_replace_all("<<Afiliacion>>", df[i, affiliation.column])%>%
    stringr::str_replace_all("<<Tipo>>", df[i, comm.type.column])

  #generate an output file name based on student name
  out_filename = df[i,name.column]
  out_datename = df[i,date.column]
  out_datename <- stringr::str_replace_all(out_datename, "/", "_")
  out_file = paste(out_filename, out_datename, sep=("-"))
  out_file_pdf = paste0("output/participation_",out_file, '.pdf')

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

unlink("temp", recursive = T, force = T)
}


