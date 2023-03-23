#####################################################
# automatically create customized certificates
#####################################################

# load needed packages
library('readr')
library('dplyr')
library('stringr')
library(dplyr)
# load data, read everything in as a string/character
# Añadir bbdd####
create_certificate <- function(
    certificate =c("attendance", "participation"),
    language =c("spanish", "english"),
    url=NULL,
    type=NULL,
    organiser=NULL,
    signer=NULL,
    signer.position=NULL,
    hours=NULL,
    lpic=NULL,
    rpic=NULL,
    signature.pic = NULL,
    name.column=NULL,
    date.column=NULL,
    title.column=NULL,
    affiliation.column=NULL,
    comm.type.column = NULL

    ){
  url <- 'https://docs.google.com/spreadsheets/d/1uwhi7IROqDcdMEEld7yuyyAoSfuuJUbK2HVlHvUNzFk/edit#gid=0'
  certificate= "attendance";language="sp";type="workshop";organiser="AEET"; signer="Darth vader";
  signer.position="Tu padre"; lpic=NULL; rpic=NULL; signature.pic=NULL ;
  name.column="Ponente"; date.column="Fecha" ; title.column="Titulo";  affiliation.column="Afiliacion";
  hours=3; comm.type.column="Tipo"

match.arg(certificate,c("attendance", "participation"), F )

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
  erase.lpic <- F; erase.lpic<-F; erase.spic<-F
  if(is.null(lpic)){png("blank.png", 150, 150, "px");dev.off(); lpic <- "blank.png";erase.lpic<-T}
  if(is.null(rpic)){png("blank.png", 150, 150, "px");dev.off(); rpic <- "blank.png";erase.rpic<-T}
  if(is.null(signature.pic)){png("blank.png", 150, 150, "px");dev.off(); spic <- "blank.png";erase.spic<-T}


df <- read_sheet(url, "Certificado", "no")

if(!(name.column)%in%colnames(df)){stop("Column '", name.column , "' is not a column of ypur Google Sheets document")}
if(!(date.column)%in%colnames(df)){stop("Column '", date.column , "' is not a column of ypur Google Sheets document")}
if(!(title.column)%in%colnames(df)){stop("Column '", title.column , "' is not a column of ypur Google Sheets document")}
if(!(comm.type.column)%in%colnames(df)){stop("Column '", comm.type.column , "' is not a column of ypur Google Sheets document")}
if(!(affiliation.column)%in%colnames(df) &
   certificate=="participation"){stop("Column '", affiliation.column , "' is not a column ofypur Google Sheets document")}


# load either pdf or word certificate template
if(certificate=="attendance"    & language == "english"){template <- readr::read_file("templates/participation_en.Rmd")}
if(certificate=="attendance"    & language == "spanish"){template <- readr::read_file("templates/participation_es.Rmd")}
if(certificate=="participation" & language == "english"){template <- readr::read_file("templates/attendance_en.Rmd")}
if(certificate=="participation" & language == "spanish"){template <- readr::read_file("templates/attendance_es.Rmd")}


template_cert <- template %>%
  str_replace_all("<<ACTO>>", type) %>%
  str_replace_all("<<GRUPO>>", organiser) %>%
  str_replace_all("<<FIRMANTE>>", signer) %>%
  str_replace_all("<<PUESTO>>", signer.position)%>%
  str_replace_all("<<HORAS>>", hours)

for (i in 1:nrow(df)){

  #replace the placeholder words in the template with the student information


  personal_cert <- template_cert %>%
    str_replace_all("<<Ponente>>", df[i, name.column]) %>%
    str_replace_all("<<Fecha>>", df[i, date.column])%>%
    str_replace_all("<<Titulo>>", df[i, title.column])%>%
    str_replace_all("<<Afiliacion>>", df[i, affiliation.column])%>%
    str_replace_all("<<Tipo>>", df[i, comm.type.column])

  #generate an output file name based on student name
  out_filename = df[i,name.column]
  out_datename = df[i,date.column]
  out_datename <- str_replace_all(out_datename, "/", "_")
  out_file = paste(out_filename, out_datename, sep=("-"))
  out_file_pdf = paste0(out_file, '.pdf')

  #save customized Rmd to a temporary file
  write_file(personal_cert, "tmp.Rmd")

  #create the certificates using R markdown.
  #it will detect the ending of the output file and use the right format
  rmarkdown::render("tmp.Rmd", output_file = out_file_pdf)

  #temporary Rmd file can be deleted.
  file.remove("tmp.Rmd")

}
if(erase.lpic){file.remove("blank.png")}
if(erase.rpic){file.remove("blank.png")}
}
