create_certificate_attendance <- function(
    language =c("spanish", "english"),
    url=NULL,
    type=NULL,
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




  if (language%in%c("sp", "s")){language<- "spanish"}
  if (language%in%c("en", "e")){language<- "english"}
  match.arg(language, c("spanish", "english"),F)

  if(is.null(url)){stop("A valid Google Sheets URL must be specfied")}
  if(is.null(type)){stop("A type of event (conference, workshop, seminar...) must be specfied")}
  if(is.null(organiser)){stop("An organiser must be specfied")}
  if(is.null(signer)){stop("An signer name must be specfied")}
  if(is.null(signer.position)){signer.position <- ""}
  if(is.null(hours)){stop("A number of hours name must be specfied")}
  if(is.null(date)){stop("A date must be specfied")}
  if(is.null(speaker)){stop("A speaker must be specfied")}

  if(!(is.character(hours))) {hours <- as.character(hours)}
  erase.lpic <- F; erase.lpic<-F; erase.spic<-F
  if(is.null(lpic)){png("blank.png", 150, 150, "px");dev.off(); lpic <- "blank.png";erase.lpic<-T}
  if(is.null(rpic)){png("blank.png", 150, 150, "px");dev.off(); rpic <- "blank.png";erase.rpic<-T}
  if(is.null(signature.pic)){png("blank.png", 150, 150, "px");dev.off(); spic <- "blank.png";erase.spic<-T}


  df <- read_sheet(url, "Certificado", "no")

  if(!(name.column)%in%colnames(df)){stop("Column '", name.column , "' is not a column of ypur Google Sheets document")}


  # load either pdf or word certificate template
  if(language == "english"){template <- readr::read_file("templates/assistance_EN.Rmd")}
  if(language == "spanish"){template <- readr::read_file("templates/assistance_ES.Rmd")}


  template_cert <- template %>%
    str_replace_all("<<ACTO>>", type) %>%
    str_replace_all("<<GRUPO>>", organiser) %>%
    str_replace_all("<<FIRMANTE>>", signer) %>%
    str_replace_all("<<PUESTO>>", signer.position)%>%
    str_replace_all("<<HORAS>>", hours)%>%
    str_replace_all("<<Fecha>>", date)%>%
    str_replace_all("<<NOMBRE PONENTE>>", speaker)

  if(!dir.exists("output")){dir.create("output")}

  for (i in 1:nrow(df)){

    #replace the placeholder words in the template with the student information


    personal_cert <- template_cert %>%
      str_replace_all("<<Asistente>>", df[i, name.column])


    #generate an output file name based on student name
    out_filename = df[i,name.column]
    out_datename = df[i,date.column]
    out_datename <- str_replace_all(out_datename, "/", "_")
    out_file = paste(out_filename, out_datename, sep=("-"))
    out_file_pdf = paste0("output/",out_file, '.pdf')

    #save customized Rmd to a temporary file
    write_file(personal_cert, "tmp.Rmd")

    #create the certificates using R markdown.
    #it will detect the ending of the output file and use the right format
    rmarkdown::render("tmp.Rmd", output_file = out_file_pdf)

    #temporary Rmd file can be deleted.
    file.remove("tmp.Rmd")

  }
  if(erase.lpic){file.remove("blank.png")}
  if(erase.rpic & file.exists("blank.png")){file.remove("blank.png")}
  if(erase.spic & file.exists("blank.png")){file.remove("blank.png")}
}

create_certificate_attendance(
  url <- 'https://docs.google.com/spreadsheets/d/1uwhi7IROqDcdMEEld7yuyyAoSfuuJUbK2HVlHvUNzFk/edit#gid=0',
language="sp",type="workshop",organiser="AEET", signer="Darth vader", hours=3,
  signer.position="Tu padre", lpic=NULL, rpic=NULL, signature.pic=NULL ,
  name.column="Ponente",speaker="McGonagall", date="01/01/2021"
)
