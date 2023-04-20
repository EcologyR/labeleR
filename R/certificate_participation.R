#' Create certificate of participation
#'
#' @param language Select english or spanish
#' @param data A data frame including information to create the certificate of participation
#' @param type Type of event (conference, workshop, seminar...)
#' @param organiser Name of the organizing entity
#' @param hours Number of hours the event has lasted
#' @param signer Person who credits the certificate
#' @param signer.position Position of the \code{signer}
#' @param lpic PNG object route. File route of the top-left image. Can be blank if set to NULL.
#' @param rpic PNG object route. File route of the top-right image. Can be blank if set to NULL.
#' @param signature.pic PNG object route. File route of a signature image. Can be blank if set to NULL.
#' @param name.column Column name of the Google Sheet column which specifies the participant's name.
#' @param affiliation.column Column name of the Google Sheet column which specifies the participant's affiliation
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
#' data= read_sheet("https://docs.google.com/spreadsheets/u/1/d/11No4aLvta2qxGhkxD7W6HfNfGmO1wpCIDvyRKFF-_gM/edit?usp=drive_web&ouid=106603768357414088091"),
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
    data=NULL,
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
if(!(date.column)%in%colnames(df)){
  stop("Column '", date.column ,
       "' is not a column of your data frame. Please select from \n",
       paste0("-", colnames(df), sep="\n"))
  }
if(!(title.column)%in%colnames(df)){
  stop("Column '", title.column ,
       "' is not a column of your data frame. Please select from \n",
       paste0("-", colnames(df), sep="\n"))
  }
if(!(comm.type.column)%in%colnames(df)){
  stop("Column '", comm.type.column ,
       "' is not a column of your data frame. Please select from \n",
       paste0("-", colnames(df), sep="\n"))
  }
if(!(affiliation.column)%in%colnames(df)){
  stop("Column '", affiliation.column ,
       "' is not a column of your data frame. Please select from \n",
       paste0("-", colnames(df), sep="\n"))
  }



# load either pdf or word certificate template
if(language == "english"){template <- tmpl_file <- "templates/participation_EN.Rmd"}
if(language == "spanish"){template <- tmpl_file <- "templates/participation_ES.Rmd"}

file.copy(tmpl_file, "tmp/participation.Rmd", overwrite = T)#create files to call them lpic@rpic to make it homogeneous

tmpl_file   <- "tmp/participation.Rmd"

 for(i in 1:nrow(df)){

if(language == "english"){out.name <- "Participation"}
if(language == "spanish"){out.name <- "Participacion"}

out.name <- paste0(out.name, "_", df[i,name.column], "_", gsub("/","-",df[i,date.column]))
output_file <- paste0(out.name,'.pdf')

rmarkdown::render(
  tmpl_file,
  output_dir = "tmp",
  output_file = output_file,
  params = list(
    type.i               = type,
    organiser.i          = organiser,
    hours.i              = hours,
    signer.i             = signer,
    signer.position.i    = signer.position,
    name.column.i        = df[i,name.column],
    affiliation.column.i = df[i,affiliation.column],
    date.column.i        = df[i,date.column],
    title.column.i       = df[i,title.column],
    comm.type.column.i   = df[i,comm.type.column]
  )
  )

if(!dir.exists("output")){dir.create("output")}

file.copy(paste0("tmp/",output_file), paste0("output/",output_file), overwrite = T)#create files to call them lpic@rpic to make it homogeneous

}

unlink("tmp", recursive = T, force = T)

}

