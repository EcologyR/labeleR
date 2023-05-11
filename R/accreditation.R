#' Function to create accreditation cards in DIN-A7 size
#'
#' @param data a data frame including names and affiliations (optional if \code{affiliation.column} is NULL) to create accreditation cards.
#' @param event Title of the event
#' @param name.column Column name of the \code{data} data frame which specifies the participant's name.
#' @param affiliation.column Column name of the \code{data} data frame which specifies the participant's affiliation.
#' @param lpic PNG object route. File route of the top-left image. Can be blank if set to NULL.
#' @param rpic PNG object route. File route of the top-right image. Can be blank if set to NULL.
#'
#' @return An 'output' folder with the PDF documents inside
#'
#' @export
#'
#' @author Julia G. de Aledo, Ignacio Ramos-Gutierrez
#'
#' @examples
#' data <- read_sheet(url='https://docs.google.com/spreadsheets/d/16smXdP-Ehwu1cEmJTbJI1DerIpUrOcD7H5Ni6z9B07M/edit#gid=0')
#' create_accreditation(data=data,
#' event="INTERNATIONAL CONFERENCE OF MUGGLEOLOGY",
#' name.column = "List",
#' affiliation.column="Affiliation",
#' lpic = NULL,
#' rpic = NULL)
create_accreditation <- function(data=NULL,
                                 event=NULL,
                                 name.column=NULL,
                                 affiliation.column=NULL,
                                 lpic=NULL,
                                 rpic=NULL){

  if(is.null(data)){
    stop(" a 'data' data.frame must be provided.
         To import from Google Sheets use function 'read_sheet()'")
    }
  if(is.null(event)){
    message("No event provided")
    event <- ""}
  if(!(name.column)%in%colnames(data)){
    stop("Column '", name.column ,
         "' is not a column of your Google Sheets document. Please select from \n",
         paste0("-", colnames(data), sep="\n"))
    }
  if(is.null(affiliation.column)){
    affiliation.column <- ""
    }
  if(!(affiliation.column)%in%c(colnames(data),"")){
    stop("Column '", affiliation.column ,
         "' is not a column of your Google Sheets document. Please select from \n",
         paste0("-", colnames(data), sep="\n"))
    }


  if(!dir.exists("tmp")){
    dir.create("tmp")
  }
  if(!dir.exists("output")){dir.create("output")}
  erase.lpic <- F
  erase.rpic <- F
  if(is.null(lpic)){
    png("tmp/blank.png", 150, 150, "px")
    plot.new()
    dev.off()
    lpic <- "tmp/blank.png"
    erase.lpic<-T
    }
  if(is.null(rpic)){
    png("tmp/blank.png", 150, 150, "px")
    plot.new()
    dev.off()
    rpic <- "tmp/blank.png"
    erase.rpic<-T
  }


  file.copy(lpic, "tmp/lpic.png", overwrite = T)#create files to call them lpic@rpic to make it homogeneous
  file.copy(rpic, "tmp/rpic.png", overwrite = T)#create files to call them lpic@rpic to make it homogeneous
   tmpl_file   <- system.file("rmarkdown/templates/accreditation/sketelon/skeleton.Rmd", package="labeleR")
   file.copy(tmpl_file, "tmp/accreditation.Rmd", overwrite = T)#create files to call them lpic@rpic to make it homogeneous




  tmpl_file   <- "tmp/accreditation.Rmd"
  out.name <- paste0("Accreditations")
  output_file <- paste0(out.name,'.pdf')

  if(file.exists(paste0("output/",output_file))){message("Accreditation file already exists. Overwriting.")}


  rmarkdown::render(
                    tmpl_file,
                    output_dir = "tmp",
                    output_file = output_file,
  params = list(
    event        = event,
    names        = data[,name.column],
    affiliations = data[,affiliation.column]
  ))

  file.copy(paste0("tmp/",output_file), paste0("output/",output_file), overwrite = T)#create files to call them lpic@rpic to make it homogeneous
  unlink("tmp", recursive = T, force = T)

}



