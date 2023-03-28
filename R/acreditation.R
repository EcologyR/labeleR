
data <- read_sheet(url='https://docs.google.com/spreadsheets/d/16smXdP-Ehwu1cEmJTbJI1DerIpUrOcD7H5Ni6z9B07M/edit#gid=0')


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


  if(!dir.exists("temp")){
    dir.create("temp")
    rm.templ <- F
  }else{
      rm.templ <- T
  }
  if(!dir.exists("output")){dir.create("output")}
  erase.lpic <- F
  erase.rpic <- F
  if(is.null(lpic)){
    png("temp/blank.png", 150, 150, "px")
    dev.off()
    lpic <- "temp/blank.png"
    erase.lpic<-T
    }
  if(is.null(rpic)){
    png("temp/blank.png", 150, 150, "px")
    dev.off()
    rpic <- "temp/blank.png"
    erase.rpic<-T
    }
  file.copy(lpic, "temp/lpic.png")#create files to call them lpic@rpic to make it homogeneous
  file.copy(rpic, "temp/rpic.png")#create files to call them lpic@rpic to make it homogeneous
   tmpl_file   <- "templates/accreditation.Rmd"
   file.copy(tmpl_file, "temp/accreditation.Rmd")#create files to call them lpic@rpic to make it homogeneous




  tmpl_file   <- "temp/accreditation.Rmd"
  out.name <- paste0("accreditations")
  output_file <- paste0(out.name,'.pdf')

  rmarkdown::render(
                    tmpl_file,
                    output_dir = "temp",
                    output_file = output_file,
  params = list(
    event        = event,
    names        = data[,name.column],
    affiliations = data[,affiliation.column]
  ))

  file.copy(paste0("temp/",output_file), paste0("output/",output_file))#create files to call them lpic@rpic to make it homogeneous
  unlink("temp", recursive = T, force = T)

}
create_accreditation(data,
                     event="INTERNATIONAL CONFERENCE OF MUGGLEOLOGY",
                     name.column = "List",
                     affiliation.column="Affiliation",
                     lpic = "templates/MinMagic.png",
                     rpic=NULL)


data <- read_sheet(url='https://docs.google.com/spreadsheets/d/16smXdP-Ehwu1cEmJTbJI1DerIpUrOcD7H5Ni6z9B07M/edit#gid=0')
event="INTERNATIONAL CONFERENCE OF MUGGLEOLOGY"
name.column = "List"
affiliation.column="Affiliation"
lpic = "templates/MinMagic.png"
rpic=NULL
