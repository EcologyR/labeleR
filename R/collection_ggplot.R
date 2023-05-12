#' Function to create accreditation cards in DIN-A7 size
#'
#' @param title Main title at the top of the labels. Can be blank if set to NULL.
#' @param subtitle Subtitle at the bottom of the labels. Can be blank if set to NULL.
#' @param family.column Column name of the \code{data} data frame which specifies the labels' family name.
#' @param taxon.column Column name of the \code{data} data frame which specifies the labels' taxon.
#' @param author.column Column name of the \code{data} data frame which specifies the taxons' author.
#' @param det.column Column name of the \code{data} data frame which specifies the determiner of the voucher.
#' @param date.det.column Column name of the \code{data} data frame which specifies the date when the voucher was determined.
#' @param area.description.column Column name of the \code{data} data frame which specifies the decription of the area
#' @param latitude.column Column name of the \code{data} data frame which specifies the latitude where the specimen was collected.
#' @param longitude.columnColumn name of the \code{data} data frame which specifies the longitude where the specimen was collected.
#' @param elevation.column  Column name of the \code{data} data frame which specifies the elevation where the specimen was collected.
#' @param field1.column Column name of the \code{data} data frame which specifying a variable of the user's choice. Can be blank if set to NULL.
#' @param field2.column Column name of the \code{data} data frame which specifying a variable of the user's choice. Can be blank if set to NULL.
#' @param field3.column Column name of the \code{data} data frame which specifying a variable of the user's choice. Can be blank if set to NULL.
#' @param collector.column Column name of the \code{data} data frame which specifies the name of the collector of the voucher.
#' @param assistants.column Column name of the \code{data} data frame which specifies the names of the collector's assistants.
#' @param date.column Column name of the \code{data} data frame which specifies the date when the specimen was collected.
#'
#' @return A pdf file with four herbarium labels per page within an 'output' folder
#'
#' @export
#'
#' @author Julia G. de Aledo, Ignacio Ramos-Gutierrez
#'
#' @examples
#' #
#' data=read_sheet("https://docs.google.com/spreadsheets/d/1Q005BDM0XyUNq5XzGWuvdzgZVMc4KhbYadVzi77h3Xw/edit?usp=sharing")
#'create_herbarium_label(
#'data=data,
#' title="Magical flora of the British Isles",
#' subtitle="Project: Eliminating plant blindness in Hogwarts students",
#' family.column="Family",
#' taxon.column="Taxon",
#' author.column="Author",
#' det.column="det/conf",
#' date.det.column="Det_date",
#' location.column="Location",
#' area.description.column="Area_description",
#' latitude.column="Latitude",
#' longitude.column="Longitude",
#' elevation.column="Elevation",
#' field1.column="life_form",
#' field2.column="Observations",
#' field3.column="Height",
#' collector.column="Collector",
#' collection.column="Collection_number",
#' assistants.column="Assistants",
#' date.column="Date"
#' )
#
#
create_collection_large_label <- function(data=data,
                                          qr=NULL,
                                          field1.column=NULL,
                                          field2.column=NULL,
                                          field3.column=NULL,
                                          field4.column=NULL,
                                          field5.column=NULL,
                                          lpic=NULL,
                                          bgcolor=NULL,
                                          textcolor=NULL
){

  if(is.null(data)){
    stop(" a 'data' data.frame must be provided.
         To import from Google Sheets use function 'read_sheet()'")
  }


  if(!is.null(qr)){
    if(!(qr %in% colnames(data))){
      message("QR value is not a column. Using string to create QR codes")
      data$qr <- qr
      qr <- "qr"
      data[,qr]<- as.character (data[,qr])
    }
  }

  if(is.null(field1.column)){
    field1.column<-""
  }
  if(!(field1.column) %in% c("",colnames(data))) {
    stop("Column '", field1.column ,
         "' is not a column of your data. Please select from \n",
         paste0("-", colnames(data), sep="\n"))
  }
  data[,field1.column] <- toupper(data[,field1.column])

  if(is.null(field2.column)){
    field2.column<-""
  }
  if(!(field2.column) %in% c("",colnames(data))) {
    stop("Column '", field2.column ,
         "' is not a column of your data. Please select from \n",
         paste0("-", colnames(data), sep="\n"))
  }
  if(is.null(field3.column)){
    field3.column<-""
  }
  if(!(field3.column) %in% c("",colnames(data))) {
    stop("Column '", field3.column ,
         "' is not a column of your data. Please select from \n",
         paste0("-", colnames(data), sep="\n"))
  }

  if(!(field4.column) %in% c("",colnames(data))) {
    stop("Column '", field4.column ,
         "' is not a column of your data. Please select from \n",
         paste0("-", colnames(data), sep="\n"))
  }

  if(is.null(field5.column)){
    field5.column<-""
  }
  if(!(field5.column) %in% c("",colnames(data))) {
    stop("Column '", field5.column ,
         "' is not a column of your data. Please select from \n",
         paste0("-", colnames(data), sep="\n"))
  }


  if(is.null(bgcolor)){
    bgcolor<-"D0ECC1"
  }

  if(is.null(textcolor)){
    textcolor<-"1E3F20"
  }


  if(!dir.exists("tmp")){
    dir.create("tmp")
  }

  if(!dir.exists("output")){dir.create("output")}

  file.copy(lpic, "tmp/lpic.png", overwrite = T)#create files to call them lpic@rpic to make it homogeneous

  tmpl_file   <- system.file("rmarkdown/templates/collection_large/skeleton/skeleton.Rmd", package="labeleR")
  file.copy(tmpl_file, "tmp/collection_large.Rmd", overwrite = T)#create files to call them lpic@rpic to make it homogeneous

  tmpl_file   <- "tmp/collection_large.Rmd"
  out.name <- paste0("collection_labels_large")
  output_file <- paste0(out.name,'.pdf')

  if(file.exists(paste0("output/",output_file))){message("Collection_labels_large file already exists. Overwriting.")}

  for (i in 1:ncol(data)){
    data[is.na(data[,i]),i]<-""
  }

  rmarkdown::render(
    tmpl_file,
    output_dir = "tmp",
    output_file = output_file,
    params = list(
      qr.i = data[,qr],
      field1.i =data[,field1.column],
      field2.i =data[,field2.column],
      field3.i =data[,field3.column],
      field4.i =data[,field4.column],
      field5.i =data[,field5.column],
      bgcolor = bgcolor,
      textcolor = textcolor)
  )


  file.copy(paste0("tmp/",output_file), paste0("output/",output_file), overwrite = T)#create files to call them lpic@rpic to make it homogeneous
  unlink("tmp", recursive = T, force = T)

}

