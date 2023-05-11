#' Function to create accreditation cards in DIN-A7 size
#'
#'

#' )
#
#
#' Title
#'
#' @param data  data frame to create small labels including the specifed column names.
#' @param qr String. Free text or column name to create QR codes. If the specified text IS NOT
#'           a column name, the specified text will be used to create the QR codes in all the labels.
#' @param field1.column column name of the first column to be used for the label.
#' @param field2.column column name of the second column to be used for the label.
#' @param field3.column column name of the third column to be used for the label.
#' @param field4.column column name of the fourth column to be used for the label.
#' @param field5.column column name of the fifth column to be used for the label.
#'
#' @return A pdf file with 16 small labels per page
#'
#' @export
#'
#' @author Julia G. de Aledo, Ignacio Ramos-Gutierrez
#'
#' @examples
#' #
#' data=read_sheet("https://docs.google.com/spreadsheets/d/1Q005BDM0XyUNq5XzGWuvdzgZVMc4KhbYadVzi77h3Xw/edit?usp=sharing")
#'create_herbarium_label()

create_collection_small_label <- function(data=data,
                                   qr=NULL,
                                   field1.column=NULL,
                                   field2.column=NULL,
                                   field3.column=NULL,
                                   field4.column=NULL,
                                   field5.column=NULL
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

  if(!dir.exists("tmp")){
    dir.create("tmp")
  }

  if(!dir.exists("output")){dir.create("output")}


  tmpl_file   <- system.file("rmarkdown/templates/collection_small/sketelon/skeleton.Rmd", package="labeleR")

  file.copy(tmpl_file, "tmp/collection_small.Rmd", overwrite = T)#create files to call them lpic@rpic to make it homogeneous

  tmpl_file   <- "tmp/collection_small.Rmd"
  out.name <- paste0("collection_labels_small")
  output_file <- paste0(out.name,'.pdf')

  if(file.exists(paste0("output/",output_file))){message("Collection_small_labels file already exists. Overwriting.")}

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
      field5.i =data[,field5.column])
  )


  file.copy(paste0("tmp/",output_file), paste0("output/",output_file), overwrite = T)#create files to call them lpic@rpic to make it homogeneous
  unlink("tmp", recursive = T, force = T)

}

