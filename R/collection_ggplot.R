#' Function to create create collection labels (4 per DIN-A4 page)
#'
#'
#'
#' @param data Data frame including to create labels.
#' @param qr String. Free text or column of \code{data} that specifies the text to create the QR code.
#'          If the specified value is not a column name of \code{data}, all the QRs will be equal, and will output the
#'          specified \code{qr}.
#' @param field1.column Column of \code{data} that specifies the text to print in the first field.
#' @param field2.column Column of \code{data} that specifies the text to print in the second field.
#' @param field3.column Column of \code{data} that specifies the text to print in the third field.
#' @param field4.column Column of \code{data} that specifies the text to print in the fourth field.
#' @param field5.column Column of \code{data} that specifies the text to print in the fifth field.
#'
#' @export
#'
#' @author Julia G. de Aledo, Ignacio Ramos-Gutierrez
#'
#' @examples
#' data <- read_sheet(“https://docs.google.com/spreadsheets/d/1Bd_IVgGup4MapTgPq-cqqP05zYl-Q4SfUCBJ5oDSrMs/edit?usp=sharing”)
#' create_collection_large_label(
#' data = data,
#' qr = "QR_code",
#' field1.column = "campo1"
#' field2.column = "campo2"
#' field3.column = "campo3"
#' field4.column = "campo4"
#' field5.column = "campo5"
#' )
#'
#'
create_collection_large_label <- function(data=data,
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


  tmpl_file   <- "templates/collection_large.Rmd"
  file.copy(tmpl_file, "tmp/collection_large.Rmd", overwrite = T)#create files to call them lpic@rpic to make it homogeneous

  tmpl_file   <- "tmp/collection_large.Rmd"
  out.name <- paste0("collection_labels_large")
  output_file <- paste0(out.name,'.pdf')

  if(file.exists(paste0("output/",output_file))){message("Collection_small_large file already exists. Overwriting.")}

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

