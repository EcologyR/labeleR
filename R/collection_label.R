#' Function to create create collection labels (8 per DIN-A4 page)
#'
#'
#'
#' @param data Data frame including to create labels.
#' @param path Folder path where the output will be printed
#' @param qr String. Free text or column of \code{data} that specifies the text to create the QR code.
#'          If the specified value is not a column name of \code{data}, all the QRs will be equal, and will output the
#'          specified \code{qr}.
#' @param field1.column Column of \code{data} that specifies the text to print in the first field.
#' @param field2.column Column of \code{data} that specifies the text to print in the second field.
#' @param field3.column Column of \code{data} that specifies the text to print in the third field.
#' @param field4.column Column of \code{data} that specifies the text to print in the fourth field.
#' @param field5.column Column of \code{data} that specifies the text to print in the fifth field.
#' @param logo PNG object route. File route of the bottom image. Can be blank if set to NULL.
#' @param bgcolor HTML color to use for the label background. Default is D0ECC1
#' @param textcolor HTML color to use for the label text. Default is 1E3F20
#'
#' @export
#'
#' @author Julia G. de Aledo, Ignacio Ramos-Gutierrez
#'
#' @examples
#' data <- read_sheet("https://docs.google.com/spreadsheets/d/
#'         1Bd_IVgGup4MapTgPq-cqqP05zYl-Q4SfUCBJ5oDSrMs/edit?usp=sharing")
#'
#' \dontrun{
#' create_collection_label(
#' data = data,
#' path = "LabeleR_output",
#' qr = "QR_code",
#' field1.column = "field1",
#' field2.column = "field2",
#' field3.column = "field3",
#' field4.column = "field6",
#' field5.column = "field7"
#' )
#' }
#'
#'
create_collection_label <- function(data=data,
                                          path=NULL,
                                          qr=NULL,
                                          field1.column=NULL,
                                          field2.column=NULL,
                                          field3.column=NULL,
                                          field4.column=NULL,
                                          field5.column=NULL,
                                          logo=NULL,
                                          bgcolor=NULL,
                                          textcolor=NULL
){

  if(is.null(data)){
    stop(" a 'data' data.frame must be provided.
         To import from Google Sheets use function 'read_sheet()'")
  }
  if(class(data)!="data.frame"){stop("The 'data' object must be a data frame.")}

  if(is.null(path)){stop("A folder path must be specified.")}
  if(!file.exists(path)){message("The specified folder does not exist. Creating folder")
    dir.create(path)}

  if(any(apply(data, 1, nchar)>150)){message("Warning: cells containing too long texts may alter the result.
Please consider shortening the content of your cells. ")}


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
         " is not a column of your 'data' object. Please select from \n",
         paste0("-", colnames(data), sep="\n"))
  }
  data[,field1.column] <- toupper(data[,field1.column])

  if(is.null(field2.column)){
    field2.column<-""
  }
  if(!(field2.column) %in% c("",colnames(data))) {
    stop("Column '", field2.column ,
         " is not a column of your 'data' object. Please select from \n",
         paste0("-", colnames(data), sep="\n"))
  }
  if(is.null(field3.column)){
    field3.column<-""
  }
  if(!(field3.column) %in% c("",colnames(data))) {
    stop("Column '", field3.column ,
         " is not a column of your 'data' object. Please select from \n",
         paste0("-", colnames(data), sep="\n"))
  }

  if(is.null(field4.column)){
    field4.column<-""
  }
  if(!(field4.column) %in% c("",colnames(data))) {
    stop("Column '", field4.column ,
         " is not a column of your 'data' object. Please select from \n",
         paste0("-", colnames(data), sep="\n"))
  }

  if(is.null(field5.column)){
    field5.column<-""
  }
  if(!(field5.column) %in% c("",colnames(data))) {
    stop("Column '", field5.column ,
         " is not a column of your 'data' object. Please select from \n",
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

  if(is.null(logo)){
      grDevices::png("tmp/logo.png", 150, 150, "px")
      graphics::par(bg="transparent")
      graphics::plot.new()
      grDevices::dev.off()
    }


  file.copy(logo, "tmp/logo.png", overwrite = T)#create files to call them logo@rpic to make it homogeneous

  tmpl_file   <- system.file("rmarkdown/templates/collection_large/skeleton/skeleton.Rmd", package="labeleR")
  file.copy(tmpl_file, "tmp/collection_large.Rmd", overwrite = T)#create files to call them logo@rpic to make it homogeneous

  tmpl_file   <- "tmp/collection_large.Rmd"
  out.name <- paste0("Collection_labels")
  output_file <- paste0(out.name,'.pdf')

  # if(file.exists(paste0("output/",output_file))){message("Collection_labels_large file already exists. Overwriting.")}

  for (i in 1:ncol(data)){
    data[is.na(data[,i]),i]<-""
  }

  bl.char <- rep("~", times=nrow(data))
  rmarkdown::render(
    tmpl_file,
    output_dir = path,
    output_file = output_file,
    params = list(
      qr.i = data[,qr],
      field1.i =if(field1.column==""){bl.char}else{data[,field1.column]},
      field2.i =if(field2.column==""){bl.char}else{data[,field2.column]},
      field3.i =if(field3.column==""){bl.char}else{data[,field3.column]},
      field4.i =if(field4.column==""){bl.char}else{data[,field4.column]},
      field5.i =if(field5.column==""){bl.char}else{data[,field5.column]},
      bgcolor = bgcolor,
      textcolor = textcolor)
  )


  # file.copy(paste0("tmp/",output_file), paste0("output/",output_file), overwrite = T)#create files to call them logo@rpic to make it homogeneous
   unlink("tmp", recursive = T, force = T)

}

