#' Create herbarium labels
#'
#' Create herbarium labels (4 per DIN-A4 page)
#'
#' @param data a data frame. Each row contains the information by species that will appear in the label.
#' @param path Character. Path to folder where the PDF file will be saved.
#' @param title Main title at the top of the labels. Can be blank if set to NULL.
#' @param subtitle Subtitle at the bottom of the labels. Can be blank if set to NULL.
#' @param qr String. Free text or column of \code{data} that specifies the text to create the QR code.
#'          If the specified value is not a column name of \code{data}, all the QRs will be equal,
#'          and will output the specified \code{qr}.
#' @param family.column Character (optional). Name of the column in `data` storing the family of the taxon.
#' @param taxon.column Character (optional). Name of the column in `data` storing the taxons' name.
#' @param author.column Character (optional). Name of the column in `data` storing the taxons' author.
#' @param det.column Character (optional). Name of the column in `data` storing the determiner of the voucher.
#' @param date.det.column Character (optional). Name of the column in `data` storing the date when the voucher
#' was determined.
#' @param location.column Character (optional). Name of the column in `data` storing where the voucher
#' was collected.
#' @param area.description.column Character (optional). Name of the column in `data` storing the decription
#' of the area.
#' @param latitude.column Character (optional). Name of the column in `data` storing the latitude
#' where the specimen was collected.
#' @param longitude.column Character (optional). Name of the column in `data` storing the longitude
#' where the specimen was collected.
#' @param elevation.column Character (optional). Name of the column in `data` storing the elevation
#' where the specimen was collected.
#' @param field1.column Character (optional). Name of the column in `data` storing the first free text to
#' appear at the top of the label.
#' @param field2.column Character (optional). Name of the column in `data` storing the second free text to
#' appear below field1.
#' @param field3.column Character (optional). Name of the column in `data` storing the second free text to
#' appear below field2.
#' @param collector.column Character (optional). Name of the column in `data` storing the name of the collector.
#' @param collection.column Character (optional). Name of the column in `data` storing the voucher's collection
#' number.
#' @param assistants.column Character (optional). Name of the column in `data` storing the names of the
#' collector's assistants.
#' @param date.column Character (optional). Name of the column in `data` storing the date when the specimen
#' was collected.
#'
#' @return A pdf file with four herbarium labels per page within an 'output' folder.
#'
#' @export
#'
#' @author Ignacio Ramos-Gutiérrez, Julia G. de Aledo, Francisco Rodríguez-Sánchez
#'
#' @examplesIf interactive()
#'
#' create_herbarium_label (
#' data = herbarium.table,
#' path = "LabeleR_output",
#' title = "Magical flora of the British Isles",
#' subtitle = "Project: Eliminating plant blindness in Hogwarts students",
#' qr = "QR_code",
#' family.column ="Family",
#' taxon.column = "Taxon",
#' author.column = "Author",
#' det.column = "det",
#' date.det.column = "Det_date",
#' location.column = "Location",
#' area.description.column = "Area_description",
#' latitude.column = "Latitude",
#' longitude.column = "Longitude",
#' elevation.column = "Elevation",
#' field1.column = "life_form",
#' field2.column = "Observations",
#' field3.column = "Height",
#' collector.column = "Collector",
#' collection.column = "Collection_number",
#' assistants.column = "Assistants",
#' date.column = "Date"
#' )

create_herbarium_label <- function(data=data,
                                   path=NULL,
                                   title=NULL,
                                   subtitle=NULL,
                                   qr=NULL,
                                   family.column=NULL,
                                   taxon.column=NULL,
                                   author.column=NULL,
                                   det.column=NULL,
                                   date.det.column=NULL,
                                   location.column=NULL,
                                   area.description.column=NULL,
                                   latitude.column=NULL,
                                   longitude.column=NULL,
                                   elevation.column=NULL,
                                   field1.column=NULL,
                                   field2.column=NULL,
                                   field3.column=NULL,
                                   collector.column=NULL,
                                   collection.column=NULL,
                                   assistants.column=NULL,
                                   date.column=NULL,
                                   keep.files = FALSE,
                                   template = NULL
                                 ){

  if(is.null(data)){
    stop(" a 'data' data.frame must be provided.")
  }
  if (!inherits(data, "data.frame")) {stop("The 'data' object must be a data frame.")}

  if (is.null(path)) {stop("A folder path must be specified.")}
  if (!file.exists(path)) {
    message("The specified folder does not exist. Creating folder")
    dir.create(path)
  }

  if(any(apply(data, 1, nchar)>150)){
  message("Warning: cells containing too long texts may alter the result.
           Please consider shortening the content of your cells.")
    }

  if(is.null(title)){
    message("No title provided")
    title <- ""}

  if(is.null(subtitle)){
    message("No subtitle provided")
    subtitle <- ""}

  if(!is.null(qr)){
  if(!(qr %in% colnames(data))){
    message("QR value is not a column. Using string to create QR codes")
    data$qr <- qr
    qr <- "qr"
    data[,qr]<- as.character (data[,qr])
    }
    }


    if (!is.null(family.column)) {
      check_column_in_df(data, family.column)
      data[,family.column] <- toupper(data[,family.column])
  }


  if(is.null(taxon.column)){
    taxon.column<-""
  }
    check_column_in_df(data, taxon.column)

  if(is.null(author.column)){
    author.column<-""
  }
    check_column_in_df(data, author.column)

  if(is.null(det.column)){
    det.column<-""
  }
    check_column_in_df(data, det.column)

  if(is.null(date.det.column)){
    det.column<-""
  }
    check_column_in_df(data, date.det.column)

  if(is.null(location.column)){
    location.column<-""
  }
    check_column_in_df(data, location.column)

  if(is.null(area.description.column)){
    area.description.column<-""
  }
    check_column_in_df(data, area.description.column)

  data[,area.description.column] <- as.character( data[,area.description.column])


  if(is.null(latitude.column)){
    latitude.column<-""
  }
  check_column_in_df(data, latitude.column)


  if(is.null(longitude.column)){
    longitude.column<-""
  }
  check_column_in_df(data, longitude.column)


  if(is.null(elevation.column)){
    elevation.column<-""
  }
  check_column_in_df(data, elevation.column)

  if(is.null(field1.column)){
    field1.column<-""
  }
  check_column_in_df(data, field1.column)

  if(is.null(field2.column)){
    field2.column<-""
  }
  check_column_in_df(data, field2.column)

  if(is.null(field3.column)){
    field3.column<-""
  }
  check_column_in_df(data, field3.column)

  if(is.null(collector.column)){
    collector.column<-""
  }
  check_column_in_df(data, collector.column)

  if(is.null(collection.column)){
    collection.column<-""
  }
  check_column_in_df(data, collection.column)

  if(is.null(assistants.column)){
    assistants.column<-""
  }
  check_column_in_df(data, assistants.column)

  if(is.null(date.column)){
    date.column<-""
  }
  check_column_in_df(data, date.column)


  ## Keep intermediate files? If no, using tempdir for intermediate files
  if (!isTRUE(keep.files)) {
    folder <- tempdir()
  } else {
    folder <- path  # all files will remain there
  }

  #### Defining Rmd template to use ####

  if (is.null(template)) { # use pkg default

      file.copy(
        from = system.file("rmarkdown/templates/herbarium/skeleton/skeleton.Rmd", package = "labeleR"),
        to = file.path(folder, "herbarium.Rmd"),
        overwrite = TRUE
      )


  }

  if (!is.null(template)) {
    stopifnot(file.exists(template))
    if (template != file.path(folder, "herbarium.Rmd")) {
      file.copy(
        from = template,
        to = file.path(folder, "herbarium.Rmd"),
        overwrite = TRUE
      )
    }
  }



  tmpl_file   <- "tmp/herbarium.Rmd"
  out.name <- paste0("Herbarium_labels")
  output_file <- paste0(out.name,'.pdf')

   for (i in 1:ncol(data)){
    data[is.na(data[,i]),i]<-"~"
  }

 bl.char <- rep("~", times=nrow(data))

  rmarkdown::render(
    input = file.path(folder, "herbarium.Rmd"),
    output_dir = path,
    output_file = output_file,
    params = list(
      title              = if(is.null(title                  )){bl.char}else{title},
      subtitle           = if(is.null(subtitle               )){bl.char}else{subtitle},
      qr.i               = if(is.null(qr                     )){NULL   }else{data [,qr]},
      family.i           = if(is.null(family.column          )){bl.char}else{data [,family.column]},
      taxon.i            = if((taxon.column           ==""   )){bl.char}else{data [,taxon.column]},
      author.i           = if((author.column          ==""   )){bl.char}else{data [,author.column]},
      det.i              = if((det.column             ==""   )){bl.char}else{data [,det.column]},
      date.det.i         = if((date.det.column        ==""   )){bl.char}else{data [,date.det.column]},
      location.i         = if((location.column        ==""   )){bl.char}else{data [,location.column]},
      area.description.i = if((area.description.column==""   )){bl.char}else{data [,area.description.column]},
      latitude.i         = if((latitude.column        ==""   )){bl.char}else{data [,latitude.column]},
      longitude.i        = if((longitude.column       ==""   )){bl.char}else{data [,longitude.column]},
      elevation.i        = if((elevation.column       ==""   )){bl.char}else{data [,elevation.column]},
      field1.i           = if((field1.column          ==""   )){bl.char}else{data [,field1.column]},
      field2.i           = if((field2.column          ==""   )){bl.char}else{data [,field2.column]},
      field3.i           = if((field3.column          ==""   )){bl.char}else{data [,field3.column]},
      collector.i        = if((collector.column       ==""   )){bl.char}else{data [,collector.column]},
      collection.i       = if((collection.column      ==""   )){bl.char}else{data [,collection.column]},
      assistants.i       = if((assistants.column      ==""   )){bl.char}else{data [,assistants.column]},
      date.i             = if((date.column            ==""   )){bl.char}else{data [,date.column]}
    )
    )

}

