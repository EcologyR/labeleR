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
create_herbarium_label <- function(data=data,
                                   title=NULL,
                                 subtitle=NULL,
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
                                 date.column=NULL
                                 ){

  if(is.null(data)){
    stop(" a 'data' data.frame must be provided.
         To import from Google Sheets use function 'read_sheet()'")
  }

  if(is.null(title)){
    message("No title provided")
    title <- ""}

  if(is.null(subtitle)){
    message("No subtitle provided")
    subtitle <- ""}

  if(is.null(family.column)){
    family.column<-""
  }
  if(!(family.column) %in% c("",colnames(data))) {
    stop("Column '", name.column ,
         "' is not a column of your Google Sheets document. Please select from \n",
         paste0("-", colnames(data), sep="\n"))
  }
  data[,family.column] <- toupper(data[,family.column])


  if(is.null(taxon.column)){
    taxon.column<-""
  }
  if(!(taxon.column) %in% c("",colnames(data))) {
    stop("Column '", taxon.column ,
         "' is not a column of your Google Sheets document. Please select from \n",
         paste0("-", colnames(data), sep="\n"))
  }

  if(is.null(author.column)){
    author.column<-""
  }
  if(!(author.column) %in% c("",colnames(data))) {
    stop("Column '", author.column ,
         "' is not a column of your Google Sheets document. Please select from \n",
         paste0("-", colnames(data), sep="\n"))
  }

  if(is.null(det.column)){
    det.column<-""
  }
  if(!(det.column) %in% c("",colnames(data))) {
    stop("Column '", det.column ,
         "' is not a column of your Google Sheets document. Please select from \n",
         paste0("-", colnames(data), sep="\n"))
  }

  if(is.null(date.det.column)){
    det.column<-""
  }
  if(!(date.det.column) %in% c("",colnames(data))) {
    stop("Column '", date.det.column ,
         "' is not a column of your Google Sheets document. Please select from \n",
         paste0("-", colnames(data), sep="\n"))
  }

  if(is.null(location.column)){
    location.column<-""
  }
  if(!(location.column) %in% c("",colnames(data))) {
    stop("Column '", location.column ,
         "' is not a column of your Google Sheets document. Please select from \n",
         paste0("-", colnames(data), sep="\n"))
  }

  if(is.null(area.description.column)){
    area.description.column<-""
  }
  if(!(area.description.column) %in% c("",colnames(data))) {
    stop("Column '", area.description.column ,
         "' is not a column of your Google Sheets document. Please select from \n",
         paste0("-", colnames(data), sep="\n"))
  }
  data[,area.description.column] <- as.character( data[,area.description.column])


  if(is.null(latitude.column)){
    latitude.column<-""
  }
  if(!(latitude.column) %in% c("",colnames(data))) {
    stop("Column '", latitude.column ,
         "' is not a column of your Google Sheets document. Please select from \n",
         paste0("-", colnames(data), sep="\n"))
  }


  if(is.null(longitude.column)){
    longitude.column<-""
  }
  if(!(longitude.column) %in% c("",colnames(data))) {
    stop("Column '", longitude.column ,
         "' is not a column of your Google Sheets document. Please select from \n",
         paste0("-", colnames(data), sep="\n"))
  }


  if(is.null(elevation.column)){
    elevation.column<-""
  }
  if(!(elevation.column) %in% c("",colnames(data))) {
    stop("Column '", elevation.column ,
         "' is not a column of your Google Sheets document. Please select from \n",
         paste0("-", colnames(data), sep="\n"))
  }

  if(is.null(field1.column)){
    field1.column<-""
  }
  if(!(field1.column) %in% c("",colnames(data))) {
    stop("Column '", field1.column ,
         "' is not a column of your Google Sheets document. Please select from \n",
         paste0("-", colnames(data), sep="\n"))
  }

  if(is.null(field2.column)){
    field2.column<-""
  }
  if(!(field2.column) %in% c("",colnames(data))) {
    stop("Column '", field2.column ,
         "' is not a column of your Google Sheets document. Please select from \n",
         paste0("-", colnames(data), sep="\n"))
  }
  if(is.null(field3.column)){
    field3.column<-""
  }
  if(!(field3.column) %in% c("",colnames(data))) {
    stop("Column '", field3.column ,
         "' is not a column of your Google Sheets document. Please select from \n",
         paste0("-", colnames(data), sep="\n"))
  }

  if(is.null(collector.column)){
    collector.column<-""
  }
  if(!(collector.column) %in% c("",colnames(data))) {
    stop("Column '", collector.column ,
         "' is not a column of your Google Sheets document. Please select from \n",
         paste0("-", colnames(data), sep="\n"))
  }
  if(is.null(collection.column)){
    collection.column<-""
  }
  if(!(collection.column) %in% c("",colnames(data))) {
    stop("Column '", collection.column ,
         "' is not a column of your Google Sheets document. Please select from \n",
         paste0("-", colnames(data), sep="\n"))
  }
  if(is.null(assistants.column)){
    assistants.column<-""
  }
  if(!(assistants.column) %in% c("",colnames(data))) {
    stop("Column '", assistants.column ,
         "' is not a column of your Google Sheets document. Please select from \n",
         paste0("-", colnames(data), sep="\n"))
  }
  if(is.null(date.column)){
    date.column<-""
  }
  if(!(date.column) %in% c("",colnames(data))) {
    stop("Column '", date.column ,
         "' is not a column of your Google Sheets document. Please select from \n",
         paste0("-", colnames(data), sep="\n"))
  }


  if(!dir.exists("tmp")){
    dir.create("tmp")
  }

  if(!dir.exists("output")){dir.create("output")}


  tmpl_file   <- "templates/herbarium.Rmd"
  file.copy(tmpl_file, "tmp/herbarium.Rmd", overwrite = T)#create files to call them lpic@rpic to make it homogeneous

  tmpl_file   <- "tmp/herbarium.Rmd"
  out.name <- paste0("Herbarium_labels")
  output_file <- paste0(out.name,'.pdf')

  if(file.exists(paste0("output/",output_file))){message("Herbarium_labels file already exists. Overwriting.")}

  for (i in 1:ncol(data)){
    data[is.na(data[,i]),i]<-""
  }

  rmarkdown::render(
    tmpl_file,
    output_dir = "tmp",
    output_file = output_file,
    params = list(
      title  = title,
      family.i = data[,family.column],
      taxon.i = data[,taxon.column],
      author.i = data[,author.column],
      det.i = data[,det.column],
      date.det.i = data[,date.det.column],
      location.i = data[,location.column],
      area.description.i = data[,area.description.column],
      latitude.i =data[,latitude.column],
      longitude.i =data[,longitude.column],
      elevation.i =data[,elevation.column],
      field1.i =data[,field1.column],
      field2.i =data[,field2.column],
      field3.i =data[,field3.column],
      collector.i =data[,collector.column],
      collection.i =data[,collection.column],
      assistants.i =data[,assistants.column],
      date.i =data[,date.column],
      subtitle= subtitle)
    )


  file.copy(paste0("tmp/",output_file), paste0("output/",output_file), overwrite = T)#create files to call them lpic@rpic to make it homogeneous
  unlink("tmp", recursive = T, force = T)

}
