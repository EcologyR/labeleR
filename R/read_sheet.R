# url <- 'https://docs.google.com/spreadsheets/d/1inkk3_oNvvt8ajdK4wOkSgPoUyE8JzENrZgSTFJEFBw/edit#gid=0'
# select.column <- "Certificado"
# select.value <- "no"
#' Title
#'
#' @param url a valid google sheets url including a table
#' @param select.column column name to filter which rows to include
#' @param select.value if select.column is not NULL, the value of the rows to be selected
#'
#' @return a data frame
#' @export
#'
#' @examples table <- read_sheet(url='https://docs.google.com/spreadsheets/d/1inkk3_oNvvt8ajdK4wOkSgPoUyE8JzENrZgSTFJEFBw/edit#gid=0')
read_sheet <- function (url, select.column=NULL, select.value=NULL) {
  dbase <- gsheet::gsheet2tbl(url)
  dbase <- as.data.frame (dbase)
  dbase[is.na(dbase[,select.column]),select.column] <- "-NA-"
  if(!is.null(select.column)){dbase <- dbase[dbase[,select.column]==select.value,]}
  if(nrow(dbase)==0){stop("No rows selected using '", select.value, "' as select.value and '", select.column, "' as select.column")}
  return(dbase)
}






