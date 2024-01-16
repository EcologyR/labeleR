
#### Change special LaTeX symbols

check_latex<- function(df = NULL, column=NULL){
  df[[column]] <- gsub("&", "\\\\&", df[[column]])
  df[[column]] <- gsub("%", "\\\\%", df[[column]])
  # df[,column] <- gsub("$", "\\$", df[,column])#added to all texts at the end
  df[[column]] <- gsub("#", "\\\\#", df[[column]])
  df[[column]] <- gsub("_", "\\\\_", df[[column]]) #document names get truncated
  # df[,column] <- gsub("{", "\\{", df[,column]) #document names get truncated
  df[[column]] <- gsub("}", "\\\\}", df[[column]])
  df[[column]] <- gsub("~", "-"  , df[[column]]) #unable to render with \\~
  df[[column]] <- gsub("\\^", "\\\\^"  , df[[column]])#name truncated and accent to the next letter
  return(df[,column, drop=FALSE])
}

check_latex_columns <- function(df= NULL, columns= NULL){
  for (column in columns) {
    df[,column] <- check_latex(df, column)
  }
  return(df)
}


#### Check columns in data

check_column_in_df <- function(df = NULL, column = NULL) {

  stopifnot(is.character(column))
  if (!(column) %in% colnames(df)) {
    stop("Column '", column ,
         " is not a column in your 'data' object. Please select from \n",
         paste0("- ", colnames(df), sep = "\n"))
  }
}


check_column_or_create_empty_char <- function(df = NULL, column = NULL) {

  if (!is.null(column)) {
    check_column_in_df(df, column)
    out <- column
  } else {
    out <- ""
  }

  out

}


#### Function to use logos/images provided by the user as PNG files,
## or create blank small images if not provided (NULL)

use_image <- function(image = NULL, name = NULL, folder = NULL) {

  ## If logos provided
  if (!is.null(image)) {
    if (!file.exists(image)) {
      stop(image, " file not found")
    } else {
      file.copy(from = image,
                to = file.path(folder, paste0(name, ".png")),
                overwrite = TRUE)
    }
  }

  ## If logos not provided
  if (is.null(image)) {  # create blank image
    oldpar <- graphics::par(no.readonly = TRUE)
    on.exit(suppressWarnings(graphics::par(oldpar)))

    grDevices::png(file.path(folder, paste0(name, ".png")), 150, 150, "px")
    graphics::par(bg = "transparent")
    graphics::plot.new()
    grDevices::dev.off()
  }

}
