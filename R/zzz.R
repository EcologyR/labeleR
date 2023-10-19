
.onAttach <- function(lib, pkg)
{
  packageStartupMessage(
    cat(" \033[90mWelcome to the\033[39m \033[4m\033[1mlabeleR\033[22m\033[24m \033[90mpackage.\033[39m\n\n",
           "\033[3mPackage developed by Ignacio Ramos-Gutiérrez, Julia G. de Aledo & Francisco Rodríguez-Sánchez\033[23m\n\n",
           "\U1F426","/","\U1D54F", "@iramosgutierrez","\n",
           "\U1F426","/","\U1D54F", "@juliagdealedo", "\n",
           "\U1F426","/","\U1D54F", "@frod_san","\n")
    )
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
    grDevices::png(file.path(folder, paste0(name, ".png")), 150, 150, "px")
    graphics::par(bg="transparent")
    graphics::plot.new()
    grDevices::dev.off()
  }

}
