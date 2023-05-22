
.onAttach <- function(lib, pkg)
{
  packageStartupMessage(
    cat("Welcome to the labeleR package! Let's create some labels!\n",
           "Amazing! This is just like magic!","\n\n",
           "Package developed by Julia G. de Aledo & Ignacio Ramos-Gutiérrez\n",

           "\U1F426", " TW: " , "@juliagdealedo", "\n",
           "\U1F426", " TW: " , "@iramosgutierrez","\n")
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
    graphics::plot.new()
    grDevices::dev.off()
  }

}
