
#' Create Book of Abstracts
#'
#' Create a book of abstracts given a dataframe or tibble.
#'
#' @param data a data frame including abstract titles, names, affiliations and abstract texts.
#' @param path Character. Path to folder where the PDF file will be saved.
#' @param filename Character. Filename of the pdf. If NULL, default is "AbstractBook".
#' @param title.column Character. Name of the column in `data`
#' storing abstracts' titles.
#' @param authors.column Character. Name of the column in `data`
#' storing abstracts' names. Different authors names must be separated using a semi-colon (';').
#' @param affiliation.column Character (optional). Name of the column in `data` storing the numbers
#' or symbols (e.g. '*' for corresponding author) representing each autor affiliations.
#' Different affiliations of each author  must be specified using a comma (',') as separator.
#' Separations between each author's affiliations must be specified using a semi-colon (';').
#' @param affiliation.address.column Name of the column in `data` specifying the address for
#' each affiliation. The number shoud be the same followed in `affiliation.column`.
#' @param text.colum Name of the column in `data` storing the abstract text.
#' @param keep.files Logical. Keep the RMarkdown template and associated files
#' in the output folder? Default is FALSE.
#' @param template Character (optional) RMarkdown template to use. If not provided,
#' using the default template included in `labeleR`.
#'
#' @return A PDF file named after `filename` is saved on disk, in the folder defined
#' by `path`. If `keep.files = TRUE`, an RMarkdown will also
#' appear in the same folder.
#'
#' @export
#'
#' @author Ignacio Ramos-Gutierrez, Julia G. de Aledo, Francisco Rodriguez-Sanchez
#'
#' @examplesIf interactive()

create_abstractbook <- function(data = NULL,
                         path = NULL,
                         filename = NULL,
                         title.column = NULL,
                         authors.column = NULL,
                         affiliation.column = NULL,
                         affiliation.address.column = NULL,
                         text.column = NULL,
                         keep.files = FALSE,
                         template = NULL) {

  ## Check arguments

  if (is.null(data)) {
    stop("Please provide a data.frame or tibble.")
  }

  if ((!(all(class(data) == "data.frame"))) & any(class(data) == "data.frame")) {data <- as.data.frame(data)}
  if (!inherits(data, "data.frame")) {stop("The 'data' object must be a data frame.")}
  data <- fill_NAs_df(data)

  if (is.null(path)) {stop("A folder path must be specified.")}
  if (!file.exists(path)) {
    message("The specified folder does not exist. Creating folder")
    dir.create(path)
  }

  if (is.null(filename)) {
    message("No file name provided")
    filename <- "AbstractBook"
  }

  # if (is.null(event)) {
  #   message("No event provided")
  #   event <- ""
  # }
  #
  if(is.null(title.column)){stop("A title column must be specified")}
  check_column_in_df(data, title.column)
  data[,title.column] <- check_latex(data, title.column)

  if(is.null(authors.column)){stop("An authors column must be specified")}
  check_column_in_df(data, authors.column)
  data[,authors.column] <- check_latex(data, authors.column)

  if (!is.null(affiliation.column)) {
    check_column_in_df(data, affiliation.column)
    data[,affiliation.column] <- check_latex(data, affiliation.column)
  }
  if (is.null(affiliation.column)) {
    affiliation.column <- ""
  }

  if (!is.null(affiliation.address.column)) {
    check_column_in_df(data, affiliation.address.column)
    data[,affiliation.address.column] <- check_latex(data, affiliation.address.column)
  }
  if (is.null(affiliation.address.column)) {
    affiliation.address.column <- ""
  }

  for(i in seq_along(nrow(data))){
  authors <- data[i,authors.column]
  authors <- gsub("; ", ";", authors)
  authors <- gsub(" ;", ";", authors)
  authors <- unlist(strsplit(authors, split=";"))

  affils <- data[i,affiliation.column]
  affils <- gsub("; ", ";", affils)
  affils <- gsub(" ;", ";", affils)
  affils <- unlist(strsplit(affils, split=";"))


  if(!(length(authors) == length(affils))) {
    stop("Authors and affiliations must have the same length.
         Please check you have used semicolons (';') as separators.")
  }


  auth.use <- paste0(authors, "\\textsuperscript{",affils,"}", collapse = ", ")
  data$auth.use[i] <- auth.use



  affils.unique <- paste0(affils, collapse = ",")
  affils.unique <- unique(unlist(strsplit(affils, split=",")))


  aff.names <- data[i,affiliation.address.column]
  aff.names <- gsub("; ", ";", aff.names)
  aff.names <- gsub(" ;", ";", aff.names)
  aff.names <- unlist(strsplit(aff.names, split=";"))

  if(!(length(affils.unique) == length(aff.names))) {
    stop("There must be the same amount of affiliations as affiliation names.
         Please check you have used semicolons (';') as separators.")
  }

  affil.use <- paste0("\\item ", affils.unique, " ", aff.names, collapse="")
  data$affil.use[i] <- affil.use
  }

  if (!is.null(text.column)) {
    check_column_in_df(data, text.column)
    data[,text.column] <- check_latex(data, text.column)
    data[,text.column] <- gsub("\n", "\\\\par ", data[,text.column])
  }
  if (is.null(text.column)) {
    text.column <- ""
  }




  ## Keep intermediate files? If no, using tempdir for intermediate files
  if (!isTRUE(keep.files)) {
    folder <- tempdir()
  } else {
    folder <- path  # all files will remain there
  }


  ## Defining Rmd template to use
  if (is.null(template)) {
    # use pkg default
    file.copy(
      from = system.file("rmarkdown/templates/abstractbook/skeleton/skeleton.Rmd", package = "labeleR"),
      to = file.path(folder, "abstractbook.Rmd"),
      overwrite = TRUE
    )
  } else {
    stopifnot(file.exists(template))
    file.copy(
      from = template,
      to = file.path(folder, "abstractbook.Rmd"),
      overwrite = TRUE
    )
  }



  ## Render
  output_file <- paste0(filename,'.pdf')
  data <- as.data.frame(data)  ## to exploit drop = TRUE when selecting cols below
  rmarkdown::render(
    input = file.path(folder, "abstractbook.Rmd"),
    output_dir = path,
    output_file = output_file,
    params = list(
      title.i        = data[, title.column   ],
      authors.i      = data[,"auth.use" ],
      affiliations.i =  data[,"affil.use"],
      text.i         = if (text.column        == "") {"~"} else {data[, text.column       ]}

    )
  )

}



