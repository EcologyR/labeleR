
#' Create Book of Abstracts
#'
#' Create a book of abstracts given a dataframe or tibble.
#'
#' @param data a data frame including abstract titles, names, affiliations and abstract texts.
#' @param path Character. Path to folder where the PDF file will be saved.
#' @param filename Character. Filename of the pdf. If NULL, default is "AbstractBook".
#' @param title.column Character. Name of the column in `data` storing abstracts' titles.
#' @param authors.column Character. Name of the column in `data` storing abstract authors' names
#' and affiliations. Numeric (and symbol, e.g. '*') affiliations MUST be specified between brackets.
#' Authors must be separated using a semi-colon (';').
#' @param affiliation.column Character. Name of the column in `data` storing the addresses for each
#' affiliation number (specified between brackets after author names, in `authors.column`).
#' Separations between  authors must be specified using a semi-colon (';').
#' @param text.colum Name of the column in `data` storing the abstract text.
#' @param toc Logical. If TRUE, a Table of Contents will be included.
#' @param toc.title Character. Title to name the Table of Contents. Default is "Index".
#' @param frontpage Character. Path to PDF file to be inserted before the book of abstracts (as front page and/or introduction).
#' @param title.cex Text font size used for the title. Default is 22.
#' @param authors.cex Text font size used for the authors' names. Default is 16.
#' @param affiliations.cex Text font size used for the affiliation addresses Default is 14.
#' @param text.cex Text font size used for the abstract main text body. Default is 14.
#' @param keep.files Logical. Keep the RMarkdown template and associated files
#' in the output folder? Default is FALSE.
#' @param template Character (optional) RMarkdown template to use. If not provided,
#' using the default template included in `labeleR`.
#'
#' @return A PDF file named after `filename` is saved on disk, in the folder defined
#' by `path`. If `keep.files = TRUE`, an RMarkdown will also
#' appear in the same folder.
#'
#' @details You can copy and modify at your convenience  \href{https://docs.google.com/forms/d/1u4SFWDobQrD8AEvKpvPCdwAZiVGd55B2dAtPmPEfU6E/copy}{this Google form template}
#' to retrieve abstract information which will match labeleR's requirements for a straightforward use.
#'
#'
#' @export
#'
#' @author Ignacio Ramos-Gutierrez, Julia G. de Aledo, Jimena Martín-Mateo, Francisco Rodriguez-Sanchez
#'
#' @examplesIf interactive()
#' create_abstractbook(
#' data=abstract.table,
#' path = "labeleR_output",
#' filename = "congress_abstractbook",
#' title.column = "abstract_title",
#' authors.column = "authors",
#' affiliation.column = "affiliation",
#' text.column = "abstract_text",
#' title.cex = 20,
#' authors.cex = 15,
#' affiliations.cex = 14,
#' text.cex = 12,
#' frontpage = "Congress_frontpage.pdf"
#')

create_abstractbook <- function(data = NULL,
                         path = NULL,
                         filename = NULL,
                         title.column = NULL,
                         authors.column = NULL,
                         affiliation.column = NULL,
                         text.column = NULL,
                         frontpage = NULL,
                         toc = TRUE,
                         toc.title = "Index",
                         title.cex = 22 ,
                         authors.cex = 16,
                         affiliations.cex = 14,
                         text.cex = 14,
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

  for(i in seq_len(nrow(data))){
  authors <- data[i,authors.column]
  authors <- gsub("; ", ";", authors)
  authors <- gsub(" ;", ";", authors)
  authors <- unlist(strsplit(authors, split=";"))


    error.open <- which(!(grepl("\\(", authors)))
    error.close<- which(!(grepl("\\)", authors)))

    if(any(error.open)){
    stop("Error in line: ",i,". Unmatched closing bracket in author(s) ",
         paste0(error.open, collapse = " & "))
         }
    if(any(error.close)){
      stop("Error in line: ",i,". Unmatched opening bracket in author(s) ",
           paste0(error.close, collapse = " & "))
    }

    auth.use <- gsub("\\(", "\\\\textsuperscript{", authors)
    auth.use <- gsub("\\)", "}", auth.use)

    auth.use <- paste0(auth.use, collapse = ", ")
    data$auth.use[i] <- auth.use



  affils.unique <-  substr( x = authors, regexpr("\\(", authors)+1,  regexpr("\\)", authors)-1)
  affils.unique <- paste0(affils.unique, collapse = ",")
  affils.unique <- strsplit(affils.unique, split=",")
  affils.unique <- unlist(affils.unique)
  affils.unique <- unique(affils.unique)

  if(any(affils.unique == "*")){
    cor.pos <- which(affils.unique == "*")
    affils.unique <- c(affils.unique[-cor.pos], affils.unique[cor.pos])
  }




  affiliations <- data[i,affiliation.column]
  affiliations <- gsub(affiliations, pattern = "; ", replacement = ";")
  affiliations <- strsplit(affiliations, split=";")
  affiliations <- unlist(affiliations)

  if(!(length(affils.unique) == length(affiliations))) {
    stop("Error in line: ",i, ". There must be the same amount of affiliations as affiliation names.
         Please check opening and closing brackets, and that you have used semicolons (';') as separators.")
  }

  if(any(substr(affiliations, 1,1) == "*")){
    cor.pos <- which(substr(affiliations, 1,1) == "*")
    # if(length(cor.pos) > 1){message("Please specify ")}
    affiliations <- c(affiliations[-cor.pos], affiliations[cor.pos])
  }

  affil.use <- paste0("\\item ",affiliations, collapse="")
  data$affil.use[i] <- affil.use
  rm(authors, auth.use, affils.unique, affiliations, affil.use)
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

  if(!is.null(frontpage)){
    if(!file.exists(frontpage)){stop("Specified PDF does not exist")}
    file.copy(from =  frontpage,
              to = file.path(folder, "frontpage.pdf"),
              overwrite = TRUE)
  }
  if(is.null(frontpage) & file.exists(paste0(folder,"/frontpage.pdf"))){
    file.remove(paste0(folder,"/frontpage.pdf"))
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
      title.i           = data[, title.column   ],
      toc               = toc,
      toc.title         = toc.title,
      authors.i         = data[,"auth.use" ],
      affiliations.i    =  data[,"affil.use"],
      text.i            = if (text.column        == "") {"~"} else {data[, text.column       ] },
      title.cex         = as.character(rep(title.cex        , times = nrow(data))) ,
      authors.cex       = as.character(rep(authors.cex      , times = nrow(data))) ,
      affiliations.cex  = as.character(rep(affiliations.cex , times = nrow(data))) ,
      text.cex          = as.character(rep(text.cex         , times = nrow(data)))

    )
  )

}



