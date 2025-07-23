#' Create multichoice exam
#'
#' Creates a multichoice exam, with 4 questions per page (first one has title plus 3 questions).
#'
#' @param data a data frame including the questions and choices, and may include a column with
#' adjacent image paths.
#' @param path Character. Path to folder where the PDF file will be saved.
#' @param filename Character. Filename of the pdf. If NULL, default is "multichoice".
#' @param title Title of the exam. If different models are rendered, they will be shown too.
#' @param question.column Character. Name of the column in `data` storing the question to be shown.
#' @param option1.column Character. Name of the column in `data` storing the CORRECT answer. It will not
#' always appear on the first place; options will be randomly sorted.
#' @param option2.column Character. Name of the column in `data` storing the second choice (INCORRECT).
#' @param option3.column Character (optional for 2 choice questions). Name of the column in `data`
#' storing the third choice (INCORRECT).
#' @param option4.column Character (optional for 3 choice questions). Name of the column in `data`
#' storing the fourth choice (INCORRECT).
#' @param image.column Character (optional). Name of the column containing file paths to an image
#'  which may appear by the side of the question.
#' @param solutions Logical. Whether or not a PDF with the solutions should be rendered.
#' @param start Numeric. Number of the first question (useful if exams are split in several parts).
#' @param seeds Numeric vector of undefined length. Seeds to randomize question order and choice order.
#' If the length of `seeds` is longer than one, a multiple choice exam will be rendered for EACH ONE specified.
#' @param frontpage Character (optional). Path to PDF file to be inserted before the document
#' (as front page and/or instructions).
#' @inheritParams create_badge
#'
#' @details
#' **font**
#' Not all fonts are able to be used. Consider only those which are stated to be 'Part of TeX Live', and have OTF and TT available. Additionally, fonts whos 'Usage'
#' differs from `\normalfont`, `\itshape` and `\bfseries` usually fail during installation and/or rendering.
#' Several fonts tried and working are:
#' - libertinus
#' - accanthis
#' - Alegreya
#' - algolrevived
#' - almendra
#' - antpolt
#' - Archivo
#' - Baskervaldx
#' - bitter
#' - tgbonum
#' - caladea
#' - librecaslon
#' - tgchorus
#' - cyklop
#' - forum
#' - imfellEnglish
#' - LobsterTwo
#' - quattrocento
#'
#' @return A PDF file is saved on disk, in the folder defined
#' by `path`. If `keep.files = TRUE`, an RMarkdown file will also appear in the same folder.
#'
#' @export
#'
#' @author  Ignacio Ramos-Gutierrez, Jimena Mateo-Martín, Julia G. de Aledo, Francisco Rodriguez-Sanchez
#'
#' @examplesIf interactive()
#' create_multichoice(
#' data = multichoice.table,
#' path = "labeleR_output",
#' filename = "example_exam",
#' title = "Example test",
#' question.column = "question",
#' option1.column = "opt1.correct",
#' option2.column = "opt2",
#' option3.column = "opt3",
#' option4.column = "opt4",
#' font = "libertinus",
#' start = 1,
#' solutions=T,
#' seeds = c(1:2)
#' )
#'
create_multichoice <- function(data = NULL,
                               path = NULL,
                               filename = NULL,
                               title = NULL,
                               question.column = NULL,
                               option1.column = NULL,
                               option2.column = NULL,
                               option3.column = NULL,
                               option4.column = NULL,
                               image.column = NULL,
                               solutions = F,
                               start = 1,
                               seeds = NULL,
                               frontpage = NULL,
                               font = NULL,
                               keep.files = FALSE,
                               template = NULL) {

  ## Check arguments

  if (is.null(data)) {
    stop("Please provide a data.frame or tibble.")
  }

  # if ((!(all(class(data) == "data.frame"))) & any(class(data) == "data.frame")) {data <- as.data.frame(data)}
  data <- as.data.frame(data)
  if (!inherits(data, "data.frame")) {stop("The 'data' object must be a data frame.")}
  data <- fill_NAs_df(data)

  if (is.null(path)) {stop("A folder path must be specified.")}
  if (!file.exists(path)) {
    message("The specified folder does not exist. Creating folder")
    dir.create(path)
  }

  if (is.null(filename)) {
    message("No file name provided")
    filename <- "Exam"
  }

  if(is.null(font)){
    font <- ""
  }else{
    font <- as.character(font)
    if(length(font)!= 1){
      stop("Font length should be 1")
    }
  }

  if (any(apply(data, 1, nchar) > 150)) {
    message("Too long texts may give undesired results. Please consider shortening long fields.")
  }

  if (!(is.null(option4.column)) & is.null(option3.column)) {
    stop("Please use option3.column instead of option4.column for a three-option multiple choice")
  }
  n.opt <- 2
  if (!is.null(option3.column)) {n.opt <- 3}
  if (!is.null(option4.column)) {n.opt <- 4}

  if (is.null(title)) {
    message("No title provided")
    title <- ""
  }

  ## Check columns are in data or create empty characters if NULL

  question.column <- check_column_or_create_empty_char(data, question.column)
  option1.column <-  check_column_or_create_empty_char(data, option1.column )
  option2.column <-  check_column_or_create_empty_char(data, option2.column )
  option3.column <-  check_column_or_create_empty_char(data, option3.column )
  option4.column <-  check_column_or_create_empty_char(data, option4.column )
  image.column <-    check_column_or_create_empty_char(data, image.column   )

  arguments <- c(question.column,option1.column,option2.column,
                 option3.column, option4.column,image.column)
  arguments <- arguments[arguments != ""]

  data <- check_latex_columns(data, arguments)




  ## Keep intermediate files? If no, using tempdir for intermediate files
  if (!isTRUE(keep.files)) {
    folder <- tempdir()
  } else {
    folder <- path  # all files will remain there
  }

  if (!is.null(frontpage)) {
    if (!file.exists(frontpage)) {stop("Specified PDF does not exist")}
    file.copy(from =  frontpage,
              to = file.path(folder, "frontpage.pdf"),
              overwrite = TRUE)
  }
  if (is.null(frontpage) & file.exists(paste0(folder,"/frontpage.pdf"))) {
    file.remove(paste0(folder,"/frontpage.pdf"))
  }

  ## Defining Rmd template to use
  if (is.null(template)) {
    # use pkg default
    file.copy(
      from = system.file("rmarkdown/templates/multichoice/skeleton/skeleton.Rmd", package = "labeleR"),
      to = file.path(folder, "multichoice.Rmd"),
      overwrite = TRUE
    )
  } else {
    stopifnot(file.exists(template))
    file.copy(
      from = template,
      to = file.path(folder, "multichoice.Rmd"),
      overwrite = TRUE
    )
  }

  #Randomize order

  if (is.null(seeds)) {stop("Please set at least one seed to start the randomization process.\n",
                           "Remember or store this number anywhere for reproducibility")}
  if (any(duplicated(seeds))) {stop("Please do not include duplicate seeds.")}


  for (seed in seeds) {

    model <- LETTERS[which(seeds==seed)]

    set.seed(seed)
    olddata <- data
    datasols <- data

    order <- sample(1:nrow(data), nrow(data), replace = FALSE)
    data <- data[order,]
    datasols <- datasols[order,]
    datasols[,2] <- paste0("\\underline{",datasols[,2],"}")


    for (i in 1:nrow(data)){
      set.seed(seed+i)
      data[i,2:(n.opt+1)] <- data[i,1+(sample(1:(n.opt), n.opt, replace = F ))]
      set.seed(seed+i)
      datasols[i,2:(n.opt+1)] <- datasols[i,1+(sample(1:(n.opt), n.opt, replace = F ))]
    }



    if (image.column != "") {
      for(i in 1:nrow(data)){
        if(data[i, image.column] != "-"){
          use_image(data[i, image.column], name = paste0("image",i), folder)
        }else{
          use_image(NULL, name = paste0("image",i), folder)
        }}
    }else{  ## if image.column was null, create blank image
      for(i in 1:nrow(data)){
        use_image(NULL, name = paste0("image",i), folder)
      }
    }





    ## Render exam
    output_file <- paste0(filename,'.pdf')
    if(length(seeds)>1){output_file <- paste0(filename,"_",model,'.pdf')}
    data <- as.data.frame(data) ## to exploit drop = TRUE when selecting cols below
    bl.char <- rep("~", times = nrow(data))

    rmarkdown::render(
      input = file.path(folder, "multichoice.Rmd"),
      output_dir = path,
      output_file = output_file,
      params = list(
        model     = if (length(seeds)   ==  1) {bl.char} else {rep(model, times=nrow(data))},
        title     = if (title           == "") {bl.char} else {title},
        start     = start,
        question.i= if (question.column == "") {bl.char} else {data[,question.column]},
        option1.i = if (option1.column  == "") {bl.char} else {data[,option1.column]},
        option2.i = if (option2.column  == "") {bl.char} else {data[,option2.column]},
        option3.i = if (option3.column  == "") {bl.char} else {data[,option3.column]},
        option4.i = if (option4.column  == "") {bl.char} else {data[,option4.column]},
        image.i = if (image.column  == "") {bl.char} else {data[,image.column]},
        font = font
      )
    )



    if (isTRUE(solutions)) {

      output_file <- paste0(filename,'_solutions.pdf')
      if(length(seeds)>1){output_file <- paste0(filename,"_",model,'_solutions.pdf')}
      datasols <- as.data.frame(datasols) ## to exploit drop = TRUE when selecting cols below
      bl.char <- rep("~", times = nrow(datasols))

      rmarkdown::render(
        input = file.path(folder, "multichoice.Rmd"),
        output_dir = path,
        output_file = paste0(output_file),
        params = list(
          model     = if (length(seeds)   ==  1) {bl.char} else {rep(model, times=nrow(data))},
          title     = if (title           == "") {bl.char} else {title},
          start     = start,
          question.i= if (question.column == "") {bl.char} else {datasols[,question.column]},
          option1.i = if (option1.column  == "") {bl.char} else {datasols[,option1.column]},
          option2.i = if (option2.column  == "") {bl.char} else {datasols[,option2.column]},
          option3.i = if (option3.column  == "") {bl.char} else {datasols[,option3.column]},
          option4.i = if (option4.column  == "") {bl.char} else {datasols[,option4.column]},
          image.i = if (image.column  == "") {bl.char} else {data[,image.column]},
          font = font
        )
      )
    }
  }


}



