
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
  df[[column]] <- gsub("\\textit" , "\\textit{"  , df[[column]], fixed = T)#open italize
  df[[column]] <- gsub("\\textbf" , "\\textbf{"  , df[[column]], fixed = T)#open bold
  df[[column]] <- gsub("\\end", "}"  , df[[column]], fixed = T)#close italize AND bold
  return(df[,column, drop=FALSE])
}

check_latex_columns <- function(df= NULL, columns= NULL){
  for (column in columns) {
    df[,column] <- check_latex(df, column)
  }
  return(df)
}

na2blank <- function(x){
  x[is.na(x)] <- "~"
  return(x)}

fill_NAs_df <- function(df = NULL){
  cols <- colnames(df)
  rows <- rownames(df)
  mat <- matrix(apply(df, 2, na2blank), nrow = length(rows), ncol=length(cols))
  df <- as.data.frame(mat)
  colnames(df) <- cols
  rownames(df) <- rows

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


#Function to change a name between brackets
check_file_name <- function(name, suffix, path){

  if(!file.exists(paste0(path, "/", name, suffix))){
    newname <- name
  }else{
    files <- list.files(path, pattern = name)
    if(length(files)==1){
      newname <- paste0(name, "(2)")
    }else{
      files <- gsub(name, "", files)
      files <- gsub(suffix, "", files)
      files <- files[files!=""]
      files <- gsub("\\(", "", files)
      files <- gsub("\\)", "", files)
      num <- max(as.numeric(files))
      newname <- paste0(name, "(",num+1 ,")")
    }
  }
  return(newname)
}

#### Function to set the SMTP server
sendmail_setup <- function(email.column, email.info){

  if( is.null(email.column) & !is.null(email.info)){
    stop("You must specify the email column")
  }

  if(!is.null(email.column) & is.null(email.info)){
    stop("You must specify an email.info object.\nUse function 'email_configuration()' to create it.")
  }

  if(!is.null(email.info) &
     (!inherits(email.info, "list") |
      !(("user") %in% names(email.info)) |
      !(("app.name") %in% names(email.info)))){
    stop("'email.info' should be a list object created using 'email_configuration()' function")
  }

  if(!is.null(email.column) & !is.null(email.info)){
    sendmail <- TRUE
  }else {
    sendmail <- FALSE
  }

  if (isTRUE(sendmail)) {
    sendmail <- utils::askYesNo("You are trying to send automatically certificates via email. Are you sure you want to continue with the automatic sending?",
                                default = FALSE,
                                prompts = getOption("askYesNo", gettext(c("Yes", "No", "Cancel"))))
    if(is.na(sendmail)){stop("Cancel button selected. Aborting.")}
  }

  if(isTRUE(sendmail)){


    blastula::create_smtp_creds_key(id = email.info$app.name,
                                    provider = "gmail",
                                    user = email.info$user,
                                    overwrite = T)
    credentials <- blastula::view_credential_keys()
    credentials <- credentials[credentials$username == email.info$user &
                                 credentials$id == email.info$app.name,]

  }

  return(sendmail)
}

#### Function to send a mail and atachment within the loop

send_mail <- function(data, row, email.info,
                     name.column, email.column,
                     attachment){
  mail.to <- data[row,email.column]

  if(is.na(mail.to)){message("email not sent to ", data[row, name.column])}#se puede mandar un auto mensaje??
  if(!is.na(mail.to)){

    mail.from <- email.info$user
    mail.subj <- email.info$subject
    mail.body <- email.info$body
    mail.cc   <- email.info$cc
    mail.bcc   <- email.info$bcc

    if(is.null(mail.subj)){mail.subj <- paste0("Certificate - ", data[row, name.column])}

    if(is.null(mail.body)){mail.body <- paste0("Certificate for ", data[row, name.column],".\n\n",
                                               "This certificate was automatically sent by labeleR using 'blastula'")}


    email <- blastula::compose_email(
      body = blastula::md(mail.body),
      footer = blastula::md(
        "Mail sent on automatically using labeleR.\r\n
https://ecologyr.github.io/labeleR/"))
    email <- blastula::add_attachment(email, file = attachment)


    blastula::smtp_send(
      email,
      credentials = blastula::creds_key(email.info$app.name),
      to = mail.to,
      from = mail.from,
      subject = mail.subj,
      cc = mail.cc,
      bcc = mail.bcc)


  }}



