

#' Setup the email server
#'
#' Create the email.info object to feed the certificate creation functions
#'
#' @param user Character. Email account used to send certificates.
#' @param app.name Name of the mail application used to send emails. To create one, access `https://myaccount.google.com/apppasswords`
#' @param subject Character. Subject of the email to be sent If not specified, labeleR will use a default value.
#' @param body Character. Body text of the email to be sent. If not specified, labeleR will use a default value.
#' @param cc Character. String (or vector of strings) containing the email addresses to send the email as a copy.
#' @param bcc Character. String (or vector of strings) containing the email addresses to send the email as a hidden copy.
#'
#' @return A list including at least a 'user' string' and an 'app.name' string. Optionally, slots 'subject',
#' 'body', 'cc' and 'bcc' can be edited to compile the email to send.
#'
#' @export
#'
#' @author Ignacio Ramos-Gutierrez, Julia G. de Aledo, Jimena Mateo-Martín, Francisco Rodriguez-Sanchez
#'
email_configuration <- function(user, app.name =NULL, subject = NULL, body = NULL, cc = NULL, bcc = NULL){

  if (!requireNamespace("keyring", quietly = TRUE)) {
    stop("For automatically sending emails, the `keyring` package must be installed.\n",
         "Please run install.packages(\"keyring\")")
  }


  credentials <- blastula::view_credential_keys()
  credentials <- credentials[credentials$username == user,]

  if(!is.null(app.name)){
    blastula::create_smtp_creds_key(id = app.name,
                                    provider = "gmail",
                                    user = user,
                                    overwrite = T)
    credentials <- blastula::view_credential_keys()
    credentials <- credentials[credentials$username == user,]
    credentials <- credentials[credentials$id == app.name,]
  }



  if(nrow(credentials) == 0 | is.null(app.name) ){
    cat(
      "You must first create a mail sending application\n(dont't worry, it is very easy, and is necessary only the first time!).\n\n",

      "- First access this link using the specified  mail user (R will open it for you):
      https://myaccount.google.com/apppasswords \n\n",

      "- Create an application.\n\n",

      "- Save the password anywhere safe, as you will be asked for it later!\n\n")

    utils::browseURL("https://myaccount.google.com/apppasswords")

    stop("Please run again this function specifying your user and application name")

  }


  email.info.ret <- list(
    "user" = user,
    "app.name" = app.name,
    "subject" = subject,
    "body" = body,
    "cc"=cc,
    "bcc" = bcc
  )

  return(email.info.ret)



}
