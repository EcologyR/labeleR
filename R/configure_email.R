

#' Configure email sending
#'
#' Configure the email application to automatically send certificates.
#'
#' @param user Character. Gmail account that will be used to send certificates.
#' @param app.name Name of the mail application used to send emails.
#' To create one, access <https://myaccount.google.com/apppasswords>.
#' @param subject Character. Subject of the email to be sent. If not specified, labeleR will use a default value.
#' @param body Character. Body text of the email to be sent. If not specified, labeleR will use a default value.
#' @param cc Character. String (or vector of strings) containing the email addresses to send the email as a copy.
#' @param bcc Character. String (or vector of strings) containing the email addresses to send the email as a hidden copy.
#'
#' @return A list including at least a 'user' string' and an 'app.name' string. Optionally, slots 'subject',
#' 'body', 'cc' and 'bcc' can be edited to compile the email to send.
#'
#' @export
#' @examples
#' \dontrun{
#' email.info <- configure_email(user = 'example@@gmail.com')
#'
#' ## If you already have created an application:
#' email.info <- configure_email(user = 'example@@gmail.com', app.name = "emailsend")
#'
#' }
#'
#'
#'
#' @author Ignacio Ramos-Gutierrez, Julia G. de Aledo, Jimena Mateo-Martín, Francisco Rodriguez-Sanchez
#'
configure_email <- function(user = NULL,
                            app.name = NULL,
                            subject = NULL,
                            body = NULL,
                            cc = NULL,
                            bcc = NULL
) {

  if (!requireNamespace("blastula", quietly = TRUE)) {
    stop("For automatically sending emails, the `blastula` package must be installed.\n",
         "Please run install.packages(\"blastula\")")
  }

  if (!requireNamespace("keyring", quietly = TRUE)) {
    stop("For automatically sending emails, the `keyring` package must be installed.\n",
         "Please run install.packages(\"keyring\")")
  }

  if (is.null(user)) {
    user <- readline("Please write here the gmail account you would like to use to send the emails: ")
    user <- gsub(" ", "", user)
  }
  stopifnot(is.character(user))
  if (!grepl("@gmail", user)) {
    stop("Please provide a complete gmail address")
  }


  credentials <- blastula::view_credential_keys()
  credentials <- credentials[credentials$username == user,]

  if (!is.null(app.name)){
    credentials <- credentials[credentials$id == app.name, ]
    if(nrow(credentials) == 0){
      stop( "No application ", app.name, " found for user ", user)
    }
  }

  if (is.null(app.name) ){
    app.exists <- utils::askYesNo("Have you already created an application for this gmail account?", default = FALSE)
    if (!isTRUE(app.exists)) {
      message(
        "You must first create a mail sending application\n(don't worry, it is very easy, and is necessary only the first time!).\n\n",

        "- First access this link using the specified  mail user (R will open it for you):
      https://myaccount.google.com/apppasswords \n\n",

        "- Choose a name for you application.\n\n",

        "- Save the password anywhere safe, as you will be asked for it later!\n\n")

      utils::browseURL("https://myaccount.google.com/apppasswords")

    }

    app.name <- readline("What is your application name? Please write it here: ")
    app.name <- gsub(" ", "", app.name)

  }


  blastula::create_smtp_creds_key(id = app.name,
                                  provider = "gmail",
                                  user = user,
                                  overwrite = TRUE)
  # credentials <- blastula::view_credential_keys()
  # credentials <- credentials[credentials$username == user,]
  # credentials <- credentials[credentials$id == app.name,]

  email.info.ret <- list(
    "user" = user,
    "app.name" = app.name,
    "subject" = subject,
    "body" = body,
    "cc" = cc,
    "bcc" = bcc
  )

  return(email.info.ret)

}
