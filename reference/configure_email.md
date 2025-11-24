# Configure email sending

Configure the email application to automatically send certificates.

## Usage

``` r
configure_email(
  user = NULL,
  app.name = NULL,
  subject = NULL,
  body = NULL,
  cc = NULL,
  bcc = NULL
)
```

## Arguments

- user:

  Character. Gmail account that will be used to send certificates.

- app.name:

  Name of the mail application used to send emails. To create one,
  access <https://myaccount.google.com/apppasswords>.

- subject:

  Character. Subject of the email to be sent. If not specified, labeleR
  will use a default value.

- body:

  Character. Body text of the email to be sent. If not specified,
  labeleR will use a default value.

- cc:

  Character. String (or vector of strings) containing the email
  addresses to send the email as a copy.

- bcc:

  Character. String (or vector of strings) containing the email
  addresses to send the email as a hidden copy.

## Value

A list including at least a 'user' string' and an 'app.name' string.
Optionally, slots 'subject', 'body', 'cc' and 'bcc' can be edited to
compile the email to send.

## Author

Ignacio Ramos-Gutierrez, Julia G. de Aledo, Jimena Mateo-Martín,
Francisco Rodriguez-Sanchez

## Examples

``` r
if (FALSE) { # \dontrun{
email.info <- configure_email(user = 'example@gmail.com')

## If you already have created an application:
email.info <- configure_email(user = 'example@gmail.com', app.name = "emailsend")

} # }


```
