#sendmails
library(gmailr)


tk <- gmailr::gm_token_read(
  path = "inst/gmailr/gmailr-token.rds"
)
gmailr::gm_auth_configure(tk$client)

gm_auth(
  email = gm_default_email(),
  path = ,
  subject = NULL,
  scopes = "full",
  cache = gargle::gargle_oauth_cache(),
  use_oob = gargle::gargle_oob_default(),
  token = NULL
)



from <- "Needed? TEMPLATE MAIL SENDER"
to   <- "mail@place.com"
subject <- "labeleR certificate - NEW"
body <- paste0("Certificate of participation",
               "\n\n\n",
               "This certificate was automatically sent by labeleR using 'gmailr'")
attach <- system.file("rmarkdown/pictures/Hogwarts_BnW.png", package = "labeleR")





email <-
  gmailr::gm_mime() |>
  gmailr::gm_to(to) |>
  gmailr::gm_from(from) |>
  gmailr::gm_subject(subject) |>
  gmailr::gm_text_body(body) |>
  gmailr::gm_attach_file(attach)


gmailr::gm_send_message(email)
