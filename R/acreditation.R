library(gsheet)

url <- 'https://docs.google.com/spreadsheets/d/16smXdP-Ehwu1cEmJTbJI1DerIpUrOcD7H5Ni6z9B07M/edit#gid=0'
df          <- gsheet::gsheet2tbl(url)
tmpl_file   <- "templates/acreditation.Rmd"
output_file <- paste0("prueba2",'.pdf')

rmarkdown::render(tmpl_file, output_file = output_file, params = list(
  event        = "INTERNATIONAL CONFERENCE OF MUGGLEOLOGY",
  names        = df$List,
  affiliations = df$Affiliation
))
