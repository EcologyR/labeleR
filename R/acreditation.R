library(gsheet)

# Load data, read everything in as a string/character
url <- 'https://docs.google.com/spreadsheets/d/16smXdP-Ehwu1cEmJTbJI1DerIpUrOcD7H5Ni6z9B07M/edit#gid=0'
df          <- gsheet::gsheet2tbl(url)
tmpl_file   <- "templates/acreditation.Rmd"
output_file <- paste0("prueba2",'.pdf')

# Create the certificates using R markdown.
# It will detect the ending of the output file and use the right format.
rmarkdown::render(tmpl_file, output_file = output_file, params = list(
  event = "1992-1993 scholar year",
  organization = "Hogwarts school of Witchcraft and Wizardry",
  names = df$List,
  affiliations = df$Affiliation
))
