

library('readr')
library('dplyr')
library('stringr')
library(gsheet)

# load data, read everything in as a string/character
# Añadir bbdd####
url <- 'https://docs.google.com/spreadsheets/d/16smXdP-Ehwu1cEmJTbJI1DerIpUrOcD7H5Ni6z9B07M/edit#gid=0'
df <- gsheet::gsheet2tbl(url)
# load either pdf or word certificate template
template <- readr::read_file("templates/acreditation.Rmd")

template_cert <- template %>%
  str_replace_all("<<Event>>", "1992-1993 scholar year") %>%
  str_replace_all("<<Organization>>", "Hogwarts school of Witchcraft and Wizardry")

personal_cert <- template_cert
for (i in 1:4){
  personal_cert <- personal_cert %>%
    str_replace_all(paste0("<<List",i,">>"), df$List[i])%>%
    str_replace_all(paste0("<<Affiliation",i,">>"), df$Affiliation[i])
}
  out_file_pdf = paste0("prueba2",'.pdf')

  #save customized Rmd to a temporary file
  write_file(personal_cert, "tmp.Rmd")

  #create the certificates using R markdown.
  #it will detect the ending of the output file and use the right format
  rmarkdown::render("tmp.Rmd", output_file = out_file_pdf)
  file.remove("tmp.Rmd")

