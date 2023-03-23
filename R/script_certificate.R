#####################################################
# automatically create customized certificates
#####################################################

# load needed packages
library('readr')
library('dplyr')
library('stringr')
library(dplyr)
# load data, read everything in as a string/character
# Añadir bbdd####
url <- 'https://docs.google.com/spreadsheets/d/1uwhi7IROqDcdMEEld7yuyyAoSfuuJUbK2HVlHvUNzFk/edit#gid=0'
df <- read_sheet(url, "Certificado", "no")
df <- df %>% filter(Certificado=="no")


# load either pdf or word certificate template
template <- readr::read_file("templates/certificate_template_pdf.Rmd")
# template <-  readr::read_file("certificate_template_pdf.Rmd")
#run through all students, generate personalized certificate for each
for (i in 1:nrow(df))
{

  #replace the placeholder words in the template with the student information
  template_cert <- template %>%
    str_replace_all(<<ACTO>>, "congreso") %>%
    str_replace_all(<<GRUPO>>, "AEET") %>%
    str_replace_all(<<firmante>>, "Ignacio Ramos") %>%
    str_replace_all(<<puesto>>, "Rey de España")

  personal_cert <- template_cert %>%
    str_replace_all("<<Ponente>>", df$Ponente[i]) %>%
    str_replace_all("<<Fecha>>", df$Fecha[i])%>%
    str_replace_all("<<Título>>", df$Título[i])

  #generate an output file name based on student name
  out_filename = df[i,'Ponente']
  out_datename = df[i,'Fecha']
  out_datename <- gsub("/", "_", out_datename)
  out_file = paste(out_filename, out_datename, sep=("-"))
  out_file_pdf = paste0(out_file, '.pdf')

  #save customized Rmd to a temporary file
  write_file(current_cert, "tmp.Rmd")

  #create the certificates using R markdown.
  #it will detect the ending of the output file and use the right format
  rmarkdown::render("tmp.Rmd", output_file = out_file_pdf)

  #temporary Rmd file can be deleted.
  file.remove("tmp.Rmd")

}
