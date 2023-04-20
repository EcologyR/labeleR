
.onAttach <- function(lib, pkg)
{
  packageStartupMessage(
    paste0("Welcome to the labeleR package! Let's create some labels!\n\n",
           "Package developed by Julia G. de Aledo & Ignacio Ramos-Gutiérrez\n",

           "\U1F426", " TW: " , "@juliagdealedo", "\n",
           "\U1F426", " TW: " , "@iramosgutierrez","\n")
    )
}

