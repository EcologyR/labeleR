% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/certificate_participation.R
\name{create_certificate_participation}
\alias{create_certificate_participation}
\title{Create certificate of participation}
\usage{
create_certificate_participation(
  data = NULL,
  path = NULL,
  language = c("English", "Spanish"),
  name.column = NULL,
  affiliation.column = NULL,
  comm.type.column = NULL,
  title.column = NULL,
  date.column = NULL,
  type = "",
  event = "",
  freetext = "",
  signer = "",
  signer.role = "",
  signature.pic = NULL,
  lpic = NULL,
  rpic = NULL,
  keep.files = FALSE,
  template = NULL
)
}
\arguments{
\item{data}{A data frame containing participants' names and contributions}

\item{path}{path Character. Path to folder where the PDF certificates will be saved.}

\item{language}{Character. Select 'English' or 'Spanish'.}

\item{name.column}{Character. Name of the column in \code{data} storing participants' name.}

\item{affiliation.column}{Character (optional). Name of the column in \code{data}
storing participants' affiliation}

\item{comm.type.column}{Character. Name of the column in \code{data} reporting
participation type (e.g. poster, oral communication, etc)}

\item{title.column}{Character. Name of the column in \code{data} storing
contribution titles.}

\item{date.column}{Character. Name of the column in \code{data} storing dates of
participation.}

\item{type}{Character (optional). Type of event (conference, workshop, seminar...)}

\item{event}{Character. Title of the event}

\item{freetext}{Character (optional). Free text to insert before the date.
Can include LaTeX commands (see examples).}

\item{signer}{Character. Person who signs the certificate}

\item{signer.role}{Character. Signer's role or position}

\item{signature.pic}{Character (optional) Path to a PNG image to appear in
the bottom, above signer's name.}

\item{lpic}{Character (optional) Path to a PNG image to appear in the top-left.}

\item{rpic}{Character (optional) Path to a PNG image to appear in the top-right.}

\item{keep.files}{Logical. Keep the Rmarkdown template and associated files
in the output folder? Default is FALSE.}

\item{template}{Character (optional) Rmarkdown template to use. If not provided,
using the default template included in \code{labeleR}.}
}
\value{
PDF certificates are saved on disk, in the folder defined
by \code{path}. If \code{keep.files = TRUE}, the Rmarkdown template and PNG logo files
will also appear in the same folder.
}
\description{
Create certificate of participation
}
\examples{
\dontshow{if (interactive()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}

data <- data.frame(
Name = c("Severus Snape", "Minerva McGonagall"),
Date = c("1/1/93", "1/1/94"),
Title = c("Advanced potions with more than six plants",
          "From cat to ape: transformations in the XX century"),
Comm.type = c("oral communication", "poster"),
House = c("Slytherin", "Gryffindor")
)


create_certificate_participation(
data = data,
path = "labeleR_output",
language = "Spanish",
name.column = "Name",
affiliation.column = "House",
comm.type.column = "Comm.type",
title.column = "Title",
date.column = "Date",
type = "online seminar",
event = "Hogwarts School of Witchcraft and Wizardry",
freetext = "which lasted 2 hours",
signer = "A.P.W.B. Dumbledore",
signer.role = "School Headmaster",
lpic = NULL,
rpic = NULL,
signature.pic = NULL
)
\dontshow{\}) # examplesIf}
}
\author{
Julia G. de Aledo, Ignacio Ramos-Gutierrez, Francisco Rodriguez-Sanchez
}
