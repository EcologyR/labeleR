% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/collection_tiny.R
\name{create_tinylabel}
\alias{create_tinylabel}
\title{Function to create small collection labels (16 per page)}
\usage{
create_tinylabel(
  data = data,
  qr = NULL,
  path = NULL,
  field1.column = NULL,
  field2.column = NULL,
  field3.column = NULL,
  field4.column = NULL,
  field5.column = NULL
)
}
\arguments{
\item{data}{Data frame including to create labels.}

\item{qr}{String. Free text or column of \code{data} that specifies the text to create the QR code.
If the specified value is not a column name of \code{data}, all the QRs will be equal, and will output the
specified \code{qr}.}

\item{path}{Folder path where the output will be printed}

\item{field1.column}{Column of \code{data} that specifies the text to print in the first field.}

\item{field2.column}{Column of \code{data} that specifies the text to print in the second field.}

\item{field3.column}{Column of \code{data} that specifies the text to print in the third field.}

\item{field4.column}{Column of \code{data} that specifies the text to print in the fourth field.}

\item{field5.column}{Column of \code{data} that specifies the text to print in the fifth field.}
}
\description{
Function to create small collection labels (16 per page)
}
\examples{
\dontrun{
data <- read_sheet("https://docs.google.com/spreadsheets/
        d/1Bd_IVgGup4MapTgPq-cqqP05zYl-Q4SfUCBJ5oDSrMs/edit?usp=sharing")
create_tinylabel(
data = data,
qr = "QR_code",
path = "LabeleR_output",
field1.column = "campo1",
field2.column = "campo2",
field3.column = "campo3",
field4.column = "campo4",
field5.column = "campo5"
)
}

}
\author{
Julia G. de Aledo, Ignacio Ramos-Gutierrez
}
