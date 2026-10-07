# Create certificate of attendance

Create certificate of attendance (1 certificate per DIN-A4 page)

## Usage

``` r
create_attendance_certificate(
  data = NULL,
  path = NULL,
  filename = NULL,
  language = c("English", "Spanish"),
  name.column = NULL,
  email.column = NULL,
  email.info = NULL,
  type = "",
  title = "",
  date = "",
  hours = "",
  freetext = "",
  signer = "",
  signer.role = "",
  signature.pic = NULL,
  lpic = NULL,
  rpic = NULL,
  font = NULL,
  keep.files = FALSE,
  template = NULL
)
```

## Arguments

- data:

  a data frame containing attendees' names (in `name.column`)

- path:

  Character. Path to folder where the PDF certificates will be saved.

- filename:

  Character. Filename of the pdf. If NULL, default is "Attendance" for
  English, "Asistencia" for Spanish".

- language:

  Character. Select 'English' or 'Spanish'.

- name.column:

  Character. Name of the column in `data` storing attendees' name.

- email.column:

  Character. Name of the column in `data` storing attendees' email
  address to automatically send them their certificates.

- email.info:

  Object created using
  [`configure_email()`](https://ecologyr.github.io/labeleR/reference/configure_email.md)
  function.

- type:

  Character (optional). Type of event (conference, workshop, seminar...)

- title:

  Character. Title of the event

- date:

  Date of the event

- hours:

  Number of hours the event has lasted

- freetext:

  Character (optional). Free text to insert between the event title and
  date. Can include LaTeX commands (see examples).

- signer:

  Character. Person who signs the certificate

- signer.role:

  Character. Signer's role or position

- signature.pic:

  Character (optional). Path to a PNG image to appear in the bottom,
  above signer's name.

- lpic:

  Character (optional). Path to a PNG image to appear in the top-left.

- rpic:

  Character (optional). Path to a PNG image to appear in the top-right.

- font:

  Character. Font face to use. Default is Latin Modern. NOTE: not all
  fonts are supported, so unexpected results may occur. A list of fonts
  is available at <https://tug.org/FontCatalogue/opentypefonts.html>.
  See Details for more information.

- keep.files:

  Logical. Keep the RMarkdown template and associated files in the
  output folder? Default is FALSE.

- template:

  Character (optional) RMarkdown template to use. If not provided, using
  the default template included in `labeleR`.

## Value

PDF certificates are saved on disk, in the folder defined by `path`. If
`keep.files = TRUE`, the RMarkdown template and PNG logo files will also
appear in the same folder.

## Details

**font** Not all fonts can be used. Consider only those which are stated
to be 'Part of TeX Live', and have OTF and TT available. Additionally,
fonts whose 'Usage' differs from `\normalfont`, `\itshape` and
`\bfseries` usually fail during installation and/or rendering.

Several fonts tried that seem to work are:

- libertinus

- accanthis

- Alegreya

- algolrevived

- almendra

- antpolt

- Archivo

- Baskervaldx

- bitter

- tgbonum

- caladea

- librecaslon

- tgchorus

- cyklop

- forum

- imfellEnglish

- LobsterTwo

- quattrocento

## Author

Ignacio Ramos-Gutierrez, Julia G. de Aledo, Francisco Rodriguez-Sanchez

## Examples

``` r
if (FALSE) { # interactive()

create_attendance_certificate(
  data = attendance.table,
  path = "labeleR_output",
  language = "English",
  name.column = "Names",
  type = "class",
  title = "Potions Class",
  date = "01/01/2021",
  hours = 200,
  freetext = "organised by {\\bf Hogwarts School year 1992-1993}",
  signer = "A.P.W.B. Dumbledore",
  signer.role = "School Headmaster",
  lpic = NULL,
  rpic = NULL,
  signature.pic = NULL,
  font = "libertinus"
)
}
```
