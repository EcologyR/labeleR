# Create badges

Create badges (8 badges per DIN-A4 page)

## Usage

``` r
create_badge(
  data = NULL,
  path = NULL,
  filename = NULL,
  event = NULL,
  name.column = NULL,
  affiliation.column = NULL,
  lpic = NULL,
  rpic = NULL,
  font = NULL,
  keep.files = FALSE,
  template = NULL
)
```

## Arguments

- data:

  a data frame including names and (optionally) affiliations.

- path:

  Character. Path to folder where the PDF file will be saved.

- filename:

  Character. Filename of the pdf. If NULL, default is "Badges".

- event:

  Character. Title of the event.

- name.column:

  Character. Name of the column in `data` storing participants' name.

- affiliation.column:

  Character (optional). Name of the column in `data` storing
  participants' affiliation.

- lpic:

  Character (optional) Path to a PNG image to be located in the badge
  top-left.

- rpic:

  Character (optional) Path to a PNG image to be located in the badge
  top-right.

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

A PDF file named "Badges.pdf" is saved on disk, in the folder defined by
`path`. If `keep.files = TRUE`, an RMarkdown and PNG lpic and rpic files
will also appear in the same folder.

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
create_badge(
  data = badges.table,
  path = "labeleR_output",
  filename = NULL,
  event = "INTERNATIONAL CONFERENCE OF MUGGLEOLOGY",
  name.column = "List",
  affiliation.column = "Affiliation",
  font = "libertinus",
  lpic = NULL,
  rpic = NULL)
}
```
