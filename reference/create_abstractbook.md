# Create Book of Abstracts

Create a book of abstracts given a dataframe or tibble.

## Usage

``` r
create_abstractbook(
  data = NULL,
  path = NULL,
  filename = NULL,
  title.column = NULL,
  authors.column = NULL,
  affiliation.column = NULL,
  text.column = NULL,
  frontpage = NULL,
  toc = TRUE,
  toc.title = "Index",
  title.cex = 22,
  authors.cex = 16,
  affiliations.cex = 14,
  text.cex = 14,
  font = NULL,
  keep.files = FALSE,
  template = NULL
)
```

## Arguments

- data:

  a data frame including titles, author names, affiliations and
  abstracts.

- path:

  Character. Path to folder where the PDF file will be saved.

- filename:

  Character. Filename of the pdf. If NULL, default is "AbstractBook".

- title.column:

  Character. Name of the column in `data` storing abstracts' titles.

- authors.column:

  Character. Name of the column in `data` storing authors' names and
  affiliations. Numeric (and symbol, e.g. '\*') affiliations MUST be
  specified between brackets. Authors must be separated using a
  semi-colon (';').

- affiliation.column:

  Character. Name of the column in `data` storing the addresses for each
  affiliation number (specified between brackets after author names, in
  `authors.column`). Separations between authors must be specified using
  a semi-colon (';').

- text.column:

  Name of the column in `data` storing the abstract text.

- frontpage:

  Character. Path to PDF file to be inserted before the book of
  abstracts (as front page and/or introduction).

- toc:

  Logical. If TRUE, a Table of Contents will be included.

- toc.title:

  Character. Title to name the Table of Contents. Default is "Index".

- title.cex:

  Text font size used for the title. Default is 22.

- authors.cex:

  Text font size used for the authors' names. Default is 16.

- affiliations.cex:

  Text font size used for the affiliation addresses Default is 14.

- text.cex:

  Text font size used for the abstract main text body. Default is 14.

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

A PDF file named after `filename` is saved on disk, in the folder
defined by `path`. If `keep.files = TRUE`, an RMarkdown will also appear
in the same folder.

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

## Note

You can copy and modify at your convenience [this Google form
template](https://docs.google.com/forms/d/1u4SFWDobQrD8AEvKpvPCdwAZiVGd55B2dAtPmPEfU6E/copy)
to retrieve abstract information which will match labeleR's requirements
for a straightforward use.

## Author

Ignacio Ramos-Gutierrez, Julia G. de Aledo, Jimena Martín-Mateo,
Francisco Rodriguez-Sanchez

## Examples

``` r
if (FALSE) { # interactive()
create_abstractbook(
data=abstract.table,
path = "labeleR_output",
filename = "congress_abstractbook",
title.column = "abstract_title",
authors.column = "authors",
affiliation.column = "affiliation",
text.column = "abstract_text",
title.cex = 20,
authors.cex = 15,
affiliations.cex = 14,
font = "libertinus",
text.cex = 12,
frontpage = "Congress_frontpage.pdf"
)
}
```
