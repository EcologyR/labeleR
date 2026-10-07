# Create herbarium labels

Create herbarium labels (4 labels per DIN-A4 page)

## Usage

``` r
create_herbarium_label(
  data = data,
  path = NULL,
  filename = NULL,
  title = NULL,
  subtitle = NULL,
  qr = NULL,
  family.column = NULL,
  taxon.column = NULL,
  author.column = NULL,
  det.column = NULL,
  date.det.column = NULL,
  location.column = NULL,
  area.description.column = NULL,
  latitude.column = NULL,
  longitude.column = NULL,
  elevation.column = NULL,
  field1.column = NULL,
  field2.column = NULL,
  field3.column = NULL,
  collector.column = NULL,
  collection.column = NULL,
  assistants.column = NULL,
  date.column = NULL,
  font = NULL,
  keep.files = FALSE,
  template = NULL
)
```

## Arguments

- data:

  a data frame. Each row contains the information by species that will
  appear in the label.

- path:

  Character. Path to folder where the PDF file will be saved.

- filename:

  Character. Filename of the pdf. If NULL, default is "Herbarium".

- title:

  Main title at the top of the labels. Can be blank if set to NULL.

- subtitle:

  Subtitle at the bottom of the labels. Can be blank if set to NULL.

- qr:

  String. Free text or column of `data` that specifies the link for the
  QR code. If the specified value of `qr` is not a column name of
  `data`, all the QRs will be equal, pointing to the same link.

- family.column:

  Character (optional). Name of the column in `data` storing the family
  of the taxon.

- taxon.column:

  Character (optional). Name of the column in `data` storing the taxon
  name.

- author.column:

  Character (optional). Name of the column in `data` storing the taxon
  author.

- det.column:

  Character (optional). Name of the column in `data` storing the
  determiner of the voucher.

- date.det.column:

  Character (optional). Name of the column in `data` storing the date
  when the voucher was determined.

- location.column:

  Character (optional). Name of the column in `data` storing where the
  voucher was collected.

- area.description.column:

  Character (optional). Name of the column in `data` storing the
  description of the area.

- latitude.column:

  Character (optional). Name of the column in `data` storing the
  latitude where the specimen was collected.

- longitude.column:

  Character (optional). Name of the column in `data` storing the
  longitude where the specimen was collected.

- elevation.column:

  Character (optional). Name of the column in `data` storing the
  elevation where the specimen was collected.

- field1.column:

  Character (optional). Name of the column in `data` storing the first
  free text to appear at the top of the label.

- field2.column:

  Character (optional). Name of the column in `data` storing the second
  free text to appear below field1.

- field3.column:

  Character (optional). Name of the column in `data` storing the second
  free text to appear below field2.

- collector.column:

  Character (optional). Name of the column in `data` storing the name of
  the collector.

- collection.column:

  Character (optional). Name of the column in `data` storing the
  voucher's collection number.

- assistants.column:

  Character (optional). Name of the column in `data` storing the names
  of the collector's assistants.

- date.column:

  Character (optional). Name of the column in `data` storing the date
  when the specimen was collected.

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

A pdf file with four herbarium labels per page within an 'output'
folder.

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
create_herbarium_label (
  data = herbarium.table,
  path = "labeleR_output",
  title = "Magical flora of the British Isles",
  subtitle = "Project: Eliminating plant blindness in Hogwarts students",
  qr = "QR_code",
  family.column ="Family",
  taxon.column = "Taxon",
  author.column = "Author",
  det.column = "det",
  date.det.column = "Det_date",
  location.column = "Location",
  latitude.column = "Latitude",
  longitude.column = "Longitude",
  elevation.column = "Elevation",
  field1.column = "life_form",
  field3.column = "Height",
  collector.column = "Collector",
  collection.column = "Collection_number",
  assistants.column = "Assistants",
  date.column = "Date",
  font = "libertinus"
)
#> No file name provided
#> Too long texts may give undesired results. Please consider shortening long fields.
#> 
#> 
#> processing file: herbarium.Rmd
#> 1/3                  
#> 2/3 [unnamed-chunk-1]
#> 3/3                  
#> output file: herbarium.knit.md
#> /Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/x86_64/pandoc +RTS -K512m -RTS herbarium.knit.md --to latex --from markdown+autolink_bare_uris+tex_math_single_backslash --output /private/var/folders/3_/l5pmqdn94qz35z1zz29038cm0000gn/T/RtmpBfQBm6/file343f3cf53454/reference/labeleR_output/Herbarium.tex --lua-filter /Library/Frameworks/R.framework/Versions/4.4-x86_64/Resources/library/rmarkdown/rmarkdown/lua/pagebreak.lua --lua-filter /Library/Frameworks/R.framework/Versions/4.4-x86_64/Resources/library/rmarkdown/rmarkdown/lua/latex-div.lua --embed-resources --standalone --highlight-style tango --pdf-engine pdflatex --variable graphics --include-in-header /var/folders/3_/l5pmqdn94qz35z1zz29038cm0000gn/T//RtmpBfQBm6/rmarkdown-str343f1388e5f7.html 
#> ! LaTeX Error: File `setspace.sty' not found.
#> 
#> ! Emergency stop.
#> <read *> 
#> Error: LaTeX failed to compile /private/var/folders/3_/l5pmqdn94qz35z1zz29038cm0000gn/T/RtmpBfQBm6/file343f3cf53454/reference/labeleR_output/Herbarium.tex. See https://yihui.org/tinytex/r/#debugging for debugging tips. See Herbarium.log for more info.
```
