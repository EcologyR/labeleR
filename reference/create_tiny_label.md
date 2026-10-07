# Create tiny labels

Create tiny labels (16 labels per DIN-A4 page)

## Usage

``` r
create_tiny_label(
  data = NULL,
  qr = NULL,
  path = NULL,
  filename = NULL,
  field1.column = NULL,
  field2.column = NULL,
  field3.column = NULL,
  field4.column = NULL,
  field5.column = NULL,
  font = NULL,
  keep.files = FALSE,
  template = NULL
)
```

## Arguments

- data:

  a data frame including information of a species

- qr:

  String. Free text or column of `data` that specifies the link for the
  QR code. If the specified value of `qr` is not a column name of
  `data`, all the QRs will be equal, pointing to the same link.

- path:

  Character. Path to folder where the PDF file will be saved.

- filename:

  Character. Filename of the pdf. If NULL, default is "Tiny_label".

- field1.column:

  Character (optional). Name of the column in `data` storing the first
  free text to appear at the top of the label.

- field2.column:

  Character (optional). Name of the column in `data` storing the second
  free text to appear below field1.

- field3.column:

  Character (optional). Name of the column in `data` storing the third
  free text to appear below field2.

- field4.column:

  Character (optional). Name of the column in `data` storing the fourth
  free text to appear below field3.

- field5.column:

  Character (optional). Name of the column in `data` storing the fifth
  free text to appear below field4.

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

A PDF file named "Tiny_label.pdf" is saved on disk, in the folder
defined by `path`. If `keep.files = TRUE`, an RMarkdown file will also
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
create_tiny_label(
  data = tiny.table,
  qr = "QR_code",
  path = "labeleR_output",
  field1.column = "field1",
  field2.column = "field2",
  field3.column = "field3",
  field4.column = "field4",
  field5.column = "field5",
  font = "libertinus"
)
#> No file name provided
#> 
#> 
#> processing file: tiny_label.Rmd
#> 1/3                  
#> 2/3 [unnamed-chunk-1]
#> 3/3                  
#> output file: tiny_label.knit.md
#> /Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/x86_64/pandoc +RTS -K512m -RTS tiny_label.knit.md --to latex --from markdown+autolink_bare_uris+tex_math_single_backslash --output /private/var/folders/3_/l5pmqdn94qz35z1zz29038cm0000gn/T/RtmpBfQBm6/file343f3cf53454/reference/labeleR_output/Tiny_label.tex --lua-filter /Library/Frameworks/R.framework/Versions/4.4-x86_64/Resources/library/rmarkdown/rmarkdown/lua/pagebreak.lua --lua-filter /Library/Frameworks/R.framework/Versions/4.4-x86_64/Resources/library/rmarkdown/rmarkdown/lua/latex-div.lua --embed-resources --standalone --highlight-style tango --pdf-engine pdflatex --variable graphics --include-in-header /var/folders/3_/l5pmqdn94qz35z1zz29038cm0000gn/T//RtmpBfQBm6/rmarkdown-str343f7ec9604e.html 
#> ! LaTeX Error: File `libertinus.sty' not found.
#> 
#> ! Emergency stop.
#> <read *> 
#> Error: LaTeX failed to compile /private/var/folders/3_/l5pmqdn94qz35z1zz29038cm0000gn/T/RtmpBfQBm6/file343f3cf53454/reference/labeleR_output/Tiny_label.tex. See https://yihui.org/tinytex/r/#debugging for debugging tips. See Tiny_label.log for more info.
```
