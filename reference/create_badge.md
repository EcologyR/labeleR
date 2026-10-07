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
#> No file name provided
#> 
#> 
#> processing file: badge.Rmd
#> 1/3                  
#> 2/3 [unnamed-chunk-1]
#> 3/3                  
#> output file: badge.knit.md
#> /Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/x86_64/pandoc +RTS -K512m -RTS badge.knit.md --to latex --from markdown+autolink_bare_uris+tex_math_single_backslash --output /private/var/folders/3_/l5pmqdn94qz35z1zz29038cm0000gn/T/RtmpBfQBm6/file343f3cf53454/reference/labeleR_output/Badges.tex --lua-filter /Library/Frameworks/R.framework/Versions/4.4-x86_64/Resources/library/rmarkdown/rmarkdown/lua/pagebreak.lua --lua-filter /Library/Frameworks/R.framework/Versions/4.4-x86_64/Resources/library/rmarkdown/rmarkdown/lua/latex-div.lua --embed-resources --standalone --highlight-style tango --pdf-engine pdflatex --variable graphics --include-in-header /var/folders/3_/l5pmqdn94qz35z1zz29038cm0000gn/T//RtmpBfQBm6/rmarkdown-str343f3728f5df.html 
#> ! LaTeX Error: File `libertinus.sty' not found.
#> 
#> ! Emergency stop.
#> <read *> 
#> Error: LaTeX failed to compile /private/var/folders/3_/l5pmqdn94qz35z1zz29038cm0000gn/T/RtmpBfQBm6/file343f3cf53454/reference/labeleR_output/Badges.tex. See https://yihui.org/tinytex/r/#debugging for debugging tips. See Badges.log for more info.
```
