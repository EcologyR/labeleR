# Create multichoice exam

Creates a multichoice exam, with 4 questions per page (first one has
title plus 3 questions).

## Usage

``` r
create_multichoice(
  data = NULL,
  path = NULL,
  filename = NULL,
  title = NULL,
  question.column = NULL,
  option1.column = NULL,
  option2.column = NULL,
  option3.column = NULL,
  option4.column = NULL,
  image.column = NULL,
  solutions = F,
  start = 1,
  seeds = NULL,
  frontpage = NULL,
  font = NULL,
  keep.files = FALSE,
  template = NULL
)
```

## Arguments

- data:

  a data frame including the questions and choices, and may include a
  column with adjacent image paths.

- path:

  Character. Path to folder where the PDF file will be saved.

- filename:

  Character. Filename of the pdf. If NULL, default is "multichoice".

- title:

  Title of the exam. If different models are rendered, they will be
  shown too.

- question.column:

  Character. Name of the column in `data` storing the question to be
  shown.

- option1.column:

  Character. Name of the column in `data` storing the CORRECT answer. It
  will not always appear on the first place; options will be randomly
  sorted.

- option2.column:

  Character. Name of the column in `data` storing the second choice
  (INCORRECT).

- option3.column:

  Character (optional for 2 choice questions). Name of the column in
  `data` storing the third choice (INCORRECT).

- option4.column:

  Character (optional for 3 choice questions). Name of the column in
  `data` storing the fourth choice (INCORRECT).

- image.column:

  Character (optional). Name of the column containing file paths to an
  image which may appear by the side of the question.

- solutions:

  Logical. Whether or not a PDF with the solutions should be rendered.

- start:

  Numeric. Number of the first question (useful if exams are split in
  several parts).

- seeds:

  Numeric vector of undefined length. Seeds to randomize question order
  and choice order. If the length of `seeds` is longer than one, a
  multiple choice exam will be rendered for EACH ONE specified.

- frontpage:

  Character (optional). Path to PDF file to be inserted before the
  document (as front page and/or instructions).

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

A PDF file is saved on disk, in the folder defined by `path`. If
`keep.files = TRUE`, an RMarkdown file will also appear in the same
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

Ignacio Ramos-Gutierrez, Jimena Mateo-Martín, Julia G. de Aledo,
Francisco Rodriguez-Sanchez

## Examples

``` r
if (FALSE) { # interactive()
create_multichoice(
data = multichoice.table,
path = "labeleR_output",
filename = "example_exam",
title = "Example test",
question.column = "question",
option1.column = "opt1.correct",
option2.column = "opt2",
option3.column = "opt3",
option4.column = "opt4",
font = "libertinus",
start = 1,
solutions=T,
seeds = c(1:2)
)
}
```
