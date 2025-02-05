---
title: 'labeleR: Automate the Production of Custom Labels, Badges, Certificates, and Other Documents'
tags:
  - R
  - Rmarkdown
  - LaTeX
  - scientific collections
  - scientific events
  - herbaria
  - automation
  - exams
authors:
  - name: Julia G. de Aledo
    orcid: 0000-0001-9065-9316
    affiliation: "1,2" # (Multiple affiliations must be quoted)
  - name: Jimena Mateo-Martín
    orcid: 0000-0000-0000-0000
    affiliation: 3
  - name: Francisco Rodríguez-Sánchez
    orcid: 0000-0002-7981-1599
    affiliation: 4
  - name: Ignacio Ramos-Gutiérrez
    orcid: 0000-0002-8675-0114
    affiliation: "1,4"
  
affiliations:
  - name: Estación Biológica de Doñana
    index: 1
  - name: Universidad Rey Juan Carlos
    index: 2
  - name: Universidad Autónoma de Madrid
    index: 3
  - name: Universidad de Sevilla
    index: 4
citation_author: de Aledo et. al.
date: '2025-02-05'
year: 2025
bibliography: paper.bib
output: rticles::joss_article
csl: apa.csl
journal: JOSS
editor_options: 
  markdown: 
    wrap: 72
---

# Summary

LabeleR is a R package, is an open-source tool designed to automate the creation of scientific collection labels, and document for events. It simplifies processes that are typically repetitive and time-consuming, offering an alternative to traditional methods, like Microsoft integrations, or expensive, complex software. LabeleR enables users a wide variety of PDF documents that can be either stored, printed, or sent automatically via email.

Users can easily choose their source database, input QR codes and logos, customize content according to specific needs and even create their own custom templates. The package has six main functions, three of them focused on the management of scientific collections (herbarium labels, collection labels, and a smaller version called “tinylabels”), and another three functions on the organization of scientific events (event badges, attendance certificates, and participation certificates). LabeleR turns a tedious process into a quick activity to obtain labels and/or certificates in PDF format for easy printing or distribution. We believe that this versatile tool contributes to scientific efficiency by simplifying complex, time-demanding labelling and organization processes.


# Pipeline

This R package encompasses eight functions that are built under the same pipeline (dataset > R > Rmarkdown > latex > PDF). Each R function includes four types of arguments: R based (data, path, filename), fixed, text written in the console or path for a picture; variable, which will refer to a column of the dataset; and mixed, which can be either written directly or refer to a column. The fixed arguments will remain by default the same across all documents (e.g. name of the project, event, the signer position). The variable ones are params hosted in a Rmarkdown template, which is written and renderized in LaTeX language with embebbed chunks of R to be able to be custom the number of results in a single PDF page (1, 4, 8, or 16). The outputs can be either stored in the local path indicated in the function and then printed or sent, ot the outputs can be sent automatically through email.


![alt text here](fig1.png)


# Citations

Citations to entries in paper.bib should be in
[rMarkdown](https://rmarkdown.rstudio.com/authoring_bibliographies_and_citations.html)
format.

For a quick reference, the following citation commands can be used: -
`@author:2001` -\> "Author et al. (2001)" - `[@author:2001]` -\>
"(Author et al., 2001)" - `[@author1:2001; @author2:2001]` -\> "(Author1
et al., 2001; Author2 et al., 2002)"

# Rendered R Figures

Figures can be plotted like so:


``` r
plot(1:10)
```

![](ms_joss_files/figure-latex/unnamed-chunk-1-1.pdf)<!-- --> 

# Acknowledgements

We acknowledge contributions from Brigitta Sipocz, Syrtis Major, and
Semyeong Oh, and support from Kathryn Johnston during the genesis of
this project.

# References
