#!/bin/bash

R -q -e 'bookdown::render_book("index.Rmd", bookdown::pdf_book(keep_tex = TRUE))'
R -q -e 'bookdown::render_book("index.Rmd")'

pdf_book(toc = TRUE, number_sections = TRUE, fig_caption = TRUE, 
..., base_format = t:tufte_book, toc_unnumbered = TRUE, 
    toc_appendix = FALSE, toc_bib = FALSE, quote_footer = NULL, 
      highlight_bw = FALSE)
