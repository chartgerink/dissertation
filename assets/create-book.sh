#!/bin/bash

R -q -e 'bookdown::render_book("index.Rmd", bookdown::tufte__book2(keep_tex = TRUE))'
R -q -e 'bookdown::render_book("index.Rmd")'
render_book('index.Rmd', output_format = pdf_book()
