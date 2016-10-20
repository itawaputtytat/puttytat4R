## Workaround for error using roxygen2
## Source:: http://stackoverflow.com/questions/29372288/build-reload-in-rstudio-on-windows-devtoolsdocument-says-devtools-not-fou
writeLines("install.packages('devtools', repos = 'https://cran.rstudio.com/')", "inst.R")
writeLines("install.packages('roxygen2', repos = 'https://cran.rstudio.com/')", "inst.R")
writeLines("install.packages('testthat', repos = 'https://cran.rstudio.com/')", "inst.R")
system("Rscript --vanilla inst.R")
