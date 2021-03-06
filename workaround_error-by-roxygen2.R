## Workaround for error using roxygen2

.libPaths( c( .libPaths(), "D:/R-libraries/") )

## Source:: http://stackoverflow.com/questions/29372288/build-reload-in-rstudio-on-windows-devtoolsdocument-says-devtools-not-fou
writeLines("install.packages('devtools', repos = 'https://cran.rstudio.com/')", "inst.R")
writeLines("install.packages('RPostgreSQL', repos = 'https://cran.rstudio.com/', dependencies = T)", "inst.R")
writeLines("install.packages('roxygen2', repos = 'https://cran.rstudio.com/')", "inst.R")
writeLines("install.packages('testthat', repos = 'https://cran.rstudio.com/')", "inst.R")
writeLines("install.packages(c('dplyr', 'zoo') , repos = 'https://cran.rstudio.com/')", "inst.R")
system("Rscript --vanilla inst.R")


## New error on 2017-01-24
install.packages("roxygen2", lib = .Library) # ... and then use personal library: Yes
