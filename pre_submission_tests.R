
pkgdown::build_site()

source("https://install-github.me/MangoTheCat/goodpractice")
library(goodpractice)
gp("C:/Users/az64mycy/Dropbox (iDiv)/research_projects/29_CoordinateCleaner/CoordinateCleaner")

#R CMD check
devtools::check_built(path = ".",
                      manual = T, cran = T)

devtools::spell_check()

devtools::check_rhub()

devtools::check_win_devel()

codemetar::write_codemeta()

devtools::release()
