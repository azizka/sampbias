library(goodpractice)
gp()

#R CMD check
devtools::check_built(path = ".",
                      manual = T, cran = T)

devtools::spell_check()

devtools::check_rhub()

devtools::check_win_devel()

codemetar::write_codemeta()

devtools::release()
