library(rhub)
library(utils)
author=utils::person(
  given = "Shneior",
  family = "Shprintsin",
  email = "shnyor360@gmail.com",
  role = c("aut", "cre")
)
author=utils::person(
  given = "Shneior",
  family = "Shprintsin",
  email = "shnyor360@gmail.com")

utils
use_description(
  fields = list(
    Package = "defactor",
    Title = "Tools for Managing and Annotating Data Frame Variables with Labels",
    Version = "0.1.0",
    Author = author,
    Maintainer = utils::person(
      given = "Shneior",
      family = "Shprintsin",
      email = "shnyor360@gmail.com"),
    Description = "Provides a set of functions to efficiently manage, annotate, and retrieve variable labels in data frames. This package allows for dynamic setting and extraction of labels based on patterns, column selections, or custom specifications. It is particularly useful for data cleaning, analysis, and preparation tasks where labeled data is critical for interpretation and reporting.",
    License = "MIT + file LICENSE",
    Encoding = "UTF-8",
    LazyData = "true",
    Roxygen = "list(markdown = TRUE)",
    RoxygenNote = "7.3.2",
    Depends = "R (>= 3.5.0)",
    Suggests = "testthat, data.table"
  )
)
use_description(fields = list(Language = "es"))
usethis::use_cran_comments()
setwd('C:/cde/r/snlib/defactor/src')
devtools::clean_dll() # If applicable
devtools::build()
devtools::check()
devtools::check_win_devel()
devtools::check_win_release()
devtools::check_win_oldrelease()
rc_new_token()
rhub::check()

rhub_setup()
rhub::rhub_doctor()
rhub::rhub_check()
rhub::rc_new_token()
rc_submit("build/defactor_0.1.0.tar.gz")
devtools::submit_cran(path = "build/defactor_0.1.0.tar.gz")
devtools::submit_cran(pkg = ".", args = NULL)

