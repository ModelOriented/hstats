#=============================================================================
# Put together the package
#=============================================================================

# WORKFLOW: UPDATE EXISTING PACKAGE
# 1) Modify package content and documentation.
# 2) Increase package number in "use_description" below.
# 3) Go through this script and carefully answer "no" if a "use_*" function
#    asks to overwrite the existing files. Don't skip that function call.
# devtools::load_all()

library(usethis)

# Sketch of description file
use_description(
  fields = list(
    Title = "Interaction Statistics",
    Version = "1.2.0",
    Description = "Fast, model-agnostic implementation of different H-statistics
    introduced by Jerome H. Friedman and Bogdan E. Popescu (2008) <doi:10.1214/07-AOAS148>. 
    These statistics quantify interaction strength per feature, feature pair, 
    and feature triple.
    The package supports multi-output predictions and can account for case weights. 
    In addition, several variants of the original statistics are provided.
    The shape of the interactions can be explored through partial dependence plots
    or individual conditional expectation plots. 'DALEX' explainers, meta learners 
    ('mlr3', 'tidymodels', 'caret') and most other models work out-of-the-box.",
    `Authors@R` = 
    "c(person('Michael', family='Mayer', role=c('aut', 'cre'), email='mayermichael79@gmail.com', comment=c(ORCID='0000-0002-6148-5756')),
       person('Przemyslaw', family='Biecek', email='przemyslaw.biecek@gmail.com', role='ctb', comment=c(ORCID='0000-0001-8423-1823'))
      )",
    Depends = "R (>= 3.2.0)",
    LazyData = NULL
  ),
  roxygen = TRUE
)
  
use_package("stats", "Imports")
use_package("utils", "Imports")
use_package("ggplot2", "Imports")

use_gpl_license(2)

# Your files that do not belong to the package itself (others are added by "use_* function")
use_build_ignore(c("^packaging.R$", "[.]Rproj$", "^logo.png$",
                   "^docu$", "^backlog$"), escape = FALSE)

# Add short docu in Markdown (without running R code)
use_readme_md()

# Longer docu in RMarkdown (with running R code). Often quite similar to readme.
# use_vignette("hstats")

# If you want to add unit tests
use_testthat()
# use_test("hstats.R")
# use_test("methods.R")

# On top of NEWS.md, describe changes made to the package
use_news_md()

# Add logo
use_logo("logo.png")

# If package goes to CRAN: infos (check results etc.) for CRAN
use_cran_comments()

use_github_links() # use this if this project is on github

# Github actions
# use_github_action("check-standard")
# use_github_action("test-coverage")
# use_github_action("pkgdown")

#=============================================================================
# Finish package building (can use fresh session)
#=============================================================================

library(devtools)

document()
test()
check(manual = TRUE, cran = TRUE)
build()
# build(binary = TRUE)
install(upgrade = FALSE)

# Run only if package is public(!) and should go to CRAN
if (FALSE) {
  check_win_devel()
  check_rhub()
  
  # Wait until above checks are passed without relevant notes/warnings
  # then submit to CRAN
  release()
}
