# Hadley's instructions for running reverse dependency checks
# collected by me. Future Berry: don't delete this file, you fool!

#remotes::install_github("r-lib/revdepcheck")
detachAll()

library("devtools")
library("revdepcheck")
revdep() # tells which packages are reverse dependencies

res <- revdep_check() # checks each of them in a temporary library. Wait for the > !
revdep_check_save_summary() # creates problems.md and README.md
revdep_check_print_problems()
