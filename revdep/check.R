library("devtools")
revdep("berryFunctions")
res <- revdep_check()
revdep_check_save_summary()
