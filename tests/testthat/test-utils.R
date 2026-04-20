test_that("utils", {
  
    data("Tengeler2020", package = "mia")
    tse <- Tengeler2020
    
    rObjects <- list(tse = tse)
    pObjects <- new.env()
    
    expect_no_error(
        tse <- .update_tse(rObjects, pObjects, "transformAssay",
            list(tse, assay.type = "counts", method = "relabundance"))
    )
    
    item <- NULL
    expect_identical(.set_optarg(item, alternative = "alternative"),
       "alternative")
    
    item <- "100"
    expect_identical(.set_optarg(item, loader = as.numeric), 100)
  
    expect_true(.check_formula("data ~ patient_status + cohort", tse))
    expect_false(.check_formula("data ~ wrong_var + sample_name", tse))
})
