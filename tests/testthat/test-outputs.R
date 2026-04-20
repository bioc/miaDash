test_that("outputs", {

    output <- new.env()
    rObjects <- new.env()
  
    overview_out <- .render_overview(output, rObjects)
    download_out <- .render_download(input, output, rObjects)
  
    expect_null(overview_out)
    expect_null(download_out)
    expect_named(output, c("object", "download"))
    
})
