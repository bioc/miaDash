
.setupCommand <- c('
# Import libraries
if (!require("BiocManager")) {
    install("BiocManager")
    library("BiocManager")
}
    
pkgs <- c("mia", "scater", "scuttle", "bluster")
    
temp <- sapply(pkgs, function(pkg) {
    if (!require(pkg, character.only = TRUE)) {
        install(pkg)
        library(pkg, character.only = TRUE)
    }
})
')

.dataCommand <- function(dataset){
    paste0(
        '# Import dataset\n',
        'data("', dataset, '", package = "mia")\n',
        'tse <- ', dataset, '\n'
    )
}

.rdsCommand <- function(file){
    paste0(
        '# Import dataset from RDS\n',
        'tse <- readRDS("', file$name, '")'
    )
}