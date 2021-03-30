removeDuplication <- function(mCol, key) {
    mongolitedt::bind_mongolitedt(mCol)
    keyQ <- list("_id" = 1)
    keyQ[[key]] <- 1
    tmp <- mCol$finddt(fields = keyQ)
    
    ## find duplicated indexes
    iis <- which(duplicated(tmp[, key]))
    dupIds <- as.numeric(unlist(tmp[iis, "_id"]))
    
    dups <- list("_id" = dupIds)
    mCol$remove(query = buildQuery(names(dups), dups), multiple = TRUE)
}



