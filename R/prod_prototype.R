#' test mongo connection in a production like environment
#'
#' test mongo connection in a production like environment
#'
#' @param collection character collection name
#' @param lconfig list database configuration passed as a list
#' @return numeric the connection total count
#' @export
prod_mongo_con <- function(collection, lconfig) {
    lconfig$collection <- collection
    do.call(mongo_connect, lconfig)
}
