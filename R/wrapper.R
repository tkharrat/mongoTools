#' @include replicate.R
NULL

#' wrapper function to connect to mongo
#'
#' wrapper function to connect to mongo
#'
#' The function assists the user to connect to a mongo-server using
#' either local host or a replica set.
#'
#' @param db_name character, name of the mongo database.
#' @param collection character, name of the mongo collection in
#'     \code{db_name}.
#' @param user,password character, credentials to connect to mongo
#'     atlas.
#' @param url character, vector clusters urls together with connection
#'     port.
#' @param replicaset character, main cluster url.
#' @param use_ssl logical, ssl connection ?
#' @param port numeric, connection port.
#' @param authSource character (optional), do we have an
#'     authentification source key?
#' @param connection_delay numeric should we apply a socket-timout ? should be
#'     provided in ms.
#' @return the result of running the command \code{mongolite::mongo()}
#' @examples
#' lconfig_local <- list(url = "localhost", replicaset = NA_character_,
#'                       port =  27017, db_name = "ExternalData",
#'                       user = "", password =  "")
#' lconfig_local$collection <- "Player"
#' m <- do.call(mongo_connect, lconfig_local)
#' print(m$count())
#'
#' @export
mongo_connect <- function(db_name, collection,
                          user = "", password = "", url = "localhost",
                          replicaset = NULL, port = 27017L,
                          use_ssl = FALSE, authSource = NULL,
                          connection_delay = NULL,
                          mongo_base = "mongodb://",
                          retry_writes = NULL,
                          majority = NULL) {
    if (!is.null(replicaset) && is.na(replicaset))
        replicaset <- NULL

    if ((user == "" & password == "") | (is.na(user) & is.na(password))) {
        ## connection to local host
        if (!grepl(":", url[1]))
            url <- paste0(url[1], ":", port)

        mongo(collection = collection, db = db_name,
              url = paste0("mongodb://",  url)
              )
    } else
        mongo_replicate(db = db_name, collection = collection,
                        username = user, password = password,
                        cluster_urls = url, replicaset = replicaset,
                        port = port,
                        use_ssl = use_ssl, authSource = authSource,
                        connection_delay = connection_delay,
                        mongo_base = mongo_base,
                        retry_writes = retry_writes,
                        majority = majority)
}

#' Test mongo connection in a production like environment
#'
#' Test mongo connection in a production like environment. Connects to
#' the collection and returns the number of records in it.
#'
#' @param collection character, name of the collection.
#' @param lconfig list, further arguments for
#'     \code{\link{mongo_connect}}.
#' @return numeric, number of records in the collection
#' @examples
#' test_mongo_con("Fixture", lconfig = list(db_name = "Examples")) # 76292
#' @export
test_mongo_con <- function(collection, lconfig) {
    lconfig$collection <- collection
    m <- do.call(mongo_connect, lconfig)
    m$count()
}

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
