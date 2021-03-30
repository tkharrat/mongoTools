#' @include urls.R
NULL

#' mongo replicate cluster driver
#'
#' mongo replicate cluster driver
#'
#' Assist the user to connect to the replicate mongo cluster.
#' @param db character, name of the mongo database.
#' @param collection character, name of the mongo collection in \code{db}.
#' @param username,password character, credentials to connect to mongo atlas.
#' @param cluster_urls character, vector clusters urls together with connection
#'     port.
#' @param replicaset character, main cluster url
#' @param use_ssl logical ssl connection ?
#' @param port numeric, connection port.
#' @param authSource character (optional), do we have an authentification source
#'     key?
#' @param connection_delay numeric should we apply a socket-timout ? should be
#'     provided in ms.
#' @return the result of running the command \code{mongolite::mongo()}
#' @examples
#' \dontrun{
#' m1 <- mongo_replicate("InternalDataBookie", "allFeatures")
#' print(m1$count())
#' }
#'
#' @export
mongo_replicate <- function(db, collection, username, password,
                           cluster_urls = aws_urls_,
                           replicaset = aws_cluster_name_,
                           port = 27017L,
                           use_ssl = FALSE, authSource = NULL,
                           connection_delay = NULL,
                           mongo_base = "mongodb://",
                           retry_writes = NULL,
                           majority = NULL) {

     if (missing(username))
        username <- Sys.getenv("aws_db_username")

    if (missing(password))
        password <- Sys.getenv("aws_db_password")

    ## =============== handle urls
    if (!is.null(port) & nchar(port) > 0) {
        if (length(cluster_urls) == 1) {
            if (grepl(",", cluster_urls) ) {
                tmp <- unlist(strsplit(cluster_urls, ","))
                if (!all(grepl(":", tmp)))
                    cluster_urls <- paste(tmp, port, sep = ":")
            } else {
                if (!grepl(":", cluster_urls))
                    cluster_urls <- paste(cluster_urls, port, sep = ":")
            }
        } else if (length(cluster_urls) == 3) {
            if (!all(grepl(":", cluster_urls)))
                cluster_urls <- paste(cluster_urls, port, sep = ":")
        } else
            stop("url format not recognised !")
    }

    ## ============== build base uri
    uri <- paste0(mongo_base, username, ":", password, "@",
                   cluster_urls)

     ## ============== authentification source
    if (!is.null(authSource) && !is.na(authSource) && nchar(authSource) > 0)
        uri <- paste0(uri, "/", authSource)

    ## check if we need to complate the url
    extra_url <- character(0)

    ## ============== replica set
    if (!is.null(replicaset) && !is.na(replicaset) && nchar(replicaset) > 0)
        extra_url <- c(extra_url, paste0("replicaSet=", replicaset))

    ## ============= ssl certificate
    if (use_ssl)
        extra_url <- c(extra_url, "ssl=true")

    ## ============ retry writes
    if (!is.null(retry_writes))
        extra_url <- c(extra_url, "retryWrites=true")

    ## =========== majority
    if (!is.null(majority))
        extra_url <- c(extra_url, "w=majority")

    ## ============== socker-timeout
    if (!is.null(connection_delay) &&
        !is.na(connection_delay) &&
        is.numeric(connection_delay))
        extra_url <- c(extra_url,
                      paste0("sockettimeoutms=", connection_delay)
                      )

    if (length(extra_url) > 0)
        uri <- paste0(uri, "?", paste(extra_url, collapse = "&"))

    ## mongo connect
    mongo(collection = collection, db = db, url = uri)
}

#' convenient function to connect to mongo-atlas cluster
#'
#' convenient function to connect to mongo-atlas cluster
#'
#' The user can go to the mongo-Atlas website to find the different urls
#' required.  He can be inspired by the shell command example.
#' @inheritParams mongo_replicate
#' @examples
#' \dontrun{
#' m2 <- mongo_atlas("ExternalData", "Player")
#' print(m2$count())
#' }
#'
#' @export
mongo_atlas <- function(db, collection, username, password,
                        cluster_urls = atlas_urls_,
                        replicaset = atlas_cluster_name_,
                        use_ssl = TRUE,
                        authSource = "admin") {

    if (missing(username))
        username <- Sys.getenv("mongo_atlas_tarak_username")

    if (missing(password))
        password <- Sys.getenv("mongo_atlas_tarak_password")

    mongo_replicate(db = db, collection = collection,
                    username = username, password = password,
                    cluster_urls = cluster_urls,
                    replicaset = replicaset,
                    use_ssl = use_ssl, authSource = authSource)
}
