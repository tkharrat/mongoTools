#' Functions to Interact with MongoDB.
#'
#' A set of tools to assist the user requesting data from mongoDB. We cover in
#' particular query generating and cluster connection. The package is meant to
#' complete mongolite.
#'
#' In order to run the examples, make sure you restore your mongoDB with the
#' dump shipped with this package. For example, this will create database
#' 'Examples':
#' 
#'     \code{mongorestore -d Examples mongoTools/inst/extdata/dump/Examples/}
#'
#' @examples
#' admin <- mongo(db = "admin")
#' ## one of the databases shown by this should be 'Examples' (see above)
#' admin$run('{"listDatabases":1}')
#' @docType package
#' @name mongoTools
#' @import mongolite
NULL
