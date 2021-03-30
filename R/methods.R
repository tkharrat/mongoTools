#' generic function to form the appropriate update query
#'
#' form a query for the appropriate update query
#'
#' @param object the object we create query for (the value).
#' @param key character field name.
#' @param ... further arguments for methods.
#' @return character, the query
#' @rdname update_query
#' @export
form_update_query <-  function(object, key, ...) {
    UseMethod("form_update_query", object)
}

#' build update query block for logical type
#'
#' build update query block for logical type
#'
#' @rdname update_query
#' @export
form_update_query.logical <- function(object, key, ...) {
    paste(.convertCharacter(key), .convertLogical(object), sep = ": ")
}

#' build update query block for numeric type
#'
#' build update query block for numeric type
#'
#' @rdname update_query
#' @export
form_update_query.numeric <- function(object, key, ...) {
    paste(.convertCharacter(key), object, sep = ": ")
}

#' build update query block for character type
#'
#' build update query block for character type
#'
#' @rdname update_query
#' @export
form_update_query.character <- function(object, key, ...) {
    paste(.convertCharacter(key), .convertCharacter(object), sep = ": ")
}

#' build update query block for Date type
#'
#' build update query block for Date type
#'
#' @rdname update_query
#' @export
form_update_query.Date <- function(object, key, ...) {
    dateChar <- paste0(object, "T00:00:00Z")
    date_qr <- paste0('{"$date": ', .convertCharacter(dateChar), '}')
    paste(.convertCharacter(key), date_qr, sep = ": ")
}

#' build update query block for \code{POSIXct} type
#'
#' build update query block for \code{POSIXct} type
#'
#' @rdname update_query
#' @export
form_update_query.POSIXct <- function(object, key, ...) {
    dateChar <- format(object, "%Y-%m-%dT%H:%M:%SZ")
    date_qr <- paste0('{"$date": ', .convertCharacter(dateChar), '}')
    paste(.convertCharacter(key), date_qr, sep = ": ")
}





