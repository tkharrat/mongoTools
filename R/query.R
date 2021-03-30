#' build a query for mongoDB request
#'
#' build a query for mongoDB request
#'
#' @param keys character vector keys
#' @param values list same length as keys with the fields values.
#' @param type character query operator. Either of length 1 (and in this case it
#'     will be recycled to be \code{length(values)}) or of same length as keys
#'     if different type should be applied to different keys. Accepted values
#'     are \code{"equality"} (for all types of values), \code{"not_equals"},
#'     \code{"less_than"}, \code{"less_than_equals"}, \code{"greater_than"},
#'     \code{"greater_than_equals"} (for numerical values only), \code{"exists"}
#'     (for logiical values) and \code{"intervals"} (for numerical values only;
#'     large bounds will be applied)
#' @return character JSON type query.
#' @examples
#' m = mongo("Fixture", "Examples")
#' ## find all fixtures from competitionId 8 in seasonId 2017
#' keys = list(competitionId = 8, seasonId = 2017)
#' m$count(buildQuery(names(keys), keys)) ## 380
#' @export
buildQuery <- function(keys, values, type = "equality") {
    ## match arg
    type <- match.arg(type,
                      choices = c("equality", "not_equals",
                                  "less_than", "less_than_equals",
                                  "greater_than", "greater_than_equals",
                                  "exists", "intervals"),
                      several.ok = TRUE)
    ## collect inputs
    if (missing(keys))
        keys <- names(values)

    ## check inputs
    stopifnot(is.list(values),
              length(keys) == length(values)
              )

    ## recycle type if needed
    if (length(type) == 1)
        type <- rep(type, length(keys))
    else
        stopifnot(length(type) == length(keys))

    ## compute the range
    range <- sapply(values, function(x) length(x) > 1)

    paste0("{", .buildAllKeys(keys, values, range, type), "}")
}

#' build a query useful for update operations.
#'
#' build a query useful for update operations.
#'
#' @inheritParams buildQuery
#' @name update_query
#' @export
buildUpdateQuery <- function(keys, values) {
    ##rhs <- buildQuery(keys, values, type)
    ## mew way to build rhs
    .wh <- function(i) {
        form_update_query(values[[i]], keys[i])
    }
    tmp <- sapply(1:length(keys), .wh)
    rhs <- paste0("{", paste(tmp, collapse = ", "), "}")

    lhs <- '\"$set\":'
    paste0("{", lhs, rhs, "}")
}

#' build date query
#'
#' build date query by searching date from midnight to midnight.
#'
#' @param dateValue Date query value as a date object
#' @param dateFieldName character date field name.
#' @param date_side character with possible values \code{"less_than"},
#' \code{"less_than_equals"}. \code{"equality"}, \code{"greater_than_equals"}
#' and \code{"greater_than"}.
#' @return character query in JSON type
#' @examples
#' m = mongo("Fixture", "Examples")
#' ## find all fixtures in Feb 3, 2017
#' m$count(buildDateQuery(as.Date("2017-02-03"))) ## 16
#' @export
buildDateQuery <- function(dateValue, dateFieldName = "gameDate",
                           date_side = c("equality",
                                         "less_than", "less_than_equals",
                                         "greater_than", "greater_than_equals")) {
    ## check inputs
    date_side <- match.arg(date_side)
    if (!lubridate::is.Date(dateValue))
        stop(paste(dateValue, "is not a valid date object !"))

    value <- .createDateQueryFromChar(dateValue, date_side)

    paste0('{',
           paste(.convertCharacter(dateFieldName), value, sep = ": "),
           '}')
}

#' build date interval query
#'
#' build date interval query
#'
#' @param date_min,date_max Date objects left and right hand side of the interval
#' @param dateFieldName character date field name.
#' @param start_type,end_type character with possible values \code{large} and
#' \code{strict}. Should the equality be large or strict.
#' @return character query in JSON type.
#' @examples
#' m = mongo("Fixture", "Examples")
#' ## find all fixtures in Feb 3, 2017 (large) and Feb 8, 2017 (strict)
#' qr <- buildIntervalQuery(as.Date("2019-02-03"), as.Date("2019-02-08"),
#'                          start_type = "large", end_type = "strict")
#' m$count(qr) ## 37
#' @export
buildIntervalQuery <- function(date_min, date_max,
                               dateFieldName = "gameDate",
                               start_type = c("large", "strict"),
                               end_type = c("large", "strict")) {
    ## check inputs
    start_type <- match.arg(start_type)
    end_type <- match.arg(end_type)
    if (!lubridate::is.Date(date_min) | !lubridate::is.Date(date_max))
        stop(paste(date_min, "and", date_max, "should be valid date object !"))

    value <- .createDateIntervalQueryFromChar(date_min, date_max,
                                              start_type, end_type)
    paste0('{',
           paste(.convertCharacter(dateFieldName), value, sep = ": "),
           '}')
}

#' build a mixed query with date and normal fields
#'
#' build a mixed query with date and normal fields
#'
#' @inheritParams buildQuery
#' @param dateValue Date query value as a date object
#' @param dateFieldName character field name.
#' @param date_side character with possible values \code{"less_than"},
#' \code{"less_than_equals"}. \code{"equality"}, \code{"greater_than_equals"}
#' and \code{"greater_than"}.
#' @return character query in JSON type
#' #' @examples
#' m = mongo("Fixture", "Examples")
#' ## find all fixtures in competition 8 seasonId 2017, played after Feb 3, 2017
#' keys = list(competitionId = 8, seasonId = 2017)
#' qr = buildMixedQuery(names(keys), keys, dateValue = as.Date("2018-02-03"), date_side = "greater_than")
#' gameDates = m$distinct(gameDate", qr)
#' min(gameDates) ## expect it to be after 2018-02-03
#' @export
buildMixedQuery <- function(keys, values,
                            type = c("equality", "not_equals",
                                     "less_than", "less_than_equals",
                                     "greater_than", "greater_than_equals",
                                     "exists", "intervals"),
                            dateValue, dateFieldName = "gameDate",
                            date_side = c("equality",
                                          "less_than", "less_than_equals",
                                          "greater_than", "greater_than_equals")) {
    qr1 <- buildQuery(keys, values, type)
    dateQr <- buildDateQuery(dateValue, dateFieldName, date_side)

    ## combine query
    paste(substr(dateQr, 1, nchar(dateQr) - 1),
          substr(qr1, 2, nchar(qr1)),
          sep = ", ")
}

#' build a mixed query with date intervals and normal fields
#'
#' build a mixed query with date intervals and normal fields
#'
#' @inheritParams buildQuery
#' @param date_min,date_max Date objects left and right hand side of
#'     the interval
#' @param dateFieldName character date field name.
#' @param start_type,end_type character with possible vslues
#'     \code{large} and \code{strict}. Should the equality be large or
#'     strict.
#' @return character query in JSON type
#' @export
buildMixedIntervalQuery <- function(keys, values,
                                    type = c("equality", "not_equals",
                                             "less_than", "less_than_equals",
                                             "greater_than", "greater_than_equals",
                                             "exists", "intervals"),
                                    date_min, date_max,
                                    dateFieldName = "gameDate",
                                    start_type = c("large", "strict"),
                                    end_type = c("large", "strict")) {
    qr1 <- buildQuery(keys, values, type)
    dateQr <- buildIntervalQuery(date_min, date_max, dateFieldName,
                                 start_type, end_type)

    ## combine query
    paste(substr(dateQr, 1, nchar(dateQr) - 1),
          substr(qr1, 2, nchar(qr1)),
          sep = ", ")
}

#' build query for objectId field
#'
#' build query for objectId field
#'
#' This query will request the \code{_id} field.
#'
#' @param objectId character objectId value
#' @return character query test strings
#' @export
buildObjectQuery <- function(objectId) {
    stopifnot(is.na(suppressWarnings(as.numeric(objectId))))
    paste0('{\"_id\" : {\"$oid\":\"',  objectId, '\"}}')
}

#' build query to extract random documents from the database
#'
#' build query to extract random documents from the database
#'
#' This query is to be used with the \code{aggregate()} method.
#'
#' @param qr character a valid query object.
#' @param nSample numeric nombre of documents to extract
#' @return a character query to pass to the aggregate method.
#' @examples
#' m = mongo("Fixture", "Examples")
#' ## find all fixtures in competition 8 seasonId 2017, played after Feb 3, 2017
#' keys = list(competitionId = 8, seasonId = 2017)
#' qr = buildQuery(names(keys), keys)
#' m$aggregate(buildRandomQuery(qr, 2))$gameId
#' @export
buildRandomQuery <- function(qr, nSample = 1) {
    paste0('[{\"$match\":', qr, '},',
           '{\"$sample\": {\"size\":', nSample, '}}]'
           )
}
