#' Build a query to compute field statistics
#'
#' Build a query to compute field statistics, currently max, min,
#' average, and standard deviation.
#'
#' @param field character, name of the field to compute statistics for.
#' @param sub_field character (optional), sub field. If provided the
#'     \code{field} name will be treated as main field and nested list
#'     structure is assumed.
#' @return character, a JSON query object.
#' @export
buildFieldStatsQuery <- function(field, sub_field) {
    stopifnot(!missing(field))

    ## start query
    qr <- '['

    ## add unwinding if subfield is provided
    if (!missing(sub_field)) {
        ## unwinding request
        qr <- paste0(qr, '{\"$unwind\":"$', field, '\"},')

        ## adjust field
        field <- paste0('\"$', field, ".", sub_field)
    } else
        field <- paste0('\"$', field)

    ## update the query with group id
    qr <- paste(qr, '{\"$group\":{\"_id\":\"null\",')

    ## add stats
    max_ <- paste0('\"max_\":{\"$max\":', field, '\"}')
    min_ <- paste0('\"min_\":{\"$min\":', field, '\"}')
    avg_ <- paste0('\"avg_\":{\"$avg\":', field, '\"}')
    sd_ <- paste0('\"sd_\":{\"$stdDevPop\":', field, '\"}')
    stats <- paste(max_, min_, avg_, sd_, sep = ", ")
    qr <- paste0(qr, stats)
    
    ## finalise the query
    paste0(qr, '}}]')
}
