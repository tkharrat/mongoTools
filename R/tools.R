## =============================================================================
## ----------------------------- Tool functions from MongoDB -------------------
## =============================================================================
.buildAllKeys <- function(keys, values, range = FALSE, type) {
    .fct <- function(i) {
        if (range[i])
            .buildOneKeyRange(keys[i], values[[i]], type[i])
        else
            .buildOneKey(keys[i], values[[i]], type[i])
    }

    paste(sapply(1:length(keys), .fct), collapse = ",")
}

.convertCharacter <- function(char) {
    paste0('\"', char, '\"')
}

.convertLogical <- function(flag) {
    ifelse(flag, "true", "false")
}

.buildOneKey <- function(key, value, type) {
    if (type == "equality") {
        value <- ifelse(is.character(value),
                        .convertCharacter(value),
                 ifelse(is.logical(value), .convertLogical(value), value)
                 )
        paste(.convertCharacter(key), value, sep = ": ")
    } else if (type == "exists") {
        stopifnot(is.logical(value))
        paste(.convertCharacter(key),
              .getExistsType(.convertLogical(value)),
              sep = ": ")
        
    } else
        paste(.convertCharacter(key),
              .getValueType(type, value),
              sep = ": ")
}

.buildOneKeyRange <- function(key, values, type) {
    ## only equality and intervals are accepted
    stopifnot(type %in% c("equality", "intervals"))
    if (any(is.character(values)))
        values <- sapply(values, .convertCharacter)
    
    if (type == "equality") {
        values <- paste0(values, collapse = ", ")
        valuesRange <- paste0("[", values, "]")
        
        rhs <- paste0('{"$in": ', valuesRange, '}')
    } else { ## intervals
        stopifnot(length(values) == 2)
        rhs <- paste0('{',
                      .getValueType("greater_than_equals", min(values), FALSE),
                      ", ",
                      .getValueType("less_than_equals", max(values), FALSE),
                      '}')
    }

    ## get left hand side
    lhs <- .convertCharacter(key)

    ## conclude
    paste(lhs, rhs, sep = ": ")
}

.getExistsType <- function(value) {
    paste0('{', paste0('"$exists": ', value), '}')
}

.getValueType <- function(type, value, addBrackets = TRUE) {
    out <- switch(type,
                  "not_equals" = {paste0('"$ne": ', value)},
                  "less_than" = {paste0('"$lt": ', value)},
                  "less_than_equals" = {paste0('"$lte": ', value)},
                  "greater_than" = {paste0('"$gt": ', value)},
                  "greater_than_equals" = {paste0('"$gte": ', value)}
                  )

    if (addBrackets) out <- paste0('{', out, '}')

    out
}

.getDateValue <- function(dateChar, zero = TRUE) {
    if (nchar(dateChar) == 10 & grepl("-", dateChar)) {
        if (zero)
            dateChar <- paste0(dateChar, "T00:00:00Z")
        else
            dateChar <- paste0(dateChar, "T23:59:59Z")
    }

    paste0('{"$date": ', .convertCharacter(dateChar), '}')
}

.createDateQueryFromChar <- function(date, date_side) {
    dateIn <- date
    dateP1 <- as.Date(as.integer(unclass(date)) + 1L,
                      origin = as.Date("1970-01-01"))
    dateInChar <- format(dateIn, "%Y-%m-%d")
    dateP1Char <- format(dateP1, "%Y-%m-%d")
    
    valueIn     <- .getDateValue(dateInChar)
    valueInLast <- .getDateValue(dateInChar, zero = FALSE)
    valueP1     <- .getDateValue(dateP1Char)
    
    lhs_large <- .getValueType("greater_than_equals", valueIn,     FALSE)
    lhs       <- .getValueType("greater_than",        valueInLast, FALSE)
    rhs       <- .getValueType("less_than",           valueP1,     FALSE)
    rhs_      <- .getValueType("less_than",           valueIn,     FALSE)
    rhs_large <- .getValueType("less_than_equals",    valueInLast, FALSE)

    switch(date_side,
           equality            = paste0('{', lhs_large, ",", rhs, '}'),
           greater_than_equals = paste0('{', lhs_large,           '}'),
           greater_than        = paste0('{', lhs,                 '}'),
           less_than           = paste0('{', rhs_,                '}'),
           less_than_equals    = paste0('{', rhs_large,           '}'),
           stop(paste("date side type", date_side, "not recognised !"))
           )
}

.createDateIntervalQueryFromChar <- function(date_min, date_max,
                                             start_type, end_type) {
    dateInChar <- format(date_min, "%Y-%m-%d")
    dateP1Char <- format(date_max, "%Y-%m-%d")

    ## lhs
    if (start_type == "large") {
        valueIn <- .getDateValue(dateInChar, zero = TRUE)
        lhs <- .getValueType("greater_than_equals", valueIn, FALSE)
    } else {
        valueIn <- .getDateValue(dateInChar, zero = FALSE)
        lhs <- .getValueType("greater_than", valueIn, FALSE)
    }

    ## rhs
    if (end_type == "large") {
        valueP1 <- .getDateValue(dateP1Char, zero = FALSE)
        rhs <- .getValueType("less_than_equals", valueP1, FALSE)
    } else {
        valueP1 <- .getDateValue(dateP1Char, zero = TRUE)
        rhs <- .getValueType("less_than", valueP1, FALSE)
    }
    
    return(paste0('{', lhs, ",", rhs, '}'))
}
