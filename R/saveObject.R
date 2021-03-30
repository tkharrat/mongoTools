#' @name insert_object
NULL

#' Insert an RDS file in mongoDB
#'
#' Insert an RDS file in mongoDB using the GridFS technique.
#'
#' Connects to a GridFS prefix and adds the RDS file to it. The name of
#' the file must have extention \code{".RDS"}, an error is thrown
#' otherwise.
#'
#' The return value is as from the write() method for GridFS objects. In
#' addition, it has attribute \code{"att"}, which is a data frame with
#' one row containing some arguments from the call (\code{remotename},
#' \code{db}, \code{prefix}) and the GridFS object returned by
#' \code{mongolite::gridfs}.
#'
#' @param objectPath character, path to the RDS file to insert.
#' @param remotename character, the name the file will be known as
#'     within the GridFS.
#' @param metadata character (in JSON format), extra info to store.
#' @param db character, database name.
#' @param prefix identifies the GridFS \code{.chunks} and
#'     \code{.files} data collections.
#' @param verbose logical, should progress messages be printed to the
#'     screen ?
#' @return logical giving the insertion status (2019-07-17 @Tarak: I
#'     believe that the value is not logical, see Details above and
#'     delete the wrong one); it also has a data.frame attribute
#'     \code{att} with the function arguments \code{objectPath},
#'     \code{remotename}, \code{db} and \code{prefix}.
#' @rdname insert_object
#' @export
insert_object_inDB <- function(objectPath, remotename, metadata,
                               db = "models", prefix = "fs", verbose = TRUE) {
    ## only RDS objects are accepted
    stopifnot(grepl("[.]RDS$", objectPath))

    ## check the metadata
    with_metadata <- FALSE
    if (!missing(metadata)) {
        test_JSON <- jsonlite::validate(metadata)
        if (test_JSON)
            with_metadata <- TRUE
        else
            warning("metadata passed is not valid JSON")
    }

    ## connect to GridFS
    fs <- gridfs(db = db, prefix = prefix)

    ## write the object to db
    write_args <- list()
    write_args$con <- objectPath
    write_args$name <- remotename
    write_args$progress <- verbose
    if (with_metadata)
        write_args$metadata <- metadata

    out <- do.call(fs$write, write_args)

    att <- data.frame(remotename = remotename, db = db, prefix = prefix,
                      stringsAsFactors = FALSE)
    l <- list()
    l[[1]] <- fs
    att$fs <- l
    attr(out, "att") <- att
    out
}

#' Read a GridFS inserted object from mongoDB
#'
#' Read a GridFS inserted object from mongoDB.
#'
#' @rdname insert_object
#' @export
read_object_fromDB <- function(remotename, db = "models", prefix = "fs",
                               verbose = TRUE) {
    ## connect to GridFS
    fs <- gridfs(db = db, prefix = prefix)

    ## extract the object
    tmp_file <- tempfile()
    out <- fs$read(remotename, tmp_file, progress = verbose)
    object <- readRDS(tmp_file)
    attr(object, "status") <- out

    object
}
