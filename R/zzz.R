.onAttach <- function(libname, pkgname){
    packageStartupMessage(
        paste("In order to run the examples, make sure you restore \n",
              "your mongoDB with the dump shipped with this package.")
        )
}
