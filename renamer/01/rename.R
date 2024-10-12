library(oce)
# NB ctd.sbe.R:39 is wrong; no such function exists (yet)
# vocab <- read.csv("sbe.csv",
#    header = FALSE,
#    col.names = c("original", "oce", "unit", "scale")
# )


renameInternal <- function(names, dictionary = "ioos.csv", debug = 0) {
    debug <- min(3L, max(debug, 0L))
    if (is.character(dictionary) && grepl(".csv(.gz){0,1}$", dictionary)) {
        oceDebug(debug, "renamerInternal() reading dictionary in \"", dictionary, "\"\n", sep = "")
        vocab <- read.csv(dictionary,
            header = FALSE,
            col.names = c("original", "oce", "unit", "scale")
        )
    }
    # The system is that ~ can represent a digit
    vocab$pattern <- paste0("^", gsub("~", "[0-9]", vocab$original), "$")
    if (debug > 1) {
        print(vocab)
    }
    rval <- data.frame()
    for (i in seq_along(names)) {
        name <- names[i]
        # Look up
        oceDebug(debug, "searching for an exact name match to \"", name, "\"\n")
        w <- which(name == vocab$original)
        if (length(w) == 0L) {
            oceDebug(debug, "no exact match, so searching patterns for a match\n")
            w <- which(sapply(vocab$pattern, \(pattern) grepl(pattern, name)))
            oceDebug(debug, vectorShow(w))
            if (length(w) > 1L) {
                warning("multiple name match on \"", name, "\"", sep = "")
                w <- NULL
            }
        }
        if (length(w) == 0L) {
            oceDebug(debug, "\"", name, "\" is not in vocabulary\n", sep = "")
            rval <- rbind(rval, c(originalName = name, oceName = name, unit = "", scale = ""))
        } else {
            oceDebug(debug, "\"", name, "\" matches vocabulary at index ", w, "\n", sep = "")
            oceDebug(debug, vectorShow(vocab[w, ]))
            rval <- rbind(rval, c(
                originalName = name,
                oceName = vocab$oce[w], unit = vocab$unit[w], scale = vocab$scale[w]
            ))
        }
    }
    colnames(rval) <- c("originalName", "oceName", "unit", "scale")
    # rename variables so x,x becomes x,x2 etc, but skip over flags
    isFlag <- grepl("Flag$", rval$oceName)
    rval$oceName[!isFlag] <- unduplicateNames(rval$oceName[!isFlag])
    rval
}

#' Rename variables according to a specified dictionary
#'
#' The is a provisional function, added in September 2024,
#' and likely to change through the end of that year.
#'
#' FIXME: explain columns and that ~ can represent any digit
#'
#' @examples
#' file <- "CTD_AT4802_001_1_DN.ODF.nc"
#' d <- read.netcdf(file)
#' d2 <- rename(d, dictionary = "ioos.csv")
#' summary(d2)
#'
#' @author Dan Kelley
rename <- function(x, dictionary = "ioos.csv", debug = 0) {
    if (!inherits(x, "oce")) stop("x is not an oce-class object")
    rval <- x
    originalNames <- names(x[["data"]])
    R <- renameInternal(originalNames, dictionary = dictionary, debug = debug)
    # set up original names
    names(rval@metadata$dataNamesOriginal) <- R$oceName
    # rename data
    names(rval@data) <- R$oceName
    # rename units
    if (!"units" %in% names(x@metadata)) {
        rval@metadata$units <- list()
    } else {
        names(rval@metadata$units) <- sapply(names(rval@metadata$units), \(n) R$oceName[which(n == R$originalName)])
    }
    # Move calibrations to metadata
    dataNames <- names(rval@data)
    for (name in dataNames) {
        item <- rval@data[[name]]
        if (is.character(item) && 1L == length(item)) {
            rval@metadata[[name]] <- item
            rval@data[[name]] <- NULL
        }
    }
    # Move flags to metadata
    dataNames <- names(rval@data)
    if (is.null(rval@metadata$flags)) {
        rval@metadata$flags <- list()
    }
    for (name in dataNames) {
        item <- rval@data[[name]]
        if (grepl("Flag$", name)) {
            rval@metadata$flags[[name]] <- item
            rval@data[[name]] <- NULL
        }
    }
    rval@processingLog <- processingLogAppend(rval@processingLog, paste(deparse(match.call()), sep = "", collapse = ""))
    rval
}
