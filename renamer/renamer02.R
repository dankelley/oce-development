library(oce)
showAll <- TRUE
# NB ctd.sbe.R:39 is wrong; no such function exists (yet)
# vocab <- read.csv("sbe.csv",
#    header = FALSE,
#    col.names = c("original", "oce", "unit", "scale")
# )


#' Rename variables according to a specified dictionary
#' The system is that ~ can represent a digit
renamerInternal <- function(names, dictionary = "ioos.csv", debug = 0) {
    debug <- min(3L, max(debug, 0L))
    if (is.character(dictionary) && grepl(".csv$", dictionary)) {
        oceDebug(debug, "renamerTest1() reading dictionary in \"", dictionary, "\"\n", sep = "")
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

renamer2 <- function(o, dictionary = "ioos.csv", debug = 0) {
    if (!inherits(o, "oce")) stop("o is not an oce-class object")
    rval <- o
    originalNames <- names(o[["data"]])
    R <- renamerInternal(originalNames, dictionary = dictionary, debug = debug)
    names(rval@metadata$dataNamesOriginal) <- R$oceName
    names(rval@data) <- R$oceName
    if (!"units" %in% names(o@metadata)) {
        warning("FIXME -- set up units from dictionary")
        units <- list()
    } else {
        warning("FIXME -- rename units")
    }
    warning("FIXME -- handle repeated names")
    if (!"flags" %in% names(o@metadata)) {
        warning("FIXME -- set up flags")
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
    rval
}

file <- "CTD_AT4802_001_1_DN.ODF.nc"
d <- read.netcdf(file)
d2 <- renamer2(d)
summary(d2)
