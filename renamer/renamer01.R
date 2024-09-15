library(oce)
# NB ctd.sbe.R:39 is wrong; no such function exists (yet)
# vocab <- read.csv("sbe.csv",
#    header = FALSE,
#    col.names = c("original", "oce", "unit", "scale")
# )

# The system is that ~ can represent a digit
# vocab$pattern <- paste0("^", gsub("~", "[0-9]", vocab$original), "$")
# if (debug > 0) print(vocab)

#' Rename variables according to a specified dictionary
renamerTest1 <- function(names, dictionary = "sbe.csv", debug = 0) {
    debug <- min(3L, max(debug, 0L))
    if (is.character(dictionary) && grepl(".csv$", dictionary)) {
        oceDebug(debug, "renamerTest1() reading dictionary file \"", dictionary, "\"\n", sep = "")
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
    rval <- vector("list", length(names))
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
            rval[[i]] <- list(name = name, unit = "", scale = "")
        } else {
            oceDebug(debug, "this name matches vocabulary item at index w=", w, "\n")
            oceDebug(debug-1, vectorShow(vocab[w, ]))
            unit <- try(parse(text = vocab$unit[w]), silent = TRUE)
            rval[[i]] <- if (inherits(unit, "try-error")) {
                list(name = vocab$oce[w], unit = vocab$unit[w], scale = vocab$scale[w])
            } else {
                list(name = vocab$oce[w], unit = parse(text = vocab$unit[w]), scale = vocab$scale[w])
            }
        }
    }
    rval
}

# Test 1
newNamesAndUnits <- renamerTest1(c("accM", "oxsatMg/L"), "sbe.csv", debug = 0)
stopifnot(all.equal(newNamesAndUnits[[1]]$name, "acceleration"))
stopifnot(all.equal(newNamesAndUnits[[2]]$name, "oxygen"))
str(newNamesAndUnits, give.attr = FALSE)

d <- read.netcdf("CTD_CAR2023011_001_496780_DN.ODF.nc")
oldNames <- names(d[["data"]])
newNames <- renamerTest1(oldNames, "ioos.csv", debug=0)
df <- data.frame(new=unname(sapply(newNames, \(x) x$name)), old=oldNames)
df
message("FIXME: Q values, OXYOCPVL01, IRRDUV01, CPHLPR03, AHSFZZ01, RecPerBin")
