library(oce)
showAll <- TRUE
# NB ctd.sbe.R:39 is wrong; no such function exists (yet)
# vocab <- read.csv("sbe.csv",
#    header = FALSE,
#    col.names = c("original", "oce", "unit", "scale")
# )


#' Rename variables according to a specified dictionary
#' The system is that ~ can represent a digit
renamerTest1 <- function(names, dictionary = "sbe.csv", debug = 0) {
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

# Test 1
test <- renamerTest1(c("accM", "oxsatMg/L"), "sbe.csv")
stopifnot(all.equal(test$oceName, c("acceleration", "oxygen")))

for (file in c(
    "CTD_AT4802_001_1_DN.ODF.nc",
    "CTD_CAR2023011_001_496780_DN.ODF.nc",
    "CTD_HL2010001_10_1_DN.ODF.nc",
    "CTD_NED1996254_003_01_DN.ODF.nc"
)) {
    cat("\n**", file, "**\n", sep = "")
    d <- read.netcdf(file)
    oldNames <- names(d[["data"]])
    R <- renamerTest1(oldNames, "ioos.csv", debug = 0)
    newNames <- R$oceNames
    df <- data.frame(
        oceName = R$oceName, originalName = R$originalName, check = oldNames,
        unit = R$unit, scale = R$scale
    )
    stopifnot(all.equal(df$originalName, df$check)) # not really needed once code works
    bad <- which(df$oceName == df$originalN &
        !(df$originalName %in% c("time", "longitude", "latitude")) &
        !grepl("^sensor", df$originalName))
    if (showAll) {
        print(knitr::kable(df[, c("originalName", "oceName", "unit", "scale")], "pipe"))
        cat("\n")
    } else {
        for (i in bad) {
            cat("* [ ] `", df$originalName[i], "`\n", sep = "")
        }
    }
}

message("NEXT: test multiple files, ideally freshly downloaded)")
message("NEXT: how to align flags with items? (wrt to e.g. salinity2) (also some names wrong)")
