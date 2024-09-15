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
    print(vocab$pattern)
    if (debug > 1) {
        print(vocab)
    }
    rval <- data.frame()
    for (i in seq_along(names)) {
        name <- names[i]
        # Look up
        oceDebug(debug, "searching for an exact name match to \"", name, "\"\n")
        w <- which(name == vocab$original)
        print(w)
        print(vocab[w, ])
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
            oceDebug(debug - 1, vectorShow(vocab[w, ]))
            rval <- rbind(rval, c(
                originalName = name,
                oceName = vocab$oce[w], unit = vocab$unit[w], scale = vocab$scale[w]
            ))
            # Leave this to later, so we can return a data frame here
            # unit <- try(parse(text = vocab$unit[w]), silent = TRUE)
            # rval[[i]] <- if (inherits(unit, "try-error")) {
            #    list(name = vocab$oce[w], unit = vocab$unit[w], scale = vocab$scale[w])
            # } else {
            #    list(name = vocab$oce[w], unit = parse(text = vocab$unit[w]), scale = vocab$scale[w])
            # }
        }
    }
    colnames(rval) <- c("originalName", "oceName", "unit", "scale")
    rval
}

# Test 1
test <- renamerTest1(c("accM", "oxsatMg/L"), "sbe.csv", debug = 1)
stopifnot(all.equal(test$oceName, c("acceleration", "oxygen")))

d <- read.netcdf("CTD_CAR2023011_001_496780_DN.ODF.nc")
oldNames <- names(d[["data"]])
R <- renamerTest1(oldNames, "ioos.csv", debug = 0)
newNames <- R$oceNames
df <- data.frame(new = R$oceName, old = R$originalName, check = oldNames)
stopifnot(all.equal(df$old, df$check))
print(df[, 1:2])

message("NEXT: test multiple files, ideally freshly downloaded)")
