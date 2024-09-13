library(oce)
# NB ctd.sbe.R:39 is wrong; no such function exists (yet)
#vocab <- read.csv("sbe.csv",
#    header = FALSE,
#    col.names = c("original", "oce", "unit", "scale")
#)
# The system is that ~ can represent a digit
#vocab$pattern <- paste0("^", gsub("~", "[0-9]", vocab$original), "$")
#if (debug > 0) print(vocab)

#' Rename variables according to a specified dictionary
renamer <- function(s, dictionary="sbe.csv", debug = 0) {
    debug <- min(3L, max(debug, 0L))
    if (is.character(dictionary) && grepl(".csv$", dictionary)) {
        oceDebug(debug, "renamer() reading dictionary file \"", dictionary, "\"\n", sep = "")
        vocab <- read.csv("sbe.csv",
            header = FALSE,
            col.names = c("original", "oce", "unit", "scale")
        )
    }
    # The system is that ~ can represent a digit
    vocab$pattern <- paste0("^", gsub("~", "[0-9]", vocab$original), "$")
    if (debug > 1) {
        print(vocab)
    }
    # Look up
    # FIXME: permit multiple names (to avoid reading the file over and over)
    oceDebug(debug, "searching for an exact name match to \"", s, "\"\n")
    w <- which(s == vocab$original)
    if (length(w) == 0L) {
        oceDebug(debug, "grepping for a pattern match\n")
        w <- which(sapply(vocab$pattern, \(pattern) grepl(pattern, s)))
        oceDebug(debug, vectorShow(w))
        if (length(w) > 1L) {
            warning("multiple name match on \"", s, "\"", sep = "")
            w <- NULL
        }
    }
    if (length(w) == 0L) {
        rval <- list(name = s, unit = "")
    } else {
        oceDebug(debug, "this name matches vocabulary item at index w=", w, "\n")
        oceDebug(debug, vectorShow(vocab[w, ]))
        unit <- try(parse(text = vocab$unit[w]), silent = TRUE)
        rval <- if (inherits(unit, "try-error")) {
            list(name = vocab$oce[w], unit = vocab$unit[w])
        } else {
            list(name = vocab$oce[w], unit = parse(text = vocab$unit[w]))
        }
    }
    rval # FIXME: allow multiples
}
renamer("accM", "sbe.csv", debug = 1)
# renamer("t090C")
# renamer("cond0S/m")
renamer("oxsatMg/L", debug = 1)
# O <- renamer("density")
