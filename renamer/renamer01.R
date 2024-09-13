library(oce)
debug <- 0
# NB ctd.sbe.R:39 is wrong; no such function exists (yet)
vocab <- read.csv("sbe.csv",
    header = FALSE,
    col.names = c("original", "oce", "unit", "scale")
)
# The system is that ~ can represent a digit
vocab$pattern <- paste0("^", gsub("~", "[0-9]", vocab$original), "$")
if (debug > 0) print(vocab)

renamer <- function(s) {
    w <- which(s == vocab$original)
    if (length(w) == 1L) {
        oceDebug(debug, "got an exact match\n")
        unit <- try(parse(text = vocab$unit[w]), silent = TRUE)
        if (inherits(unit, "try-error")) {
            list(name = vocab$oce[w], unit = vocab$unit[w])
        } else {
            list(name = vocab$oce[w], unit = parse(text = vocab$unit[w]))
        }
    } else {
        oceDebug(debug, "grepping for a pattern match\n")
        w <- which(sapply(vocab$pattern, \(pattern) grepl(pattern, s)))
        oceDebug(debug, vectorShow(w))
        if (length(w) == 0L) {
            list(name = s, unit = "")
        } else if (length(w) == 1L) {
            print(vocab[w,])
            unit <- try(parse(text = vocab$unit[w]), silent = TRUE)
            if (inherits(unit, "try-error")) {
                list(name = vocab$oce[w], unit = vocab$unit[w])
            } else {
                list(name = vocab$oce[w], unit = parse(text = vocab$unit[w]))
            }
        } else {
            warning("multiple name match on \"", s, "\"", sep = "")
            s
        }
    }
}
renamer("accM")
#renamer("t090C")
#renamer("cond0S/m")
#renamer("oxsatMg/L")
#O <- renamer("density")
