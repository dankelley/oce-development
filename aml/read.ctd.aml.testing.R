#' Read AML ctd format 3 (in development)
#'
#' This is an ad-hoc attempt to read files provided by a user
#' in late September, 2024.  See \dQuote{Details} for some
#' provisos.
#'
#' This function was based on 4 sample files, evidently created
#' with AML Sailfish 1.4.8.0 software. No documentation was
#' made available, so the code was written by inspection
#' of the files and some guessing on the format.  This means
#' that the code is likely to be brittle against
#' file variations.
#'
#' It is not envisioned that much support will be provided for
#' this file format, given the lack of documentation.  This is the
#' third format seen for AML files, and it seems likely that there
#' are other formats in existence. Another factor mitigating against
#' oce adding high support for this format is the
#' fact that the files made available to the author contain
#' startling errors in the stated units of for density and sound
#' speed, which raises questions about the development
#' state of the AML software.
#'
#' @param file character value naming a file.
#'
#' @param encoding ignored.
#'
#' @param debug ignored.
#'
#' @author Dan Kelley
read.ctd.aml.type3 <- function(file, encoding, debug) {
    getMetadata <- function(name, numeric = TRUE, default = NA) { # name is in title-case
        w <- grep(paste0("^", name, "="), lines)
        wlen <- length(w)
        if (wlen == 0) {
            default
        } else {
            if (numeric) as.numeric(gsub(".*=", "", lines[w])) else gsub(".*=", "", lines[w])
        }
    }
    lines <- readLines(file, warn = FALSE)
    # Get metadata (FIXME: possibly get other items)
    longitude <- getMetadata("Longitude")
    latitude <- getMetadata("Latitude")
    # Get data
    w <- grep("^Columns=", lines)
    col.names <- strsplit(gsub(".*=", "", lines[w]), ",")[[1]]
    w <- grep("\\[MeasurementData\\]", lines)
    data <- read.csv(file, skip = w + 1, col.names = col.names)
    rval <- as.ctd(
        salinity = data$Salinity, temperature = data$Temperature,
        pressure = data$Pressure, latitude = latitude, longitude = longitude
    )
    for (name in col.names) {
        if (!name %in% c("Salinity", "Temperature", "Pressure", "Date", "Time")) {
            rval <- oceSetData(rval, tolower(name), data[[name]],
                note = paste("Add", tolower(name))
            )
        }
    }
    if ("Date" %in% col.names && "Time" %in% col.names) {
        time <- as.POSIXct(paste(data[["Date"]], data[["Time"]]), tz = "UTC")
        rval <- oceSetData(rval, "time", time, note = "Add time")
    }
    w <- grep("^Units=", lines)
    units <- strsplit(gsub("^Units=", "", lines[w]), ",")[[1]]
    # FIXME: detect incorrect units, and report if found
    for (i in seq_along(col.names)) {
        if (!col.names[i] %in% c("Salinity", "Temperature", "Pressure", "Date", "Time", "time")) {
            rval@metadata$units[[tolower(col.names[i])]] <- units[i]
        }
    }
    rval
}
