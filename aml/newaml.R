# Preliminary reading of new (?) AML ctd format. Only location
# is scanned from metadata, but reading other things would be easy.
# The problem is that we do not have documentation on the format,
# so it doesn't seem worthwhile writing a lot of code at this time.
#
# Note, especially, that units are not scanned.

png("newame_%d.png")

library(oce)

read.ctd.aml.testing <- function(file) {
    lines <- readLines(file)
    w <- grep("^Longitude=", lines)
    longitude <- as.numeric(gsub(".*=", "", lines[w]))
    w <- grep("^Latitude=", lines)
    latitude <- as.numeric(gsub(".*=", "", lines[w]))
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
    rval
}

ctd <- read.ctd.aml.testing("2024-09-04_11-02-33.csv")
plot(ctd)
summary(ctd)

# Notice the bad salinities.  Let's explore and trim
hist(ctd[["salinity"]])
badS <- ctd[["salinity"]] < 30 # bad points at end of dataset
ctdTrimmed <- subset(ctd, !badS)
plot(ctdTrimmed)

