library(oce)
source("read.ctd.aml.testing.R")

if (!interactive()) png("test04_%02d.png")
files <- list.files(pattern = "*.csv")

for (file in list.files(pattern = "*.csv")) {
    ctd <- read.ctd.aml.type3(file) |> ctdTrim()
    plot(ctd[["salinity"]], type = "o", cex = 0.5)
    title(file)
}
