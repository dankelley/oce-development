library(oce)
source("read.ctd.aml.testing.R")

if (!interactive()) png("test03_%02d.png")

for (file in list.files(pattern = "*.csv")) {
    message(file)
    ctd <- read.ctd.aml.type3(file) |> ctdTrim()
    summary(ctd)
    # Use 'unesco' because a sample file lacks location information.
    plot(ctd, eos = "unesco")
}
