library(oce)
source("read.ctd.aml.testing.R")

if (!interactive()) png("test02_%02d.png")

for (file in list.files(pattern = "*.csv")) {
    message(file)
    ctd <- read.ctd.aml.type3(file)
    summary(ctd)
    # Use 'unesco' because a sample file lacks location information.
    plot(ctd, eos = "unesco")
}
