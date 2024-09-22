library(oce)
source("rename.R")
url <- "https://cioosatlantic.ca/erddap/files/bio_maritimes_region_ecosystem_survey_ctd/Maritimes%20Region%20Ecosystem%20Survey%20Summer/2023/CTD_CAR2023011_137_497544_DN.ODF.nc"
file <- gsub(".*/", "", url)
if (!file.exists(file)) {
    download.file(url, file)
}
d <- read.netcdf(file)
d2 <- rename(d, dictionary = "ioos.csv.gz")
summary(d2)
