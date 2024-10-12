library(oce)
colnames <- strsplit(readLines("01_test_sigma_theta.csv", encoding = "latin1", n = 1), ",")[[1]]
d <- read.csv("01_test_sigma_theta.csv", encoding = "latin1", skip = 1)
names(d) <- colnames
o <- new("oce")
for (name in names(d)) {
    o@data[name] <- d[name]
}
summary(o)
oo <- rename(o, "~/git/oce/inst/extdata/dictionary_sbe.csv")
summary(oo)
ooo <- rename(o, "sbe")
summary(ooo)
