encoding <- "latin1"
csv <- "01_test_sigma_theta.csv"
names <- strsplit(readLines(csv, encoding = encoding), ",")[[1]]
print(names)
# next is no good because we are asked to avoid useBytes (issue 1977)
grep("sigma-\xe900", names, perl = TRUE, useBytes = TRUE)
# next fails
grep("sigma-\xe900", names, fixed = TRUE)
# next works but that means oce code has non-UTF chars, which
# is not permitted
grep("sigma-é00", names, perl = TRUE)#, useBytes = TRUE)
# Next works.  Maybe this is okay, given the range of names
# I've seen.
grep("sigma-[^0:9]00", names, perl = TRUE)#, useBytes = TRUE)
