# 01 files

Tests with new inst/extdata/dictionary_sbe.csv file, specifying it directly to
avoid slow rebuilds for each test. Since I am not (yet) ready to slice out the
long conditional block, I copied information from
inst/extdata/d201211_0011.cnv.gz into a local csv file so I could read it
outside oce.

Things are going well *except* for the accented-e problem with this CNV file.
In R/ctd.sbe.R I handle that by doing something extra instead of a regexp on
the name.  But I *really* want to get things working on name alone.  That means
I need to figure out how to match that non-UTF character.  The trick is that
this is going to have to work on all machines, and I recall problems with that
before.  See the extensive documentation in R/ctd.sbe.R for more.
