# day 2 afternoon
# Author: tim
###############################################################################

# more functions, plots, and loops
setwd("/home/tim/git/MaxNetR/MaxNetR/day2afternoon")

# this will load an object called HFD into our workspace
load("/home/tim/git/MaxNetR/MaxNetR/day2morning/HFDall.Rdata")

# OK, now let's see some more effective processing of long-format data.

wmean <- function(x, w){
	sum(x * w) / sum(w)
}
# a misnomer. But since the fertility curve
denssd1 <- function(x,w){
	mab <- wmean(x = x, w = w) # use the other function for mularity
	sqrt(
			sum((x - mab)^2 * w) /               # sum of squared residuals, weighted
			   sum(w)              # sum of weights
	)                                            # square the results
}

# but actually we could have been even more modular.
# pro-tip: always be a modular as possible! That way you only need
# to change things in one place if you notice a problem
denssd2 <- function(x,w){
	mab   <- wmean(x = x, w = w) # use the other function for mularity
	avvsq <- wmean(x = (x - mab)^2, w = w)
	sqrt(avvsq)
}

denssd()


# give it a test:
denssd1(x = 12.5:55.5, w = SWE[,"1891"])
denssd2(x = 12.5:55.5, w = SWE[,"1891"])

# ahhh
SWE[,"1891"] # looks like the early years have NAs rather than 0s?
# we can make the function to throw them out:
wmean <- function(x, w){
	sum(x * w, na.rm = TRUE) / sum(w, na.rm = TRUE)
}


denssd1(x = 12.5:55.5, w = SWE[,"1891"]) # fails
denssd2(x = 12.5:55.5, w = SWE[,"1891"]) # works, because we were modular!!!

# So now I think it's a good idea to see how to process blocks of data.

# there are numerous old-fashioned R ways to do this,
#


