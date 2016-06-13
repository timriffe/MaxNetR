
# Author: tim
###############################################################################


# the data we're going to play with was compiled straight from the 
# Human Fertiltiy Database into R like so. I already ran this code so you don't need to
#
# install.packages("HMDHFDplus")
#library(HMDHFDplus)
#Countries <- unique(c(getHFDcountries(),"ESP","IRL")) # later two are in prelim status, but OK
## you just need to register and define the objects
## pw (password)
## us (username) as text strings.
## Then it downloads and sticks it together
#HFD <- do.call(rbind, lapply(Countries, function(XYZ, .us, .pw){
#					Dati <- readHFDweb(XYZ, "asfrRR",username = .us, password = .pw)
#					Dati$CNTRY <- XYZ
#					Dati
#				}, .us = us, .pw = pw))
## crap, the orig file doesn't contain exposures...
#Exp <- do.call(rbind, lapply(Countries, function(XYZ, .us, .pw){
#					Dati <- readHFDweb(XYZ, "exposRR",username = .us, password = .pw)
#					Dati$CNTRY <- XYZ
#					Dati
#				}, .us = us, .pw = pw))
## quick check to make sure dims match and sorted same:
#nrow(HFD) == nrow(Exp)
#HFD          <- HFD[with(HFD, order(CNTRY, Year, Age)), ]
#Exp          <- Exp[with(Exp, order(CNTRY, Year, Age)), ]
#
## this better be TRUE!!!
#all(with(HFD, paste0(CNTRY,Year,Age)) == with(Exp, paste0(CNTRY,Year,Age)))
#
#HFD$Exposure <- Exp$Exposure
#HFD$Births   <- HFD$Exposure * HFD$ASFR
## now save it out:
#save(HFD, file = "/home/tim/git/MaxNetR/MaxNetR/day2morning/HFDall.Rdata")
##############################

# you'll need to change this to whatever your working folder
# is for this session.
setwd("/home/tim/git/MaxNetR/MaxNetR/day2morning")

# this will load an object called HFD into our workspace
load("HFDall.Rdata")
# show all objects in workspace
ls() 

# what do we have here?
str(HFD)
head(HFD)
dim(HFD)

# what countries do we have?
unique(HFD$CNTRY)

# what age-range do we have?
range(HFD$Age)

# --------------------------------------
# let's reshape some. This is a very useful package, and it
# comes with the basic R installation:
library(reshape2)

# acast() creates an array from a long piece of data. It uses a formula syntax:
HFDarray <- acast(HFD, Age~Year~CNTRY, value.var = "ASFR")
dim(HFDarray)
dimnames(HFDarray)
# so we have age by year by country. 
# These are age-period matrices stacked by country

# You can grab any value using names then:

# age 25, 1950, fertility rate for Sweden:
HFDarray["25","1950","SWE"]
# all age 25 fertility rates available in 1950
HFDarray["25","1950", ]
# Sweden 1950 fertility schedule:
HFDarray[, "1950", "SWE"]

# let's make a spaghetti plot
ages <- 12:55
matplot(ages,HFDarray[,,"SWE"],type = 'l')

# all gray and solid lines
matplot(ages,HFDarray[,,"SWE"],
		type = 'l', 
		lty = 1, 
		col = "#00000050")

# add x and y labels, and a title:
matplot(ages,HFDarray[,,"SWE"],
		type = 'l', 
		lty = 1, 
		col = "#00000050",
		xlab = "Age",
		ylab = "ASFR",
		main = "Sweden ASFR since 1891")

# can we highlight a time trend?

years <- as.integer(colnames(HFDarray))
# how about we highlight every 5th year using some color gradient?

yr5 <- years[years %% 5 == 0] # introducing modulo!
# these are integers, but we need characters to select:
yr5c <- as.character(yr5)
# Make a color ramp:
library(RColorBrewer)
display.brewer.all()
# I pick "Blues"
mypalette <- brewer.pal(9,"Blues")[-c(1,2)]
length(mypalette)
length(yr5) # uh oh, we need more blues!!

# introducing the ramp function.
# 1) give it a vector of colors
# 2) tell it which color space to interpolate over:
# 3) it spits back a ramp function for you
myramp <- colorRampPalette(mypalette, space = "Lab")
# it works like this: just ask for N colors 
# and it will interpolate through the space you gave
# and give you N colors in 'even' intervals.
mycolors <- myramp(length(yr5))

# start fresh, duller gray for background lines:
matplot(ages,HFDarray[,,"SWE"],
		type = 'l', 
		lty = 1, 
		col = "#1A1A1A40",
		xlab = "Age",
		ylab = "ASFR",
		main = "Sweden ASFR since 1891")

# the color lines
matplot(ages,HFDarray[,yr5c,"SWE"],
		type = 'l', 
		lty = 1, 
		lwd = 2, # thicker lines
		col = mycolors,
		xlab = "Age",
		ylab = "ASFR",
		main = "Sweden ASFR since 1891") 

# it's cool, but you can't see the light white.
# how about a light gray background? Now that's a pain already:

# 1) empty plot
plot(NULL, 
		type = "n", 
		xlim = range(ages), 
		ylim = range(HFDarray[, yr5c, "SWE"]),
		xlab = "Age",
		ylab = "ASFR",
		main = "Sweden ASFR since 1891")
# draw a grey rectangle for the background?
rect(
		par("usr")[1], 
		par("usr")[3], 
		par("usr")[2], 
		par("usr")[4], 
		col = gray(.92)) # ?rect
# now add the color lines
matplot(ages,HFDarray[, yr5c,"SWE"],
		type = 'l', 
		lty = 1, 
		lwd = 2, # thicker lines
		col = mycolors,
		add = TRUE) 

# but what if I want to keep using that gray background. Do I really need
# to repeat all that code? No! Write a function...
# bg is an argument with a default value
myplot <- function(xlim, ylim, bg = gray(.92), ...){
	plot(NULL, 
			type = "n", 
			xlim = xlim, 
			ylim = ylim, 
			...) # explain the dots!
	rect(
			par("usr")[1], 
			par("usr")[3], 
			par("usr")[2], 
			par("usr")[4], 
			col = bg)
}
# now let's use it:
myplot(xlim = range(ages), 
		ylim = range(HFDarray[, yr5c, "SWE"]),
		xlab = "Age",
		ylab = "ASFR",
		main = "Sweden ASFR since 1891")
matplot(ages,HFDarray[, ,"SWE"],
		type = 'l', 
		lty = 1, 
		lwd = 2, # thicker lines
		col = paste0(myramp(length(years)),60),
		add = TRUE) 
# ahhhh now we see undulations!
# locator(1)
text(28, .13, "undulations?", col = "magenta", srt = -35, cex = 2)
################################################################


