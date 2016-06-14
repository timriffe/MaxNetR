# day 2 morning
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
# you can also do it manually in the tabs above in Rstudio:
# Session/set working directory


# clear all objects in workspace:
rm(list = ls())

# this will load an object called HFD into our workspace
load("HFDall.Rdata")
# show all objects in workspace
ls() 

# what do we have here?
str(HFD)
head(HFD)
dim(HFD)
class(HFD)
summary(HFD)

# what countries do we have?
unique(HFD$CNTRY)

# what age-range do we have?
range(HFD$Age)

# --------------------------------------
# let's reshape some. This is a very useful package, and it
# comes with the basic R installation:
library(reshape2)

# acast() creates an array from a long piece of data. It uses a formula syntax:
# (this avoids a loop)
head(HFD)
HFDarray    <- acast(HFD, Age ~ Year ~ CNTRY, value.var = "ASFR")
dim(HFDarray)
dimnames(HFDarray)
HFDarray[ , , "TWN"]

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
SWE <- HFDarray[,,"SWE"]
dim(SWE)
matplot(ages, HFDarray[,,"SWE"], type = 'l')

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

years     <- as.integer(colnames(HFDarray))
# how about we highlight every 5th year using some color gradient?
yr5       <- years[years %% 5 == 0] # introducing modulo!
# these are integers, but we need characters to select:
yr5c      <- as.character(yr5)


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

#mypalette <- c("white","royalblue","blue")
myramp    <- colorRampPalette(mypalette, space = "Lab")

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
		col = gray(.95)) #
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
		#col = myramp(length(years)),
		 col = paste0(
		   myramp(length(years)),
		   60
		              ),
		add = TRUE) 
# ahhhh now we see undulations!
# locator(1)
text(28, .13, "undulations?
     maybe?", col = "magenta", srt = -35, cex = 2)
# fromxy <- locator(1)
# toxy   <- locator(1)
# arrows(fromxy$x, fromxy$y, toxy$x, toxy$y)

################################################################
# stuff to expand:

# 1) TFR for each year and country, plot as lines.
# (this avoids a loop)
TFR   <- apply(
          HFDarray, 
          3, 
          colSums)

TFR   <- apply(
          HFDarray, 
          c(2,3), 
          sum)


matplot(years, TFR, type = 'l', lty = 1, col = "#00000080")

# 1.1) what is the average TFR per year in the HFD?
# (this avoids a loop)
lines(years, rowMeans(TFR, na.rm = TRUE), col = "red")

# 1.2) what is the population-weighted average TFR per year?
head(HFD)
Bx <- acast(HFD, Age~Year, sum, value.var = "Births")
Ex <- acast(HFD, Age~Year, sum, value.var = "Exposure")
dim(Bx)
dim(Ex)

Fx <- Bx / Ex
# OK, so bigger countries are having a bigger swing up?
lines(years, colSums(Fx), col = "blue")

ls()

# 2) MAB for each year and country, plot as lines
# cool, we can write a function:

wmean <- function(x, w){
	result <- sum(x * w) / sum(w)
	return(result)
}

# 2.1) how does it work?
# fertility rates are the weights
# age is the thing being weighted
wmean(x = 12:55 + .5, w = Fx[,"1891"])
# MAB = mean age at childbearing in a mortality-free stationary population.

# 2.2) so how about all MAB in the HFD?
# here's an esoteric (one of many) R- way of doing this calc:
MAB <- apply(HFDarray, c(2,3), wmean, x = 12.5:55.5)
MAB

# wmeanMat <- function(x, W){
#   result <- colSums(x * W, na.rm = TRUE) / colSums(W, na.rm = TRUE)
#   return(result)
# }
# apply(HFDarray, 3, wmean, x = 12.5:55.5)

# but possibly the easier to read loop way of doing 
# it would look like this:
# 2.2.1) make a container:
MAB2 <- MAB * NA
# iterate over years (y takes steps)
for (y in 1:length(years)){ 
	# iterate over countries
	for (cn in 1:dim(HFDarray)[3]){ 
	  # (cn takes steps)
		# since y and cn are counting integers, we can use
		# them to index our results container.
		# the same y and cn index to our HFD rates array.
		MAB2[y, cn] <- wmean(x = 12.5:55.5, w = HFDarray[, y, cn])
	}
}
#apply(HFDarray, c(2,3), wmean, x = 12.5:55.5)

dim(TFR)
dim(MAB)

# 2.3) let's plot this against TFR:
# 2.3.1) turn these matrices into vectors:
tfr <- c(TFR)
mab <- c(MAB)

# what's the default scatter aesthetic?
plot(tfr, mab)
# how would we like to modify this?
# do we want to animate it?
plot(tfr, 
     mab,
     cex = .5)

# choose solid points
plot(tfr, 
     mab,
     cex = .5,
     pch = 19)

# 
plot(tfr, 
     mab,
     cex = .5,
     pch = 19,
     col = "#00000050")

# TFR, MAB

plot(tfr, 
     mab,
     type = "n",
     xlab = "TFR",
     ylab = "MAB")
countries <- dimnames(HFDarray)[[3]]
for (cn in countries){
  lines(TFR[,cn], 
        MAB[,cn], 
        col = "#0000A150", 
        lwd = .7)
}
points(tfr, mab, 
       cex = .5,
       pch = 19,
       col = "#00000050")
lines(TFR[,"SWE"],MAB[,"SWE"],col = "red")
lines(TFR[,"USA"],MAB[,"USA"],col = "blue")

###############################################
# I don't want to be responsible for neck pain:
plot(tfr, 
     mab,
     cex = .5,
     pch = 19,
     col = "#00000050",
     axes = FALSE,
     xlab = "TFR",
     ylab = "MAB")
ylims <- range(mab, na.rm = TRUE)
xlims <- range(tfr, na.rm = TRUE)
# permission to draw outside of plotting area
par(xpd = TRUE)
# axis line
segments(xlims[1]-.2, 
         ylims[1], 
         xlims[1]-.2, 
         ylims[2])
# artisanal ticks:
yticks <- fivenum(mab)
segments(xlims[1]-.2,
         yticks,
         xlims[1]-.25,
         yticks)
text(xlims[1]-.25, yticks, round(yticks,1), pos = 2)

# again for x axis:
xticks <- fivenum(tfr)
# axis line
segments(xlims[1], 
         ylims[1] - .3, 
         xlims[2], 
         ylims[1]-.3)

# tick marks
segments(xticks,
         ylims[1] - .3,
         xticks,
         ylims[1] - .5)
text(xticks, ylims[1]-.5, round(xticks,1), pos = 1)
# let's do this on the fly

#####################################################
# 3) look at surface: are those really undulations?
#    Hypothesis: these are postponements, and each implies
#    a temporary drop in period measures. If you look
#    at cohort rates, they might not look like undulations at all
dimnames(HFDarray)
SWE <- HFDarray[, , "SWE"]

# 1) the most basic surface function is image().
# it does simple rasters.
image(SWE) # and it does an old-fashioned transpose

# get centered ages
ages.centered   <- ages + .5
# get centered years:
years.centered  <- years + .5
# try again:
image(years.centered, 
      ages.centered, 
	  t(SWE),
	  xlim = range(years) + c(0,1),
	  ylim = range(ages) + c(0,1),
	  asp = 1)
# it gets wide! But the colors aren't so great.
# we could make a new ramp or recycle the old one:
myramp(10)
# there is a cool function called pretty() in R
# that finds 'pretty' break points for you.

pretty(SWE)
pretty(SWE,12) # very useful.
# we need 1 less color than there are breaks
# so lets set some breaks:
brks <- pretty(SWE,12)
# and set some colors:
cols <- myramp(length(brks) - 1)

# retry:
image(years.centered, 
		ages.centered, 
		t(SWE),
		xlim = range(years) + c(0,1),
		ylim = range(ages) + c(0,1),
		asp = 1,
		breaks = brks, # set breaks
		col = cols)    # ans colors

# but I don't want light blue where there are 0s:
SWE[SWE == 0] <- NA
image(years.centered, 
	  ages.centered, 
	  t(SWE),
	  xlim = range(years) + c(0,1),
	  ylim = range(ages) + c(0,1),
      asp = 1,
      breaks = brks, # set breaks and colors
      col = cols,
      xlab = "Year",
      ylab = "Age")    

# OK doesn't make a huge difference.
# one could add a legend, but contours are a bit better methinks:
contour(x=years.centered, 
		    y=ages.centered, 
		z=t(SWE),
		breaks = brks,
		method="flattest",
		col = "#00000080", # OK, but I wish the labels were brighter
		add = TRUE) # this is key

# Lexis grid lines?
# there's actually a lot more one can do with this kind of plot.
# requests?

source("LexRef5.R")
image(years.centered, 
      ages.centered, 
      t(SWE),
      xlim = range(years) + c(0,1),
      ylim = range(ages) + c(0,1),
      asp = 1,
      breaks = brks, # set breaks and colors
      col = cols,
      xlab = "Year",
      ylab = "Age")    
LexRef5(ages,years,col="#FFFFFF40")
# OK doesn't make a huge difference.
# one could add a legend, but contours are a bit better methinks:
contour(x=years.centered, 
        y=ages.centered, 
        z=t(SWE),
        breaks = brks,
        method="flattest",
        col = "#000000", # OK, but I wish the labels were brighter
        add = TRUE) # this is key

###########################################################
# appended from to end from final script

wmean <- function(x, w){
	sum(x * w) / sum(w)
}

# a misnomer. But since the fertility curve
denssd1 <- function(x,w){
	mab <- wmean(x = x, w = w)  # use the other function for mularity
	sqrt(
			sum((x - mab)^2 * w) /  # sum of squared residuals, weighted
					sum(w)              # sum of weights
	)                           # square the results
}

# but actually we could have been even more modular.
# pro-tip: always be a modular as possible! That way you only need
# to change things in one place if you notice a problem
denssd2 <- function(x,w){
	mab   <- wmean(x = x, w = w) # use the other function for mularity
	avvsq <- wmean(x = (x - mab)^2, w = w)
	sqrt(avvsq)
}




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

sdSWE <- apply(SWE,2,denssd2,x=12.5:55.5)
plot(years, sdSWE, type = 'l')

plot(years, MAB[,"SWE"],type = 'l',
     ylim = c(22,40))
polygon(x=c(years, rev(years)),
        y = c(MAB[,"SWE"]-sdSWE, rev(MAB[,"SWE"]+sdSWE)),
        col = paste0(gray(.5),50),
        border = NA)

# plot lower quartile, and upper quartile for
# lower and upper bounds of confidence region

plot(SWE[,1],type='l')
SWE[is.na(SWE)] <- 0
plot(cumsum(SWE[,1]), type='l')

SWEcumsum <- apply(SWE, 2, cumsum)

SWE1 <- t(t(SWEcumsum) / SWEcumsum[nrow(SWEcumsum), ])

spf <- splinefun(x=12.5:55.5~SWE1[,1])
spf(c(.1,.5,.9))
fertquantile <- function(fx, probs = c(.5)){
  spf <- splinefun(x=12.5:55.5~fx)
  spf(probs)
}

SWEq <- apply(SWE1, 2,fertquantile, probs =  c(.1,.5,.9))

plot(years, SWEq[2,], type = 'l',ylim=c(17,42))
lines(years,MAB[,"SWE"], col = "red")
polygon(x=c(years, rev(years)),
        y = c(SWEq[1,], rev(SWEq[3,])),
        col = paste0(gray(.5),50),
        border = NA)

####################################
plot(tfr, 
     mab,
     cex = .5,
     pch = 19,
     col = "#00000050",
     axes = FALSE,
     xlab = "TFR",
     ylab = "MAB")
ylims <- range(mab, na.rm = TRUE)
xlims <- range(tfr, na.rm = TRUE)
# permission to draw outside of plotting area
par(xpd = TRUE)
# axis line
segments(xlims[1]-.2, 
         ylims[1], 
         xlims[1]-.2, 
         ylims[2])
# artisanal ticks:
yticks <- fivenum(mab)
segments(xlims[1]-.2,
         yticks,
         xlims[1]-.25,
         yticks)
text(xlims[1]-.25, yticks, round(yticks,1), pos = 2)

# again for x axis:
xticks <- fivenum(tfr)
# axis line
segments(xlims[1], 
         ylims[1] - .3, 
         xlims[2], 
         ylims[1]-.3)

# tick marks
segments(xticks,
         ylims[1] - .3,
         xticks,
         ylims[1] - .5)
text(xticks, ylims[1]-.5, round(xticks,1), pos = 1)
# let's do this on the fly
mod1 <- lm(mab~tfr)
newdata <- data.frame(tfr=seq(.8,4.5,by=.1))
pred <- predict(mod1, newdata = newdata, se.fit = TRUE)
polygon(x = c( newdata$tfr , rev( newdata$tfr )),
        y = c( pred$fit - pred$se.fit*2, 
              rev( pred$fit +  pred$se.fit *2)),
        col = paste0(gray(.5),50),
        border = NA)
abline(mod1, xpd=FALSE)

###########################################
install.packages("coefplot")
library(coefplot)
coefplot(mod1)
plot(mod1)

data(diamonds)
head(diamonds)

model1 <- lm(price ~ carat + cut*color, data=diamonds)
model2 <- lm(price ~ carat*color, data=diamonds)
model3 <- glm(price > 10000 ~ carat*color, data=diamonds)
coefplot(model1)
coefplot(model2)
coefplot(model3)
coefplot(model1, predictors="color")
coefplot(model1, predictors="color", strict=TRUE)
coefplot(model1, coefficients=c("(Intercept)", "color.Q"))
coefplot(model1, predictors="cut", coefficients=c("(Intercept)", "color.Q"), strict=TRUE)
coefplot(model1, predictors="cut", coefficients=c("(Intercept)", "color.Q"), strict=FALSE)
coefplot(model1, predictors="cut", coefficients=c("(Intercept)", "color.Q"), 
         strict=TRUE, newNames=c(color.Q="Color", "cut^4"="Fourth"))
coefplot(model1, predictors=c("(Intercept)", "carat"), newNames=c(carat="Size"))
coefplot(model1, predictors=c("(Intercept)", "carat"), 
         newNames=c(carat="Size", "(Intercept)"="Constant"))
####################################################



####################################################
# time out, how about that again, but for matrices?
# the same thing for matrices.
Wmean <- function(x,w){
	# add a nice check for conformity:
	stopifnot(length(x) == nrow(w) | 
					nrow(x) == nrow(w)   | 
					nrow(x) == length(w) | 
					length(x) == length(w))
	xW <- x * w
	colSums(xW, na.rm = TRUE) / colSums(w, na.rm = TRUE)
}
Wmean(12.5:55.5, SWE)


# cool, I can overwrite wmean to work like this and get the sd for each year:
wmean <- Wmean

x <- 12.5:55.5
w <- SWE