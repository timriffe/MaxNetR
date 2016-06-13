
# Author: tim
###############################################################################


# the data we're going to play with was compiled straight from the 
# Human Fertiltiy Database into R like so:
# install.packages("HMDHFDplus")
library(HMDHFDplus)
Countries <- unique(c(getHFDcountries(),"ESP","IRL")) # later two are in prelim status, but OK
# you just need to register and define the objects
# pw (password)
# us (username) as text strings.
# Then it downloads and sticks it together
HFD <- do.call(rbind, lapply(Countries, function(XYZ, .us, .pw){
					Dati <- readHFDweb(XYZ, "asfrRR",username = .us, password = .pw)
					Dati$CNTRY <- XYZ
					Dati
				}, .us = us, .pw = pw))
# crap, the orig file doesn't contain exposures...
Exp <- do.call(rbind, lapply(Countries, function(XYZ, .us, .pw){
					Dati <- readHFDweb(XYZ, "exposRR",username = .us, password = .pw)
					Dati$CNTRY <- XYZ
					Dati
				}, .us = us, .pw = pw))
# quick check to make sure dims match and sorted same:
nrow(HFD) == nrow(Exp)
HFD          <- HFD[with(HFD, order(CNTRY, Year, Age)), ]
Exp          <- Exp[with(Exp, order(CNTRY, Year, Age)), ]

# this better be TRUE!!!
all(with(HFD, paste0(CNTRY,Year,Age)) == with(Exp, paste0(CNTRY,Year,Age)))

HFD$Exposure <- Exp$Exposure
HFD$Births   <- HFD$Exposure * HFD$ASFR
# now save it out:
save(HFD, file = "")
##############################