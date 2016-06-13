setwd("/home/tim/git/MaxNetR/MaxNetR/")
# on windows, separate file paths using double backslashes:
# bla\\bla\\bla.txt

getwd() # this is where you are

# downloaded files from here:
#http://www.ine.es/prodyser/micro_mnp_defun.htm
# 2014 data, with documentation.
# open excel to show how to make useful

# note: we saved this thing out from Excel:
fwf.info <- read.csv("day1afternoon/MetaData.csv")
# only need first four columns:
head(fwf.info)
fwf.info <- fwf.info[, 1:4]

head(fwf.info)
?read.fwf
Dat <- read.fwf("day1afternoon/Anonimizado Defunciones nivel educativo sin causaA2014.TXT",
         widths = fwf.info[,"Long."], col.names = fwf.info[,"Variable"])

head(Dat)
dim(Dat)

# some columns are just NAs.
# 1) is.na() asks each value if it's an NA: T/F
# 2) TRUE = 1, so colSums() us how many TRUE are iun the column
# 3) if the number TRUE is equal to the length of rows, then we mark for removal
onlyNAs <- colSums(is.na(Dat)) == nrow(Dat)
onlyNAs
names(onlyNAs) <- NULL
# the opppsite TF value is needed to positively select columns
Dat <- Dat[,!onlyNAs]
dim(Dat)


# print(object.size(Dat),units="Mb")
# 1) make date class columns for birth and death. We have month and year info, so
# make all dates mid-month:

Dat$Bday <- as.Date(
                paste(Dat$ANON, Dat$MESN, "15", sep = "-")
                    )
range(Dat$Bday) # oldest was born in 1901!

Dat$Dday <- as.Date(
               paste(Dat$ANODEF, Dat$MESDEF, "15",sep="-")
             )
range(Dat$Dday)
head(Dat)
# 2) calculate lifespan (approx)
Dat$L <- (Dat$Dday - Dat$Bday) / 365.25
range(as.integer(Dat$Bday))
class(Dat$Dday)

# 2.1) what class is L anyway:
class(Dat$L) # yikes
Dat$L <- as.numeric(Dat$L) # this I understand

range(Dat$L)

# 3) age at death in completed years:
Dat$A <- floor(Dat$L)

# 4) recode Sexo to "m", "f"

Dat$SEXO <- ifelse(Dat$SEXO == 1, "m",   # if 1, then "m"
                   ifelse(Dat$SEXO == 6, "f", # if 6 then "f"
                          NA)) # otherwise NA
table(Dat$SEXO)
class(Dat$SEXO)

# 5) take a look with a histogram: Deaths by sex and age:
hist(Dat$A[Dat$SEXO == "f"], 
     col = "#FF000050", 
     border = NA, 
     main = "Deaths by age",
     xlab = "Age",
     ylab = "Count")

hist(Dat$A[Dat$SEXO == "m"], 
     col = "#0000FF50", 
     border = NA,
     add = TRUE)

# and seasonality?
barplot(
  table(Dat$MESDEF),
  space=0, 
  border = NA)

# lifelines on the Lexis diagram?
# start an empty device
plot(NULL, 
     type = "n", 
     xlim = range(as.integer(Dat$Bday) / 365.25) + 1970, 
     ylim = c(0, 113),
     asp = 1)
# select 10k random people
random.people <- sample(1:nrow(Dat), size = 10000, replace = FALSE)
# draw lifelines
segments(as.integer(Dat$Bday[random.people]) / 365.25 + 1970,  # x0
         0,                                                    # y0
         as.integer(Dat$Dday[random.people]) / 365.25 + 1970,  # x1
         as.integer(Dat$Dday[random.people]) / 365.25  -       # y1
           as.integer(Dat$Bday[random.people]) / 365.25  ,     # low visibility color
         col = "#00000005",
         lwd = .5)

######################################################
# cut out NA provinces
Dat       <- Dat[!is.na(Dat$CPRON), ]
# get unique provinces
provinces <- sort(unique(Dat$CPRON))

# make a container to save results into
result    <- rep(0, length(provinces))
# a first loop
for (pr in 1:length(provinces)){ # pr <- 1
  Dati       <- Dat[Dat$CPRON == provinces[pr], ]
  result[pr] <- mean(Dati$L)
}

fivenum(result)



# Now, first attempt to read in Matthias' data:
library(foreign)
?read.dta
?foreign
DHS <- read.dta("day1afternoon/Columbia DHS wave 5_FILES/COIR53FL.DTA")
head(DHS)
# it turned out to need either a re-labelling or a code book
# or something. Yikes. Very wide data indeed.
head(colnames(DHS))
dim(DHS)
attr(DHS,"var.labels")
str(DHS)
colnames(DHS)
attr(DHS,"col.labels")
"sh105" %in% colnames(DHS)
colnames(DHS)[grepl("sh",colnames(DHS))]
colnames(DHS)

nrow(DHS)



#################################
# Unmodel <- read.csv("day1morning/modelvalues.csv",
#                     )
# class(Unmodel)
# head(Unmodel)
# 
# matplot(Unmodel$Age,Unmodel[, -1], type = 'l')
# 
# expit <- function(x){
#   exp(x)/(1+exp(x))
# }
# matplot(Unmodel$Age,1-expit(Unmodel[, -1]), type = 'l')
# 
# 
# 
# 
# 
