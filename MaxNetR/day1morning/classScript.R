
# my new file, just R

a <- 1+1

####
help("sum")
?sum
sum(1,2,3,3)


#####
?lm

# copied and pasted out of the help file:
ctl     <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt     <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
group   <- gl(2, 10, 20, labels = c("Ctl","Trt"))
class(group)

# linear models and other regression stuff likes factors for categorical variables
group[group=="Trt"]
# BUT you can just use character, because it's more generally useful
group2[group2 == "Trt"]
group2 <- c(rep("Ctl",10), rep("Trt",10))


weight  <- c(ctl, trt)
lm.D9   <- lm(weight ~ group2)
lm.D90  <- lm(weight ~ group - 1) # omitting intercept
# we do this cuz we know it will work! to get an
# example of how to use someone's code

# these kinds of functions usually return lists...
# (that's why you need to know how to work with them)
names(lm.D90 )
str(lm.D90)
#################################

# remove stuff from your workspace
rm(lm.D9, lm.D90)
rm(trt, ctl)

#################################

# make a list, it can hold anything
a <- list(a = 1:5, b = "my_name", 
          c = list(a="Carlos",b="Mine"))

# lots of ways to grab stuff out of the list
a$a
a$c$a
a[["c"]][["a"]]
a[[3]][[1]]
a[[1]]

#################################
# vectors:
x <- 1:10
y <- runif(10)

# this does scatterplot by default
plot(x,y)
plot(x,sort(y))

# take a look at vectors in console
x
y

# the most basic property
length(x)

# we can operate because conformable
x+y

# but watch out! if length is divisible it repeats and doesn't warn!
x2 <- 1:5
x2 + y

# x3 <- runif(1e8)
# hist(x3)
###################################

# data.frame 

myDataFrame <- data.frame(age = 0:10, 
                          sex = "f", 
                          pop = round(
                                  runif(11)*1000
                                )
                          )

# functions execute from the inside out
# take a peek
head(myDataFrame)

# dimensions, called in different ways
dim(myDataFrame)
nrow(myDataFrame)
ncol(myDataFrame)

# grabbing
# myDataFrame$age
# myDataFrame[["age"]]
# myDataFrame[, 1]
myDataFrame[,"age"] # most robust IMO

# indexing
myDataFrame[1:5,"age"]
lessThan5 <- myDataFrame[,"age"] < 5
myDataFrame[lessThan5,]

# watch out for NAs when doing logical indexing:
myDataFrame$age[4] <- NA
lessThan5And <- myDataFrame[,"age"] < 5 & !is.na( myDataFrame[,"age"])
lessThan5Or <- myDataFrame[,"age"] < 5 | is.na( myDataFrame[,"age"])

myDataFrame[lessThan5And,]
myDataFrame[lessThan5Or,]
# 
mean(myDataFrame$pop)

# is.X for asking
is.list(a)

# as.X to coerce something to something else:

# coerces to lowest common denominator data type
myDataFrame2 <- as.matrix(myDataFrame)
##########################################################
# matrix

?matrix # look at help
A <- matrix(data=rnorm(11^2), nrow=11, ncol = 11)
A[nrow(A), ncol(A)] # lower right corner
A[A[,1]>0, ]

# dimensions and length
dim(A)
length(A)

# spits back vector
A[A > 0]

# warns for negatives: zeros become -Inf
# negatives become NaN. Hard to compute more with this
log(A)
# impute 0 or less with NA
A[A<=0]<- NA
log(A)

# named indexing
colnames(A) <- letters[1:ncol(A)]
rownames(A) <- LETTERS[1:nrow(A)]
A

# OR
A <- matrix(data=rnorm(11^2), nrow=11, ncol = 11, 
            dimnames = list(LETTERS[1:ncol(A)], # rows
                            letters[1:nrow(A)])) # columns

# indexing by name:
A["A",c("a","b")]

# dimensions are important:
a <- 0:10
length(a)
dim(A)

# vectors go elementwise into rows
A + a
# and you need to do something fancy to get into the columns
t(t(A) + a)

# basic operators work elementwise
A <- A + 1
A / 2

# matrix multiplication
t(A) %*% a

# male a matrix that is invertible
A <- matrix(rnorm(11^2),11,11)
solve(A)

# also very useful
diag(a)

##################################

setwd("git/MaxNetR")
Unmodel <- read.csv("/home/tim/git/MaxNetR/MaxNetR/day1morning/modelvalues.csv")
class(Unmodel)
head(Unmodel)

matplot(Unmodel$Age,Unmodel[, -1], type = 'l')







