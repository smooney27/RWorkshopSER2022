# Slide 5

# Data used in this example:  https://biolincc.nhlbi.nih.gov/studies/dig/)

# Read in the digitalis investigation group data file
dig <- read.csv("http://www.columbia.edu/~sjm2186/EPIC_R/dig.csv")
# Look at the first six observations
head(dig)
# Look at the structure of the data file in a couple of different ways:
# 1. Rows
nrow(dig)
# 2. Columns
ncol(dig)
# 3. Dimensions (rows and columns)
dim(dig)
# 4. Column names
colnames(dig)
# Notice: the length of the vector of columns is the number of columns
length(colnames(dig))
ncol(dig)
# 5. Structure of the dataset
str(dig)
# 6. Simple 2x2 table
table(dig$TRTMT, dig$DEATH)
# 7. Some other descriptives
range(dig$AGE)
mean(dig$AGE)
sd(dig$AGE)
median(dig$AGE)
hist(dig$AGE)
# 8. Chi-squared test
chisq.test(table(dig$TRTMT, dig$DEATH))
# 9. Linear regression predicting diastolic blood pressure by age:
# Age is not a statisitcally significant predictor 
model <- lm(DIABP ~ AGE, data=dig)
summary(model)
# But BMI is (barely)
model <- lm(DIABP ~ BMI, data=dig)
summary(model)

# Slide 6
help(table)
?table

# Slide 10
c()
c(1,2,3)
c(FALSE, TRUE)
c(F,T)

myvector <- c("a", "b", "c")
logicalvector <- c(FALSE)

# Slide 11
dig$AGE

# Slide 12
vec <- c(1, 3, 5)
vec
vec[1] # 1
vec[2] # 3
vec[3] # 5

# Slide 14: Challenge #1
myvec <- c(1, 4, 7)
myvec[2]

# Slide 17
mymatrix <- matrix(c("a", "b", "c", "d"), nrow=2)

x <- c(1, 2, 3, 4)
matrix(x, nrow=2)
table(dig$TRTMT, dig$DEATH)

# Slide 18
x <- c(1,2,3)
y <- c(4,5,6)
cbind(x,y)
rbind(x,y)

# Slide 19
mtx <- matrix(c(1, 3, 5, 7), nrow=2)
mtx
mtx[1,1] # 1
mtx[2,2] # 7


# Slide 21
myarray <- array(1:12, dim=c(2, 3, 2))
                 

# Slide 22
array(1:12, dim=c(2, 3, 2))
array(1:12, dim=c(2, 2, 3))

y <- c("a","b","c","d","e","f","g","h","i","j","k","l")
array(y, dim=c(2, 2, 3))


# Slide 23: Challenge #2



# First Break

  
# Slide 26: Warmup Challenge: Construct a 4 unit vector wherein the first value 
# is 20, the second is 40, the third is 60 and the fourth is 80


# Slide 28
x <- c(1, 4, 3)
x * 2
x + 3
x/2
x/x
x + c(1, 3, 4)
x + c(1, 2) 

# Slide 29
sum(x)
cumsum(x)
sort(x)
var(x)
sd(x)
length(x)

# Slide 30
mymatrix <- matrix(c(10,190,10,390), nrow=2, byrow=T)
mymatrix
# Chi-square test on the matrix
chisq.test(mymatrix)
# Proportions of total in the given cell
prop.table(mymatrix)
# Row-wise proportions
prop.table(mymatrix, margin=1)
# Column-wise proportions
prop.table(mymatrix, margin=2)

# Slide 33
x <- c(1, 3, 2, 1, 3)
factor(x, labels=c("low", "medium", "high"))

# Slide 35
namedvec <- c(first=1, second=2)
namedvec

# Slide 36
namedvec[1]
namedvec[c(1,2)]
namedvec['first']
namedvec[c(TRUE, FALSE)]

# Slide 37
c("word", 3)
mode(c("word", 3))

# Slide 38
c(2.3, FALSE)

# Slide 40
mylist <- list(1, "word", TRUE)
mylist

# Slide 41
mylist <- list(1, 
               c("word 1", "word 2"), 
               TRUE)


# Slide 43
myvec <- c(2, 3, 4)
mean(myvec)

mylist <- list(2, 3, 4)
mean(mylist) # invalid

list(2, c(2,3,4))
c(2, c(2,3,4))

# Slide 44
mylist <- list(2, 3, c(2, 4))
mylist[[1]]
mylist[[3]]
mylist[[3]][2]

# Slide 45
results <- list(pval=0.5, 
                test="fisher exact test", 
                df=23)
results[[1]]
results[['pval']]
results$pval

# Slide 46: Challenge: Compute the mean of 1,3, 5, 7, and 9.


# Slide 47: Challenge: Create a list with three items, where:
# The first item is named values and is a vector containing 1, 5, and 9
# The second item is named mean and is the mean of 1, 5, and 9
# The third item is named pval and has the value 0.163


# Slide 49
df <- data.frame(nums=1:5, words=c("one", "two", "three", "four", "five"))

# Slide 50
df[1,2]
df[2,1]
df[1,"words"]

# Slide 51
df$words
df['1',]

