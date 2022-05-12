
# Slide 2
data(infert)

# Slide 3
dig <- read.csv("http://www.columbia.edu/~sjm2186/EPIC_R/dig.csv")


# Slide 12 - exploring data frames
head(infert)
tail(infert)
str(infert)
rownames(infert)
colnames(infert)
dimnames(infert)
dim(infert)


# Slide 13 - More working with data frames
nrow(infert)
ncol(infert)
length(infert) 
dim(infert)
View(infert)



# Slide 14: Challenge
# URL for a CSV file with data about survival (or not) of 
# passengers on the Titanic: 
# http://www.columbia.edu/~sjm2186/EPIC_R/titanic.csv
# Challenge: read the CSV into R and figure out how many rows of data it has.

titanic<- read.csv("http://www.columbia.edu/~sjm2186/EPIC_R/titanic.csv")
nrow(titanic)
table(titanic$sex, useNA="always")
table(titanic$sex, useNA="no")
table(titanic$age, useNA="always")
table(titanic$age, useNA="no")
table(titanic$age)
table(titanic$age, useNA="ifany")
table(titanic$sex, useNA="ifany")

prop.table(table(titanic$age))
prop.table(table(titanic$age, useNA="always"))


# Slide 15: Common data operations
table(infert$case)
xtabs(~case, data=infert)

table(infert$case, infert$parity)
xtabs(~case+parity, data=infert)

prop.table(table(infert$case))
prop.table(table(infert$case, infert$parity))
prop.table(table(infert$case, infert$parity), margin=1)
prop.table(table(infert$case, infert$parity), margin=2)

# Slide 16 -- Intuition test 
length(table(c(1,2,2,2,2,4))) 



# Slide 17 -- Indexing by position
head(infert)
infert[1,]
infert[c(1,2),]
infert[1:2,]
infert[2:1,]

infert[-1,]
infert[-(1:244),]
infert[-(1:244),c("education", "age")]

# Slide 18 -- Indexing by name
infert["1",]

infert[1,]

# Slide 19 -- More Indexing by name
data(USArrests)
head(USArrests)
dim(USArrests)

USArrests[1,] # works
USArrests["Alabama",] # works
USArrests["New York",] # works
USArrests["1",] # returns an error

# Slide 20
infert[,1]
infert[,1:2]
infert[,'age']
infert[,c(T,T,F,F,F,F,F,F)]

infert$age
infert[,2]


# Slide 21: Indexing by boolean
rows_over_40 <- infert$age > 40
rows_over_40
table(rows_over_40)
infert[rows_over_40,]
infert[infert$age > 40,]
infert[1,2]
infert[1,]
infert[,2]
head(infert)

older_subcohort <- infert[infert$age > 40,]
older_subcohort

# Slide 22: Adding columns
infert$rows_over_40 <- infert$age > 40
head(infert)
table(infert$rows_over_40)
table(infert$rows_over_40, infert$age)


# Slide 23
# Challenge: There is a dataset built into R called esoph.  
# Load it and figure out how many rows it has.

data(esoph)
esoph[2,3]
head(esoph)


# Slide 24 - Caution about vector conversion
class(infert[,1])
class(infert[,c(1,5)])

# Slide 25 - Example where the caution matters
grep("ca", colnames(infert))
ca_colnames <- colnames(infert)[grep("ca", colnames(infert))]
ca_colnames
cat_colnames <- colnames(infert)[grep("cat", colnames(infert))]
cat_colnames
class(infert[, ca_colnames])
class(infert[, cat_colnames])
head(infert)

# Slide 26 - Sort and order
myvec<-c(6,3,2,4,5)
sort(myvec)
order(myvec)

# Slide 27 - using order
ages<-sort(infert$age)
ages
age_order<-order(infert$age)
age_order
infert[ages,]$age # wrong
infert[age_order,]$age # correct
infert[order(infert$age),]$age # correct
age_ordered_infert <- infert[order(infert$age),]
head(age_ordered_infert)

# Slide 28 -- arrange
library(plyr)
infert_by_age <- arrange(infert, age)
head(infert_by_age)


# Slide 29 -- attach
attach(infert) # Not recommended
order(age)

# Slide 30
# Challenge: Load the USArrests dataset (built into R) and print 
# out the list of states, sorted by murder arrest rate 

# Slide 32 -- built in utility functions
str(infert) # show the structure of an object
head(infert) # print the start of a vector/data frame
table(infert$age) # print a frequency table

fivenum(infert$age) # "five number" summary 
summary(infert$age) # generic object summary
t.test(table(infert$age, infert$case)) # run a t-test
lm(case~age, data=infert) # run a linear regression


# Slide 33
mean.ignoreNA <- function(vec) {
  return(mean(vec, na.rm=TRUE))
}

# Slide 35
max(c(1, 3, 2, 1)) # 3
help(max)


library(Amelia)

# Slide 39
births <- read.dta('http://www.columbia.edu/~sjm2186/P9489/births.dta') # Wrong if package not loaded
library(foreign)
births <- read.dta('http://www.columbia.edu/~sjm2186/P9489/births.dta')


# Slide 42
# Challenge:Load the 'epitools' package, load the oswego dataset 
# that's built into epitools, and compute the odds ratio for ill,
# conditional on consuming coffee 

# Slide 43
# Challenge: Load the titanic survivor dataset from 
# http://www.columbia.edu/~sjm2186/P9489/titanic.csv
# Use epitab to compute risk ratios (with confidence intervals) 
# for death for second and third class passengers


