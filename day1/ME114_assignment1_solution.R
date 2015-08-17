## Assignment 1 R code, R only version
## ME114, 2015 

## 1. To be done on your system.

## 2.1 
obj1_1 <- read.table(text = "
                             a  b    c    d 
                             1  2  4.3  Yes
                             3 4L  5.1   No
                             ")
## Probably not what we were expecting , since the `a - d` were values rather than variable names.

## 2.2
obj2_1 <- read.table(text = "
                             a  b    c    d 
                             1  2  4.3  Yes
                             3 4L  5.1   No
                             ", header=TRUE)
## `stringsAsFactors=TRUE` reads in the non-numeric data as type `character` rather
##  than creating factors from them.

## 2.3
obj3_1 <- read.table(text = "
                             a  b    c    d 
                             1  2  4.3  Yes
                             3 4L  5.1   No
                             ", header=TRUE, stringsAsFactors=FALSE)
obj3_1$b <- as.integer(obj3_1$b)
obj3_1$d <- factor(obj3_1$d)
str(obj3_1)

## 2.4
obj4_1 <- read.table(text = "
                             a  b    c    d 
                             1  2  4.3  Yes
                             3 4L  5.1   No
                             ", header=TRUE, stringsAsFactors=FALSE)
tmp <- gsub("L", "", obj4_1$b)
obj4_1$b <- as.integer(tmp)
str(obj4_1)

## 2.5
obj5_1 <- data.frame(obj4_1)
str(obj5_1)
## Actually, it was already a `data.frame`.


## 3. Working with the `dplyr` package

## 3.1
require(foreign)
dail2002 <- read.dta("http://www.kenbenoit.net/files/dail2002.dta")

## 3.2
require(dplyr)
dail2002FF <- filter(dail2002, party=="ff")
summary(dail2002FF$party)

## 3.3
FFspend <- select(dail2002FF, spend_total, constituency) %>%
    group_by(constituency) %>% 
    summarise(medspend = median(spend_total))


# Sort and plot the 42 median spending values using an index plot. 
plot(sort(FFspend$medspend), ylab="Median constituency spending for FF")

# For extra credit, do the same using `aggregate` instead of dplyr.
FFspend2 <- aggregate(dail2002FF$spend_total, 
                      list(constituency=dail2002FF$constituency), 
                      median)

## 4. Working with the `reshape2` package
    
library(reshape2)
# rename votes1st
names(dail2002)[which(names(dail2002FF)=="votes1st")] <- "count1"
dail2002melted <- melt(select(dail2002, wholename, district, count1, count2:count16, m), 
                       id.vars = c("wholename", "district", "m"), 
                       variable.name= "count", 
                       value.name = "votes")
# strip off the number after "count" in the count variable
dail2002melted$ncount <- as.numeric(gsub("count", "", as.character(dail2002melted$count)))
dail2002maxcount <- filter(dail2002melted, votes>0) %>%
    group_by(district, m) %>% 
    summarise(maxcount = max(ncount))
# clear relationship between constituency size and number of counts
with(dail2002maxcount, table(m, maxcount))
