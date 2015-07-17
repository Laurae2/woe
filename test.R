library(devtools)
remove.packages("woe")

install_github("oriono/woe")

# the following install seems wired. not sure where it installs the package. 
# install.packages(
#   "~/Documents/Fun/woe"
#   , repos = NULL
#   , type = "source"
# )

setwd("/Users/zzhao/Documents/Fun/")
# install from local
install("woe")
library(woe)

# http://www.r-bloggers.com/r-credit-scoring-woe-information-value-in-woe-package/

iv.mult(german_data,"gb",cutoff=0.05)

iv.mult(german_data,"gb")

iv.plot.summary(iv.mult(german_data,"gb",TRUE))
iv.mult(german_data,"gb",vars=c("housing","duration"))

iv.mult(german_data,"gb",TRUE,vars=c("housing","duration"),rcontrol=rpart.control(cp=0.003))

iv.plot.woe(iv.mult(german_data,"gb",vars=c("housing","duration"),summary=FALSE,rcontrol=rpart.control(cp=0.003)))

iv.plot.woe(iv.mult(german_data,"gb",vars=c("housing","duration"),summary=FALSE))

## testing missing data handling 
a <- german_data

a$duration[a$duration==12] <- NA
a$housing[a$housing=="for free"] <- NA
a$housing <- as.factor(ifelse(is.na(a$housing),"Mssing",a$housing))
table(german_data$housing)

iv.mult(a,"gb",TRUE,vars=c("housing","duration"))
iv.mult(german_data,"gb",TRUE,vars=c("housing","duration"))
sum(is.na(a$duration))
sum(is.na(german_data$duration))

iv.str(german_data,"housing","gb")
b <- german_data


iv.str(a,"housing","gb")
iv.num(german_data,"duration","gb")
iv.num(a,"duration","gb")s

sum(!is.na(a[a$duration > 43.5 &a$gb =="bad",1]))
length(a[a$duration >43.5,1])
length(a[a$duration <=43.5&a$duration >=8.5,1])
length(a[,1])


# test iv.num block 





# test block ends



gllevels(b$housing)
attributes(b$housing)

table(b$housing)
