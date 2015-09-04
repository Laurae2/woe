library(devtools)
remove.packages("woe")

#install_github("oriono/woe")

# the following install seems wired. not sure where it installs the package. 
# install.packages(
#   "~/Documents/Fun/woe"
#   , repos = NULL
#   , type = "source"
# )

setwd("/Users/zzhao/Documents/Fun")
# install from local
install("woe")
library(woe)
library(rpart)
# http://www.r-bloggers.com/r-credit-scoring-woe-information-value-in-woe-package/


iv.summary(iv.mult(german_data,"gb"),ivcutoff=0.05)

iv.plot.summary(iv.summary(iv.mult(german_data,"gb"),ivcutoff=0.05))

iv.plot.summary(iv.summary(iv.mult(german_data,"gb",sql = TRUE),ivcutoff=0.05))



iv.mult(german_data,y="gb",vars=varlist(german_data,"numeric"))


iv.mult(german_data,"gb",vars=c("housing","duration"),rcontrol=rpart.control(cp=0.03))



# test iv.replace.woe 

a <- german_data

a$duration[a$duration==12] <- NA
a$housing[a$housing=="for free"] <- NA
a$housing <- as.factor(ifelse(is.na(a$housing),"missing",as.character(a$housing)))
table(a$housing)

x <- iv.replace.woe(a,iv.mult(a,"gb",vars=c('duration','housing'),sql=TRUE))
c<-rbind.fill(iv.mult(a,"gb",ivcutoff=0.05,sql=TRUE))

bb<-iv.replace.woe(iv.replace.woe(a,iv.mult(a,"gb",ivcutoff=0.05,sql=TRUE,topbin=TRUE,tbcutoff=0.2)),iv.mult(a,"gb",ivcutoff=0.05,sql=TRUE))

