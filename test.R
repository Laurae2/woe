library(devtools)
remove.packages("woe")

#install_github("oriono/woe")

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

tt <-iv.mult(german_data,"gb",cutoff=0.05)
iv.mult(german_data,"gb",ivcutoff=0.05,sql=TRUE)
iv.mult(german_data,"gb",cutoff=0.05)
iv.mult(german_data,"gb",ivcutoff=0.05,sql=TRUE,topbin=TRUE,tbcutoff=0.2)
iv.mult(german_data,"gb",ivcutoff=0.05,topbin=TRUE)
iv.mult(german_data,"gb",TRUE)


t <- iv.mult(german_data,"gb")

iv.plot.summary(iv.mult(german_data,"gb",TRUE))
iv.mult(german_data,"gb",vars=c("housing","duration"))

iv.mult(german_data,"gb",TRUE,vars=c("housing","duration"),rcontrol=rpart.control(cp=0.003))

iv.plot.woe(iv.mult(german_data,"gb",vars=c("housing","duration"),summary=FALSE,rcontrol=rpart.control(cp=0.003)))

iv.plot.woe(iv.mult(german_data,"gb",vars=c("housing","duration"),summary=FALSE))

## testing missing data handling 
a <- german_data

a$duration[a$duration==12] <- NA
a$housing[a$housing=="for free"] <- NA
a$housing <- as.factor(ifelse(is.na(a$housing),"missing",as.character(a$housing)))
table(a$housing)

iv.mult(a,"gb",vars=c("housing","duration"))
iv.mult(german_data,"gb",TRUE,vars=c("housing","duration"))
sum(is.na(a$duration))
sum(is.na(german_data$duration))

cc <- iv.num(german_data,"duration","gb")
b <- german_data


iv.str(a,"housing","gb")
system.time(iv.num(german_data,"duration","gb",verbose=TRUE))
iv.num(german_data,"duration","gb",rcontrol=rpart.control(cp=0.003))
system.time(iv.num(a,"duration","gb",verbose=TRUE))


a$obs_id <- row.names(a)


levels(iv.binning.simple(german_data,"credit_amount"))


# test iv.replace.woe 

a <- german_data

a$duration[a$duration==12] <- NA
a$housing[a$housing=="for free"] <- NA
a$housing <- as.factor(ifelse(is.na(a$housing),"missing",as.character(a$housing)))
table(a$housing)

x <- iv.replace.woe(a,iv.mult(a,"gb",vars=c("duration","housing")))

any(grepl("-Inf",as.character(aa[[2]]$class)))

aa[[2]]$class[grep("-Inf",as.character(aa[[2]]$class))]

# sql output function
  
  #char 

  #num 



