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
iv.mult(german_data,"gb",ivcutoff=0.05)
iv.mult(german_data,"gb",ivcutoff=0.05,sql=TRUE,topbin=TRUE,tbcutoff=0.2)
iv.mult(german_data,"gb",ivcutoff=0.05,topbin=TRUE)
iv.mult(german_data,"gb",TRUE)


t <- iv.mult(german_data,"gb")

iv.plot.summary(iv.mult(german_data,"gb",TRUE))
iv.mult(german_data,"gb",vars=c("housing","duration"),excl="housing")

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
c<-rbind.fill(iv.mult(a,"gb",ivcutoff=0.05,sql=TRUE))

bb<-iv.replace.woe(iv.replace.woe(a,iv.mult(a,"gb",ivcutoff=0.05,sql=TRUE,topbin=TRUE,tbcutoff=0.2)),iv.mult(a,"gb",ivcutoff=0.05,sql=TRUE))


# sql output function
  
iv.trans.code <- function(vars,iv) {
  
  iv_df <- rbind.fill(iv)
  
  for (n in vars) { 
    
    if(grepl("_woe",n))
    {  
      sqlstr <- paste("case",paste(iv_df[iv_df$variable==substr(n,1,nchar(n)-4),]$sql_code,collapse= "\n"),"end as",n,",\n\n")
      
    } else {
      
      sqlstr <- paste(paste(iv_df[grepl(as.character(n),iv_df$sql_code),]$sql_code,collapse= "\n"),"\n")
    }
    sink('transformation code.txt',append=TRUE)
    cat(sqlstr)
    sink()
  }
  
}

iv.trans.code(c("duration_woe","housing_woe"),iv.mult(a,"gb",ivcutoff=0.05,sql=TRUE))

