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




