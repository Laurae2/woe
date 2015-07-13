library(devtools)

install_github("oriono/woe")

library(woe)

http://www.r-bloggers.com/r-credit-scoring-woe-information-value-in-woe-package/
  
iv.mult(german_data,"gb",TRUE)


iv.plot.summary(iv.mult(german_data,"gb",TRUE))

iv.mult(german_data,"gb",vars=c("housing","duration"))

iv.plot.woe(iv.mult(german_data,"gb",vars=c("housing","duration"),summary=FALSE))



