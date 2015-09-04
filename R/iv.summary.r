#' Summary of Information Value for WOE calculated variables
#'
#' Create a summary table of information value of all variables, sorted by IV from highest to lowest 
#' @param woe list from /code{iv.mult()} with /code{topbin=FALSE}
#' @param ivcutoff Only display variables with IV > cutoff in summary
#' @param verbose Prints additional details when TRUE. Useful mainly for debugging.
#' @export
#' @examples
#' iv.summary(iv.mult(german_data,"gb"))
#' iv.summary(iv.mult(german_data,"gb",vars=c("ca_status","housing","job","duration"),sql=TRUE),ivcutoff = 0.005)


iv.summary <- function(woe,ivcutoff=0.001,verbose=FALSE) {

    if (verbose) cat(paste("Preparing summary","\n"))
    woe <- rbind.fill(woe)
    woe <- sqldf("select 
                        variable as Variable,
                        sum(miv) as InformationValue, 
                        count(*) as Bins,
                        sum(case when bad_rate = 0 or bad_rate = 1 then 1 else 0 end) as ZeroBins
                     from woe 
                     group by variable 
                     order by InformationValue desc") 

    woe$Strength[woe$InformationValue >= 1] <- 1
    woe$Strength[woe$InformationValue >= .4 & woe$InformationValue < 1] <- 2
    woe$Strength[woe$InformationValue >= .15 & woe$InformationValue < .4] <- 3
    woe$Strength[woe$InformationValue >= .05 & woe$InformationValue < .15] <- 4
    woe$Strength[woe$InformationValue >= .01 & woe$InformationValue < .05] <- 5
    woe$Strength[woe$InformationValue < .01] <- 6
    woe$Strength <- factor(woe$Strength, levels=c(1,2,3,4,5,6), 
                              labels= c("Suspicious","Very strong","Strong","Average","Weak","Very weak"))
    woe <- woe[woe$InformationValue>ivcutoff,]
    woe
  }