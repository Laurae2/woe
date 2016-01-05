#' Plot WoE patterns for variables
#'
#' Plots calculated WoE for every variable
#'
#' @param iv WoE data for variables - output from iv.mult with summary=FALSE (false is default for iv.mult)
#' @export
#' @examples
#' iv.plot.woe(iv.mult(german_data,"gb"))
#' iv.plot.woe(iv.mult(german_data,"gb",vars=c("ca_status","housing","age","duration"),summary=FALSE))

iv.plot.woe <- function(iv) {
  x <- rbind.fill(iv)
  # add row index for reorder factor level later 
  # change from x$idx <- as.numeric(rownames(x)) since there might be levels with same notation..which will mess up the order
  x$idx <- ave(as.numeric(rownames(x)),x$variable,FUN = rank)
  woe <- NULL
  ggplot(data=x) + geom_bar(aes(y=woe,x=reorder(class,idx),fill=woe),stat="identity",position="identity") + 
    facet_wrap(facets=~variable, scales="free_x")  +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(linetype="dashed",colour="grey"),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    xlab("Variable") +
    ylab("WoE") +
    ggtitle("Weight of Evidence (WoE) Patterns")
}

