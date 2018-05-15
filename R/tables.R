# tables.R - DESC
# /tables.R

# Copyright European Union, 2018
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.


# TODO final table
# mean(performance) for 1, 5, 10, 20 years

# TODO initial table 1
# mean(perf) by indicator
# - CONVERT 0 to < 0.01
# - CONVERT 1 to > 0.99

# summTable {{{

summTable <- function(data,  indicators=c("S3", "S6", "F2", "Y1", "T2"),
  probs=c(0.10, 0.50, 0.90), ...) {

  # SUBSET indicators
  data <- data[indicator %in% indicators,]
  
  # CALCULATE quantiles
  qdata <- data[,as.list(quantile(data, probs=probs, na.rm=TRUE)),
    keyby=list(indicator, name, mp)]
  
  qdata[, fig:=paste0(format(`50%`, digits=2, scientific=FALSE, trim=TRUE), " (",
    format(`10%`, digits=2, scientific=FALSE, trim=TRUE), "-",
    format(`90%`, digits=2, scientific=FALSE, trim=TRUE), ")")]
  
  qtab <- dcast(qdata, mp ~ name, value.var = "fig")
  
  # CALCULATE means
  mdata <- data[, .(fig=format(mean(data), digits=2, scientific=FALSE, trim=TRUE)),
    keyby=list(indicator, name, mp)]
  
  mtab <- dcast(mdata, mp ~ name, value.var = "fig")
  
  # ASSEMBLE table
  tab <- cbind(qtab[,1], qtab[,4], mtab[,2], mtab[,3], qtab[,6], mtab[,5])
  
  # RANK by row
  qrank <- dcast(qdata, mp ~ name, value.var = "50%")
  cols <- colnames(qrank)[-1]
  qrank[, (cols) := lapply(.SD, frank, ties.method="first"), .SDcols = cols]
  qrank <- qrank[, colnames(tab), with=FALSE]
  
  # COLOUR by rank
  colours <- c(rep("000000", 2), rep("343434", 4), rep("665544", 2))
  
  #for(i in seq(dim(tab)[1])) {
  #  for(j in seq(2, dim(tab)[2])) {
  #    tab[i, j] <- paste0("\\cellcolor[HTML]{",
  #    colours[unlist(qrank[i, j, with=FALSE])], "}{", tab[i, j, with=FALSE], "}")
  #  }
  #}
  
  # OUTPUT table
  return(xtable(tab, ...))

} # }}}

# resTable {{{
# data <- perft[year == 2023]
# desc <- Reduce(rbind,lapply(indicators, function(x) as.data.frame(x[2:3])))

resTable <- function(data, desc=desc, ...) {

  # COMPUTE median by indicator & mp
  mdat <- data[, .(data=median(data)), by=.(indicator, mp, name)]

  # MERGE indicators decsription
  tdat <- merge(mdat, desc, by.x='name', by.y='name')
  #tdat[indicator == "F1", name] <- "P(SB>0.20SB0)"
  tab <- dcast(tdat, desc + name ~ mp, value.var = "data")
  # FORMAT zeroes
  tab[tab == 0] <- NA
  # BUG
  tab[15,2] <- "P(SB>0.20SB0)"

  xtable(tab, ...)
} # }}}
