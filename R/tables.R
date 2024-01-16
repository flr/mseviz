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

foo <- function(expr) {

 # CHANGE [] to  _{}
 expr <- gsub("[", "_{", expr, fixed=TRUE)
 expr <- gsub("]", "}", expr, fixed=TRUE)
 expr <- gsub("hat\\((.*)\\)", "\\\\hat{\\1}", expr)
 # expr <- gsub("<=", "\\\\leq", expr, fixed=TRUE)
 # expr <- gsub(">=", "\\\\geq", expr, fixed=TRUE)
 # expr <- gsub("%*%", "\\\\dot", expr, fixed=TRUE)
 return(paste0("$", expr, "$"))
}

# summTable {{{

summTable <- function(data,  statistics=unique(data[['statistic']]),
  probs=c(0.10, 0.50, 0.90), ...) {

  # SUBSET statistics
  data <- data[statistic %in% statistics,]

  # CONVERT name to LaTeX
  data[, name:=foo(name)]

  # CALCULATE quantiles
  qdata <- data[,as.list(quantile(data, probs=probs, na.rm=TRUE)),
    keyby=list(statistic, name, mp)]
  
  qdata[, fig:=paste0(
    format(round(`50%`, 2), digits=2, scientific=FALSE, trim=TRUE), " (",
    format(round(`10%`, 2), digits=1, trim=TRUE, scientific=FALSE), "-",
    format(round(`90%`, 2), digits=1, trim=TRUE, scientific=FALSE), ")")]
 
  qtab <- dcast(qdata, mp ~ name, value.var = "fig")
  
  # CALCULATE means
  mdata <- data[, .(fig=format(mean(data, na.rm=TRUE), digits=2,
    scientific=FALSE, trim=TRUE)), keyby=list(statistic, name, mp)]
  
  mtab <- dcast(mdata, mp ~ name, value.var = "fig")
  
  # ASSEMBLE table
  # tab <- cbind(qtab[,1], qtab[,4], mtab[,2], mtab[,3], qtab[,6], mtab[,5])

  tab <- qtab
  
  # RANK by row
  qrank <- dcast(qdata, mp ~ name, value.var = "50%")
  cols <- colnames(qrank)[-1]
  qrank[, (cols) := lapply(.SD, frank, ties.method="first"), .SDcols = cols]
  qrank <- qrank[, colnames(tab), with=FALSE]
  
  # COLOUR by rank
  colours <- c(rep("000000", 2), rep("343434", 4), rep("665544", 2))
  
  # OUTPUT table
  return(xtable(tab, ...))

} # }}}

# resTable {{{
# data <- perft[year == 2023]
# desc <- Reduce(rbind,lapply(indicators, function(x) as.data.frame(x[2:3])))

# TODO: FIX statistics

resTable <- function(data, statistics=unique(data[['statistic']]), ...) {

  desc <- Reduce(rbind, lapply(statistics, function(x) as.data.frame(x[2:3])))

  # COMPUTE mean by statistic & mp
  mdat <- data[, .(data=mean(data, na.rm=TRUE)), by=.(statistic, mp, name)]

  # MERGE statistics description
  tdat <- merge(mdat, desc, by.x='name', by.y='name')
  
  tab <- dcast(tdat, desc + name ~ mp, value.var = "data")

  # CONVERT name to LaTeX
  tab[, name:=foo(name)]
  
  # FORMAT zeroes
  # tab[tab == 0] <- NA
  
  xtable(tab, ...)
} # }}}
