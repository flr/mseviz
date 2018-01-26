# plots.R - DESC
# ioalbmse/R/plots.R

# Copyright European Union, 2015-2016
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# plotBPs {{{

#' Boxplot by MP for a range of indicators
#' Figure 3
#' @examples
#'
#' data(perf)
#' 
#' plotBPs(perf, indicators=c("T1", "S6", "F2", "Y1", "S3"))
#' plotBPs(perf, indicators=c("S3", "S6", "F2", "Y1", "T1"))
#' plotBPs(perf, indicators=c("S3", "S6", "F2", "Y1", "T1"), target=list(S3=1))
#' plotBPs(perf, indicators=c("S3", "S6", "F2", "Y1", "T1"),
#'   target=list(S3=1), limit=c(S3=0.2))
#' plotBPs(perf, target=list(S3=1), limit=c(S3=0.2))


plotBPs <- function(data, indicators=c("S3", "S6", "F2", "Y1", "T1"),
  target=missing, limit=missing) {

  # SUBSET indicators
  data <- data[indicator %in% indicators,]
  # ORDER name as of indicators
  cols <- unique(data[,c('indicator','name')])
  data[, name:=factor(name, levels=cols$name[match(cols$indicator, indicators)],
    ordered=TRUE)]
  
  # PLOT
  p <- ggplot(data,
    # data ~ mp, colour by mp
    aes(x=mp, y=data, colour=mp)) +
    # PLOT boxplot by mp
    geom_boxplot(outlier.shape = NA, aes(fill=mp), colour="black") +
    # PANELS per indicators
    facet_wrap(~name, scales='free_y', labeller="label_parsed") +
    xlab("") + ylab("") +
    # DELETE x-axis labels, LEGEND in 6th panel
    # TODO legend pos by no. of panels
    theme(axis.text.x=element_blank(), legend.position=c(.85,.15),
    # DELETE legend title
    legend.title=element_blank())

  # TARGET
  if(!missing(target)) {
    dat <- data[indicator %in% names(target),]
    dat[, target:=unlist(target)[match(indicator, names(target))]]
    p <- p + geom_hline(data=dat, aes(yintercept=target), colour="red",
      linetype="longdash", size=1)
  }
  
  # LIMIT
  if(!missing(limit)) {
    dat <- data[indicator %in% names(limit),]
    dat[, limit:=unlist(limit)[match(indicator, names(limit))]]
    p <- p + geom_hline(data=dat, aes(yintercept=limit), colour="green",
      linetype="longdash", size=1)
  }

  return(p)
} # }}}

# plotTOs {{{

#' Trade-offs plot by MP for a range of indicators
#' Figure 4
#' @examples
#'
#' data(perf)
#' plotTOs(perf)

plotTOs <- function(data, x="Y1", y=c("S3", "S6", "F2", "T1"),
  probs=c(0.10, 0.50, 0.90), size=1, alpha=0.75) {

  # CALCULATE quantiles
  data <- data[, as.list(quantile(data, probs=probs, na.rm=TRUE)),
    keyby=list(indicator, name, year, mp)]

  # SUBSET indicators
  daty <- data[indicator %in% y,]
  setnames(daty, seq(5, 4 + length(probs)), paste0("y", paste0(probs*100, "%")))
  datx <- data[indicator %in% x,]
  setnames(datx, seq(5, 4 + length(probs)), paste0("x", paste0(probs*100, "%")))
  
  # MERGE x into y
  data <- cbind(daty, datx[,-(1:4)])

  p <- ggplot(data, aes(x=`x50%`, y=`y50%`)) + xlab(unique(datx$name)) + ylab("") +
  # PLOT lines
  geom_linerange(aes(ymin=`y10%`, ymax=`y90%`), size=size, alpha=alpha) +
  geom_linerangeh(aes(xmin=`x10%`, xmax=`x90%`), size=size, alpha=alpha) +
  # PLOT median dots
  geom_point(aes(fill=mp), shape=21, size=4) + facet_wrap(~name) +
  scale_shape(solid=FALSE) + theme(legend.title=element_blank())

  return(p)
}
# }}}

# plotKobe {{{

plotKobe <- function(data, x="S3", y="S6", year=max(data$year), alpha=1, size=0.75, colkey="mp") {
  
  # HACK: scoping issue in data.table due to match arg & col names
  ye <- year
  
  dat <- data[indicator %in% c(x, y),][year %in% ye,]
  names <- unique(dat[indicator %in% c(x, y), name])
  
  # TURN run and mp (if it exists) into character
  dat[, 'run' := lapply(.SD, as.character), .SDcols='run']
  dat[, mp := lapply(.SD, as.character), .SDcols='mp']

  dat <- dcast(dat, year + run + mp ~ indicator, sep="",
    value.var=c('10%','25%','50%','75%','90%'))
  
  p <- ggplot(dat, aes_q(x=as.name(paste0("50%", x)),
    y=as.name(paste0("50%", y)), group=as.name(colkey))) +
    # KOBE
    geom_rect(aes(xmin=1, xmax=Inf, ymin=0, ymax=1), colour='green', fill='green') +
    geom_rect(aes(xmin=0, xmax=1, ymin=0, ymax=1), colour='yellow', fill='yellow') +
    geom_rect(aes(xmin=1, xmax=Inf, ymin=1, ymax=Inf), colour='orange', fill='orange') +
    geom_rect(aes(xmin=0, xmax=1, ymin=1, ymax=Inf), colour='red', fill='red') +
    # vertical lines
    geom_linerange(aes_q(ymin=as.name(paste0("10%", y)),
      ymax=as.name(paste0("90%", y))), size=size, alpha=alpha) +
  #  geom_linerange(aes_q(ymin=as.name(paste0("25%", y)),
  #    ymax=as.name(paste0("75%", y))), size=1, alpha=alpha) +
    # horizontal lines
    ggstance::geom_linerangeh(aes_q(xmin=as.name(paste0("10%", x)),
      xmax=as.name(paste0("90%", x))), size=size, alpha=alpha) +
  #  ggstance::geom_linerangeh(aes_q(xmin=as.name(paste0("25%", x)),
  #    xmax=as.name(paste0("75%", x))), size=1, alpha=alpha) +
    # 50% point
    geom_point(aes_q(fill=as.name(colkey)), shape=21, size=4) + scale_shape(solid=FALSE) +
    xlab("SB/SBMSY") +
    ylab("F/FMSY") +
    scale_x_continuous(limits = c(0, max(dat$`90%S3`)), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, max(dat$`90%S5`)), expand = c(0, 0))
  
  return(p + theme(legend.position="none"))
}
# }}}

# plotOMR {{{
plotOMruns <- function(om, runs, refpts, qname="ssb",
  ylab=paste0(toupper(qname), " (", units(qua), ")")) {

  # GET element, slot or method
  foo <- get(qname)
  

  if(!missing(refpts)) {
    qua <- foo(om) %/% refpts
    quas <- lapply(runs, function(x) foo(x) %/% refpts)
  } else {
    qua <- foo(om)
    quas <- lapply(runs, foo)
  }

  if(qname == "fbar") {
    quas <- lapply(quas, function(x) {x[x > 1.5] <- 1.5; return(x)})
  }

  p1 <- plot(qua) + ylab(ylab) +
    geom_vline(xintercept=as.numeric(ISOdate(dims(qua)$maxyear,1,1)), linetype=2, colour='darkgrey')

  p2 <- plot(quas) + facet_wrap(~qname) + ylab(ylab) +
    geom_vline(xintercept=as.numeric(ISOdate(dims(qua)$maxyear,1,1)), linetype=2, colour='darkgrey')

  if(!missing(refpts)) {
    p1 <- p1 + geom_hline(aes(yintercept=1), linetype=2) 
    p2 <- p2 + geom_hline(aes(yintercept=1), linetype=2) 
  }

  # TODO DO with grid.arrange
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(4, 2)))
  vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y)
  print(p1, vp = vplayout(1, 1:2))
  print(p2, vp = vplayout(2:4, 1:2))

  invisible()
} # }}}
