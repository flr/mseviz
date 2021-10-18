# plots.R - DESC
# ioalbmse/R/plots.R

# Copyright European Union, 2015-2016
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# plotBPs {{{

#' Boxplot by MP for a range of statistics
#' Figure 3
#' @examples
#' data(perf)
#' # A data.table of performance statistics per run,
#' head(perf)
#' # plot selected statistics
#' plotBPs(perf, statistics=c("SB0", "FMSY", "green"))
#' # Add targets and limits by statistics, as named vectors
#' plotBPs(perf, statistics=c("SB0", "FMSY", "green"),
#'   target=c(SB0=0.40, FMSY=1, green=0.5), limit=c(SB0=0.10))

plotBPs <- function(data, statistics=unique(data$statistic),
  target=missing, limit=missing, yminmax=c(0.10, 0.90), lowupp=c(0.25, 0.75)) {

  # CHECK quantiles
  if(any(c(length(yminmax), length(lowupp)) != 2))
    stop("'yminmax' and 'lowupp' must be both of length 2")

  # SUBSET statistics
  data <- data[statistic %in% statistics,]
  
  # ORDER name as of statistics
  cols <- unique(data[,c('statistic','name')])
  data[, name:=factor(name, levels=cols$name[match(cols$statistic, statistics)],
    ordered=TRUE)]
  
  # ORDER mp as in input
  dat[, mp:=factor(mp, levels=unique(mp))]

  dat <- data[, .(ymin=quantile(data, yminmax[1], na.rm=TRUE),
    lower=quantile(data, lowupp[1], na.rm=TRUE),
    middle=median(data, na.rm=TRUE), upper=quantile(data, lowupp[2], na.rm=TRUE),
    ymax=quantile(data, yminmax[2], na.rm=TRUE)), by=.(mp, statistic, name)]


  # PLOT
  p <- ggplot(dat,
    aes(x=mp, ymin=ymin, lower=lower, middle=middle,
      upper=upper, ymax=ymax, fill=mp)) +
    # data ~ mp, colour by mp
    # PLOT boxplot by mp
    geom_boxplot(stat="identity") +
    # PANELS per statistics
    facet_wrap(~name, scales='free_y') +
    xlab("") + ylab("") +
    # DELETE x-axis labels, LEGEND in 6th panel
    # TODO legend pos by no. of panels
    theme(axis.text.x=element_blank(), legend.position=c("right"),
    # DELETE legend title
    legend.title=element_blank())

  # TARGET
  if(!missing(target)) {
    dat <- data[statistic %in% names(target),]
    dat[, target:=unlist(target)[match(statistic, names(target))]]
    p <- p + geom_hline(data=dat, aes(yintercept=target), colour="green",
      linetype="longdash", size=1)
  }
  
  # LIMIT
  if(!missing(limit)) {
    dat <- data[statistic %in% names(limit),]
    dat[, limit:=unlist(limit)[match(statistic, names(limit))]]
    p <- p + geom_hline(data=dat, aes(yintercept=limit), colour="red",
      linetype="longdash", size=1)
  }

  return(p)
} # }}}

# plotTOs {{{

#' Trade-offs plot by MP for a range of statistics
#' Figure 4
#' @examples
#' data(perf)
#' plotTOs(perf, x="C", y=c("SBMSY", "FMSY", "green", "SB0"))

plotTOs <- function(data, x=unique(data$statistic)[1],
  y=setdiff(unique(data$statistic), x), probs=c(0.10, 0.50, 0.90),
   size=0.50, alpha=0.75) {
  
  # ORDER mp as in input
  data[, mp:=factor(mp, levels=unique(mp))]

  # CALCULATE quantiles
  data <- data[, as.list(quantile(data, probs=probs, na.rm=TRUE)),
    keyby=list(statistic, name, year, mp)]

  # LABELS probs
  xsyms <- syms(paste0("x", probs))
  ysyms <- syms(paste0("y", probs))

  # SUBSET statistics
  daty <- data[statistic %in% y,]
  setnames(daty, seq(5, 4 + length(probs)), paste0("y", probs))
  datx <- data[statistic %in% x,]
  setnames(datx, seq(5, 4 + length(probs)), paste0("x", probs))
  
  # MERGE x into y
  dat <- cbind(daty, datx[,-(1:4)])
 
  p <- ggplot(dat, aes(x=!!xsyms[[2]], y=!!ysyms[[2]])) +
    xlab(unique(datx$name)) + ylab("") +
  # PLOT lines
  geom_linerange(aes(ymin=!!ysyms[[1]], ymax=!!ysyms[[3]]), size=size, alpha=alpha) +
  geom_linerange(aes(xmin=!!xsyms[[1]], xmax=!!xsyms[[3]]), size=size, alpha=alpha) +
  # PLOT median dots
  geom_point(aes(fill=mp), shape=21, size=4) +
  facet_wrap(~name, scales="free_y") +
  scale_shape(solid=FALSE) + theme(legend.title=element_blank())

  return(p)
}
# }}}

# kobeMPs {{{

#' @examples
#' kobeMPs(perf)

kobeMPs <- function(data, x="S3", y="S5", xlim=0.40, ylim=1.4,
  probs=c(0.10, 0.50, 0.90), size=0.75, alpha=1) {
  
  # CALCULATE quantiles
  data <- data[, as.list(quantile(data, probs=probs, na.rm=TRUE)),
    keyby=list(statistic, name, year, mp)]

  # SUBSET statistics
  daty <- data[statistic %in% y,]
  setnames(daty, seq(5, 4 + length(probs)), paste0("y", paste0(probs*100, "%")))
  datx <- data[statistic %in% x,]
  setnames(datx, seq(5, 4 + length(probs)), paste0("x", paste0(probs*100, "%")))
  
  # MERGE x into y
  data <- cbind(daty, datx[,-(1:4)])
  lim <- pmax(round(apply(data[,c('y90%','x90%')] / 0.5, 2, max)) * 0.5, 1.5)

  # PLOT
  p <- ggplot(data, aes(x=`x50%`, y=`y50%`)) +
    # Kobe background
    geom_rect(aes(xmin=1, xmax=Inf, ymin=0, ymax=1), colour='green',
      fill='green') +
    geom_rect(aes(xmin=0, xmax=1, ymin=0, ymax=1), colour='yellow',
      fill='yellow') +
    geom_rect(aes(xmin=1, xmax=Inf, ymin=1, ymax=Inf), colour='orange', 
      fill='orange') +
    geom_rect(aes(xmin=0, xmax=1, ymin=1, ymax=Inf), colour='red',
      fill='red') +
    # Central GRID
    geom_hline(aes(yintercept=1)) + geom_vline(aes(xintercept=1)) +
    # lims
    scale_x_continuous(expand = c(0, 0), limits = c(0, lim['x90%'])) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, lim['y90%'])) +
    # DROP background grid
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      panel.background = element_blank(), axis.line = element_blank()) +
    # PLOT LRPs
    geom_segment(aes(x=xlim, xend=Inf, y=ylim, yend=ylim), colour='gray') +
    geom_segment(aes(x=xlim, xend=xlim, y=0, yend=ylim), colour='gray') +
    # PLOT lines
    geom_linerange(aes(ymin=`y10%`, ymax=`y90%`), size=size, alpha=alpha) +
    geom_linerange(aes(xmin=`x10%`, xmax=`x90%`), size=size, alpha=alpha) +
    # PLOT median dots
    geom_point(aes(fill=mp), shape=21, size=4) +
    scale_shape(solid=FALSE) + theme(legend.title=element_blank()) +
    # LABELS
    labs(x=expression(SB/SB[MSY]), y=expression(F/F[MSY])) +
    annotate("text", x = xlim - xlim * 0.35, y = 0.1,
      label = "SB[lim]", parse=TRUE) +
    annotate("text", x = 0.90, y = 0.1, label = "SB[targ]", parse=TRUE) +
    annotate("text", x = lim["x90%"] - lim["x90%"] * 0.10,
      y = ylim + ylim * 0.10,
      label = "F[lim]", parse=TRUE) +
    annotate("text", x =  lim["x90%"] - lim["x90%"] * 0.10, y = 1.10,
      label = "F[targ]", parse=TRUE)

  return(p)
} # }}}

# kobeTS {{{

kobeTS <- function(perfts) {

  ggplot(perfts, aes(x=ISOdate(year, 1, 1), y=data, fill=statistic)) + 
    geom_col(colour="black", size=0.5) +
    scale_discrete_manual(name = "Kobe Quadrant", aesthetics=c("fill"),
      values=c(green="darkgreen", red="red", yellow="yellow2", orange="orange")) +
    facet_wrap(~mp) +
    xlab("") + ylab("") +
    theme(legend.position=c(.85,.15))

} # }}}

# plotOMruns {{{

plotOMruns <- function(om, runs, limit=missing, target=missing, iter=NULL,
  probs=c(0.10, 0.25, 0.50, 0.75, 0.90), iyear=dims(om)$maxyear, ylab="", ylim=missing) {

  # CHECK classses
  if(!is(om, "FLQuant") | !is(runs, "FLQuants"))
    stop("om and runs must be of class FLQuant and FLQuants respectively.")
 
  # PLOT om
  p1 <- ggplotFL::plot(om, probs=probs) + xlim(NA, iyear + 1) +
    geom_vline(xintercept=iyear)

  # RPs
  if(!missing(limit))
    p1 <- p1 + geom_hline(aes(yintercept=limit), colour="red", linetype=2)
  if(!missing(target))
    p1 <- p1 + geom_hline(aes(yintercept=target), colour="green", linetype=2)

  p2 <- ggplotFL::plot(runs, probs=probs, iter=iter) + facet_wrap(~qname,
    ncol=2) + ylab(ylab) + geom_vline(xintercept=iyear)

  # RPs
  if(!missing(limit))
    p2 <- p2 + geom_hline(aes(yintercept=limit), colour="red", linetype=2)
  if(!missing(target))
    p2 <- p2 + geom_hline(aes(yintercept=target), colour="green", linetype=2)

  if(!missing(ylim))
    p2 <- p2 + coord_cartesian(ylim=ylim)
  
  p <- p1 + p2 + plot_layout(ncol=1, heights=c(1, length(runs) / 2))

  return(p)

} # }}}
