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

# kobeMPs {{{

#' @examples
#' kobeMPs(perf)

kobeMPs <- function(data, x="S3", y="S6", xlim=0.40, ylim=1.4,
  probs=c(0.10, 0.50, 0.90), size=0.75, alpha=1) {
  
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

  # PLOT
  p <-ggplot(data, aes(x=`x50%`, y=`y50%`)) +
    # Kobe background
    geom_rect(aes(xmin=1, xmax=Inf, ymin=0, ymax=1), colour='green', fill='green') +
    geom_rect(aes(xmin=0, xmax=1, ymin=0, ymax=1), colour='yellow', fill='yellow') +
    geom_rect(aes(xmin=1, xmax=Inf, ymin=1, ymax=Inf), colour='orange', fill='orange') +
    geom_rect(aes(xmin=0, xmax=1, ymin=1, ymax=Inf), colour='red', fill='red') +
    # Central GRID
    geom_hline(aes(yintercept=1)) + geom_vline(aes(xintercept=1)) +
    # lims
    scale_x_continuous(expand = c(0, 0),limits = c(0, 2)) +
    scale_y_continuous(expand = c(0, 0),limits = c(0, 2)) +
    # DROP background grid
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      panel.background = element_blank(), axis.line = element_blank()) +
    # PLOT lines
    geom_linerange(aes(ymin=`y10%`, ymax=`y90%`), size=size, alpha=alpha) +
    geom_linerangeh(aes(xmin=`x10%`, xmax=`x90%`), size=size, alpha=alpha) +
    # PLOT median dots
    geom_point(aes(fill=mp), shape=21, size=4) +
    scale_shape(solid=FALSE) + theme(legend.title=element_blank()) +
    # PLOT LRPs
    geom_segment(aes(x=xlim, xend=2, y=ylim, yend=ylim)) +
    geom_segment(aes(x=xlim, xend=xlim, y=0, yend=ylim)) +
    # LABELS
    labs(x=expression(SB/SB[MSY]), y=expression(F/F[MSY])) +
    annotate("text", x = 0.25, y = 0.1, label = "SB[lim]", parse=TRUE) +
    annotate("text", x = 0.85, y = 0.1, label = "SB[targ]", parse=TRUE) +
    annotate("text", x = 1.85, y = 1.5, label = "F[lim]", parse=TRUE) +
    annotate("text", x = 1.85, y = 1.1, label = "F[targ]", parse=TRUE)

  return(p)
} # }}}

# plotOMruns {{{

#' @examples
#' data(omruns)
#' plotOMruns(om[qname=="F",], runs[qname=="F",], limit=0.8, target=0.4)
#' plotOMruns(om[qname=="C",], runs[qname=="C",])

plotOMruns <- function(om, runs, limit=missing, target=missing,
  probs=c(0.10, 0.25, 0.50, 0.75, 0.90)) {
  
  # COMPUTE om quantiles
  if("iter" %in% colnames(om) && length(unique(om$iter)) > 1) {
    om <- om[, as.list(quantile(data, probs=probs, na.rm=TRUE)),
    keyby=list(year)]
  } else {
    om[, `50%`:=data]
  }

  # PLOT om
  p1 <- ggplot(om, aes(x=year, y=`50%`)) + geom_line() +
    ylab("") + xlab("")
  # RPs
  if(!missing(limit))
    p1 <- p1 + geom_hline(aes(yintercept=limit), colour="red", linetype=2)
  if(!missing(target))
    p1 <- p1 + geom_hline(aes(yintercept=target), colour="green", linetype=2)

  # COMPUTE runs quantiles
  runs <- runs[, as.list(quantile(data, probs=probs, na.rm=TRUE)),
    keyby=list(year, mp)]

  # PLOT runs
  p2 <- ggplot(runs, aes(x=year)) +
    # QUANTILES
    geom_ribbon(aes(ymin=`10%`, ymax=`90%`), fill="red", alpha=0.15) +
    geom_ribbon(aes(ymin=`25%`, ymax=`75%`), fill="red", alpha=0.30) +
    # MEDIAN
    geom_line(aes(y=`50%`)) +
    facet_wrap(~mp, ncol=2)
    # RPs
    if(!missing(limit))
      p2 <- p2 + geom_hline(aes(yintercept=limit), colour="red", linetype=2)
    if(!missing(target))
      p2 <- p2 + geom_hline(aes(yintercept=target), colour="green", linetype=2)

  # TODO DO with grid.arrange
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(4, 2)))
  vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y)
  print(p1, vp = vplayout(1, 1:2))
  print(p2, vp = vplayout(2:4, 1:2))

  invisible()
} # }}}
