# plots.R - DESC
# ioalbmse/R/plots.R

# Copyright European Union, 2015-2016
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# plotBPs {{{

#' Boxplot by MP for a range of indicators
#' Figure 3

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
    facet_wrap(~name, scales='free_y') +
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
    p <- p + geom_hline(data=dat, aes(yintercept=target), colour="green",
      linetype="longdash", size=1)
  }
  
  # LIMIT
  if(!missing(limit)) {
    dat <- data[indicator %in% names(limit),]
    dat[, limit:=unlist(limit)[match(indicator, names(limit))]]
    p <- p + geom_hline(data=dat, aes(yintercept=limit), colour="red",
      linetype="longdash", size=1)
  }

  return(p)
} # }}}

# plotTOs {{{

#' Trade-offs plot by MP for a range of indicators
#' Figure 4
#' @examples
#' data(perf)
#' plotTOs(perf)

plotTOs <- function(data, x="Y1", y=c("S3", "S6", "F2", "T2"),
  probs=c(0.10, 0.50, 0.90), size=0.50, alpha=0.75) {

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

  p <- ggplot(data, aes(x=`x50%`, y=`y50%`)) +
    xlab(unique(datx$name)) + ylab("") +
  # PLOT lines
  geom_linerange(aes(ymin=`y10%`, ymax=`y90%`), size=size, alpha=alpha) +
  geom_linerangeh(aes(xmin=`x10%`, xmax=`x90%`), size=size, alpha=alpha) +
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
    keyby=list(indicator, name, year, mp)]

  # SUBSET indicators
  daty <- data[indicator %in% y,]
  setnames(daty, seq(5, 4 + length(probs)), paste0("y", paste0(probs*100, "%")))
  datx <- data[indicator %in% x,]
  setnames(datx, seq(5, 4 + length(probs)), paste0("x", paste0(probs*100, "%")))
  
  # MERGE x into y
  data <- cbind(daty, datx[,-(1:4)])
  lim <- pmax(round(apply(data[,c('y90%','x90%')] / 0.5, 2, max)) * 0.5, 1.5)

  # PLOT
  p <- ggplot(data, aes(x=`x50%`, y=`y50%`)) +
    # Kobe background
    geom_rect(aes(xmin=1, xmax=Inf, ymin=0, ymax=1), colour='green', fill='green') +
    geom_rect(aes(xmin=0, xmax=1, ymin=0, ymax=1), colour='yellow', fill='yellow') +
    geom_rect(aes(xmin=1, xmax=Inf, ymin=1, ymax=Inf), colour='orange', fill='orange') +
    geom_rect(aes(xmin=0, xmax=1, ymin=1, ymax=Inf), colour='red', fill='red') +
    # Central GRID
    geom_hline(aes(yintercept=1)) + geom_vline(aes(xintercept=1)) +
    # lims
    scale_x_continuous(expand = c(0, 0), limits = c(0, lim['x90%'])) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, lim['y90%'])) +
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
    geom_segment(aes(x=xlim, xend=Inf, y=ylim, yend=ylim)) +
    geom_segment(aes(x=xlim, xend=xlim, y=0, yend=ylim)) +
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

  ggplot(perfts, aes(x=ISOdate(year, 1, 1), y=data, fill=indicator)) + 
    geom_col(colour="black", size=0.5) +
    scale_discrete_manual(name = "Kobe Quadrant", aesthetics=c("fill"),
      values=c(green="darkgreen", red="red", yellow="yellow2", orange="orange")) +
    facet_wrap(~mp) +
    xlab("") + ylab("") +
    theme(axis.text.x=element_blank(), legend.position=c(.85,.15))

} # }}}

# plotOMruns {{{

plotOMruns <- function(om, runs, limit=missing, target=missing, iter=NULL,
  probs=c(0.10, 0.25, 0.50, 0.75, 0.90), iyear=dims(om)$maxyear, ylab="") {

  # CHECK classses
  if(!is(om, "FLQuant") | !is(runs, "FLQuants"))
    stop("om and runs must be of class FLQuant and FLQuants respectively.")
 
  # PLOT om
  p1 <- plot(om, probs=probs, iter=iter) + xlim(NA, iyear + 1) +
    geom_vline(xintercept=iyear)

  # RPs
  if(!missing(limit))
    p1 <- p1 + geom_hline(aes(yintercept=limit), colour="red", linetype=2)
  if(!missing(target))
    p1 <- p1 + geom_hline(aes(yintercept=target), colour="green", linetype=2)

  p2 <- plot(runs, probs=probs, iter=iter) + facet_wrap(~qname, ncol=2) + ylab(ylab) +
    geom_vline(xintercept=iyear)

  # RPs
  if(!missing(limit))
    p2 <- p2 + geom_hline(aes(yintercept=limit), colour="red", linetype=2)
  if(!missing(target))
    p2 <- p2 + geom_hline(aes(yintercept=target), colour="green", linetype=2)

  # TODO Same limits for p1 and p2
  # TODO MATCH scale and panel size OM vs. runs in plotOMruns
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(4, 2)))

  vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y)
  print(p1, vp = vplayout(1, 1:2))
  print(p2, vp = vplayout(2:4, 1:2))

  invisible()
} # }}}
