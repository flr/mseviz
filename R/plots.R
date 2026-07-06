# plots.R - DESC
# mseviz/R/plots.R

# Copyright European Union, 2015-2016
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# - PLOT from performance

# plotBPs {{{

#' Boxplot by MP for a range of statistics
#' Figure 3
#' @examples
#' data(perf)
#' # A data.table of performance statistics per run,
#' head(perf)
#' # plot selected statistics
#' plotBPs(perf, statistics = c("SB0", "FMSY", "green"))
#' # Use FLR's own colourblind-friendly palette
#' plotBPs(perf, statistics = c("SB0", "FMSY", "green")) +
#'   scale_fill_flr()
#' # Add targets and limits by statistics, as named vectors
#' plotBPs(perf,
#'   statistics = c("SB0", "FMSY", "green"),
#'   target = c(SB0 = 0.40, FMSY = 1, green = 0.5), limit = c(SB0 = 0.10)
#' )
#' # Add references inm gray
#' plotBPs(perf,
#'   statistics = c("SB0", "FMSY", "green"),
#'   reference = c(SB0 = 0.50)
#' )
#' # size controls the diameter of the point behind thin boxplots
#' plotBPs(perf, statistics = c("SB0", "FMSY", "green"), size = 3)
#' # Signal MPs by type (color) and target level (hue)
#' plotBPs(perf, statistics = c("SB0", "FMSY", "green")) +
#'   scale_fill_manual(values = c(
#'     "#f70e4a", "#fa537d", "#fc98b1",
#'     "#1189af", "#30beeb", "#83d8f3"
#'   ))
plotBPs <- function(data, statistics = unique(data$statistic), size = 3,
  target = missing, limit = missing, reference = missing,
  yminmax = c(0.10, 0.90), lowupp = c(0.25, 0.75), show.mean = NULL) {

  # check single year 
  if(length(data[, unique(year)]) > 1)
    stop("plotBPs currently only works for a single 'year', please subset or aggregate")
    # data <- data[, .(data=mean(data)), by=.(om, statistic, name, desc, iter,
    #   type, run, mp, label)]

  # CHECK quantiles
  if (any(c(length(yminmax), length(lowupp)) != 2)) {
    stop("'yminmax' and 'lowupp' must be both of length 2")
  }
  
  # SET label if missing
  if (is.null(data[["label"]])) {
    data[, label := mp]
  }

  # ORDER label as in input
  if (!is.factor(data$label))
    data[, label := factor(label, levels = unique(label))]
  
  # SUBSET statistics
  data <- data[statistic %in% statistics, ]

  # ORDER name as of statistics
  data <- data[, name := factor(name, levels = unique(name)[match(statistics,
    unique(statistic))], ordered = TRUE)]

  dat <- data[, .(
    ymin = quantile(data, yminmax[1], na.rm = TRUE),
    lower = quantile(data, lowupp[1], na.rm = TRUE),
    middle = median(data, na.rm = TRUE),
    mean = mean(data, na.rm = TRUE),
    upper = quantile(data, lowupp[2], na.rm = TRUE),
    ymax = quantile(data, yminmax[2], na.rm = TRUE)),
  by = .(label, statistic, name, year, biol)]

  # MERGE name + year if needed
  id <- dat[, length(unique(year)) == 2, by = name]
  dat[name %in% as.character(id[id$V1, name]),
    name := as.character(paste0(name, " (", year, ")"))]

  # MEAN for show.mean statistics
  if (!is.null(show.mean)) {
    dat[statistic %in% show.mean, middle := mean]
  }

  # PLOT
  p <- ggplot(dat, aes(x = label, ymin = ymin, lower = lower, middle = middle,
      upper = upper, ymax = ymax, fill = label, group=label)) +
    # data ~ label, colour by label
    # PLOT boxplot by label
    geom_boxplot(stat = "identity") +
    # PLOT point by label, useful if boxplot is very thin
    geom_point(aes(x = label, y = middle), colour = "black", size = size * 1.20,
      inherit.aes = FALSE) +
    geom_point(aes(x = label, y = middle, fill = label, colour = NULL), shape = 21,
      size = size, inherit.aes = FALSE) +
    # DELETE axis labels, LEGEND in 6th panel
    xlab("") + ylab("") +
    # TODO legend pos by no. of panels
    theme(axis.text.x = element_blank(), legend.position = c("right"),
      # DELETE legend title
      legend.title = element_blank())

    # PANELS per statistics
  if(length(unique(dat$biol)) == 1)
    p <- p + facet_wrap(~name, scales = "free", labeller = "label_parsed")
  else
    p <- p + facet_grid(name~biol, scales = "free", labeller = "label_parsed")

  # TODO: ADD white and dotted line at median (mean) for show.mean facets

  # TARGET
  if (!missing(target)) {
    nms <- names(target)
    dat <- data[statistic %in% nms, ]
    dat[, target := unlist(..target)[match(statistic, nms)]]
    p <- p + geom_hline(
      data = dat, aes(yintercept = target), colour = "green",
      linetype = "longdash", size = 1
    )
  }

  # LIMIT
  if (!missing(limit)) {
    nms <- names(limit)
    dat <- data[statistic %in% nms, ]
    dat[, limit := unlist(..limit)[match(statistic, nms)]]
    p <- p + geom_hline(
      data = dat, aes(yintercept = limit), colour = "red",
      linetype = "longdash", size = 1
    )
  }

  # REFERENCE
  if (!missing(reference)) {
    nms <- names(reference)
    dat <- data[statistic %in% nms, ]
    dat[, reference := unlist(reference)[match(statistic, nms)]]
    p <- p + geom_hline(
      data = dat, aes(yintercept = reference), colour = "gray",
      linetype = "longdash", size = 1
    )
  }

  return(p)
} # }}}

# plotTOs {{{

#' Trade-offs plot by MP for a range of statistics
#' Figure 4
#' @examples
#' data(perf)
#' plotTOs(perf, x = "C", y = c("FMSY", "risk1", "SB0"))

plotTOs <- function(  data, x = unique(data$statistic)[1],
  y = setdiff(unique(data$statistic), x), probs = c(0.10, 0.50, 0.90),
  size = 0.50, linewidth=size, alpha = 0.75) {

  # SET label if missing
  if (is.null(data[["label"]])) {
    data[, label := mp]
  }

  # ORDER label as in input
  if (!is.factor(data$label))
    data[, label := factor(label, levels = unique(label))]
  
  # CALCULATE quantiles
  data <- data[, as.list(quantile(data, probs = probs, na.rm = TRUE)),
    keyby = list(statistic, name, year, label)
  ]

  # LABELS probs
  xsyms <- syms(paste0("x", probs))
  ysyms <- syms(paste0("y", probs))

  # SUBSET statistics
  daty <- data[statistic %in% y, ]
  setnames(daty, seq(5, 4 + length(probs)), paste0("y", probs))
  datx <- data[statistic %in% x, ]
  setnames(datx, seq(5, 4 + length(probs)), paste0("x", probs))

  # MERGE x into y
  dat <- cbind(daty, datx[, -(1:4)])

  p <- ggplot(dat, aes(x = !!xsyms[[2]], y = !!ysyms[[2]])) +
    xlab(parse(text = paste0('"', unique(datx$name), '"'))) +
    ylab("") +
    # PLOT lines
    geom_linerange(aes(ymin = !!ysyms[[1]], ymax = !!ysyms[[3]]),
      linewidth = linewidth,
      alpha = alpha
    ) +
    geom_linerange(aes(xmin = !!xsyms[[1]], xmax = !!xsyms[[3]]),
      linewidth = linewidth,
      alpha = alpha
    ) +
    # PLOT median dots
    geom_point(aes(fill = label), shape = 21, size = 4) +
    facet_wrap(~name, scales = "free_y", labeller = "label_parsed") +
    scale_shape(solid = FALSE) +
    theme(legend.title = element_blank())

  return(p)
}
# }}}

# kobeMPs {{{

#' @examples
#' data(perf)
#' kobeMPs(perf)
#' kobeMPs(perf, Ftarget = 0.80, SBtarget = 0.80)
kobeMPs <- function(
  data, x = "SBMSY", y = "FMSY", SBlim = 0.40, Flim = 1.4, Ftarget = NULL,
  SBtarget = NULL, probs = c(0.10, 0.50, 0.90), size = 0.75, alpha = 1) {

  # SET label if missing
  if (is.null(data[["label"]])) {
    data[, label := mp]
  }

  # CALCULATE quantiles
  data <- data[, as.list(quantile(data, probs = probs, na.rm = TRUE)),
    keyby = list(statistic, name, year, label)]

  # GET probs names
  xnprobs <- paste0("x", paste0(probs * 100, "%"))
  ynprobs <- paste0("y", paste0(probs * 100, "%"))

  # SET as symbools
  xsyms <- syms(xnprobs)
  ysyms <- syms(ynprobs)

  # SUBSET statistics
  daty <- data[statistic %in% y, ]
  setnames(daty, seq(5, 4 + length(probs)), ynprobs)
  datx <- data[statistic %in% x, ]
  setnames(datx, seq(5, 4 + length(probs)), xnprobs)

  # READJUST limits
  xlim <- ceiling(max(datx[[xnprobs[length(probs)]]]) * 2) / 2
  ylim <- ceiling(max(daty[[ynprobs[length(probs)]]]) * 2) / 2

  # MERGE x into y
  data <- cbind(daty, datx[, -(1:4)])

  # PLOT
  p <- ggplot(data, aes(x = `x50%`, y = `y50%`)) +
    # DRAW Kobe background
    geom_rect(aes(xmin = 1, xmax = Inf, ymin = 0, ymax = 1),
      colour = "green",
      fill = "green"
    ) +
    geom_rect(aes(xmin = 0, xmax = 1, ymin = 0, ymax = 1),
      colour = "yellow",
      fill = "yellow"
    ) +
    geom_rect(aes(xmin = 1, xmax = Inf, ymin = 1, ymax = Inf),
      colour = "orange",
      fill = "orange"
    ) +
    geom_rect(aes(xmin = 0, xmax = 1, ymin = 1, ymax = Inf),
      colour = "red",
      fill = "red"
    ) +
    # DRAW central GRID
    geom_hline(aes(yintercept = 1)) +
    geom_vline(aes(xintercept = 1)) +
    # SET lims
    scale_x_continuous(expand = c(0, 0), limits = c(0, xlim)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, ylim)) +
    # DROP background grid
    theme(
      panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      panel.background = element_blank(), axis.line = element_blank()
    ) +
    # PLOT lines
    geom_linerange(aes(ymin = !!ysyms[[1]], ymax = !!ysyms[[3]]),
      size = size, alpha = alpha) +
    geom_linerange(aes(xmin = !!xsyms[[1]], xmax = !!xsyms[[3]]),
      size = size, alpha = alpha) +
    # PLOT median dots
    geom_point(aes(fill = label), shape = 21, size = 4) +
    scale_shape(solid = FALSE) +
    theme(legend.title = element_blank()) +
    # LABELS
    labs(x = expression(SB / SB[MSY]), y = expression(F / F[MSY]))

  # Limit
  if (!is.null(SBlim) & !is.null(Flim)) {
    p <- p +
      annotate(
        geom = "segment", x = SBlim, xend = Inf, y = Flim, yend = Flim,
        colour = "gray"
      ) +
      annotate(
        geom = "segment", x = SBlim, xend = SBlim, y = 0, yend = Flim,
        colour = "gray"
      ) +
      annotate("text",
        x = SBlim, y = 0.10, hjust = -0.10,
        label = "SB[lim]", parse = TRUE
      ) +
      annotate("text",
        x = xlim * 0.90, y = Flim, vjust = -0.10,
        label = "F[lim]", parse = TRUE
      )
  }

  # Target
  if (!is.null(SBtarget) & !is.null(Ftarget)) {
    p <- p +
      annotate(
        geom = "segment", x = SBtarget, xend = Inf, y = Ftarget, yend = Ftarget,
        size = 0.25, linetype = 2
      ) +
      annotate(
        geom = "segment", x = SBtarget, xend = SBtarget, y = 0, yend = Ftarget,
        size = 0.25, linetype = 2
      ) +
      annotate("text",
        x = SBtarget, y = 0.10, hjust = -0.10,
        label = "SB[targ]", parse = TRUE
      ) +
      annotate("text",
        x = xlim * 0.90, y = Ftarget, vjust = -0.10,
        label = "F[targ]", parse = TRUE
      )
  }

  return(p)
} # }}}

# kobeTimeSeries {{{

kobeTimeSeries <- function(perfts) {

  # SUBSET Kobe stats
  perfts <- perfts[statistic %in% c("green", "yellow", "orange", "red")]

  perfts[, statistic := factor(statistic, levels=c("green", "yellow", "orange", "red"))]

  # No. of labels
  nlab <- perfts[, length(unique(label))]

  # PLO
  ggplot(perfts, aes(x = ISOdate(year, 1, 1), y = data, fill = statistic)) +
    geom_col(position="fill") +
    # geom_tile(position="fill") +
    # geom_vline(aes(xintercept=ISOdate(year, 1, 1)), colour="grey", alpha=0.5) +
    scale_discrete_manual(
      name = "Kobe Quadrant", aesthetics = c("fill"),
      values = c(
        green = "darkgreen", red = "red", yellow = "yellow2",
        orange = "orange"),
      # TODO: ADD logical AND symbol \u2227
      labels = c(
        green = expression("P("*B>B[MSY]~"\U2227"~F<F[MSY]*")"),
        yellow = expression("P("*B<B[MSY]~"\U2227"~F<F[MSY]*")"),
        orange = expression("P("*B>B[MSY]~"\U2227"~F>F[MSY]*")"),
        red = expression("P("*B<B[MSY]~"\U2227"~F>F[MSY]*")"))) +
    facet_wrap(~label) +
    xlab("") +
    ylab("") +
    # theme(legend.position = c(.85, .15)) +
    theme(legend.position = "bottom", 
    legend.justification = ifelse(nlab == 1, "left", "center")) +
    scale_y_continuous(labels = scales::percent)

} # }}}

# plotTimeSeries {{{

plotTimeSeries <- function(dat, statistics = c("SB", "R", "C", "F"), 
  worms=NULL) {

  # SET label if missing
  if (!"label" %in% colnames(dat)) {
    dat[, label := mp]
  }

  # GET dims
  yrs <- dat[mp != "", unique(year)]
  yr <- yrs[floor(length(yrs) / 2)]

  # EXTRACT statistics
  dat <- dat[statistic %in% statistics, ]

  # SET name as factior with levels as in 'statistics' argument
  levs <- dat[statistic %in% statistics, .SD[1], by=statistic][, .(statistic, name)]
  dat[, name := factor(name, levels=setNames(levs$name, nm=levs$statistic)[statistics])]

  # CONSTRUCT plot
  p <- ggplot(dat, aes(x = ISOdate(year, 1, 1), y = data, group = label)) +
    geom_flquantiles(aes(fill = label, colour = label),
      probs = c(0.1, 0.50, 0.90),
      alpha = 0.4
    ) +
    geom_flquantiles(aes(fill = label, colour = label),
      probs = c(0.25, 0.50, 0.75),
      alpha = 0.6
    ) +
    facet_grid(name ~ ., scales = "free", labeller = label_parsed) +
    xlab("") +
    ylab("")

  # SAMPLE 5 iters if worms=TRUE
  if(isTRUE(worms)) {
    worms <- sample(dat[, unique(iter)], 5)
  # or SAMPLE worms if single value
  } else if (is.numeric(worms) & length(worms) == 1) {
    worms <- sample(dat[, unique(iter)], worms)
  }

  # ADD worms by om
  if(is.numeric(worms) | is.character(worms)) {
    p <- p + geom_line(data=dat[iter %in% worms],
      aes(group=interaction(label, iter), colour=as.factor(iter)), alpha=0.5,
      show.legend=FALSE) +
      # EXCLUDE iters in worms from legend
      scale_color_discrete(breaks = unique(p$data$label))
  }

  # ADD line at start of MP years
  p <- p + geom_vline(xintercept = ISOdate(yrs[1], 1, 1),
    linetype = 4, alpha = 0.3)

  # SUBSET data for labels
  labdat <- p$data[mp != ""][year == yr, .(
    year = unique(ISOdate(year, 1, 1)),
    data = median(data)
  ), by = .(label, statistic)]

  # TEST: ADD labels
  if (length(statistics) == 1) {
    p <- p + geom_label_repel(data = labdat, aes(
      x = year, y = data,
      label = label, colour = label
    ), alpha = 0.7) +
      theme(legend.position = "none", legend.title = element_blank())
  } else {
    p <- p +
      guides(colour = guide_legend(title="", position="bottom", nrow = 1)) +
      theme(legend.title = element_blank())
  }

  return(p)
}
# }}}

# plotTSPanel {{{

plotTSPanel <- function(dat, metric = c("SB")) {

  # EXTRACT metric
  dat <- dat[statistic == metric, ]

  # SEPARATE periods
  da0 <- dat[mp == "", ]
  da1 <- dat[mp != ""]

  # SORT da1 by type, mp

  p0 <- ggplot(da0, aes(x = ISOdate(year, 1, 1), y = data, group = mp)) +
    geom_flquantiles(fill = "grey", probs = c(0.1, 0.50, 0.90), alpha = 0.2) +
    geom_flquantiles(fill = "grey", probs = c(0.25, 0.50, 0.75), alpha = 0.5) +
    xlab("") +
    ylab(metric)

  p1 <- ggplot(da1, aes(x = ISOdate(year, 1, 1), y = data, group = mp)) +
    geom_flquantiles(aes(fill = type), probs = c(0.1, 0.50, 0.90), alpha = 0.2) +
    geom_flquantiles(aes(fill = type), probs = c(0.25, 0.50, 0.75), alpha = 0.5) +
    facet_wrap(~label, ncol = 2) +
    xlab("") +
    ylab(metric)

  return(p0 / p1)

}

# }}}

# plot_perfTable {{{

plot_perfTable <- function(dt, row_var = "statistic", col_var = "label",
  value_var = "data", label_var = "name", highest_best = TRUE, values_best = 2,
  shade_color = "#2ca02c", fill_na = "grey97", show_mad = TRUE,
  category_var = NULL) {

  dt <- as.data.table(dt)

  has_category <- !is.null(category_var) && category_var %in% names(dt)

  # SUMMARISE mean(mad)
  if (has_category) {
    smry <- dt[,
      .(med         = median(get(value_var), na.rm = TRUE),
        mad_val     = mad(get(value_var),    na.rm = TRUE),
        row_label   = unique(get(label_var))[1L],
        category_lb = unique(get(category_var))[1L]),
      by = c(row_var, col_var)
    ]
  } else {
    smry <- dt[,
      .(med       = median(get(value_var), na.rm = TRUE),
        mad_val   = mad(get(value_var),    na.rm = TRUE),
        row_label = unique(get(label_var))[1L]),
      by = c(row_var, col_var)
    ]
  }

  # SET direction per row

  row_vals <- unique(smry[[row_var]])

  # highest_best can be:
  #  - a single logical: applies to every row_var value
  #  - a named logical vector, named by row_var values: per-row_var direction
  #  - an unnamed character vector of label_var ("name") values: those names
  #    are treated as "highest is best", every other name as "lowest is best"
  if (is.character(highest_best)) {
    dir_vec <- setNames(rep(FALSE, length(row_vals)), row_vals)
    dir_vec[names(dir_vec) %in% highest_best] <- TRUE
    dir_key <- row_var
  } else if (length(highest_best) == 1L || is.null(names(highest_best))) {
    dir_vec <- setNames(rep(as.logical(highest_best[1L]), length(row_vals)),
                        row_vals)
    dir_key <- row_var
  } else {
    dir_vec                      <- setNames(rep(TRUE, length(row_vals)), row_vals)
    dir_vec[names(highest_best)] <- as.logical(highest_best)
    dir_key <- row_var
  }

  dir_dt <- data.table(tmp_key = names(dir_vec), direction = unname(dir_vec))
  setnames(dir_dt, "tmp_key", dir_key)
  smry   <- merge(smry, dir_dt, by = dir_key, all.x = TRUE)

  # COMPUTE direction-aware rank within each row, 1 = best
  smry[, signed_med := ifelse(direction, -med, med)]
  smry[, rank_val   := frank(signed_med, ties.method = "min"), by = row_var]
  smry[, rank_grp := fifelse(rank_val <= values_best, as.character(rank_val), "none")]
  smry[, rank_grp := factor(rank_grp, levels = c(as.character(seq_len(values_best)), 
    "none"))]

    # SET fornmat decimals
  fmt <- function(x) {
    out <- character(length(x))
    a   <- abs(x)
    out[a < 10]              <- sprintf("%.2f", x[a < 10])
    out[a >= 10  & a < 100]  <- sprintf("%.1f", x[a >= 10 & a < 100])
    out[a >= 100]            <- sprintf("%.0f", x[a >= 100])
    out[is.na(x)]            <- NA_character_
    out
  }

  # BUILD cell text, with or without MAD
  if (show_mad) {
    smry[, cell := paste0(fmt(med), "\n(", fmt(mad_val), ")")]
  } else {
    smry[, cell := fmt(med)]
  }

  # ORDER rows and columns
  if (has_category) {
    row_order <- unique(smry[, .(row_label, category_lb)])$row_label
  } else {
    row_order <- unique(smry$row_label)
  }
  col_order <- unique(as.character(smry[[col_var]]))

  smry[, row_label_f := factor(row_label, levels = rev(row_order))]
  smry[, col_f        := factor(get(col_var), levels = col_order)]
  if (has_category) {
    smry[, category_f := factor(category_lb, levels = unique(category_lb))]
  }

  # COLOUR ramp for ranks 1-values_best, with luminance-based text colour
  # rank "1" (best) gets the darkest shade; shade_color = NA disables shading
  if (isFALSE(shade_color)) {
    fill_values <- c(setNames(rep(fill_na, values_best),
      as.character(seq_len(values_best))), "none" = fill_na)
  } else {
    ramp <- grDevices::colorRampPalette(c(shade_color, "white"))(values_best)
    fill_values <- c(setNames(ramp, as.character(seq_len(values_best))),
                   "none" = fill_na)
  }

# relative luminance (WCAG-style)
luminance <- function(hex) {
  rgb_mat <- grDevices::col2rgb(hex) / 255
  lin <- apply(rgb_mat, 2, function(ch) {
    ifelse(ch <= 0.03928, ch / 12.92, ((ch + 0.055) / 1.055)^2.4)
  })
  as.numeric(0.2126 * lin[1, ] + 0.7152 * lin[2, ] + 0.0722 * lin[3, ])
}

# helper: blend two colours in RGB space (t in [0,1])
blend_hex <- function(col1, col2 = "white", t = 0.5) {
  a <- grDevices::col2rgb(col1)
  b <- grDevices::col2rgb(col2)
  m <- round((1 - t) * a + t * b)
  grDevices::rgb(m[1], m[2], m[3], maxColorValue = 255)
}

if (isFALSE(shade_color)) {
  fill_values <- c(
    setNames(rep(fill_na, values_best), as.character(seq_len(values_best))),
    "none" = fill_na
  )
} else {
  # choose luminance spacing by number of ranks:
  # fewer ranks -> larger spacing; more ranks -> finer spacing
  L0 <- luminance(shade_color)
  Lmax <- 0.92
  min_delta <- 0.12
  target_end <- min(Lmax, L0 + max(min_delta, 0.55 / max(1, values_best - 1)))

  target_L <- if (values_best == 1L) L0 else seq(L0, target_end, length.out = values_best)

  # find blend-to-white factor t to hit each target luminance
  get_t_for_L <- function(Lt, base_col) {
    f <- function(t) luminance(blend_hex(base_col, "white", t)) - Lt
    lo <- 0; hi <- 1
    flo <- f(lo); fhi <- f(hi)
    if (flo >= 0) return(0)
    if (fhi <= 0) return(1)
    uniroot(f, c(lo, hi), tol = 1e-4)$root
  }

  ts <- vapply(target_L, get_t_for_L, numeric(1), base_col = shade_color)
  ramp <- vapply(ts, function(t) blend_hex(shade_color, "white", t), character(1))

  fill_values <- c(
    setNames(ramp, as.character(seq_len(values_best))),
    "none" = fill_na
  )
}

text_col_lookup <- setNames(
  ifelse(luminance(fill_values) < 0.45, "white", "black"),
  names(fill_values)
)
smry[, text_col := text_col_lookup[as.character(rank_grp)]]




  # BUILD plot

  p <- ggplot(smry, aes(x = col_f, y = row_label_f, fill = rank_grp)) +
    geom_tile(color = "white", linewidth = 1) +
    geom_text(aes(label = cell, color = text_col), size = 3, lineheight = 0.9) +
    scale_fill_manual(values = fill_values, guide = "none") +
    scale_color_identity() +
    scale_x_discrete(position = "top") +
    scale_y_discrete(labels=function(x)parse(text = as.character(x))) +
    labs(x = NULL, y = NULL) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid      = element_blank(),
      axis.text.x     = element_text(angle = 0, hjust = 0.5),
      axis.text.x.top = element_text(angle = 0, hjust = 0.5),
      plot.caption    = element_text(hjust = 0, size = 8, color = "grey30")
    )

  if (has_category) {
    p <- p + facet_grid(category_f ~ ., scales = "free_y", space = "free_y",
                        switch = "y") +
      theme(
        strip.placement    = "outside",
        strip.text.y.left  = element_text(angle = 0, face = "bold", hjust = 0),
        panel.spacing.y    = unit(0.6, "lines")
      )
  }

  return(p)
}

# }}}

# - PLOT from classes

# plotOMruns {{{

plotOMruns <- function(
  om, runs, limit = missing, target = missing, iter = NULL,
  probs = c(0.10, 0.25, 0.50, 0.75, 0.90), iyear = dims(om)$maxyear,
  ylab = "", ylim = "missing"
) {
  # CHECK classses
  if (!is(om, "FLQuant") | !is(runs, "FLQuants")) {
    stop("om and runs must be of class FLQuant and FLQuants respectively.")
  }

  # PLOT om
  p1 <- ggplotFL::plot(om, probs = probs) + xlim(NA, iyear + 1) +
    geom_vline(xintercept = iyear)

  # RPs
  if (!missing(limit)) {
    p1 <- p1 + geom_hline(aes(yintercept = limit), colour = "red", linetype = 2)
  }
  if (!missing(target)) {
    p1 <- p1 + geom_hline(aes(yintercept = target), colour = "green", linetype = 2)
  }

  # iters
  if (length(iter) == 1) {
    iter <- unname(unlist(lapply(quantile(c(om[, ac(dims(om)$maxyear)]),
      probs = seq(0.05, 0.95, length = iter)
    ), function(i) {
      which.min(abs(c(om[, ac(dims(om)$maxyear)]) - i))
    })))
  }

  # PLOT mps
  p2 <- ggplotFL::plot(runs, probs = probs, iter = iter) +
    facet_wrap(~qname, ncol = 2, dir = "v") +
    ylab(ylab) + geom_vline(xintercept = iyear)

  # RPs
  if (!missing(limit)) {
    p2 <- p2 + geom_hline(aes(yintercept = limit), colour = "red", linetype = 2)
  }
  if (!missing(target)) {
    p2 <- p2 + geom_hline(aes(yintercept = target), colour = "green", linetype = 2)
  }

  if (!missing(ylim)) {
    p2 <- p2 + coord_cartesian(ylim = ylim)
  }

  p <- p1 + p2 + plot_layout(ncol = 1, heights = c(1, length(runs) / 2))

  return(p)
} # }}}
