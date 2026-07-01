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

# plot_perfTable {{{

plot_perfTable <- function(dt, row_var = "statistic", col_var = "label",
  value_var = "data", label_var = "name", highest_best = TRUE,
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
    name_vals <- unique(smry$row_label)
    dir_vec   <- setNames(rep(FALSE, length(name_vals)), name_vals)
    dir_vec[names(dir_vec) %in% highest_best] <- TRUE
    dir_key   <- "row_label"
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
  smry[, rank_grp   := fifelse(rank_val <= 3, as.character(rank_val), "none")]
  smry[, rank_grp   := factor(rank_grp, levels = c("1", "2", "3", "none"))]

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

  # COLOUR ramp for ranks 1-3, with luminance-based text colour
  # rank "1" (best) gets the darkest shade; shade_color = NA disables shading
  if (isFALSE(shade_color)) {
    fill_values <- c("1" = fill_na, "2" = fill_na, "3" = fill_na, "none" = fill_na)
  } else {
    ramp        <- grDevices::colorRampPalette(c(shade_color, "white"))(5L)[1:3]
    fill_values <- c("1" = ramp[1], "2" = ramp[2], "3" = ramp[3], "none" = fill_na)
  }

  # relative luminance (WCAG-style) -> decide white vs black text per fill
  luminance <- function(hex) {
    rgb_mat <- grDevices::col2rgb(hex) / 255
    lin     <- apply(rgb_mat, 2, function(ch) {
      ifelse(ch <= 0.03928, ch / 12.92, ((ch + 0.055) / 1.055)^2.4)
    })
    0.2126 * lin[1, ] + 0.7152 * lin[2, ] + 0.0722 * lin[3, ]
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

# perfTable {{{

perfTable <- function(dt, row_var = "statistic", col_var = "label",
  value_var = "data", label_var = "name", highest_best = TRUE,
  shade_color = "#2ca02c", show_mad = TRUE, category_var = NULL) {

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

  if (length(highest_best) == 1L || is.null(names(highest_best))) {
    dir_vec <- setNames(rep(as.logical(highest_best[1L]), length(row_vals)),
                        row_vals)
  } else {
    dir_vec                      <- setNames(rep(TRUE, length(row_vals)), row_vals)
    dir_vec[names(highest_best)] <- as.logical(highest_best)
  }

  dir_dt <- data.table(tmp_key = names(dir_vec), direction = unname(dir_vec))
  setnames(dir_dt, "tmp_key", row_var)
  smry   <- merge(smry, dir_dt, by = row_var, all.x = TRUE)

  # COMPUTE direction-aware rank within each row, 1 = best
  smry[, signed_med := ifelse(direction, -med, med)]
  smry[, rank_val   := frank(signed_med, ties.method = "min"), by = row_var]
  smry[, rank_grp   := fifelse(rank_val <= 3, as.character(rank_val), "none")]
  smry[, rank_grp   := factor(rank_grp, levels = c("1", "2", "3", "none"))]

  # SET format decimals
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
    smry[, cell := paste0(fmt(med), " (", fmt(mad_val), ")")]
  } else {
    smry[, cell := fmt(med)]
  }

  # ORDER rows and columns
  if (has_category) {
    row_meta  <- unique(smry[, .(row_label, category_lb)])
    row_order <- row_meta$row_label
  } else {
    row_order <- unique(smry$row_label)
  }
  col_order <- unique(as.character(smry[[col_var]]))

  # COLOUR ramp for ranks 1-3, with luminance-based text colour
  ramp <- rev(grDevices::colorRampPalette(c(shade_color, "white"))(5L)[1:3])
  fill_values <- c("1" = ramp[1], "2" = ramp[2], "3" = ramp[3])

  # relative luminance (WCAG-style) -> decide white vs black text per fill
  luminance <- function(hex) {
    rgb_mat <- grDevices::col2rgb(hex) / 255
    lin     <- apply(rgb_mat, 2, function(ch) {
      ifelse(ch <= 0.03928, ch / 12.92, ((ch + 0.055) / 1.055)^2.4)
    })
    0.2126 * lin[1, ] + 0.7152 * lin[2, ] + 0.0722 * lin[3, ]
  }
  text_col_lookup <- setNames(
    ifelse(luminance(fill_values) < 0.45, "white", "black"),
    names(fill_values)
  )

  # PIVOT wide: one column per col_var level
  wide_cell <- dcast(
    smry,
    formula   = as.formula(paste0(row_var, " + row_label ~ ", col_var)),
    value.var = "cell"
  )
  wide_rank <- dcast(
    smry,
    formula   = as.formula(paste0(row_var, " ~ ", col_var)),
    value.var = "rank_grp"
  )

  present_cols <- intersect(col_order, names(wide_cell))
  setcolorder(wide_cell, c(row_var, "row_label", present_cols))
  setcolorder(wide_rank, c(row_var, intersect(col_order, names(wide_rank))))

  wide_cell <- wide_cell[match(row_order, row_label)]
  wide_rank <- wide_rank[match(wide_cell[[row_var]], wide_rank[[row_var]])]

  # BUILD display data.frame
  disp <- as.data.frame(wide_cell[, c("row_label", present_cols), with = FALSE])
  colnames(disp)[1L] <- "Statistic"

  # BUILD table
  note_dir <- if (all(dir_vec))        "Higher values are better."
              else if (!any(dir_vec))  "Lower values are better."
              else                     "Direction varies by row."

  mad_note <- if (show_mad) "Cells: median (MAD). " else "Cells: median. "

  tab <- tt(disp,
            notes = paste0(mad_note, note_dir,
                          " Shading highlights the top 3 values per row ",
                          "(darkest = best)."))

  # APPLY per-cell background + text colour (top 3 ranks per row only)
  for (ri in seq_len(nrow(wide_rank))) {
    for (ci in seq_along(present_cols)) {
      rk <- as.character(wide_rank[ri, get(present_cols[ci])])
      if (!is.na(rk) && rk %in% names(fill_values)) {
        tab <- style_tt(tab,
                        i          = ri,
                        j          = ci + 1L,
                        background = fill_values[[rk]],
                        color      = text_col_lookup[[rk]])
      }
    }
  }

  # GROUP rows by category, if present (spanning row-header bands)
  if (has_category) {
    cat_per_row <- row_meta$category_lb[match(row_order, row_meta$row_label)]
    rl          <- rle(as.character(cat_per_row))
    grp_starts  <- cumsum(c(1L, head(rl$lengths, -1L)))
    grp_index   <- setNames(as.list(grp_starts), rl$values)
    tab <- group_tt(tab, i = grp_index)
  }

  tab
}
# }}}
