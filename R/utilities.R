# utilities.R - DESC
# mseviz/R/utilities.R

# Copyright (c) WMR, 2026.
# Author: Iago MOSQUEIRA <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

# facet_limits {{{

facet_limits <- function(variable_col, prob_pattern = "^P\\(") {
  structure(
    list(variable_col = variable_col, prob_pattern = prob_pattern),
    class = "facet_limits"
  )
}

ggplot_add.facet_limits <- function(object, plot, object_name) {
  data <- plot$data
  variable_col <- object$variable_col
  prob_pattern <- object$prob_pattern
  
  vars <- unique(data[[variable_col]])
  sentinels <- data.table::rbindlist(lapply(vars, \(v) {
    upper <- if (grepl(prob_pattern, v)) 1 else NA_real_
    data.table::data.table(variable = v, value = c(0, upper))
  }))[!is.na(value)]
  data.table::setnames(sentinels, "variable", variable_col)
  
  plot + geom_blank(data = sentinels, aes(x = NA, y = value), inherit.aes = FALSE)
}

# }}}
