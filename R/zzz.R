# zzz.R - DESC
# mseviz/R/zzz.R

# Copyright (c) WMR, 2026.
# Author: Iago MOSQUEIRA <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

.onLoad <- function(libname, pkgname) {
  registerS3method("ggplot_add", "facet_limits", ggplot_add.facet_limits, 
    envir = asNamespace("ggplot2"))
}
