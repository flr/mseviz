# plots.R - DESC
# /plots.R

# Copyright European Union, 2017
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

library(mseviz)

# plotTOs

data(perf)

# 1. Boxplots

plotBPs(perf)
plotBPs(perf, target=c(S3=1))

# 2. Trade-offs

plotTOs(perf, "Y1", c("S3", "S5", "T1", "S6"))


