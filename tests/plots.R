# plots.R - DESC
# /plots.R

# Copyright European Union, 2017
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# plotTOs

data(perf)

# 1. Boxplots

plotBPs(perf) + coord_fixed(ratio=4/4)
plotBPs(perf, target=c(S3=1)) + coord_fixed(ratio=4/4)

# 2. 
