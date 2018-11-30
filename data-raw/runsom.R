# runsom.R - DESC
# /runsom.R

# Copyright European Union, 2018
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

library(FLasher)
library(FLRP)

# DATA
data(ple4)
data(ple4.indices)

# SR and BRP
p4sr <- fmle(as.FLSR(ple4, model='ricker'))
p4rp <- brp(FLBRP(ple4, sr=p4sr))

# OMP
omp <- propagate(fwdWindow(ple4, p4rp, end=2038)[, ac(2008:2038)], 250)
residuals <- rlnorm(250, residuals(p4sr), 0.5)[,sample(1:51, 30)]
dimnames(residuals) <- list(year=2009:2038)

# fwd(omp)
omps <- FLStocks(parallel::mclapply(seq(0.1, 0.4, length=6), function(x) {
  fwd(omp, sr=p4sr, residuals=residuals,
    control=fwdControl(year=2009:2038, quant='f', value=runif(250*30, x, x*1.5)))
  }, mc.cores = 6))

# RUNS
names(omps) <- paste0("MP", 1:6)

library(data.table)

# METRICS
runs <- lapply(omps, metrics, metrics=list(F=fbar, SB=ssb, C=catch))
runs <- rbindlist(lapply(runs, as.data.frame, drop=TRUE), idcol="mp")

# OM
om <- data.table(as.data.frame(metrics(ple4,
  metrics=list(F=fbar, SB=ssb, C=catch)), drop=TRUE))
# om[, iter:=1]
#om <- om[,c(1,4,2,3)]

save(om, runs, file="../data/omruns.RData")
