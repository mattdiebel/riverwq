# River Long-Term Trends Analysis
# Bootstrap script

# Run models.r script before running this script

# Tasks in this script:
# 1. Load model from models script
# 2. Bootstrap model
# 3. Summarize bootstrap results as confidence intervals
# 4. Flag extremely wide intervals to omit from plots

# Load packages
library(EGRET)
library(EGRETci)
library(foreach)
library(doParallel)
library(parallel)
library(iterators)

# Define file paths
ampath = "C:/DNR/WQ/LTT/EGRET/LTT/app/models/"
adpath = "C:/DNR/WQ/LTT/EGRET/LTT/app/data/"

# Load data
load(paste0(adpath,"stations.RData"))
load(paste0(adpath,"parameters.RData"))
n = nrow(stations)
np = nrow(parameters)

# Loop through stations and parameters
for (s in 38:n) {
  for (p in 1:np) {
    
    #Remove objects from previous model
    rm(list=setdiff(ls(), c("ampath","adpath","stations","parameters","n","np","s","p")))
  
    # Print station and parameter
    cat(stations$station_name[s], "-", parameters$abbrev[p], "\n\n")
    flush.console()
    
    # Load model if it exists
    temp = try(load(paste0(ampath, parameters$abbrev[p], "_", stations$swims_id[s], ".RData")), silent=TRUE)
    if ('try-error' %in% class(temp)) {next}
    if (!("WY" %in% names(eList))) {next}
    if (is.na(eList$surfaces)) {next}
    
    # Bootstrap annual estimates
    cat("Bootstrapping annual estimates...", "\n\n")
    flush.console()
    nBoot = 100
    blockLength = 200
    nCores = detectCores() - 1
    cl = makeCluster(nCores)
    registerDoParallel(cl)
    time = system.time({
    	WYreps = foreach(n = 1:nBoot,.packages=c('EGRETci')) %dopar% {
    		annualResults = bootAnnual(eList,
    		                           blockLength,
    		                           startSeed=n)
    	}
    })
    cat("Bootstrapping run time:", round(time[3]/60,2), "minutes", "\n\n\n")
    flush.console()
    stopCluster(cl)
    
    # Summarize bootstrap results
    WY = eList$WY
    conc = matrix(NA, nrow=nrow(WYreps[[1]]), ncol=nBoot)
    flux = conc
    for (r in 1:nBoot) {
    	conc[,r] = WYreps[[r]][,2]
    }
    for (r in 1:nBoot) {
    	flux[,r] = WYreps[[r]][,3]
    }
    for (y in 1:nrow(WY)) {
    	WY$FNConc05[y] = quantile(conc[y,], probs=0.05, na.rm=TRUE)
    	WY$FNConc25[y] = quantile(conc[y,], probs=0.25, na.rm=TRUE)
    	WY$FNConc50[y] = quantile(conc[y,], probs=0.5, na.rm=TRUE)
    	WY$FNConc75[y] = quantile(conc[y,], probs=0.75, na.rm=TRUE)
    	WY$FNConc95[y] = quantile(conc[y,], probs=0.95, na.rm=TRUE)
    	WY$FNFlux05[y] = quantile(flux[y,], probs=0.05, na.rm=TRUE)*365*2.20462/2000
    	WY$FNFlux25[y] = quantile(flux[y,], probs=0.25, na.rm=TRUE)*365*2.20462/2000
    	WY$FNFlux50[y] = quantile(flux[y,], probs=0.5, na.rm=TRUE)*365*2.20462/2000
    	WY$FNFlux75[y] = quantile(flux[y,], probs=0.75, na.rm=TRUE)*365*2.20462/2000
    	WY$FNFlux95[y] = quantile(flux[y,], probs=0.95, na.rm=TRUE)*365*2.20462/2000
    }
    WY[WY$blankTime==1,10:19] = NA
    
    # Flag extremely wide intervals to omit from plots
    WY$plot_flux = NA
    WY$FCI90 = WY$FNFlux95 - WY$FNFlux05
    WY$plot_flux[WY$FCI90 < 3*median(WY$FCI90, na.rm=TRUE)] = 1
    
    WY$plot_conc = NA
    WY$CCI90 = WY$FNConc95 - WY$FNConc05
    WY$plot_conc[WY$CCI90 < 3*median(WY$CCI90, na.rm=TRUE)] = 1
    
    # Save WRTDS model and bootstrap output
    eList$WY = WY
    eList$WYreps = WYreps
    save(eList, file=paste0(ampath, parameters$abbrev[p], "_", stations$swims_id[s], ".RData"))
    
  }
}	
