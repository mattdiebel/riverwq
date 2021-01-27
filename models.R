# River Long-Term Trends Analysis
# Models script

# Run assemble_data.r script before running this script

# Tasks in this script:
# 1. Load dataset from assemble_data script
# 2. Fit WRTDS model
# 3. Iteratively remove outliers based on Grubb's test on model residuals
# 4. Remove data gaps from Daily predictions
# 5. Build water year table
# 6. Define periods for plotting
# 7. Seasonal trends stratified by discharge

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
for (s in 1:n) {
	for (p in 1:np) {
	  
	  # Remove objects from previous model
	  rm(list=setdiff(ls(), c("ampath","adpath","stations","parameters","n","np","s","p")))
	  
	  # Print station and parameter
	  cat(stations$station_name[s], "-", parameters$abbrev[p], "\n\n")
	  flush.console()
	  
	  # Load model if it exists
	  temp = try(load(paste0(ampath, parameters$abbrev[p], "_", stations$swims_id[s], ".RData")), silent=TRUE)
	  if ('try-error' %in% class(temp)) {next}
	  
	  if (!("WY" %in% names(eList))) {next}
	  WY = eList$WY
	  if (nrow(WY)<5) {next}
	  if (nrow(eList$Sample)<50) {next}
	  
	  # Fit model
	  cat("Estimating model...", "\n\n")
	  flush.console()
	  nCores = detectCores() - 1
	  cl = makeCluster(nCores)
	  registerDoParallel(cl)
	  time = system.time({
	    eList = try(
	      modelEstimation(eList, minNumObs=50, minNumUncen=0, windowY=7, windowQ=2, windowS=0.5, 
	                      edgeAdjust=TRUE, verbose=FALSE, run.parallel=TRUE), silent=TRUE)
	  })
	  cat("Model estimation run time:", time[3], "seconds", "\n\n\n")
	  flush.console()
	  stopCluster(cl)
	  if ('try-error' %in% class(eList)) {next}
	  
	  # Make augmented Sample for plotting
	  eList_temp = eList
	  eList_temp = makeAugmentedSample(eList_temp)
	  eList$Sample = eList_temp$Sample
	  Sample = eList$Sample
	  
	  hist(Sample$rResid, breaks=30,
	       xlab="Residual",
	       main=paste0(eList$INFO$station_nm, " - ", eList$INFO$constitAbbrev))
	  flush.console()
		
	  # Outlier loop
	  diff = 1
	  ni = nrow(eList$Sample)
	  
	  while (diff>0 & ni>=60) {
	    ni = nrow(eList$Sample)
	    t = qt(0.01/(2*ni),ni-2)
	    Gcrit = (ni-1)/sqrt(ni)*sqrt(t^2/(ni-2+t^2))
	    
	    # Fit model
  		cat("Estimating model...", "\n\n")
  		flush.console()
  		nCores = detectCores() - 1
  		cl = makeCluster(nCores)
  		registerDoParallel(cl)
  		time = system.time({
    		eList = try(
    				modelEstimation(eList, minNumObs=50, minNumUncen=0, windowY=7, windowQ=2, windowS=0.5, 
    				    edgeAdjust=TRUE, verbose=FALSE, run.parallel=TRUE), silent=TRUE)
  		})
  		cat("Model estimation run time:", time[3], "seconds", "\n\n\n")
  		flush.console()
  		stopCluster(cl)
  		if ('try-error' %in% class(eList)) {next}
  		
      # Make augmented Sample for plotting
  		eList_temp = eList
  		eList_temp = makeAugmentedSample(eList_temp)
  		eList$Sample = eList_temp$Sample
  		Sample = eList$Sample
  		
  		for (i in 1:ni) {
  		  mu = mean(Sample$rResid[-i], na.rm=TRUE)
  		  sd = sd(Sample$rResid[-i], na.rm=TRUE)
  		  Sample$G[i] = abs((Sample$rResid[i]-mu)/sd)
  		}
  		Gmax = max(Sample$G, na.rm=TRUE)
  		Sample = Sample[!(Sample$G>Gcrit & Sample$G==Gmax),]
  		Sample = Sample[!is.na(Sample$G),]
  		diff = ni-nrow(Sample)
  		hist(Sample$rResid, breaks=30,
  		     xlab="Residual",
  		     main=paste0(eList$INFO$station_nm, " - ", eList$INFO$constitAbbrev))
  		flush.console()
  		eList$Sample = Sample
	  }
		
		# Remove data gaps from Daily predictions
		eList$Daily[!(eList$Daily$waterYear %in% WY$waterYear[WY$blankTime==0]),c("yHat","SE","ConcDay","FluxDay","FNConc","FNFlux")] = NA
		
		# Build water year table
		WYFN = tableResults(eList, qUnit=3, fluxUnit=5)
		colnames(WYFN) = c("waterYear","Discharge","Conc","FNConc","Flux","FNFlux")
		WY = merge(WY[,1:4], WYFN, all.x=TRUE)
		WY$blankTime[WY$Discharge=="NaN"] = 1
		WY$blankTime[is.na(WY$Discharge)] = 1
		
		# Define periods for plotting
		WY$period[1] = period = 1
		for (y in 2:nrow(WY)) {
			if (WY$blankTime[y]==1) {WY$period[y] = 0}
			if (WY$blankTime[y]==0 & WY$blankTime[y-1]==1) {period = period + 1}
			if (WY$blankTime[y]==0) {WY$period[y] = period}	
		}
		periods = data.frame(period = 1:max(WY$period))
		for (i in 1:nrow(periods)) {
		  periods$start[i] = min(which(WY$period==i))
		  periods$end[i] = max(which(WY$period==i))
		}
		
		# Seasonal trends stratified by discharge
		seasons = list()
		names = c("Winter","Spring","Summer","Fall")
		dates = c("01-15","04-15","07-15","10-15")
		months = list(c(12,1,2),c(3,4,5),c(6,7,8),c(9,10,11))
		trends = list()
		legend = list()	
		for (t in 1:4) {
		  seasons[[t]] = list(name=names[t], dates=dates[t], months=months[[t]])
		  Q10 = quantile(eList$Daily$Q[eList$Daily$Month %in% months[[t]]], probs=0.1)*35.3147
		  Qmn = mean(eList$Daily$Q[eList$Daily$Month %in% months[[t]]])*35.3147
		  Q90 = quantile(eList$Daily$Q[eList$Daily$Month %in% months[[t]]], probs=0.9)*35.3147
		  seasons[[t]]$flows = c(Q10,Qmn,Q90)
      seasons[[t]]$legend = c(paste(round(Q10,0),"cfs"),paste(round(Qmn,0),"cfs"),paste(round(Q90,0),"cfs"))
			seasons[[t]]$trends = plotConcTimeSmooth(eList, qUnit=1, q1=Q10, q2=Qmn, q3=Q90, centerDate=seasons[[t]]$dates, 
			  yearStart=eList$INFO$DecLow, yearEnd=eList$INFO$DecHigh, minNumObs=20, minNumUncen=20, logScale=FALSE, 
  			printLegend=FALSE, printValues=TRUE, edgeAdjust=FALSE)
			colnames(seasons[[t]]$trends)[2:4] = c("low","mean","high")
			seasons[[t]]$trends$waterYear = round(seasons[[t]]$trends$year-0.25,0)
			seasons[[t]]$trends[seasons[[t]]$trends$waterYear %in% WY$waterYear[WY$blankTime==1],2:4] = NA
		}
		ymin = min(seasons[[1]]$trends[2:4],seasons[[2]]$trends[2:4],seasons[[3]]$trends[2:4],seasons[[4]]$trends[2:4], na.rm=TRUE)
		ymax = max(seasons[[1]]$trends[2:4],seasons[[2]]$trends[2:4],seasons[[3]]$trends[2:4],seasons[[4]]$trends[2:4], na.rm=TRUE)
		seasons$ylims=c(ymin,ymax)
		
		# Save eList
		eList$periods = periods
		eList$WY = WY
		eList$seasons = seasons
		save(eList, file=paste0(ampath, parameters$abbrev[p], "_", stations$swims_id[s], ".RData"))
		
	}
}	

