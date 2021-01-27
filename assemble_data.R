# River Long-Term Trends Analysis
# Assemble Data script

# Tasks in this script:
# 1. Set up model info 
# 2. Assemble sample dataset from user-defined dataset and/or NWIS
# 3. Download flow data
# 4. Prune sparse periods from datasets

# Load packages
library(RODBC)
library(EGRET)
library(dataRetrieval)

# Define file paths
dpath = "C:/DNR/WQ/LTT/EGRET/LTT/data/"
ampath = "C:/DNR/WQ/LTT/EGRET/LTT/app/models/"
adpath = "C:/DNR/WQ/LTT/EGRET/LTT/app/data/"

# Load files
load(paste0(dpath,"stations.RData"))
load(paste0(dpath,"parameters.RData"))
load(paste0(dpath,"data.RData"))
n = nrow(stations)
np = nrow(parameters)

# Loop through stations and parameters
for (s in 1:n) {
	for (p in 1:np) {
	  
	  #Remove objects from previous model
	  rm(list=setdiff(ls(), c("dpath","ampath","stations","parameters","data","n","np","s","p")))
	  
		# Print station and parameter
		print(paste(stations$station_name[s], parameters$abbrev[p]))
		flush.console()
		
		# Set up model info
		if(is.na(stations$usgs_station_id[s])) {ID = stations$flow_station_id[s]} else {ID = stations$usgs_station_id[s]}
		INFO = readNWISInfo(ID, parameters$USGS_code[p], interactive=FALSE)
		INFO$shortName = stations$station_name[s]
		INFO$staAbbrev = stations$abbrev[s]
		INFO$paramShortName = parameters$name[p]
		INFO$constitAbbrev = parameters$abbrev[p]
		INFO$concLabel = paste(INFO$constitAbbrev, "(mg/L)")
		INFO$loadLabel = paste(INFO$constitAbbrev, "(tons/yr)")
		
		# Assemble sample dataset
		  export = data.frame()
  		
		# SWIMS database
  		SWIMS = data[data$STATION_ID==stations$swims_id[s] & data$DNR_PARAMETER_CODE==parameters$SWIMS_code[p],]
  		if(nrow(SWIMS)>0) {
    		SWIMS$REMARK = NA
    		SWIMS = SWIMS[,c("DATE","REMARK","RESULT_AMT","LOD_AMT")]
    		SWIMS$REMARK[is.na(SWIMS$RESULT_AMT)] = "<"
    		SWIMS$RESULT_AMT[is.na(SWIMS$RESULT_AMT)] = SWIMS$LOD_AMT[is.na(SWIMS$RESULT_AMT)]
    		SWIMS$REMARK[SWIMS$RESULT_AMT==0] = "<"
    		SWIMS$RESULT_AMT[SWIMS$RESULT_AMT==0] = min(SWIMS$RESULT_AMT[SWIMS$RESULT_AMT!=0], na.rm=TRUE)
    		SWIMS = data.frame(date=SWIMS$DATE, remark=SWIMS$REMARK, result=SWIMS$RESULT_AMT)
    		SWIMS = unique(SWIMS)
    		export = SWIMS
  		}

    		# NWIS database	
  		NWIS = try(readNWISSample(stations$usgs_station_id[s], parameters$USGS_code[p]), silent=TRUE)
  		if (is.data.frame(NWIS)) {
  		  if (nrow(NWIS)>0) {
    		  NWIS_low = NWIS[,c("Date","ConcLow","Uncen")]
    		  NWIS_high = NWIS[,c("Date","ConcHigh","Uncen")]
    		  colnames(NWIS_low)[2] = "Conc"
    		  colnames(NWIS_high)[2] = "Conc"
    		  NWIS = rbind(NWIS_low,NWIS_high)
    		  NWIS = unique(NWIS)
    		  NWIS$Conc[NWIS$Uncen==0] = min(NWIS$Conc, na.rm=TRUE)
    		  NWIS$Uncen[NWIS$Uncen==1] = NA
    		  NWIS$Uncen[NWIS$Uncen==0] = "<"
    		  NWIS = data.frame(date=NWIS$Date, remark=NWIS$Uncen, result=NWIS$Conc)
    		  export = rbind(SWIMS,NWIS)
  		  }
  		}
  		
  		# Export and read in sample
  		if(nrow(export)==0) {next}
  		export = export[order(export$date),]
  		file = paste(parameters$abbrev[p], "_", stations$swims_id[s],".csv", sep="")
  		write.csv(export, paste(dpath, file, sep=""), row.names=FALSE)
  		Sample = readUserSample(dpath, file, separator=",")
  		Sample = removeDuplicates(Sample)
  		
  		# Convert multiple samples per day to interval-censored format
  		dates = aggregate(Sample$Date, by=list(Sample$Date), FUN=length)
  		colnames(dates) = c("Date","Samples")
  		dates = dates$Date[dates$Samples>1]
  		if(length(dates)>0) {
    		Sample1 = Sample[!(Sample$Date %in% dates),]
    		Sample2 = Sample[Sample$Date %in% dates,]
    		Sample2a = data.frame()
    		for (d in dates) {
    		  Sample2d = Sample2[Sample2$Date==d,]
    		  if(anyNA(Sample2d$ConcLow)) {Sample2d$ConcLow = NA} else {Sample2d$ConcLow = min(Sample2d$ConcLow)}
    		  Sample2d$ConcHigh = max(Sample2d$ConcHigh)
    		  Sample2a = rbind(Sample2a, Sample2d[1,])
    		}
    		Sample = rbind(Sample1, Sample2a)
    		Sample = Sample[order(Sample$Date),]
  		}
  		SampleAll = Sample
		
		# Save eList
		eList = list(INFO=INFO, Sample=Sample)
		eList = fixSampleFrame(eList)
		eList$SampleAll = eList$Sample
		Sample = SampleAll = eList$Sample
		save(eList, file=paste0(ampath, parameters$abbrev[p], "_", stations$swims_id[s], ".RData"))
		
		# Prune Sample
		WY = aggregate(Date~waterYear, Sample, length)
		WY = WY[WY$Date>=4,]
		if(nrow(WY)==0) {next}
		Sample = Sample[Sample$waterYear>=WY$waterYear[1],]
		
		# Download flow data
		QstartDate = as.Date(paste0(WY$waterYear[1]-1,"-10-01"))
		QendDate = as.Date(paste0(WY$waterYear[nrow(WY)],"-09-30"))
		Daily = try(readNWISDaily(stations$flow_station_id[s], "00060", QstartDate, QendDate), silent=TRUE)
		  # Go back 1 year if Q data not available for last year
  		if ('try-error' %in% class(Daily)) {
  		  QendDate = QendDate - 365
  		  Daily = try(readNWISDaily(stations$flow_station_id[s], "00060", QstartDate, QendDate), silent=TRUE)
  		}
		  # Go back 1 year if Q data not available for last year	
		  if ('try-error' %in% class(Daily)) {
  		  QendDate = QendDate - 365
  		  Daily = try(readNWISDaily(stations$flow_station_id[s], "00060", QstartDate, QendDate), silent=TRUE)
  		}
		
		# Adjust flow with drainage area ratio
		Daily$Q = Daily$Q/stations$flow_wq_ratio[s]
		Daily$LogQ = log(Daily$Q)
		
		# Save eList
		eList = mergeReport(INFO, Daily, Sample)
		eList$SampleAll = SampleAll
		save(eList, file=paste0(ampath, parameters$abbrev[p], "_", stations$swims_id[s], ".RData"))

		# Prune Sample and Daily
    		# Prune WYs with incomplete flow records
    		WY = aggregate(Date~waterYear, Daily[!(is.na(Daily$Q)),], length)
    		WY = WY[WY$Date>=365,]
    		WY = WY[!(WY$Date<366 & WY$waterYear %in% c(475:525*4)),]
    		Sample = Sample[Sample$waterYear %in% WY$waterYear,]
    		Daily = Daily[Daily$waterYear %in% WY$waterYear,]
    		
    		# Create WY table
    		WY = data.frame(waterYear=c(min(Sample$waterYear):max(Sample$waterYear)), Samples=0, blankTime=0, 
    		  period=0, minBlank=0, minBlankEnd=0, blankStart=0, blankEnd=0)
    		count = aggregate(Date~waterYear, Sample, length)
    		ny = nrow(WY)
    		for (y in 1:ny) {
    			if (WY$waterYear[y] %in% count$waterYear) {WY$Samples[y] = count$Date[count$waterYear==WY$waterYear[y]]}
    		}
    		if (nrow(WY)<5) {next}
    		
    		# Prune sparse WYs at start of record and prune WYs with fewer than 4 samples
    		if (WY$Samples[1]<4) {WY$blankTime[1]=1}
    		for (y in 2:(ny-1)) {
    			n4 = max(WY$Samples[c(y+1:4)], na.rm=TRUE)
    			if (WY$Samples[y]<4 & n4<4) {WY$blankTime[y]=1}
    			if (WY$Samples[y]<4 & WY$blankTime[y-1]==1) {WY$blankTime[y]=1}
    		}	
    		if (WY$Samples[ny]<4) {WY$blankTime[ny]=1}
    		
    		if (sum(WY$blankTime[1:3])>0 & WY$blankTime[4]==1) {WY$blankTime[1:3]=1}
    		for (y in 3:(ny-2)) {
    			if (sum(WY$blankTime[(y-2):(y+2)], na.rm=TRUE)>2) {WY$blankTime[y]=1}
    		}
    
    		# Prune strings of blankTime at beginning and end of record
    		for (y in 1:ny) {
    			WY$minBlank[y] = min(WY$blankTime[1:y])
    			WY$minBlankEnd[y] = min(WY$blankTime[y:ny])
    		}			
    		WY = WY[WY$minBlank==0,]
    		WY = WY[WY$minBlankEnd==0,]
    		
    		# Move to next model if not >=50 samples and >=5 years of record
    		if (nrow(WY)<5) {next}
    		Sample = Sample[Sample$waterYear>=min(WY$waterYear) & Sample$waterYear<=max(WY$waterYear),]
    		if (nrow(Sample)<50) {next}
    		Daily = Daily[Daily$waterYear>=min(WY$waterYear) & Daily$waterYear<=max(WY$waterYear),]
    	
		# Save eList
		eList = mergeReport(INFO, Daily, Sample)
		eList$SampleAll = SampleAll
		eList$WY = WY
		save(eList, file=paste0(ampath, parameters$abbrev[p], "_", stations$swims_id[s], ".RData"))
		
	}
}	

