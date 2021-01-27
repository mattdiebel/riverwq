# River Long-Term Trends Analysis
# Summary script

# Run bootstrap.r script before running this script

# Define file paths
ampath = "C:/DNR/WQ/LTT/EGRET/LTT/app/models/"
adpath = "C:/DNR/WQ/LTT/EGRET/LTT/app/data/"

# Load data
load(paste0(adpath,"stations.RData"))
load(paste0(adpath,"parameters.RData"))
n = nrow(stations)
np = nrow(parameters)

#### Year Pairs ####
year_pairs = list()

for (p in 1:np) {
  
  year_pairs[[p]] = list()
  tras = data.frame()

  for (s in 1:n) {
    
    #Remove objects from previous model
    rm(list=setdiff(ls(), 
      c("ampath","adpath","stations","parameters","year_pairs","tras","WYs","n","np","s","p")))
    
    # Print station and parameter
    print(paste(stations$station_name[s], parameters$abbrev[p]))
    flush.console()
    
    # Load model if it exists
    temp = try(load(paste0(ampath, parameters$abbrev[p], "_", stations$swims_id[s], ".RData")), silent=TRUE)
    if ('try-error' %in% class(temp)) {next}
    if (!("WY" %in% names(eList))) {next}
    if (is.na(eList$surfaces)) {next}
    
    # Make year_pairs
    yl = nrow(eList$WY)
    WY = eList$WY
    tra = data.frame()
    
    for (ys in 1:(yl-1)) {
      if (is.na(WY$FNConc[ys])) {next}
      for (ye in ((ys+1):yl)) {
        if(is.na(WY$FNConc[ye])) {next}
        
        tr = data.frame(
          swims_id = NA,
          station_name = NA,
          first_year = NA,
          last_year = NA,
          years = NA,
          first_conc = NA,
          last_conc = NA,
          first_flux = NA,
          last_flux = NA,
          delta_pct = NA,
          delta_pct_abs = NA,
          delta_pct_year = NA,
          delta_pct_year_abs = NA,
          direction = NA,
          prob = NA)
        
        tr$swims_id[1] = stations$swims_id[s]
        tr$station_name[1] = stations$station_name[s]
        tr$first_year[1] = first_year = WY$waterYear[ys]
        tr$last_year[1] = last_year = WY$waterYear[ye]
        tr$years[1] = nyears = last_year-first_year
        tr$first_conc[1] = first_conc = WY$FNConc[ys]
        tr$last_conc[1] = last_conc = WY$FNConc[ye]
        tr$first_flux[1] = WY$FNFlux[ys]
        tr$last_flux[1] = WY$FNFlux[ye]
        tr$delta_pct[1] = delta_pct = (last_conc-first_conc)/first_conc*100
        tr$delta_pct_abs[1] = abs(delta_pct)
        tr$delta_pct_year[1] = delta_pct_year = delta_pct/nyears
        tr$delta_pct_year_abs[1] = abs(delta_pct_year)
        
          up = 0
          down = 0
          for (r in 1:100) {
            WYreps = eList$WYreps[[r]]
            start = WYreps$FNConc[ys]
            if(is.na(start)) {next}
            end = WYreps$FNConc[ye]
            if(is.na(end)) {next}
            if(end-start>0) {up = up+1}
            if(end-start<0) {down = down+1}
          }
          if(delta_pct>=0) {
            tr$direction[1] = "increase"
            tr$prob[1] = up/(up+down)
          } else {
            tr$direction[1] = "decrease"
            tr$prob[1] = down/(up+down)
          }
          
          tra = rbind(tra, tr)
      }
    }
    tras = rbind(tras, tra)
  }
  year_pairs[[p]] = tras
}

names(year_pairs) = parameters$abbrev
filename = paste0(adpath,"year_pairs.RData")
save(year_pairs, file=filename)



#### Years ####
years = list()
last_year = 2017

for (p in 1:np) {
  
  years[[p]] = list()
  WYs = data.frame()
  
  for (s in 1:n) {
    
    #Remove objects from previous model
    rm(list=setdiff(ls(), c("ampath","adpath","stations","parameters","last_year","years","WYs","n","np","s","p")))
    
    # Print station and parameter
    print(paste(stations$station_name[s], parameters$abbrev[p]))
    flush.console()
    
    # Load model if it exists
    temp = try(load(paste0(ampath, parameters$abbrev[p], "_", stations$swims_id[s], ".RData")), silent=TRUE)
    if ('try-error' %in% class(temp)) {next}
    if (!("WY" %in% names(eList))) {next}
    if (is.na(eList$surfaces)) {next}
    
    # Make years
    WY = data.frame(swims_id=stations$swims_id[s],
                    station_name=stations$station_name[s],
                    eList$WY[,c("waterYear","FNConc","FNFlux")])
    if (max(WY$waterYear)<last_year) {
      WYA = data.frame(swims_id=stations$swims_id[s],
                       station_name=stations$station_name[s],
                       waterYear=c((max(WY$waterYear)+1):last_year),
                       FNFlux=WY$FNFlux[WY$waterYear==max(WY$waterYear)],
                       FNConc=WY$FNConc[WY$waterYear==max(WY$waterYear)])
      WY = rbind(WY, WYA)
    }
    WY$FNConc = approx(WY$waterYear[!is.na(WY$FNConc)], WY$FNConc[!is.na(WY$FNConc)], WY$waterYear)$y
    WY$FNFlux = approx(WY$waterYear[!is.na(WY$FNFlux)], WY$FNFlux[!is.na(WY$FNFlux)], WY$waterYear)$y
    WY$FNFlux = WY$FNFlux*(stations$area_WI[s]/stations$drainage_area_mi2[s])
    WY$WSA = stations$area_WI[s]
    WYs = rbind(WYs, WY)
  }
  years[[p]] = WYs
}

names(years) = parameters$abbrev
filename = paste0(adpath,"years.RData")
save(years, file=filename)


#### Parameter pairs ####
par_pairs = list()

for (p in 1:np) {
  
  par_pairs[[p]] = list()
  
  for (q in 1:np) {
    
    print(paste(parameters$abbrev[p],parameters$abbrev[q]))
    
    par_pairs[[p]][[q]] = list()
    par1 = parameters$abbrev[p]
    lab1 = paste(par1,"(mg/L)")
    p1 = years[[par1]]
    p1 = p1[,c("swims_id","station_name","waterYear","FNConc")]
    par2 = parameters$abbrev[q]
    lab2 = paste(par2,"(mg/L)")
    p2 = years[[par2]]
    p2 = p2[,c("swims_id","station_name","waterYear","FNConc")]
    par_pair = merge(p1, p2, by=c("swims_id","station_name","waterYear"))
    par_pair$swims_id = as.character(par_pair$swims_id)
    par_pair$station_name = as.character(par_pair$station_name)
    par_pair$opacity = 0.7
    par_pair$lwd = 0
    par_pair$size = 20

    xdata = data.frame()
    
    for (s in 1:nrow(stations)) {
      sdata = par_pair[par_pair$swims_id==stations$swims_id[s],]
      if (nrow(sdata)==0) {next}
      sdata$color[sdata$swims_id==stations$swims_id[s]] = stations$color[s]
      sdata$id = sdata$swims_id
      sdata$animYear = sdata$waterYear
      sadata = data.frame()
      
      for (y in 2:nrow(sdata)) {
        adata = sdata[max(1,(y-10)):(y-1),]
        la = nrow(adata)
        
        for (z in 1:la) {
          adata$opacity[z] = adata$opacity[z]*(exp(-0.4*(la-z+1)))
          adata$size[z] = adata$size[z]*(-0.1*(la-z)+1)
        }
        adata$animYear = sdata$waterYear[y]
        sadata = rbind(sadata,adata)  
      }
      xdata = rbind(xdata,sdata,sadata)  
    }
    
    par_pair = xdata
    par_pair$id[par_pair$waterYear!=par_pair$animYear] = paste0(
      par_pair$swims_id[par_pair$waterYear!=par_pair$animYear],
      par_pair$waterYear[par_pair$waterYear!=par_pair$animYear])
    par_pair$lwd[par_pair$waterYear==par_pair$animYear] = 1
    par_pairs[[p]][[q]] = par_pair

  }
  names(par_pairs[[p]]) = parameters$abbrev
}
names(par_pairs) = parameters$abbrev
filename = paste0(adpath,"par_pairs.RData")
save(par_pairs, file=filename)

