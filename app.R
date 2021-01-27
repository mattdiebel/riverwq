# River Long-Term Trends Analysis
# Shiny app

library(shiny)
library(leaflet)
library(plotly)

load("data/stations.RData")
load("data/parameters.RData")
load("data/year_pairs.RData")
load("data/years.RData")
load("data/par_pairs.RData")

last_year = max(years[["TP"]]$waterYear, na.rm=TRUE)

ui = fluidPage(
  tags$head(tags$style(HTML(".shiny-output-error-validation {color: red;}"))),
  title = "WI LTT Rivers",
  h3("Long-Term River Water Quality Trends in Wisconsin"),
  hr(),
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        h4("Select site from map"),
        "Circle size proportional to most recent concentration within selected time period",
        leafletOutput("map", height='620px'),
        br()),
      fluidRow(
        column(5,
          selectInput("parameter", 
             label=h4("Select parameter"), 
             choices=c("Total Phosphorus",
                       "Orthophosphate",
                       "Nitrate",
                       "Kjeldahl Nitrogen",
                       "Ammonia",
                       "Chlorophyll a",
                       "Total Suspended Solids",
                       "Chloride",
                       "Silica"), 
             selected="Total Phosphorus")
        ),
        column(7,
          sliderInput("WYrange",
            label=h4("Select time period"),
            min=1961,
            max=last_year,
            value=c(1961,last_year),
            step=1,
            sep="")
        )
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Methods",
                 br(),
                 "This app summarizes water quality information for Wisconsin's major rivers. All data and analyses are updated annually. This app was developed with ", a(href="https://shiny.rstudio.com/", "R shiny."),
                 br(), br(),
                 tags$b("Monitoring Program:"),
                 br(),
                 "The Long Term Trends (LTT) Rivers monitoring program is a baseline monitoring activity conducted by the Wisconsin DNR Water Quality Bureau. The LTT Rivers program was developed to track and analyze water quality trends over time in Wisconsin's rivers. The current version of the network, initiated in 2001, now consists of 43 sites, with a minimum of one site per major river basin, generally located near the mouth of each river, at or near a USGS stream flow gauge. Routine monitoring at many LTT sites started in the 1970s, which provides a perspective on water quality trends since the passage of the Clean Water Act.", br(), br(),
                 tags$b("River Water Quality Summary:"),
                 br(),
                 "River water quality trends are highly variable among parameters and regions of the state. Concentrations of total phosphorus and total suspended solids have decreased in most rivers over the last several decades. In contrast, concentrations of chloride and nitrate have increased in most rivers over this period. In many rivers, water quality improved dramatically during early part of the record, and has stabilized recently. The largest reductions in total phosphorus have occurred in southern Wisconsin, and many of the rivers with large phosphorus reductions also had large suspended solids reductions. Nitrate concentrations have increased in most rivers in agricultural basins in Wisconsin. Chloride concentrations have increased in nearly all rivers in Wisconsin, even in mostly forested basins. The reasons for these trends are likely a combination of changes in land management practices, including agricultural production systems, erosion control, and nutrient management, improvements in wastewater treatment, and increases in road salt use. This analysis does not definitively identify causes of specific trends; however the plots of seasonal and flow-specific trends may be used to infer likely causes, as described below.",
                 br(), br(),
                 tags$b("Analysis Methods:"),
                 br(),
                 "Water quality trends are analyzed using a statistical model called Weighted Regressions on Time, Discharge, and Season (", a(href="https://pubs.usgs.gov/tm/04/a10/", "WRTDS"), "). This model estimates long-term trends in each water quality parameter while controlling for the influence of river flow (discharge) and season on water quality.",
                 br(), br(),
                 tags$ul(
                   tags$li(tags$b("Flow normalization:"), "Annual and seasonal trends are flow-normalized, which means that the influence of variation in river flow on water quality has been removed. As stated by Hirsch and colleagues in the first", a(href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3307614/pdf/jawr0046-0857.pdf", "paper"), "on the WRTDS model, 'The resulting flow-normalized annual concentration and flux histories are very smooth temporally because they eliminate all the variation that is due to the random variation in streamflow. These results should provide a much clearer indication of true progress (or deterioration) toward (or away from) the achievement of water-quality goals. What is meant by true progress (or deterioration) is change in water-quality drivers such as land use, land-use practices, or point source loading. Because the flow-normalized records are not driven by random variations in streamflow and because they are much more stable than the actual record of water quality, they are appropriate to use when computing changes over time.'"),
                   br(),
                   tags$li(tags$b("Uncertainty in trends"), "is estimated by bootstrapping with the", a(href="https://cran.r-project.org/web/packages/EGRETci/vignettes/EGRETci.html", "EGRETci"), "R package). In this analysis, bootstrapping means taking many random samples of the water quality dataset, and for each random sample, re-estimating the WRTDS model. The distribution of trends from these models is an esimate of the uncertainty in the actual trends. 90% confidence intervals are plotted on the annual concentration and flux plots as the 5th to 95th percentiles of the bootstrap distribution."),
                   br(),
                   tags$li(tags$b("Season/flow-specific trends:"), "Season/flow plots are created by plotting the model-estimated concentration for a specific day of year (the middle of each season) at three different discharges (10th percentile, mean, and 90th percentile). These plots are useful for understanding the influence of season and streamflow on concentration at a point in time. For example, total suspended solids concentrations are typically highest at high flow during the spring and summer. These plots are also useful for determining the conditions under which the greatest changes in water quality over time have occurred. For example, changes in low flow concentrations are usually caused by changes in point source inputs, while changes in high flow concentrations are usually caused by changes in non-point source inputs."),
                   br(),
                   tags$li(tags$b("Data gaps:"), "Several sites have extended gaps in their water quality records due to shifts in monitoring priorities over the years. For each parameter at each site, no annual concentration or flux estimates are provided for years that had fewer than four samples.")
                 ),
                 br(),
                 tags$b("Frequently Asked Questions:"),
                 br(),
                 tags$ul(
                   tags$li(tags$b("What are 'healthy' levels of each water quality parameter, and can I compare the flow-normalized concentrations at a site to these levels?"), "Among the parameters included in this analysis, Wisconsin has surface water quality criteria for total phosphorus and ammonia. Most of the larger rivers included in the LTT program have TP criteria of 0.1 mg/L, and a few of the smaller rivers have TP criteria of 0.075 mg/L (", a(href="https://docs.legis.wisconsin.gov/code/admin_code/nr/100/102", "Wisconsin Administrative Code NR 102"), "). These criteria are expressed as growing season (May-Oct) median concentrations, so the annual mean concentrations estimated by the WRTDS model are not directly comparable to these criteria. Ammonia criteria are more complex because they depend on pH, temperature, exposure time, and the types of aquatic organisms present (", a(href="https://docs.legis.wisconsin.gov/code/admin_code/nr/100/105", "Wisconsin Administrative Code NR 105"), "). The lowest ammonia criterion is 0.55 mg/L, for sport fish for 30 days exposure at pH 8.5 and 25 deg C. All of the LTT river sites are ", a(href="http://dnr.wi.gov/topic/surfacewater/assessments.html", "assessed every two years"), " for attainment of all applicable water quality criteria."),
                   br(),
                   tags$li(tags$b("Can a trend analysis result be used to evaluate the effect of a particular action, such as a watershed project or wastewater treatment upgrade?"), "In some cases, the effect of a single major change is evident in the water quality record. For example, in 1978, a large discharge of ammonia to the Oconto River ceased (", a(href="https://dnr.wi.gov/files/PDF/pubs/ss/SS0164.pdf", "details"), "). The abruptness of this change is clear in the sample data. However, the WRTDS model portrays the change as occurring more gradually because of the way that it weights samples. Most water quality changes in large rivers are in fact gradual because they are caused by the cumulative effect of many changes that are not synchronized. In some cases, the effects of projects that intend to improve water quality may be obscured or accentuated by other 'unintentional' changes in the watershed, such as urbanization or changes in farming practices. Therefore, one must be cautious about attributing a quantitative change in water quality from this analysis to any particular action."),
                   br(),
                   tags$li(tags$b("What is a water year?"), "The timespan between October 1 and September 30 of the next year, differing from the calendar year because part of the precipitation that falls in late autumn and winter accumulates as snow and does not drain until the following spring or summer. A water year is named for the calendar year in which it ends."),
                   br()
                  )
        ),
        
        tabPanel("Concentration Summary", br(),
                 fluidRow(
                   plotlyOutput("concSummaryPlot", height=720)),
                 fluidRow(br(),
                   "This plot summarizes the status and trends of the selected parameter at all sites over the selected time period, with the selected site highlighted. The horizontal axis is the flow-normalized concentration in the most recent water year of the selected time period. The vertical axis is the annual percent change in the parameter across the selected time period (Note that for non-linear trends, this value may not represent all parts of the time period). The size of each circle is proportional to the length of the record within the selected time period.")),
        
        tabPanel("Flux Summary", br(),
                 fluidRow(
                   plotlyOutput("fluxSummaryPlot", height=720)),
                 fluidRow(br(),
                          "This plot shows trends in the flux (load) of the selected parameter at all non-nested sites. The width of each colored band in a particular year is the flux at one site and the total height of all bands is the sum of fluxes at all sites with flux estimates for that year. In this plot, sites are sorted by length of record, data gaps are filled by interpolation, and fluxes for sites whose watersheds are partially in other states are reduced by the fraction of watershed outside of Wisconsin.")),
        
        tabPanel("Parameter Comparison",
                 fluidPage(
                   selectInput("parameter2", 
                                label=h4("Select parameter"), 
                                choices=c("Total Phosphorus",
                                          "Orthophosphate",
                                          "Nitrate",
                                          "Kjeldahl Nitrogen",
                                          "Ammonia",
                                          "Chlorophyll a",
                                          "Total Suspended Solids",
                                          "Chloride",
                                          "Silica"), 
                                selected="Nitrate"),
                   plotlyOutput("parPairPlot", height=650), br(),
                   "This plot shows how the relationship between any two water quality parameters has changed over time at all sites, with the selected site highlighted. Select the parameter for the horizontal axis from the menu below the map. Select the parameter for the vertical axis from the menu above this plot. Then press play to start the animation. The animation may be moved manually and paused by dragging the date slider.")),

        tabPanel("Annual Concentration",
                 h4(textOutput("conc_name"), align="center"),
                 plotlyOutput("concPlot", height=720),
                 helpText("This plot shows WRTDS-estimated annual mean concentrations of the selected parameter. Points are annual estimates (not flow normalized), line is flow-normalized estimate, and gray band (if present) is the 90% confidence interval around the flow-normalized estimate (LCL and UCL are lower and upper confidence limits).")),
        
        tabPanel("Annual Flux",
                 h4(textOutput("flux_name"), align="center"),
                 plotlyOutput("fluxPlot", height=720),
                 helpText("This plot shows WRTDS-estimated annual mean flux of the selected parameter. Points are annual estimates (not flow normalized), line is flow-normalized estimate, and gray band (if present) is the 90% confidence interval around the flow-normalized estimate.")),
        
        tabPanel("Season/Flow",
                h4(textOutput("season_name"), align="center"),
                fluidRow(
                  column(6, plotlyOutput("winterPlot")),
                  column(6, plotlyOutput("springPlot"))
                ),
                fluidRow(
                  column(6, plotlyOutput("summerPlot")),
                  column(6, plotlyOutput("fallPlot"))
                ),
                fluidRow(
                  "These plots are useful for understanding the influence of season and streamflow on concentration at a point in time and for determining the conditions under which the greatest changes in water quality over time have occurred. For each season, the low and high flows are the 10th and 90th percentiles of flows during that season at that site."
                )),
        
        tabPanel("Daily Concentration",
                 h4(textOutput("daily_name"), align="center"),
                 plotlyOutput("dailyPlot", height=800), 
                 helpText("The top plot shows measured and daily WRTDS-estimated concentrations of the selected parameter. The bottom plot shows river discharge. Drag or stretch the range slider bar at the bottom of the plot to change the time span. Note that while for some sites and parameters, the WRTDS model does not reproduce measured concentrations very well, for most purposes, we are interested not in the fit to any particular daily value, but rather to summary statistics, such as annual flow-weight mean concentration. The annual plots with confidence intervals illustrate the model fit when aggregated at an annual time step."))
        
      )
    )
  )
)

#### Server #############################################################################

server = function(input, output, session) {
  
  # Reactive value list
  RV = reactiveValues(
    click = NULL,
    station = "543001",
    parameter = "TP",
    parameter2 = "NO3")
  
  # Observe station selection
  observeEvent(input$map_marker_click, {
    RV$click = input$map_marker_click
  })
  
  observeEvent(RV$click, {
    RV$station = RV$click$id
  })  
  
  # Render plot names
  output$conc_name = renderText({stations$station_name[stations$swims_id==RV$station]})
  output$flux_name = renderText({stations$station_name[stations$swims_id==RV$station]})
  output$season_name = renderText({stations$station_name[stations$swims_id==RV$station]})
  output$daily_name = renderText({stations$station_name[stations$swims_id==RV$station]})
  output$site_name = renderText({stations$station_name[stations$swims_id==RV$station]})
  output$drainage = renderText({stations$drainage_area_mi2[stations$swims_id==RV$station]})
  
  # Observe parameter and period selections
  observeEvent(input$parameter, {
    observeEvent(input$parameter2, {
      observeEvent(input$WYrange, {
        
        # Update station selection
        observe({
          updateSelectInput(session,"station",
                            choices=stations$station_name,
                            selected=ifelse(is.null(input$map_marker_click),"Rock River at Afton, WI", 
                                            stations$station_name[stations$swims_id==input$map_marker_click$id]))
        
            # Map marker border width
            stations$weight = 1
            stations$weight[trend$swims_id==RV$station] = 5
            RV$weight = stations$weight
              
            RV$parameter = parameters$abbrev[parameters$name==input$parameter]
            if (RV$parameter=="CHL") {RV$concUnits = "ug/L"} else {RV$concUnits = "mg/L"}
            
            RV$parameter2 = parameters$abbrev[parameters$name==input$parameter2]
            if (RV$parameter2=="CHL") {RV$concUnits2 = "ug/L"} else {RV$concUnits2 = "mg/L"}
            
            # Set up data for parameter comparison plot  
            par_pair = par_pairs[[RV$parameter]][[RV$parameter2]]
            par_pair$lwd[par_pair$swims_id==RV$station & par_pair$waterYear==par_pair$animYear] = 5
            par_pair = par_pair[order(par_pair$animYear, par_pair$lwd),]
            RV$par_pair = par_pair
        
        })
        
        # Restrict period to be at least 5 years
        if(input$WYrange[2]-input$WYrange[1]<5) {
          if(input$WYrange[2]>last_year-5) {
            updateSliderInput(session, "WYrange", value=c(input$WYrange[2]-5,(input$WYrange[2])))
          } else {
          updateSliderInput(session, "WYrange", value=c(input$WYrange[1],(input$WYrange[1]+5)))
          }
        }
        RV$WYrange = input$WYrange
        
        # Set up data for concentration summary plot
        all_trends = year_pairs[[RV$parameter]]
        trend = stations
        ast = data.frame()
        
        # Select periods for each station that are within selected period
        for (s in 1:nrow(trend)) {
          st = all_trends[all_trends$swims_id==trend$swims_id[s],]
          if(nrow(st)==0) {next}
          st$fyd = st$first_year - RV$WYrange[1]
          st$lyd = RV$WYrange[2] - st$last_year
          st = st[st$fyd>=0 & st$lyd>=0,]
          st = st[st$fyd==min(st$fyd) & st$lyd==min(st$lyd),]
          ast = rbind(ast, st)
        }
        trend = merge(trend, ast[,!names(ast) %in% "station_name"], by="swims_id", all.x=TRUE)
        trend = trend[order(trend$station_name),]
        
        # Concentration summary plot marker size
        trend$size = (((1-0.3)/(max(trend$years, na.rm=TRUE)-min(trend$years, na.rm=TRUE)))
                      *(trend$years-max(trend$years, na.rm=TRUE))+1)*30
        RV$trend = trend
        RV$nsites = nrow(trend[!is.na(trend$years),])
        
        # Map marker radius
        radius = trend$last_conc
        radius[is.na(radius)] = median(radius, na.rm=TRUE)
        RV$radius = (((1-0.2)/(max(radius)-min(radius)))*(radius-max(radius))+1)*17
        
        # Map and concentration summary plot colors
        trend$color = NA
        trend$color[trend$direction=="increase" & trend$prob>=0.95] = "#FF5722"
        trend$color[trend$direction=="increase" & trend$prob>=0.75 & trend$prob<0.95] = "#FFAF96"
        trend$color[trend$prob<0.75] = "#939393"
        trend$color[trend$direction=="decrease" & trend$prob>=0.75 & trend$prob<0.95] = "#B5C0FF"
        trend$color[trend$direction=="decrease" & trend$prob>=0.95] = "#3F51B5"
        RV$color = trend$color
        
        # Map and concentration summary plot opacities
        trend$opacity[!is.na(trend$color)] = 0.7
        trend$opacity[is.na(trend$color)] = 0
        RV$opacity = trend$opacity
        
        # Set up data for flux summary plot
        flux = years[[RV$parameter]]
        flux = flux[flux$swims_id %in% stations$swims_id[stations$flux_plot==1],]
        RV$flux = flux
        flux_stations = stations[stations$flux_plot==1,]
        for (s in 1:nrow(flux_stations)) {
          flux_stations$first_year[s] = min(flux$waterYear[flux$swims_id==flux_stations$swims_id[s]])
        }
        flux_stations = flux_stations[order(flux_stations$first_year),]
        RV$flux_stations = flux_stations

      })
    })
  })
  
  # Map
  output$map = renderLeaflet({
    leaflet() %>%
      addProviderTiles("Esri.WorldTopoMap") %>%
      addCircleMarkers(lng=stations$lon, lat=stations$lat, layerId=stations$swims_id, 
                       radius=RV$radius, color="black", weight=1, fillColor=RV$color, 
                       fill=T, stroke=TRUE, fillOpacity=RV$opacity, opacity=0.7,
                       popup=stations$station_name) %>%
      addLegend("topright",
                title=parameters$name[parameters$abbrev==RV$parameter],
                opacity=RV$opacity,
                colors=c("#FF5722",
                         "#FFAF96",
                         "#939393",
                         "#B5C0FF",
                         "#3F51B5",
                         "#FFFFFF"),
                labels=c("Clear Increase",
                         "Possible Increase",
                         "No Trend",
                         "Possible Decrease",
                         "Clear Decrease",
                         "Not Enough Data"))
  })
  
    
  
  #### Plots ################################################################################
  
  # Daily plot
  output$dailyPlot = renderPlotly({
    
    validate(
      need(try(
        load(paste0("models/",RV$parameter,"_",RV$station,".RData"))), 
        "There are no samples of this parameter at this site."))
    
    Daily = eList$Daily
    WY = eList$WY
    periods = eList$periods
    SampleUncen = eList$Sample[!is.na(eList$Sample$ConcLow),]
    SampleCen = eList$Sample[is.na(eList$Sample$ConcLow),]
    if("rObserved" %in% names(SampleCen)) {SampleCen$ConcPlot = SampleCen$rObserved}
    else {SampleCen$ConcPlot = SampleCen$ConcAve}
    SampleOutlier = eList$SampleAll[(eList$SampleAll$Date %in% eList$Daily$Date) & !(eList$SampleAll$Date %in% eList$Sample$Date),]
    SampleNoQ = eList$SampleAll[!(eList$SampleAll$Date %in% eList$Daily$Date),]
    end_date = max(eList$SampleAll$Date) + 30
    resi_sd = sd(SampleUncen$rResid)
    
    p1 = plot_ly(type="scatter", mode="lines")
    
    if("ConcDay" %in% colnames(Daily)) {
    if(!("periods" %in% names(eList))) {periods = data.frame(start=1,end=length(unique(Daily$waterYear)))}
    for (i in 1:nrow(periods)) {
      if(i==1) {legend=TRUE} else {legend=FALSE}
      ir = c(periods$start[i]:periods$end[i])
      if(!("WY" %in% names(eList))) {pDaily = Daily} 
      else {pDaily = Daily[Daily$waterYear %in% WY$waterYear[ir],]}
            p1 = add_trace(p1,
                          data=pDaily,
                          x=~Date,
                          y=~ConcDay,
                          name="WRTDS Estimate",
                          showlegend=legend,
                          line=list(width=1, color="gray"),
                          hoverlabel=list(bgcolor="white"),
                          hoverinfo="text", 
                          text=paste0("WRTDS Estimate", "<br>", 
                                      format(pDaily$Date, "%B %d, %Y"), "<br>", 
                                      RV$parameter, ": ", signif(pDaily$ConcDay,3), " ", RV$concUnits))
      }
    }
    
    p1 = add_trace(p1,
                   data=SampleUncen,
                   x=~Date,
                   y=~ConcAve,
                   mode="markers",
                   name="Measured",
                   marker=list(
                     color="black",
                     line=list(
                       color="black",
                       width=1
                     )),
                   hoverlabel=list(bgcolor="black"),
                   hoverinfo="text", 
                   text=paste0("Measured", "<br>", 
                               format(SampleUncen$Date, "%B %d, %Y"), "<br>", 
                               RV$parameter, ": ", signif(SampleUncen$ConcAve,3), " ", RV$concUnits))
    
    if(nrow(SampleNoQ)>0) {
    p1 = add_trace(p1,
                   data=SampleNoQ,
                   x=~Date,
                   y=~ConcAve,
                   mode="markers",
                   name="Measured (No Flow)",
                   marker=list(
                     color="gray",
                     line=list(
                       color="black",
                       width=1
                     )),
                   hoverlabel=list(bgcolor="black"),
                   hoverinfo="text", 
                   text=paste0("Measured (No Flow)", "<br>", 
                               format(SampleNoQ$Date, "%B %d, %Y"), "<br>", 
                               RV$parameter, ": ", signif(SampleNoQ$ConcAve,3), " ", RV$concUnits))
    }
    
    # if(nrow(SampleOutlier)>0) {
    #   p1 = add_trace(p1,
    #                  data=SampleOutlier,
    #                  x=~Date,
    #                  y=~ConcAve,
    #                  mode="markers",
    #                  name="Measured (Outlier)",
    #                  marker=list(
    #                    color="gray",
    #                    line=list(
    #                      color="black",
    #                      width=1
    #                    )),
    #                  hoverlabel=list(bgcolor="black"),
    #                  hoverinfo="text", 
    #                  text=paste0("Measured (Outlier)", "<br>", 
    #                              format(SampleOutlier$Date, "%B %d, %Y"), "<br>", 
    #                              RV$parameter, ": ", signif(SampleOutlier$ConcAve,3), " ", RV$concUnits))
    # }
    
    if(nrow(SampleCen)>0) {
    p1 = add_trace(p1,
                   data=SampleCen,
                   x=~Date,
                   y=~ConcPlot,
                   mode="markers",
                   name="Estimated Censored",
                   marker=list(
                     color="white",
                     line=list(
                       color="black",
                       width=1
                     )),
                   hoverlabel=list(bgcolor="black"),
                   hoverinfo="text", 
                   text=paste0("Estimated Censored", "<br>", 
                               format(SampleCen$Date, "%B %d, %Y"), "<br>", 
                               RV$parameter, ": ", signif(SampleCen$ConcPlot,3), " ", RV$concUnits))
    }

      
    p1 = layout(p1,
               yaxis=list(title=paste0(RV$parameter," (",RV$concUnits,")"), type="log"),
               xaxis=list(title="", range=list(end_date-365*10, end_date)))
    
    p2 = plot_ly(type="scatter", mode="lines")
    
    if ("Q" %in% colnames(Daily)) {
    if(!("periods" %in% names(eList))) {periods = data.frame(start=1,end=length(unique(Daily$waterYear)))}
    for (i in 1:nrow(periods)) {
      if(i==1) {legend=TRUE} else {legend=FALSE}
      ir = c(periods$start[i]:periods$end[i])
      if(!("WY" %in% names(eList))) {pDaily = Daily} 
      else {pDaily = Daily[Daily$waterYear %in% WY$waterYear[ir],]}
        p2 = add_trace(p2,
                       data=pDaily,
                       x=~Date,
                       y=~Q*35.31467,
                       name="Discharge",
                       showlegend=legend,
                       line=list(width=1, color="black"),
                       hoverlabel=list(bgcolor="white"),
                       hoverinfo="text", 
                       text=paste0(format(pDaily$Date, "%B %d, %Y"), "<br>", 
                                   signif(pDaily$Q*35.31467,3), " cfs"))
      }
    }
    
    p2 = layout(p2,
                yaxis=list(title="Discharge (cfs)", type="log"),
                xaxis=list(title="", range=list(end_date-365*10, end_date))
                )
    
    p3 = subplot(p1, p2, nrows=2, heights=c(0.7,0.3), shareX=TRUE, titleY=TRUE)
    
    p3 = layout(p3,
                xaxis=list(rangeslider=list(type="date", thickness=0.05)))
    
  })
  
  # Annual concentration plot
  output$concPlot = renderPlotly({
    
    validate(
      need(try(
        load(paste0("models/",RV$parameter,"_",RV$station,".RData"))), 
        "There are no samples of this parameter at this site."))
    
    validate(need("WY" %in% names(eList),
        "There are not enough samples of this parameter at this site to evaluate trends."))
    
    validate(need("FNConc" %in% names(eList$WY),
                  "There are not enough samples of this parameter at this site to evaluate trends."))
    
    WY = eList$WY
    periods = eList$periods
    
    if ("FNConc95" %in% colnames(WY)) {
      ymax = max(WY$FNConc95[WY$plot_conc==1], WY$FNConc, WY$Conc, na.rm=TRUE)*1.25
    } else {
      ymax = max(WY$Conc, WY$FNConc, na.rm=TRUE)*1.25
    }
    
    p = plot_ly(type="scatter", mode="lines", showlegend=FALSE)
      
    if ("FNConc05" %in% colnames(WY)) {
      for (i in 1:nrow(periods)) {
        ir = c(periods$start[i]:periods$end[i])
        
        p = add_trace(p,
                      data=WY[ir,],
                      x=~waterYear,
                      y=~FNConc05,
                      line=list(width=0),
                      hoverlabel=list(bgcolor="white"), 
                      hoverinfo="text",
                      text=paste0("Water Year: ", WY$waterYear[ir], "<br>", 
                                  "LCL flow-normalized", "<br>", 
                                  RV$parameter, ": ", signif(WY$FNConc05[ir],3), " ", RV$concUnits))
          
        p = add_trace(p,
                      data=WY[ir,],
                      x=~waterYear,
                      y=~FNConc95,
                      line=list(width=0),
                      fill="tonexty",
                      fillcolor="lightgray",
                      hoverlabel=list(bgcolor="white"),
                      hoverinfo="text",
                      text=paste0("Water Year: ", WY$waterYear[ir], "<br>", 
                                  "UCL flow-normalized", "<br>",
                                  RV$parameter, ": ", signif(WY$FNConc95[ir],3), " ", RV$concUnits))
        }
      }
      
      p = add_trace(p,
                    data=WY,
                    x=~waterYear,
                    y=~FNConc,
                    line=list(width=1, color="black"),
                    hoverlabel=list(bgcolor="white"),
                    hoverinfo="text", 
                    text=paste0("Water Year: ", WY$waterYear, "<br>", 
                                "Flow-normalized", "<br>",
                                RV$parameter, ": ", signif(WY$FNConc,3), " ", RV$concUnits))
      
      p = add_trace(p,
                    data=WY,
                    x=~waterYear,
                    y=~Conc,
                    mode="markers",
                    marker=list(color="black"),
                    hoverlabel=list(bgcolor="white"),
                    hoverinfo="text",
                    text=paste0("Water Year: ", WY$waterYear, "<br>",
                                RV$parameter, ": ", signif(WY$Conc,3), " ", RV$concUnits))
      
      p = layout(p,
                 yaxis=list(title=paste0(RV$parameter," (",RV$concUnits,")"), range=list(0,ymax)),
                 xaxis=list(title=""))
    p  
  })
  
  # Annual flux plot
  output$fluxPlot = renderPlotly({
    
    validate(
      need(try(
        load(paste0("models/",RV$parameter,"_",RV$station,".RData"))), 
        "There are no samples of this parameter at this site."))
    
    validate(need("WY" %in% names(eList),
        "There are not enough samples of this parameter at this site to evaluate trends."))
    
    validate(need("FNFlux" %in% names(eList$WY),
                  "There are not enough samples of this parameter at this site to evaluate trends."))
    
    WY = eList$WY
    periods = eList$periods
    
    if ("FNFlux95" %in% colnames(WY)) {
      ymax = max(WY$FNFlux95[WY$plot_flux==1], WY$FNFlux, WY$Flux, na.rm=TRUE)*1.25
    } else {
      ymax = max(WY$Flux, WY$FNFlux, na.rm=TRUE)*1.25
    }
    
    p = plot_ly(type="scatter", mode="lines", showlegend=FALSE)
    
    if ("FNFlux05" %in% colnames(WY)) {
      for (i in 1:nrow(periods)) {
        ir = c(periods$start[i]:periods$end[i])
        
        p = add_trace(p,
                      data=WY[ir,],
                      x=~waterYear, 
                      y=~FNFlux05,
                      line=list(width=0),
                      hoverlabel=list(bgcolor="white"),
                      hoverinfo="text",
                      text=paste0("Water Year: ", WY$waterYear[ir], "<br>", 
                                  "LCL flow-normalized", "<br>",
                                  RV$parameter, ": ", signif(WY$FNFlux05[ir],3), " tons"))
          
        p = add_trace(p,
                      data=WY[ir,],
                      x=~waterYear, 
                      y=~FNFlux95,
                      line=list(width=0),
                      fill="tonexty",
                      fillcolor="lightgray",
                      hoverlabel=list(bgcolor="white"),
                      hoverinfo="text",
                      text=paste0("Water Year: ", WY$waterYear[ir], "<br>", 
                                  "UCL flow-normalized", "<br>",
                                  RV$parameter, ": ", signif(WY$FNFlux95[ir],3), " tons"))
      }
    }
    
    p = add_trace(p, 
                  data=WY,
                  x=~waterYear,
                  y=~FNFlux,
                  line=list(width=1, color="black"),
                  hoverlabel=list(bgcolor="white"),
                  hoverinfo="text",
                  text=paste0("Water Year: ", WY$waterYear, "<br>", 
                              "Flow-normalized", "<br>",
                              RV$parameter, ": ", signif(WY$FNFlux,3), " tons"))
      
    p = add_trace(p,
                data=WY,
                x=~waterYear,
                y=~Flux,
                mode="markers",
                marker=list(color="black"),
                hoverlabel=list(bgcolor="white"),
                hoverinfo="text",
                text=paste0("Water Year: ", WY$waterYear, "<br>",
                            RV$parameter, ": ", signif(WY$Flux,3), " tons"))
      
    p = layout(p,
             yaxis=list(title=paste(RV$parameter,"(tons)"), range=list(0,ymax)),
             xaxis=list(title=""))
    p  
  })
  
  # Winter plot
  output$winterPlot = renderPlotly({
    
    validate(
      need(try(
        load(paste0("models/",RV$parameter,"_",RV$station,".RData"))), 
        "There are no samples of this parameter at this site."))
    
    validate(need("seasons" %in% names(eList),
                  "There are not enough samples of this parameter at this site to evaluate trends."))
    
    t = 1
    seasons = eList$seasons
    trends = seasons[[t]]$trends
    legend = seasons[[t]]$legend
    WY = eList$WY
    
    p = plot_ly(type="scatter", mode="lines")
    
    p = add_trace(p,
                  data=trends,
                  x=~waterYear,
                  y=~low,
                  name=legend[1], 
                  line=list(dash="dot", width=1, color="black"),
                  hoverinfo="text", 
                  hoverlabel=list(bgcolor="white"), 
                  text=paste0("Water Year: ", trends$waterYear, "<br>", 
                              "Low flow (", legend[1], ")", "<br>",
                              RV$parameter, ": ", signif(trends$low,3), " ", RV$concUnits))
    
    p = add_trace(p,
                  x=~waterYear,
                  y=~mean,
                  name=legend[2], 
                  line=list(dash="dash", width=1, color="black"),
                  hoverinfo="text", 
                  hoverlabel=list(bgcolor="white"), 
                  text=paste0("Water Year: ", trends$waterYear, "<br>", 
                              "Mean flow (", legend[2], ")", "<br>",
                              RV$parameter, ": ", signif(trends$mean,3), " ", RV$concUnits))
    
    p = add_trace(p,
                  x=~waterYear,
                  y=~high,
                  name=legend[3], 
                  line=list(dash="solid", width=1, color="black"),
                  hoverinfo="text", 
                  hoverlabel=list(bgcolor="white"), 
                  text=paste0("Water Year: ", trends$waterYear, "<br>", 
                              "High flow (", legend[3], ")", "<br>",
                              RV$parameter, ": ", signif(trends$high,3), " ", RV$concUnits))
    
    p = layout(p, 
               title=seasons[[t]]$name,
               yaxis=list(title=paste0(RV$parameter," (",RV$concUnits,")"), range=c(0,seasons$ylims[2])),
               xaxis=list(title=""))
    p
  })
  
  # Spring plot
  output$springPlot = renderPlotly({
    
    validate(
      need(try(
        load(paste0("models/",RV$parameter,"_",RV$station,".RData"))), 
        "There are no samples of this parameter at this site."))
    
    validate(need("seasons" %in% names(eList),
                  ""))
    
    t = 2
    seasons = eList$seasons
    trends = seasons[[t]]$trends
    legend = seasons[[t]]$legend
    WY = eList$WY
    
    p = plot_ly(type="scatter", mode="lines")
    
    p = add_trace(p,
                  data=trends,
                  x=~waterYear,
                  y=~low,
                  name=legend[1], 
                  line=list(dash="dot", width=1, color="black"),
                  hoverinfo="text", 
                  hoverlabel=list(bgcolor="white"), 
                  text=paste0("Water Year: ", trends$waterYear, "<br>", 
                              "Low flow (", legend[1], ")", "<br>",
                              RV$parameter, ": ", signif(trends$low,3), " ", RV$concUnits))
    
    p = add_trace(p,
                  x=~waterYear,
                  y=~mean,
                  name=legend[2], 
                  line=list(dash="dash", width=1, color="black"),
                  hoverinfo="text", 
                  hoverlabel=list(bgcolor="white"), 
                  text=paste0("Water Year: ", trends$waterYear, "<br>", 
                              "Mean flow (", legend[2], ")", "<br>",
                              RV$parameter, ": ", signif(trends$mean,3), " ", RV$concUnits))
    
    p = add_trace(p,
                  x=~waterYear,
                  y=~high,
                  name=legend[3], 
                  line=list(dash="solid", width=1, color="black"),
                  hoverinfo="text", 
                  hoverlabel=list(bgcolor="white"), 
                  text=paste0("Water Year: ", trends$waterYear, "<br>", 
                              "High flow (", legend[3], ")", "<br>",
                              RV$parameter, ": ", signif(trends$high,3), " ", RV$concUnits))
    
    p = layout(p, 
               title=seasons[[t]]$name,
               yaxis=list(title=paste0(RV$parameter," (",RV$concUnits,")"), range=c(0,seasons$ylims[2])),
               xaxis=list(title=""))
    p
  })
  
  # Summer plot
  output$summerPlot = renderPlotly({
    
    validate(
      need(try(
        load(paste0("models/",RV$parameter,"_",RV$station,".RData"))), 
        "There are no samples of this parameter at this site."))
    
    validate(need("seasons" %in% names(eList),
                  ""))
    
    t = 3
    seasons = eList$seasons
    trends = seasons[[t]]$trends
    legend = seasons[[t]]$legend
    WY = eList$WY
    
    p = plot_ly(type="scatter", mode="lines")
    
    p = add_trace(p,
                  data=trends,
                  x=~waterYear,
                  y=~low,
                  name=legend[1], 
                  line=list(dash="dot", width=1, color="black"),
                  hoverinfo="text", 
                  hoverlabel=list(bgcolor="white"), 
                  text=paste0("Water Year: ", trends$waterYear, "<br>", 
                              "Low flow (", legend[1], ")", "<br>",
                              RV$parameter, ": ", signif(trends$low,3), " ", RV$concUnits))
    
    p = add_trace(p,
                  x=~waterYear,
                  y=~mean,
                  name=legend[2], 
                  line=list(dash="dash", width=1, color="black"),
                  hoverinfo="text", 
                  hoverlabel=list(bgcolor="white"), 
                  text=paste0("Water Year: ", trends$waterYear, "<br>", 
                              "Mean flow (", legend[2], ")", "<br>",
                              RV$parameter, ": ", signif(trends$mean,3), " ", RV$concUnits))
    
    p = add_trace(p,
                  x=~waterYear,
                  y=~high,
                  name=legend[3], 
                  line=list(dash="solid", width=1, color="black"),
                  hoverinfo="text", 
                  hoverlabel=list(bgcolor="white"), 
                  text=paste0("Water Year: ", trends$waterYear, "<br>", 
                              "High flow (", legend[3], ")", "<br>",
                              RV$parameter, ": ", signif(trends$high,3), " ", RV$concUnits))
    
    p = layout(p, 
               title=seasons[[t]]$name,
               yaxis=list(title=paste0(RV$parameter," (",RV$concUnits,")"), range=c(0,seasons$ylims[2])),
               xaxis=list(title=""))
    p
  })
  
  # Fall plot
  output$fallPlot = renderPlotly({
    
    validate(
      need(try(
        load(paste0("models/",RV$parameter,"_",RV$station,".RData"))), 
        "There are no samples of this parameter at this site."))
    
    validate(need("seasons" %in% names(eList),
       ""))
    
    t = 4
    seasons = eList$seasons
    trends = seasons[[t]]$trends
    legend = seasons[[t]]$legend
    WY = eList$WY
    
    p = plot_ly(type="scatter", mode="lines")
    
    p = add_trace(p,
                  data=trends,
                  x=~waterYear,
                  y=~low,
                  name=legend[1], 
                  line=list(dash="dot", width=1, color="black"),
                  hoverinfo="text", 
                  hoverlabel=list(bgcolor="white"), 
                  text=paste0("Water Year: ", trends$waterYear, "<br>", 
                              "Low flow (", legend[1], ")", "<br>",
                              RV$parameter, ": ", signif(trends$low,3), " ", RV$concUnits))
    
    p = add_trace(p,
                  x=~waterYear,
                  y=~mean,
                  name=legend[2], 
                  line=list(dash="dash", width=1, color="black"),
                  hoverinfo="text", 
                  hoverlabel=list(bgcolor="white"), 
                  text=paste0("Water Year: ", trends$waterYear, "<br>", 
                              "Mean flow (", legend[2], ")", "<br>",
                              RV$parameter, ": ", signif(trends$mean,3), " ", RV$concUnits))
    
    p = add_trace(p,
                  x=~waterYear,
                  y=~high,
                  name=legend[3], 
                  line=list(dash="solid", width=1, color="black"),
                  hoverinfo="text", 
                  hoverlabel=list(bgcolor="white"), 
                  text=paste0("Water Year: ", trends$waterYear, "<br>", 
                              "High flow (", legend[3], ")", "<br>",
                              RV$parameter, ": ", signif(trends$high,3), " ", RV$concUnits))
    
    p = layout(p, 
               title=seasons[[t]]$name,
               yaxis=list(title=paste0(RV$parameter," (",RV$concUnits,")"), range=c(0,seasons$ylims[2])),
               xaxis=list(title=""))
    p
  })
  
  # Concentration summary plot
  output$concSummaryPlot = renderPlotly({
    
    validate(need(RV$nsites>0,
                  "There are not enough samples of this parameter at any site during the selected time period to evaluate trends. Select a different time period."))
    
    all = RV$trend
    all$color = RV$color
    all = all[!is.na(all$years),]
    sel = all[all$swims_id==RV$station,]
    
    p = plot_ly(type="scatter", mode="markers", showlegend=FALSE)
    
    p = add_trace(p,
                  data=all,
                  x=~last_conc, 
                  y=~delta_pct_year, 
                  marker=list(size=~size,
                              color=~color,
                              line=list(
                                width=0
                              )),
                  hoverinfo="text", 
                  hoverlabel=list(bgcolor="white"), 
                  text=paste0(all$station_name, "<br>",
                    signif(all$first_conc,3), " ", RV$concUnits, " in ", all$first_year, "<br>",
                    signif(all$last_conc,3), " ", RV$concUnits," in ", all$last_year, "<br>",
                    round(all$delta_pct_abs,0), "% ", all$direction, " in ", all$years, " years", "<br>",
                    round(all$delta_pct_year_abs,1), "% ", all$direction, " per year"))
    
    if(nrow(sel)==1) {  
    p = add_trace(p, 
                  data=sel,
                  x=~last_conc,
                  y=~delta_pct_year,
                  marker=list(size=~size,
                              color='rgba(0,0,0,0)',
                              line=list(
                                color="black",
                                width=3)),
                  hoverinfo="text", 
                  hoverlabel=list(bgcolor="white"), 
                  text=paste0(sel$station_name, "<br>",
                    signif(sel$first_conc,3), " ", RV$concUnits, " in ", sel$first_year, "<br>",
                    signif(sel$last_conc,3), " ", RV$concUnits, " in ", sel$last_year, "<br>",
                    round(sel$delta_pct_abs,0), "% ", sel$direction, " in ", sel$years, " years", "<br>",
                    round(sel$delta_pct_year_abs,1), "% ", sel$direction, " per year"))
    }
      
    p = layout(p,
               title=input$parameter, 
               yaxis=list(title="Annual % Change"), 
               xaxis=list(title=paste0(RV$parameter," (",RV$concUnits,")")))
    p
    
  })
    
  # Flux summary plot
  output$fluxSummaryPlot = renderPlotly({
    
    p = plot_ly(type="scatter",
                mode="none",
                stackgroup="one",
                showlegend=FALSE)
    
    for (s in 1:nrow(RV$flux_stations)) {
      sdata = RV$flux[RV$flux$swims_id==RV$flux_stations$swims_id[s],]
      if(nrow(sdata)==0) {next}
      p = add_trace(p,
                    name = RV$flux_stations$station_name[s],
                    data=sdata,
                    x=~waterYear, 
                    y=~FNFlux,
                    hoverinfo="text", 
                    hoverlabel=list(bgcolor="white"), 
                    text=paste0(RV$flux_stations$station_name[s], "<br>",
                                sdata$waterYear, " ", RV$parameter,
                                " Flux: ", signif(sdata$FNFlux,3), " tons"))
      
      cflux = RV$flux[RV$flux$swims_id %in% RV$flux_stations$swims_id[1:s],]
      cflux = aggregate(cflux[,c("FNFlux","WSA")], by=list(cflux$waterYear), FUN="sum")
      names(cflux) = c("waterYear","FNFlux","WSA")
      
      p = add_trace(p,
                    name = "cflux",
                    data=cflux,
                    x=~waterYear, 
                    y=0,
                    hoverinfo="text", 
                    hoverlabel=list(bgcolor="white"),
                    text=paste0(RV$parameter, " flux from ",
                                round((cflux$WSA/56081)*100,0),
                                "% of WI in ", cflux$waterYear, ": ",
                                signif(cflux$FNFlux,3), " tons", "<br>",
                                RV$flux_stations$station_name[s], "<br>",
                                "plus all sites lower on plot"))
    }
    
    p = layout(p,
               title=parameters$name[parameters$abbrev==RV$parameter], 
               yaxis=list(title=paste(RV$parameter,"(tons)")), 
               xaxis=list(title="", range=c(1959,2021)))
    p
    
  })
  
  # Parameter pair scatter plot animation
  output$parPairPlot = renderPlotly({

    p = plot_ly(type="scatter", mode="markers", showlegend=FALSE)
    
    p = add_trace(p,
                  data=RV$par_pair,
                  x=~FNConc.x,
                  y=~FNConc.y,
                  frame=~animYear,
                  ids=~id,
                  marker=list(size=~size,
                              color="#319131",
                              opacity=~opacity,
                              line=list(
                                color="black",
                                width=~lwd
                              )),
                  hoverinfo="text",
                  text=~station_name,
                  hoverlabel=list(bgcolor="white"))
    
    p = animation_slider(p, currentvalue = list(prefix="",font = list(color="black")))
    
    p = layout(p,
               yaxis=list(title=paste0(RV$parameter2," (",RV$concUnits2,")")),
               xaxis=list(title=paste0(RV$parameter," (",RV$concUnits,")")))
    
    p
    
  })

}

shinyApp(ui, server)
