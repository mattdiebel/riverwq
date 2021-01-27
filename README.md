# riverwq

[R Shiny app](https://wisconsindnr.shinyapps.io/riverwq/) for exploring river water quality trends in Wisconsin. Trends are characterized with WRTDS in [EGRET](https://github.com/USGS-R/EGRET). To adapt this analysis to a new location:
1. Create your own versions of the stations, parameters, and data files in the same format as the examples here.
2. Edit the file paths at the top of each of the following files, then run them in the following order:
    - assemble_data.R
    - models.R
    - bootstrap.R
    - summary.R
    - app.R
