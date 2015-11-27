## Set of functions to plot and analyse metocean data
## R L HARRIS 2015-10-17
## ===================================================

#' Function to read Mike by DHI text file into a data frame
#'
#'
#' @param data_file ascii file with time series of metocean data
#' @return data_frame
#' @keywords metocean
#' @export
#' @examples
#' readMikeDFS0()
readMikeDFS0 <- function(data_file){
  library(zoo)
  ## TODO: Direct dfs0 connector for R.Net - Need to include long - lat in output
  # Open file as a connector for readlines
  con <- file(data_file)

  # Define data frame from variables data
  data_frame <- read.table(data_file,header=FALSE,sep="\t",skip=3,stringsAsFactors = FALSE,na.strings=c("-1E-30"))
  data_frame[[1]] <- as.POSIXct(data_frame[[1]])

  # Define column names from line 2 in the MIKE format file
  col_names <- strsplit((readLines(con,n = 2)[2]),"\t")
  col_names <- as.vector(col_names[[1]])
  colnames(data_frame) <- col_names
  colnames(data_frame)[1] <- "date"
  colnames(data_frame) <- gsub(pattern = "\\s+", replacement = "_", x = colnames(data_frame), perl=TRUE)

  # Close connection
  close(con)

  # Convert to zoo object
  #data_frame <- zoo(data_frame[seq(2,length(data_frame),1)],
  #  order.by = data_frame[[1]])

  return(data_frame)
}

#' Function to calculate exceedance plots and tables
#'
#'
#' @param df time series of metocean data
#' @param variable index of variable for plotting
#' @param percentiles list of percentiles between 0 and 1
#' @param xlabel string input of xlab for plots
#' @param type string 'monthly' 'season'
#' @param hemisphere southern or northern
#' @return dataframe of exceedances
#' @export
#' @examples
#' getExceedance()
getExceedance <- function(df,variable,
   	percentiles = c(c(0,0.01,0.02,0.05),seq(0.1,0.9,0.1),c(0.95,0.98,0.99,1)),
   	xlabel, type = "annual", hemisphere = "northern", output_type = "plot"){
  	library(lubridate)
    library(Hmisc)
    library(xtable)
    library(tableplot)

  	# Helper function to determine the months in a season
   	getSeasonalData <- function(df,season){
          spring <- c("Sep","Oct","Nov")
          summer <- c("Dec","Jan","Feb")
          autumn <- c("Mar","Apr","May")
          winter <- c("Jun","Jul","Aug")
          if (is.zoo(df)){
            my_season <- df[which(month(ymd_hms(time(df)), label = TRUE) %in% get(season)),]
          }
          else {
            my_season <- df[which(month(ymd_hms(df[[1]]),label = TRUE) %in% get(season)),]
          }
          return (my_season)
  	}
  	# Helper function to get a subset of of the month
  	getMonthlyData <- function(df,month){
          data_results <- df[which(month(ymd_hms(df[[1]]),label = TRUE) %in% month),]
          return(data_results)
  	}

    plotSeasonalECDF <- function(df){
          par(mar=c(5.1, 4.1, 2.1, 2.1),xpd = FALSE)
          cols <- c("red","blue","orange","green")
          seasons <- c("spring","summer","autumn","winter")
          # Plot seasonal ecdf
          plot(ecdf(df[[variable]]),lwd=2,xlab = xlabel,ylab = "Probability on non-exceedance[%]",main="",yaxt="n")

          axis(2,at=percentiles,labels=percentiles)
          grid(col="dark grey",ny="")
          abline(h=percentiles,col="dark grey", lty=3)

          sapply(1:length(seasons), function (x) {plot(ecdf(getSeasonalData(df,seasons[x])[[variable]]),col=cols[x],lwd=2,add=TRUE,xlab="",ylab="",yaxt="n")})

          plot(ecdf(df[[variable]]),lwd=2,xlab = "",ylab = "",main="",add=TRUE,yaxt="n")

          # Legend for plotting
          legend("bottomright", legend=c("annual",seasons), col=c("black",cols),lwd=2,cex=0.8)
    }
  	#
    tabulateSeasonalECDF <- function(df){
          annual <- quantile(df[[variable]],probs = percentiles, na.rm = TRUE)
          row_names <- names(annual)
          annual <- as.numeric(annual)
          seasons <- c("spring","summer","autumn","winter")
          seasons_quants <- sapply(1:length(seasons), function(x) {as.numeric(
                  quantile(getSeasonalData(df,seasons[x])[[variable]],probs = percentiles, na.rm = TRUE))})
          colnames(seasons_quants) <- seasons
          all_df <- cbind(seasons_quants,annual)
          rownames(all_df) <- row_names
          #print(colnames(all_df))
          colnames(all_df) <- capitalize(colnames(all_df))
          print(xtable(all_df), type="html")
    }

    if (output_type == "plot"){
      plotSeasonalECDF(df)
    }
    if (output_type == "table"){
      tabulateSeasonalECDF(df)
      }
}


#' Function to plot wind and wave rose plots
#'
#'
#' @param df time series of metocean data
#' @param hs magnitude variable
#' @param dm direction variable
#' @param bins number of bins in the
#' @export
#' @examples
#' plotRose()
plotRose <- function(df,hs,wd,bins){
    library(openair)
    windRose()
    return(plotObj)
}

#' Function to join time series of files into a single file
#'
#'
#' @param timeFiles vector of strings giving the file names
#' @param rootDir string of the root directory of all the files
#' @return df dataframe with date and variables
#' @export
#' @examples
#' combineData(rootDir,timeFiles)
combineData <- function(rootDir,timeFiles){
  readWaveParameter <- function(file,parameter){
    # Funciton to read individual wave parameters into a time series
    myParamData <- read.table(file = file,header = FALSE,sep="\t",
                              col.names=c("date",parameter))
    myParamData <- myParamData[unique(myParamData[,1]),]
    myParamData$date <- as.POSIXct(myParamData$date)
    #myParamData <- xts(myParamData[[parameter]],myParamData$dat)
    return(myParamData)
  }
  hs_file <- paste(rootDir,timeFiles[1],sep="")
  tp_file <- paste(rootDir,timeFiles[2],sep="")
  dp_file <- paste(rootDir,timeFiles[3],sep="")
  testHs <- readWaveParameter(hs_file,"Hs")
  testTp <- readWaveParameter(tp_file,"Tp")
  testDp <- readWaveParameter(dp_file,"Dp")

  # Combine all files together.
  df <- testHs
  df$Tp <- testTp$Tp
  df$Dp <- testDp$Dp
  return(df)
}

#' Function to generate a pivot table of percentage occurances
#'
#'
#' @param df1 Combined data frame
#' @param variables vecotr of named variables within data frame
#' @param target the value variable
#' @param bins vector of bins for each of the named variables
#' @return pivot_table
#' @export
#' @examples
#' generatePivot(df1,variables,target,bins)
generatePivot <- function(df1,variables,target,bins,...){
    # # Description ==============================================================
    # Author:     Rhydar Lee Harris
    # Date:       2015-11-24 T21:25:13Z
    # Type:       METOCEAN ANALYSIS
    # Description:  Generates a pivot table of wave conditions from a given time
    #
    # ============================================================================
    # Args:
    #   dataFile,variable,...
    # Returns:
    #     table.tex where table.tex is a latex table for inclusion into a report
    library(reshape2)
    library(plyr)
    library(reshape2,dplyr)
    # Make first bin
    # wave_data_bins <- cbind(wave_data_complete,hs_bins = cut(wave_data_complete$Hm0,breaks = hs_bins))
    tmp <- cut(df1[[variables[1]]],breaks = unlist(bins[1]))
    df_list <- c(paste(variables,"_bins",sep=""))
    assign(paste(df_list[1]),as.data.frame(tmp))

    df1_binned <- cbind(df1,get(df_list[1]))
    colnames(df1_binned)[length(df1_binned)] <- df_list[1]

    for (i in 2:length(variables)){
        tmp <- cut(df1[[variables[i]]],breaks = unlist(bins[i]))
        assign(paste(df_list[i]),as.data.frame(tmp))

        df1_binned <- cbind(df1_binned,get(df_list[i]))
        colnames(df1_binned)[length(df1_binned)] <- df_list[i]
    }
    # Aggregate functions
    mean_power <- function(x) {length(x)}

    # Convert to matrix
    pivot_table <- dcast(df1_binned, as.formula(paste(df_list[1],"~",df_list[2])),
                         fun.aggregate = function(x) mean_power(x),value.var = target,margins=FALSE,fill = 0,drop = FALSE)

    rownames(pivot_table) <- pivot_table[,1]
    pivot_table <- pivot_table[c(-1)]
    #if (grepl("NA",names(pivot_table),ignore.case = TRUE)){
    #    pivot_table <- pivot_table[, -which(names(pivot_table) %in% c("NA"))]
    #}
    rownames(pivot_table) <- as.character(unlist(bins[1])[2:length(unlist(bins[1]))])
    colnames(pivot_table) <- as.character(unlist(bins[2])[2:length(unlist(bins[2]))])
    return(pivot_table)
}


#' Function to plot wave scatter plot
#'
#'
#' @param input_df
#' @param col_index Index of required variables for scatter plot
#' @return multiple scatter plot objects
#' @export
#' @examples
#' plotScatter(input_df,col_index)
plotScatter <- function(input_df,col_index = c(2,3,4),labels,...){
  # # Description ==============================================================
  # Author:      Rhydar Lee Harris
  # Date:        2015-03-28 T20:38:24Z
  # Type:        Wave hexbin - Check for DF and Zoo
  # Description:  Generates a hexbin scatter plot matrix
  # ============================================================================
  # Args:
  #   input_df,my_col
  # Returns:
  #      Returns a plot of the data
  library(openair)

  # Get original plotting parameters - Can change after, required for multi-plotting
  #olpdar <- par(no.readonly = TRUE)
  Hs <- colnames(input_df)[col_index[1]]
  Tp <- colnames(input_df)[col_index[2]]
  Dp <- colnames(input_df)[col_index[3]]

  t1 <- scatterPlot(input_df,x=Hs,y=Tp,method = "hexbin",xbins=100,hemisphere="southern",cols = "jet",aspect=1)#,type="season")
  t2 <- scatterPlot(input_df,x=Hs,y=Dp,method = "hexbin",xbins=100,hemisphere="southern",cols = "jet",aspect=1)
  t3 <- scatterPlot(input_df,x=Tp,y=Dp,method = "hexbin",xbins=100,cols = "jet",aspect=1,hemisphere="southern")#,type="season")

  t1$plot$xlab <- list(label = labels[1],cex = 1,scales=list(cex=1))
  t1$plot$ylab <- list(label = labels[2],cex = 1,scales=list(cex=1))
  t1$plot$x.scales$cex = c(1,1)
  t1$plot$y.scales$cex = c(1,1)

  t2$plot$xlab <- list(label = labels[1],cex = 1,scales=list(cex=1))
  t2$plot$ylab <- list(label = labels[3],cex = 1,scales=list(cex=1))
  t2$plot$x.scales$cex = c(1,1)
  t2$plot$y.scales$cex = c(1,1)

  t3$plot$xlab <- list(label = labels[2],cex = 1,scales=list(cex=1))
  t3$plot$ylab <- list(label = labels[3],cex = 1,scales=list(cex=1))
  t3$plot$x.scales$cex = c(1,1)
  t3$plot$y.scales$cex = c(1,1)
  scatterPlots <- list(t1,t2,t3)
  return(scatterPlots)
}

#' Function to plot wave time series
#'
#'
#' @param input_df
#' @return Time series of Hmo, Tp, and Dp
#' @export
#' @examples
#' plotScatter(input_df,col_index)
plotWaveParams <- function(nww3Param){
    plotWaveParam <- function(x,y,myylab,bottom="FALSE",...){
        # Function to plot time series of wave data
        # Save current plot options
        # Convert to tz
        par(mar=c(2,5,1,1))#,oma=c(1,1,1,1))
        plot(x,y,type="l",xaxt="n",yaxt="n",xlab = "", ylab = "",lwd=0.1)
        grid(col="dark grey",lty=2)
        par(new=T)

        if (bottom == FALSE){
            xlab = ""
            plot(x,y,type="l",xlab = xlab, ylab = myylab, col="blue",lwd=0.1, cex.axis = 1.3,cex.lab=1.3)
        }
        else {
            xlab = ""
            plot(x,y,type="l",xlab = xlab, ylab = myylab, col="blue",lwd=0.1, cex.axis = 1.3,cex.lab=1.3)
        }
    }
    op <- par(no.readonly = TRUE)
    # Set 3 by 1 plot
    hm0lab <- expression(paste("H"[m0]," [m]"))
    tplab <- expression(paste("T"[p]," [s]"))
    dplab <- expression(paste("Wave direction TN [deg]"))
    layout(matrix(c(1,2,3), 3, 1, byrow = TRUE))
    plotWaveParam(nww3Param$date,nww3Param$Hs,myylab = hm0lab,xaxt="n")
    plotWaveParam(nww3Param$date,nww3Param$Tp, myylab = tplab,xaxt="n")
    plotWaveParam(nww3Param$date,nww3Param$Dp, myylab = dplab,bottom="TRUE")
    par(op)
}


#' Function to get a pivot table of Hm0 Tp occurances
#'
#'
#' @param input_df
#' @param variables = atomic vector of input variable names for pivot table
#' @param bins = atomic vector of bins for each of the variables
#' @export
#' @examples
#' generatePivot(df,c("Hm0","Tp"),bins = c())
generatePivot <- function(df1,variables,target,bins,...){
    # # Description ==============================================================
    # Author:     Rhydar Lee Harris
    # Date:       2014-08-24 T21:25:13Z
    # Type:       METOCEAN ANALYSIS
    # Description:  Generates a pivot table of wave conditions from a given time
    #
    # ============================================================================
    # Args:
    #   dataFile,variable,...
    # Returns:
    #     table.tex where table.tex is a latex table for inclusion into a report
    library(reshape2)
    library(plyr)
    library(reshape2,dplyr)
    # Make first bin
    # wave_data_bins <- cbind(wave_data_complete,hs_bins = cut(wave_data_complete$Hm0,breaks = hs_bins))
    tmp <- cut(df1[[variables[1]]],breaks = unlist(bins[1]))
    df_list <- c(paste(variables,"_bins",sep=""))
    assign(paste(df_list[1]),as.data.frame(tmp))

    df1_binned <- cbind(df1,get(df_list[1]))
    colnames(df1_binned)[length(df1_binned)] <- df_list[1]

    for (i in 2:length(variables)){
        tmp <- cut(df1[[variables[i]]],breaks = unlist(bins[i]))
        assign(paste(df_list[i]),as.data.frame(tmp))

        df1_binned <- cbind(df1_binned,get(df_list[i]))
        colnames(df1_binned)[length(df1_binned)] <- df_list[i]
    }
    # Aggregate functions
    mean_power <- function(x) {length(x)}

    # Convert to matrix
    pivot_table <- dcast(df1_binned, as.formula(paste(df_list[1],"~",df_list[2])),
                         fun.aggregate = function(x) mean_power(x),value.var = target,margins=FALSE,fill = 0,drop = FALSE)

    rownames(pivot_table) <- pivot_table[,1]
    pivot_table <- pivot_table[c(-1)]
    #if (grepl("NA",names(pivot_table),ignore.case = TRUE)){
    #    pivot_table <- pivot_table[, -which(names(pivot_table) %in% c("NA"))]
    #}
    rownames(pivot_table) <- as.character(unlist(bins[1])[2:length(unlist(bins[1]))])
    colnames(pivot_table) <- as.character(unlist(bins[2])[2:length(unlist(bins[2]))])
    return(pivot_table)
}


#' Function to calculate Tz from wave parameter data
#'
#'
#' @param input_df
#' @param hsindex - index of Hs
#' @param tpindex - index of Tp
#' @export
#' @examples
#' addTz()
addTz <- function(df1,hsindex,tpindex){
    Tp <- df1[,tpindex]
    Hs <- df1[,hsindex]

    wave_gamma <- 3.3

    df1 <- cbind(df1,wave_gamma)
    ratioHsTp <- Tp/sqrt(Hs)
    df1 <- cbind(df1,ratioHsTp)

    # Ratio Hs Tp < 3.6
    # DNV-RP-H103
    df1$wave_gamma[which(df1$ratioHsTp <= 3.6)] <- 5

    df1$wave_gamma[which(df1$ratioHsTp > 3.6 & df1$ratioHsTp < 5)] <-
        exp(5.75 - 1.15*df1$ratioHsTp[which(df1$ratioHsTp > 3.6 & df1$ratioHsTp < 5)])

    df1$wave_gamma[which(df1$ratioHsTp >= 5)] <- 1

    # Overide values calculated

    wave_gamma <- 3.3
    Tz <- Tp*(0.6673 + 0.05037*wave_gamma - 0.006230*wave_gamma^2 + 0.0003341*wave_gamma^3)
    Te <- 1.18 * Tz
    PowWave <- 0.49*((df1[,hsindex])^2)*Te
    return(cbind(df1,Tz,Te,PowWave))
}

#' Function from openair to plot a wave rose from a time series
#'
#' @param input_df_time_series
#' @export
#' @examples
#' waveRose()
waveRose <- function (mydata, ws = "ws", wd = "wd", ws2 = NA, wd2 = NA, ws.int = 2,
                      angle = 30, type = "default", bias.corr = TRUE, cols = "default",
                      grid.line = NULL, width = 1, seg = NULL, auto.text = TRUE,
                      breaks = 4, offset = 10, max.freq = NULL, paddle = TRUE,
                      key.header = NULL, key.footer = "(m/s)", key.position = "bottom",
                      key = TRUE, dig.lab = 5, statistic = "prop.count", pollutant = NULL,
                      annotate = TRUE, border = NA, ...)
{
    if (is.null(seg))
        seg <- 0.9
    if (length(cols) == 1 && cols == "greyscale") {
        trellis.par.set(list(strip.background = list(col = "white")))
        calm.col <- "black"
    }
    else {
        calm.col <- "forestgreen"
    }
    current.strip <- trellis.par.get("strip.background")
    on.exit(trellis.par.set("strip.background", current.strip))
    if (360/angle != round(360/angle)) {
        warning("In windRose(...):\n  angle will produce some spoke overlap",
                "\n  suggest one of: 5, 6, 8, 9, 10, 12, 15, 30, 45, etc.",
                call. = FALSE)
    }
    if (angle < 3) {
        warning("In windRose(...):\n  angle too small", "\n  enforcing 'angle = 3'",
                call. = FALSE)
        angle <- 3
    }
    extra <- list(...)
    extra$xlab <- if ("xlab" %in% names(extra))
        quickText(extra$xlab, auto.text)
    else quickText("", auto.text)
    extra$ylab <- if ("ylab" %in% names(extra))
        quickText(extra$ylab, auto.text)
    else quickText("", auto.text)
    extra$main <- if ("main" %in% names(extra))
        quickText(extra$main, auto.text)
    else quickText("", auto.text)
    rounded <- FALSE
    if (all(mydata[, wd]%%10 == 0, na.rm = TRUE))
        rounded <- TRUE
    if (is.character(statistic)) {
        ok.stat <- c("prop.count", "prop.mean", "abs.count",
                     "frequency")
        if (!is.character(statistic) || !statistic[1] %in% ok.stat) {
            warning("In windRose(...):\n  statistic unrecognised",
                    "\n  enforcing statistic = 'prop.count'", call. = FALSE)
            statistic <- "prop.count"
        }
        if (statistic == "prop.count") {
            stat.fun <- length
            stat.unit <- "%"
            stat.scale <- "all"
            stat.lab <- "Frequency of counts by wave direction (%)"
            stat.fun2 <- function(x) signif(mean(x, na.rm = TRUE),
                                            3)
            stat.lab2 <- "mean"
            stat.labcalm <- function(x) round(x, 1)
        }
        if (statistic == "prop.mean") {
            stat.fun <- function(x) sum(x, na.rm = TRUE)
            stat.unit <- "%"
            stat.scale <- "panel"
            stat.lab <- "Proportion contribution to the mean (%)"
            stat.fun2 <- function(x) signif(mean(x, na.rm = TRUE),
                                            3)
            stat.lab2 <- "mean"
            stat.labcalm <- function(x) round(x, 1)
        }
        if (statistic == "abs.count" | statistic == "frequency") {
            stat.fun <- length
            stat.unit <- ""
            stat.scale <- "none"
            stat.lab <- "Count by wind direction"
            stat.fun2 <- function(x) round(length(x), 0)
            stat.lab2 <- "count"
            stat.labcalm <- function(x) round(x, 0)
        }
    }
    if (is.list(statistic)) {
        stat.fun <- statistic$fun
        stat.unit <- statistic$unit
        stat.scale <- statistic$scale
        stat.lab <- statistic$lab
        stat.fun2 <- statistic$fun2
        stat.lab2 <- statistic$lab2
        stat.labcalm <- statistic$labcalm
    }
    vars <- c(wd, ws)
    diff <- FALSE
    rm.neg <- TRUE
    if (!is.na(ws2) & !is.na(wd2)) {
        vars <- c(vars, ws2, wd2)
        diff <- TRUE
        rm.neg <- FALSE
        mydata$ws <- mydata[, ws2] - mydata[, ws]
        mydata$wd <- mydata[, wd2] - mydata[, wd]
        id <- which(mydata$wd < 0)
        if (length(id) > 0)
            mydata$wd[id] <- mydata$wd[id] + 360
        pollutant <- "ws"
        key.footer <- "ws"
        wd <- "wd"
        ws <- "ws"
        vars <- c("ws", "wd")
        if (missing(angle))
            angle <- 10
        if (missing(offset))
            offset <- 20
        if (is.na(breaks[1])) {
            max.br <- max(ceiling(abs(c(min(mydata$ws, na.rm = TRUE),
                                        max(mydata$ws, na.rm = TRUE)))))
            breaks <- c(-1 * max.br, 0, max.br)
        }
        if (missing(cols))
            cols <- c("lightskyblue", "tomato")
        seg <- 1
    }
    if (any(type %in% openair:::dateTypes))
        vars <- c(vars, "date")
    if (!is.null(pollutant))
        vars <- c(vars, pollutant)
    mydata <- openair:::checkPrep(mydata, vars, type, remove.calm = FALSE,
                                  remove.neg = rm.neg)
    mydata <- na.omit(mydata)
    if (is.null(pollutant))
        pollutant <- ws
    mydata$x <- mydata[, pollutant]
    mydata[, wd] <- angle * ceiling(mydata[, wd]/angle - 0.5)
    mydata[, wd][mydata[, wd] == 0] <- 360
    mydata[, wd][mydata[, ws] == 0] <- -999
    if (length(breaks) == 1)
        breaks <- 0:(breaks - 1) * ws.int
    if (max(breaks) < max(mydata$x, na.rm = TRUE))
        breaks <- c(breaks, max(mydata$x, na.rm = TRUE))
    if (min(breaks) > min(mydata$x, na.rm = TRUE))
        warning("Some values are below minimum break.")
    breaks <- unique(breaks)
    mydata$x <- cut(mydata$x, breaks = breaks, include.lowest = FALSE,
                    dig.lab = dig.lab)
    labs <- gsub("[(]|[)]|[[]|[]]", "", levels(mydata$x))
    labs <- gsub("[,]", " to ", labs)
    prepare.grid <- function(mydata) {
        if (all(is.na(mydata$x)))
            return()
        levels(mydata$x) <- c(paste("x", 1:length(labs), sep = ""))
        all <- stat.fun(mydata[, wd])
        calm <- mydata[mydata[, wd] == -999, ][, pollutant]
        mydata <- mydata[mydata[, wd] != -999, ]
        calm <- stat.fun(calm)
        weights <- tapply(mydata[, pollutant], list(mydata[,
                                                           wd], mydata$x), stat.fun)
        freqs <- tapply(mydata[, pollutant], mydata[, wd], length)
        if (stat.scale == "all") {
            calm <- calm/all
            weights <- weights/all
        }
        if (stat.scale == "panel") {
            temp <- stat.fun(stat.fun(weights)) + calm
            calm <- calm/temp
            weights <- weights/temp
        }
        weights[is.na(weights)] <- 0
        weights <- t(apply(weights, 1, cumsum))
        if (stat.scale == "all" | stat.scale == "panel") {
            weights <- weights * 100
            calm <- calm * 100
        }
        panel.fun <- stat.fun2(mydata[, pollutant])
        u <- mean(sin(2 * pi * mydata[, wd]/360))
        v <- mean(cos(2 * pi * mydata[, wd]/360))
        mean.wd <- atan2(u, v) * 360/2/pi
        if (all(is.na(mean.wd))) {
            mean.wd <- NA
        }
        else {
            if (mean.wd < 0)
                mean.wd <- mean.wd + 360
            if (mean.wd > 180)
                mean.wd <- mean.wd - 360
        }
        weights <- cbind(data.frame(weights), wd = as.numeric(row.names(weights)),
                         calm = calm, panel.fun = panel.fun, mean.wd = mean.wd,
                         freqs = freqs)
        weights
    }
    if (paddle) {
        poly <- function(wd, len1, len2, width, colour, x.off = 0,
                         y.off = 0) {
            theta <- wd * pi/180
            len1 <- len1 + off.set
            len2 <- len2 + off.set
            x1 <- len1 * sin(theta) - width * cos(theta) + x.off
            x2 <- len1 * sin(theta) + width * cos(theta) + x.off
            x3 <- len2 * sin(theta) - width * cos(theta) + x.off
            x4 <- len2 * sin(theta) + width * cos(theta) + x.off
            y1 <- len1 * cos(theta) + width * sin(theta) + y.off
            y2 <- len1 * cos(theta) - width * sin(theta) + y.off
            y3 <- len2 * cos(theta) + width * sin(theta) + y.off
            y4 <- len2 * cos(theta) - width * sin(theta) + y.off
            lpolygon(c(x1, x2, x4, x3), c(y1, y2, y4, y3), col = colour,
                     border = border)
        }
    }
    else {
        poly <- function(wd, len1, len2, width, colour, x.off = 0,
                         y.off = 0) {
            len1 <- len1 + off.set
            len2 <- len2 + off.set
            theta <- seq((wd - seg * angle/2), (wd + seg * angle/2),
                         length.out = (angle - 2) * 10)
            theta <- ifelse(theta < 1, 360 - theta, theta)
            theta <- theta * pi/180
            x1 <- len1 * sin(theta) + x.off
            x2 <- rev(len2 * sin(theta) + x.off)
            y1 <- len1 * cos(theta) + x.off
            y2 <- rev(len2 * cos(theta) + x.off)
            lpolygon(c(x1, x2), c(y1, y2), col = colour, border = border)
        }
    }
    mydata <- cutData(mydata, type, ...)
    results.grid <- ddply(mydata, type, prepare.grid)
    results.grid$calm <- stat.labcalm(results.grid$calm)
    results.grid$mean.wd <- stat.labcalm(results.grid$mean.wd)
    if (bias.corr & rounded) {
        wd <- seq(10, 360, 10)
        tmp <- angle * ceiling(wd/angle - 0.5)
        id <- which(tmp == 0)
        if (length(id > 0))
            tmp[id] <- 360
        tmp <- table(tmp)
        vars <- grep("x", names(results.grid))
        results.grid[, vars] <- results.grid[, vars] * mean(tmp)/tmp
    }
    strip.dat <- openair:::strip.fun(results.grid, type, auto.text)
    strip <- strip.dat[[1]]
    strip.left <- strip.dat[[2]]
    pol.name <- strip.dat[[3]]
    if (length(labs) < length(cols)) {
        col <- cols[1:length(labs)]
    }
    else {
        col <- openColours(cols, length(labs))
    }
    if (is.null(max.freq)) {
        max.freq <- max(results.grid[, (length(type) + 1):(length(labs) +
                                                               length(type))], na.rm = TRUE)
    }
    else {
        max.freq <- max.freq
    }
    off.set <- max.freq * (offset/100)
    box.widths <- seq(0.002^0.25, 0.016^0.25, length.out = length(labs))^4
    box.widths <- box.widths * max.freq * angle/5
    legend <- list(col = col, space = key.position, auto.text = auto.text,
                   labels = labs, footer = key.footer, header = key.header,
                   height = 0.6, width = 1.5, fit = "scale", plot.style = if (paddle) "paddle" else "other")
    legend <- openair:::makeOpenKeyLegend(key, legend, "windRose")
    temp <- paste(type, collapse = "+")
    myform <- formula(paste("x1 ~ wd | ", temp, sep = ""))
    mymax <- 2 * max.freq
    myby <- if (is.null(grid.line))
        pretty(c(0, mymax), 10)[2]
    else grid.line
    if (myby/mymax > 0.9)
        myby <- mymax * 0.9
    xyplot.args <- list(x = myform, xlim = 1.03 * c(-max.freq -
                                                        off.set, max.freq + off.set), ylim = 1.03 * c(-max.freq -
                                                                                                          off.set, max.freq + off.set), data = results.grid, type = "n",
                        sub = stat.lab, strip = strip, strip.left = strip.left,
                        as.table = TRUE, aspect = 1, par.strip.text = list(cex = 0.8),
                        scales = list(draw = FALSE), panel = function(x, y, subscripts,
                                                                      ...) {
                            panel.xyplot(x, y, ...)
                            angles <- seq(0, 2 * pi, length = 360)
                            sapply(seq(off.set, mymax, by = myby), function(x) llines(x *
                                                                                          sin(angles), x * cos(angles), col = "grey85",
                                                                                      lwd = 1))
                            dat <- results.grid[subscripts, ]
                            upper <- max.freq + off.set
                            larrows(-upper, 0, upper, 0, code = 3, length = 0.1)
                            larrows(0, -upper, 0, upper, code = 3, length = 0.1)
                            ltext(upper * -1 * 0.95, 0.07 * upper, "W", cex = 0.7)
                            ltext(0.07 * upper, upper * -1 * 0.95, "S", cex = 0.7)
                            ltext(0.07 * upper, upper * 0.95, "N", cex = 0.7)
                            ltext(upper * 0.95, 0.07 * upper, "E", cex = 0.7)
                            if (nrow(dat) > 0) {
                                dat$x0 <- 0
                                for (i in 1:nrow(dat)) {
                                    for (j in seq_along(labs)) {
                                        tmp <- paste("poly(dat$wd[i], dat$x", j -
                                                         1, "[i], dat$x", j, "[i], width * box.widths[",
                                                     j, "], col[", j, "])", sep = "")
                                        eval(parse(text = tmp))
                                    }
                                }
                            }
                            ltext(seq((myby + off.set), mymax, myby) * sin(pi/4),
                                  seq((myby + off.set), mymax, myby) * cos(pi/4),
                                  paste(seq(myby, mymax, by = myby), stat.unit,
                                        sep = ""), cex = 0.7)
                            if (annotate) if (statistic != "prop.mean") {
                                if (!diff) {
                                    ltext(max.freq + off.set, -max.freq - off.set,
                                          label = paste(stat.lab2, " = ", dat$panel.fun[1],
                                                        "\ncalm = ", dat$calm[1], stat.unit, sep = ""),
                                          adj = c(1, 0), cex = 0.7, col = calm.col)
                                }
                                if (diff) {
                                    ltext(max.freq + off.set, -max.freq - off.set,
                                          label = paste("mean ws = ", round(dat$panel.fun[1],
                                                                            1), "\nmean wd = ", round(dat$mean.wd[1],
                                                                                                      1), sep = ""), adj = c(1, 0), cex = 0.7,
                                          col = calm.col)
                                }
                            } else {
                                ltext(max.freq + off.set, -max.freq - off.set,
                                      label = paste(stat.lab2, " = ", dat$panel.fun[1],
                                                    stat.unit, sep = ""), adj = c(1, 0), cex = 0.7,
                                      col = calm.col)
                            }
                        }, legend = legend)
    xyplot.args <- openair:::listUpdate(xyplot.args, extra)
    plt <- do.call(xyplot, xyplot.args)
    if (length(type) == 1) {
        plot(plt)
    }
    else {
        plt <- useOuterStrips(plt, strip = strip, strip.left = strip.left)
        plot(plt)
    }
    newdata <- results.grid
    output <- list(plot = plt, data = newdata, call = match.call())
    class(output) <- "openair"
    invisible(output)
}


#' Function from openair to plot a wave rose from a time series
#'
#' @param input_df_time_series
#' @export
#' @examples
#' plotSeasonalRose()
plotSeasonalRose <- function(nww3Param){
    require(openair)
    #oldNames <- names(nww3Param)
    #colnames(nww3Param) <- names
    waveRose(mydata = nww3Param,
             ws = "Hs",
             wd = "Dp",
             angle = 10,
             type = "season",
             hemisphere="southern",
             auto.text = FALSE,
             breaks = c(seq(0.2,2.2,0.2)),
             key.header = expression(paste('H'[m0],' [m]')),
             annotate = TRUE,
             key.position = "right",
             key.footer = "",

             paddle = FALSE,
             par.settings=list(fontsize=list(text=12),mar=c(0,0,0,0)),
             main="Seasonal rose plot of the significant wave height at the PLEM"
    )
}
