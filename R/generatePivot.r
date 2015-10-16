## Set of functions to process wave data ##
library(reshape2)
library(plyr)
generatePivot <- function(df1,variables,bins,...){
    # # Description ==============================================================
    # Author:  		Rhydar Lee Harris
    # Date:    		2014-08-24 T21:25:13Z
    # Type:    		METOCEAN ANALYSIS
    # Description:	Generates a pivot table of wave conditions from a given time
    #
    # ============================================================================
    # Args:
    # 	dataFile,variable,...
    # Returns:
    #    	table.tex where table.tex is a latex table for inclusion into a report
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

    # Convert to matrix
    pivot_table <- dcast(df1_binned, as.formula(paste(df_list[1],"~",df_list[2])),margins=TRUE,fill = 0,drop = FALSE,)

    rownames(pivot_table) <- pivot_table[,1]
    pivot_table <- pivot_table[c(-1)]
    if (grepl("NA",names(pivot_table),ignore.case = TRUE)){
        pivot_table <- pivot_table[, -which(names(pivot_table) %in% c("NA"))]
    }
    pivot_table <- 100*pivot_table/length(df1[,1])

    return(pivot_table)
}

getExcelWaveData <- function(excelFile){
  # # Description ==============================================================
  # Author:  		Rhydar Lee Harris
  # Date:    		2014-08-24 T21:38:50Z
  # Type:    		TMP:DELLETE
  # Description:	tmp file to extract data from an excel file containing wave
  # 				data
  # ============================================================================
  # Args:
  # 	excelFile
  # Returns:
  #    	dfwave
  return(dfwave)
}

getDataFromExcel <- function(mydirectory,myexcelfile,mysheet,mycolnames,myrow){
        # Get all data from excel given
        wb <- loadWorkbook(myexcelfile)
        mydataframe <- readWorksheet(wb,mysheet,startRow = myrow)
        colnames(mydataframe) <- mycolnames
        return(mydataframe)
}

getWaveDataGrib <- function(waveDataGrib){
   # # Description ==============================================================
   # Author:      Rhydar Lee Harris
   # Date:        2014-09-21 T22:12:36Z
   # Type:        METOCEAN:WAVES
   # Description:  Get wave data from a grib file
   # ============================================================================
   # Args:
   #   waveDataGrib
   # Returns:
   #      waveDataTS
   library(rNOMADS)
   library(raster)
   return(waveDataTS)
 }

getWaveData <- function(hs_text,tp_text,dp_text,...){
  # # Description ==============================================================
  # Author:      Rhydar Lee Harris
  # Date:        2015-02-23 T19:13:59Z
  # Type:        Wave processing
  # Description:  Concatentate individual NCEP time series into a workable data set
  # ============================================================================
  # Args:
  #   hs_text,tp_text,dp_text,...
  # Returns:
  #      WaveConditions
  library(zoo)
  hs_data <- read.table(hs_text,head=FALSE,sep="\t")
  hs_data <- hs_data[!duplicated(hs_data$V1),]
  hs_zoo <- read.zoo(hs_data,tz = "",format="%Y-%m-%d %H:%M:%S",index=1)
  tp_data <- read.table(tp_text,head=FALSE,sep="\t")
  tp_data <- tp_data[!duplicated(tp_data$V1),]
  tp_zoo <- read.zoo(tp_data,tz = "",format="%Y-%m-%d %H:%M:%S",index=1)
  dp_data <- read.table(dp_text,head=FALSE,sep="\t")
  dp_data <- dp_data[!duplicated(dp_data$V1),]
  dp_zoo <- read.zoo(dp_data,tz = "",format="%Y-%m-%d %H:%M:%S",index=1)
  wave_data <- merge(hs_zoo,tp_zoo,dp_zoo,all=TRUE)
  return(wave_data)
}

plotExceedance <- function(input_zoo,percentiles = c(0,0.01,0.05,0.10,0.20,0.50,0.80,0.90,0.95,0.99,1),...){
  # # Description ==============================================================
  # Author:      Rhydar Lee Harris
  # Date:        2015-03-06 T20:21:50Z
  # Type:        Helper function to plot exceedance plots
  # Description:  Plots an exceedance curve and gives a table
  # ============================================================================
  # Args:
  #   tmpDf
  # Returns:
  #      df_ecdf
  # Modify par plot:
  library(tableplot)
  library(PerformanceAnalytics)
  def.par <- par(no.readonly = TRUE)
  par(cex=2,oma=c(1,1,1,1),mar=c(3,3,3,3))
  #par(oma=c(0,0,0,0))
  layout(matrix(c(1,2),2,1,byrow=TRUE),
         widths=c(1,0.5),heights=c(2,1))

  #percentiles <- c(0.06,0.10,0.25,0.50,0.75,0.90,0.95)
  #legendPercentiles <- c("6%","10%","25%","50%","75%","90%","95%")
  # Calculate and plot ecdf
  ecdf_zoo <- ecdf(as.numeric(input_zoo))
  plot(ecdf_zoo,...)
  grid()
  quantiles_table <- quantile(as.numeric(input_zoo),percentiles)
  textplot(quantiles_table,halign="right",valign="center",cex=1)
  par(def.par)
  return(ecdf_zoo)
}

getBinnedData <- function(input_df,binning_vect,binning_var){
  # # Description ==============================================================
  # Author:      Rhydar Lee Harris
  # Date:        2015-03-28 T20:07:10Z
  # Type:        Generic function for binning data
  # Description:  Function for binning data
  # ============================================================================
  # Args:
  #   input_df, binning_vector
  # Returns:
  #      binned_df
  # Refs:
  # http://r.789695.n4.nabble.com/Binning-or-grouping-data-td890071.html
  library(reshape2)
  input_df <- cbind(input_df,
                    vect_cat = cut(input_df[[binning_var]],
                                   breaks = binning_vect))
  return_df <- with(input_df,tapply(Tp,list(vect_cat,c(0,5,10,15,20)),length))
  return(return_df)
}


#' Function to generate scatter plots for wave data
#'
#' Plots scatter diagrams for wave parameters of specified
#' @param Dataframe of time series of wave data
#' @return Returns and plot object
#' @keywords scatter, waves, processing
#' @export
#' @examples
#' plotScatter(input_df)
plotScatter <- function(input_df,my_col_brew,col_index = c(2,3,4),col_hm0,col_tp,col_dir,...){
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
  Hs <- "Hm0"
  Tp <- "Tp_interpolated"
  Dp <- "Dir_mean_at_Tp"

  a <- scatterPlot(input_df,x=Hs,y=Tp,
                               method = "hexbin",xbins=100,
                               hemisphere="southern",
                   cols = "jet",
                   aspect=1,
                   key=FALSE,
                   key.title = "What",hexbin=FALSE,silent=TRUE,hexbinplot.colorkey = 0,
                   ref.x = list(v = c(col_hmo),col="dark gray",lwd=3),
                   ref.y = list(h = c(col_tp),col="dark gray",lwd=3))
  b <- scatterPlot(input_df,x=Hs,y=Dp,method = "hexbin",xbins=100,hemisphere="southern",cols = "jet",aspect=1,
                   ref.x = list(v = c(col_hmo),col="dark gray",lwd=3),
                   ref.y = list(h = c(col_dir),col="dark gray",lwd=3))
  c <-scatterPlot(input_df,x=Dp,y=Tp,method = "hexbin",xbins=100,hemisphere="southern",cols = "jet",aspect=1,
                  ref.x = list(v = c(col_dir),col="dark gray",lwd=3),
                  ref.y = list(h = c(col_tp),col="dark gray",lwd=3))

  plots <- list("a" = a,"b" = b,"c" = c)

  return(plots)
}

fix_scatter <- function(oa_obj,xlab,ylab){
    oa_obj$plot$xlab <- xlab
    oa_obj$plot$ylab <- ylab
    #oa_obj$
    return(oa_obj)
}


#' A Metocean function
#'
#' This function calculates wave power based on Hs and Tp
#' @param Hs - Significant wave height [m], Tp - Peak wave period
#' @keywords Metocean
#' @export
#' @examples
#' calcWavePower()
calcWavePower <- function(Hs,Tp){
  # # Description ==============================================================
  # Author:      Rhydar Lee Harris
  # Date:        2015-04-04 T20:32:40Z
  # Type:        Wave power calculations
  # Description:  Jonas Stolte
  # ============================================================================
  # Args:
  #   Hs,Tp
  # Returns:
  #      P = Wave power kW/m P = rho * g^2 * H^2 * Tp /(64 * pi) ~= 0.5 * Hs * Tp
  P <- 0.5 * Hs * Hs * Tp
  return(P)
}


