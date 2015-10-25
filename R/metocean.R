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
#' @param bins vector of bins for each of the named variables
#' @return pivot_table
#' @export
#' @examples
#' generatePivot(df1,variables,bins)
generatePivot <- function(df1,variables,bins,...){
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
  t3<- scatterPlot(input_df,x=Tp,y=Dp,method = "hexbin",xbins=100,cols = "jet",aspect=1,hemisphere="southern")#,type="season")

  t1$plot$xlab <- labels[1]
  t1$plot$ylab <- labels[2]

  t2$plot$xlab <- labels[1]
  t2$plot$ylab <- labels[3]

  t3$plot$xlab <- labels[2]
  t3$plot$ylab <- labels[3]

  scatterPlots <- list(c(t1,t2,t3))
  return(scatterPlots)
}
