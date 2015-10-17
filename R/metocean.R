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
#' @keywords 
#' @export 
#' @examples 
#' getExceedance() 
getExceedance <- function(df,variable,
 	percentiles = c(c(0,0.01,0.02,0.05),seq(0.1,0.9,0.1),c(0.95,0.98,0.99,1)),
 	xlabel, type = "season", hemisphere = "northern"){
	require(lubridate)
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
	# 
    return()
}
