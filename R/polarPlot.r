library (circular)
library(colorspace)
library(RColorBrewer)
library(openair)
library(lubridate)
library(zoo)

require(circular,colorspace)
plotPolar <- function(my_data,my_runs,my_colors,my_limits,my_marker=1,my_val,my_legend,my_type,...){
	par(mar=c(0,6,0,1),oma=c(0,0,0,0),cex.axis=0.95)
	#legend_lines <- character()
	plotNullPlot <- function(){
		radial.plot(0,radial.lim=my_limits,show.grid.labels=FALSE,show.grid=FALSE,
			mar=par()$mar,oma=par()$oma)
	}
	print (par()$oma)
	plotRadialGrid <- function(){
        degrees <- c(0,45,90,135,180,225,270,315)
        radians <- degrees * 2 * pi/360
        my_labels <- c("0","45","90","135","180","225","270","315")
        #labels <- labels~degree
        radial.grid(radial.lim=my_limits,grid.pos=seq(0,my_limits[2],length.out=11),grid.col="grey",start = pi,label.pos = radians,labels=my_labels)
	}
	plotLinePolar <- function(xx,yy,...){
		radial.plot(lengths = xx,radial.pos = rad(yy),rp.type = "p", start = pi, show.grid = FALSE, add = TRUE,radial.lim=my_limits,...)
	}
	plotMarker <- function(my_marker,my_val){
        radial.plot(
        lengths = rep(my_val,36),#seq(0,2*pi,by = 2*pi/36),
        radial.lim = c(my_limits),
        rp.type = "p",
        show.grid.labels = FALSE,
        start = pi,
        lwd=3,
        lty=2,
        labels="",
        show.grid=FALSE,
        add=TRUE,line.col="#3C3C3C")
        legend(x = -0.1 * my_limits[2],y = 1.05*my_val,legend=as.character(my_marker*my_val),text.col = "#3C3C3C",cex=1.1,box.lwd = 0,box.col = "white",bg =rgb(1,230,255,alpha = 0.9,maxColorValue = 255))
	}
	plotNullPlot()
	plotRadialGrid()
	x <- as.data.frame(seq(1,length(my_data[1,]) - 1,1))
	apply(x,1,function(x){xx <- my_data[,x + 1];yy <- my_runs$WIND_DIR;plotLinePolar(xx,yy,line.col = my_colors[x],lwd=2)})#;legend_lines <<- append(legend_lines,values = as.character(x))})
	par(new=T)
	plotMarker(my_marker = my_marker, my_val = my_val)
    plotMarker(my_marker = my_marker, my_val = my_limits[2])
    par(xpd = TRUE)
    legend(-1.5*my_limits[2],0.5*my_limits[2],legend=my_legend,col = my_colors,lty=1,lwd=2,title = my_type,cex=0.95)#legend=legend_lines,col = my_colors,lty=1,lwd=2,title = my_type,cex=0.95)
    par(xpd=FALSE)
    par(oldpar)
}

getData <- function(sheet="Tension",my_type = "Lines",my_columns=NULL,vessel_type="FSRU"){
  #my_data <- alldata$FSRU_Tension
	my_data <- alldata[[paste(vessel_type,sheet,sep="_")]]
	if (is.null(my_columns)){
		my_data <- my_data
	}
	else {
		my_data <- my_data[my_columns]
	}
	legend_entry <- colnames(my_data)
	if (sheet == "Tension"){
		pattern <- "X"
	}
	if (sheet == "Motions"){
		pattern <- "X\\d."
	}
	if (sheet == "FCompression"){
		pattern <- "X"
	}
	legend_entry <- gsub(pattern = pattern,replacement = "",x = legend_entry)
	my_data <<- my_data
	return(legend_entry)
}

plotMotions <- function(motions="Translation",my_limits=c(0,2)){
    if (motions == "Translation"){
        my_columns <- c(1,2,3)
        my_cols <- c("red","orange")
        my_val <- 1.0
        my_marker = 1
    }
    if (motions == "Rotation"){
        my_columns <- c(1,7)
        my_cols <- c("dark blue")
        my_val <- 0.25
        my_marker = 1
    }
    my_type <- "Motions"
    legend_entry <- getData(sheet = "Motions",my_type = my_type,my_columns=my_columns)

    my_runs <- alldata$RUNS_FILE_OCIMF#[[runs_file]]
    tmp_data <- as.data.frame(apply(x <- as.data.frame(seq(2,length(my_data[1,]),1)),1,function(x){abs(my_data[,x] - mean(as.numeric(my_data[,x])))}))
    my_data <- cbind(my_data[,1],tmp_data)
    plotPolar(my_data,my_runs,my_cols,my_limits,my_type=my_type,1,my_val,my_marker,my_legend=legend_entry[2:length(legend_entry)])
    colnames(my_data) <- legend_entry
    return(my_data)
}

plotFenders <- function(my_limits=c(0,3000000),my_columns){
	#my_columns <- NULL
	my_cols <- c("red","orange","blue","purple")
  my_type = "Fenders"
	legend_entry <- getData(sheet = "FCompression",my_type = my_type,my_columns=my_columns)
    my_runs <- alldata$RUNS_FILE_OCIMF#[[runs_file]]
    plotPolar(my_data,my_runs,my_cols,my_limits,my_marker = 0.001,my_val = 1500000,my_type=my_type,my_legend=legend_entry[1:length(legend_entry)-1])
    colnames(my_data) <- legend_entry
    return(my_data)
}

plotLines <- function(my_limits=c(0,500000),line_colors){
	my_columns <- NULL
	my_cols <- line_colors
    my_type <- "Lines"
	legend_entry <- getData(sheet = "Tension",my_type = my_type,my_columns=my_columns)
    my_runs <- alldata$RUNS_FILE_OCIMF#[[runs_file]]
    plotPolar(my_data,my_runs,my_cols,my_limits,my_marker = 0.001,my_val = 250000,my_type=my_type,my_legend=legend_entry[2:length(legend_entry)])
    colnames(my_data) <- legend_entry
    return(my_data)
}

plotPreTension <- function(my_data,my_cols){
  line_data <- my_data$Decklines
  line_data <- line_data[order(line_data$Line),]
  mbl <- unique(line_data$LineMBL)
  mbl_nice <- round_any(mbl,accuracy=100,f=ceiling)

  par(mar=c(2,8,0,0),oma=c(0,2,2,2))
  barplot(line_data$Tension_EQ,
    col = my_cols,
    xlab = "Mooring lines",
    ylab="Maximum mooring line tension [kN]",
    cex.axis = 0.85,cex.names = 0.9,axes = TRUE,cex.lab=0.85,
    ylim = c(0,mbl_nice),
    xlim = c(0,19),
    names=line_data$Line,
    las=3)

  my_col <- rgb(0,0,0,alpha = 0.8,maxColorValue = 256)

  axis(2,labels = paste(sprintf("%.0f",seq(0,mbl,by=0.1*mbl)*100/mbl),"%"),
    col.axis="gray28",las=1,cex=0.85,cex.axis=0.85,col="gray28",at=seq(0,mbl,by=0.1*mbl),pos=-5)#,labels=0:500,col=3)

  mtext("Percentage MBL",col="gray28",side=2,line=8.5,cex=0.85)

  abline(h = mbl*0.5,col="gray28",lty=4)

  legend(x = 0, y = 0.5*mbl,legend="SWL = 50% MBL",cex = 0.8,bty = "n",text.col="gray28")

  grid(col="black")
}

lineTableSummary <- function(my_data){
  line_data <- my_data$Decklines
  line_data <- line_data[order(line_data$Line),]
  df <- line_data[,c("Line","TotalLngth","Tension_EQ")]
  colnames(df) <- c("Line","Length [m]","Pretension [kN]")
  return (df)
}

tableSummary <- function(){
    # Function
    wind_angle <- alldata[[runs_file]]$WIND_DIR

    # Get all line tension data
    tension <- alldata[[paste(Vessel_type,"Tension",sep="_")]]
    # Get maximum value for all line tensions
    max_line_tensions <- apply(tension,2,max)
    max_line <- max(max_line_tensions)

    mbl <- unique(alldata[[paste("Decklines",Vessel_type,sep="")]]$LineMBL)
    mbl_nice <- round_any(mbl,accuracy=100,f=ceiling)

    par(mar=c(2,8,0,0),oma=c(0,2,2,2))
    barplot(max_line_tensions[c(2:length(max_line_tensions))]*0.001,
        col = line_colors,
        xlab = "Mooring lines",
        ylab="Maximum mooring line tension [kN]",
        cex.axis = 0.85,cex.names = 0.70,axes = TRUE,cex.lab=0.85,
        ylim = c(0,mbl_nice),
        names=gsub("X",replacement = "",
                   x = rownames(as.data.frame(max_line_tensions))[c(2:length(max_line_tensions))]))

    my_col <- rgb(0,0,0,alpha = 0.8,maxColorValue = 256)

    axis(2,labels = paste(sprintf("%.0f",seq(0,mbl,by=0.1*mbl)*100/mbl),"%"),
         col.axis="gray28",las=1,cex=0.85,cex.axis=0.85,col="gray28",at=seq(0,mbl,by=0.1*mbl),pos=-5)#,labels=0:500,col=3)

    mtext("Percentage MBL",col="gray28",side=2,line=8.5,cex=0.85)

    abline(h = mbl*0.5,col="gray28",lty=4)

    legend(x = 0, y = 0.5*mbl,legend="SWL = 50% MBL",cex = 0.8,bty = "n",text.col="gray28")

    grid(col="black")
    # Get line index where maximum occurs - If more than one, select the first
    line_max <- which(max_line_tensions == max_line)[1]
    line_max <- which(tension[[line_max]] == max_line)

    max_line_idx <- line_max # as.matrix(which(tension == max_line)) #/ num_cols_tension
    max_line_wind <- wind_angle[max_line_idx]
    if(max_line_wind < 0){
        max_line_wind <- 360 + max_line_wind
    }


    # Get all line compression data
    compression <- alldata[[paste(Vessel_type,"FCompression",sep="_")]]
    # Get maximum values for all fenders
    max_fend_comp <- apply(compression,2,max)
    max_fend <- max(max_fend_comp)

    # Get line index where maximum occurs - If more than one, select the first
    fend_max <- which(max_fend_comp == max_fend)[1]
    fend_max <- which(compression[[fend_max]] == max_fend)

    max_fend_idx <- fend_max
    max_fend_wind <- wind_angle[max_fend_idx]
    if(max_fend_wind < 0){
        max_fend_wind <- 360 + max_fend_wind
    }

    #num_cols_compression <- length(compression)
    surge <- alldata[[paste(Vessel_type,"Motions",sep="_")]][,2]
    sway <- alldata[[paste(Vessel_type,"Motions",sep="_")]][,3]
    yaw <- alldata[[paste(Vessel_type,"Motions",sep="_")]][,7]

    abs_surge <-abs(surge - mean(surge)) # Calc max abs value of surge [m]
    max_surge <- max(abs_surge)
    max_surge_idx <- which(abs_surge == max_surge)[1]
    max_surge_wind <- wind_angle[max_surge_idx]
    if(max_surge_wind < 0){
        max_surge_wind <- 360 + max_surge_wind
    }

    abs_sway <-abs(sway - mean(sway)) # Calc max abs value of sway [m]
    max_sway <- max(abs_sway)
    max_sway_idx <- which(abs_sway == max_sway)[1]
    max_sway_wind <- wind_angle[max_sway_idx]
    if(max_sway_wind < 0){
        max_sway_wind <- 360 + max_sway_wind
    }

    abs_yaw <-abs(yaw - mean(yaw)) # Calc max abs value of yaw [deg]
    max_yaw <- max(abs_yaw)
    max_yaw_idx <- which(abs_yaw == max_yaw)[1]
    max_yaw_wind <- wind_angle[max_yaw_idx]
    if(max_yaw_wind < 0){
        max_yaw_wind <- 360 + max_yaw_wind
    }

    units <- c("kN","kN","m","m","deg")
    property <- c("Maximum line tension",
        "Maximum fender reaction",
        "Maximum surge motion","Maximum sway motion",
        "Maximum yaw angle")
    value <- sprintf(c(rep("%.0f",2),rep("%.2f",3)), c(0.001*max_line,0.001*max_fend,max_surge,max_sway,max_yaw))
    wind_angle <- c(max_line_wind,max_fend_wind,max_surge_wind,max_sway_wind,max_yaw_wind)

    summaryTable <- cbind(property,value,units,wind_angle)
    colnames(summaryTable) <- c("Property","Value","Units","Wind angle [deg]")

    return(summaryTable)
}



# =========================
plotAllLines2 <- function(bollard_list,color_list,lwd=lwd,max_lim=max_lim,runs_file){
        legend_lines <- character()
        legend_colors <- character()
        for (jj in 1:length(bollard_list)){
                testdf <- getLineSubset(bollard_name = bollard_list[jj])
                testdf <- testdf[order(testdf$Line),]
                my_shades <- getLineColors(testdf,color_list[jj],motions=FALSE)
                for (ii in 1:length(testdf[,1])){
                        line_num <- testdf$Line[ii]
                        legend_lines <- append(legend_lines,values = line_num)
                        legend_colors <- append(legend_colors,values = my_shades[ii])
                }
        }
        return(legend_colors)
}

# ===== Standard processing for wind time series
# Plots include seasonal wind rose
# Annual wind rose
# Exceedance plots for seasons and annual

getWindData <- function(wind_data_file){
        # Get and clean data
        wind_data <- read.delim(file = wind_data_file, header=FALSE, stringsAsFactors=FALSE,skip=3,na.strings = "-9999")
        colnames(wind_data) <- c("date","ws","wd")
        wind_data$date <- as.POSIXct(wind_data$date)
        return (wind_data)
}

plotWindRoseSeasonal <- function(wind_data){
        par(mar=c(0,0,0,0))
        cols <- rev(brewer.pal(n = 10,name = "Spectral"))
        breaks <- seq(2,20,2)
        my.statistic <- list("fun"=length,"unit" = "%","scale" = "all",
            "lab" = "" ,
            "fun2" = function(x) signif(mean(x, na.rm = TRUE), 3), "lab2" = "mean","labcalm" = function(x) round(x, 1))
        windRose(wind_data,type = "season",hemisphere="southern",angle = 10,
            col=cols,key.position = "right",border=FALSE,
            paddle = FALSE,breaks = breaks,key=TRUE,annotate=FALSE,xyplotargs=list("border"=FALSE),statistic=my.statistic)
        par(oldpar)
}

plotWindRoseAnnual <- function(wind_data,params){
    par(mar=c(0,0,0,0))
        cols <- rev(brewer.pal(n = 10,name = "Spectral"))
        breaks <- seq(2,20,2)
        my.statistic <- list("fun"=length,"unit" = "%","scale" = "all",
            "lab" = "" ,
            "fun2" = function(x) signif(mean(x, na.rm = TRUE), 3), "lab2" = "mean","labcalm" = function(x) round(x, 1))
        windRose(wind_data,angle = 10,col=cols,key.cex = 1.2,
            key.position = "right",paddle = FALSE,breaks = breaks,annotate=FALSE,
            statistic=my.statistic)
        par(oldpar)
}

getSeasonalData <- function(wind_data,season){
        spring <- c("Sep","Oct","Nov")
        summer <- c("Dec","Jan","Feb")
        autumn <- c("Mar","Apr","May")
        winter <- c("Jun","Jul","Aug")
        if (is.zoo(wind_data)){
          my_season <- wind_data[which(month(ymd_hms(time(wind_data)), label = TRUE) %in% get(season)),]
        }
        else {
          my_season <- wind_data[which(month(ymd_hms(wind_data[[1]]),label = TRUE) %in% get(season)),]
        }
        return (my_season)
}

getMonthlyData <- function(wind_data,month){
        data_results <- wind_data[which(month(ymd_hms(wind_data[[1]]),label = TRUE) %in% month),]
        return(data_results)
}

plotSeasonalECDF <- function(wind_data,variable,xlable){
        par(mar=c(5.1, 4.1, 2.1, 2.1),xpd = FALSE)
        cols <- c("red","blue","orange","green")
        seasons <- c("spring","summer","autumn","winter")
        percentiles <- 100 * c(seq(0,0.9,0.1),0.95,0.99,1)
        #par(xaxp = 0.01 * percentiles)
        plot(ecdf(wind_data[[variable]]),lwd=2,xlab = xlable,ylab = "Probability on non-exceedance[%]",main="",yaxt="n")#,xaxp=percentiles)
        axis(2,at=percentiles/100,labels=percentiles)
        grid(col="dark grey",ny="")
        abline(h=percentiles/100,col="dark grey", lty=3)
        sapply(1:length(seasons), function (x) {plot(ecdf(getSeasonalData(wind_data,seasons[x])[[variable]]),col=cols[x],lwd=2,add=TRUE,xlab="",ylab="",yaxt="n")})
        plot(ecdf(wind_data[[variable]]),lwd=2,xlab = "",ylab = "",main="",add=TRUE,yaxt="n")
        #par(xpd = TRUE)
        legend("bottomright", legend=c("annual",seasons), col=c("black",cols),lwd=2,cex=0.8)
}

tabulateSeasonalECDF <- function(wind_data,percentiles = c(0,0.01,0.02,0.05,seq(0.1,0.9,0.1),0.95,0.98,0.99,1),variable){
        annual <- quantile(wind_data[[variable]],probs = percentiles, na.rm = TRUE)
        row_names <- names(annual)
        annual <- as.numeric(annual)
        seasons <- c("spring","summer","autumn","winter")
        seasons_quants <- sapply(1:length(seasons), function(x) {as.numeric(
                quantile(getSeasonalData(wind_data,seasons[x])[[variable]],probs = percentiles, na.rm = TRUE))})
        colnames(seasons_quants) <- seasons
        all_df <- cbind(seasons_quants,annual)
        rownames(all_df) <- row_names
        return(all_df)
}

tabulateMonthlyECDF <- function(wind_data,percentiles = c(0,0.01,0.02,0.05,seq(0.1,0.9,0.1),0.95,0.98,0.99,1),variable){
      annual <- quantile(wind_data[[variable]],probs = percentiles, na.rm = TRUE)
      row_names <- names(annual)
      months <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
      months_quants <- sapply(1:length(months), function(x) {as.numeric(
        quantile(getMonthlyData(wind_data,months[x])[[variable]],probs = percentiles, na.rm = TRUE))})
      colnames(months_quants) <- months
      all_df <- months_quants
      rownames(all_df) <- row_names
      return(all_df)
}

# Helper functions --------------------------------------------------------
readWaveData <- function (myfolder,myfile,DateIndex,Hm0Index,TpIndex,MWDIndex) {
  # Reads in a mike export formatted text file, given folder name, file name and
  # specific indexes for Hm0, Tp and MWD
  wavefile <- paste(myfolder,myfile,sep="")
  #waveData <- read.zoo(wavefile,header=FALSE,sep="\t",skip=3,index = 1, tz = "")
  waveData <- read.table(wavefile,header=FALSE,sep="\t",skip=3,col.names=c("date","PMBHm0","Hm0Ph1","Hm0Ph2",
                                                                           "PMBTp","TpPh1","TpPh2","PMBMWD","MWDPh1","MWDPh2"))
  #mywavedata <- as.data.frame(cbind(date = as.Date.POSIXct(waveData[,1]),
                                    #Hm0 = as.numeric(waveData[,Hm0Index]),
                                    #Tp = as.numeric(waveData[,TpIndex]),
                                    #MWD = as.numeric(waveData[,MWDIndex])))
  #return (mywavedata)
  return (waveData)
}

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


writeExcel <- function(filename,dataframe,sheet,start_cell){
  require(XLConnect)
  start_row <- gsub("[^0-9]","", start_cell)
  start_col <- gsub("[^A-Z]", "", start_cell)
  wb <- loadWorkbook(filename = filename,create = TRUE)
  createSheet(wb,name=sheet)
  createName(wb,name=sheet,formula = paste(sheet,"!",paste("$",start_col,"$",start_row,sep=""),sep=""))
  writeNamedRegion(wb,data = dataframe,header = FALSE,rownames = FALSE,name = sheet)
  saveWorkbook(object = wb)
}

#points <- c("101","102","103","201","301","302","303","304")

#sheet_prefix <- c("Exceedance_months","Exceedance_seasons")


#sapply(seq(1:length(sheet_prefix)),function(x){
#  sapply(seq(1:length(points)),function(y){

#    if (x == 1){tmpdf <- tabulateMonthlyECDF(wind_data = Wave_Time_Series_Mean,variable = paste("Hm0",points[y],sep="_"))
#      writeExcel(filename = "Wave_Timeseries_Months_All_Exceedance_Wave.xlsx",dataframe = tmpdf,sheet = paste(sheet_prefix[x],points[y],sep="_"),start_cell = "B4")}
#    else if (x == 2){tmpdf <- tabulateSeasonalECDF(wind_data = Wave_Time_Series_Mean,variable = paste("Hm0",points[y],sep="_"))
#      writeExcel(filename = "Wave_Timeseries_Months_All_Exceedance_Wave.xlsx",dataframe = tmpdf,sheet = paste(sheet_prefix[x],points[y],sep="_"),start_cell = "B4")}

#  })
#})

