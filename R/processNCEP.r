### MENG 2014 ###
#rm(list =ls())
list.of.packages <- c("xts", "reshape","openair","lattice","plyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(xts)
library(lattice)
library(plyr)
library(openair)
library(dplyr)
# Combine wave data -------------------------------------------------------
readWaveParameter <- function(file,parameter){
        # Funciton to read individual wave parameters into a time series
        myParamData <- read.table(file = file,header = FALSE,sep="\t",
                                  col.names=c("date",parameter))
        myParamData <- myParamData[unique(myParamData[,1]),]
        myParamData$date <- as.POSIXct(myParamData$date)
        #myParamData <- xts(myParamData[[parameter]],myParamData$dat)
        return(myParamData)
}

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
                plot(x,y,type="l",xlab = xlab, ylab = myylab, col="blue",lwd=0.7)
        }
        else {
                xlab = ""
                plot(x,y,type="l",xlab = xlab, ylab = myylab, col="blue",lwd=0.7)
        }
}

convertToTs <- function(x,y){
        require(xts)
        myts <- xts(y,x)
        return(myts)
}

combineData <- function(...){
        hs_file <- paste(root_dir,"nww3.hs.txt",sep="")
        tp_file <- paste(root_dir,"nww3.tp.txt",sep="")
        dp_file <- paste(root_dir,"nww3.dp.txt",sep="")
        testHs <- readWaveParameter(hs_file,"Hs")
        testTp <- readWaveParameter(tp_file,"Tp")
        testDp <- readWaveParameter(dp_file,"Dp")

        nww3Param <- testHs
        nww3Param$Tp <- testTp$Tp
        nww3Param$Dp <- testDp$Dp
        return(nww3Param)
}

plotWaveParams <- function(nww3Param,wave_parameters){
        op <- par(no.readonly = TRUE)
        # Set 3 by 1 plot
        layout(matrix(c(1,2,3), 3, 1, byrow = TRUE))
        plotWaveParam(nww3Param$date,nww3Param[[wave_parameters[1]]],myylab = "Hm0 [m]",xaxt="n")
        plotWaveParam(nww3Param$date,nww3Param[[wave_parameters[2]]], myylab = "Tp [s]",xaxt="n",type="p")
        plotWaveParam(nww3Param$date,nww3Param[[wave_parameters[3]]], myylab = "Wave direction [deg TN]",bottom="TRUE")
        par(op)
}
#' A Metocean function
#'
#' This function reads in a Mike by DHI time series file, renames headers and brings into a DF
#' @param File name text
#' @keywords Metocean
#' @export
#' @examples
#' plotSeasonalRose()
plotSeasonalRose <- function(nww3Param,wave_params,breaks){
        require(xts)
        require(lattice)
        require(plyr)
        require(openair)
        require(dplyr)
        #oldNames <- names(nww3Param)
        #colnames(nww3Param) <- names
        my.statistic <- list("fun"=length,"unit" = "%","scale" = "all",
            "lab" = "" ,
            "fun2" = function(x) signif(mean(x, na.rm = TRUE), 3), "lab2" = "mean","labcalm" = function(x) round(x, 1))
        waveRose(mydata = nww3Param,
                 ws = wave_params[1],
                 wd = wave_params[3],
                 angle = 5,
                 type = "season",
                 hemisphere="southern",
                 auto.text = FALSE,
                 breaks = breaks,
                 key=TRUE,
                 key.header = expression(paste('H'[m0],' [m]')),
                 annotate = FALSE,
                 key.position = "right",
                 key.footer = "",
                 paddle = FALSE,
                 par.settings=list(fontsize=list(text=12),mar=c(0,0,0,0)),
                 main="",
                 statistic=my.statistic
                 )
}

#' A Metocean function
#'
#' This function reads in a Mike by DHI time series file, renames headers and brings into a DF
#' @param File name text
#' @keywords Metocean
#' @export
#' @examples
#' plotAnnualWaveRose()
plotAnnualWaveRose <- function(nww3Param,wave_params,breaks){
    require(xts)
    require(lattice)
    require(plyr)
    require(openair)
    require(dplyr)
     my.statistic <- list("fun"=length,"unit" = "%","scale" = "all",
            "lab" = "" ,
            "fun2" = function(x) signif(mean(x, dna.rm = TRUE), 3), "lab2" = "mean","labcalm" = function(x) round(x, 1))
        waveRose(mydata = nww3Param,
                 ws = wave_params[1],
                 wd = wave_params[3],
                 angle = 10,
                 auto.text = FALSE,
                 breaks = breaks,
                 key.header = expression(paste('H'[m0],' [m]')),
                 annotate = FALSE,
                 key.position = "right",
                 key.footer = "",
                 paddle = FALSE,
                 par.settings=list(fontsize=list(text=12),mar=c(0,0,0,0)),
                 main="",
                 statistic=my.statistic
                 )
}
#' A Metocean function
#'
#' This function reads in a Mike by DHI time series file, renames headers and brings into a DF
#' @param File name text
#' @keywords Metocean
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

#root_dir <- "../../Data/Wave/NCEP/processed/"
#nww3Param <- combineData(root_dir)
#plotSeasonalRose(nww3Param)
#png(filename = "test.png",width = 5.45 * 600, height = 5.45 * 600, res = 600)
#scatterPlot(nww3Param,x="Hs",y="Tp",
#            method = "hexbin",
#            xbins=100,
#            cols = "jet",
#            hemisphere="southern",
#            key.position = "left",
#            key = FALSE,
            #cex.title=1,
            #cex.label = 0.8,
            #legend.width=100,
#            aspect=1,
            #cex.xlab=1,
            #legend.inner=0.00,
#            type="season",
#            xlab=expression(paste(H[s]," [m]")),
#            ylab=expression(paste(T[p]," [s]"))
#            )
#dev.off()
