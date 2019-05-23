#
# ncr_comp.R
# Author: Dr. Robin Haunschild
# Version: 0.0.1
# Date: 01/22/2019
#

#' @title Create a spectrogram using data from the free software CRExplorer
#'
#' @description
#' Provide the contents of CSV files from the 'CRExplorer' in data frames, e.g. df1 and df2,
#' and the function call ncr_comp(df1, df2, py1, py2) creates a plot with both sets of NCR values.
#' Here, py1 and py2 are the lowest and highest publication year to be used in the plot.
#' The function ncr_comp takes some optional arguments to modify its behaviour, see arguments and details.
#'
#' @details
#' ncr_comp <- function(df1, df2, py1, py2, col_cr = "red", smoothing = TRUE, par_pch = 20, ...)
#' Only the arguments df1, df2, py1, and py2 are necessary. All other aruments are optional.
#' Please use the function \code{legend} to add a user-defined legend
#' The solid curve represents the data from df1 and the dotted curve represents the data from df2.
#'
#' Literature:
#'
#' - Thor, A., Bornmann, L., Marx, W., Haunschild, R., Leydesdorff, L., & Mutz, Ruediger (2017). Website of the free software 'CRExplorer', http://www.crexplorer.net
#'
#' @param df1 data frame 1 with reference publication year and number of cited references, e. g., as exported from the CRExplorer (File > Export > CSV (Graph)).
#' @param df2 data frame 2 with reference publication year and number of cited references, e. g., as exported from the CRExplorer (File > Export > CSV (Graph)).
#' @param py1 determines lowest reference publication year which should be shown in the graph.
#' @param py2 determines highest reference publication year which should be shown in the graph.
#' @param smoothing boolean variable (optional parameter) which determines if the lines of the spectrogram are smoothed or not.
#' (T: yes apply smoothing, F: no do not apply smoothing). The default value is T.
#' @param col_cr character color name value to determine color of the line and points of the number of cited references (optional parameter). The default value is "red".
#' @param par_pch integer value to set the point type (optional parameter). The default value is 20.
#' @param ... additional arguments to pass to the \link{plot}, \link{points}, and \link{lines} functions.
#'
#' @export

fit_ncr <- function(df, type="NCR") {
  if(type=="Med") {
    df$NCR <- df$Median.5
  }
  lyear <- tail(df$Year, 1)
  dftmp <- data.frame(lyear+1.0, 0, 0)
  colnames(dftmp) <- colnames(df)
  df <- rbind(df, dftmp)
  
  # Splines fit without NCR line in negative NCR regime
  ncr <- spline(df$Year, df$NCR, method="periodic", n=10*length(df$NCR))
  if(type=="NCR") {
    ncr$y[ncr$y<0] <- 0
  }
  ncr_max <- length(ncr$x)-10
  ncr$x <- ncr$x[1:ncr_max]
  ncr$y <- ncr$y[1:ncr_max]
  df <- df[df$Year<lyear+1, ]
  return(ncr)
}

ncr_comp <- function(df1, df2, py1, py2, col_cr = "red", smoothing = TRUE, par_pch = 20, ...) {
    df1$Median.5 <- 0
    df2$Median.5 <- 0
    colnames(df1) <- c('Year', 'NCR', 'Median.5')
    colnames(df2) <- c('Year', 'NCR', 'Median.5')
    df1 <- df1[df1$Year>=py1 & df1$Year<=py2,]
    df2 <- df2[df2$Year>=py1 & df2$Year<=py2,]
    dfm <- merge(df1, df2, by='Year', all=TRUE)
    dfm[is.na(dfm)] <- 0
    df1m <- data.frame(dfm$Year, dfm$NCR.x, dfm$Median.5.x)
    colnames(df1m) <- c('Year', 'NCR', 'Median.5')
    df2m <- data.frame(dfm$Year, dfm$NCR.y, dfm$Median.5.y)
    colnames(df2m) <- c('Year', 'NCR', 'Median.5')
    if(smoothing) {

      df1f <- fit_ncr(df1m)
#      df <- df1m
#      lyear <- tail(df$Year, 1)
#      dftmp <- data.frame(lyear+1.0, 0, 0)
#      colnames(dftmp) <- colnames(df)
#      df <- rbind(df, dftmp)
#      ncr <- spline(df$Year, df$NCR, method="periodic", n=10*length(df$NCR))
#      ncr$y[ncr$y<0] <- 0
#      ncr_max <- length(ncr$x)-10
#      ncr$x <- ncr$x[1:ncr_max]
#      ncr$y <- ncr$y[1:ncr_max]
#      df1f <- df[df$Year<lyear+1, ]

      df2f <- fit_ncr(df2m)
#      df <- df2m
#      lyear <- tail(df$Year, 1)
#      dftmp <- data.frame(lyear+1.0, 0, 0)
#      colnames(dftmp) <- colnames(df)
#      df <- rbind(df, dftmp)
#      ncr <- spline(df$Year, df$NCR, method="periodic", n=10*length(df$NCR))
#      ncr$y[ncr$y<0] <- 0
#      ncr_max <- length(ncr$x)-10
#      ncr$x <- ncr$x[1:ncr_max]
#      ncr$y <- ncr$y[1:ncr_max]
#      df2f <- df[df$Year<lyear+1, ]

    } else {
        df1f <- data.frame(df1m$Year, df1m$NCR)
        df2f <- data.frame(df2m$Year, df2m$NCR)
    }
    
    max1 <- max(df1f$y)
    max2 <- max(df2f$y)
    
    if(max1>max2) {
       plot(df1f, type = 'l', lty = 1, col = col_cr, ylab = 'Number of cited references', xlab = 'Reference publication year', ...)
       lines(df2f, lty = 3, col = col_cr)
    } else {
       plot(df2f, type = 'l', lty = 3, col = col_cr, ylab = 'Number of cited references', xlab = 'Reference publication year', ...)
       lines(df1f, lty = 1, col = col_cr)
    }
}

