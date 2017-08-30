#
# rpys.R
# Author: Dr. Robin Haunschild
# Version: 0.0.1
# Date: 08/30/2017
#

#' @title Create a spectrogram using data from the free software CRExplorer
#'
#' @description
#' Provide the contents of the CSV file from the CRExplorer in a data frame, e.g. df,
#' and the function call rpys(df, py1, py2) creates the spectrogram.
#' Here, py1 and py2 are the lowest and highest publication year to be used in the plot.
#' The function rpys takes some optional arguments to modify its behaviour, see arguments and details.
#'
#' @details
#' rpys(df=data_frame, py1=integer_value, py2=integer_value, smoothing=boolean, col_cr=character_color_name, col_med=character_color_name, par_pch=integer, ...)
#' Only the arguments df, py1, and py2 are necessary. All other aruments are optional.
#'
#' Literature:
#'
#' - Thor, A., Bornmann, L., Marx, W., Haunschild, R., Leydesdorff, L., & Mutz, Ruediger (2017). Website of the free software CRExplorer, http://www.crexplorer.net
#'
#' An example data frame is provided as \code{rpys_example_data} in the package. It can be used to create an example spectrogram.
#'
#' @examples
#'
#' data(rpys_example_data)
#'
#' rpys(rpys_example_data, 1935, 2010)
#'
#' @param df data frame with reference publication year, number of cited references, and median deviation as exported from the CRExplorer (File > Export > CSV (Graph)).
#' @param py1 determines lowest reference publication year which should be shown in the graph.
#' @param py2 determines highest reference publication year which should be shown in the graph.
#' @param smoothing boolean variable (optional parameter) which determines if the lines of the spectrogram are smoothed or not.
#' (T: yes apply smoothing, F: no do not apply smoothing). The default value is T.
#' @param col_cr character color name value to determine color of the line and points of the number of cited references (optional parameter). The default value is "red".
#' @param col_med character color name value to determine color of the line and points of the median deviation (optional parameter). The default value is "blue".
#' @param par_pch integer value to set the point type (optional parameter). The default value is 20.
#' @param ... additional arguments to pass to the \link{plot}, \link{points}, and \link{lines} functions.
#'
#' @export
rpys <- function(df, py1, py2, col_cr="red", col_med="blue", smoothing=TRUE, par_pch=20, ...) {
  colnames(df) <- c("Year", "NCR", "Median.5")
  nuller <- (df$Year/df$Year)-1
  df <- df[df$Year<py2+1, ]
  df <- df[df$Year>py1-1, ]
  if(smoothing) {
    dftmp <- data.frame(py2+1,0,0)
    colnames(dftmp) <- colnames(df)
    df <- rbind(df, dftmp)
    
    # Splines plot without NCR line in negative NCR regime
    ncr <- spline(df$Year, df$NCR, method="periodic", n=10*length(df$NCR))
    ncr$y[ncr$y<0] <- 0
    max_val <- length(ncr$x)-10
    ncr$x <- ncr$x[1:max_val]
    ncr$y <- ncr$y[1:max_val]
    med5 <- spline(df$Year, df$Median.5, n=10*length(df$NCR))
    med5$x <- med5$x[1:max_val]
    med5$y <- med5$y[1:max_val]
    df <- df[df$Year<py2+1, ]
  } else {
    ncr <- data.frame(df$Year, df$NCR)
    med5 <- data.frame(df$Year, df$Median.5)
  }
  plot(df$Year, df$NCR, type='p', pch=par_pch, col=col_cr, xlim=c(min(df$Year),max(df$Year)), ylim=c(min(df$Median.5), max(df$NCR)), xlab='Cited reference year', ylab='Number of cited references', ...)
  points(df$Year, df$Median.5, type='p', pch=par_pch, col=col_med, ...)
  abline(h=0,col="black")
  lines(ncr, col=col_cr, ...)
  lines(med5, col=col_med, ...)
}

