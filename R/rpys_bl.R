#
# rpys_bl.R
# Author: Dr. Robin Haunschild
# Version: 0.0.2
# Date: 10/07/2021
#

#' @title Create a spectrogram with bars and lines using data from the free software CRExplorer
#'
#' @description
#' Provide the contents of the CSV (Graph) file from the 'CRExplorer' in a data frame, e.g. df,
#' and the function call \link{rpys_bl}(df) creates a spectrogram. Previously, you should use
#' the function \link{rpys} for a plain line graph to determin the proper parameters, e.g., x_offset and x_range.
#' Determination of the proper x_offset and x_range is a bit tricky.
#' Usage of a wrong value of x_range will cause an error. Usage of a wrong value of x_offset will produce
#' a plot. However, the line for the median deviation and the bars might not be at the proper location.
#' First, adjust x_range if necessary, and second, adjust x_offset so that the x axis is properly aligned
#' with the line and bars. Comapare the plot from \link{rpys_bl} with your data and the plot from the function \link{rpys}.
#' The function \link{rpys_bl} takes some optional arguments to modify its behaviour, see arguments and details.
#'
#' @details
#' rpys_bl(df=data_frame, py1=integer_value, py2=integer_value, x_range=integer_value, smoothing=boolean, 
#' col_cr=character_color_name, col_med=character_color_name, col_ol=character_color_name, par_mar=integer_vector, plot_NCR=boolean, plot_Med=boolean,
#' x_offset=integer_value, x_min=integer_value, x_max=integer_value, x_step1=integer_value, x_step2=integer_value, y1_min=integer_value, y1_max=integer_value, y1_step=integer_value,
#' y2_min=integer_value, y2_max=integer_value, y2_step=integer_value,
#' lx=integer_value, ly=integer_value,
#' pl_offset=integer_value, bar_border=string_value, outliers=integer_value, lpos=integer_value, pl_cex=floating_point_value,
#' TFmin=integer_value,TFmax=integer_value, ...)
#' Only the argument df is necessary. All other aruments are optional, but many should be provided to produce nice plots.
#'
#' Literature:
#'
#' - Thor, A., Bornmann, L., & Haunschild, R. (2021). Website of the free software 'CRExplorer', http://www.crexplorer.net
#' - Thor, A., Bornmann, L., & Haunschild, R. (2018). CitedReferencesExplorer (CRExplorer) manual. Retrieved December 19, 2019, from https://andreas-thor.github.io/cre/manual.pdf
#' - Tukey, J. W. (1977). Exploratory data analysis. Boston, MA, USA: Addison-Wesley Publishing Company.
#'
#' An example data frame is provided as \code{rpys_example_data} in the package. It can be used to create an example spectrogram.
#'
#' @examples
#'
#' data(rpys_example_data)
#'
#' rpys_bl(rpys_example_data)
#'
#' rpys_bl(rpys_example_data, x_min=1930, x_max=2020, x_range=91, x_offset=1, lx=1926, ly=135, 
#' y1max=300, y1_step=50, y2_min=-150, y2_max=150, y2_step=25, lpos=1)
#' 
#' rpys_bl(rpys_example_data, py1=1930, py2=2020, x_offset=1, lx=1926, ly=135, y1max=300, 
#' y1_step=50, y2_min=-150, y2_max=150, y2_step=25, lpos=1)
#'
#' @param df data frame with reference publication year, number of cited references, and median deviation as exported from the CRExplorer (File > Export > CSV (Graph)).
#' @param py1 determines lowest reference publication year which should be shown on the x axis (optional parameter). The default is the minimum RPY.
#' @param py2 determines highest reference publication year which should be shown on the x axis (optional parameter). The default is the maximum RPY.
#' @param x_min determines lowest reference publication year which should be shown on the x axis (optional parameter). The default is the minimum RPY.
#' @param x_max determines highest reference publication year which should be shown on the x axis (optional parameter). The default is the maximum RPY.
#' @param x_range is the range of the x axis (optional parameter). The default is py2-py1+1.
#' @param x_offset determines the x axis offset to adjust the median deviation curve properly (optional parameter). The default is 0.
#' @param x_step1 is the interval of major x tics (optional parameter).
#' @param x_step2 is the interval of minor x tics (optional parameter).
#' @param y1_min is the minimum left y axis value (optional parameter).
#' @param y1_max is the maximum left y axis value (optional parameter).
#' @param y1_step is the interval left y axis (optional parameter).
#' @param y2_min is the minimum right y axis value (optional parameter).
#' @param y2_max is the maximum right y axis value (optional parameter).
#' @param y2_step is the interval right y axis (optional parameter).
#' @param lx is the x position of the legend (optional parameter).
#' @param ly is the y position of the legend according to the right y axis (optional parameter).
#' @param pl_offset is the offset of the year label (optional parameter).
#' @param bar_border is the color around the bars (optional parameter).
#' @param outliers is an integer that indicates if outliers should be detected (optional parameter):
#' (0: no outlier detection, 1: outliers are detected and marked, 2: only extreme outliers are detected and marked)
#' @param lpos is an integer that determines the position of the outlier year label around the point (optional parameter).
#' Values of 1, 2, 3, and 4, respectively indicate positions below, to the left of, above, and to the right of the specified coordinates.
#' @param pl_cex is the cex value of the year labels (optional parameter).
#' @param TFmin is the first year that should be used for outlier detection according to Tukey's fences.
#' @param TFmax is the last year that should be used for outlier detection according to Tukey's fences.
#' @param smoothing boolean variable (optional parameter) which determines if the lines of the spectrogram are smoothed or not.
#' (T: yes apply smoothing, F: no do not apply smoothing). The default value is T.
#' @param col_cr is a character color name value to determine color of the bars of the number of cited references (optional parameter). The default value is "grey".
#' @param col_med is a character color name value to determine color of the line of the median deviation (optional parameter). The default value is "blue".
#' @param col_ol is a character color name value to determine color of the outlier labels (optional parameter). The default value is "red".
#' @param par_mar integer vector to set the margins (optional parameter). The default value is c(5, 5, 1, 5).
#' @param plot_NCR boolean variable (optional parameter) which determines the NCR curve should be plotted.
#' @param plot_Med boolean variable (optional parameter) which determines the median deviation curve should be plotted.
#' @param ... additional arguments to pass to the \link{plot} function.
#'
#' @export
rpys_bl <- function(df, py1=min(df$Year), py2=max(df$Year), x_range=py2-py1+1,
                    col_cr="grey", col_med="blue", col_ol='red', smoothing=TRUE, par_mar = c(5, 5, 1, 5),
                    x_offset=0, x_min=py1, x_max=py2, x_step1=10, x_step2=5, y1_min=0, y1_max=max(df$NCR), y1_step=(max(df$NCR)-min(df$NCR))/5,
                    y2_min=min(df$Median.5), y2_max=max(df$Median.5), y2_step=(max(df$Median.5)-min(df$Median.5))/5,
                    lx=median(df$Year), ly=median(df$Median.5),
                    pl_offset=(max(df$NCR)-min(df$NCR))/50, bar_border='white', outliers=2, lpos=3, pl_cex=0.9,
                    TFmin=py1,TFmax=py2,
                    plot_NCR=TRUE, plot_Med=TRUE, ...) {

  if(length(df)>3) {
     df <- df[,c(1,2,3)]
   }
   colnames(df) <- c("Year", "NCR", "Median.5")
   df1 <- df
   df00 <- as.data.frame(matrix(data=c(min(df1$Year)-2, 0L, 0L), nrow=1, ncol=3))
   df0 <- as.data.frame(matrix(data=c(min(df1$Year)-1, 0L, 0L), nrow=1, ncol=3))

   colnames(df0) <- colnames(df1)
   colnames(df00) <- colnames(df1)
   df <- rbind(rbind(df00, df0), df1)

   if(smoothing) {
     med <- spline(df$Year, df$Median.5, method="periodic", n=10*length(df$Year))
   } else {
     med <- data.frame(df$Year, df$Median.5)
     colnames(med) <- c('x', 'y')
   }
   df_med <- data.frame(med$x, med$y)
   colnames(df_med) <- c('Year', 'NCR')

   med5p <- df1[df1$Median.5>=0 & df1$Year>=TFmin & df1$Year<=TFmax,]$Median.5
   TFu1 <- quantile(med5p, 3/4)+1.5*(quantile(med5p, 3/4)-quantile(med5p, 1/4))
   TFu2 <- quantile(med5p, 3/4)+3*(quantile(med5p, 3/4)-quantile(med5p, 1/4))
   px_TFu1 <- df1[df1$Median.5>TFu1,]$Year
   px_TFu2 <- df1[df1$Median.5>TFu2,]$Year
   
   if(outliers == 2) {
      px <- px_TFu2
      py <- df[df$Year %in% px,3]
   } else if(outliers == 1) {
     px <- px_TFu1
     py <- df[df$Year %in% px,3]
   } else {
     px <- NA
     py <- NA
   }
   
   xlab_text <- ''
   ylab_text <- ''
   log_axes <- !plot_Med
   if(!plot_Med) {
     xlab_text <- 'Reference publication year'
     ylab_text <- 'Number of cited references'
   }

   par(mar = par_mar)
   if(plot_NCR) {
      barData <- df$NCR
      x <- barplot(barData, 
             axes = FALSE,
             col = col_cr, 
             border=bar_border,
             xlab = xlab_text,
             ylab = ylab_text,
             ylim = c(y1_min, y1_max) )[, 1]
      axis(1, at = 1.2*seq(1, x_range, x_step1)+x_offset, labels = seq(x_min, x_max, x_step1))
      rug(x = 1.2*seq(1, x_range, x_step2)+x_offset, ticksize = -0.01, side = 1)
      axis(2, at = seq(y1_min, y1_max, y1_step), labels = seq(y1_min, y1_max, y1_step))
   }
   if(plot_Med) {
      if(plot_NCR) { 
         par(new = TRUE)
         plot(x = df_med$Year, y = df_med$NCR, type = "l", col = col_med, lwd=1.5, axes = FALSE, xlab = "Reference publication year", ylab = "Number of cited references", ylim=c(y2_min, y2_max), ...)
         axis(4, at = seq(y2_min, y2_max, y2_step), labels = seq(y2_min, y2_max, y2_step), col=col_med, col.axis=col_med)
         mtext("Five-year-median deviation", side = 4, line = 3, cex = par("cex.lab"), col=col_med)
      } else {
           plot(x = df_med$Year, y = df_med$NCR, type = "l", col = col_med, lwd=1.5, axes = TRUE, xlab = "Reference publication year", ylab = "Five-year-median deviation", ylim=c(y2_min, y2_max), ...)
      }
      abline(h=0, lty='dotted', col=col_med)
      if(outliers>0) {
         points(px, py, pch=8, col=col_ol)
      for(i in seq(1, length(px))) {text(px[i]+1, py[i]+pl_offset, px[i], col=col_ol, srt=90, pos=lpos, cex=pl_cex) }
      }
   }
   if(plot_NCR & plot_Med) {
     legend(lx, ly, legend=c('Number of cited references (NCR)', 'Deviation from the 5-year-median'), lty=1, lwd=c(5, 1.5), col=c(col_cr, col_med))
   }
   box()

}
