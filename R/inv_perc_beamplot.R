#
# inv_perc_beamplot.R
# Author: Dr. Robin Haunschild
# Version: 0.0.1
# Date: 10/12/2021
#

#' @title Create a beamplot using inverted percentile values
#'
#' @description
#' Create a beamplot using inverted percentile values.
#'
#' @details
#' inv_perc_beamplot(rd, au_name='Name of researcher')
#' Only the rd is argument mandatory. It has to be a dataframe with two columns: (i) publication year and (ii) inverted percentile value with one row per paper/dataset.
#'
#' Literature:
#'
#' - Haunschild, R., Bornmann, L., & Adams, J. (2019). R package for producing beamplots as a preferred alternative to the h index when assessing single researchers (based on downloads from Web of Science), Scientometrics, DOI 10.1007/s11192-019-03147-3, preprint: https://arxiv.org/abs/1905.09095
#' - Bornmann, L. & Marx, W. (2014a). Distributions instead of single numbers: percentiles and beam plots for the assessment of single researchers. Journal of the American Society of Information Science and Technology, 65(1), 206–208
#' - Bornmann, L. & Marx, W. (2014b). How to evaluate individual researchers working in the natural and life sciences meaningfully? A proposal of methods based on percentiles of citations. Scientometrics, 98(1), 487-509. DOI: 10.1007/s11192-013-1161-y.
#' - Bornmann, L., & Haunschild, R. (2018). Plots for visualizing paper impact and journal impact of single researchers in a single graph. Scientometrics, 115(1), 385-394. DOI: 10.1007/s11192-018-2658-1.

#'
#' @examples
#'
#' \dontrun{inv_perc_beamplot(rd, au_name='Name of researcher')}
#'
#' @param rd is a dataframe with two columns: (i) publication year and (ii) inverted percentile value with one row per paper/dataset.
#' @param au_name is the name of the researcher this beamplot belongs to.
#' @param ... further parameters passed to stripchart.
#'
#' @export

inv_perc_beamplot <- function(rd, au_name = 'Example Researcher', ...) {
  
# replace column names
  colnames(rd)<-c('py','perc')

#remove rows with missing values
  rd <- rd[!is.na(rd$perc),]

# split in list
  lrd <- split(rd$perc,rd$py)
  lrd <- lapply(lrd, sort)
  xvals <- seq(100, 0, by= -10)
  yvals <- seq( min(rd$py), max(rd$py), by= 1)

#  par(mar=c(5,6,4,2) + 0.2)
# for plotting many points:  pch='.', ps=0.01
  stripchart(rd$perc ~ rd$py, method='stack', offset=0.1, xaxt='n',  pch=18, las=1, xlim=c(100,0),
           main= au_name, 
           xlab='Low Impact ---------------- Percentile ---------------- High Impact', 
           ylab='Publication Year', ...
  )

  axis(1, xvals, xvals)
  axis(2, yvals, yvals)


# plot 50% and median
  abline(v=50, lty=5)
  abline(v=median(rd$perc, na.rm=TRUE), lty=5, col='red')

# plot year lines
  for (i in 1:length(lrd)){
    if (length(lrd[[i]])) {segments(lrd[[i]][1],i, tail(lrd[[i]],1), i)}
  }
# plot medians
  for (i in 1:length(lrd)){
    if (length(lrd[[i]])){points(median(lrd[[i]], na.rm=TRUE),i-0.2, pch=17, col='red')}
  }
}
