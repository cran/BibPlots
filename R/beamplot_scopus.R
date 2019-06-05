#
# beamplot.R
# Author: Dr. Robin Haunschild
# Version: 0.0.1
# Date: 06/05/2019
#

#' @title Create a beamplot using raw citations from a Scopus download
#'
#' @description
#' Create a beamplot using raw citations from a Scopus download. Use the CSV/Excel format
#' and provide the downloaded file name.
#' A simple weighting of citation counts is also available for comparison of older with newer publications.
#'
#' @details
#' beamplot_scopus(scopus_file="Scopus.csv", do_weight=boolean)
#' Only the argument scopus_file is mandatory. The argument do_weight is optional and FALSE by default.
#'
#' Literature:
#'
#' - Haunschild, R., Bornmann, L., & Adams, J. (2019). R package for producing beamplots as a preferred alternative to the h index when assessing single researchers (based on downloads from Web of Science), Scientometrics, DOI 10.1007/s11192-019-03147-3, preprint: https://arxiv.org/abs/1905.09095
#'
#' @examples
#'
#' \dontrun{beamplot_scopus("Scopus.csv")}
#'
#' @param scopus_file is the file name of the downloaded Scopus export in the format CSV/Excel.
#' @param do_weight is a boolean to spcify if citation counts should be weighted with their age. The older the publication, the smaller the weight. The weight depends on on the difference between the year until that citations are counted (i.e., the current calendar year in the case of Scopus downloads) and the publication year. A weighting factor of 1 is used for a difference of 0, 1/2 for a difference of 1, ..., and 1/11 for differences of ten or more.
#' @param ... further parameters passed to stripchart.
#'
#' @export

beamplot_scopus <- function(scopus_file, do_weight=FALSE, ...) {

   df <- read.csv(scopus_file, sep=',', quote='"')
   cits <- data.frame(df$Year, df$Cited.by)
   colnames(cits) <- c('PY', 'TC')
   cits[is.na(cits$TC),]$TC <- 0
   xlabel <- 'Number of citations'

   if(do_weight) {
  #
  # Simple weighting with age and attenuation after ten years.
      weights <- as.data.frame(cbind(df$Year, 0))
      colnames(weights) <- c('PY', 'x')
      weights$x <- (as.numeric(format(Sys.time(), "%Y"))-weights$PY+1)
      weights[weights$x>10,]$x <- 11
      cits_cn <- colnames(cits)
#      df_cits <- merge(cits, weights, by = 'PY')
      df_cits <- as.data.frame(cbind(cits, weights))
      cits <- data.frame(df_cits$PY, df_cits$TC/df_cits$x)
      colnames(cits) <- cits_cn

# Simple linear weighting without attenuation after ten years ...
#     cits$TC <- cits$TC/(as.numeric(format(Sys.time(), "%Y"))-cits$PY+1)
     xlabel <- 'Number of age-weighted citations'
   }

   lrd <- split(cits$TC, cits$PY)
   yvals <- seq( min(cits$PY), max(cits$PY), by= 1)
   par(mar=c(5,6,6,2) + 0.2)

   stripchart(cits$TC ~ cits$PY, method='stack', offset=0.2, pch=18, las=1,
           xlab=xlabel, 
           ylab='Publication Year', ...
   )

   abline(v=median(cits$TC, na.rm=TRUE), lty=5, col='black')

# Plot year lines
   for (i in 1:length(lrd)){
     if (length(lrd[[i]])) {segments(min(lrd[[i]]), i, max(lrd[[i]]), i)}
   }
# Plot median of citations in each year
   for (i in 1:length(lrd)){
     if (length(lrd[[i]])){points(median(lrd[[i]], na.rm=TRUE),i-0.2, pch=17, col='black')}
   }

# Plot number of papers of upper x axis
   npubs <- matrix(0)
   for (i in 1:length(lrd)){
     if (length(lrd[[i]])) { npubs[i] <- length(lrd[[i]]) } else { npubs[i] <- 0 }
   }

   x2vals <- seq( 0, max(npubs), by= 1)
   f <- max(cits$TC)/max(npubs)
   axis(3, at=f*x2vals, labels=x2vals, xlim=c(0, max(npubs)), col.ticks="red", col="red", col.lab="red", col.axis="red")
   for (i in 1:length(lrd)){
     if (length(lrd[[i]])){points(f*npubs[[i]],i+0.2, pch=16, col='red')}
   }

   mtext(side=3, line=3, 'Number of publications', col="red")
   abline(v=f*median(npubs, na.rm=TRUE), lty=5, col='red')

}

