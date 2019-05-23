#
# beamplot.R
# Author: Dr. Robin Haunschild
# Version: 0.0.1
# Date: 05/20/2019
#

#' @title Create a beamplot using raw citations from a WoS download
#'
#' @description
#' Create a beamplot using raw citations from a WoS download. Use the format
#' "Other File Format --> Tab-delimited (Win, UTF-8)" and provide the downloaded file name.
#' a simple weighting of citation counts is also available for comparison of older with newer publications.
#'
#' @details
#' beamplot(wos_file="WoS_savedrecs.txt", do_weight=boolean)
#' Only the argument wos_file is mandatory. The argument do_weight is optional and FALSE by default.
#'
#' Literature:
#'
#' - Haunschild, R., Bornmann, L., & Adams, J. (2019). R package for producing beamplots as a preferred alternative to the h index when assessing single researchers (based on downloads from Web of Science), preprint: https://arxiv.org/abs/1905.09095
#'
#' @examples
#'
#' \dontrun{beamplot("WoS_savedrecs.txt")}
#'
#' @param wos_file is the file name of the downloaded WoS export in the format Tab-delimited (Win, UTF-8).
#' @param do_weight is a boolean to spcify if citation counts should be weighted with their age. The older the publication, the smaller the weight. The weight depends on on the difference between the year until that citations are counted (i.e., the current calendar year in the case of WoS downloads) and the publication year. A weighting factor of 1 is used for a difference of 0, 1/2 for a difference of 1, ..., and 1/11 for differences of ten or more.
#' @param ... further parameters passed to stripchart.
#'
#' @export

beamplot <- function(wos_file, do_weight=FALSE, ...) {

   rd <- read.csv(wos_file, sep='\t', header=FALSE, quote="")
   fl <- head(rd, 1)
   rd <- read.csv(wos_file, sep='\t', header=FALSE, quote="", skip=1)
   fl<-as.matrix(fl)
   colnames(fl) <- NULL
   colnames(rd) <- fl
   cits <- data.frame(rd$PY, rd$TC)
   colnames(cits) <- c('PY', 'TC')
   xlabel <- 'Number of citations'

   if(do_weight) {
  #
  # Simple weighting with age and attenuation after ten years.
      weights <- as.data.frame(cbind(rd$PY, 0))
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

