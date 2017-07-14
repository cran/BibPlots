#
# percentiles_scatter_plot.R
# Author: Dr. Robin Haunschild
# Version: 0.0.1
# Date: 07/10/2017
#

#' @title Create a scatter plot using journal and paper percentile values
#'
#' @description
#' Provide journal and paper percentile values in a data frame, e.g. df,
#' and the function call jpscatter(df) creates the scatter plot.
#' The function jpscatter takes some optional arguments to modify its behaviour, see arguments and details.
#'
#' @details
#' jpscatter(df=data_frame, off_set=numeric_value, print_stats=boolean, do_plot=boolean, digits=integer)
#' Only the argument df is necessary. All other aruments are optional.
#'
#' Literature:
#'
#' - Bornmann, L., & Haunschild, R. (2017). Plots for visualizing paper impact and journal impact of single researchers in a single graph, https://arxiv.org/abs/1707.04050
#'
#'
#' An example data frame is provided as \code{example_researcher} in the package. It can be used to create a scatter plot using default values.
#'
#' @examples
#'
#' data(example_researcher)
#'
#' jpscatter(example_researcher)
#'
#' @param df data frame with journal and paper percentiles
#' @param off_set determines the location of additional plotted information (number of points in each
#' quadrant), values between 0 and 40 might be useful (optional parameter). The default value is 0.
#' @param print_stats boolean variable (optional parameter) which determines if the additional statistical values are printed
#' to the R console (T: yes print, F: no do not print). The default value is T.
#' @param do_plot boolean variable (optional parameter) which determines if the scatter plot is actually produced
#' (T: yes plot, F: no do not plot). The default value is T.
#' @param digits integer value to determine the number of desired digits after the decimal point for statistical values (optional parameter). The default value is 1.
#' @param ... additional arguments to pass to the \link{plot} function
#'
#' @export

jpscatter <- function(df, off_set=0, print_stats=TRUE, do_plot=TRUE, digits=1, ...) {

colnames(df) <- c('jif_perc', 'p_perc')
df <- df[!is.na(df$p_perc),]
df <- df[!is.na(df$jif_perc),]

df2 <- df[df$jif_perc>=50 & df$p_perc<50,]
df1 <- df[df$jif_perc>=50 & df$p_perc>=50,]
df4 <- df[df$jif_perc<50 & df$p_perc>=50,]
df3 <- df[df$jif_perc<50 & df$p_perc<50,]
n <- length(df$p_perc)
nq1 <- length(df1$p_perc)
nq2 <- length(df2$p_perc)
nq3 <- length(df3$p_perc)
nq4 <- length(df4$p_perc)
pq1 <- round(nq1/n*100, digits)
pq2 <- round(nq2/n*100, digits)
pq3 <- round(nq3/n*100, digits)
pq4 <- round(nq4/n*100, digits)
nc1 <- nq1+nq4
nc2 <- nq2+nq3
pc1 <- round(nc1/n*100, digits)
pc2 <- round(nc2/n*100, digits)
nr1 <- nq1+nq2
nr2 <- nq3+nq4
pr1 <- round(nr1/n*100, digits)
pr2 <- round(nr2/n*100, digits)

avg1_jif_perc <- mean(df1$jif_perc)
avg1_p_perc <- mean(df1$p_perc)
avg1 <- data.frame(avg1_p_perc, avg1_jif_perc)

avg2_jif_perc <- mean(df2$jif_perc)
avg2_p_perc <- mean(df2$p_perc)
avg2 <- data.frame(avg2_p_perc, avg2_jif_perc)

avg3_jif_perc <- mean(df3$jif_perc)
avg3_p_perc <- mean(df3$p_perc)
avg3 <- data.frame(avg3_p_perc, avg3_jif_perc)

avg4_jif_perc <- mean(df4$jif_perc)
avg4_p_perc <- mean(df4$p_perc)
avg4 <- data.frame(avg4_p_perc, avg4_jif_perc)

avg_jif_perc <- mean(df$jif_perc)
avg_p_perc <- mean(df$p_perc)

if(do_plot) {
   par(mar = c(5, 6, 4, 2) + 0.1)
   plot(df$p_perc, df$jif_perc, type='p', pch=16, xlim=c(0,100), ylim=c(0,100), 
     ylab="Journal impact",
     xlab="Paper impact", ...)
   abline(h=50, col='red')
   abline(v=50, col='red')
   abline(a=0,b=1, col='red')

   x1 <- 40-off_set
   x2 <- 60+off_set

   text(x1,100, bquote(paste('n'['q2']*'=', .(nq2), '; ', .(pq2),'%')))
   text(x2,100, bquote(paste('n'['q1']*'=', .(nq1), '; ', .(pq1),'%')))
   text(x1,0, bquote(paste('n'['q3']*'=', .(nq3), '; ', .(pq3),'%')))
   text(x2,0, bquote(paste('n'['q4']*'=', .(nq4), '; ', .(pq4),'%')))

   mtext(bquote(paste('n'['c2']*'=', .(nc2), '; ', .(pc2), '%\t\t\t\t\t\t\t\tn'['c1']*'=', .(nc1), '; ', .(pc1), '%')), side=3, line=1)
   mtext(bquote(paste('n'['r2']*'=', .(nr2), '; ', .(pr2), '%\t\t\t\tn'['r1']*'=', .(nr1), '; ', .(pr1), '%')), side=4, line=1)

   points(avg1, col='red', type='p', pch=0)
   points(avg2, col='red', type='p', pch=0)
   points(avg3, col='red', type='p', pch=0)
   points(avg4, col='red', type='p', pch=0)

   abline(h=avg_jif_perc, col='red', lty=2)
   abline(v=avg_p_perc, col='red', lty=2)
}

if(print_stats) {
   print(paste('n(r1)=', nr1, '; ', pr1, '%', sep=''))
   print(paste('n(r2)=', nr2, '; ', pr2, '%', sep=''))
   print(paste('n(c1)=', nc1, '; ', pc1, '%', sep=''))
   print(paste('n(c2)=', nc2, '; ', pc2, '%', sep=''))
   print(paste('n(q1)=', nq1, '; ', pq1, '%', sep=''))
   print(paste('n(q2)=', nq2, '; ', pq2, '%', sep=''))
   print(paste('n(q3)=', nq3, '; ', pq3, '%', sep=''))
   print(paste('n(q4)=', nq4, '; ', pq4, '%', sep=''))
   print(paste('avg(q1)=', round(avg1$avg1_p_perc,digits), ', ', round(avg1$avg1_jif_perc,digits), sep=''))
   print(paste('avg(q2)=', round(avg2$avg2_p_perc,digits), ', ', round(avg2$avg2_jif_perc,digits), sep=''))
   print(paste('avg(q3)=', round(avg3$avg3_p_perc,digits), ', ', round(avg3$avg3_jif_perc,digits), sep=''))
   print(paste('avg(q4)=', round(avg4$avg4_p_perc,digits), ', ', round(avg4$avg4_jif_perc,digits), sep=''))
   print(paste('avg(total)=', round(avg_p_perc,digits), ', ', round(avg_jif_perc,digits), sep=''))
}

}
