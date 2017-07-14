#
# Bland-Altman-Bib-plot_base_graphics.R
# Author: Dr. Robin Haunschild
# Version: 0.0.1
# Date: 07/10/2017
#

#' @title Create a Bland-Altman plot using journal and paper percentile values
#'
#' @description
#' Provide journal and paper percentile values in a data frame, e.g. df,
#' and the function call BlandAltmanBibPlot(df) creates the Bland-Altman plot.
#' BlandAltmanBibPlot takes some optional arguments to modify its behaviour, see arguments and details.
#'
#' @details
#' BlandAltmanBibPlot(df=data_frame, off_set=numeric_value, print_stats=boolean, do_plot=boolean)
#' Only the argument df is necessary. All other aruments are optional.
#'
#' Literature:
#'
#' - Bland, J. M., & Altman, D. G. (1986). Statistical Methods for Assessing Agreement between Two Methods of Clinical Measurement. Lancet, 1(8476), 307-310, https://www.ncbi.nlm.nih.gov/pubmed/2868172
#'
#' - Bornmann, L., & Haunschild, R. (2017). Plots for visualizing paper impact and journal impact of single researchers in a single graph, https://arxiv.org/abs/1707.04050
#'
#'
#' An example data frame is provided as \code{example_researcher} in the package. It can be used to create a Bland-Altman plot using default values.
#'
#' @examples
#'
#' data(example_researcher)
#'
#' BlandAltmanBibPlot(example_researcher)
#'
#' @param df data frame with journal and paper percentiles
#' @param off_set determines the location of additional plotted information (number of points in each
#' quadrant), values between 0 and 40 might be useful (optional parameter). The default value is 0.
#' @param print_stats boolean variable (optional parameter) which determines if the additional statistical values are printed
#' to the R console (T: yes print, F: no do not print). The default value is T.
#' @param do_plot boolean variable (optional parameter) which determines if the Bland-Altman plot is actually produced
#' (T: yes plot, F: no do not plot). The default value is T.
#' @param digits integer value to determine the number of desired digits after the decimal point for statistical values (optional parameter). The default value is 1.
#' @param ... additional arguments to pass to the \link{plot} function
#'
#' @export


BlandAltmanBibPlot <- function(df, off_set=0, print_stats=TRUE, do_plot=TRUE, digits=1, ...) {

colnames(df) <- c('jif_perc', 'p_perc')
df <- df[!is.na(df$p_perc),]
df <- df[!is.na(df$jif_perc),]
df_t10 <- df[df$p_perc>=90,]
df_b90 <- df[df$p_perc<90,]
perc_diff <- df$p_perc-df$jif_perc
perc_avg <- (df$jif_perc+df$p_perc)/2
df <- data.frame(perc_diff, perc_avg)
perc_diff <- df_t10$p_perc-df_t10$jif_perc
perc_avg <- (df_t10$jif_perc+df_t10$p_perc)/2
df_t10 <- data.frame(perc_diff, perc_avg)
perc_diff <- df_b90$p_perc-df_b90$jif_perc
perc_avg <- (df_b90$jif_perc+df_b90$p_perc)/2
df_b90 <- data.frame(perc_diff, perc_avg)


df2 <- df[df$perc_diff>=0 & df$perc_avg<50,]
df1 <- df[df$perc_diff>=0 & df$perc_avg>=50,]
df4 <- df[df$perc_diff<0 & df$perc_avg>=50,]
df3 <- df[df$perc_diff<0 & df$perc_avg<50,]
n <- length(df$perc_avg)
nq1 <- length(df1$perc_avg)
nq2 <- length(df2$perc_avg)
nq3 <- length(df3$perc_avg)
nq4 <- length(df4$perc_avg)
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

avg1_perc_diff <- mean(df1$perc_diff)
avg1_perc_avg <- mean(df1$perc_avg)
avg1 <- data.frame(avg1_perc_avg, avg1_perc_diff)

avg2_perc_diff <- mean(df2$perc_diff)
avg2_perc_avg <- mean(df2$perc_avg)
avg2 <- data.frame(avg2_perc_avg, avg2_perc_diff)

avg3_perc_diff <- mean(df3$perc_diff)
avg3_perc_avg <- mean(df3$perc_avg)
avg3 <- data.frame(avg3_perc_avg, avg3_perc_diff)

avg4_perc_diff <- mean(df4$perc_diff)
avg4_perc_avg <- mean(df4$perc_avg)
avg4 <- data.frame(avg4_perc_avg, avg4_perc_diff)

avg_diff <- mean(df$perc_diff)
avg_perc <- mean(df$perc_avg)

if(do_plot) {
   par(mar = c(5, 6, 4, 2) + 0.1)
   plot(df_b90$perc_avg, df_b90$perc_diff, type='p', pch=16, xlim=c(0,100), ylim=c(-100,100), 
     ylab="Difference(paper impact - journal impact)\nHigher journal impact\t\tHigher paper impact",
     xlab="Low impact\t\t\t\t\t\t\t\t\tHigh impact\nAverage of paper impact and journal impact", ...)
   points(df_t10$perc_avg, df_t10$perc_diff, type='p', pch=1)
   abline(h=0, col='red')
   abline(v=50, col='red')

   x1 <- 40-off_set
   x2 <- 60+off_set

   text(x1,95, bquote(paste('n'['q2']*'=', .(nq2), '; ', .(pq2),'%')))
   text(x2,95, bquote(paste('n'['q1']*'=', .(nq1), '; ', .(pq1),'%')))
   text(x1,-95, bquote(paste('n'['q3']*'=', .(nq3), '; ', .(pq3),'%')))
   text(x2,-95, bquote(paste('n'['q4']*'=', .(nq4), '; ', .(pq4),'%')))

   mtext(bquote(paste('n'['c2']*'=', .(nc2), '; ', .(pc2), '%\t\t\t\t\t\t\t\tn'['c1']*'=', .(nc1), '; ', .(pc1), '%')), side=3, line=1)
   mtext(bquote(paste('n'['r2']*'=', .(nr2), '; ', .(pr2), '%\t\t\t\tn'['r1']*'=', .(nr1), '; ', .(pr1), '%')), side=4, line=1)

   points(avg1, col='red', type='p', pch=0)
   points(avg2, col='red', type='p', pch=0)
   points(avg3, col='red', type='p', pch=0)
   points(avg4, col='red', type='p', pch=0)

   abline(h=avg_diff, col='red', lty=2)
   abline(v=avg_perc, col='red', lty=2)
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
   print(paste('avg(q1)=', round(avg1$avg1_perc_avg,digits), ', ', round(avg1$avg1_perc_diff,digits), sep=''))
   print(paste('avg(q2)=', round(avg2$avg2_perc_avg,digits), ', ', round(avg2$avg2_perc_diff,digits), sep=''))
   print(paste('avg(q3)=', round(avg3$avg3_perc_avg,digits), ', ', round(avg3$avg3_perc_diff,digits), sep=''))
   print(paste('avg(q4)=', round(avg4$avg4_perc_avg,digits), ', ', round(avg4$avg4_perc_diff,digits), sep=''))
   print(paste('avg(total)=', round(avg_perc,digits), ', ', round(avg_diff,digits), sep=''))
}

}
