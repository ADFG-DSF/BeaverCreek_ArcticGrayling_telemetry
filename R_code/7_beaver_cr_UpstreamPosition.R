# Script 7: Upstream Position
# Matt Tyers, August 2024

# The purpose of this script is to visualize upstream position over the sequence
# of surveys, potentially as it relates to other categorical variables


source("R_code/5_beaver_cr_ByIndividual_analyses.R")

write_output <- FALSE  # whether to write figures & tables to external file




### first making a decent upstream distance plot by season and by survey
byseason <- seasonal_locs_widelist$upstream_km
bysurvey <- all_locs_widelist$upstream_km

plotseq2 <- function(mat, x=NULL, xax=NULL, lwd=NULL, col=NULL, ...) {
  if(is.null(x)) x <- 1:ncol(mat)
  if(is.null(lwd)) lwd <- rep(1, nrow(mat))
  if(is.null(col)) col <- rep(1, nrow(mat))
  plot(NA, xlim=range(x, na.rm=TRUE), ylim=range(mat, na.rm=T), 
       ylab="Upstream Position (rkm)", xlab="", xaxt="n",...=...)
  if(is.null(xax)) xax <- x
  axis(side=1, at=x, labels=xax, las=2)
  for(i in 1:nrow(mat)) {
    xplot <- x#jitter(x)
    points(xplot, mat[i,])
    lines(xplot, mat[i,], 
          col=adjustcolor(col[i], alpha.f=.4), 
          lwd=lwd[i])
    lines(xplot[!is.na(mat[i,])], mat[i,][!is.na(mat[i,])], 
          col=adjustcolor(col[i], alpha.f=.2), 
          lwd=lwd[i], lty=2)
  }
}


# ---- write the base plot to an external figure

par(mfrow=c(1,1))
xax <- sort(unique(all_locs$SurveyDate))

if(write_output) {
  png(filename="R_output/UpstreamPosition.png",
      width=10, height=7, units="in", res=300)
  par(family="serif")
  par(mar=c(6, 4, 4, 2))
}
plotseq2(bysurvey, xax=xax)
if(write_output) {
  dev.off()
}




### none of these do anything interesting!
plotseq2(bysurvey, xax=xax,
         col=as.numeric(as.factor(by_indiv$winter_section)))
legend("bottomright", legend=levels(by_indiv$winter_section), 
       col=1:length(levels(by_indiv$winter_section)), lty=1)

plotseq2(bysurvey, xax=xax,
         col=as.numeric(as.factor(by_indiv$spring_section)))
legend("bottomright", legend=levels(by_indiv$spring_section), 
       col=1:length(levels(by_indiv$spring_section)), lty=1)

plotseq2(bysurvey, xax=xax,
         col=as.numeric(as.factor(by_indiv$summer_section)))
legend("bottomright", legend=levels(by_indiv$summer_section), 
       col=1:length(levels(by_indiv$summer_section)), lty=1)

plotseq2(bysurvey, xax=xax,
         col=1+as.numeric(as.factor(by_indiv$winter_designation)))
legend("bottomright", legend=levels(by_indiv$winter_designation), 
       col=1+1:length(levels(by_indiv$winter_designation)), lty=1)

plotseq2(bysurvey, xax=xax,
         col=1+as.numeric(as.factor(by_indiv$spring_designation)))
legend("bottomright", legend=levels(by_indiv$spring_designation), 
       col=1+1:length(levels(by_indiv$spring_designation)), lty=1)

plotseq2(bysurvey, xax=xax,
         col=1+as.numeric(as.factor(by_indiv$summer_designation)))
legend("bottomright", legend=levels(by_indiv$summer_designation), 
       col=1+1:length(levels(by_indiv$summer_designation)), lty=1)



plotseq2(bysurvey, xax=xax,
         col=1+as.numeric(as.factor(by_indiv$winterFid)))
legend("bottomright", legend=levels(by_indiv$winterFid), 
       col=1+1:length(levels(by_indiv$winterFid)), lty=1)

plotseq2(bysurvey, xax=xax,
         col=1+as.numeric(as.factor(by_indiv$springFid)))
legend("bottomright", legend=levels(by_indiv$springFid), 
       col=1+1:length(levels(by_indiv$springFid)), lty=1)

plotseq2(bysurvey, xax=xax,
         col=1+as.numeric(as.factor(by_indiv$summerFid)))
legend("bottomright", legend=levels(by_indiv$summerFid), 
       col=1+1:length(levels(by_indiv$summerFid)), lty=1)



plotseq2(bysurvey, xax=xax,
         col=1+as.numeric(as.factor(by_indiv$winterFid2)))
legend("bottomright", legend=levels(by_indiv$winterFid2), 
       col=1+1:length(levels(by_indiv$winterFid2)), lty=1)

plotseq2(bysurvey, xax=xax,
         col=1+as.numeric(as.factor(by_indiv$springFid2)))
legend("bottomright", legend=levels(by_indiv$springFid2), 
       col=1+1:length(levels(by_indiv$springFid2)), lty=1)

plotseq2(bysurvey, xax=xax,
         col=1+as.numeric(as.factor(by_indiv$summerFid2)))
legend("bottomright", legend=levels(by_indiv$summerFid2), 
       col=1+1:length(levels(by_indiv$summerFid2)), lty=1)


# plotseq2(bysurvey, x=sort(unique(all_locs$SurveyDate)))

## would like to add colored kdens based on time of year, actually maybe not needed

plotseq2(byseason)
