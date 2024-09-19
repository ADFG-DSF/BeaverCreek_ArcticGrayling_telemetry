source("R_code/1_beaver_cr_data.R")  # loading data

write_output <- FALSE  # whether to write figures & tables to external file



### trying kernel density thing, doesn't do much
dens1 <- with(seasonal_locs, 
              makeriverdensity(seg=seg, vert=vert, survey=Season,
                               rivers=beaver_cr_op,
                               # kernel = "rect", 
                               bw=10000))
mains <- c("2021 Tagging",   "2022 Overwintering",   "2022 Spring Spawning", "2022 Oversummering",  
           "2023 Overwintering",   "2023 Spring Spawning", "2023 Oversummering")

if(write_output) {
  png(filename="R_output/Kernel_density.png",
      width=9, height=10, units="in", res=300)
}
par(mfrow=c(2,3))
par(mar=c(2,2,4,1))
par(family="serif")
plot(dens1, whichplots=2:length(dens1),
     scalebyN = FALSE, main=mains, yaxt="n", xaxt="n", scalebar=FALSE)
if(write_output) {
  dev.off()
}


# calculate difference from avg density, create new density object!
# NOTE: dens1 now has tagging, which we don't want, so we want 
# $densities objects 2-7 instead of 1-6
nseason <- unname(table(seasonal_locs$Season))
weights <- mean(nseason)/nseason
meandens <- list()
for(i_segment in 1:length(dens1$densities[[1]])) {
  meandens[[i_segment]] <- NA
  for(i_vertex in 1:length(dens1$densities[[1]][[i_segment]])) {
    meandens[[i_segment]][i_vertex] <- mean(c(dens1$densities[[7]][[i_segment]][i_vertex],
                                            dens1$densities[[2]][[i_segment]][i_vertex],
                                            dens1$densities[[3]][[i_segment]][i_vertex],
                                            dens1$densities[[4]][[i_segment]][i_vertex],
                                            dens1$densities[[5]][[i_segment]][i_vertex],
                                            dens1$densities[[6]][[i_segment]][i_vertex])*
                                              weights)
  }
}
# trialdens <- dens1
# trialdens$densities <- list(meandens)
# trialdens$survey <- rep(1,length(trialdens$survey))
# plot(trialdens)

densitydiffs_positive <- dens1
densitydiffs_negative <- dens1
for(i_survey in 1:length(dens1$densities)) {
  for(i_segment in 1:length(dens1$densities[[1]])) {
    for(i_vertex in 1:length(dens1$densities[[1]][[i_segment]])) {
      thediff <- dens1$densities[[i_survey]][[i_segment]][i_vertex]*weights[i_survey] -
        meandens[[i_segment]][i_vertex]
      densitydiffs_negative$densities[[i_survey]][[i_segment]][i_vertex] <-
        ifelse(thediff < 0, -thediff, 0)
      densitydiffs_positive$densities[[i_survey]][[i_segment]][i_vertex] <-
        ifelse(thediff > 0, thediff, 0)
        
    }
  }
}

if(write_output) {
  png(filename="R_output/Kernel_density_NegAnomaly.png",
      width=9, height=10, units="in", res=300)
}
par(mfrow=c(2,3))
par(mar=c(2,2,4,1))
par(family="serif")
plot(densitydiffs_negative, whichplots=2:length(dens1),
     ramp="blue", linecol = "grey", 
     main=mains, yaxt="n", xaxt="n", scalebar=FALSE)#, scalebyN = FALSE)
if(write_output) {
  dev.off()
}

if(write_output) {
  png(filename="R_output/Kernel_density_PosAnomaly.png",
      width=9, height=10, units="in", res=300)
}
par(mfrow=c(2,3))
par(mar=c(2,2,4,1))
par(family="serif")
plot(densitydiffs_positive, whichplots=2:length(dens1),
     ramp="red", linecol = "grey", 
     main=mains, yaxt="n", xaxt="n", scalebar=FALSE)#, scalebyN = FALSE)
if(write_output) {
  dev.off()
}




### for each sequence of seasons, plot upstream movement as blue, downstream as red
x <- seasonal_locs_widelist$snap_x
y <- seasonal_locs_widelist$snap_y
# seg <- seasonal_locs_widelist$seg
# vert <- seasonal_locs_widelist$vert
ups <- seasonal_locs_widelist$upstream_km
mains <- c("2021 Tagging to 2022 Overwintering",
           "2022 Overwintering to Spring Spawning",
           "2022 Spring Spawning to Oversummering",
           "2022 Oversummering to 2023 Overwintering",
           "2023 Overwintering to Spring Spawning",
           "2023 Spring Spawning to Oversummering")

if(write_output) {
  png(filename="R_output/Direction_Arrows.png",
      width=9, height=10, units="in", res=300)
}
par(mfrow=c(2,3))
par(mar=c(2,2,4,2))
par(family="serif")
for(j in 1:6) {
  plot(beaver_cr_op, empty = TRUE, linecol="grey", main=mains[j], yaxt="n", xaxt="n")
  # for(i in 1:nrow(segs)) {
  #   points(seg = segs[i, j+0:1], vert = verts[i, j+0:1])
  # }
  points(x=x[,j], y=y[,j])
  points(x=x[,j+1], y=y[,j+1])
  arrows(x0=x[,j], x1=x[,j+1],
           y0=y[,j], y1=y[,j+1],
           col=adjustcolor(ifelse(ups[,j] < ups[,j+1], 4, 2), alpha.f=.7),
         length=0.1, lwd=2)
  legend("topleft", lwd=2, col=adjustcolor(c(2,4), alpha.f=0.7), legend=c("Downstream","Upstream"))
}
if(write_output) {
  dev.off()
}

