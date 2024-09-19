# Script 5: By-Individual Analysis
# Matt Tyers, August 2024

# The purpose of this script is to look for relationships between individual-level
# variables (using the dataframe constructed in Script 4).  

# THIS IS NOT INTENDED AS BIOMETRICALLY ROBUST YET.  Rather, the output from this
# script should be interpreted as an exploratory data analysis (EDA), in which
# all reasonable pairwise relationships between variables are VISUALIZED.

# In addition to automated EDA plots, seasonal fidelity is explored and visualized
# using empirical CDF plots, and a summary table is created.


source("R_code/4_beaver_cr_ByIndividual.R")

write_output <- FALSE  # whether to write figures & tables to external file


summary(by_indiv)

# simplifying categories? what to do with fish with multiple cats, eg Headwaters/Upper
by_indiv$winter_section_orig <- by_indiv$winter_section
by_indiv$spring_section_orig <- by_indiv$spring_section
by_indiv$summer_section_orig <- by_indiv$summer_section
by_indiv$winter_designation_orig <- by_indiv$winter_designation
by_indiv$spring_designation_orig <- by_indiv$spring_designation
by_indiv$summer_designation_orig <- by_indiv$summer_designation

# simplifying categories, take 1: 
# defining fish with multiple cats as the lesser-observed category
# don't know if this is appropriate but it's worth a try
sorter <- function(x) {
  for(icat in sort(unique(x))) {
    trysplit <- strsplit(icat, split="/")[[1]]
    if(length(trysplit) > 1) {
      firsts <- sum(x==trysplit[1], na.rm=TRUE)
      seconds <- sum(x==trysplit[2], na.rm=TRUE)
      if(firsts < seconds) {
        x[x==icat] <- trysplit[1]
      } else {
        x[x==icat] <- trysplit[2]
      }
    }
  }
  return(x)
}
# table(by_indiv$winter_section)
# table(sorter(x=by_indiv$winter_section))

by_indiv$winter_section <- sorter(by_indiv$winter_section)
by_indiv$spring_section <- sorter(by_indiv$spring_section)
by_indiv$summer_section <- sorter(by_indiv$summer_section)
by_indiv$winter_designation <- sorter(by_indiv$winter_designation)
by_indiv$spring_designation <- sorter(by_indiv$spring_designation)
by_indiv$summer_designation <- sorter(by_indiv$summer_designation)



## initial plot of all variables with respect to one another
plot(by_indiv[,-c(1,3,19:24)])


## a first try at more appropriate plots of variables wrt one another
numerics <- c("numeric","integer","array")
factors <- c("character","factor")
magicplot <- function(x,y,...) {
  if((class(x) %in% numerics) & (class(y) %in% numerics)) {
    plot(x,y,...=...)
  }
  if(class(x) %in% factors & (class(y) %in% numerics)) {
    boxplot(y ~ x,...=...)
  }
  if((class(x) %in% numerics) & class(y) %in% factors) {
    # do nothing
  }
  if(class(x) %in% factors & class(y) %in% factors) {
    mosaicplot(table(x,y),...=...)
  }
}

dothese <- c(2, 4:18)
for(j in dothese) {
  par(mfrow=c(4,4))
  for(i in dothese) {
    if(i != j) {
      magicplot(by_indiv[,i], by_indiv[,j], 
                xlab=names(by_indiv)[i], ylab=names(by_indiv)[j])
    }
  }
}

par(mfrow=c(4,4))
for(i in dothese) {
  if(class(by_indiv[,i]) %in% numerics) {
    hist(by_indiv[,i], main=names(by_indiv)[i])
  }
}
hist(by_indiv$n_surveys, main="nsurveys")
hist(by_indiv$cumuldist_km/(by_indiv$n_surveys-1), main="cumuldist/survey")
hist(by_indiv$winter22winter23_km/by_indiv$homerange_km, main="winter/hr")
hist(by_indiv$spring22spring23_km/by_indiv$homerange_km, main="spring/hr")
hist(by_indiv$summer22summer23_km/by_indiv$homerange_km, main="summer/hr")


# par(mfrow=c(2,3))
# with(seasonal_locs, kfunc(seg=seg, vert=vert, survey=Season,
#                           rivers=beaver_cr_op, maxdist=15*1000))



# Plots of empirical CDF of seasonal fidelity (expressed as distance)

if(write_output) {
  png(filename="R_output/FidelityCDF_allsummer.png",
      width=8, height=6, units="in", res=300)
}
par(mfrow=c(1,1))
par(family="serif")
plot(ecdf(by_indiv$winter22winter23_km), col=4,
     xlim = c(0,80), ylim=c(0,1.05),
     xlab="Distance (rkm)", ylab="Cumulative Density", main="")
lines(ecdf(by_indiv$spring22spring23_km), col=3)
lines(ecdf(by_indiv$summer22summer23_km), col=2)
lines(ecdf(by_indiv$summer21summer22_km), col=2, lty=2)
lines(ecdf(by_indiv$summer21summer23_km), col=2, lty=3)
legend(
  x=48, y=0.85,
  # "bottomright", 
  lwd=1, pch=16, col=c(4,3,2,2,2), lty=c(1,1,2,1,3),
       legend=c("Winter 2022 - Winter 2023", 
                "Spring 2022 - Spring 2023", 
                "Summer 2021 - Summer 2022", 
                "Summer 2022 - Summer 2023", 
                "Summer 2021 - Summer 2023"))
lines(rep(3,2),0:1)
lines(rep(15,2),0:1)
text(c(3,15), rep(1,2), labels=paste(c(3,15),"rkm"), pos=3)
if(write_output) {
  dev.off()
}



# Plots of empirical CDF of seasonal fidelity (expressed as fraction of homerange)

if(write_output) {
  png(filename="R_output/FidelityCDF_2_allsummer.png",
      width=8, height=6, units="in", res=300)
}
par(mfrow=c(1,1))
par(family="serif")
plot(ecdf(by_indiv$winter22winter23_km/by_indiv$homerange_km), col=4,
     xlim = c(0,1), ylim=c(0,1.05),
     xlab="Distance (rkm) / Homerange (rkm)", ylab="Cumulative Density", main="")
lines(ecdf(by_indiv$spring22spring23_km/by_indiv$homerange_km), col=3)
lines(ecdf(by_indiv$summer22summer23_km/by_indiv$homerange_km), col=2)
lines(ecdf(by_indiv$summer21summer22_km/by_indiv$homerange_km), col=2, lty=2)
lines(ecdf(by_indiv$summer21summer23_km/by_indiv$homerange_km), col=2, lty=3)
legend(
  x=.61, y=0.65,
  # "bottomright", 
  lwd=1, pch=16, col=c(4,3,2,2,2), lty=c(1,1,2,1,3),
  legend=c("Winter 2022 - Winter 2023", 
           "Spring 2022 - Spring 2023", 
           "Summer 2021 - Summer 2022", 
           "Summer 2022 - Summer 2023", 
           "Summer 2021 - Summer 2023"))
lines(rep(0.05,2),0:1)
lines(rep(.2,2),0:1)
text(c(.05,.2), rep(1,2), labels=paste(c(.05,.2)*100,"%"), pos=3)
if(write_output) {
  dev.off()
}



# Constructing a summary table of fidelity

fidelity_tab <- 
  data.frame(med_dist_km = 
               c(median(by_indiv$winter22winter23_km, na.rm=TRUE),
                 median(by_indiv$spring22spring23_km, na.rm=TRUE),
                 median(by_indiv$summer21summer22_km, na.rm=TRUE),
                 median(by_indiv$summer22summer23_km, na.rm=TRUE),
                 median(by_indiv$summer21summer23_km, na.rm=TRUE)),
             prop_lessthan0.5 =
               c(mean(by_indiv$winter22winter23_km < 0.5, na.rm=TRUE),
                 mean(by_indiv$spring22spring23_km < 0.5, na.rm=TRUE),
                 mean(by_indiv$summer21summer22_km < 0.5, na.rm=TRUE),
                 mean(by_indiv$summer22summer23_km < 0.5, na.rm=TRUE),
                 mean(by_indiv$summer21summer23_km < 0.5, na.rm=TRUE)),
             prop_lessthan3 =
               c(mean(by_indiv$winter22winter23_km < 3, na.rm=TRUE),
                 mean(by_indiv$spring22spring23_km < 3, na.rm=TRUE),
                 mean(by_indiv$summer21summer22_km < 3, na.rm=TRUE),
                 mean(by_indiv$summer22summer23_km < 3, na.rm=TRUE),
                 mean(by_indiv$summer21summer23_km < 3, na.rm=TRUE)),
             prop_lessthan15 =
               c(mean(by_indiv$winter22winter23_km < 15, na.rm=TRUE),
                 mean(by_indiv$spring22spring23_km < 15, na.rm=TRUE),
                 mean(by_indiv$summer21summer22_km < 15, na.rm=TRUE),
                 mean(by_indiv$summer22summer23_km < 15, na.rm=TRUE),
                 mean(by_indiv$summer21summer23_km < 15, na.rm=TRUE)),
             med_dist_p_hr = 
               c(median(by_indiv$winter22winter23_km/by_indiv$homerange_km, na.rm=TRUE),
                 median(by_indiv$spring22spring23_km/by_indiv$homerange_km, na.rm=TRUE),
                 median(by_indiv$summer21summer22_km/by_indiv$homerange_km, na.rm=TRUE),
                 median(by_indiv$summer22summer23_km/by_indiv$homerange_km, na.rm=TRUE),
                 median(by_indiv$summer21summer23_km/by_indiv$homerange_km, na.rm=TRUE)),
             prop_lessthan5 =
               c(mean(by_indiv$winter22winter23_km/by_indiv$homerange_km < 0.05, na.rm=TRUE),
                 mean(by_indiv$spring22spring23_km/by_indiv$homerange_km < 0.05, na.rm=TRUE),
                 mean(by_indiv$summer21summer22_km/by_indiv$homerange_km < 0.05, na.rm=TRUE),
                 mean(by_indiv$summer22summer23_km/by_indiv$homerange_km < 0.05, na.rm=TRUE),
                 mean(by_indiv$summer21summer23_km/by_indiv$homerange_km < 0.05, na.rm=TRUE)))
rownames(fidelity_tab) <- c("Winter 2022 - Winter 2023",
                            "Spring 2022 - Spring 2023",
                            "Summer 2021 - Summer 2022",
                            "Summer 2022 - Summer 2023",
                            "Summer 2021 - Summer 2023")
colnames(fidelity_tab) <- c("Median Distance (rkm)", 
                            "Proportion < 0.5 rkm", 
                            "Proportion < 3 rkm", 
                            "Proportion < 15 rkm",
                            "Median (Distance / Homerange)", 
                            "Proportion < 5% of Homerange")
fidelity_tab
if(write_output) {
  write.csv(fidelity_tab, file="R_output/Tables/Fidelity_tab.csv")
}




# re-expressing fidelity (distance) as binned, calculated from distance

breaks <- c(0,0.5,3,15,100)
by_indiv$winterFid <- cut(by_indiv$winter22winter23_km, breaks=breaks, include.lowest = TRUE)
by_indiv$springFid <- cut(by_indiv$spring22spring23_km, breaks=breaks, include.lowest = TRUE)
by_indiv$summerFid_21_22 <- cut(by_indiv$summer21summer22_km, breaks=breaks, include.lowest = TRUE)
by_indiv$summerFid_22_23 <- cut(by_indiv$summer22summer23_km, breaks=breaks, include.lowest = TRUE)
by_indiv$summerFid_21_23 <- cut(by_indiv$summer21summer23_km, breaks=breaks, include.lowest = TRUE)

# looking at binned fidelity wrt other variables
these <- c(2, 4, 5, 9:18, 25:27)
for(i in 25:27) {
  par(mfrow=c(4,4))
  for(j in these) {
    if(i!=j) magicplot(x=by_indiv[,i], y=by_indiv[,j],
                       xlab=names(by_indiv)[i], ylab=names(by_indiv)[j])
  }
}



# re-expressing fidelity (distance) as binned, calculated from frac of homerange

breaks <- c(0,0.05,0.2,1)
by_indiv$winterFid2 <- cut(by_indiv$winter22winter23_km/by_indiv$homerange_km, 
                           breaks=breaks, include.lowest = TRUE)
by_indiv$springFid2 <- cut(by_indiv$spring22spring23_km/by_indiv$homerange_km, 
                           breaks=breaks, include.lowest = TRUE)
by_indiv$summerFid2_21_22 <- cut(by_indiv$summer21summer22_km/by_indiv$homerange_km, 
                           breaks=breaks, include.lowest = TRUE)
by_indiv$summerFid2_22_23 <- cut(by_indiv$summer22summer23_km/by_indiv$homerange_km, 
                           breaks=breaks, include.lowest = TRUE)
by_indiv$summerFid2_21_23 <- cut(by_indiv$summer22summer23_km/by_indiv$homerange_km, 
                           breaks=breaks, include.lowest = TRUE)

# looking at binned fidelity wrt other variables
these <- c(2, 4, 5, 9:18, 28:30)
for(i in 28:30) {
  par(mfrow=c(4,4))
  for(j in these) {
    if(i!=j) magicplot(x=by_indiv[,i], y=by_indiv[,j],
                       xlab=names(by_indiv)[i], ylab=names(by_indiv)[j])
  }
}



## FINAL VERSION - plotting all variables with respect to one another

magicplot2 <- function(x,y,xlab,ylab,logy) {
  if(class(x) %in% numerics & class(y) %in% numerics) {
    if(!logy) {
    plot(x, y, xlab=xlab, ylab=ylab,
         main = paste("Reg pval =", round(summary(lm(y~x))$coefficients[2,4], 4)))
    } else {
      y <- y+.01
      plot(x, y, xlab=xlab, ylab=ylab, log="y", las=1,
           main = paste("Reg pval =", round(summary(lm(log(y)~x))$coefficients[2,4], 4)))
    }
  }
  if(class(x) %in% factors & class(y) %in% numerics) {
    if(!logy) {
      boxplot(y ~ x, xlab=xlab, ylab=ylab,
         main = paste("Anova pval =", round(summary(lm(y~x))$coefficients[2,4], 4)))
    } else {
      y <- y+.01
      boxplot(y ~ x, xlab=xlab, ylab=ylab, log="y", las=1,
              main = paste("Anova pval =", round(summary(lm(log(y)~x))$coefficients[2,4], 4)))
    }
  }
  if(class(x) %in% numerics & class(y) %in% factors) {
    boxplot(x ~ y, xlab=ylab, ylab=xlab,
            main = paste("Anova pval =", round(summary(lm(x~y))$coefficients[2,4], 4)))
  }
  if(class(x) %in% factors & class(y) %in% factors) {
    thetab <- table(x,y)
    mosaicplot(thetab[,ncol(thetab):1], 
               xlab=xlab, ylab=ylab, col=grey.colors(ncol(thetab)),
            main = paste("chi^2 pval =", round(chisq.test(thetab)$p.value, 4)))
  }
}

names(by_indiv)
# we added columns 9 & 10, 30 & 31, 35 & 36

# simplifying categories to take up less space in plots
for(i in 9:11+2) {
  xx <- by_indiv[,i]
  xx[xx=="Lower"] <- "Lwr"
  xx[xx=="Middle"] <- "Mid"
  xx[xx=="Upper"] <- "Upr"
  xx[xx=="Headwaters"] <- "Head"
  by_indiv[,i] <- factor(xx, levels=c("Lwr","Mid","Upr","Head"))
}
for(i in 12:14+2) {
  xx <- by_indiv[,i]
  xx[xx=="Mainstem"] <- "Main"
  xx[xx=="Headwaters"] <- "Head"
  by_indiv[,i] <- factor(xx, levels=c("Main","Trib","Head"))
}



# defining which variables (by index) to plot as X vs Y
xid <- c(2,9:14+2,15:18+2)
yid <- c(4:(8+2),25:(30+4)+2)#,15:18

names(by_indiv)[xid]
names(by_indiv[yid])

# defining which Y variables (by index) to plot on log scale
logy <- rep(FALSE, ncol(by_indiv))
logy[4:(8+2)] <- TRUE

## actually making all the plots!
plotcount <- 1
for(j in yid) {
  if(write_output) {
    png(filename=paste0("R_output/EDA_",plotcount,".png"),
        width=10, height=7, units="in", res=100)
  }
  par(mfrow=c(3,4))
  for(i in xid) {
    magicplot2(x = by_indiv[,i], y = by_indiv[,j], logy=logy[j],
               xlab= names(by_indiv)[i], ylab=names(by_indiv)[j])
  }
  if(write_output) {
    dev.off()
  }
  plotcount <- plotcount + 1
}

if(write_output) {
  png(filename=paste0("R_output/EDA_",plotcount,".png"),
      width=10, height=7, units="in", res=100)
}
par(mfrow=c(3,4))
for(i in xid[-1]) {
  j <- xid[1]
  magicplot2(x = by_indiv[,i], y = by_indiv[,j], logy=logy[j],
             xlab= names(by_indiv)[i], ylab=names(by_indiv)[j])
}
if(write_output) {
  dev.off()
}




# > names(by_indiv)
# [1] "Fish"                    "Length_mm"               "n_surveys"              
# [4] "homerange_km"            "cumuldist_km"            "winter22winter23_km"    
# [7] "spring22spring23_km"     "summer21summer22_km"     "summer22summer23_km"    
# [10] "summer21summer23_km"     "winter_section"          "spring_section"         
# [13] "summer_section"          "winter_designation"      "spring_designation"     
# [16] "summer_designation"      "mn_upstream_km"          "mn_winter_upstream_km"  
# [19] "mn_spring_upstream_km"   "mn_summer_upstream_km"   "winter_section_orig"    
# [22] "spring_section_orig"     "summer_section_orig"     "winter_designation_orig"
# [25] "spring_designation_orig" "summer_designation_orig" "winterFid"              
# [28] "springFid"               "summerFid_21_22"         "summerFid_22_23"        
# [31] "summerFid_21_23"         "winterFid2"              "springFid2"             
# [34] "summerFid2_21_22"        "summerFid2_22_23"        "summerFid2_21_23" 