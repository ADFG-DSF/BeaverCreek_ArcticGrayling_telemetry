# Script 3: Discrete Movement
# Matt Tyers, August 2024

# The purpose of this script is to illustrate and summarize movement between
# river sections (lower, middle, upper, headwaters) between seasons, as well
# as movement between river designations (mainstem, tributary, headwaters).

# This script is motivated by the hope that aggregate patterns in seasonal 
# movement will be evident when simplifying the study area to discrete regions.

# section and designation were defined in Script 1, and can be modified there if desired.



source("R_code/1_beaver_cr_data.R")  # loading data

library(ggsankey)   # for Sankey plots

write_output <- FALSE  # whether to write figures & tables to external file



datelabels <- c("2021 Tagging",
                "2022 Overwintering",
                "2022 Spring Spawning",
                "2022 Oversummering",
                "2023 Overwintering",
                "2023 Spring Spawning",
                "2023 Oversummering")


# ------------------  showing section & designation -------------------- #

if(write_output) {
  png(filename="R_output/Section_labels.png",
      width=8, height=6, units="in", res=300)
}

sectionlabels <- c("Lower","Middle","Upper","Headwaters")
sectioncols <- rainbow(length(sectionlabels)) %>% 
  adjustcolor(red.f=.85, blue.f=.85, green.f=.85)

par(mfrow=c(1,2))
par(family="serif")
par(mar=c(2.1, 2.1, 4.1, 2.1))

plot(beaver_cr_op, empty=TRUE, ylim=c(1702000, 1845000), 
     main="Section", xaxt="n", yaxt="n")
sections <- as.numeric(as.factor(sectiontable$section))
for(i in 1:nrow(sectiontable)) {
  lines(beaver_cr_op$lines[[i]], 
        col=sectioncols[sections[i]],
        lwd=2)
}
# with(all_locs, riverpoints(seg=seg, vert=vert, rivers=beaver_cr_op,
#                            col=adjustcolor(1, alpha.f=.1)))
legend("topleft", lwd=2, col=sectioncols, legend=sectionlabels, cex=.7)


designationlabels <- c("Mainstem","Tributaries","Headwaters")
sectioncols <- rainbow(length(designationlabels)) %>% 
  adjustcolor(red.f=.85, blue.f=.85, green.f=.85)

plot(beaver_cr_op, empty=TRUE, ylim=c(1702000, 1845000), 
     main="Designation", xaxt="n", yaxt="n")
sections <- as.numeric(as.factor(sectiontable$mainstem))
for(i in 1:nrow(sectiontable)) {
  lines(beaver_cr_op$lines[[i]], 
        col=sectioncols[sections[i]],
        lwd=2)
}
# with(all_locs, riverpoints(seg=seg, vert=vert, rivers=beaver_cr_op,
#                            col=adjustcolor(1, alpha.f=.1)))
legend("topleft", lwd=2, col=sectioncols, legend=designationlabels, cex=.7)

if(write_output) {
  dev.off()
}


# ------------------ Sankey Plot: River Section -------------------- #

fulllevels <- rev(sectionlabels)

Sankey_bysection <- seasonal_locs_widelist$section %>%
  make_long(names(seasonal_locs_widelist$section)) %>%
  mutate(node = factor(node, levels=rev(sort(unique(node))))) %>%
  mutate(next_node = factor(next_node, levels=rev(sort(unique(next_node))))) %>%
  ggplot(aes(x = x,
             next_x = next_x,
             node = node,
             next_node = next_node,
             fill = node,
             label=node)) +
  geom_sankey(alpha=.7, type="sankey") +  # show.legend = F
  theme_bw() +
  theme(text=element_text(family="serif")) +
  scale_x_discrete(labels = str_wrap(datelabels, width=10)) +
  scale_y_continuous(breaks=NULL) +
  scale_fill_discrete(labels=rev(c("Lower","Middle","Upper","Headwaters"))) +
  guides(fill = guide_legend(reverse = TRUE))+
  # theme(axis.text.x=element_text(angle=90)) +
  labs(x="", fill="")

Sankey_bysection
if(write_output) ggsave(Sankey_bysection, filename="R_output/Section_Sankey.png",
                        height=6, width=8, units="in")

# ------------------ Sankey Plot: Designation -------------------- #

Sankey_bydesignation <- seasonal_locs_widelist$mainstem %>%
  make_long(names(seasonal_locs_widelist$mainstem)) %>%
  mutate(node = factor(node, levels=rev(sort(unique(node))))) %>%
  mutate(next_node = factor(next_node, levels=rev(sort(unique(next_node))))) %>%
  ggplot(aes(x = x,
             next_x = next_x,
             node = node,
             next_node = next_node,
             fill = node,
             label=node)) +
  geom_sankey(alpha=.7, type="sankey") +  # show.legend = F
  theme_bw() +
  theme(text=element_text(family="serif")) +
  scale_x_discrete(labels = str_wrap(datelabels, width=10)) +
  scale_y_continuous(breaks=NULL) +
  scale_fill_discrete(labels=rev(c("Mainstem","Tributaries","Headwaters"))) +
  guides(fill = guide_legend(reverse = TRUE))+
  # theme(axis.text.x=element_text(angle=90)) +
  labs(x="", fill="")

Sankey_bydesignation
if(write_output) ggsave(Sankey_bydesignation, filename="R_output/Designation_Sankey.png",
                        height=6, width=8, units="in")


# ------------------ Discrete Time-series Plot: River Section -------------------- #

if(write_output) {
  png(filename="R_output/Section_DiscreteTS.png",
      width=8, height=7, units="in", res=300)
}

par(mfrow=c(1,1))

sectionmatraw <- as.data.frame(seasonal_locs_widelist$section)
sectionmat <- matrix(nrow=nrow(sectionmatraw),
                     ncol=ncol(sectionmatraw))
for(j in 1:ncol(sectionmat)) {
  sectionmat[,j] <- as.numeric(factor(sectionmatraw[,j], 
                                      levels=rev(c("1_Lower","2_Middle","3_Upper","4_Headwaters"))))
}
offset <- order(rowMeans(sectionmat))/200 - .25
par(family="serif")
parmar <- par("mar")
par(mar=c(9,6,4,1)+.1)
plot(NA, xlim=c(1,ncol(sectionmat)),
     ylim=c(0.5, length(fulllevels)+.5),
     yaxt="n", xaxt="n",
     xlab="", ylab="",
     yaxs="i")
for(i in 1:nrow(sectionmat)) {
  lines(as.numeric(sectionmat[i,])+offset[i], col=adjustcolor(4, alpha.f=.5))
}
abline(h=seq(.5,9.5,by=1), lty=1)

axis(side=1, at=1:ncol(sectionmat), labels=datelabels, las=2)
axis(side=2, at=1:length(fulllevels), labels=fulllevels, las=2)

for(j in 1:ncol(sectionmat)) text(x=rep(j,length(fulllevels)), y=1:length(fulllevels), labels=table(sectionmat[,j]), cex=.7, font=2)

if(write_output) {
  dev.off()
}


# ------------------ Discrete Time-series Plot: Mainstem -------------------- #

if(write_output) {
  png(filename="R_output/Designation_DiscreteTS.png",
      width=8, height=7, units="in", res=300)
}

par(mfrow=c(1,1))

mainstemmatraw <- as.data.frame(seasonal_locs_widelist$mainstem)
mainstemmat <- matrix(nrow=nrow(mainstemmatraw),
                     ncol=ncol(mainstemmatraw))
fulllevels <- rev(c("Mainstem","Tributaries","Headwaters"))
for(j in 1:ncol(mainstemmat)) {
  mainstemmat[,j] <- as.numeric(factor(mainstemmatraw[,j], 
                                      levels=rev(c("1_Mainstem","2_Trib","3_Headwaters"))))
}
offset <- order(rowMeans(mainstemmat))/200 - .25
par(family="serif")
parmar <- par("mar")
par(mar=c(9,6,4,1)+.1)
plot(NA, xlim=c(1,ncol(mainstemmat)),
     ylim=c(0.5, length(fulllevels)+.5),
     yaxt="n", xaxt="n",
     xlab="", ylab="",
     yaxs="i")
for(i in 1:nrow(mainstemmat)) {
  lines(as.numeric(mainstemmat[i,])+offset[i], col=adjustcolor(4, alpha.f=.5))
}
abline(h=seq(.5,9.5,by=1), lty=1)

axis(side=1, at=1:ncol(mainstemmat), labels=datelabels, las=2)
axis(side=2, at=1:length(fulllevels), labels=fulllevels, las=2)

for(j in 1:ncol(mainstemmat)) text(x=rep(j,length(fulllevels)), y=1:length(fulllevels), labels=table(mainstemmat[,j]), cex=.7, font=2)

if(write_output) {
  dev.off()
}



# ------------------ making some quick tables for the same info ------------------ #
tab1 <- with(seasonal_locs, table(Season, section)) 
season_section <- data.frame(n = rowSums(tab1),
                         n_Lower = tab1[,1],
                         n_Middle = tab1[,2],
                         n_Upper = tab1[,3],
                         n_Headwaters = tab1[,4])
p_Lower <- with(season_section, n_Lower/n)
p_Middle <- with(season_section, n_Middle/n)
p_Upper <- with(season_section, n_Upper/n)
p_Headwaters <- with(season_section, n_Headwaters/n)
se_Lower <- sqrt(p_Lower*(1-p_Lower)/(season_section$n))
se_Middle <- sqrt(p_Middle*(1-p_Middle)/(season_section$n))
se_Upper <- sqrt(p_Upper*(1-p_Upper)/(season_section$n))
se_Headwaters <- sqrt(p_Headwaters*(1-p_Headwaters)/(season_section$n))
season_section$p_Lower <- paste0(round(p_Lower, 2), " (",
                                round(se_Lower, 2), ")")
season_section$p_Middle <- paste0(round(p_Middle, 2), " (",
                                  round(se_Middle, 2), ")")
season_section$p_Upper <- paste0(round(p_Upper, 2), " (",
                                 round(se_Upper, 2), ")") 
season_section$p_Headwaters <- paste0(round(p_Headwaters, 2), " (",
                                  round(se_Headwaters, 2), ")")
rownames(season_section) <- datelabels
season_section
if(write_output){
  write.csv(season_section, file="R_output/Tables/Section_bySeason.csv")
}


tab1 <- with(seasonal_locs, table(Season, mainstem)) 
season_des <- data.frame(n = rowSums(tab1),
                         n_Mainstem = tab1[,1],
                         n_Tributary = tab1[,2],
                         n_Headwaters = tab1[,3])
p_Mainstem <- with(season_des, n_Mainstem/n)
p_Tributary <- with(season_des, n_Tributary/n)
p_Headwaters <- with(season_des, n_Headwaters/n)
se_Mainstem <- sqrt(p_Mainstem*(1-p_Mainstem)/(season_des$n))
se_Tributary <- sqrt(p_Tributary*(1-p_Tributary)/(season_des$n))
se_Headwaters <- sqrt(p_Headwaters*(1-p_Headwaters)/(season_des$n))
season_des$p_Mainstem <- paste0(round(p_Mainstem, 2), " (",
                                round(se_Mainstem, 2), ")")
season_des$p_Tributary <- paste0(round(p_Tributary, 2), " (",
                                 round(se_Tributary, 2), ")")
season_des$p_Headwaters <- paste0(round(p_Headwaters, 2), " (",
                                  round(se_Headwaters, 2), ")")
rownames(season_des) <- datelabels
season_des
if(write_output){
  write.csv(season_des, file="R_output/Tables/Designation_bySeason.csv")
}



tab1 <- with(all_locs, table(SurveyDate, section)) 
rownames(tab1)[1] <- "Tagging"
survey_section <- data.frame(n = rowSums(tab1),
                             n_Lower = tab1[,1],
                             n_Middle = tab1[,2],
                             n_Upper = tab1[,3],
                             n_Headwaters = tab1[,4])
p_Lower <- with(survey_section, n_Lower/n)
p_Middle <- with(survey_section, n_Middle/n)
p_Upper <- with(survey_section, n_Upper/n)
p_Headwaters <- with(survey_section, n_Headwaters/n)
se_Lower <- sqrt(p_Lower*(1-p_Lower)/(survey_section$n))
se_Middle <- sqrt(p_Middle*(1-p_Middle)/(survey_section$n))
se_Upper <- sqrt(p_Upper*(1-p_Upper)/(survey_section$n))
se_Headwaters <- sqrt(p_Headwaters*(1-p_Headwaters)/(survey_section$n))
survey_section$p_Lower <- paste0(round(p_Lower, 2), " (",
                                 round(se_Lower, 2), ")")
survey_section$p_Middle <- paste0(round(p_Middle, 2), " (",
                                  round(se_Middle, 2), ")")
survey_section$p_Upper <- paste0(round(p_Upper, 2), " (",
                                 round(se_Upper, 2), ")") 
survey_section$p_Headwaters <- paste0(round(p_Headwaters, 2), " (",
                                      round(se_Headwaters, 2), ")")
survey_section
if(write_output){
  write.csv(survey_section, file="R_output/Tables/Section_bySurvey.csv")
}


tab1 <- with(all_locs, table(SurveyDate, mainstem)) 
rownames(tab1)[1] <- "Tagging"
survey_des <- data.frame(n = rowSums(tab1),
                         n_Mainstem = tab1[,1],
                         n_Tributary = tab1[,2],
                         n_Headwaters = tab1[,3])
p_Mainstem <- with(survey_des, n_Mainstem/n)
p_Tributary <- with(survey_des, n_Tributary/n)
p_Headwaters <- with(survey_des, n_Headwaters/n)
se_Mainstem <- sqrt(p_Mainstem*(1-p_Mainstem)/(survey_des$n))
se_Tributary <- sqrt(p_Tributary*(1-p_Tributary)/(survey_des$n))
se_Headwaters <- sqrt(p_Headwaters*(1-p_Headwaters)/(survey_des$n))
survey_des$p_Mainstem <- paste0(round(p_Mainstem, 2), " (",
                                round(se_Mainstem, 2), ")")
survey_des$p_Tributary <- paste0(round(p_Tributary, 2), " (",
                                 round(se_Tributary, 2), ")")
survey_des$p_Headwaters <- paste0(round(p_Headwaters, 2), " (",
                                  round(se_Headwaters, 2), ")")
survey_des
if(write_output){
  write.csv(survey_des, file="R_output/Tables/Designation_bySurvey.csv")
}

