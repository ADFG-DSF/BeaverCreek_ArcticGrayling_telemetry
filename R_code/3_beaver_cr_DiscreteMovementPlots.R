source("R_code/1_beaver_cr_data.R")

library(ggsankey)   # for Sankey plots



datelabels <- c("2022 Overwintering",
                "2022 Spring Spawning",
                "2022 Oversummering",
                "2023 Overwintering",
                "2023 Spring Spawning",
                "2023 Oversummering")


# ------------------ Sankey Plot: River Section -------------------- #

fulllevels <- rev(c("Lower","Middle","Upper","Headwaters"))

seasonal_locs_widelist$section[,-1] %>%
  make_long(names(seasonal_locs_widelist$section[,-1])) %>%
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
  scale_x_discrete(labels=datelabels) +
  scale_y_continuous(breaks=NULL) +
  scale_fill_discrete(labels=rev(c("Lower","Middle","Upper","Headwaters"))) +
  guides(fill = guide_legend(reverse = TRUE))+
  theme(axis.text.x=element_text(angle=90)) +
  labs(x="", fill="")

# ------------------ Sankey Plot: Mainstem -------------------- #

seasonal_locs_widelist$mainstem[,-1] %>%
  make_long(names(seasonal_locs_widelist$mainstem[,-1])) %>%
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
  scale_x_discrete(labels=datelabels) +
  scale_y_continuous(breaks=NULL) +
  scale_fill_discrete(labels=rev(c("Mainstem","Tributaries","Headwaters"))) +
  guides(fill = guide_legend(reverse = TRUE))+
  theme(axis.text.x=element_text(angle=90)) +
  labs(x="", fill="")



# ------------------ Discrete Time-series Plot: River Section -------------------- #

sectionmatraw <- as.data.frame(seasonal_locs_widelist$section[,-1])
sectionmat <- matrix(nrow=nrow(sectionmatraw),
                     ncol=ncol(sectionmatraw))
for(j in 1:ncol(sectionmat)) {
  sectionmat[,j] <- as.numeric(factor(sectionmatraw[,j], 
                                      levels=rev(c("1_Lower","2_Middle","3_Upper","4_Headwaters"))))
}
offset <- order(rowMeans(sectionmat))/200 - .25
par(family="serif")
parmar <- par("mar")
par(mar=c(9,8,4,1)+.1)
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



# ------------------ Discrete Time-series Plot: Mainstem -------------------- #

mainstemmatraw <- as.data.frame(seasonal_locs_widelist$mainstem[,-1])
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
par(mar=c(9,8,4,1)+.1)
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
