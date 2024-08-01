source("R_code/1_beaver_cr_data.R")


write_output <- FALSE  # whether to write figures & tables to external file



## for all_locs & seasonal_locs SEPARATELY:

# calculate sequential upstream distances (pairwise)

upstreamseq_all <- with(all_locs, 
                        upstreamseq(unique = Fish,
                                    survey = SurveyDate,
                                    seg = seg, vert = vert,
                                    rivers = beaver_cr_op))/1000
upstreamseq_seasonal <- with(seasonal_locs, 
                        upstreamseq(unique = Fish,
                                    survey = Season,
                                    seg = seg, vert = vert,
                                    rivers = beaver_cr_op))/1000
names(upstreamseq_seasonal) <- c("2022 Overwintering to Spring Spawning",
                                 "2022 Spring Spawning to Oversummering",
                                 "2022 Oversummering to 2023 Overwintering",
                                 "2023 Overwintering to Spring Spawning",
                                 "2023 Spring Spawning to Oversummering")


# # plot sequential upstream distances (pairwise)
# plotseq(upstreamseq_all)
# plotseq(upstreamseq_seasonal)

directional_dist_bysurvey <- upstreamseq_all %>%
  pivot_longer(cols = names(upstreamseq_all), 
               values_to = "Distance_km", 
               names_to = "interval") %>%
  ggplot(aes(x = interval, y = Distance_km)) +
  geom_boxplot() +
  labs(x="", y="Upstream Distance (km)") +
  theme_bw() +
  theme(text=element_text(family="serif")) +
  # scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
  theme(axis.text.x=element_text(angle=60, hjust=1)) 

directional_dist_bysurvey
if(write_output) {
  ggsave(directional_dist_bysurvey, 
         filename="R_output/directional_dist_bysurvey.png",
         height=6, width=8, units="in")
}


directional_dist_byseason <- upstreamseq_seasonal %>%
  pivot_longer(cols = names(upstreamseq_seasonal), 
               values_to = "Distance_km", 
               names_to = "interval") %>%
  mutate(interval = factor(interval, levels = names(upstreamseq_seasonal))) %>%
  ggplot(aes(x = interval, y = Distance_km)) +
  geom_boxplot() +
  labs(x="", y="Upstream Distance (km)") +
  theme_bw() +
  theme(text=element_text(family="serif")) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
  # theme(axis.text.x=element_text(angle=60, hjust=1))

directional_dist_byseason
if(write_output) {
  ggsave(directional_dist_byseason, 
         filename="R_output/directional_dist_byseason.png",
         height=5, width=7, units="in")
}


# table: n, mean, sd, se, proportions up/down
upstreamseq_all_summary <- data.frame(n = apply(upstreamseq_all, 2, \(x) sum(!is.na(x))),
                                      Mean = colMeans(upstreamseq_all, na.rm=TRUE),
                                      SD = apply(upstreamseq_all, 2, sd, na.rm=TRUE),
                                      `SE(Mean)` = apply(upstreamseq_all, 2, se, na.rm=TRUE),
                                      n_up = colSums(upstreamseq_all > 0, na.rm=TRUE),
                                      n_down = colSums(upstreamseq_all < 0, na.rm=TRUE))
p_up <- with(upstreamseq_all_summary, n_up/n)
p_down <- with(upstreamseq_all_summary, n_down/n)
se_p_up <- sqrt(p_up*(1-p_up)/(upstreamseq_all_summary$n-1))
se_p_down <- sqrt(p_down*(1-p_down)/(upstreamseq_all_summary$n-1))
upstreamseq_all_summary$p_up <- paste0(round(p_up, 2), " (",
                                       round(se_p_up, 2), ")")
upstreamseq_all_summary$p_down <- paste0(round(p_down, 2), " (",
                                       round(se_p_down, 2), ")")

upstreamseq_seasonal_summary <- data.frame(n = apply(upstreamseq_seasonal, 2, \(x) sum(!is.na(x))),
                                      Mean = colMeans(upstreamseq_seasonal, na.rm=TRUE),
                                      SD = apply(upstreamseq_seasonal, 2, sd, na.rm=TRUE),
                                      `SE(Mean)` = apply(upstreamseq_seasonal, 2, se, na.rm=TRUE),
                                      n_up = colSums(upstreamseq_seasonal > 0, na.rm=TRUE),
                                      n_down = colSums(upstreamseq_seasonal < 0, na.rm=TRUE))
p_up <- with(upstreamseq_seasonal_summary, n_up/n)
p_down <- with(upstreamseq_seasonal_summary, n_down/n)
se_p_up <- sqrt(p_up*(1-p_up)/(upstreamseq_seasonal_summary$n-1))
se_p_down <- sqrt(p_down*(1-p_down)/(upstreamseq_seasonal_summary$n-1))
upstreamseq_seasonal_summary$p_up <- paste0(round(p_up, 2), " (",
                                       round(se_p_up, 2), ")")
upstreamseq_seasonal_summary$p_down <- paste0(round(p_down, 2), " (",
                                         round(se_p_down, 2), ")")

if(write_output) {
  write.csv(upstreamseq_all_summary, file = "R_output/directional_dist_bysurvey.csv")
  write.csv(upstreamseq_seasonal_summary, file = "R_output/directional_dist_byseason.csv")
}



mosaicplot(upstreamseq_all_summary[c("n_up","n_down")])
mosaicplot(upstreamseq_seasonal_summary[c("n_up","n_down")])

# refine these better, maybe make them their own file
plotseq(as.data.frame(all_locs_widelist$upstream_km[,-1]), type="dotline")
plotseq(as.data.frame(seasonal_locs_widelist$upstream_km[,-1]), type="dotline")



# calculate homeranges by individual
# plot the big mover(s)

# plot upstream position

# kernel density plots
# kernel density change??


# calculate cumulative distance by individual

# by-individual medians/modes: section, upstream_km

# cumulative dist, homerange ~ sectionmode + upstream_mn + length (ish)

# dropouts???  mortality?? -> look into this before asking about it