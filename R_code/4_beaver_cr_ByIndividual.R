# Script 4: Constructing a summary table by individual
# Matt Tyers, August 2024

# The purpose of this script is to construct a summary dataframe, in which each
# row represents an individual tagged fish, with columns for variables related 
# to movement, location, and individual-level description.

# The intent is to create a dataset that can be used to relate patterns in 
# movement, geographic use, size, etc.

# The resulting dataset has the following columns:
#   Fish: Tag ID (formatted as number)
#   Length_mm: Length measured at tagging
#   n_surveys: Number of aerial surveys detected
#   homerange_km: Minimum observed homerange (river km)
#   cumuldist_km: Total cumulative distance (river km)
#   winterwinter_km: Distance between winter locations in 2022 and 2023 (river km)
#   springspring_km: Distance between spring locations in 2022 and 2023 (river km)
#   summersummer_km: Distance between summer locations in 2022 and 2023 (river km)
#   wintersection: River sections observed for winter surveys
#   springsection: River sections observed for spring surveys
#   summersection: River sections observed for summer surveys
#   winterdesignation: River designation (mainstem/trib/etc) observed for winter surveys
#   springdesignation: River designation (mainstem/trib/etc) observed for spring surveys
#   summerdesignation: River designation (mainstem/trib/etc) observed for summer surveys
#   mn_upstream_km: Mean distance from Yukon river confluence (river km) for all observations
#   mn_winter_upstream_km: Mean distance from Yukon river confluence (river km) for winter
#   mn_spring_upstream_km: Mean distance from Yukon river confluence (river km) for spring
#   mn_summer_upstream_km: Mean distance from Yukon river confluence (river km) for summer



source("R_code/1_beaver_cr_data.R")  # loading data

write_output <- FALSE  # whether to write figures & tables to external file



# ---- by individual:
# length
by_indiv <- tagging_data[c("Fish","Length_mm")]
rownames(by_indiv) <- tagging_data$Fish

# number of surveys
by_indiv$n_surveys <- table(all_locs$Fish)

# homerange (by survey)
hr <- with(all_locs, homerange(unique=Fish, survey=Survey, seg=seg, vert=vert, rivers=beaver_cr_op))
by_indiv$homerange_km <- NA
by_indiv$homerange_km[by_indiv$n_surveys > 1] <- hr$ranges$range/1000

# cumulative distance (by survey)
by_indiv$cumuldist_km <- NA
for(i in 1:nrow(by_indiv)) {
  # grab vectors of locations ONLY when observed
  segs <- all_locs_widelist$seg[i,][!is.na(all_locs_widelist$seg[i,])]
  verts <- all_locs_widelist$vert[i,][!is.na(all_locs_widelist$vert[i,])]
  if(length(segs) > 1) {
    by_indiv$cumuldist_km[i] <- 0
    for(j in 2:length(segs)) {
      by_indiv$cumuldist_km[i] <- by_indiv$cumuldist_km[i] +
        riverdistance(startseg = segs[j-1], endseg=segs[j],
                      startvert = verts[j-1], endvert=verts[j],
                      rivers = beaver_cr_op)/1000
    }
  }
}

by_indiv$winterwinter_km <- NA
by_indiv$springspring_km <- NA
by_indiv$summersummer_km <- NA
for(i in 1:nrow(seasonal_locs_widelist$seg)) {
  segs <- seasonal_locs_widelist$seg[i,]
  verts <- seasonal_locs_widelist$vert[i,]

  # winter-winter distance
  if(!is.na(segs$`1_2022_Overwintering`) & !is.na(segs$`4_2023_Overwintering`)) {
    by_indiv$winterwinter_km[by_indiv$Fish == rownames(seasonal_locs_widelist$seg)[i]] <-
      riverdistance(startseg=segs$`1_2022_Overwintering`,
                                                 endseg=segs$`4_2023_Overwintering`,
                                                 startvert=verts$`1_2022_Overwintering`,
                                                 endvert=verts$`4_2023_Overwintering`,
                                                 rivers=beaver_cr_op)/1000
  }

  # spring-spring distance
  if(!is.na(segs$`2_2022_Spring_Spawning`) & !is.na(segs$`5_2023_Spring_Spawning`)) {
    by_indiv$springspring_km[by_indiv$Fish == rownames(seasonal_locs_widelist$seg)[i]] <-
      riverdistance(startseg=segs$`2_2022_Spring_Spawning`,
                                                 endseg=segs$`5_2023_Spring_Spawning`,
                                                 startvert=verts$`2_2022_Spring_Spawning`,
                                                 endvert=verts$`5_2023_Spring_Spawning`,
                                                 rivers=beaver_cr_op)/1000
  }

  # summer-summer distance
  if(!is.na(segs$`3_2022_Oversummering`) & !is.na(segs$`6_2023_Oversummering`)) {
    by_indiv$summersummer_km[by_indiv$Fish == rownames(seasonal_locs_widelist$seg)[i]] <-
      riverdistance(startseg=segs$`3_2022_Oversummering`,
                                                 endseg=segs$`6_2023_Oversummering`,
                                                 startvert=verts$`3_2022_Oversummering`,
                                                 endvert=verts$`6_2023_Oversummering`,
                                                 rivers=beaver_cr_op)/1000
  }
}




# winter section
# winter designation
# spring section
# spring designation
# summer section
# summer designation
by_indiv$winter_section <- NA
by_indiv$spring_section <- NA
by_indiv$summer_section <- NA
by_indiv$winter_designation <- NA
by_indiv$spring_designation <- NA
by_indiv$summer_designation <- NA
collapser <- function(x) {
  ifelse(length(x) > 1, 
         paste(x, collapse="/"),
         ifelse(length(x) == 0, NA, x))
}
for(i in 1:nrow(seasonal_locs_widelist$seg)) {
  sec <- substr(seasonal_locs_widelist$section[i,], 3,99)
  des <- substr(seasonal_locs_widelist$mainstem[i,], 3,99)
  
  by_indiv$winter_section[by_indiv$Fish == rownames(seasonal_locs_widelist$seg)[i]] <-
    collapser(sort(unique(sec[c(1,4)][!is.na(sec[c(1,4)])])))
  
  by_indiv$winter_designation[by_indiv$Fish == rownames(seasonal_locs_widelist$seg)[i]] <-
    collapser(sort(unique(des[c(1,4)][!is.na(des[c(1,4)])])))
  
  by_indiv$spring_section[by_indiv$Fish == rownames(seasonal_locs_widelist$seg)[i]] <-
    collapser(sort(unique(sec[c(2,5)][!is.na(sec[c(2,5)])])))
  
  by_indiv$spring_designation[by_indiv$Fish == rownames(seasonal_locs_widelist$seg)[i]] <-
    collapser(sort(unique(des[c(2,5)][!is.na(des[c(2,5)])])))
  
  by_indiv$summer_section[by_indiv$Fish == rownames(seasonal_locs_widelist$seg)[i]] <-
    collapser(sort(unique(sec[c(3,6)][!is.na(sec[c(3,6)])])))
  
  by_indiv$summer_designation[by_indiv$Fish == rownames(seasonal_locs_widelist$seg)[i]] <-
    collapser(sort(unique(des[c(3,6)][!is.na(des[c(3,6)])])))
}


# mean upriver position
by_indiv$mn_upstream_km <- tapply(all_locs$upstream_km,
                                  all_locs$Fish,
                                  mean, na.rm=TRUE)

# mean upriver position - winter
# mean upriver position - spring
# mean upriver position - summer
by_indiv$mn_winter_upstream_km <- NA
by_indiv$mn_spring_upstream_km <- NA
by_indiv$mn_summer_upstream_km <- NA
for(i in 1:nrow(seasonal_locs_widelist$seg)) {
  by_indiv$mn_winter_upstream_km[by_indiv$Fish == rownames(seasonal_locs_widelist$seg)[i]] <-
    mean(unlist(seasonal_locs_widelist$upstream_km[i, c(1,4)]), na.rm=TRUE)
  
  by_indiv$mn_spring_upstream_km[by_indiv$Fish == rownames(seasonal_locs_widelist$seg)[i]] <-
    mean(unlist(seasonal_locs_widelist$upstream_km[i, c(2,5)]), na.rm=TRUE)
  
  by_indiv$mn_summer_upstream_km[by_indiv$Fish == rownames(seasonal_locs_widelist$seg)[i]] <-
    mean(unlist(seasonal_locs_widelist$upstream_km[i, c(3,6)]), na.rm=TRUE)
}


if(write_output) {
  write.csv(by_indiv, file="R_output/ByIndividual.csv")
}