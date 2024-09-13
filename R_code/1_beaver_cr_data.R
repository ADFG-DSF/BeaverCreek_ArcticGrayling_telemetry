# Script 1: Beaver Creek Data
# Matt Tyers, July 2024

# This script is specific to loading packages, loading data, and doing data manipulation.
# This script will be source()d at the beginning of subsequent scripts for clarity.

# The important objects loaded into the R workspace are:
#   beaver_cr_op: the rivernetwork object for riverdist computation
#   all_locs: a long-format dataframe with all telemetry observations
#   seasonal_locs: a wide-format dataframe with telemetry observations for each season
#   all_locs_widelist: a list of wide-format tables for all telem observations,
#     where each row is a fish and each column is a survey
#   seasonal_locs_widelist: a list of wide-format tables for each season,
#     where each row is a fish and each column is a season
#   tagging_data: data recorded at tagging.  Probably will only use $Length_mm.


# Loading Packages

library(riverdist)   # for spatial analysis
library(tidyverse)   # for data manipulation
library(dsftools)    # some additional analysis tools



# loading data: river network

# load(file = "R_data/beaver_cr_rivernetwork.Rdata")
# 
# # further data manipulation
# beaver_cr_op <- buildsegroutes(dissolve(trimriver(trim = c(56,59,49,46,54,53,34,31,48,50,55),
#                                                   rivers = beaver_cr)))
# save(beaver_cr_op, file = "R_data/beaver_cr_rivernetwork_op.Rdata")

load(file = "R_data/beaver_cr_rivernetwork_op.Rdata")
plot(beaver_cr_op)



# loading data: all telemetry locations
# compiling a single dataframe all_locs

AKalbers <- "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 
    +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80"
all_locs <- read.csv("R_data/all_locations.csv")
all_locs_albers <- sf::sf_project(pts=all_locs[,4:3], to=AKalbers)
points(all_locs_albers, pch=16, col=2)

all_locs_segvert <- xy2segvert(x = all_locs_albers[,1],
                               y = all_locs_albers[,2],
                               rivers = beaver_cr_op)
riverpoints(seg = all_locs_segvert$seg,
            vert = all_locs_segvert$vert,
            rivers = beaver_cr_op,
            pch = 15, col = 4)

all_locs$x <- all_locs_albers[,1]
all_locs$y <- all_locs_albers[,2]
all_locs$seg <- all_locs_segvert$seg
all_locs$vert <- all_locs_segvert$vert


# adding tagging date
tagging_data <- read.csv("R_data/tagging_data.csv")
tagging_data$Frequency
all_locs$Fish[all_locs$Date=="Tagging"]
all(tagging_data$Frequency[tagging_data$Frequency != "34-90"] == all_locs$Fish[all_locs$Date=="Tagging"])
all_locs$Date[all_locs$Date=="Tagging"] <- tagging_data$Tagdate[tagging_data$Frequency != "34-90"]

all_locs$Date <- as.Date(all_locs$Date, format="%m/%d/%Y")

all_locs$Survey <- NA
all_locs$Survey[all_locs$Date <= as.Date("2021-07-24")] <- 0
all_locs$Survey[all_locs$Date == "2021-12-22"] <- 1
all_locs$Survey[all_locs$Date == "2022-03-15"] <- 2
all_locs$Survey[all_locs$Date == "2022-04-22"] <- 3
all_locs$Survey[all_locs$Date == "2022-05-18"] <- 4
all_locs$Survey[all_locs$Date == "2022-05-25"] <- 5
all_locs$Survey[all_locs$Date == "2022-06-02"] <- 6
all_locs$Survey[all_locs$Date == "2022-07-31"] <- 7
all_locs$Survey[all_locs$Date == "2022-08-19"] <- 8
all_locs$Survey[all_locs$Date == "2022-10-25"] <- 9
all_locs$Survey[all_locs$Date == "2023-03-16"] <- 10
all_locs$Survey[all_locs$Date == "2023-05-09"] <- 11
all_locs$Survey[all_locs$Date == "2023-05-18"] <- 12
all_locs$Survey[all_locs$Date == "2023-05-24"] <- 13
all_locs$Survey[all_locs$Date == "2023-06-26"] <- 14


# standardizing (ordering) tag numbers and removing fish 90

# this means it will work!!
all(rowSums(table(all_locs$Fish, 
              as.numeric(gsub("[^0-9.-]", "", substr(all_locs$Fish, 4, 99)))) != 0)==1)

all_locs$Fish <- as.numeric(gsub("[^0-9.-]", "", substr(all_locs$Fish, 4, 99))) # %>% 
  # as.numeric %>%
  # unique %>%
  # sort

all_locs <- subset(all_locs, Fish != 90)

# need to fill in OP river sections for each observation
# probably make a table and then left_join
sectiontable <- data.frame(seg=1:37, section=NA, mainstem=NA)
sectiontable$section[sectiontable$seg %in% c(23, 22, 36, 33, 21, 35, 20, 33)] <- "1_Lower"
sectiontable$section[sectiontable$seg %in% c(34, 19, 26, 18, 27, 17)] <- "2_Middle"
sectiontable$section[sectiontable$seg %in% c(29, 37, 28, 16, 32, 15, 31, 14, 13, 24)]  <- "3_Upper"
sectiontable$section[sectiontable$seg %in% c(8, 25, 7, 12, 2, 11, 10, 30, 1, 
                                             3, 4, 6, 7, 5, 9)] <- "4_Headwaters"
sectiontable$mainstem <- ifelse(sectiontable$section == "4_Headwaters", "3_Headwaters",
                                ifelse(sectiontable$seg %in% 13:23, "1_Mainstem",
                                       "2_Trib"))
all_locs <- left_join(all_locs, sectiontable)

table(all_locs$section)
with(all_locs, table(Survey, section)) %>% mosaicplot

table(all_locs$mainstem)
with(all_locs, table(Survey, mainstem)) %>% mosaicplot

# add median tagging date so I can use dates later
all_locs$SurveyDate <- as.Date(ifelse(all_locs$Survey == 0,
                              median(all_locs$Date[all_locs$Survey == 0]),
                              all_locs$Date))




# loading data: seasonal aggregations
# compiling a single dataframe seasonal_locs

# seasonal_locs <- read.csv("R_data/seasonal_consolidation.csv")
seasonal_locs <- read.csv("R_data/seasonal_consolidation_withtagging.csv")

seasonal_locs_albers <- sf::sf_project(pts=seasonal_locs[,4:3], to=AKalbers)
plot(beaver_cr_op)
points(seasonal_locs_albers, pch=16, col=2)

seasonal_locs_segvert <- xy2segvert(x = seasonal_locs_albers[,1],
                               y = seasonal_locs_albers[,2],
                               rivers = beaver_cr_op)
riverpoints(seg = seasonal_locs_segvert$seg,
            vert = seasonal_locs_segvert$vert,
            rivers = beaver_cr_op,
            pch = 15, col = 4)

seasonal_locs$x <- seasonal_locs_albers[,1]
seasonal_locs$y <- seasonal_locs_albers[,2]
seasonal_locs$seg <- seasonal_locs_segvert$seg
seasonal_locs$vert <- seasonal_locs_segvert$vert

# standardizing (ordering) tag numbers and removing fish 90

# this means it will work!!
all(rowSums(table(seasonal_locs$Fish, 
                  as.numeric(gsub("[^0-9.-]", "", substr(seasonal_locs$Fish, 4, 99)))) != 0)==1)

seasonal_locs$Fish <- as.numeric(gsub("[^0-9.-]", "", substr(seasonal_locs$Fish, 4, 99))) # %>% 
# as.numeric %>%
# unique %>%
# sort

seasonal_locs <- subset(seasonal_locs, Fish != 90)

seasonal_locs <- left_join(seasonal_locs, sectiontable)
with(seasonal_locs, table(Season, section))%>% mosaicplot
with(seasonal_locs, table(Season, mainstem))%>% mosaicplot

seasonal_locs$upstream_km <- with(seasonal_locs, mouthdist(seg=seg, vert=vert, rivers=beaver_cr_op))/1000
all_locs$upstream_km <- with(all_locs, mouthdist(seg=seg, vert=vert, rivers=beaver_cr_op))/1000



## adding snapped x & y coordinates to both datasets

segvert2xy <- function(seg, vert, rivers) {
  if(length(seg) != length(vert)) stop("Unequal vectors")
  xy <- matrix(nrow=length(seg), ncol=2)
  for(i in 1:length(seg)) {
    xy[i, ] <- rivers$lines[[seg[i]]][vert[i],]
  }
  return(xy)
}

all_xy <- segvert2xy(seg=all_locs$seg, vert=all_locs$vert,
                          rivers=beaver_cr_op)
all_locs$snap_x <- all_xy[,1]
all_locs$snap_y <- all_xy[,2]
seasonal_xy <- segvert2xy(seg=seasonal_locs$seg, vert=seasonal_locs$vert,
                          rivers=beaver_cr_op)
seasonal_locs$snap_x <- seasonal_xy[,1]
seasonal_locs$snap_y <- seasonal_xy[,2]



## then need to make lists of wide-format for all_locs and seasonal_locs

all_locs_widelist <- list()
all_locs_widelist$Date <- pivot_wider(all_locs, id_cols = Fish, names_from = Survey, 
                                      values_from = Date, names_sort = TRUE)
all_locs_widelist$x <- pivot_wider(all_locs, id_cols = Fish, names_from = Survey, 
                                   values_from = x, names_sort = TRUE)
all_locs_widelist$y <- pivot_wider(all_locs, id_cols = Fish, names_from = Survey, 
                                   values_from = y, names_sort = TRUE)
all_locs_widelist$snap_x <- pivot_wider(all_locs, id_cols = Fish, names_from = Survey, 
                                   values_from = snap_x, names_sort = TRUE)
all_locs_widelist$snap_y <- pivot_wider(all_locs, id_cols = Fish, names_from = Survey, 
                                   values_from = snap_y, names_sort = TRUE)
all_locs_widelist$seg <- pivot_wider(all_locs, id_cols = Fish, names_from = Survey, 
                                     values_from = seg, names_sort = TRUE)
all_locs_widelist$vert <- pivot_wider(all_locs, id_cols = Fish, names_from = Survey, 
                                      values_from = vert, names_sort = TRUE)
all_locs_widelist$section <- pivot_wider(all_locs, id_cols = Fish, names_from = Survey, 
                                         values_from = section, names_sort = TRUE)
all_locs_widelist$mainstem <- pivot_wider(all_locs, id_cols = Fish, names_from = Survey, 
                                         values_from = mainstem, names_sort = TRUE)
all_locs_widelist$upstream_km <- pivot_wider(all_locs, id_cols = Fish, names_from = Survey, 
                                             values_from = upstream_km, names_sort = TRUE)

seasonal_locs_widelist <- list()
seasonal_locs_widelist$x <- pivot_wider(seasonal_locs, id_cols = Fish, names_from = Season, 
                                        values_from = x, names_sort = TRUE) %>% arrange(Fish)
seasonal_locs_widelist$y <- pivot_wider(seasonal_locs, id_cols = Fish, names_from = Season, 
                                        values_from = y, names_sort = TRUE) %>% arrange(Fish)
seasonal_locs_widelist$snap_x <- pivot_wider(seasonal_locs, id_cols = Fish, names_from = Season, 
                                        values_from = snap_x, names_sort = TRUE) %>% arrange(Fish)
seasonal_locs_widelist$snap_y <- pivot_wider(seasonal_locs, id_cols = Fish, names_from = Season, 
                                        values_from = snap_y, names_sort = TRUE) %>% arrange(Fish)
seasonal_locs_widelist$seg <- pivot_wider(seasonal_locs, id_cols = Fish, names_from = Season, 
                                     values_from = seg, names_sort = TRUE) %>% arrange(Fish)
seasonal_locs_widelist$vert <- pivot_wider(seasonal_locs, id_cols = Fish, names_from = Season, 
                                      values_from = vert, names_sort = TRUE) %>% arrange(Fish)
seasonal_locs_widelist$section <- pivot_wider(seasonal_locs, id_cols = Fish, names_from = Season, 
                                              values_from = section, names_sort = TRUE) %>% arrange(Fish)
seasonal_locs_widelist$mainstem <- pivot_wider(seasonal_locs, id_cols = Fish, names_from = Season, 
                                              values_from = mainstem, names_sort = TRUE) %>% arrange(Fish)
seasonal_locs_widelist$upstream_km <- pivot_wider(seasonal_locs, id_cols = Fish, names_from = Season, 
                                             values_from = upstream_km, names_sort = TRUE) %>% arrange(Fish)

# making the $Fish column a row name rather than a column
for(i_list in 1:length(all_locs_widelist)) {
  all_locs_widelist[[i_list]] <- as.data.frame(all_locs_widelist[[i_list]])
  rownames(all_locs_widelist[[i_list]]) <- as.vector(all_locs_widelist[[i_list]][,1])
  all_locs_widelist[[i_list]] <- all_locs_widelist[[i_list]][,-1]
}
for(i_list in 1:length(seasonal_locs_widelist)) {
  seasonal_locs_widelist[[i_list]] <- as.data.frame(seasonal_locs_widelist[[i_list]])
  rownames(seasonal_locs_widelist[[i_list]]) <- as.vector(seasonal_locs_widelist[[i_list]][,1])
  seasonal_locs_widelist[[i_list]] <- seasonal_locs_widelist[[i_list]][,-1]
}

# making sure the tagging_data dataframe is formatted equivalently
tagging_data$Fish <- as.numeric(gsub("[^0-9.-]", "", substr(tagging_data$Frequency, 4, 99)))
tagging_data <- tagging_data[order(tagging_data$Fish),]
tagging_data <- tagging_data[tagging_data$Fish != 90,]
