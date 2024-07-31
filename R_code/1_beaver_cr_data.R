library(riverdist)   # for spatial analysis
library(tidyverse)   # for data manipulation

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
substr(all_locs$Fish, 4, 99) %>%
  unique %>% sort

gsub("[^0-9.-]", "", substr(all_locs$Fish, 4, 99)) %>% 
  as.numeric %>%
  unique %>%
  sort

# this means it worked!!
all(rowSums(table(all_locs$Fish, 
              as.numeric(gsub("[^0-9.-]", "", substr(all_locs$Fish, 4, 99)))) != 0)==1)

## THIS IS NOT FINISHED YET, I THINK I NEED THE SECOND THING ABOVE



# loading data: seasonal aggregations
# compiling a single dataframe all_locs
