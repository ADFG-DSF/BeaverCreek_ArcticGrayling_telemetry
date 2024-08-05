source("R_code/4_beaver_cr_ByIndividual.R")

summary(by_indiv)

# simplifying categories? what to do with fish with multiple cats, eg Headwaters/Upper
by_indiv$winter_section_orig <- by_indiv$winter_section
by_indiv$spring_section_orig <- by_indiv$spring_section
by_indiv$summer_section_orig <- by_indiv$summer_section
by_indiv$winter_designation_orig <- by_indiv$winter_designation
by_indiv$spring_designation_orig <- by_indiv$spring_designation
by_indiv$summer_designation_orig <- by_indiv$summer_designation

# simplifying categories, take 1: 
# defining as the lesser-observed category
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

plot(by_indiv[,-c(1,3,19:24)])
