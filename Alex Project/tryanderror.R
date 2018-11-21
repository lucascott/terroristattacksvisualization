library(ggplot2)
library(dplyr)

data = read.csv("raw_data.csv", header = T)

# Select variables
sel_vars <- c("eventid", "iyear", "imonth", "iday", "country_txt", "region_txt", "provstate", "city", "latitude", "longitude", "specificity", "vicinity", "summary", "crit1", "crit2", "crit3", "doubtterr", "multiple", "success", "suicide", "attacktype1_txt", "targtype1_txt", "targsubtype1_txt", "natlty1_txt", "gname", "guncertain1", "nperps", "nperpcap", "claimed", "weaptype1_txt", "weapsubtype1_txt", "nkill", "nkillter", "nwound", "nwoundte", "property", "propextent_txt", "ishostkid", "ransom", "scite1", "scite2", "scite3", "INT_LOG", "INT_IDEO", "INT_MISC", "INT_ANY")
data <- select(data, sel_vars)
sample <- data[1:5000,]
completeFun <- function(fdata, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

sample <- data
s1 <- sample
s1 <- completeFun(sample, c("iyear","region_txt", "nkill"))
s1 <- s1 %>% group_by(iyear, s1$region_txt) %>% tally(sort=TRUE)


r <-aggregate(s1$nkill, by=list(year = s1$iyear, region1 = s1$region_txt), FUN=sum)

colnames(r) <- c("year", "region", "fatalities")
ggplot(r, aes(x=r[,1], y=r[,3], colour=r[,2], group = interaction(r[2]))) + geom_point(size=3) + geom_line(size=1)
r[]