# To remove the rows with null values in specific columns
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

# To filter data by region
selRegion <- function(data, desiredRegion){
  return(data[which(data$region == desiredRegion),])
}

# To apply string similiarity on the search page
myLevSim = function (str1, str2) {
  fast = T
  innerFunc = function(strArr,str2){
    return(max(levenshteinSim(strArr, str2)))
  }
  fastInnerFunc = function(strArr,str2){
    return(ifelse(str2 %in% strArr, 1, 0))
  }
  srt1Arr = strsplit(tolower(str1),"\\s+")
  str2 = tolower(trimws(str2))
  if(fast == T){
    d = lapply(srt1Arr, fastInnerFunc, str2 = str2)
  }
  else{
    d = lapply(srt1Arr, innerFunc, str2 = str2)
  }
  return (d)
}

# To convert number to strings
intToStr <- function(num){
  if(num / 1000000000 >= 1)
    return(paste(as.character(round(num / 1000000000, 2)), "B")) # 1.83 B
  else if (num / 100000000 >= 1)
    return(paste(substr(as.character(round(num / 1000000, 0)),1,3), "M")) # 188 M
  else if (num / 10000000 >= 1)
    return(paste(substr(as.character(round(num / 1000000, 1)),1,4), "M")) # 17.5 M 
  else if(num / 1000000 >= 1)
    return(paste(as.character(round(num / 1000000, 2)), "M")) # 1.53 M
  else if (num / 100000 >= 1)
    return(paste(substr(as.character(round(num / 1000, 2)),1,3), "k")) # 168 k
  else if (num / 10000 >= 1)
    return(paste(substr(as.character(round(num / 1000, 2)),1,4), "k")) # 11.4 k
  else if (num / 1000 >= 1)
    return(paste(as.character(round(num / 1000, 2)), "k")) # 1.38 k
  else
    return(as.character(num))
}