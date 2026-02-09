sliceDataset <- function(index, 
                         experimentPlan, 
                         preparedData){
  grp <- experimentPlan[index, ]
  dt <- preparedData[year >= grp$trainStartYear & 
                             year <= grp$testEndYear ]
  return(dt)
}
