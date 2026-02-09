messagePerc <- function(i, total, whichPerc = 2){
  milestoneSize <- total/(100/whichPerc)
  if (i == 1 || floor(i / milestoneSize) > floor((i - 1) / milestoneSize)) {
    pct <- round((i / total) * 100)
    message(sprintf("Slicing Progress: %d%% complete (%d/%d)", pct, i, total))
  }
}