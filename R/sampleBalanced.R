sampleBalanced <- function(pool, nS, seed) {
  if (nrow(pool) == 0) return(NULL)
  all_strata <- unique(pool$indiv_step_id)
  set.seed(seed)
  selected <- sample(all_strata, size = min(length(all_strata), nS))
  return(pool[indiv_step_id %in% selected])
}