# caribouNN NEWS

## Refit (branch `feature/refit-disjoint-sets`, 2026-10-07)

An audit of the first experiment found problems that invalidate its comparisons. This branch rebuilds the
experiment; nothing from the old runs should be reused.

### Data leakage and sampling (CRITICAL in the old code)
- Old `sampleBalanced()` drew train, validation and test **independently from overlapping pools**: ~70% of
  FutureTainted validation strata and ~60% of Internal test strata were also in training.
  New `buildGroupSplits()` makes all sets disjoint; `verifySplits()` is a hard gate (and `auditSplitManifests()`
  re-verifies the saved manifests after the run).
- One **shared test set** per split (50% of the test-year strata) is scored by all three regimes (paired).
  Internal no longer has a multi-year test window.
- FutureTainted uses exactly the FutureUnseen strata, randomly re-allocated: identical data, animals, bursts.
- Internal is restricted (`matchAnimals = TRUE`, default) to the animals of that pool: same animals and same number of
  strata in every regime. Bursts are reported per set (they cannot be equalised without changing the regimes).
- All complexity levels and regimes of a split share the same strata (the old group seed depended on the
  complexity level, and `sum(utf8ToInt(groupId))` was not unique).
- The animal/burst "caps" of the old plan were never enforced; they are replaced by the identical-pool construction
  above (counts per set are written to `splitSummary.csv`).
- Spatial arm: blocks (+ buffer) held out from every pool in every regime, for a subset of splits.

### Model fitting
- Unseen animals ("strangers"): the old `$mean(dim = 0)` failed silently inside a warning-only `tryCatch`
  (R torch is 1-based), so unseen animals kept random embeddings. Now: a reserved embedding row = mean of the trained
  animals' rows (plain R), used for validation and test scoring, with a unit test.
- luz removed. Explicit training loop: the LR scheduler and checkpoint monitor the **validation** loss; per-epoch history saved.
- Best weights are saved, reloaded into a fresh network and re-scored: error if the loss differs (the old code only warned).
- No silent fallbacks: failures raise errors (collected in `errors/`, the run fails at the end).
- Standardisation uses training statistics only; clipped at +/- 10 in every model.

### Outputs
- Per-stratum losses (validation and test) with stratum id, animal, burst, year, seen flag, loss, p(true), rank
  (`_perStratum.rds`), full test scores, history, scaling, weights, provenance. The old `_rawLosses` were per-batch means.

### Reproducibility
- RNG kind pinned (`Mersenne-Twister/Inversion/Rejection`) inside every sampling call; seeds from a documented string hash;
  torch seeds set per model; every seed in `seedRegistry.csv`; provenance per model and per run.
- Parallelism by SLURM array (`runSlice`) instead of in-process forking.

### Related changes in caribouNN_Global (same branch name)
- Calendar-year counters: `timeSince{Fire,Harvest}` equal to `year - 1900` (no record) recoded to 40, as in the published iSSA.
- Analysis starts in 2013. Covariate ranking: within-stratum permutation importance on held-out strata.

# caribouNN 0.0.1 (21 January 2026)

- initial module version
