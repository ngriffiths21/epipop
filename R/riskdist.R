#' @export
riskdist <- function (outcome, covs, data) {
  expanded <- unique(data[covs])

  expanded$.risk <-
    map_dbl(1:nrow(expanded),
        ~ out_expect(outcome, expanded[.x,,drop = FALSE], data))

  expanded$.p <-
    map_dbl(1:nrow(expanded),
        ~ pop_ct(expanded[.x,covs,drop = FALSE], data))
  expanded
}

matches_covs <- function (match, data) {
  matches <- map(names(match), ~ data[[.x]] == match[[.x]])
  reduce(matches, `&`)
}

out_expect <- function (outcome, row, data) {
  mean(data[matches_covs(row, data),outcome])
}

pop_ct <- function (row, data) {
  sum(matches_covs(row, data)) / nrow(data)
}
