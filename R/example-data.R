#' Data for a hypothetical experiment on time to competence in echinoid larvae.
#'
#' When exposed to potassium chloride, and echinoid larvae will either
#' metamorhpose, in which case it has reached metamorphic competence, or it will
#' fail to metamorphose, in which case it is not competent.
#'
#' Note that the total of the "success" and "failure" counts don't need to
#' add up to the same value, they just happen to in this data set.
#'
#' @return A `data.frame` with example data.
#' @export
metamorphosis <- function() {
  # days:                 The predictor variable - look at time to competence.
  # ration:               Treatment groups.
  # beaker:               Replicate groups within each treatment.
  # metamorphosed:        The "success" count column.
  # did.not.metamorphose: The "failure" count column.
  tibble::tribble(
    ~days, ~ration, ~beaker, ~metamorphosed, ~did.not.metamorphose,
    1,   "low",     "A",              0,                    10,
    1,   "low",     "B",              0,                    10,
    1,   "low",     "C",              0,                    10,
    1,  "high",     "D",              0,                    10,
    1,  "high",     "E",              0,                    10,
    1,  "high",     "F",              0,                    10,

    2,   "low",     "A",              1,                     9,
    2,   "low",     "B",              0,                    10,
    2,   "low",     "C",              2,                     8,
    2,  "high",     "D",              1,                     9,
    2,  "high",     "E",              1,                     9,
    2,  "high",     "F",              1,                     9,

    2,   "low",     "A",              2,                     8,
    2,   "low",     "B",              2,                     8,
    2,   "low",     "C",              4,                     6,
    2,  "high",     "D",              2,                     8,
    2,  "high",     "E",              2,                     8,
    2,  "high",     "F",              4,                     6,

    3,   "low",     "A",              2,                     8,
    3,   "low",     "B",              3,                     7,
    3,   "low",     "C",              3,                     7,
    3,  "high",     "D",              5,                     5,
    3,  "high",     "E",              6,                     4,
    3,  "high",     "F",              5,                     5,

    4,   "low",     "A",              2,                     8,
    4,   "low",     "B",              3,                     7,
    4,   "low",     "C",              3,                     7,
    4,  "high",     "D",              8,                     2,
    4,  "high",     "E",              7,                     3,
    4,  "high",     "F",              7,                     3,

    5,   "low",     "A",              5,                     5,
    5,   "low",     "B",              6,                     4,
    5,   "low",     "C",              5,                     5,
    5,  "high",     "D",              8,                     2,
    5,  "high",     "E",              9,                     1,
    5,  "high",     "F",              8,                     2,

    6,   "low",     "A",              6,                     4,
    6,   "low",     "B",              8,                     2,
    6,   "low",     "C",              8,                     2,
    6,  "high",     "D",             10,                     0,
    6,  "high",     "E",             10,                     0,
    6,  "high",     "F",              9,                     1,

    7,   "low",     "A",              7,                     3,
    7,   "low",     "B",              9,                     1,
    7,   "low",     "C",              8,                     2,
    7,  "high",     "A",             10,                     0,
    7,  "high",     "B",             10,                     0,
    7,  "high",     "C",             10,                     0,
  )
}

#' Small data set for use testing functions in this package.
#'
#' @return A `data.frame` with example data.
#' @export
sanity.check <- function() {
  tibble::tribble(
    ~label, ~pass, ~fail,
    "p3f0",     3,     0, # Three with result=T.
    "p0f2",     0,     2, # Two with result=F.
    "p1f1",     1,     1, # One with result=T, one with result=F.
    "p0f0",     0,     0, # None in output.
  )
}


