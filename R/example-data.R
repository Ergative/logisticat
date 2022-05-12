#' Data for a hypothetical experiment on time to competence in echinoid larvae.
#'
#' When exposed to potassium chloride, and echinoid larvae will either
#' metamorphose, in which case it has reached metamorphic competence, or it will
#' fail to metamorphose, in which case it is not competent.
#'
#' Note that the total of the "success" and "failure" counts don't need to
#' add up to the same value, they just happen to in this data set.
#'
#' @return A `data.frame` with example data.
#' @export
metamorphosis <- function() {
  # days:                 The predictor variable - age in days.
  # ration:               Treatment groups.
  # beaker:               Replicate groups within each treatment.
  # metamorphosed:        The "success" count column.
  # did.not.metamorphose: The "failure" count column.
  tibble::tribble(
    ~days, ~ration, ~beaker, ~metamorphosed, ~did.not.metamorphose,
       11,   "low",     "A",              0,                    10,
       11,   "low",     "B",              0,                    10,
       11,   "low",     "C",              0,                    10,
       11,  "high",     "D",              0,                    10,
       11,  "high",     "E",              0,                    10,
       11,  "high",     "F",              0,                    10,

       11,   "low",     "A",              1,                     9,
       11,   "low",     "B",              0,                    10,
       11,   "low",     "C",              2,                     8,
       11,  "high",     "D",              1,                     9,
       11,  "high",     "E",              1,                     9,
       11,  "high",     "F",              1,                     9,

       12,   "low",     "A",              2,                     8,
       12,   "low",     "B",              2,                     8,
       12,   "low",     "C",              4,                     6,
       12,  "high",     "D",              2,                     8,
       12,  "high",     "E",              2,                     8,
       12,  "high",     "F",              4,                     6,

       13,   "low",     "A",              2,                     8,
       13,   "low",     "B",              3,                     7,
       13,   "low",     "C",              3,                     7,
       13,  "high",     "D",              5,                     5,
       13,  "high",     "E",              6,                     4,
       13,  "high",     "F",              5,                     5,

       14,   "low",     "A",              2,                     8,
       14,   "low",     "B",              3,                     7,
       14,   "low",     "C",              3,                     7,
       14,  "high",     "D",              8,                     2,
       14,  "high",     "E",              7,                     3,
       14,  "high",     "F",              7,                     3,

       15,   "low",     "A",              5,                     5,
       15,   "low",     "B",              6,                     4,
       15,   "low",     "C",              5,                     5,
       15,  "high",     "D",              8,                     2,
       15,  "high",     "E",              9,                     1,
       15,  "high",     "F",              8,                     2,

       16,   "low",     "A",              6,                     4,
       16,   "low",     "B",              8,                     2,
       16,   "low",     "C",              8,                     2,
       16,  "high",     "D",             10,                     0,
       16,  "high",     "E",             10,                     0,
       16,  "high",     "F",              9,                     1,

       17,   "low",     "A",              7,                     3,
       17,   "low",     "B",              9,                     1,
       17,   "low",     "C",              8,                     2,
       17,  "high",     "A",             10,                     0,
       17,  "high",     "B",             10,                     0,
       17,  "high",     "C",             10,                     0,
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


