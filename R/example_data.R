metamorphosis <- tibble::tribble(
  ~days, ~ration, ~beaker, ~metamorphosed, ~did.not.metamorphose,
      1,   "low",     "A",              0,                    10,
      1,   "low",     "B",              0,                    10,
      1,   "low",     "C",              0,                    10,
      1,  "high",     "A",              0,                    10,
      1,  "high",     "B",              0,                    10,
      1,  "high",     "C",              0,                    10,

      2,   "low",     "A",              1,                     9,
      2,   "low",     "B",              0,                    10,
      2,   "low",     "C",              2,                     8,
      2,  "high",     "A",              1,                     9,
      2,  "high",     "B",              1,                     9,
      2,  "high",     "C",              1,                     9,

      2,   "low",     "A",              2,                     8,
      2,   "low",     "B",              2,                     8,
      2,   "low",     "C",              4,                     6,
      2,  "high",     "A",              2,                     8,
      2,  "high",     "B",              2,                     8,
      2,  "high",     "C",              4,                     6,

      3,   "low",     "A",              2,                     8,
      3,   "low",     "B",              3,                     7,
      3,   "low",     "C",              3,                     7,
      3,  "high",     "A",              5,                     5,
      3,  "high",     "B",              6,                     4,
      3,  "high",     "C",              5,                     5,

      4,   "low",     "A",              2,                     8,
      4,   "low",     "B",              3,                     7,
      4,   "low",     "C",              3,                     7,
      4,  "high",     "A",              8,                     2,
      4,  "high",     "B",              7,                     3,
      4,  "high",     "C",              7,                     3,

      5,   "low",     "A",              5,                     5,
      5,   "low",     "B",              6,                     4,
      5,   "low",     "C",              5,                     5,
      5,  "high",     "A",              8,                     2,
      5,  "high",     "B",              9,                     1,
      5,  "high",     "C",              8,                     2,

      6,   "low",     "A",              6,                     4,
      6,   "low",     "B",              8,                     2,
      6,   "low",     "C",              8,                     2,
      6,  "high",     "A",             10,                     0,
      6,  "high",     "B",             10,                     0,
      6,  "high",     "C",              9,                     1,

      7,   "low",     "A",              7,                     3,
      7,   "low",     "B",              9,                     1,
      7,   "low",     "C",              8,                     2,
      7,  "high",     "A",             10,                     0,
      7,  "high",     "B",             10,                     0,
      7,  "high",     "C",             10,                     0,
)

# counts.to.reps(sanity.check, pass, fail, result)
# # A tibble: 7 x 2
# #   label  result
# #    <chr>  <lgl>
# #   1 p3f0  TRUE
# #   2 p3f0  TRUE
# #   3 p3f0  TRUE
# #   4 p1f1  TRUE
# #   5 p0f2  FALSE
# #   6 p0f2  FALSE
# #   7 p1f1  FALSE
sanity.check <- tibble::tribble(
  ~label, ~pass, ~fail,
  "p3f0",     3,     0, # Three with result=T.
  "p0f2",     0,     2, # Two with result=F.
  "p1f1",     1,     1, # One with result=T, one with result=F.
  "p0f0",     0,     0, # None in output.
)


