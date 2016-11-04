testi <-
  data.frame(col1 = c(1,   NA, NA,  4,    5,   6,  7, NA, 12),
             col2 = c("a", NA, NA , "b", "b", "c", "c", NA, 'd'),
             col3 = c(1, NA, NA, 5, 8, 9, 12, NA, 15))

intrpldf(testi, min = -2, max = 3, stepsize = 1, "col1", showLog = T)
