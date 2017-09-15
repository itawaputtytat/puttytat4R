testi <-
  data.frame(col1 = c(1, 2, NA, NA,  4,    5,   6,  7, NA, 12),
             col2 = c(NA, "a", NA, NA , "b", "b", "c", "c", NA, 'd'),
             col3 = c(NA, 1, NA, NA, 5, 8, 9, 12, NA, 15),
             col4 = c(NA, NA, NA, NA, 4, 7, NA, 9, 5, 10),
             stringsAsFactors = F)

testi
intrpldf(testi, min = -2, max = 7, stepsize = 1, colname4ref = "col1", showLog = T)
intrpldf(testi, min = -2, max = 7, stepsize = 1, colname4ref = "col1", showLog = T,
         colnames2excl = "col2")
intrpldf(testi, min = -2, max = 15, stepsize = 1, colname4ref = "col1", showLog = T,
         colnames2excl = "col3",
         replace_preceding = T)
intrpldf(testi, min = -2, max = 15, stepsize = 1, colname4ref = "col1", showLog = T,
         colnames2excl = "col3",
         replace_preceding = F)

