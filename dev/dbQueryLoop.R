set4query <- c()
set4query$src <- "t_can_full_aggr_dist_m_rnd1_max_dist2sxx_v2"
set4query$save2df_prefix <- "can"
set4query$sxx   <- c(2)
set4query$round <- c("normal")
set4query$subid <- c(1,5)
set4query$distvar <- "dist_m_rnd1"
set4query$dist1   <- -75
set4query$dist1n  <- 0
set4query$dist2   <- 50
set4query$var_session <-
  c("subid",
    "round_txt",
    "time_s",
    "dist_m_rnd1",
    "gps_lat",
    "gps_long")
set4query$var_sxx <-
  c("_dist_s_rnd1",
    "_dist_m_rnd1")
set4query$var_data <-
  c("steerangle_deg",
    "speed_kmh")




# Create query ------------------------------------------------------------

query <- c()



# SELECT ------------------------------------------------------------------

set4query$var_sxx_compl <-
  sapply(set4query$sxx, function(sxx)
    paste(sprintf("s%02d", sxx), set4query$var_sxx, sep = "") )
set4query$var_sxx_compl <- as.vector(set4query$var_sxx_compl)

query$SELECT <-
  c(set4query$var_session,
    set4query$var_sxx_compl,
    set4query$var_data)

# set4query$SELECT <-
#   sapply(set4query$varlist, function(var)
#       paste(set4query$src, ".", var, sep = ""))
# set4query$SELECT <- as.vector(set4query$SELECT)
query$SELECT <-
  paste("SELECT", paste(query$SELECT, collapse = ",\n"), sep = "\n")
cat(query$SELECT)



# FROM --------------------------------------------------------------------

query$FROM <- paste("FROM", set4query$src, sep = "\n")
cat(query$FROM)


# WHERE -------------------------------------------------------------------

query$WHERE$subid <-
  paste(paste("subid", "=", set4query$subid), collapse = " OR\n")

query$WHERE$round_txt <-
  paste(paste("round_txt", "= '", set4query$round, "'", sep = ""),
        collapse = " OR\n")

## INSERT FILTER HERE !!!!!
temp_dist1 <- set4query$dist1
temp_dist2 <- set4query$dist2

query$WHERE$dist2sxx <-
  lapply(set4query$sxx, function(sxx)
    paste(sprintf("s%02d", sxx), "_", set4query$distvar, " >= ", temp_dist1,
          " AND\n",
          sprintf("s%02d", sxx), "_", set4query$distvar, " <= ", temp_dist2,
          sep = ""))

query$WHERE <-
  paste("WHERE",
        paste("(\n", c(query$WHERE$subid,
                      query$WHERE$round_txt,
                      query$WHERE$dist2sxx), "\n)",
              collapse = " AND ",
              sep = ""), sep = "\n")

cat(query$WHERE)



# Complete string ---------------------------------------------------------

dbquery <- paste(query, collapse = "\n\n")
cat(dbquery)




# Load data ---------------------------------------------------------------

dat <- dbGetQuery(dbconn, dbquery)
