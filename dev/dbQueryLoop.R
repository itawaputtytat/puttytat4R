set4query <- c()
set4query$src <- "t_can_full_aggr_dist_m_rnd1_max_dist2sxx_v2"
set4query$save2df_prefix <- "can"
set4query$sxx   <- c(2, 4)
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



# Test --------------------------------------------------------------------




# Load data ---------------------------------------------------------------

dat <- dbGetQuery(dbconn, query)
