#' @title Convert GPS to XY (m)
#' @export
conv.GPS2XY <- function(dat,
                        col_name_gps_lon,
                        col_name_gps_lat,
                        gps_lon_origin,
                        gps_lat_origin,
                        return_only_x = F,
                        return_only_y = F) {

    dat <-
      dat %>%

    ## Test data
    #data.frame(gps_lon = 11.639306, gps_lat = 48.078605) %>%

    ## Compute distance from origin (critical distance) to each coordinate
    ## ... by keeping constant either lateral or longitudinal coordinate
    rowwise() %>%

    ## Longitude
    mutate_(.dots = setNames(list(
      interp(~distm( c(gps_lon_origin, gps_lat_origin),
                     c(gps_lon, gps_lat_origin)),
             gps_lon_origin = gps_lon_origin,
             gps_lat_origin = gps_lat_origin,
             gps_lon = as.name(col_name_gps_lon))),
      #gps_lat = as.name(col_name_gps_lat))),
      #paste_(col_name_gps_lon, "dist"))
      "x")
    ) %>%

    ## Latitude
    mutate_(.dots = setNames(list(
      interp(~distm( c(gps_lon_origin, gps_lat_origin),
                     c(gps_lon_origin, gps_lat)),
             gps_lon_origin = gps_lon_origin,
             gps_lat_origin = gps_lat_origin,
             #gps_lon = as.name(col_name_gps_lon),
             gps_lat = as.name(col_name_gps_lat))),
      #paste_(col_name_gps_lat, "dist"))
      "y")
    ) %>%


    ## Correct distances by taking origin into account
    mutate_(.dots = setNames(list(
      interp(~ ifelse(gps_lon < gps_lon_origin,
                      gps_lon_dist * -1,
                      gps_lon_dist),
             gps_lon_origin = gps_lon_origin,
             gps_lon = as.name(col_name_gps_lon),
             #gps_lon_dist = as.name(paste_(col_name_gps_lon, "dist")) )),
             gps_lon_dist = as.name("x") )),
      #paste_(col_name_gps_lon, "dist"))
      "x")
    ) %>%
    mutate_(.dots = setNames(list(
      interp(~ ifelse(gps_lat < gps_lat_origin,
                      gps_lat_dist * -1,
                      gps_lat_dist),
             gps_lat_origin = gps_lat_origin,
             gps_lat = as.name(col_name_gps_lat),
             #gps_lon_dist = as.name(paste_(col_name_gps_lon, "dist")) )),
             gps_lat_dist = as.name("y") )),
      #paste_(col_name_gps_lon, "dist"))
      "y")
    )

  if (return_only_x) {
    return(dat$x)
  }

  if (return_only_y) {
    return(dat$y)
  }

  if (!return_only_x & !return_only_y) {
    return(dat)
  }
}


# conv.GPS2XY(data.frame(gps_lon = 11.639306, gps_lat = 48.078605),
#             "gps_lon",
#             "gps_lat",
#             11.63825455,
#             48.07737816)
