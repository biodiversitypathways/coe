calculate_f150_co2 <- function(distance_km) {
  # Assumptions
  fuel_efficiency_l_per_100km <- 15.7  # Ford F-150 city fuel efficiency (L/100 km)
  co2_per_liter <- 2.31               # kg CO2 per liter for gasoline
  # Convert distance to fuel consumed (liters)
  fuel_used <- (distance_km / 100) * fuel_efficiency_l_per_100km
  # Calculate CO2 emissions (kg)
  co2_emissions <- fuel_used * co2_per_liter
  return(co2_emissions)
}

distance_km <- 100
co2_emissions <- calculate_f150_co2(distance_km)
co2_emissions

stoken <- httr::config(token = strava_oauth("emissions", app_client_id = "147439", app_secret = Sys.getenv("STRAVA_SECRET"), app_scope = "activity:read_all"))


stoken <- httr::config(token = strava_oauth(app_name, app_client_id, app_secret, app_scope="activity:read_all"))
my_acts <- get_activity_list(stoken, after = as.Date('2020-12-31'))
