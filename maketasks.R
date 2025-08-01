wt_auth()

wt_get_recordings(5466) |>
  as_tibble() |>
  unnest(everything()) |>
  filter(grepl('EET',location)) |>
  mutate(recording_date_time = as.POSIXct(recording_date_time),
         julian = yday(recording_date_time),
         hour = hour(recording_date_time),
         typ = case_when(hour %in% c(0:3,23) ~ "Night",
                         hour %in% c(4:7) ~ "Dawn",
                         hour %in% c(8:20) ~ "Day",
                         hour %in% c(21:22) ~ "Dusk",
                         TRUE ~ NA_character_)) |>
  filter(between(julian, 130, 190)) |>
  group_by(location, typ) |>
  sample_n(1, replace = F) |>
  ungroup() |>
  rename(length_seconds = recording_length) |>
  wt_make_aru_tasks(output = "/users/alexandremacphail/desktop/coeagain1.csv", task_method = "1SPT", task_length = 180)
