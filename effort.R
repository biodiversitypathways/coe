coe_rec_sum <- wt_get_sync("organization_recording_summary", "data", organization = 5466)
eff <- coe_rec_sum |>
  mutate(year = year(recording_date_time)) |>
  filter(year == 2025) |>
  group_by(location, year) %>%
  summarise(
    n_recordings = n(),
    total_hours = sum(recording_length, na.rm = TRUE) / 3600,
    total_days = n_distinct(as_date(recording_date_time)),
    .groups = "drop"
  ) |>
  arrange(-total_hours) |>
  filter(!total_days < 2)

ggplot(eff, aes(x=reorder(location,-total_days), y=total_days)) +
  geom_bar(stat = "identity") +
  theme_bw()
