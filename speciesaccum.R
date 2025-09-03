first_detection <- coe |>
  group_by(species_code) |>
  arrange(recording_date_time) |>
  summarise(
    FirstSite = first(location),
    FirstDate = first(recording_date_time),
    .groups = "drop"
  )

coe2 <- coe |>
  mutate(year = year(recording_date_time)) |>
  left_join(first_detection, by = "species_code") |>
  mutate(IsFirstDetection = location == FirstSite)

# Create species-by-site matrix
species_matrix <- coe2 |>
  group_by(year, location, species_code) |>
  summarise(individual_count = max(individual_order, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(
    names_from = species_code,
    values_from = individual_count,
    values_fill = 0
  ) |>
  dplyr::select(-year, -location) |>
  as.matrix()

spec_accum <- specaccum(species_matrix, method = "random")

accum_data <- data.frame(
  Sites = spec_accum$sites,
  Richness = spec_accum$richness,
  SD = spec_accum$sd) |>
  mutate(Lower = Richness - SD,
         Upper = Richness + SD)

first_detection_data <- first_detection |>
  mutate(SiteOrder = match(FirstSite, unique(coe$location))) |>
  left_join(accum_data, by = c("SiteOrder" = "Sites"))

ggplot(accum_data, aes(x = Sites, y = Richness)) +
  geom_line(color = "blue", size = 1) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "lightblue", alpha = 0.4) +
  geom_point(
    data = first_detection_data,
    aes(x = SiteOrder, y = Richness),
    size = 3
  ) +
  labs(
    title = "Species accumulation curve",
    x = "Number of Locations",
    y = "Cumulative number of species"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14)
  )
