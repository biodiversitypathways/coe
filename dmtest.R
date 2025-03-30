tt <- read_csv("/users/alexandremacphail/R/coe/DM1test.csv") |>
  rename(Time = 1, LmaxdBA = 2, LEQdBA = 3, LmindBA = 4, LpeakdBA = 5) |>
  mutate(Time = ymd_hms(Time), Hour = hour(Time))  # Extract the hour

ggplot(tt, aes(x = as.numeric(Hour), y = LEQdBA)) +  # Convert Hour to numeric
  geom_point(alpha = 0.6) +
  geom_smooth() +  # Fit a linear trend line
  theme_bw(base_size = 14) +
  labs(
    title = "LEQdBA Grouped by Hour",
    x = "Hour of the Day",
    y = "LEQ (dBA)"
  )

Lmax <- max(tt$LmaxdBA, na.rm = TRUE)
LAeq_24h <- mean(tt$LEQdBA, na.rm = TRUE)
LAeq_16h <- mean(tt$LEQdBA[hour(tt$Time) >= 7 & hour(tt$Time) < 23], na.rm = TRUE)  # Daytime hours 7-23
LAeq_8h <- mean(tt$LEQdBA[hour(tt$Time) < 7 | hour(tt$Time) >= 23], na.rm = TRUE)  # Nighttime hours 23-7

L1 <- quantile(tt$LmaxdBA, 0.01, na.rm = TRUE)
L5 <- quantile(tt$LmaxdBA, 0.05, na.rm = TRUE)
L10 <- quantile(tt$LmaxdBA, 0.10, na.rm = TRUE)
L50 <- quantile(tt$LmaxdBA, 0.50, na.rm = TRUE)
L90 <- quantile(tt$LmaxdBA, 0.90, na.rm = TRUE)
L95 <- quantile(tt$LmaxdBA, 0.95, na.rm = TRUE)

prop_LAeq_24h_55 <- mean(tt$LEQdBA > 55, na.rm = TRUE) * 100
prop_LAeq_24h_65 <- mean(tt$LEQdBA > 65, na.rm = TRUE) * 100

prop_LAeq_16h_55 <- mean(tt$LEQdBA > 55 & hour(tt$Time) >= 7 & hour(tt$Time) < 23, na.rm = TRUE) * 100
prop_LAeq_16h_65 <- mean(tt$LEQdBA > 65 & hour(tt$Time) >= 7 & hour(tt$Time) < 23, na.rm = TRUE) * 100
prop_LAeq_8h_40 <- mean(tt$LEQdBA > 40 & (hour(tt$Time) < 7 | hour(tt$Time) >= 23), na.rm = TRUE) * 100
prop_LAeq_8h_50 <- mean(tt$LEQdBA > 50 & (hour(tt$Time) < 7 | hour(tt$Time) >= 23), na.rm = TRUE) * 100
prop_LAeq_8h_55 <- mean(tt$LEQdBA > 55 & (hour(tt$Time) < 7 | hour(tt$Time) >= 23), na.rm = TRUE) * 100


hourly_avg <- tt %>%
  group_by(Hour) %>%
  summarise(
    avg_LmaxdBA = mean(LmaxdBA, na.rm = TRUE),
    avg_LEQdBA = mean(LEQdBA, na.rm = TRUE),
    avg_LmindBA = mean(LmindBA, na.rm = TRUE),
    avg_LpeakdBA = mean(LpeakdBA, na.rm = TRUE)
  )

list(
  Lmax = Lmax,
  LAeq_24h = LAeq_24h,
  LAeq_16h = LAeq_16h,
  LAeq_8h = LAeq_8h,
  L1 = L1,
  L5 = L5,
  L10 = L10,
  L50 = L50,
  L90 = L90,
  L95 = L95,
  prop_LAeq_24h_55 = prop_LAeq_24h_55,
  prop_LAeq_24h_65 = prop_LAeq_24h_65,
  prop_LAeq_16h_55 = prop_LAeq_16h_55,
  prop_LAeq_16h_65 = prop_LAeq_16h_65,
  prop_LAeq_8h_40 = prop_LAeq_8h_40,
  prop_LAeq_8h_50 = prop_LAeq_8h_50,
  prop_LAeq_8h_55 = prop_LAeq_8h_55,
  hourly_avg = hourly_avg
)

results <- tibble(
  Metric = c("Lmax", "LAeq 24h", "LAeq 16h", "LAeq 8h", "L1", "L5", "L10", "L50", "L90", "L95",
             "Proportion LAeq > 55 (24h)", "Proportion LAeq > 65 (24h)",
             "Proportion LAeq > 55 (16h)", "Proportion LAeq > 65 (16h)",
             "Proportion LAeq > 40 (8h)", "Proportion LAeq > 50 (8h)", "Proportion LAeq > 55 (8h)"),
  Value = c(Lmax, LAeq_24h, LAeq_16h, LAeq_8h, L1, L5, L10, L50, L90, L95,
            prop_LAeq_24h_55, prop_LAeq_24h_65, prop_LAeq_16h_55, prop_LAeq_16h_65,
            prop_LAeq_8h_40, prop_LAeq_8h_50, prop_LAeq_8h_55)
)

# Print the results as a nicely formatted table
results %>%
  kable("html", caption = "Noise Metrics Analysis Results (Percentages)") %>%
  kable_styling(full_width = F, position = "center")
