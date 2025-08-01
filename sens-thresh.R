decs <- decibels_filtered |>
  mutate(hour = hour(Time_minute),
         julian = yday(Time_minute)) |>
  group_by(location) |>
  summarise(mean = mean(LEQ_dB_A),
            sd = sd(LEQ_dB_A)) |>
  ungroup()

bb <- coe |>
  mutate(julian = yday(recording_date_time),
         hour = hour(recording_date_time)) |>
  dplyr::select(location, recording_date_time, julian, hour, aru_task_status, species_code, individual_order, individual_count) |>
  filter(aru_task_status == "Transcribed") |>
  distinct() |>
  left_join(decs, by = c("location")) |>
  group_by(location, recording_date_time, julian, hour, aru_task_status, mean, sd, species_code) |>
  summarise(individual_order = max(individual_order)) |>
  ungroup()

bbi_filtered <- bb |> filter(!is.na(mean), !is.na(individual_order))
modi <- lm(individual_order ~ mean, data = bbi_filtered, weights = 1/sd)
anova(modi, segi)
davies.test(modi, seg.Z = ~mean)
segi <- segmented(modi, seg.Z = ~mean, npsi = 1)
bpi <- as.numeric(segi$psi[1, "Est."])
bbi_filtered$fit <- predict(segi)
gam_mod <- gam(individual_order ~ s(mean), data = bbi)
plot(gam_mod, residuals = TRUE, pch = 19)
summary(gam_mod)

p1 <- ggplot(bbi_filtered, aes(x = mean, y = individual_order)) +
  geom_point(alpha = 0.6, color = "gray40") +
  geom_line(aes(y = fit), color = "firebrick", size = 1) +
  geom_vline(xintercept = bpi, linetype = "dashed", color = "blue") +
  labs(
    title = "Breakpoint: Maximnum Count per Species per Location ~ Mean LEQ dBA",
    subtitle = paste("Breakpoint at", round(bpi, 2), "dBA"),
    x = "Mean LEQ dBA",
    y = "Maximum count"
  ) +
  theme_minimal()

bb1 <- coe |>
  mutate(julian = yday(recording_date_time),
         hour = hour(recording_date_time)) |>
  dplyr::select(location, recording_date_time, julian, hour, aru_task_status, species_code, individual_order, individual_count) |>
  filter(aru_task_status == "Transcribed") |>
  distinct() |>
  left_join(decs, by = c("location")) |>
  group_by(location, recording_date_time, julian, hour, aru_task_status, mean, sd) |>
  summarise(rich = n_distinct(species_code)) |>
  ungroup()

bb0_filtered <- bb1 |> filter(!is.na(mean), !is.na(rich))
mod2 <- lm(rich ~ mean, data = bb0_filtered, weights = 1/sd)
seg2 <- segmented(mod2, seg.Z = ~mean, npsi = 1)
bp2 <- as.numeric(seg2$psi[1, "Est."])
bb0_filtered$fit <- predict(seg2)

p2 <- ggplot(bb0_filtered, aes(x = mean, y = rich)) +
  geom_point(alpha = 0.6, color = "gray40") +
  geom_line(aes(y = fit), color = "firebrick", size = 1) +
  geom_vline(xintercept = bp2, linetype = "dashed", color = "blue") +
  labs(
    title = "Breakpoint: Species Richness per Location ~ Mean LEQ dBA",
    subtitle = paste("Breakpoint at", round(bp2, 2), "dBA"),
    x = "Mean LEQ dBA",
    y = "Richness"
  ) +
  theme_minimal()

bb2 <- wt_download_report(1750, 'ARU', 'birdnet')

bbb <- bb2 |>
  mutate(julian = yday(recording_date_time),
         hour = hour(recording_date_time)) |>
  dplyr::select(location, recording_date_time, julian, hour, species_code) |>
  distinct() |>
  left_join(decs, by = c("location")) |>
  group_by(location, recording_date_time, julian, hour, mean, sd) |>
  summarise(rich = n_distinct(species_code)) |>
  ungroup()

bb3 <- bbb |> filter(!is.na(mean), !is.na(rich))
mod3 <- lm(rich ~ mean, data = bb3, weights = 1/sd)
seg3 <- segmented(mod3, seg.Z = ~mean, npsi = 1)
bp3 <- as.numeric(seg3$psi[1, "Est."])
bb3$fit <- predict(seg3)

p3 <- ggplot(bb3, aes(x = mean, y = rich)) +
  geom_point(alpha = 0.6, color = "gray40") +
  geom_line(aes(y = fit), color = "firebrick", size = 1) +
  geom_vline(xintercept = bp3, linetype = "dashed", color = "blue") +
  labs(
    title = "Breakpoint: BirdNET Detection Species Richness ~ Mean LEQ dBA",
    subtitle = paste("Breakpoint at", round(bp3, 2), "dBA"),
    x = "Mean LEQ dBA",
    y = "BirdNET Species Richness"
  ) +
  theme_minimal()

bbbb <- bb2 |>
  mutate(julian = yday(recording_date_time),
         hour = hour(recording_date_time)) |>
  dplyr::select(location, recording_date_time, julian, hour, species_code) |>
  distinct() |>
  left_join(decs, by = c("location")) |>
  group_by(location, recording_date_time, julian, hour, mean, sd) |>
  tally() |> ungroup()

bb4 <- bbbb |> filter(!is.na(mean), !is.na(n))
mod4 <- lm(n ~ mean, data = bb4, weights = 1/sd)
seg4 <- segmented(mod4, seg.Z = ~mean, npsi = 1)
anova(mod4, seg4)
davies.test(mod4, seg.Z = ~mean)
bp4 <- as.numeric(seg4$psi[1, "Est."])
bb4$fit <- predict(seg4)
library(mgcv)
gam_mod <- gam(n ~ s(mean), data = bb4)
plot(gam_mod, residuals = TRUE, pch = 19)
summary(gam_mod)

p4 <- ggplot(bb4, aes(x = mean, y = n)) +
  geom_point(alpha = 0.6, color = "gray40") +
  geom_line(aes(y = fit), color = "firebrick", size = 1) +
  geom_vline(xintercept = bp4, linetype = "dashed", color = "blue") +
  labs(
    title = "Breakpoint: BirdNET Tally ~ Mean LEQ dBA",
    subtitle = paste("Breakpoint at", round(bp4, 2), "dBA"),
    x = "Mean LEQ dBA",
    y = "BirdNET Tally"
  ) +
  theme_minimal()

(p1 / p2 / p3 / p4) + plot_annotation(title = "Breakpoints")
