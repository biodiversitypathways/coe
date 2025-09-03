#Start looking at sensitivity thresholds

decs <- decibels_filtered |>
  mutate(hour = hour(Time),
         julian = yday(Time)) |>
  group_by(location) |>
  summarise(mean = mean(LEQ_dB_A),
            sd = sd(LEQ_dB_A)) |>
  ungroup()

ggplot(decs, aes(x=reorder(location,-mean), y=mean)) +
  geom_point() +
  theme_bw() +
  coord_flip()

bb <- coe |>
  wt_tidy_species(remove = c("insect","abiotic","unknown")) |>
  mutate(julian = yday(recording_date_time),
         hour = hour(recording_date_time)) |>
  dplyr::select(location, recording_date_time, julian, hour, aru_task_status, species_code, individual_order, individual_count) |>
  filter(aru_task_status == "Transcribed") |>
  distinct() |>
  inner_join(decs, by = c("location")) |>
  group_by(location, recording_date_time, julian, hour, aru_task_status, mean, sd, species_code) |>
  summarise(individual_order = max(individual_order)) |>
  ungroup()

landcover <- read_csv("/users/alexandremacphail/downloads/alc.csv")

bb <- bb |>
  inner_join(
    landcover |> dplyr::select(location, elevation:Wetland),
    by = "location"
  ) |>
  pivot_longer(
    cols = c(Developed:Wetland),
    names_to = "landcover",
    values_to = "value"
  ) |>
  mutate(value = replace_na(value, 0))

ggplot(bb, aes(x=mean, y=value, colour=landcover)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~landcover) +
  theme_bw()

library(dplyr)
library(mgcv)
library(segmented)
library(MuMIn)

bbi_filtered <- bb %>%
  filter(!is.na(mean), !is.na(individual_order))

# linear model
modi <- lm(
  individual_order ~ mean + julian + hour + landcover * value,
  data = bbi_filtered,
  weights = 1/sd
)

# segmented regression (only modifies 'mean')
segi <- segmented(modi, seg.Z = ~mean, npsi = 1)

# GAM
gam_long <- gam(
  individual_order ~
    s(mean) +
    s(julian, bs = "cc", k = min(12, length(unique(bbi_filtered$julian)) - 1)) +
    s(hour, bs = "cc", k = min(24, length(unique(bbi_filtered$hour)) - 1)) +
    landcover * value,
  data = bbi_filtered,
  weights = 1/sd
)


model.sel(modi, segi, gam_long)




p1 <- ggplot(bbi_filtered, aes(x = mean, y = individual_order)) +
  geom_point(alpha = 0.6, color = "gray40") +
  geom_smooth() +
  #geom_line(aes(y = fit), color = "firebrick", size = 1) +
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

bb1 <- bb1 |>
  inner_join(
    landcover |> dplyr::select(location, elevation:Wetland),
    by = "location"
  ) |>
  pivot_longer(
    cols = c(Developed:Wetland),
    names_to = "landcover",
    values_to = "value"
  ) |>
  mutate(value = replace_na(value, 0))

bb0_filtered <- bb1 |> filter(!is.na(mean), !is.na(rich))
mod2 <- lm(rich ~ mean + julian + hour + landcover * value, data = bb0_filtered, weights = 1/sd)
seg2 <- segmented(mod2, seg.Z = ~mean, npsi = 1)
bp2 <- as.numeric(seg2$psi[1, "Est."])
bb0_filtered$fit <- predict(seg2)

p2 <- ggplot(bb0_filtered, aes(x = mean, y = rich)) +
  geom_point(alpha = 0.6, color = "gray40") +
  geom_smooth() +
  #geom_line(aes(y = fit), color = "firebrick", size = 1) +
  geom_vline(xintercept = bp2, linetype = "dashed", color = "blue") +
  labs(
    title = "Breakpoint: Species Richness per Location ~ Mean LEQ dBA",
    subtitle = paste("Breakpoint at", round(bp2, 2), "dBA"),
    x = "Mean LEQ dBA",
    y = "Richness"
  ) +
  theme_minimal()

bb2 <- wt_download_report(1750, 'ARU', 'birdnet')

#or coebn

bbb <- coebn |>
  mutate(julian = yday(recording_date),
         hour = hour(recording_date)) |>
  dplyr::select(location_name, recording_date, julian, hour, species_code) |>
  distinct() |>
  left_join(decs, by = c("location_name" = "location")) |>
  group_by(location_name, recording_date, julian, hour, mean, sd) |>
  summarise(rich = n_distinct(species_code)) |>
  ungroup()

bb3 <- bbb |> filter(!is.na(mean), !is.na(rich))
mod3 <- lm(rich ~ mean, data = bb3, weights = 1/sd)
seg3 <- segmented(mod3, seg.Z = ~mean, npsi = 1)
bp3 <- as.numeric(seg3$psi[1, "Est."])
bb3$fit <- predict(seg3)

p3 <- ggplot(bb3, aes(x = mean, y = rich)) +
  geom_point(alpha = 0.6, color = "gray40") +
  geom_smooth() +
  #geom_line(aes(y = fit), color = "firebrick", size = 1) +
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
  tally() |>
  ungroup()

bb4 <- bbbb |> filter(!is.na(mean), !is.na(n))
mod4 <- lm(n ~ mean, data = bb4, weights = 1/sd)
seg4 <- segmented(mod4, seg.Z = ~mean, npsi = 1)
anova(mod4, seg4)
davies.test(mod4, seg.Z = ~mean)
bp4 <- as.numeric(seg4$psi[1, "Est."])
bb4$fit <- predict(seg4)
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

bb_total <- coe |>
  mutate(julian = yday(recording_date_time),
         hour = hour(recording_date_time)) |>
  dplyr::select(location, recording_date_time, julian, hour, aru_task_status,
                species_code, individual_order) |>
  filter(aru_task_status == "Transcribed") |>
  distinct() |>
  left_join(decs, by = c("location")) |>
  group_by(location, recording_date_time, julian, hour, aru_task_status, mean, sd, species_code) |>
  summarise(individual_order = max(individual_order)) |>
  ungroup() |>
  group_by(location, recording_date_time, julian, hour, aru_task_status, mean, sd) |>
  summarise(total = sum(individual_order)) |>
  ungroup()

bb5 <- bb_total
mod5 <- lm(total ~ mean, data = bb5, weights = 1/sd)
seg5 <- segmented(mod5, seg.Z = ~mean, npsi = 1)
anova(mod5, seg5)
davies.test(mod5, seg.Z = ~mean)
bp5 <- as.numeric(seg5$psi[1, "Est."])
bb5$fit <- predict(seg5)
gam_mod <- gam(total ~ s(mean), data = bb5)
plot(gam_mod, residuals = TRUE, pch = 19)
summary(gam_mod)

p5 <- ggplot(bb5, aes(x = mean, y = total)) +
  geom_point(alpha = 0.6, color = "gray40") +
  #geom_line(aes(y = fit), color = "firebrick", size = 1) +
  geom_vline(xintercept = bp5, linetype = "dashed", color = "blue") +
  labs(
    title = "Breakpoint: Total Location Count ~ Mean LEQ dBA",
    subtitle = paste("Breakpoint at", round(bp4, 2), "dBA"),
    x = "Mean LEQ dBA",
    y = "Total Location Count"
  ) +
  theme_minimal()


(p1 / p2 / p3 / p4 / p5) + plot_annotation(title = "Breakpoints")
















gam_nb <- gam(individual_order ~ s(mean) + s(julian) + s(hour),
              family = nb(),
              data = bbi_filtered)
summary(gam_nb)
plot(gam_nb, residuals = TRUE, pch = 19, cex = 0.6, shade = TRUE)

newdat <- with(bbi_filtered, data.frame(
  mean   = seq(min(mean, na.rm = TRUE), max(mean, na.rm = TRUE), length.out = 200),
  julian = median(julian, na.rm = TRUE),
  hour   = median(hour, na.rm = TRUE)
))

# Predict with SE
pred <- predict(gam_nb, newdata = newdat, type = "link", se.fit = TRUE)

# Back-transform from link (log for nb) to response scale
newdat$fit <- gam_nb$family$linkinv(pred$fit)
newdat$upper <- gam_nb$family$linkinv(pred$fit + 2 * pred$se.fit)
newdat$lower <- gam_nb$family$linkinv(pred$fit - 2 * pred$se.fit)

# Plot on response scale
ggplot(bbi_filtered, aes(x = mean, y = individual_order)) +
  geom_point(alpha = 0.4, color = "gray40") +
  geom_line(data = newdat, aes(x = mean, y = fit), color = "firebrick", size = 1) +
  geom_ribbon(data = newdat, aes(x = mean, ymin = lower, ymax = upper),
              fill = "firebrick", alpha = 0.2) +
  labs(x = "Mean LEQ dBA", y = "Predicted maximum count") +
  theme_minimal()








bbi_filtered <- bb |>
  filter(!is.na(mean), !is.na(individual_order))

# function to run models and return augmented dataframe
fit_species <- function(df) {
  modi <- lm(individual_order ~ mean, data = df, weights = 1/sd)
  segi <- try(segmented(modi, seg.Z = ~mean, npsi = 1), silent = TRUE)

  # fallback if segmented fails
  if(inherits(segi, "try-error")) {
    df$bpi <- NA
    df$fit <- NA
  } else {
    bpi <- as.numeric(segi$psi[1, "Est."])
    df$bpi <- bpi
    df$fit <- predict(segi)
  }

  gam_mod <- gam(individual_order ~ s(mean), data = df)
  df$gam_fit <- predict(gam_mod)
  df
}

# run per species
bbi_models <- bbi_filtered |>
  group_split(species_code) |>
  map_df(fit_species)

# plot with facets
p <- ggplot(bbi_models, aes(x = mean, y = individual_order)) +
  geom_point(alpha = 0.6, color = "gray40") +
  geom_smooth() +
  geom_line(aes(y = gam_fit), color = "red", linewidth = 1) +
  geom_vline(aes(xintercept = bpi), linetype = "dashed", color = "blue") +
  labs(
    title = "Breakpoint: Maximum Count per Species per Location ~ Mean LEQ dBA",
    x = "Mean LEQ dBA",
    y = "Maximum count"
  ) +
  facet_wrap(~species_code) +
  theme_bw()

