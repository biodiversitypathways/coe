library(dplyr)
library(purrr)
library(fs)
library(tuneR)
library(soundecology)
library(stringr)
library(tidyr)

coe_r <- wt_download_report(1750, 'ARU', 'recording') |> mutate(year = year(recording_date_time)) |> filter(year == 2025)
wt_download_media(coe_r, output = "/users/alexandremacphail/R/coe/recs", type = "recording")

ndsi_summary <- dir_ls("/users/alexandremacphail/R/coe/recs", glob = "*.flac") %>%
  as_tibble() %>%
  rename(file_path = value) %>%
  mutate(file_path = as.character(file_path)) %>%
  mutate(wav_path = map_chr(file_path, ~ {
    wav_path <- str_replace(.x, "\\.flac$", ".wav")
    av::av_audio_convert(.x, wav_path)
    wav_path
  })) %>%
  # run NDSI on each wav
  mutate(ndsi_obj = map(wav_path, ~ {
    wav <- readWave(.x)
    soundecology::ndsi(wav, anthro_min = 0, anthro_max = 2000,
                       bio_min = 2000, bio_max = 12000)
  })) %>%
  unnest(ndsi_obj)

ndsi_summary %>%
  unnest_longer(ndsi_obj) %>%
  group_by(file_path) %>%
  slice(1:2) %>%
  summarise(mean_ndsi = mean(ndsi_obj)) %>%
  ungroup() %>%
  mutate(base = tools::file_path_sans_ext(basename(file_path))) %>%
  separate(base, into = c("location", "recording_date_time"), sep = "_") %>%
  group_by(location) %>%
  summarise(mean_ndsi = mean(mean_ndsi)) %>%
  ungroup() %>%
  inner_join(decs, by = "location") %>%
  ggplot(aes(x=mean,y=mean_ndsi)) +
  geom_point() +
  geom_smooth()
