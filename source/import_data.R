library(tidyverse)
library(googlesheets4)
conflicted::conflicts_prefer(dplyr::filter)

icp_key <- "1-Arczbqcx46s9RO4Fmn77Jh0aG4C2aYPdqo18CqREvc"
ic_key  <- "1m_bETGzGrPd49cpCtjYoiGH9cantpY3_2tIBF95DxlM"
sheet   <- "QRY Dataset"
lod_sheet <- "Detectielimieten"

detection_limits <-
  bind_rows(read_sheet(icp_key, lod_sheet),
            read_sheet(ic_key, lod_sheet)) |>
  rename(instrument = "toestel")


data_icp <- read_sheet(icp_key, sheet) |>
  select(-`Test number`) |>
  mutate(instrument = "ICP",
         row_nr = paste0("ICP", sprintf("%04d", row_number()))) |>
  pivot_longer(cols = Al:Zn, names_to = "element", values_to = "value")

data_ic <- read_sheet(ic_key, sheet) |>
  mutate(instrument = "IC",
         row_nr = paste0("IC", sprintf("%04d", row_number()))) |>
  pivot_longer(cols = Cl:Ca,
               names_to = "element",
               values_to = "value")

data_all <- bind_rows(data_icp, data_ic) |>
  transmute(lab_id = Labo_ID,
            type = ifelse(substring(lab_id,1,2) %in% c("BL", "PBL", "SBL"),
                          "blank",
                          ifelse(substring(lab_id,1,1) == "2",
                          "project",
                          "reference")),
         filter = Filter,
         n_filters = `Aantal Filters Labo`,
         field_id = Veldcode,
         sampler = Staalnemer,
         matrix = Matrix,
         instrument,
         row_nr,
         element,
         value) |>
  filter(!is.na(value)) |>
  left_join(detection_limits |> select(instrument, element, lod),
            join_by(instrument, element)) |>
  mutate(element = paste(instrument, element, sep = "_"))

ggplot(data_all, aes(x = log(value + 0.25))) +
  facet_wrap(~element, scales = "free") +
  geom_histogram()


#controlestalen apart houden


