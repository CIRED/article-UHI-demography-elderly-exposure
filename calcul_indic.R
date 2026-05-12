library(tidyverse)
library(ncdf4)
library(stars)   # or terra depending on your workflow
library(purrr)
library(dplyr)

# shapefile ---------------------------------------------------------------
shapefile_init <- st_read("./data/Communes/COMMUNE_SPF.shp") 
st_crs(shapefile_init) <- 3857


shapefile <- shapefile_init%>%
  filter(INSEE_REG == "11") %>%
  st_transform(4326)



# NOMBRE DE NUITS TROPICALES ----

files <- list.files("./data/NFR010D/",
                    pattern = "^tn",
                    full.names = TRUE)

NFR010D <- map_dfr(files, ~ read_ncdf(.x) %>% as.data.frame())

nuit_trop_COM_NFR010D <- NFR010D %>%
  mutate(year = year(date)) %>%
  group_by(year,COM) %>%
  summarise(nb_nuit_trop = sum(tn > 20, na.rm = TRUE)) %>% 
  mutate(COM = as.character(COM))


files <- list.files("./data/NFR010C/",
                    pattern = "^tn",
                    full.names = TRUE)

NFR010C <- map_dfr(files, ~ read_ncdf(.x) %>% as.data.frame())

nuit_trop_COM_NFR010C <- NFR010C %>%
  mutate(year = year(date)) %>%
  group_by(year,COM) %>%
  summarise(nb_nuit_trop = sum(tn > 20, na.rm = TRUE)) %>% 
  mutate(COM = as.character(COM))

shapefile2 <- shapefile %>%
  left_join(nuit_trop_COM_NFR010D, by = c("INSEE_COM" = "COM"))%>% 
  rename("nb_nuit_trop_D" = "nb_nuit_trop") %>% 
  left_join(nuit_trop_COM_NFR010C, by = c("INSEE_COM" = "COM","year" = "year")) %>% 
  rename("nb_nuit_trop_C" = "nb_nuit_trop")

tmp <- shapefile2 %>% 
  group_by(geometry) %>% 
  summarise(nb_nuit_trop = (sum(nb_nuit_trop_C)/22 > 8) )
  
plot(tmp
     )

# Personnes exposées aux nuits tropicales -------------------------------


var <- "tn"

ee <- stars::read_ncdf("/cnrm/ville/USERS/corneillel/data/Communes/NFR010C/tn_comSPF_NFR010C_*.nc")
ff <- stars::read_ncdf("/cnrm/ville/USERS/corneillel/data/Communes/NFR010D/tn_comSPF_NFR010D_*.nc")

ee <- ee %>% filter(lubridate::month(date) %in% 5:9)
ff <- ff %>% filter(lubridate::month(date) %in% 5:9)

# Dates NT
dates_nt_90 <- ee %>%
  as_tibble() %>%
  filter(tn > 20) %>%
  pull(date) %>%
  unique()

ee <- ee %>% filter(date %in% dates_nt_90)
ff <- ff %>% filter(date %in% dates_nt_90)

# dictionnaires → listes nommées
dic_NT_1990 <- ee %>%
  as_tibble() %>%
  filter(tn > 20) %>%
  group_by(date) %>%
  summarise(COM = list(unique(COM))) %>%
  deframe()

dic_NT_2018 <- ff %>%
  as_tibble() %>%
  filter(tn > 20) %>%
  group_by(date) %>%
  summarise(COM = list(unique(COM))) %>%
  deframe()

# (3) TEMPÉRATURES MOYENNES

simu <- "NFR010D"
simu2 <- "NFR010C"

for (var in c("tn", "tm", "tx")) {
  
  aa2 <- stars::read_ncdf(paste0("/cnrm/ville/USERS/corneillel/data/Communes/", simu, "/", var, "_comSPF_", simu, "_*.nc"))
  bb2 <- stars::read_ncdf(paste0("/cnrm/ville/USERS/corneillel/data/Communes/", simu2, "/", var, "_comSPF_", simu2, "_*.nc"))
  
  aa2 <- aa2 %>% filter(lubridate::month(date) %in% 5:9)
  bb2 <- bb2 %>% filter(lubridate::month(date) %in% 5:9)
  
  mean_2018 <- aa2 %>%
    as_tibble() %>%
    group_by(COM) %>%
    summarise(val = mean(.data[[var]], na.rm = TRUE))
  
  mean_1990 <- bb2 %>%
    as_tibble() %>%
    group_by(COM) %>%
    summarise(val = mean(.data[[var]], na.rm = TRUE))
  
  diff <- mean_2018 %>%
    left_join(mean_1990, by = "COM", suffix = c("_2018", "_1990")) %>%
    mutate(val_diff = val_2018 - val_1990)
  
  shapefile <- shapefile %>%
    left_join(mean_2018, by = c("INSEE_COM" = "COM")) %>%
    rename(!!paste0(var, "_mean_2018") := val) %>%
    left_join(mean_1990, by = c("INSEE_COM" = "COM")) %>%
    rename(!!paste0(var, "_mean_1990") := val) %>%
    left_join(diff %>% select(COM, val_diff), by = c("INSEE_COM" = "COM")) %>%
    rename(!!paste0(var, "_mean_diff_2018_1990") := val_diff)
}


#### end