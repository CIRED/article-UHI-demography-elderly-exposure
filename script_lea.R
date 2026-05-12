# ─────────────────────────────────────────────
# 0) LIBRARIES
# ─────────────────────────────────────────────
library(sf)
library(terra)
library(tidyverse)
library(readxl)
library(lubridate)

# ─────────────────────────────────────────────
# 1) LOAD & PREPARE SHAPEFILE
# ─────────────────────────────────────────────
shapefile <- st_read("./data/Communes/COMMUNE_SPF.shp") %>%
  filter(INSEE_REG == "11") %>%
  st_set_crs(3857) %>%        # define CRS (no reprojection)
  st_transform(4326)          # reprojection

# keep a pure attribute table version for joins
shp_attr <- shapefile %>% st_drop_geometry()

# ─────────────────────────────────────────────
# 2) POPULATION HARMONISATION
# ─────────────────────────────────────────────
df2 <- read_excel("./data/pop_sexe_age_communes_1990.xlsx")
df3 <- read_excel("./data/pop_sexe_age_communes_2018.xlsx")

sum_cols <- function(df, cols) {
  rowSums(df[, cols], na.rm = TRUE)
}

# --- define age groups
dic_age_df2 <- list(
  "65-H" = c("ageq_rec01s1rpop1990","ageq_rec02s1rpop1990","ageq_rec03s1rpop1990",
             "ageq_rec04s1rpop1990","ageq_rec05s1rpop1990","ageq_rec06s1rpop1990",
             "ageq_rec07s1rpop1990","ageq_rec08s1rpop1990","ageq_rec09s1rpop1990",
             "ageq_rec10s1rpop1990","ageq_rec11s1rpop1990","ageq_rec12s1rpop1990",
             "ageq_rec13s1rpop1990"),
  "65+H" = c("ageq_rec14s1rpop1990","ageq_rec15s1rpop1990","ageq_rec16s1rpop1990",
             "ageq_rec17s1rpop1990","ageq_rec18s1rpop1990","ageq_rec19s1rpop1990",
             "ageq_rec20s1rpop1990"),
  "65-F" = c("ageq_rec01s2rpop1990","ageq_rec02s2rpop1990","ageq_rec03s2rpop1990",
             "ageq_rec04s2rpop1990","ageq_rec05s2rpop1990","ageq_rec06s2rpop1990",
             "ageq_rec07s2rpop1990","ageq_rec08s2rpop1990","ageq_rec09s2rpop1990",
             "ageq_rec10s2rpop1990","ageq_rec11s2rpop1990","ageq_rec12s2rpop1990",
             "ageq_rec13s2rpop1990"),
  "65+F" = c("ageq_rec14s2rpop1990","ageq_rec15s2rpop1990","ageq_rec16s2rpop1990",
             "ageq_rec17s2rpop1990","ageq_rec18s2rpop1990","ageq_rec19s2rpop1990",
             "ageq_rec20s2rpop1990")
)

for (name in names(dic_age_df2)) {
  df2[[name]] <- sum_cols(df2, dic_age_df2[[name]])
}

df2 <- df2 %>%
  mutate(
    `65+` = `65+H` + `65+F`,
    `65-` = `65-H` + `65-F`,
    popTOT = `65+` + `65-`
  )

df3 <- df3 %>%
  mutate(
    `65+` = `65+H` + `65+F`,
    `65-` = `65-H` + `65-F`,
    popTOT = `65+` + `65-`
  )

# --- harmonisation communes
com_equi <- c(
  "77028"="77433","77149"="77109","77166"="77316","77170"="77316",
  "77299"="77316","77399"="77504","77491"="77316",
  "78251"="78551","78503"="78320","78524"="78158",
  "91182"="91228","91222"="91390","95259"="95040"
)

for (i in names(com_equi)) {
  target <- com_equi[[i]]
  
  df2[df2$INSEE == target, c("65+","65-")] <-
    df2[df2$INSEE == target, c("65+","65-")] +
    df2[df2$INSEE == i, c("65+","65-")]
  
  df2 <- df2 %>% filter(INSEE != i)
}

# fractions
df2 <- df2 %>% mutate(frac_age_90 = `65+` / popTOT)
df3 <- df3 %>% mutate(frac_age_18 = `65+` / popTOT)

# ─────────────────────────────────────────────
# 3) IPR CLASSIFICATION
# ─────────────────────────────────────────────
zones <- read_csv("./data/IRIS/zones_IPR.csv") %>%
  mutate(Insee = as.character(Insee))

get_codes <- function(label) {
  zones %>% filter(EntiteGeo_SDRIF.E == label) %>% pull(Insee)
}

iris_hyper <- shapefile %>% filter(INSEE_COM %in% get_codes("Hypercentre"))
iris_rural <- shapefile %>% filter(INSEE_COM %in% get_codes("Communes rurales"))

# ─────────────────────────────────────────────
# 4) NETCDF PROCESSING (terra)
# ─────────────────────────────────────────────

read_nc_stack <- function(path_pattern) {
  files <- Sys.glob(path_pattern)
  rast(files)
}

# example: TN 2018
tn_2018 <- read_nc_stack("./data/tn_comSPF_NFR010D/tn_comSPF_NFR010D_*.nc")

# extract time
dates <- as.Date(time(tn_2018))

# keep summer
summer_idx <- which(month(dates) %in% c(6,7,8))
tn_summer <- tn_2018[[summer_idx]]

# count tropical nights (>20°C)
tn_binary <- tn_summer > 20
n_trop <- app(tn_binary, sum, na.rm = TRUE)

# extract to polygons
nuit_trop_df <- terra::extract(n_trop, vect(shapefile), fun = mean, na.rm = TRUE) %>%
  as_tibble() %>%
  rename(nuit_trop_18 = 2)

shapefile <- shapefile %>%
  mutate(ID = row_number()) %>%
  left_join(nuit_trop_df, by = c("ID" = "ID"))

# ─────────────────────────────────────────────
# 5) MEAN TEMPERATURES
# ─────────────────────────────────────────────
compute_mean_var <- function(var, simu) {
  r <- read_nc_stack(paste0("/cnrm/ville/USERS/corneillel/data/Communes/", simu, "/", var, "_*.nc"))
  dates <- as.Date(time(r))
  idx <- which(month(dates) %in% 5:9)
  r_sub <- r[[idx]]
  app(r_sub, mean, na.rm = TRUE)
}

tn_mean_2018 <- compute_mean_var("tn", "NFR010D")
tn_mean_1990 <- compute_mean_var("tn", "NFR010C")

tn_diff <- tn_mean_2018 - tn_mean_1990

# extract to polygons
extract_to_sf <- function(r, name) {
  terra::extract(r, vect(shapefile), fun = mean, na.rm = TRUE) %>%
    as_tibble() %>%
    rename(!!name := 2)
}

shapefile <- shapefile %>%
  mutate(ID = row_number()) %>%
  left_join(extract_to_sf(tn_mean_2018, "tn_mean_2018"), by = c("ID"="ID")) %>%
  left_join(extract_to_sf(tn_mean_1990, "tn_mean_1990"), by = c("ID"="ID")) %>%
  left_join(extract_to_sf(tn_diff, "tn_diff"), by = c("ID"="ID"))

# ─────────────────────────────────────────────
# 6) FINAL OUTPUT
# ─────────────────────────────────────────────
st_write(shapefile, "output_communes_enriched.geojson", delete_dsn = TRUE)