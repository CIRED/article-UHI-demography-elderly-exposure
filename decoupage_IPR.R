library(tidyverse)

df <- read_csv("/cnrm/ville/USERS/corneillel/data/IRIS/zones_IPR.csv") %>%
  mutate(Insee = as.character(Insee))

split_lists <- df %>%
  group_split(EntiteGeo_SDRIF.E) %>%
  set_names(unique(df$EntiteGeo_SDRIF.E))

get_codes <- function(label) {
  df %>%
    filter(EntiteGeo_SDRIF.E == label) %>%
    pull(Insee)
}

hyper <- get_codes("Hypercentre")
rural <- get_codes("Communes rurales")
petit <- get_codes("Petites villes")
moyen <- get_codes("Villes moyennes")
agglo <- get_codes("Couronne d'agglomération")
coeur_agglo <- get_codes("Cœur d'agglomération")

iris_hyper <- shapefile %>% filter(INSEE_COM %in% hyper)
iris_rural <- shapefile %>% filter(INSEE_COM %in% rural)
iris_petit <- shapefile %>% filter(INSEE_COM %in% petit)
iris_moyen <- shapefile %>% filter(INSEE_COM %in% moyen)
iris_agglo <- shapefile %>% filter(INSEE_COM %in% agglo)
iris_coeur_agglo <- shapefile %>% filter(INSEE_COM %in% coeur_agglo)