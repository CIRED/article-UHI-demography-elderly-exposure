library(tidyverse)
library(readxl)

df2 <- read_excel("pop_sexe_age_communes_1990.xlsx")
df3 <- read_excel("pop_sexe_age_communes_2018.xlsx")

sum_cols <- function(df, cols) {
  df %>%
    mutate(val = rowSums(select(., all_of(cols)), na.rm = TRUE)) %>%
    pull(val)
}

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
    `65-` = `65-H` + `65-F`
  )

# Harmonisation communes
com_equi <- c(
  "77028"="77433","77149"="77109","77166"="77316","77170"="77316",
  "77299"="77316","77399"="77504","77491"="77316","78251"="78551",
  "78503"="78320","78524"="78158","91182"="91228","91222"="91390",
  "95259"="95040"
)

for (i in names(com_equi)) {
  target <- com_equi[[i]]
  
  df2[df2$INSEE == target, c("65+","65-","65+H","65+F","65-H","65-F")] <-
    df2[df2$INSEE == target, c("65+","65-","65+H","65+F","65-H","65-F")] +
    df2[df2$INSEE == i, c("65+","65-","65+H","65+F","65-H","65-F")]
  
  df2 <- df2 %>% filter(INSEE != i)
}

df2 <- df2 %>%
  mutate(popTOT = `65+` + `65-`)


dic_age_df3 <- list(
  "65+H" = c("SEXE1_AGEPYR1065","SEXE1_AGEPYR1080"),
  		
  "65+F" = c("SEXE2_AGEPYR1065","SEXE2_AGEPYR1080")
)

for (name in names(dic_age_df3)) {
  df3[[name]] <- sum_cols(df3, dic_age_df3[[name]])
}




# fractions
df2 <- df2 %>%
  mutate(frac_age_90 = `65+` / popTOT)

df3 <- df3 %>%
  mutate(frac_age_18 = `65+` / popTOT)

df2 <- df2 %>%
  mutate(
    `65+_inv` = popTOT * df3$frac_age_18,
    `65-_inv` = popTOT * (1 - df3$frac_age_18)
  )

df3 <- df3 %>%
  mutate(
    `65+_inv` = popTOT * df2$frac_age_90,
    `65-_inv` = popTOT * (1 - df2$frac_age_90)
  )