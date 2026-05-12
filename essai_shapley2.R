# =========================
# LIBRARIES
# =========================
library(dplyr)
library(gtools)
library(ggplot2)
library(purrr)

# =========================
# DATA (REPLACE THIS)
# =========================
exposure_tn_65_plus_abs<-read_delim("./data/exposure_tn_classes_65+_abs.csv",delim=";") %>% 
  rename ("temperature" = "...1") 

exposure_tn_65_plus_new <- exposure_tn_65_plus_abs %>% 
  pivot_longer(cols=-c(temperature)) %>% 
  separate_wider_delim(cols = name,delim="_",names = c("LULC","POP","AGE")) %>% 
  filter(!grepl("REP",AGE)) %>% 
  separate_wider_position(LULC,widths = c( 4,LULC = 2)) %>% 
  separate_wider_position(POP,widths = c( 3,POP = 2)) %>% 
  separate_wider_position(AGE,widths = c( 3,AGE = 2)) %>% 
  filter(temperature == "[16;17[") %>% 
  dplyr::select(-temperature) %>% 
  mutate(LULC=as.double(LULC),
         POP=as.double(POP),
         AGE=as.double(AGE))


df <- exposure_tn_65_plus_new


# =========================
# VALUE FUNCTION
# =========================
get_value <- function(state) {
  df %>%
    filter(
      LULC == state["LULC"],
      AGE  == state["AGE"],
      POP  == state["POP"]
    ) %>%
    pull(value)
}

# =========================
# ALL PERMUTATIONS
# =========================
vars <- c("LULC", "AGE", "POP")
perms <- permutations(3, 3, vars)

# =========================
# SINGLE PATH DECOMPOSITION
# =========================
decompose_path <- function(order) {
  
  state <- c(LULC = 90, AGE = 90, POP = 90)
  prev <- get_value(state)
  
  out <- list()
  
  for (i in 1:3) {
    
    var <- order[i]
    state[var] <- 18
    
    new <- get_value(state)
    
    out[[i]] <- data.frame(
      permutation = paste(order, collapse = "→"),
      step = var,
      contribution = new - prev
    )
    
    prev <- new
  }
  
  bind_rows(out)
}

# =========================
# ALL PERMUTATION CONTRIBUTIONS (DATAFRAME YOU REQUESTED)
# =========================
perm_df <- bind_rows(lapply(1:nrow(perms), function(i) {
  decompose_path(perms[i, ])
}))

# =========================
# BOOTSTRAP SHAPLEY (DISTRIBUTION)
# =========================
B <- 500

boot_list <- vector("list", B)

set.seed(123)

for (b in 1:B) {
  
  boot_list[[b]] <- bind_rows(lapply(1:nrow(perms), function(i) {
    
    decompose_path(perms[i, ]) %>%
      mutate(boot = b)
    
  }))
}

boot_df <- bind_rows(boot_list)

# =========================
# SHAPLEY SUMMARY (MEAN + CI)
# =========================
shapley_summary <- boot_df %>%
  group_by(step) %>%
  summarise(
    mean = mean(contribution),
    ci_low = quantile(contribution, 0.025),
    ci_high = quantile(contribution, 0.975),
    .groups = "drop"
  )

# =========================
# FINAL WATERFALL DATA
# =========================
v0 <- get_value(c(LULC=90, AGE=90, POP=90))
v1 <- get_value(c(LULC=18, AGE=18, POP=18))

wf <- data.frame(
  step = c("All 90", shapley_summary$step, "All 18"),
  value = c(v0, shapley_summary$mean, v1),
  ci_low = c(NA, shapley_summary$ci_low, NA),
  ci_high = c(NA, shapley_summary$ci_high, NA),
  type = c("total", rep("change", 3), "total")
)

wf <- wf %>%
  mutate(
    x = seq_along(step),
    group = case_when(
      type == "total" ~ "total",
      value >= 0 ~ "positive",
      TRUE ~ "negative"
    )
  )

# =========================
# PLOT (WATERFALL + CI)
# =========================
p <- ggplot(wf) +
  
  # CI ribbons
  geom_rect(
    data = wf %>% filter(type == "change"),
    aes(
      xmin = x - 0.25,
      xmax = x + 0.25,
      ymin = ci_low,
      ymax = ci_high
    ),
    fill = "grey80",
    alpha = 0.6
  ) +
  
  # bars
  geom_rect(aes(
    xmin = x - 0.4,
    xmax = x + 0.4,
    ymin = pmin(value, lag(value, default = v0)),
    ymax = pmax(value, lag(value, default = v0)),
    fill = group
  )) +
  
  # connectors
  geom_segment(
    data = wf[-1,],
    aes(
      x = x - 0.4,
      xend = x + 0.4,
      y = lag(value, default = v0),
      yend = lag(value, default = v0)
    ),
    color = "grey40"
  ) +
  
  # labels
  geom_text(aes(
    x = x,
    y = value,
    label = round(value, 2)
  ), vjust = -0.6) +
  
  scale_fill_manual(values = c(
    "total" = "black",
    "positive" = "#2ca25f",
    "negative" = "#de2d26"
  )) +
  
  scale_x_continuous(
    breaks = wf$x,
    labels = wf$step
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank()
  ) +
  
  labs(
    title = "Shapley Waterfall with Bootstrap Uncertainty",
    x = NULL,
    y = "Value"
  )

# =========================
# EXPORTS
# =========================
ggsave("shapley_waterfall_final.pdf", p, width = 9, height = 6)
write.csv(perm_df, "permutation_contributions.csv", row.names = FALSE)
write.csv(boot_df, "bootstrap_shapley_distribution.csv", row.names = FALSE)
