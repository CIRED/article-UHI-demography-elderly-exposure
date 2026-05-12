# =========================
# LIBRARIES
# =========================
library(dplyr)
library(gtools)
library(ggplot2)

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
get_value <- function(lulc, age, pop) {
  df %>%
    filter(LULC == lulc, AGE == age, POP == pop) %>%
    pull(value)
}

# =========================
# SHAPLEY (ONE RUN)
# =========================
compute_shapley <- function() {
  
  vars <- c("LULC", "AGE", "POP")
  perms <- permutations(3, 3, vars)
  
  shap <- c(LULC = 0, AGE = 0, POP = 0)
  v000 <- get_value(90, 90, 90)
  
  for (p in 1:nrow(perms)) {
    
    order <- perms[p, ]
    state <- c(LULC = 90, AGE = 90, POP = 90)
    prev <- v000
    
    for (i in 1:3) {
      
      var <- order[i]
      state[var] <- 18
      
      new <- get_value(state["LULC"], state["AGE"], state["POP"])
      
      shap[var] <- shap[var] + (new - prev)
      prev <- new
    }
  }
  
  shap / factorial(3)
}

# =========================
# BOOTSTRAP
# =========================
set.seed(123)
B <- 500

boot_mat <- replicate(B, compute_shapley(), simplify = "matrix")
boot_mat <- t(boot_mat)

# mean
shap_mean <- colMeans(boot_mat)

# 95% percentile CI (ASYMMETRIC)
ci_low <- apply(boot_mat, 2, quantile, probs = 0.025)
ci_high <- apply(boot_mat, 2, quantile, probs = 0.975)

# =========================
# FINAL DATAFRAME
# =========================
plot_df <- data.frame(
  factor = names(shap_mean),
  estimate = shap_mean,
  ci_low = ci_low,
  ci_high = ci_high
)

# order (optional aesthetic control)
plot_df$factor <- factor(plot_df$factor,
                         levels = plot_df$factor)

# significance flag (CI excludes zero)
plot_df$significant <- !(plot_df$ci_low <= 0 & plot_df$ci_high >= 0)

# =========================
# PUBLICATION-GRADE PLOT
# =========================
p <- ggplot(plot_df, aes(x = factor, y = estimate)) +
  
  # CI ribbons (asymmetric)
  geom_errorbar(
    aes(ymin = ci_low, ymax = ci_high),
    width = 0.15,
    linewidth = 0.8,
    color = "black"
  ) +
  
  # bars
  geom_col(
    aes(fill = estimate > 0),
    width = 0.6,
    color = "black",
    linewidth = 0.4
  ) +
  
  # zero line
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.5) +
  
  # labels
  geom_text(
    aes(label = round(estimate, 2)),
    vjust = -0.6,
    size = 4
  ) +
  
  scale_fill_manual(values = c(
    "TRUE" = "#2ca25f",
    "FALSE" = "#de2d26"
  )) +
  
  labs(
    x = NULL,
    y = "Shapley contribution",
    title = "Shapley Decomposition with 95% Bootstrap Confidence Intervals"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "grey85"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )

# =========================
# SAVE (PUBLICATION QUALITY)
# =========================
ggsave(
  "shapley_ci_publication.pdf",
  p,
  width = 7,
  height = 5,
  device = cairo_pdf
)
