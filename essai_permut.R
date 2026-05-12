# =========================
# LIBRARIES
# =========================
library(dplyr)
library(ggplot2)
library(gtools)
library(purrr)

# =========================
# YOUR DATA (replace this)
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
# FUNCTION: BUILD WATERFALL
# =========================
get_waterfall <- function(order, df) {
  
  current <- c(LULC = 90, AGE = 90, POP = 90)
  steps <- list(current)
  
  for (cat in order) {
    current[cat] <- 18
    steps <- append(steps, list(current))
  }
  
  steps_df <- bind_rows(lapply(steps, as.data.frame.list))
  
  result <- steps_df %>%
    left_join(df, by = c("LULC", "AGE", "POP"))
  
  result$step <- c("All 90", order)
  
  # Compute differences
  result <- result %>%
    mutate(
      diff = value - lag(value),
      diff = ifelse(is.na(diff), value, diff)
    )
  
  # Waterfall geometry
  result <- result %>%
    mutate(
      end = cumsum(diff),
      start = lag(end, default = 0)
    )
  
  # Add final total bar
  final_value <- tail(result$value, 1)
  
  final_row <- data.frame(
    LULC = 18, AGE = 18, POP = 18,
    value = final_value,
    step = "All 18",
    diff = final_value,
    start = 0,
    end = final_value
  )
  
  result <- bind_rows(result[1,], result[-1,], final_row)
  
  # Bar types
  result$type <- "change"
  result$type[1] <- "total"
  result$type[nrow(result)] <- "total"
  
  return(result)
}

# =========================
# PERMUTATIONS
# =========================
perms <- permutations(n = 3, r = 3, v = c("LULC", "AGE", "POP"))
perms_list <- split(perms, row(perms))

# =========================
# BUILD ALL DATA
# =========================
all_data <- map2_df(
  perms_list,
  seq_along(perms_list),
  function(p, i) {
    
    order <- as.character(p)
    df_path <- get_waterfall(order, df)
    
    df_path$scenario <- paste(order, collapse = " → ")
    
    return(df_path)
  }
)

# =========================
# PLOT FUNCTION
# =========================
plot_faceted_waterfall <- function(data) {
  
  data <- data %>%
    group_by(scenario) %>%
    mutate(
      x = factor(step, levels = step),
      fill_group = case_when(
        type == "total" ~ "total",
        diff >= 0 ~ "positive",
        TRUE ~ "negative"
      )
    ) %>%
    ungroup()
  
  ggplot(data, aes(x = x)) +
    
    # bars
    geom_rect(aes(
      xmin = as.numeric(x) - 0.4,
      xmax = as.numeric(x) + 0.4,
      ymin = pmin(start, end),
      ymax = pmax(start, end),
      fill = fill_group
    )) +
    
    # connectors
    geom_segment(
      data = data %>% group_by(scenario) %>% slice(-1),
      aes(
        x = as.numeric(x) - 0.4,
        xend = as.numeric(x) + 0.4,
        y = start,
        yend = start
      ),
      color = "grey40",
      linewidth = 0.3
    ) +
    
    # labels
    geom_text(aes(
      y = end,
      label = ifelse(type == "change",
                     sprintf("%+.1f", diff),
                     round(value, 1))
    ), vjust = -0.5, size = 3) +
    
    scale_x_discrete(drop = FALSE) +   # ✅ FIX
    
    scale_fill_manual(values = c(
      "total"    = "black",
      "positive" = "#59A14F",
      "negative" = "#E15759"
    )) +
    
    facet_wrap(~ scenario, ncol = 3, scales = "free_x") +
    
    labs(
      x = NULL,
      y = "Value"
    ) +
    
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "none",
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 30, hjust = 1),
      strip.text = element_text(face = "bold")
    )
}

# =========================
# CREATE & SAVE FIGURE
# =========================
p <- plot_faceted_waterfall(all_data)

ggsave(
  "faceted_waterfall_publication.pdf",
  p,
  width = 12,
  height = 8
)
