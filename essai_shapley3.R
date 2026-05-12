# =========================
# LIBRARIES
# =========================
library(dplyr)
library(gtools)
library(ggplot2)
library(purrr)

# =========================
# USER INPUT
# =========================
vars <- c("LULC", "AGE", "POP")  # <- ADD MORE VARIABLES HERE

start_val <- 90
end_val   <- 18

# =========================
# DATA (MUST CONTAIN ALL COMBINATIONS)
# =========================
df <- expand.grid(setNames(rep(list(c(start_val, end_val)), length(vars)), vars))

# Example values (REPLACE)
set.seed(1)
df$value <- runif(nrow(df), 100, 200)

# =========================
# VALUE FUNCTION
# =========================
get_value <- function(state) {
  df %>%
    filter(across(all_of(vars), ~ . == state[cur_column()])) %>%
    pull(value)
}

# =========================
# ALL PERMUTATIONS
# =========================
k <- length(vars)
perms <- permutations(k, k, vars)

# =========================
# PATH DECOMPOSITION
# =========================
decompose_path <- function(order) {
  
  state <- setNames(rep(start_val, k), vars)
  prev <- get_value(state)
  
  out <- list()
  
  for (i in seq_along(order)) {
    
    var <- order[i]
    state[var] <- end_val
    
    new <- get_value(state)
    
    out[[i]] <- data.frame(
      permutation = paste(order, collapse = "→"),
      variable = var,
      contribution = new - prev
    )
    
    prev <- new
  }
  
  bind_rows(out)
}

# =========================
# ALL PERMUTATION CONTRIBUTIONS
# =========================
perm_df <- map_dfr(1:nrow(perms), ~ decompose_path(perms[.x, ]))

# =========================
# SHAPLEY (EXACT, NO BOOTSTRAP)
# =========================
shapley_df <- perm_df %>%
  group_by(variable) %>%
  summarise(
    shapley = mean(contribution),
    .groups = "drop"
  )

# =========================
# START / END VALUES
# =========================
v_start <- get_value(setNames(rep(start_val, k), vars))
v_end   <- get_value(setNames(rep(end_val, k), vars))

# =========================
# WATERFALL DATA
# =========================
wf <- data.frame(
  step = c("All start", shapley_df$variable, "All end"),
  value = c(v_start, shapley_df$shapley, v_end),
  type = c("total", rep("change", k), "total")
)

# cumulative positioning
wf <- wf %>%
  mutate(
    diff = value,
    diff[1] <- v_start,
    end = cumsum(diff),
    start = lag(end, default = 0),
    x = seq_along(step),
    group = case_when(
      type == "total" ~ "total",
      diff >= 0 ~ "positive",
      TRUE ~ "negative"
    )
  )

# =========================
# WATERFALL PLOT
# =========================
p <- ggplot(wf) +
  
  geom_rect(aes(
    xmin = x - 0.4,
    xmax = x + 0.4,
    ymin = pmin(start, end),
    ymax = pmax(start, end),
    fill = group
  )) +
  
  geom_segment(
    data = wf[-1,],
    aes(
      x = x - 0.4,
      xend = x + 0.4,
      y = start,
      yend = start
    ),
    color = "grey40"
  ) +
  
  geom_text(aes(
    x = x,
    y = end,
    label = round(value, 2)
  ), vjust = -0.6) +
  
  scale_x_continuous(
    breaks = wf$x,
    labels = wf$step
  ) +
  
  scale_fill_manual(values = c(
    "total" = "black",
    "positive" = "#2ca25f",
    "negative" = "#de2d26"
  )) +
  
  labs(
    title = "Generalized Shapley Waterfall",
    x = NULL,
    y = "Value"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank()
  )

# =========================
# EXPORT
# =========================
ggsave("generalized_shapley_waterfall.pdf", p, width = 9, height = 6)

write.csv(perm_df, "permutation_contributions.csv", row.names = FALSE)
write.csv(shapley_df, "shapley_values.csv", row.names = FALSE)