#Figures - Revised - assistance from LLM
# -----------------------------
# BAR PLOTS - Figure 1
# AJRCCM-style revised version
# -----------------------------

library(ggplot2)
library(ggpattern)
library(scales)
library(patchwork)

# -----------------------------
# DATA

df_a <- data.frame(
  category = c("RSV LRTI-attributable", "Non-RSV LRTI-attributable"),
  value = c(29299, 293159)
)

# -----------------------------
# GLOBAL THEME
# -----------------------------

theme_ajrccm <- theme_classic(
  base_family = "Arial",
  base_size = 10
) +
  theme(
    text = element_text(family = "Arial"),
    
    legend.position = "none",
    
    plot.title = element_text(
      face = "bold",
      size = 10,
      hjust = 0.5,
      lineheight = 0.95,
      margin = margin(b = 6)
    ),
    
    axis.title.y = element_text(
      size = 10,
      margin = margin(r = 10)
    ),
    
    axis.text.x = element_text(
      face = "bold",
      size = 8.5,
      margin = margin(t = 5)
    ),
    
    axis.text.y = element_text(size = 8),
    
    axis.line = element_line(linewidth = 0.7),
    axis.ticks = element_line(linewidth = 0.7),
    
    panel.grid = element_blank(),
    
    # Larger left margin fixes clipping
    plot.margin = margin(
      t = 8,
      r = 12,
      b = 8,
      l = 24
    )
  )

# -----------------------------
# PANEL A
# -----------------------------

p1 <- ggplot(
  df_a,
  aes(
    x = "Total wheeze/asthma cases",
    y = value,
    fill = category
  )
) +
  
  geom_bar(
    stat = "identity",
    width = 0.42,
    color = "black",
    linewidth = 0.6
  ) +
  
  annotate(
    "text",
    x = 1,
    y = sum(df_a$value) + 12000,
    label = "322,458",
    fontface = "bold",
    family = "Arial",
    size = 3.3
  ) +
  
  scale_fill_manual(
    values = c(
      "RSV LRTI-attributable" = "#003B8E",
      "Non-RSV LRTI-attributable" = "grey80"
    )
  ) +
  
  scale_y_continuous(
    labels = comma,
    limits = c(0, 340000),
    expand = expansion(mult = c(0, 0.02))
  ) +
  
  # Fixes bar width appearance by adding x padding
  scale_x_discrete(expand = expansion(mult = c(0.45, 0.45))) +
  
  labs(
    title = "A. All childhood wheeze/asthma cases",
    y = "Number of cases",
    x = NULL
  ) +
  
  theme_ajrccm

# -----------------------------
# PANEL B
# -----------------------------

p2 <- ggplot() +
  
  # Main solid portion
  geom_rect(
    aes(
      xmin = 0.79,
      xmax = 1.21,
      ymin = 0,
      ymax = 21259
    ),
    fill = "#003B8E",
    color = "black",
    linewidth = 0.6
  ) +
  
  # Patterned portion
  geom_rect_pattern(
    aes(
      xmin = 0.79,
      xmax = 1.21,
      ymin = 21259,
      ymax = 29299
    ),
    fill = "white",
    color = "#003B8E",
    pattern = "stripe",
    pattern_fill = "#003B8E",
    pattern_colour = "#003B8E",
    pattern_angle = 45,
    pattern_density = 0.5,
    pattern_spacing = 0.02,
    linewidth = 0.6
  ) +
  
  annotate(
    "text",
    x = 1,
    y = 29299 + 1400,
    label = "29,299",
    fontface = "bold",
    family = "Arial",
    size = 3.3
  ) +
  
  # Critical fix:
  # make x limits symmetric so bar width visually matches panel A
  scale_x_continuous(
    limits = c(0.5, 1.5),
    breaks = 1,
    labels = "RSV LRTI-attributable\nwheeze/asthma cases",
    expand = expansion(mult = c(0, 0))
  ) +
  
  scale_y_continuous(
    labels = comma,
    limits = c(0, 32000),
    expand = expansion(mult = c(0, 0.03))
  ) +
  
  labs(
    title = "B. RSV LRTI-attributable\nwheeze/asthma cases",
    y = "Number of cases",
    x = NULL
  ) +
  
  theme_ajrccm

# -----------------------------
# COMBINE WITH SPACING
# -----------------------------

final_plot <- p1 + plot_spacer() + p2 +
  plot_layout(widths = c(1, 0.18, 1))

# -----------------------------
# DISPLAY
# -----------------------------

final_plot

# -----------------------------
# SAVE FIGURE
# AJRCCM-friendly exports
# -----------------------------

# Vector PDF (preferred for submission)

ggsave(
  filename = "Figure1_barplots_AJRCCM.pdf",
  plot = final_plot,
  width = 180,
  height = 110,
  units = "mm",
  device = cairo_pdf,
  bg = "white"
)

ggsave(
  filename = "Figure1_barplots_AJRCCM.tiff",
  plot = final_plot,
  width = 200,
  height = 110,
  units = "mm",
  dpi = 600,
  compression = "lzw",
  bg = "white"
)

# Decreased width of the bar-plots
# Right graph text is cut off


# -----------------------------
# FOREST PLOT
# -----------------------------

p <- ggplot(
  df,
  aes(
    x = PAF,
    y = RiskFactor
  )
) +
  
  # Reference line
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    color = "gray50",
    linewidth = 0.7
  ) +
  
  # Confidence intervals
  geom_errorbar(
    aes(
      xmin = LowerCI,
      xmax = UpperCI
    ),
    orientation = "y",
    height = 0.18,
    linewidth = 0.9,
    color = "#0B4FA8"
  ) +
  
  # Points
  geom_point(
    size = 3,
    color = "#0B4FA8"
  ) +
  
  # -----------------------------
# RIGHT-SIDE COLUMNS
# -----------------------------

# Exposure window
geom_text(
  aes(
    x = 24,
    label = ExposureWindow
  ),
  hjust = 0,
  size = 3.2
) +
  
  # RR + CI
  geom_text(
    aes(
      x = 39,
      label = RR_CI
    ),
    hjust = 0,
    size = 3.2
  ) +
  
  # Prevalence
  geom_text(
    aes(
      x = 58,
      label = paste0(Prevalence, "%")
    ),
    hjust = 0,
    size = 3.2
  ) +
  
  # -----------------------------
# COLUMN HEADERS
# -----------------------------

# LEFT HEADER (moved further left for centering)
annotate(
  "text",
  x = -3.5,
  y = 7.7,
  label = "Modifiable risk factor",
  fontface = "bold",
  hjust = 1,
  size = 3.5
) +
  
  annotate(
    "text",
    x = 24,
    y = 7.7,
    label = "Exposure window",
    fontface = "bold",
    hjust = 0,
    size = 3.5
  ) +
  
  annotate(
    "text",
    x = 39,
    y = 7.7,
    label = "RR (95% CI)",
    fontface = "bold",
    hjust = 0,
    size = 3.5
  ) +
  
  annotate(
    "text",
    x = 58,
    y = 7.7,
    label = "Prevalence",
    fontface = "bold",
    hjust = 0,
    size = 3.5
  ) +
  
  # -----------------------------
# BOTTOM LABEL (BELOW AXIS NUMBERS)
# -----------------------------

annotate(
  "text",
  x = 10,
  y = -0.55,
  label = "Population attributable fraction (%)",
  size = 3.8
) +
  
  # -----------------------------
# AXES
# -----------------------------

scale_x_continuous(
  limits = c(-2, 70),
  breaks = seq(0, 20, 5),
  expand = expansion(mult = c(0.01, 0.02))
) +
  
  scale_y_discrete(
    expand = expansion(mult = c(0.12, 0.22))
  ) +
  
  labs(
    x = NULL,
    y = NULL
  ) +
  
  coord_cartesian(clip = "off") +
  
  # -----------------------------
# THEME
# -----------------------------

theme_minimal(base_size = 11) +
  
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    
    axis.text.y = element_text(
      size = 10,
      hjust = 1
    ),
    
    axis.text.x = element_text(
      size = 10
    ),
    
    plot.margin = margin(
      t = 25,
      r = 45,
      b = 60,
      l = 20
    )
  )

# -----------------------------
# DISPLAY
# -----------------------------

p

# OPTIONAL: TIFF for journals
ggsave(
  filename = "Figure2_forestplot_highres.tiff",
  plot = p,
  width = 12,
  height = 6,
  units = "in",
  dpi = 600,
  compression = "lzw",
  bg = "white"
)

# -----------------------------
# FIGURE 2
# Population Attributable Fraction (PAF)
# Childhood wheeze/asthma risk factors
# AJRCCM-style forest plot
# -----------------------------

library(ggplot2)
library(dplyr)
library(patchwork)

# -----------------------------
# DATA
# -----------------------------

df <- data.frame(
  RiskFactor = factor(
    c(
      "RSV LRTI",
      "Secondhand smoke exposure",
      "Early-life allergen sensitization",
      "Childhood overweight/obesity",
      "Prenatal smoking",
      "Traffic-related air pollution",
      "Indoor mold exposure"
    ),
    levels = rev(c(
      "RSV LRTI",
      "Secondhand smoke exposure",
      "Early-life allergen sensitization",
      "Childhood overweight/obesity",
      "Prenatal smoking",
      "Traffic-related air pollution",
      "Indoor mold exposure"
    ))
  ),
  
  ExposureWindow = c(
    "Infancy",
    "Postnatal",
    "Infancy–early\nchildhood",
    "Childhood",
    "Prenatal",
    "Early childhood",
    "Early childhood"
  ),
  
  RR_CI = c(
    "3.8 (3.2–4.6)",
    "2.8 (2.1–3.9)",
    "1.1 (0.9–1.3)",
    "1.3 (1.2–1.4)",
    "1.9 (1.4–2.5)",
    "1.1 (1.0–1.3)",
    "1.1 (0.9–1.3)"
  ),
  
  Prevalence = c(
    20,
    41,
    6,
    9,
    8,
    4,
    4
  ),
  
  PAF = c(
    9.1,
    11.5,
    9.5,
    4.1,
    6.7,
    0.5,
    0.4
  ),
  
  LowerCI = c(
    2.5,
    8.5,
    6.0,
    3.2,
    2.9,
    0.0,
    -0.4
  ),
  
  UpperCI = c(
    19.2,
    14.6,
    14.4,
    5.9,
    11.4,
    1.7,
    1.4
  )
)

# -----------------------------
# FOREST PLOT PANEL
# -----------------------------

forest_plot <- ggplot(
  df,
  aes(
    x = PAF,
    y = RiskFactor
  )
) +
  
  # Reference line
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    colour = "grey50",
    linewidth = 0.7
  ) +
  
  # Confidence intervals
  geom_errorbar(
    aes(
      xmin = LowerCI,
      xmax = UpperCI
    ),
    orientation = "y",
    height = 0.18,
    linewidth = 0.9,
    colour = "#003B8E"
  ) +
  
  # Points
  geom_point(
    size = 3,
    colour = "#003B8E"
  ) +
  
  scale_x_continuous(
    limits = c(-2, 25),
    breaks = seq(0, 50, 5),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  
  labs(
    x = "Population attributable fraction (%)",
    y = NULL
  ) +
  
  theme_minimal(
    base_family = "Arial",
    base_size = 9
  ) +
  
  theme(
    # Vertical gridlines like published figure
    panel.grid.major.x = element_line(
      colour = "grey85",
      linewidth = 0.5
    ),
    
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    
    axis.text.y = element_text(
      size = 7.5,
      hjust = 1
    ),
    
    axis.text.x = element_text(
      size = 8
    ),
    
    axis.title.x = element_text(
      size = 9,
      margin = margin(t = 8)
    ),
    
    plot.margin = margin(
      t = 20,
      r = 10,
      b = 20,
      l = 10
    )
  )

# -----------------------------
# TABLE PANEL
# -----------------------------

table_df <- data.frame(
  y = seq(length(df$RiskFactor), 1),
  Exposure = df$ExposureWindow,
  RR = df$RR_CI,
  Prev = paste0(df$Prevalence, "%")
)

table_plot <- ggplot(table_df) +
  
  # Exposure window
  
  geom_text(
    aes(
      x = 1.5,
      y = y,
      label = Exposure
    ),
    family = "Arial",
    hjust = 0.5,
    size = 2.0
  ) +
  
  # RR
  
  geom_text(
    aes(
      x = 5.5,
      y = y,
      label = RR
    ),
    family = "Arial",
    hjust = 0.5,
    size = 2.0
  ) +
  
  # Prevalence
  
  geom_text(
    aes(
      x = 9.5,
      y = y,
      label = Prev
    ),
    family = "Arial",
    hjust = 0.5,
    size = 2.0
  ) +
  
  # -----------------------------
# HEADERS
# -----------------------------

annotate(
  "text",
  x = 1.5,
  y = 8,
  label = "Exposure\nwindow",
  fontface = "bold",
  family = "Arial",
  hjust = 0.5,
  size = 2.4
) +
  
  annotate(
    "text",
    x = 5.5,
    y = 8,
    label = "RR (95% CI)",
    fontface = "bold",
    family = "Arial",
    hjust = 0.5,
    size = 2.4
  ) +
  
  annotate(
    "text",
    x = 9.5,
    y = 8,
    label = "Prevalence",
    fontface = "bold",
    family = "Arial",
    hjust = 0.5,
    size = 2.4
  ) +
  
  coord_cartesian(
    xlim = c(0, 10.5),
    ylim = c(0.5, 8.4),
    clip = "off"
  ) +
  
  theme_void() +
  
  theme(
    plot.margin = margin(
      t = 20,
      r = 20,
      b = 20,
      l = 20
    )
  )

# -----------------------------
# COMBINE PANELS
# -----------------------------

final_plot <- forest_plot + table_plot +
  plot_layout(
    widths = c(1.9, 1.1)
  )

# -----------------------------
# DISPLAY
# -----------------------------

final_plot

# -----------------------------
# EXPORTS
# -----------------------------

# PDF (vector)

ggsave(
  filename = "Figure2_PAF_forestplot.pdf",
  plot = final_plot,
  width = 200,
  height = 120,
  units = "mm",
  device = cairo_pdf,
  bg = "white"
)

# TIFF (journal submission)

ggsave(
  filename = "Figure2_PAF_forestplot.tiff",
  plot = final_plot,
  width = 200,
  height = 120,
  units = "mm",
  dpi = 600,
  compression = "lzw",
  bg = "white"
)

# -----------------------------
## LIVING ABSTRACT - Figure 3
# -----------------------------
library(ggplot2)
library(ggpattern)
library(patchwork)
library(dplyr)
# -----------------------------
# PANEL A data - total wheeze burden vs RSV-LRTI attributable

# Creatining squares (100)
df_a <- expand.grid(
  x = 1:10,
  y = 1:10
)

# Bottom 9 squares = RSV attributable (ie 9%)
df_a$group <- c(
  rep("RSV-LRTI attributable wheeze/asthma\n29,299 cases (9.1%)", 9),
  rep("Non-RSV-LRTI attributable wheeze/asthma\n293,159 cases", 91)
)

# PANEL B DATA - RSV Attributable wheeze burden vs prevention

df_b <- expand.grid(
  x = 1:10,
  y = 1:10
)

# 27% prevented (hatched)
# 73% residual (solid)

df_b$group <- c(
  rep("Prevented under Nirsevimab\n8,040 cases (27.4%)", 27),
  rep("Residual RSV-LRTI attributable burden\n21,259 cases", 73)
)

# -----------------------------
# PANEL A

p1 <- ggplot(df_a,
             aes(x = x,
                 y = y,
                 fill = group)) +
  
  geom_tile(
    color = "white",
    linewidth = 2,
    width = 0.9,
    height = 0.9
  ) +
  
  scale_fill_manual(
    values = c(
      "RSV-LRTI attributable wheeze/asthma\n29,299 cases (9.1%)" = "#003B8E",
      "Non-RSV-LRTI attributable wheeze/asthma\n293,159 cases" = "lightgray"
    ),
    name = NULL
  ) +
  
  coord_equal() +
  
  labs(
    title = "A. All childhood wheeze/asthma burden by age 6",
    subtitle = "Each square represents 1% of total childhood\n wheeze/asthma burden (n=322,458 cases)"
  ) +
  
  theme_void() +
  
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8),
    plot.title = element_text(
      face = "bold",
      size = 11,
      hjust = 0.5
    ),
    plot.subtitle = element_text(
      size = 9,
      hjust = 0.5
    )
  )

# -----------------------------
# PANEL B 

p2 <- ggplot(df_b,
             aes(x = x,
                 y = y)) +
  
  geom_tile_pattern(
    aes(
      pattern = group,
      fill = group
    ),
    color = "white",
    linewidth = 2,
    width = 0.9,
    height = 0.9,
    
    pattern_fill = "#003B8E",
    pattern_colour = "#003B8E",
    pattern_angle = 45,
    pattern_density = 0.5,
    pattern_spacing = 0.015
  ) +
  
  scale_fill_manual(
    values = c(
      "Prevented under Nirsevimab\n8,040 cases (27.4%)" = "white",
      "Residual RSV-LRTI attributable burden\n21,259 cases" = "#003B8E"
    ),
    name = NULL
  ) +
  
  scale_pattern_manual(
    values = c(
      "Prevented under Nirsevimab\n8,040 cases (27.4%)" = "stripe",
      "Residual RSV-LRTI attributable burden\n21,259 cases" = "none"
    ),
    name = NULL
  ) +
  
  coord_equal() +
  
  labs(
    title = "B. RSV-LRTI-attributable wheeze/asthma burden by age 6",
    subtitle = "Each square represents 1% of RSV-attributable\n wheeze/asthma burden (n=29,299 cases)"
  ) +
  
  theme_void() +
  
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8),
    plot.title = element_text(
      face = "bold",
      size = 11,
      hjust = 0.5
    ),
    plot.subtitle = element_text(
      size = 9,
      hjust = 0.5
    )
  )

# -----------------------------
# COMBING PLOTS

p1 + p2


## OLD FIGURES

# Load libraries
#library(ggplot2)
#library(dplyr)
#library(showtext)

# Enable Arial font
#font_add("Arial", regular = "arial.ttf")
#showtext_auto()

# Input data 
#data <- data.frame(
#  scenario = c("Natural History", "Nirsevimab", "Counterfactual"),
#  mean_10k = c(880, 858, 800),
#  lower_10k = c(819, 791, 693),
#  upper_10k = c(941, 922, 888)
  
#)


# Uncertainty to be added in - 

# Convert to per 100,000
#data <- data %>%
#  mutate(
#    mean = mean_10k * 10,
#    lower = lower_10k * 10,
#    upper = upper_10k * 10
#  )

# Extract bounds for shaded counterfactual region
#status <- data %>% filter(scenario == "Natural History")
#no_rsv <- data %>% filter(scenario == "Counterfactual")

# ---------------------------
# Plot
# ---------------------------
#p <- ggplot(data, aes(x = reorder(scenario, mean), y = mean)) +
  
  # Shaded RSV-removable burden
#  annotate("rect",
#           xmin = -Inf, xmax = Inf,
#           ymin = no_rsv$lower,
#           ymax = status$upper,
#           fill = "grey80", alpha = 0.2) +
  
  # Bars
#  geom_col(width = 0.6, fill = "grey60", color = "black") +
  
  # Error bars (uncertainty intervals)
#  geom_errorbar(aes(ymin = lower, ymax = upper),
#                width = 0.4, size = 0.6) +
  
  # Flip coordinates for journal style
#  coord_flip() +
  
  # Labels
#  labs(
#    title = "Effect of RSV Prevention on Asthma Prevalence",
#    subtitle = "Shaded region indicates total asthma burden potentially attributable to RSV",
#    x = "",
#    y = "Asthma Prevalence per 100,000 Children"
#  ) +
  
  # AJRCCM-like theme
#  theme_classic(base_size = 13, base_family = "Arial") +
#  theme(
#    plot.title = element_text(face = "bold", family = "Arial", hjust = 0.5),
#    plot.subtitle = element_text(family = "Arial", hjust = 0.5),
#    axis.title = element_text(family = "Arial"),
#    axis.text = element_text(family = "Arial"),
#    axis.line = element_line(color = "black"),
#    axis.ticks = element_line(color = "black")
#  )

# Print plot
#print(p)

#library(ggplot2)
#library(dplyr)
#library(scales)
#library(showtext)

# Enable Arial font
#font_add("Arial", regular = "arial.ttf")
#showtext_auto()

# =========================
# INPUT DATA
# =========================
#natural_history <- 322458
#counterfactual <- 293159
#nirsevimab <- 314418

# Derived point estimates
#prevented_nirsevimab <- natural_history - nirsevimab   # 8040
#preventable_max <- natural_history - counterfactual    # 29299

# =========================
# UNCERTAINTY INTERVALS
# (reordered so lower < upper)
# =========================
#df <- data.frame(
#  Scenario = c("Nirsevimab", "Counterfactual"),
#  Cases_Prevented = c(prevented_nirsevimab, preventable_max),
#  lower = c(2180, 7858),
#  upper = c(17331, 62234)
#)

# =========================
# PLOT
# =========================
#p <- ggplot(df, aes(x = Scenario, y = Cases_Prevented, fill = Scenario)) +
  
#  geom_col(width = 0.6, color = "black") +
  
  # Error bars (95% UI)
#  geom_errorbar(aes(ymin = lower, ymax = upper),
#                width = 0.15,
#                size = 0.6,
#                color = "black") +
  
  
  # Grayscale fill
#  scale_fill_manual(values = c(
#    "Nirsevimab" = "grey60",
#    "All RSV prevented" = "grey95"
#  )) +
  
#  scale_y_continuous(
#    labels = comma,
#    expand = expansion(mult = c(0, 0.15))
#  ) +
  
#  labs(
#    title = "Asthma Cases Prevented Under RSV Prevention Scenarios",
#    subtitle = "Bars show point estimate; error bars indicate 95% uncertainty interval",
#    y = "Cases prevented by age 6 years",
#    x = NULL
#  ) +
  
  # Arial theme
#  theme_minimal(base_size = 12, base_family = "Arial") +
#  theme(
#    legend.position = "none",
#    panel.grid.major.x = element_blank(),
#    panel.grid.minor = element_blank(),
#    panel.grid.major.y = element_line(color = "grey85"),
#    axis.text = element_text(color = "black", family = "Arial"),
#    axis.title = element_text(color = "black", family = "Arial"),
#    plot.title = element_text(face = "bold",family = "Arial", hjust = 0.5),
#    plot.subtitle = element_text(size = 10,family = "Arial",hjust = 0.5)
#  )

#p

# Load libraries
#library(ggplot2)
#library(dplyr)

# Data frame of crude PAF estimates
# Replace placeholder values with your model outputs

#df <- data.frame(
#  risk_factor = c(
#    "RSV LRTI",
#    "Tobacco smoke exposure",
#    "Indoor allergens (sensitized)",
#    "Air pollution (PM2.5/NO2)",
#    "Respiratory infections (non-RSV)"
#  ),
  
#  paf = c(
#    9.09,   # your estimate
    
    # placeholders below (replace with your values)
#    15.0,
#    12.0,
#    8.0,
#    10.0
#  ),
#  
#  lower = c(
#    2.49,   # RSV lower UI
#    8.0,
#    5.0,
#    3.0,
#    4.0
#  ),
  
#  upper = c(
 #   19.20,  # RSV upper UI
#    22.0,
#    20.0,
#    14.0,
#    18.0
#  )
#)

# Order factors by effect size (optional but usually helpful)
#df <- df %>%
#  mutate(risk_factor = reorder(risk_factor, paf))

# Forest plot
#ggplot(df, aes(x = paf, y = risk_factor)) +
#  geom_point(size = 3) +
#  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2) +
#  geom_vline(xintercept = 0, linetype = "dashed") +
#  labs(
#    title = "Contribution of Exposure to Wheeze/Asthma Prevalence",
#    x = "Population Attributable Fraction (%)",
#    y = ""
#  ) +
#  theme_minimal(base_size = 12, base_family = "Arial")
#+
#  theme( axis.title = element_text(color = "black", family = "Arial"),
#         plot.title = element_text(face = "bold",family = "Arial",hjust = 0.5)
#  )

# -----------------------------
# FIGURE 2
# Population Attributable Fraction (PAF)
# Childhood wheeze/asthma risk factors
# AJRCCM-style forest plot
# Aligned label + forest + right-table version
# -----------------------------

library(ggplot2)
library(dplyr)
library(patchwork)
library(ggtext)

# -----------------------------
# DATA
# -----------------------------

df <- data.frame(
  RiskFactor = c(
    "RSV LRTI",
    "Secondhand smoke exposure",
    "Early-life allergen sensitization",
    "Childhood overweight/obesity",
    "Prenatal smoking",
    "Traffic-related air pollution",
    "Indoor mold exposure"
  ),
  ExposureWindow = c(
    "Infancy",
    "Postnatal",
    "Infancy–early\nchildhood",
    "Childhood",
    "Prenatal",
    "Early childhood",
    "Early childhood"
  ),
  RR_CI = c(
    "3.8 (3.2–4.6)",
    "2.8 (2.1–3.9)",
    "1.1 (0.9–1.3)",
    "1.3 (1.2–1.4)",
    "1.9 (1.4–2.5)",
    "1.1 (1.0–1.3)",
    "1.1 (0.9–1.3)"
  ),
  Prevalence = c(20, 41, 6, 9, 8, 4, 4),
  PAF = c(9.1, 11.5, 9.5, 4.1, 6.7, 0.5, 0.4),
  LowerCI = c(2.5, 8.5, 6.0, 3.2, 2.9, 0.0, -0.4),
  UpperCI = c(19.2, 14.6, 14.4, 5.9, 11.4, 1.7, 1.4)
)

# -----------------------------
# ORDERING
# RSV first; remaining rows sorted by descending PAF
# -----------------------------

df <- bind_rows(
  df %>% filter(RiskFactor == "RSV LRTI"),
  df %>% filter(RiskFactor != "RSV LRTI") %>% arrange(desc(PAF))
) %>%
  mutate(
    row = rev(seq_len(n())),
    RiskLabel = ifelse(RiskFactor == "RSV LRTI", "<b>RSV LRTI</b>", RiskFactor),
    Prev = paste0(Prevalence, "%")
  )

n_rows <- nrow(df)
y_limits <- c(0.5, n_rows + 0.75)
y_header <- n_rows + 0.45
separator_y <- n_rows - 0.5

# -----------------------------
# COMMON THEME SETTINGS
# -----------------------------

base_font <- "Arial"
body_size <- 8
header_size <- 8.5
blue <- "#003B8E"

# -----------------------------
# LEFT LABEL PANEL
# -----------------------------

label_plot <- ggplot(df, aes(y = row)) +
  geom_hline(yintercept = separator_y, colour = "grey45", linewidth = 0.6) +
  ggtext::geom_richtext(
    aes(x = 1, label = RiskLabel),
    hjust = 1,
    vjust = 0.5,
    fill = NA,
    label.color = NA,
    label.padding = grid::unit(rep(0, 4), "pt"),
    family = base_font,
    size = body_size / ggplot2::.pt,
    lineheight = 0.95
  ) +
  scale_y_continuous(limits = y_limits, expand = c(0, 0)) +
  coord_cartesian(xlim = c(0, 1), clip = "off") +
  theme_void(base_family = base_font) +
  theme(plot.margin = margin(t = 15, r = 4, b = 22, l = 8))

# -----------------------------
# FOREST PLOT PANEL
# -----------------------------

forest_plot <- ggplot(df, aes(x = PAF, y = row)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50", linewidth = 0.7) +
  geom_hline(yintercept = separator_y, colour = "grey45", linewidth = 0.6) +
  geom_errorbar(
    aes(xmin = LowerCI, xmax = UpperCI),
    orientation = "y",
    height = 0.16,
    linewidth = 0.85,
    colour = blue
  ) +
  geom_point(
    aes(size = ifelse(RiskFactor == "RSV LRTI", 3.8, 3.0)),
    colour = blue
  ) +
  scale_size_identity() +
  scale_y_continuous(limits = y_limits, expand = c(0, 0)) +
  scale_x_continuous(
    limits = c(-2, 25),
    breaks = seq(0, 25, 5),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  labs(x = "Population attributable fraction (%)", y = NULL) +
  theme_minimal(base_family = base_font, base_size = 9) +
  theme(
    panel.grid.major.x = element_line(colour = "grey86", linewidth = 0.5),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    axis.text.x = element_text(size = 8),
    axis.title.x = element_text(size = 9, margin = margin(t = 7)),
    plot.margin = margin(t = 15, r = 8, b = 16, l = 0)
  )

# -----------------------------
# RIGHT TABLE PANEL
# Use a wider x-range and fixed x positions so columns do not bunch together.
# -----------------------------

x_exp <- 0.0
x_rr <- 4.0
x_prev <- 7.7

right_table_plot <- ggplot(df, aes(y = row)) +
  geom_hline(yintercept = separator_y, colour = "grey45", linewidth = 0.6) +
  geom_text(
    aes(x = x_exp, label = ExposureWindow),
    hjust = 0,
    vjust = 0.5,
    family = base_font,
    size = body_size / ggplot2::.pt,
    lineheight = 0.95
  ) +
  geom_text(
    aes(x = x_rr, label = RR_CI),
    hjust = 0,
    vjust = 0.5,
    family = base_font,
    size = body_size / ggplot2::.pt,
    lineheight = 0.95
  ) +
  geom_text(
    aes(x = x_prev, label = Prev),
    hjust = 0,
    vjust = 0.5,
    family = base_font,
    size = body_size / ggplot2::.pt,
    lineheight = 0.95
  ) +
  annotate(
    "text", x = x_exp, y = y_header, label = "Exposure window",
    hjust = 0, vjust = 0, fontface = "bold", family = base_font,
    size = header_size / ggplot2::.pt
  ) +
  annotate(
    "text", x = x_rr, y = y_header, label = "RR (95% CI)",
    hjust = 0, vjust = 0, fontface = "bold", family = base_font,
    size = header_size / ggplot2::.pt
  ) +
  annotate(
    "text", x = x_prev, y = y_header, label = "Prevalence",
    hjust = 0, vjust = 0, fontface = "bold", family = base_font,
    size = header_size / ggplot2::.pt
  ) +
  scale_y_continuous(limits = y_limits, expand = c(0, 0)) +
  coord_cartesian(xlim = c(-0.05, 10.2), clip = "off") +
  theme_void(base_family = base_font) +
  theme(plot.margin = margin(t = 15, r = 12, b = 22, l = 10))

# -----------------------------
# COMBINE
# -----------------------------

final_plot <- label_plot + forest_plot + right_table_plot +
  plot_layout(widths = c(1.45, 2.35, 2.65))

# -----------------------------
# DISPLAY
# -----------------------------

final_plot

# -----------------------------
# EXPORTS
# -----------------------------

ggsave(
  filename = "Figure2_PAF_forestplot_aligned_fixed.pdf",
  plot = final_plot,
  width = 200,
  height = 120,
  units = "mm",
  device = cairo_pdf,
  bg = "white"
)

ggsave(
  filename = "Figure2_PAF_forestplot_aligned_fixed.tiff",
  plot = final_plot,
  width = 200,
  height = 120,
  units = "mm",
  dpi = 600,
  compression = "lzw",
  bg = "white"
)
