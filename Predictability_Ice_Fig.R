# ============================================================
# Projecting an Ice Free Arctic
# Why "no” ice can mean "some” ice figure
# ============================================================

# Install these once if needed:
# install.packages(c("ggplot2", "dplyr", "tidyr", "scales"))

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)


# ------------------------------------------------------------
# 1. X AXIS
# ------------------------------------------------------------
# 0 = earlier / thick safe ice
# 1 = later / no ice

x <- seq(0, 1, length.out = 500)


# ------------------------------------------------------------
# 2. HELPER FUNCTIONS
# ------------------------------------------------------------

# Smooth transition between two values
smooth_transition <- function(x, midpoint, steepness = 15) {
  1 / (1 + exp(-steepness * (x - midpoint)))
}

# Smooth U-shaped curve:
# high at beginning, low in middle, high at end
u_curve <- function(x,
                    left_height,
                    minimum,
                    right_height,
                    left_mid = 0.30,
                    right_mid = 0.68,
                    steepness = 14) {
  
  decline <- 1 - smooth_transition(x, left_mid, steepness)
  rise    <- smooth_transition(x, right_mid, steepness)
  
  minimum +
    (left_height - minimum) * decline +
    (right_height - minimum) * rise
}


# ------------------------------------------------------------
# 3. DEFINE OPPORTUNITY CURVES
# ------------------------------------------------------------

# Coastal communities
# Slightly larger than species under thick ice,
# drops during unstable transition,
# recovers moderately under no ice.
coastal <- u_curve(
  x,
  left_height  = 0.90,
  minimum      = 0.10,
  right_height = 0.55,
  left_mid     = 0.31,
  right_mid    = 0.70
)


# Ice-dependent species
# High under thick ice, then rapidly disappears.
species <- 0.82 * (
  1 - smooth_transition(
    x,
    midpoint = 0.34,
    steepness = 18
  )
)


# Shipping
# No real opportunity under thick ice.
# Begins once thin/unsafe ice develops.
shipping <- 0.88 *
  smooth_transition(
    x,
    midpoint = 0.43,
    steepness = 12
  )


# Fishing fleets
# Begin AFTER shipping and increase toward open water.
fishing <- 0.90 *
  smooth_transition(
    x,
    midpoint = 0.52,
    steepness = 13
  )


# Tourism
# Some opportunity under thick/safe ice,
# low during unstable transition,
# high again later.
tourism <- u_curve(
  x,
  left_height  = 0.48,
  minimum      = 0.10,
  right_height = 0.82,
  left_mid     = 0.31,
  right_mid    = 0.69
)


# Military / security
# High -> low -> high
military <- u_curve(
  x,
  left_height  = 0.72,
  minimum      = 0.12,
  right_height = 0.84,
  left_mid     = 0.31,
  right_mid    = 0.69
)


# Global climate / albedo
# Strongly associated with ice;
# declines rapidly through transition and reaches ~0.
global_climate <- 0.80 * (
  1 - smooth_transition(
    x,
    midpoint = 0.31,
    steepness = 18
  )
)


# ------------------------------------------------------------
# 4. PUT EVERYTHING INTO ONE DATA FRAME
# ------------------------------------------------------------

df <- bind_rows(
  
  data.frame(
    x = x,
    group = "Coastal communities",
    opportunity = coastal,
    row = 7
  ),
  
  data.frame(
    x = x,
    group = "Ice-dependent species",
    opportunity = species,
    row = 6
  ),
  
  data.frame(
    x = x,
    group = "Fishing fleets",
    opportunity = fishing,
    row = 5
  ),
  
  data.frame(
    x = x,
    group = "Tourism",
    opportunity = tourism,
    row = 4
  ),
  
  data.frame(
    x = x,
    group = "Military & security",
    opportunity = military,
    row = 3
  ),
  
  data.frame(
    x = x,
    group = "Shipping",
    opportunity = shipping,
    row = 2
  ),
  
  data.frame(
    x = x,
    group = "Global climate",
    opportunity = global_climate,
    row = 1
  )
)


# ------------------------------------------------------------
# 5. CONVERT OPPORTUNITY INTO RIBBON THICKNESS
# ------------------------------------------------------------

# Maximum ribbon thickness
max_height <- 0.65

df <- df %>%
  mutate(
    ymin = row - opportunity * max_height / 2,
    ymax = row + opportunity * max_height / 2
  )


# ------------------------------------------------------------
# 6. WHITE -> BLUE GRADIENT
# ------------------------------------------------------------

# Divide x into many narrow strips.
# This allows the ribbons to gradually become darker blue
# from earlier -> later.

df <- df %>%
  mutate(
    segment = cut(
      x,
      breaks = seq(0, 1, length.out = 101),
      include.lowest = TRUE,
      labels = FALSE
    )
  )

gradient_df <- df %>%
  group_by(group, row, segment) %>%
  summarise(
    xmin = min(x),
    xmax = max(x),
    ymin = mean(ymin),
    ymax = mean(ymax),
    .groups = "drop"
  ) %>%
  mutate(
    gradient_value = xmin
  )


# ------------------------------------------------------------
# 7. LABEL POSITIONS
# ------------------------------------------------------------

labels <- data.frame(
  row = 7:1,
  label = c(
    "Coastal\ncommunities",
    "Ice-dependent\nspecies",
    "Fishing\nfleets",
    "Tourism",
    "Military &\nsecurity",
    "Shipping",
    "Global\nclimate"
  )
)


# ------------------------------------------------------------
# 8. BUILD FIGURE
# ------------------------------------------------------------

p <- ggplot() +
  
  # ----------------------------------------------------------
# ICE-CONDITION SECTION BACKGROUNDS
# ----------------------------------------------------------

annotate(
  "rect",
  xmin = 0,
  xmax = 0.33,
  ymin = 0.4,
  ymax = 7.6,
  fill = "#F4FAFD",
  alpha = 0.35
) +
  
  annotate(
    "rect",
    xmin = 0.33,
    xmax = 0.68,
    ymin = 0.4,
    ymax = 7.6,
    fill = "#F8F8F8",
    alpha = 0.35
  ) +
  
  annotate(
    "rect",
    xmin = 0.68,
    xmax = 1,
    ymin = 0.4,
    ymax = 7.6,
    fill = "#F4FAFD",
    alpha = 0.35
  ) +
  
  
  # ----------------------------------------------------------
# GRADIENT RIBBONS
# ----------------------------------------------------------

geom_rect(
  data = gradient_df,
  aes(
    xmin = xmin,
    xmax = xmax + 0.002,
    ymin = ymin,
    ymax = ymax,
    fill = gradient_value
  ),
  color = NA
) +
  
  scale_fill_gradient(
    low = "white",
    high = "#0072B2",
    guide = "none"
  ) +
  
  
  # ----------------------------------------------------------
# VERTICAL ICE-STATE DIVIDERS
# ----------------------------------------------------------

geom_vline(
  xintercept = c(0.33, 0.68),
  linetype = "dashed",
  linewidth = 0.65,
  color = "#4D4D4D"
) +
  
  
  # ----------------------------------------------------------
# CATEGORY LABELS
# ----------------------------------------------------------

geom_text(
  data = labels,
  aes(
    x = -0.035,
    y = row,
    label = label
  ),
  hjust = 1,
  size = 4.6,
  fontface = "bold",
  lineheight = 0.9,
  color = "#152238"
) +
  
  
  # ----------------------------------------------------------
# ICE-STATE HEADINGS
# ----------------------------------------------------------

annotate(
  "text",
  x = 0.165,
  y = 8.35,
  label = "Thick ice",
  size = 5.5,
  fontface = "bold",
  color = "#152238"
) +
  
  annotate(
    "text",
    x = 0.165,
    y = 8.02,
    label = "(Predictable)",
    size = 4.6,
    fontface = "italic",
    color = "#152238"
  ) +
  
  
  annotate(
    "text",
    x = 0.505,
    y = 8.35,
    label = "Thin ice",
    size = 5.5,
    fontface = "bold",
    color = "#152238"
  ) +
  
  annotate(
    "text",
    x = 0.505,
    y = 8.02,
    label = "(Unpredictable)",
    size = 4.6,
    fontface = "italic",
    color = "#152238"
  ) +
  
  
  annotate(
    "text",
    x = 0.84,
    y = 8.35,
    label = "No ice / open water",
    size = 5.5,
    fontface = "bold",
    color = "#152238"
  ) +
  
  annotate(
    "text",
    x = 0.84,
    y = 8.02,
    label = "(Predictable)",
    size = 4.6,
    fontface = "italic",
    color = "#152238"
  ) +
  
  
  # ----------------------------------------------------------
# TIME ARROW
# ----------------------------------------------------------

annotate(
  "segment",
  x = 0,
  xend = 1,
  y = 0.25,
  yend = 0.25,
  linewidth = 0.7,
  color = "#152238",
  arrow = arrow(
    length = unit(0.18, "cm"),
    type = "open"
  )
) +
  
  
  annotate(
    "text",
    x = 0.12,
    y = -0.02,
    label = "Earlier",
    size = 4.4,
    fontface = "italic"
  ) +
  
  annotate(
    "text",
    x = 0.505,
    y = -0.02,
    label = "Transition period",
    size = 4.4,
    fontface = "italic"
  ) +
  
  annotate(
    "text",
    x = 0.86,
    y = -0.02,
    label = "Later",
    size = 4.4,
    fontface = "italic"
  ) +
  
  annotate(
    "text",
    x = 0.50,
    y = -0.42,
    label = "Time (relative)",
    size = 4.8,
    fontface = "bold"
  ) +
  
  
  # ----------------------------------------------------------
# LIMITS
# ----------------------------------------------------------

coord_cartesian(
  xlim = c(-0.20, 1.02),
  ylim = c(-0.6, 8.6),
  clip = "off"
) +
  
  
  # ----------------------------------------------------------
# CLEAN THEME
# ----------------------------------------------------------

theme_void() +
  
  theme(
    plot.margin = margin(
      t = 25,
      r = 25,
      b = 30,
      l = 30
    )
  )


# ------------------------------------------------------------
# 9. DISPLAY
# ------------------------------------------------------------

p


# ------------------------------------------------------------
# 10. SAVE HIGH-RESOLUTION VERSION
# ------------------------------------------------------------

ggsave(
  "Arctic_Opportunity_Figure.png",
  p,
  width = 12,
  height = 7,
  dpi = 300,
  bg = "white"
)