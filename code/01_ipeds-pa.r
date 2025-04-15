library(tidyverse)
library(scales)
library(ggrepel)
library(ggtext)

# read in theme
source("code/helper01_theme.r")

# read in data - IPEDS data on completions in PA/PP masters programs
ipeds = read.csv("data/data.csv") |>
  mutate(
    cip = as.factor(
      recode(
        cip,
        "4404" = "Master of Public Administration (MPA)",
        "4405" = "Master of Public Policy (MPP)"
      )
    )
  ) |>
  group_by(cip) |>
  mutate(
    diff = students - lag(students, n = 1L, order_by = ay_ending),
    percent_diff = diff / lag(students, n = 1L, order_by = ay_ending),
    updown.chg.perc = case_when(percent_diff >= 0 ~ "up", TRUE ~ "down"),
  ) |>
  ungroup()

# Breaks for y-axis
breaks = scales::extended_breaks(n = 7)(ipeds$students)
breaks = breaks[2:length(breaks)]

# Annotation
mpa_label_vals = ipeds |>
  filter(cip == "Master of Public Administration (MPA)") |>
  arrange(desc(students)) |>
  slice_max(students, n = 1)

mpampp = ipeds |>
  ggplot() +
  geom_line(aes(y = students, x = ay_ending, group = cip, color = cip)) +
  # MPA Annotation
  annotate(
    "text",
    x = mpa_label_vals$ay_ending,
    y = mpa_label_vals$students * 1.1,
    label = "Largest year of MPA graduates,\n2014 at 13,032",
    size = 3.25,
    family = "Public Sans"
  ) +
  annotate(
    "segment",
    x = mpa_label_vals$ay_ending,
    xend = mpa_label_vals$ay_ending,
    y = mpa_label_vals$students + 125,
    yend = mpa_label_vals$students * 1.045,
    linewidth = .35
  ) +
  geom_richtext(
    data = ipeds  |> group_by(cip) |> filter(ay_ending == max(ay_ending)),
    aes(
      x = ay_ending,
      y = students,
      label = label_comma()(students)),
    fill = NA, label.color = NA, hjust = 0, size = 3.25
  ) +
  coord_cartesian(clip = "off", xlim = c(1987, 2023)) +
  scale_x_continuous(
    breaks = seq(1987, 2023, 2),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_continuous(
    limits = c(-1, 14500),
    breaks = breaks,
    expand = c(0, 0),
    labels = label_comma()
  ) +
  scale_colour_manual(
    values = c("#ff5e00", "#0072b2")
  ) +
  guides(
    x = guide_axis(cap = "both"), # Cap both ends
  ) +
  # Theming
  labs(
    title = "MPP Degrees Stagnate as MPA Degrees Decline",
    subtitle = "<span style='color:#ff5e00'>Master of Public Administration (MPA)</span> and <span style='color:#0072b2'>Master of Public Policy (MPP)</span> degrees earned by academic year, 1987-2023",
    caption = "Author: Chris Goodman (@cbgoodman), Data: IPEDS, CIP 44.04 (Public Administration) and 44.05 (Public Policy Analysis).",
    y = NULL,
    x = NULL
  ) +
  theme_cbgoodman() +
  theme(
    # no legend
    legend.position = "none",
    # adjust to change label on lines
    axis.text.x = element_text(
      size = 11,
      hjust = 0.95,
      vjust = 0.5,
      angle = 90
    ),
    plot.margin = unit(c(1, 2.5, 1, 1), "cm")
  )

ggsave(
  plot = mpampp,
  "img/mpampp.png",
  path = ".",
  width = 10,
  height = 6,
  units = "in",
  dpi = "retina",
  bg = "#ffffff",
  device = grDevices::png
)

pal = c("down" = "#C44D58", "up" = "#617A89")

mpamppdiff = ipeds |>
  ggplot() +
  geom_bar(
    stat = "identity",
    aes(y = percent_diff, x = ay_ending, fill = updown.chg.perc, color = NULL),
    #position = position_nudge(x = 15)
  ) +
  facet_wrap(~cip, nrow = 2,
    #labeller = labeller(industry.code = label)
  ) +
  scale_x_continuous(
    limits = c(1987, 2024),
    breaks = seq(1988, 2023, 2),
    expand = expansion(mult = c(0.075, 0.025))
  ) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(-0.2, 0.5),
    expand = c(0, 0)
  ) +
  scale_fill_manual(
    values = pal,
    guide = "none"
  ) +
  # Theming
  labs(
    title = "Public affairs degree completions have been declining recently",
    subtitle = "Change in Master of Public Administration (MPA) and Master of Public Policy (MPP) degrees earned by academic year, 1988-2023",
    caption = "Author: Chris Goodman (@cbgoodman), Data: IPEDS, CIP 44.04 (Public Administration) and 44.05 (Public Policy Analysis).",
    y = NULL,
    x = NULL) +
  theme_cbgoodman() +
  theme(
    # no legend
    legend.position = "none",
    # adjust to change label on lines
    axis.text.y = element_text(
      angle = 0,
      vjust = -.5,
      margin = margin(r = -30)
    ),
    # rotate x-axis labels
    axis.text.x = element_text(
      size = 11,
      hjust = 0.95,
      vjust = 0.5,
      angle = 90
    )
  )

ggsave(
  plot = mpamppdiff,
  "img/mpamppdiff.png",
  path = ".",
  width = 10,
  height = 8,
  units = "in",
  dpi = "retina",
  bg = "#ffffff",
  device = grDevices::png
)
