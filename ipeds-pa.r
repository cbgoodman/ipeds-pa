library(tidyverse)
library(scales)
library(ggrepel)
library(ggtext)

# read in data - IPEDS data on completions in PA/PP masters programs
ipeds = read.csv("data.csv") |>
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
    y = mpa_label_vals$students * 1.075,
    label = "Largest year of MPA graduates,\n2014 at 13,032",
    size = 2.5,
    family = "Public Sans"
  ) +
  annotate(
    "segment",
    x = mpa_label_vals$ay_ending,
    xend = mpa_label_vals$ay_ending,
    y = mpa_label_vals$students + 100,
    yend = mpa_label_vals$students * 1.045,
    linewidth = .25
  ) +
  geom_richtext(
    data = ipeds  |> group_by(cip) |> filter(ay_ending == max(ay_ending)), 
    aes(
      x = ay_ending,
      y = students,
      label = label_comma()(students)),
    fill = NA, label.color = NA, hjust = 0, size = 2.5
  ) +
  coord_cartesian(clip = "off", xlim = c(1987,2022)) +
  scale_x_continuous(
    breaks = c(1987, 1992, 1997, 2002, 2007, 2012, 2017, 2022),
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
    subtitle = "<span style='color:#ff5e00'>Master of Public Administration (MPA)</span> and <span style='color:#0072b2'>Master of Public Policy (MPP)</span> degrees earned by academic year, 1987-2022",
    caption = "Author: Chris Goodman (@cbgoodman), Data: IPEDS, CIP 44.04 (Public Administration) and 44.05 (Public Policy Analysis).",
    y = NULL,
    x = NULL
  ) +
  theme_minimal(base_family = "Public Sans") +
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    axis.text.y = element_text(
      angle = 0,
      vjust = -.5,
      margin = margin(r = -30)
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      colour = "light grey",
      linetype = "dashed",
      linewidth = .35
    ),
    axis.ticks.x = element_line(color = "light grey"),
    axis.ticks.y = element_blank(),
    axis.line.x = element_line(
      colour = "light grey",
      linetype = "solid",
      linewidth = .5
    )
  ) +
  # breathing room for the plot
  theme(plot.margin = unit(c(0.5, 1.25, 0.5, 0.5), "cm")) +
  # make the plot title bold and modify the bottom margin a bit
  theme(
    plot.title = element_textbox_simple(
      family = "Public Sans SemiBold",
      size = 13,
      margin = margin(b = 11)
    )
  ) +
  # make the subtitle italic
  theme(
    plot.subtitle = element_textbox_simple(
      family = "Public Sans Italic",
      size = 11,
      margin = margin(b = 15)
    )
  ) +
  theme(
    plot.caption = element_text(
      size = 8,
      hjust = 0,
      margin = margin(t = 15)
    )
  )

ggsave(
  plot = mpampp,
  "mpampp.png",
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
    limits = c(1987, 2023),
    breaks = c(1988, 1992, 1997, 2002, 2007, 2012, 2017, 2022),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(-0.1, 0.4),
    expand = c(0, 0)
  ) +
  scale_fill_manual(
    values = pal,
    guide = "none"
  ) +
  # Theming
  labs(
    title = "Public affairs degree completions have been declining recently",
    subtitle = "Change in Master of Public Administration (MPA) and Master of Public Policy (MPP) degrees earned by academic year, 1988-2022",
    caption = "Author: Chris Goodman (@cbgoodman), Data: IPEDS, CIP 44.04 (Public Administration) and 44.05 (Public Policy Analysis).",
    y = NULL,
    x = NULL) +
  theme_minimal(base_family = "Public Sans") +
  # light, dotted major y-grid lines only
  theme(panel.grid = element_line()) +
  theme(panel.grid.major.y = element_line(color = "#2b2b2b", linetype = "dotted", size = 0.15)) +
  theme(panel.grid.major.x = element_blank()) +
  theme(panel.grid.minor.x = element_blank()) +
  theme(panel.grid.minor.y = element_blank()) +
  # light x-axis line only
  theme(axis.line = element_line()) +
  theme(axis.line.y = element_blank()) +
  theme(axis.line.x = element_blank()) +
  # tick styling
  theme(axis.ticks = element_line()) +
  theme(axis.ticks.x = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  theme(axis.ticks.length = unit(5, "pt")) +
  # x-axis labels
  theme(axis.text.x = element_text(size = 11, hjust = 0.95, vjust = 0.5, angle = 90)) +
  # breathing room for the plot
  theme(plot.margin = unit(rep(0.5, 4), "cm")) +
  # move the y-axis tick labels over a bit
  #theme(axis.text.y = element_text(margin = margin(r = -5))) +
  #theme(axis.text.x = element_text(margin = margin(r = -5))) +
  # make the plot title bold and modify the bottom margin a bit
  theme(plot.title = element_text(family = "Public Sans SemiBold", size = 13, margin = margin(b = 15))) +
  # make the subtitle italic
  theme(plot.subtitle = element_text(family = "Public Sans Italic", size = 11)) +
  # increase size of facet text
  theme(strip.text.x = element_text(size = 11)) +
  theme(plot.caption = element_text(size = 8, hjust = 0, margin = margin(t = 15)))

ggsave(
  plot = mpamppdiff,
  "mpamppdiff.png",
  path = ".",
  width = 10,
  height = 8,
  units = "in",
  dpi = "retina",
  bg = "#ffffff",
  device = grDevices::png
)
