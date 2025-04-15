# Theme
theme_cbgoodman = function(base_size = 15) {
  theme_minimal(base_size = base_size, base_family = "Public Sans") %+replace%
  theme(
    #legend.position = "none",
    panel.background = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      colour = "light grey",
      linetype = "dashed",
      linewidth = .35
    ),
    #axis.ticks.x = element_line(color = "light grey"),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(hjust = 0.5),
    axis.ticks.y = element_blank(),
    axis.line.x = element_line(
      colour = "light grey",
      linetype = "solid",
      linewidth = .5
    ),
    # Adjust titles and captions
    plot.title.position = "plot",
    plot.caption.position = "plot",
    # breathing room for the plot + expand right for labels
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    # make the plot title bold and modify the bottom margin a bit
    plot.title = element_textbox_simple(
      family = "Public Sans SemiBold",
      size = 22,
      margin = margin(b = 11),
      color = "#333333"
    ),
    # make the subtitle italic
    plot.subtitle = element_textbox_simple(
      family = "Public Sans Italic",
      size = 13,
      margin = margin(b = 26),
      color = "#333333"
    ),
    plot.caption = element_text(
      size = 9,
      hjust = 0,
      margin = margin(t = 15),
      color = "#333333"
    )
  ) # Done defining theme
}