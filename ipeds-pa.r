library(tidyverse)

# read in data - IPEDS data on completions in PA/PP masters programs
ipeds <- read.csv("data.csv")

p <- ggplot(ipeds) +
  geom_line(aes(x = ay_ending, y = cip_4404_students, color = "MPA", size = 0.5)) +
  geom_line(aes(x = ay_ending, y = cip_4405_students, color = "MPP"), size = 0.5) +
  scale_x_continuous(breaks = seq(1987,2017, by = 5)) +
  scale_y_continuous(breaks = seq(0,14000, by = 2000)) +
  scale_color_manual(values = c(
    "MPA" = "#E69F00",
    "MPP" = "#617A89")) +
  guides(col = guide_legend(title = NULL, ncol=2)) +
  # Theming
  labs(
    title="Growth in Earned Graduate Degrees in Public Affairs",
    subtitle="Master of Public Administration (MPA) and Master of Public Policy (MPP) degrees earned by academic year",
    caption="Author: Chris Goodman (@cbgoodman), Data: IPEDS, CIP 44.04 (MPA) and 44.05 (MPP).",
    y="Completions",
    x="Academic year, ending") +
  theme_minimal(base_family="Open Sans Condensed Light") +
  theme(
    legend.position = "bottom",
    legend.title.align = 0.5,
    legend.text.align = 0) +
  # light, dotted major y-grid lines only
  theme(panel.grid=element_line())+
  theme(panel.grid.major.y=element_line(color="#2b2b2b", linetype="dotted", size=0.15))+
  theme(panel.grid.major.x=element_blank())+
  theme(panel.grid.minor.x=element_blank())+
  theme(panel.grid.minor.y=element_blank())+
  # light x-axis line only
  theme(axis.line=element_line())+
  theme(axis.line.y=element_blank())+
  theme(axis.line.x=element_blank())+
  # tick styling
  theme(axis.ticks=element_line())+
  theme(axis.ticks.x=element_blank())+
  theme(axis.ticks.y=element_blank())+
  theme(axis.ticks.length=unit(5, "pt"))+
  # x-axis labels
  theme(axis.text.x=element_text(size=10, hjust=0.95,vjust=0.2))+
  # breathing room for the plot
  theme(plot.margin=unit(rep(0.5, 4), "cm"))+
  # move the y-axis tick labels over a bit
  theme(axis.text.y=element_text(margin=margin(r=-5)))+
  theme(axis.text.x=element_text(margin=margin(r=-5)))+
  # make the plot title bold and modify the bottom margin a bit
  theme(plot.title=element_text(family="Open Sans Condensed Bold", margin=margin(b=15)))+
  # make the subtitle italic
  theme(plot.subtitle=element_text(family="Open Sans Condensed Light Italic"))+
  theme(plot.caption=element_text(size=8, hjust=0, margin=margin(t=15)))
ggsave(plot=p, "mpampp.png", width=8, height=6, units="in", dpi="retina")

mpp <- ggplot(ipeds) +
  aes(x = ay_ending, y = cip_4405_students) +
  geom_line(size = 0.5) +
  scale_x_continuous(breaks = seq(1987,2017, by=5)) +
  # Theming
  labs(
    title="Growth in MPP Degrees Earned",
    subtitle="Master of Public Policy (MPP) degrees earned by academic year",
    caption="Author: Chris Goodman (@cbgoodman), Data: IPEDS, CIP 44.05.",
    y="Completions",
    x="Academic year, ending") +
  theme_minimal(base_family="Open Sans Condensed Light") +
  # light, dotted major y-grid lines only
  theme(panel.grid=element_line())+
  theme(panel.grid.major.y=element_line(color="#2b2b2b", linetype="dotted", size=0.15))+
  theme(panel.grid.major.x=element_blank())+
  theme(panel.grid.minor.x=element_blank())+
  theme(panel.grid.minor.y=element_blank())+
  # light x-axis line only
  theme(axis.line=element_line())+
  theme(axis.line.y=element_blank())+
  theme(axis.line.x=element_blank())+
  # tick styling
  theme(axis.ticks=element_line())+
  theme(axis.ticks.x=element_blank())+
  theme(axis.ticks.y=element_blank())+
  theme(axis.ticks.length=unit(5, "pt"))+
  # x-axis labels
  theme(axis.text.x=element_text(size=10, hjust=0.95,vjust=0.2))+
  # breathing room for the plot
  theme(plot.margin=unit(rep(0.5, 4), "cm"))+
  # move the y-axis tick labels over a bit
  theme(axis.text.y=element_text(margin=margin(r=-5)))+
  theme(axis.text.x=element_text(margin=margin(r=-5)))+
  # make the plot title bold and modify the bottom margin a bit
  theme(plot.title=element_text(family="Open Sans Condensed Bold", margin=margin(b=15)))+
  # make the subtitle italic
  theme(plot.subtitle=element_text(family="Open Sans Condensed Light Italic"))+
  theme(plot.caption=element_text(size=8, hjust=0, margin=margin(t=15)))
