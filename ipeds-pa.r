library(tidyverse)

# read in data - IPEDS data on completions in PA/PP masters programs
ipeds <- read.csv("data.csv")
ipeds <- transform(ipeds, cip = as.character(cip))

p <- ggplot(ipeds) +
  aes(x = ay_ending, y = students, group = cip) +
  geom_line(aes(color = cip), size = 0.5) +
  scale_x_continuous(breaks = seq(1987,2017, by = 5)) +
  scale_y_continuous(breaks = seq(0,14000, by = 2000)) +
  scale_color_manual(values = c("#E69F00", "#617A89"),
    labels=c("MPA", "MPP")) +
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

attach(ipeds)
ipeds <- ipeds[order(cip, ay_ending),]
# Percent change
ipeds.data <- ipeds %>%
  group_by(cip) %>%
  mutate(diff = students - lag(students),
    percent_diff = diff / lag(students) * 100)

diff <- ggplot(ipeds.data) +
  aes(x = ay_ending, y = percent_diff, fill = cip) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_x_continuous(breaks = seq(1988,2017, by = 2),
    limits = c(1987.5,2017.5)) +
  scale_y_continuous(breaks = seq(-20,40, by = 5)) +
  scale_fill_manual(values = c("#E69F00", "#617A89"),
    labels=c("MPA", "MPP")) +
  guides(col = guide_legend(title = NULL, ncol=2)) +
  # Theming
  labs(
    title="Public affairs degree completions have been declining recently",
    subtitle="Change in Master of Public Administration (MPA) and Master of Public Policy (MPP) degrees earned by academic year, 1988-2017",
    caption="Author: Chris Goodman (@cbgoodman), Data: IPEDS, CIP 44.04 (MPA) and 44.05 (MPP).",
    y="Annual percent change in completions",
    x="Academic year, ending") +
  theme_minimal(base_family="Open Sans Condensed Light") +
  theme(
    legend.position = "bottom",
    legend.title.align = 0.5,
    legend.text.align = 0,
    legend.title = element_blank()) +
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
  theme(axis.text.x=element_text(size=10,vjust=0.2))+
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
ggsave(plot=diff, "mpamppdiff.png", width=8, height=6, units="in", dpi="retina")
