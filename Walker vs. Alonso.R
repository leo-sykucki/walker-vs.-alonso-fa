first_data <- read.csv("1st_baseman_data_over_3years.csv")
cw <- read.csv("Christian_Walker_2024.csv")
pa <- read.csv("Pete_Alonso_2024.csv")

##batting average for all 1st baseman


first_ba <- first_data %>%
  select(player_name, ba)%>%
  mutate(type = "BA") %>%
  mutate(percentile = rank(ba)) %>%
  mutate(percentile = percentile/max(percentile)) %>%
  mutate(percentile = percentile * 100) %>%
  mutate(percentile = round(percentile))
 
low_ba <- first_data %>%
  select(player_name, ba) %>%
  mutate(type = "low", percentile = 0)

high_ba <- first_data %>%
  select(player_name, ba) %>%
  mutate(type = "high", percentile = 100)

lowhigh_ba <- rbind(low_ba, high_ba)

first_ba <- rbind(lowhigh_ba, first_ba)

##BAtting average visual Pete Alonso

pa_ba <- first_ba %>%
  filter(player_name == "Alonso, Pete",
         type %in% c("BA", "high", "low")) %>%
  ggplot(first_ba, mapping = aes(x= percentile, y= type, colour = (percentile))) +
  geom_line() + geom_point(size = 9)  +
  ggtitle("") + xlim(0, 100) + ylim("BA") +
  xlab("") + ylab("") + theme(
    plot.title = element_text(color = "black", size = 15, face = "italic"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x= element_blank(),
    axis.ticks.y  =element_blank(),
    axis.ticks.x  =element_blank(),
    axis.text.y = element_text(size=12, face="italic", colour = "black"))+
  geom_segment(aes(x = 0, xend = 100, y = type, yend = type), color = "#9b9b9b", size = 1) +
  geom_point(aes(x = 0, y = type), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = 50, y = type), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = 100, y = type), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = percentile, y = type, fill = percentile), pch = 21, color = "black", size = 10) +
  geom_text(aes(label=percentile),hjust=.5, vjust=.4, color = "black",
            size = 5)+theme(legend.position = "none")+
  scale_fill_gradient2(midpoint = 50, low = "#2952a3", mid = "#ffffff", high = "#cc0000") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())

##Batting Average visual Christian Walker


cw_ba <- first_ba %>%
  filter(player_name == "Walker, Christian",
         type %in% c("BA", "high", "low")) %>%
  ggplot(first_ba, mapping = aes(x= percentile, y= type, colour = (percentile))) +
  geom_line() + geom_point(size = 9)  +
  ggtitle("") + xlim(0, 100) + ylim("BA") +
  xlab("") + ylab("") + theme(
    plot.title = element_text(color = "black", size = 15, face = "italic"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x= element_blank(),
    axis.ticks.y  =element_blank(),
    axis.ticks.x  =element_blank(),
    axis.text.y = element_text(size=12, face="italic", colour = "black"))+
  geom_segment(aes(x = 0, xend = 100, y = type, yend = type), color = "#9b9b9b", size = 1) +
  geom_point(aes(x = 0, y = type), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = 50, y = type), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = 100, y = type), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = percentile, y = type, fill = percentile), pch = 21, color = "black", size = 10) +
  geom_text(aes(label=percentile),hjust=.5, vjust=.4, color = "black",
            size = 5)+theme(legend.position = "none")+
  scale_fill_gradient2(midpoint = 50, low = "#2952a3", mid = "#ffffff", high = "#cc0000") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())

##Hrs for all firstbaseman


first_hrs <- first_data %>%
  select(player_name, hrs)%>%
  mutate(type = "HRS") %>%
  mutate(percentile = rank(hrs)) %>%
  mutate(percentile = percentile/max(percentile)) %>%
  mutate(percentile = percentile * 100) %>%
  mutate(percentile = round(percentile))

low_hrs <- first_data %>%
  select(player_name, hrs) %>%
  mutate(type = "low", percentile = 0)

high_hrs <- first_data %>%
  select(player_name, hrs) %>%
  mutate(type = "high", percentile = 100)

lowhigh_hrs <- rbind(low_hrs, high_hrs)

first_hrs <- rbind(lowhigh_hrs, first_hrs)

## HR Visual for Pete Alonso

pa_hrs <- first_hrs %>%
  filter(player_name == "Alonso, Pete",
         type %in% c("HRS", "high", "low")) %>%
  ggplot(first_hrs, mapping = aes(x= percentile, y= type, colour = (percentile))) +
  geom_line() + geom_point(size = 9)  +
  ggtitle("") + xlim(0, 100) + ylim("HRS") +
  xlab("") + ylab("") + theme(
    plot.title = element_text(color = "black", size = 15, face = "italic"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x= element_blank(),
    axis.ticks.y  =element_blank(),
    axis.ticks.x  =element_blank(),
    axis.text.y = element_text(size=12, face="italic", colour = "black"))+
  geom_segment(aes(x = 0, xend = 100, y = type, yend = type), color = "#9b9b9b", size = 1) +
  geom_point(aes(x = 0, y = type), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = 50, y = type), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = 100, y = type), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = percentile, y = type, fill = percentile), pch = 21, color = "black", size = 10) +
  geom_text(aes(label=percentile),hjust=.5, vjust=.4, color = "black",
            size = 5)+theme(legend.position = "none")+
  scale_fill_gradient2(midpoint = 50, low = "#2952a3", mid = "#ffffff", high = "#cc0000") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())

#HR Visual for Christian Walker

cw_hrs <- first_hrs %>%
  filter(player_name == "Walker, Christian",
         type %in% c("HRS", "high", "low")) %>%
  ggplot(first_hrs, mapping = aes(x= percentile, y= type, colour = (percentile))) +
  geom_line() + geom_point(size = 9)  +
  ggtitle("") + xlim(0, 100) + ylim("HRS") +
  xlab("") + ylab("") + theme(
    plot.title = element_text(color = "black", size = 15, face = "italic"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x= element_blank(),
    axis.ticks.y  =element_blank(),
    axis.ticks.x  =element_blank(),
    axis.text.y = element_text(size=12, face="italic", colour = "black"))+
  geom_segment(aes(x = 0, xend = 100, y = type, yend = type), color = "#9b9b9b", size = 1) +
  geom_point(aes(x = 0, y = type), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = 50, y = type), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = 100, y = type), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = percentile, y = type, fill = percentile), pch = 21, color = "black", size = 10) +
  geom_text(aes(label=percentile),hjust=.5, vjust=.4, color = "black",
            size = 5)+theme(legend.position = "none")+
  scale_fill_gradient2(midpoint = 50, low = "#2952a3", mid = "#ffffff", high = "#cc0000") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())

##Hits for all firstbaseman


first_hits <- first_data %>%
  select(player_name, hits)%>%
  mutate(type = "HITS") %>%
  mutate(percentile = rank(hits)) %>%
  mutate(percentile = percentile/max(percentile)) %>%
  mutate(percentile = percentile * 100) %>%
  mutate(percentile = round(percentile))

low_hits <- first_data %>%
  select(player_name, hits) %>%
  mutate(type = "low", percentile = 0)

high_hits <- first_data %>%
  select(player_name, hits) %>%
  mutate(type = "high", percentile = 100)

lowhigh_hits <- rbind(low_hits, high_hits)

first_hits <- rbind(lowhigh_hits, first_hits)

## Hits Visual for Pete Alonso

pa_hits <- first_hits %>%
  filter(player_name == "Alonso, Pete",
         type %in% c("HITS", "high", "low")) %>%
  ggplot(first_hits, mapping = aes(x= percentile, y= type, colour = (percentile))) +
  geom_line() + geom_point(size = 9)  +
  ggtitle("Pete Alonso") + xlim(0, 100) + ylim("HITS") +
  xlab("") + ylab("") + theme(
    plot.title = element_text(color = "black", size = 15, face = "italic"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x= element_blank(),
    axis.ticks.y  =element_blank(),
    axis.ticks.x  =element_blank(),
    axis.text.y = element_text(size=10, face="italic", colour = "black"))+
  geom_segment(aes(x = 0, xend = 100, y = type, yend = type), color = "#9b9b9b", size = 1) +
  geom_point(aes(x = 0, y = type), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = 50, y = type), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = 100, y = type), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = percentile, y = type, fill = percentile), pch = 21, color = "black", size = 10) +
  geom_text(aes(label=percentile),hjust=.5, vjust=.4, color = "black",
            size = 5)+theme(legend.position = "none")+
  scale_fill_gradient2(midpoint = 50, low = "#2952a3", mid = "#ffffff", high = "#cc0000") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())

#Hits Visual for Christian Walker

cw_hits <- first_hits %>%
  filter(player_name == "Walker, Christian",
         type %in% c("HITS", "high", "low")) %>%
  ggplot(first_hits, mapping = aes(x= percentile, y= type, colour = (percentile))) +
  geom_line() + geom_point(size = 9)  +
  ggtitle("Christian Walker") + xlim(0, 100) + ylim("HITS") +
  xlab("") + ylab("") + theme(
    plot.title = element_text(color = "black", size = 15, face = "italic"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x= element_blank(),
    axis.ticks.y  =element_blank(),
    axis.ticks.x  =element_blank(),
    axis.text.y = element_text(size=10, face="italic", colour = "black"))+
  geom_segment(aes(x = 0, xend = 100, y = type, yend = type), color = "#9b9b9b", size = 1) +
  geom_point(aes(x = 0, y = type), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = 50, y = type), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = 100, y = type), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = percentile, y = type, fill = percentile), pch = 21, color = "black", size = 10) +
  geom_text(aes(label=percentile),hjust=.5, vjust=.4, color = "black",
            size = 5)+theme(legend.position = "none")+
  scale_fill_gradient2(midpoint = 50, low = "#2952a3", mid = "#ffffff", high = "#cc0000") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())

##OBP for all firstbaseman


first_obp <- first_data %>%
  select(player_name, obp)%>%
  mutate(type = "OBP") %>%
  mutate(percentile = rank(obp)) %>%
  mutate(percentile = percentile/max(percentile)) %>%
  mutate(percentile = percentile * 100) %>%
  mutate(percentile = round(percentile))

low_obp <- first_data %>%
  select(player_name, obp) %>%
  mutate(type = "low", percentile = 0)

high_obp <- first_data %>%
  select(player_name, obp) %>%
  mutate(type = "high", percentile = 100)

lowhigh_obp <- rbind(low_obp, high_obp)

first_obp <- rbind(lowhigh_obp, first_obp)

## OBP Visual for Pete Alonso

pa_obp <- first_obp %>%
  filter(player_name == "Alonso, Pete",
         type %in% c("OBP", "high", "low")) %>%
  ggplot(first_obp, mapping = aes(x= percentile, y= type, colour = (percentile))) +
  geom_line() + geom_point(size = 9)  +
  ggtitle("") + xlim(0, 100) + ylim("OBP") +
  xlab("") + ylab("") + theme(
    plot.title = element_text(color = "black", size = 15, face = "italic"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x= element_blank(),
    axis.ticks.y  =element_blank(),
    axis.ticks.x  =element_blank(),
    axis.text.y = element_text(size=12, face="italic", colour = "black"))+
  geom_segment(aes(x = 0, xend = 100, y = type, yend = type), color = "#9b9b9b", size = 1) +
  geom_point(aes(x = 0, y = type), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = 50, y = type), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = 100, y = type), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = percentile, y = type, fill = percentile), pch = 21, color = "black", size = 10) +
  geom_text(aes(label=percentile),hjust=.5, vjust=.4, color = "black",
            size = 5)+theme(legend.position = "none")+
  scale_fill_gradient2(midpoint = 50, low = "#2952a3", mid = "#ffffff", high = "#cc0000") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())

#OBP Visual for Christian Walker

cw_obp <- first_obp %>%
  filter(player_name == "Walker, Christian",
         type %in% c("OBP", "high", "low")) %>%
  ggplot(first_obp, mapping = aes(x= percentile, y= type, colour = (percentile))) +
  geom_line() + geom_point(size = 9)  +
  ggtitle("") + xlim(0, 100) + ylim("OBP") +
  xlab("") + ylab("") + theme(
    plot.title = element_text(color = "black", size = 15, face = "italic"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x= element_blank(),
    axis.ticks.y  =element_blank(),
    axis.ticks.x  =element_blank(),
    axis.text.y = element_text(size=12, face="italic", colour = "black"))+
  geom_segment(aes(x = 0, xend = 100, y = type, yend = type), color = "#9b9b9b", size = 1) +
  geom_point(aes(x = 0, y = type), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = 50, y = type), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = 100, y = type), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = percentile, y = type, fill = percentile), pch = 21, color = "black", size = 10) +
  geom_text(aes(label=percentile),hjust=.5, vjust=.4, color = "black",
            size = 5)+theme(legend.position = "none")+
  scale_fill_gradient2(midpoint = 50, low = "#2952a3", mid = "#ffffff", high = "#cc0000") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())

##SLG for all firstbaseman


first_slg <- first_data %>%
  select(player_name, slg)%>%
  mutate(type = "SLG") %>%
  mutate(percentile = rank(slg)) %>%
  mutate(percentile = percentile/max(percentile)) %>%
  mutate(percentile = percentile * 100) %>%
  mutate(percentile = round(percentile))

low_slg <- first_data %>%
  select(player_name, slg) %>%
  mutate(type = "low", percentile = 0)

high_slg <- first_data %>%
  select(player_name, slg) %>%
  mutate(type = "high", percentile = 100)

lowhigh_slg <- rbind(low_slg, high_slg)

first_slg <- rbind(lowhigh_slg, first_slg)

## SLG Visual for Pete Alonso

pa_slg <- first_slg %>%
  filter(player_name == "Alonso, Pete",
         type %in% c("SLG", "high", "low")) %>%
  ggplot(first_slg, mapping = aes(x= percentile, y= type, colour = (percentile))) +
  geom_line() + geom_point(size = 9)  +
  ggtitle("") + xlim(0, 100) + ylim("SLG") +
  xlab("") + ylab("") + theme(
    plot.title = element_text(color = "black", size = 15, face = "italic"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x= element_blank(),
    axis.ticks.y  =element_blank(),
    axis.ticks.x  =element_blank(),
    axis.text.y = element_text(size=12, face="italic", colour = "black"))+
  geom_segment(aes(x = 0, xend = 100, y = type, yend = type), color = "#9b9b9b", size = 1) +
  geom_point(aes(x = 0, y = type), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = 50, y = type), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = 100, y = type), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = percentile, y = type, fill = percentile), pch = 21, color = "black", size = 10) +
  geom_text(aes(label=percentile),hjust=.5, vjust=.4, color = "black",
            size = 5)+theme(legend.position = "none")+
  scale_fill_gradient2(midpoint = 50, low = "#2952a3", mid = "#ffffff", high = "#cc0000") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())

## SLG Visual for Christian Walker

cw_slg <- first_slg %>%
  filter(player_name == "Walker, Christian",
         type %in% c("SLG", "high", "low")) %>%
  ggplot(first_slg, mapping = aes(x= percentile, y= type, colour = (percentile))) +
  geom_line() + geom_point(size = 9)  +
  ggtitle("") + xlim(0, 100) + ylim("SLG") +
  xlab("") + ylab("") + theme(
    plot.title = element_text(color = "black", size = 15, face = "italic"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x= element_blank(),
    axis.ticks.y  =element_blank(),
    axis.ticks.x  =element_blank(),
    axis.text.y = element_text(size=12, face="italic", colour = "black"))+
  geom_segment(aes(x = 0, xend = 100, y = type, yend = type), color = "#9b9b9b", size = 1) +
  geom_point(aes(x = 0, y = type), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = 50, y = type), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = 100, y = type), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = percentile, y = type, fill = percentile), pch = 21, color = "black", size = 10) +
  geom_text(aes(label=percentile),hjust=.5, vjust=.4, color = "black",
            size = 5)+theme(legend.position = "none")+
  scale_fill_gradient2(midpoint = 50, low = "#2952a3", mid = "#ffffff", high = "#cc0000") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())

##OPS for all firstbaseman


first_ops <- first_data %>%
  select(player_name)%>%
  mutate(ops = first_data$obp + first_data$slg) %>%
  mutate(type = "OPS") %>%
  mutate(percentile = rank(ops)) %>%
  mutate(percentile = percentile/max(percentile)) %>%
  mutate(percentile = percentile * 100) %>%
  mutate(percentile = round(percentile))

low_ops <- first_data %>%
  select(player_name) %>%
  mutate(ops = first_data$obp + first_data$slg) %>%
  mutate(type = "low", percentile = 0)

high_ops <- first_data %>%
  select(player_name) %>%
  mutate(ops = first_data$obp + first_data$slg) %>%
  mutate(type = "high", percentile = 100)

lowhigh_ops <- rbind(low_ops, high_ops)

first_ops <- rbind(lowhigh_ops, first_ops)

## OPS Visual for Pete Alonso

pa_ops <- first_ops %>%
  filter(player_name == "Alonso, Pete",
         type %in% c("OPS", "high", "low")) %>%
  ggplot(first_ops, mapping = aes(x= percentile, y= type, colour = (percentile))) +
  geom_line() + geom_point(size = 9)  +
  ggtitle("") + xlim(0, 100) + ylim("OPS") +
  xlab("") + ylab("") + theme(
    plot.title = element_text(color = "black", size = 15, face = "italic"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x= element_blank(),
    axis.ticks.y  =element_blank(),
    axis.ticks.x  =element_blank(),
    axis.text.y = element_text(size=12, face="italic", colour = "black"))+
  geom_segment(aes(x = 0, xend = 100, y = type, yend = type), color = "#9b9b9b", size = 1) +
  geom_point(aes(x = 0, y = type), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = 50, y = type), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = 100, y = type), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = percentile, y = type, fill = percentile), pch = 21, color = "black", size = 10) +
  geom_text(aes(label=percentile),hjust=.5, vjust=.4, color = "black",
            size = 5)+theme(legend.position = "none")+
  scale_fill_gradient2(midpoint = 50, low = "#2952a3", mid = "#ffffff", high = "#cc0000") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())

#OPS Visual for Christian Walker

cw_ops <- first_ops %>%
  filter(player_name == "Walker, Christian",
         type %in% c("OPS", "high", "low")) %>%
  ggplot(first_ops, mapping = aes(x= percentile, y= type, colour = (percentile))) +
  geom_line() + geom_point(size = 9)  +
  ggtitle("") + xlim(0, 100) + ylim("OPS") +
  xlab("") + ylab("") + theme(
    plot.title = element_text(color = "black", size = 15, face = "italic"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x= element_blank(),
    axis.ticks.y  =element_blank(),
    axis.ticks.x  =element_blank(),
    axis.text.y = element_text(size=12, face="italic", colour = "black"))+
  geom_segment(aes(x = 0, xend = 100, y = type, yend = type), color = "#9b9b9b", size = 1) +
  geom_point(aes(x = 0, y = type), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = 50, y = type), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = 100, y = type), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = percentile, y = type, fill = percentile), pch = 21, color = "black", size = 10) +
  geom_text(aes(label=percentile),hjust=.5, vjust=.4, color = "black",
            size = 5)+theme(legend.position = "none")+
  scale_fill_gradient2(midpoint = 50, low = "#2952a3", mid = "#ffffff", high = "#cc0000") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())


## K% for all firstbaseman


first_k_percent <- first_data %>%
  select(player_name, k_percent)%>%
  mutate(type = "K %") %>%
  mutate(percentile = rank(-k_percent)) %>%
  mutate(percentile = percentile/max(percentile)) %>%
  mutate(percentile = percentile * 100) %>%
  mutate(percentile = round(percentile))

low_k_percent <- first_data %>%
  select(player_name, k_percent) %>%
  mutate(type = "low", percentile = 0)

high_k_percent <- first_data %>%
  select(player_name, k_percent) %>%
  mutate(type = "high", percentile = 100)

lowhigh_k_percent <- rbind(low_k_percent, high_k_percent)

first_k_percent <- rbind(lowhigh_k_percent, first_k_percent)

## K % Visual for Pete Alonso

pa_k_percent <- first_k_percent %>%
  filter(player_name == "Alonso, Pete",
         type %in% c("K %", "high", "low")) %>%
  ggplot(first_k_percent, mapping = aes(x= percentile, y= type, colour = (percentile))) +
  geom_line() + geom_point(size = 9)  +
  ggtitle("") + xlim(0, 100) + ylim("K %") +
  xlab("") + ylab("") + theme(
    plot.title = element_text(color = "black", size = 15, face = "italic"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x= element_blank(),
    axis.ticks.y  =element_blank(),
    axis.ticks.x  =element_blank(),
    axis.text.y = element_text(size=12, face="italic", colour = "black"))+
  geom_segment(aes(x = 0, xend = 100, y = type, yend = type), color = "#9b9b9b", size = 1) +
  geom_point(aes(x = 0, y = type), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = 50, y = type), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = 100, y = type), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = percentile, y = type, fill = percentile), pch = 21, color = "black", size = 10) +
  geom_text(aes(label=percentile),hjust=.5, vjust=.4, color = "black",
            size = 5)+theme(legend.position = "none")+
  scale_fill_gradient2(midpoint = 50, low = "#2952a3", mid = "#ffffff", high = "#cc0000") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())

## K % Visual for Christian Walker

cw_k_percent <- first_k_percent %>%
  filter(player_name == "Walker, Christian",
         type %in% c("K %", "high", "low")) %>%
  ggplot(first_k_percent, mapping = aes(x= percentile, y= type, colour = (percentile))) +
  geom_line() + geom_point(size = 9)  +
  ggtitle("") + xlim(0, 100) + ylim("K %") +
  xlab("") + ylab("") + theme(
    plot.title = element_text(color = "black", size = 15, face = "italic"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x= element_blank(),
    axis.ticks.y  =element_blank(),
    axis.ticks.x  =element_blank(),
    axis.text.y = element_text(size=12, face="italic", colour = "black"))+
  geom_segment(aes(x = 0, xend = 100, y = type, yend = type), color = "#9b9b9b", size = 1) +
  geom_point(aes(x = 0, y = type), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = 50, y = type), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = 100, y = type), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = percentile, y = type, fill = percentile), pch = 21, color = "black", size = 10) +
  geom_text(aes(label=percentile),hjust=.5, vjust=.4, color = "black",
            size = 5)+theme(legend.position = "none")+
  scale_fill_gradient2(midpoint = 50, low = "#2952a3", mid = "#ffffff", high = "#cc0000") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())

## BB % for all firstbaseman


first_bb_percent <- first_data %>%
  select(player_name, bb_percent)%>%
  mutate(type = "BB %") %>%
  mutate(percentile = rank(bb_percent)) %>%
  mutate(percentile = percentile/max(percentile)) %>%
  mutate(percentile = percentile * 100) %>%
  mutate(percentile = round(percentile))

low_bb_percent <- first_data %>%
  select(player_name, bb_percent) %>%
  mutate(type = "low", percentile = 0)

high_bb_percent <- first_data %>%
  select(player_name, bb_percent) %>%
  mutate(type = "high", percentile = 100)

lowhigh_bb_percent <- rbind(low_bb_percent, high_bb_percent)

first_bb_percent <- rbind(lowhigh_bb_percent, first_bb_percent)

## BB % Visual for Pete Alonso

pa_bb_percent <- first_bb_percent %>%
  filter(player_name == "Alonso, Pete",
         type %in% c("BB %", "high", "low")) %>%
  ggplot(first_bb_percent, mapping = aes(x= percentile, y= type, colour = (percentile))) +
  geom_line() + geom_point(size = 9)  +
  ggtitle("") + xlim(0, 100) + ylim("BB %") +
  xlab("") + ylab("") + theme(
    plot.title = element_text(color = "black", size = 15, face = "italic"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x= element_blank(),
    axis.ticks.y  =element_blank(),
    axis.ticks.x  =element_blank(),
    axis.text.y = element_text(size=12, face="italic", colour = "black"))+
  geom_segment(aes(x = 0, xend = 100, y = type, yend = type), color = "#9b9b9b", size = 1) +
  geom_point(aes(x = 0, y = type), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = 50, y = type), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = 100, y = type), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = percentile, y = type, fill = percentile), pch = 21, color = "black", size = 10) +
  geom_text(aes(label=percentile),hjust=.5, vjust=.4, color = "black",
            size = 5)+theme(legend.position = "none")+
  scale_fill_gradient2(midpoint = 50, low = "#2952a3", mid = "#ffffff", high = "#cc0000") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())

## BB % Visual for Christian Walker

cw_bb_percent <- first_bb_percent %>%
  filter(player_name == "Walker, Christian",
         type %in% c("BB %", "high", "low")) %>%
  ggplot(first_bb_percent, mapping = aes(x= percentile, y= type, colour = (percentile))) +
  geom_line() + geom_point(size = 9)  +
  ggtitle("") + xlim(0, 100) + ylim("BB %") +
  xlab("") + ylab("") + theme(
    plot.title = element_text(color = "black", size = 15, face = "italic"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x= element_blank(),
    axis.ticks.y  =element_blank(),
    axis.ticks.x  =element_blank(),
    axis.text.y = element_text(size=12, face="italic", colour = "black"))+
  geom_segment(aes(x = 0, xend = 100, y = type, yend = type), color = "#9b9b9b", size = 1) +
  geom_point(aes(x = 0, y = type), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = 50, y = type), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = 100, y = type), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = percentile, y = type, fill = percentile), pch = 21, color = "black", size = 10) +
  geom_text(aes(label=percentile),hjust=.5, vjust=.4, color = "black",
            size = 5)+theme(legend.position = "none")+
  scale_fill_gradient2(midpoint = 50, low = "#2952a3", mid = "#ffffff", high = "#cc0000") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())

# Alonso VS. Walker Final Visual

ggarrange(


plot_alonso1 <- ggarrange(pa_ba, pa_hits, pa_hrs, pa_slg, pa_obp, pa_ops, pa_k_percent, pa_bb_percent, nrow = 3, ncol = 3),

plot_walker1 <- ggarrange(cw_ba, cw_hits, cw_hrs, cw_slg, cw_obp, cw_ops, cw_k_percent, cw_bb_percent, nrow = 3, ncol = 3),

nrow = 1, ncol = 2)

title <- expression(atop(bold("1st Baseman Offensive Percentiles from 2022-2024"), scriptstyle("MIN 200 PA")))

annotate_figure(plot, top = text_grob(title))


# Creates A Color Palette, Reversed (Blue/Less Frequent to Red/More Frequent) With 9 Different Shades, Broken Into 16
heat_colors_interpolated <- colorRampPalette(paletteer::paletteer_d("RColorBrewer::RdBu", n = 9, direction = -1))(16)

# Shows The Color Scale
heat_colors_interpolated %>% scales::show_col()

# Alonso pitches

pete_pitches <- pa %>%
  select(pitch_name, plate_x, plate_z, p_throws)

## Catorgorizing Pitches

pete_pitches$pitch_name[pete_pitches$pitch_name == "4-Seam Fastball"] <- "Fastball"
pete_pitches$pitch_name[pete_pitches$pitch_name == "Sinker"] <- "Fastball"
pete_pitches$pitch_name[pete_pitches$pitch_name == "Cutter"] <- "Fastball"

pete_pitches$pitch_name[pete_pitches$pitch_name == "Curveball"] <- "Breakingball"
pete_pitches$pitch_name[pete_pitches$pitch_name == "Knuckle Curve"] <- "Breakingball"
pete_pitches$pitch_name[pete_pitches$pitch_name == "Slurve"] <- "Breakingball"
pete_pitches$pitch_name[pete_pitches$pitch_name == "Slider"] <- "Breakingball"
pete_pitches$pitch_name[pete_pitches$pitch_name == "Sweeper"] <- "Breakingball"

pete_pitches$pitch_name[pete_pitches$pitch_name == "Changeup"] <- "Offspeed"
pete_pitches$pitch_name[pete_pitches$pitch_name == "Eephus"] <- "Offspeed"
pete_pitches$pitch_name[pete_pitches$pitch_name == "Knuckleball"] <- "Offspeed"
pete_pitches$pitch_name[pete_pitches$pitch_name == "Split-Finger"] <- "Offspeed"

## Alonso RH Pitches

pete_rh <- pete_pitches %>%
  filter(pete_pitches$p_throws == "R")

## Usage RH Pitches TB

pete_freq_rh <- pete_rh%>%group_by(pitch_name)%>%tally()
pete_total_rh <- as.numeric(sum(pete_freq_rh$n))
pete_freq_rh$`Usage %` <- round((pete_freq_rh$n/pete_total_rh)*100, digits = 0)
pete_freq_rh <- subset(pete_freq_rh, select = -c(n))



# Merge Main DataSet with Usage %
pete_rh <- merge(pete_rh, pete_freq_rh)

# Add in Parameters for Strike Zone / Plate
Left <- -8.5/12
Right <- 8.5/12
Bottom <- 18.29/12
Top <- 44.08/12

# This is to Make a 3x3 Strike Zone (Vertical and Horizontal Lines in Zone)
Width <- (Right - Left) / 3
Height <- (Top - Bottom) / 3

# Alonso RH Fastball Heatmap

pete_rh_fb <- pete_rh%>%filter(pitch_name == "Fastball")

hm_pa_rh_fb <- ggplot(pete_rh_fb, mapping = aes(x=plate_x, y= plate_z)) +
  stat_density2d_filled()  +
  scale_fill_manual(values = c(heat_colors_interpolated), aesthetics = c("fill", "color")) +
  # The Box (Bottom, Top, Left, Right)
  geom_segment(x = (Left), y = (Bottom), xend = (Right), yend = (Bottom)) +
  geom_segment(x = (Left), y = (Top), xend = (Right), yend = (Top)) +
  geom_segment(x = (Left), y = (Bottom), xend = (Left), yend = (Top)) +
  geom_segment(x = (Right), y = (Bottom), xend = (Right), yend = (Top)) +
  
  # Horizontal Lines (Bottom Inner, Top Inner)
  geom_segment(x = (Left), y = (Bottom + Height), xend = (Right), yend = (Bottom + Height)) +
  geom_segment(x = (Left), y = (Top - Height), xend = (Right), yend = (Top - Height)) +
  
  # Vertical Lines (Left Inner, Right Inner)
  geom_segment(x = (Left + Width), y = (Bottom), xend = (Left + Width), yend = (Top)) +
  geom_segment(x = (Right - Width), y = (Bottom), xend = (Right - Width), yend = (Top)) +
  
  # Plate (Bottom, Left Side, Left Diagonal, Right Diagonal, Right Side)  
  geom_segment(x = (Left), y = (0), xend = (Right), yend = (0)) +
  geom_segment(x = (Left), y = (0), xend = (Left), yend = (4.25/12)) +
  geom_segment(x = (Left), y = (4.25/12), xend = (0), yend = (8.5/12)) +
  geom_segment(x = (Right), y = (4.25/12), xend = (Right), yend = (0)) +
  geom_segment(x = (0), y = (8.5/12), xend = (Right), yend = (4.25/12)) +
  
  xlim(-3,3) + ylim(0, 5)  + ggtitle(paste("RH-Fastball", pete_rh_fb$`Usage %`,"%")) +
  theme(
    legend.position = "none",
    plot.title = element_text(color = "black", size = 15, face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(color = "black", size = 1.5, fill = NA))


## Alosnso Offspeed heatmap

pete_rh_os <- pete_rh%>%filter(pitch_name == "Offspeed")

hm_pa_rh_os <- ggplot(pete_rh_os, mapping = aes(x=plate_x, y= plate_z)) +
  stat_density2d_filled()  +
  scale_fill_manual(values = c(heat_colors_interpolated), aesthetics = c("fill", "color")) +
  # The Box (Bottom, Top, Left, Right)
  geom_segment(x = (Left), y = (Bottom), xend = (Right), yend = (Bottom)) +
  geom_segment(x = (Left), y = (Top), xend = (Right), yend = (Top)) +
  geom_segment(x = (Left), y = (Bottom), xend = (Left), yend = (Top)) +
  geom_segment(x = (Right), y = (Bottom), xend = (Right), yend = (Top)) +
  
  # Horizontal Lines (Bottom Inner, Top Inner)
  geom_segment(x = (Left), y = (Bottom + Height), xend = (Right), yend = (Bottom + Height)) +
  geom_segment(x = (Left), y = (Top - Height), xend = (Right), yend = (Top - Height)) +
  
  # Vertical Lines (Left Inner, Right Inner)
  geom_segment(x = (Left + Width), y = (Bottom), xend = (Left + Width), yend = (Top)) +
  geom_segment(x = (Right - Width), y = (Bottom), xend = (Right - Width), yend = (Top)) +
  
  # Plate (Bottom, Left Side, Left Diagonal, Right Diagonal, Right Side)  
  geom_segment(x = (Left), y = (0), xend = (Right), yend = (0)) +
  geom_segment(x = (Left), y = (0), xend = (Left), yend = (4.25/12)) +
  geom_segment(x = (Left), y = (4.25/12), xend = (0), yend = (8.5/12)) +
  geom_segment(x = (Right), y = (4.25/12), xend = (Right), yend = (0)) +
  geom_segment(x = (0), y = (8.5/12), xend = (Right), yend = (4.25/12)) +
  
  xlim(-3,3) + ylim(0, 5)  + ggtitle(paste("RH-Offspeed", pete_rh_os$`Usage %`,"%")) +
  theme(
    legend.position = "none",
    plot.title = element_text(color = "black", size = 15, face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(color = "black", size = 1.5, fill = NA))


## Alonso Breakingball Heatmap

pete_rh_bb <- pete_rh%>%filter(pitch_name == "Breakingball")

hm_pa_rh_bb <- ggplot(pete_rh_bb, mapping = aes(x=plate_x, y= plate_z)) +
  stat_density2d_filled()  +
  scale_fill_manual(values = c(heat_colors_interpolated), aesthetics = c("fill", "color")) +
  # The Box (Bottom, Top, Left, Right)
  geom_segment(x = (Left), y = (Bottom), xend = (Right), yend = (Bottom)) +
  geom_segment(x = (Left), y = (Top), xend = (Right), yend = (Top)) +
  geom_segment(x = (Left), y = (Bottom), xend = (Left), yend = (Top)) +
  geom_segment(x = (Right), y = (Bottom), xend = (Right), yend = (Top)) +
  
  # Horizontal Lines (Bottom Inner, Top Inner)
  geom_segment(x = (Left), y = (Bottom + Height), xend = (Right), yend = (Bottom + Height)) +
  geom_segment(x = (Left), y = (Top - Height), xend = (Right), yend = (Top - Height)) +
  
  # Vertical Lines (Left Inner, Right Inner)
  geom_segment(x = (Left + Width), y = (Bottom), xend = (Left + Width), yend = (Top)) +
  geom_segment(x = (Right - Width), y = (Bottom), xend = (Right - Width), yend = (Top)) +
  
  # Plate (Bottom, Left Side, Left Diagonal, Right Diagonal, Right Side)  
  geom_segment(x = (Left), y = (0), xend = (Right), yend = (0)) +
  geom_segment(x = (Left), y = (0), xend = (Left), yend = (4.25/12)) +
  geom_segment(x = (Left), y = (4.25/12), xend = (0), yend = (8.5/12)) +
  geom_segment(x = (Right), y = (4.25/12), xend = (Right), yend = (0)) +
  geom_segment(x = (0), y = (8.5/12), xend = (Right), yend = (4.25/12)) +
  
  xlim(-3,3) + ylim(0, 5)  + ggtitle(paste("RH-Breakingball", pete_rh_bb$`Usage %`,"%")) +
  theme(
    legend.position = "none",
    plot.title = element_text(color = "black", size = 15, face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(color = "black", size = 1.5, fill = NA))


## Alonso LH Pitches

pete_lh <- pete_pitches %>%
  filter(pete_pitches$p_throws == "L")

## Usage LH Pitches TB

pete_freq_lh <- pete_lh%>%group_by(pitch_name)%>%tally()
pete_total_lh <- as.numeric(sum(pete_freq_lh$n))
pete_freq_lh$`Usage %` <- round((pete_freq_lh$n/pete_total_lh)*100, digits = 0)
pete_freq_lh <- subset(pete_freq_lh, select = -c(n))

# Merge Main DataSet with Usage %
pete_lh <- merge(pete_lh, pete_freq_lh)

## Alonso LH Fastball heatmap

pete_lh_fb <- pete_lh%>%filter(pitch_name == "Fastball")

hm_pa_lh_fb <- ggplot(pete_lh_fb, mapping = aes(x=plate_x, y= plate_z)) +
  stat_density2d_filled()  +
  scale_fill_manual(values = c(heat_colors_interpolated), aesthetics = c("fill", "color")) +
  # The Box (Bottom, Top, Left, Right)
  geom_segment(x = (Left), y = (Bottom), xend = (Right), yend = (Bottom)) +
  geom_segment(x = (Left), y = (Top), xend = (Right), yend = (Top)) +
  geom_segment(x = (Left), y = (Bottom), xend = (Left), yend = (Top)) +
  geom_segment(x = (Right), y = (Bottom), xend = (Right), yend = (Top)) +
  
  # Horizontal Lines (Bottom Inner, Top Inner)
  geom_segment(x = (Left), y = (Bottom + Height), xend = (Right), yend = (Bottom + Height)) +
  geom_segment(x = (Left), y = (Top - Height), xend = (Right), yend = (Top - Height)) +
  
  # Vertical Lines (Left Inner, Right Inner)
  geom_segment(x = (Left + Width), y = (Bottom), xend = (Left + Width), yend = (Top)) +
  geom_segment(x = (Right - Width), y = (Bottom), xend = (Right - Width), yend = (Top)) +
  
  # Plate (Bottom, Left Side, Left Diagonal, Right Diagonal, Right Side)  
  geom_segment(x = (Left), y = (0), xend = (Right), yend = (0)) +
  geom_segment(x = (Left), y = (0), xend = (Left), yend = (4.25/12)) +
  geom_segment(x = (Left), y = (4.25/12), xend = (0), yend = (8.5/12)) +
  geom_segment(x = (Right), y = (4.25/12), xend = (Right), yend = (0)) +
  geom_segment(x = (0), y = (8.5/12), xend = (Right), yend = (4.25/12)) +
  
  xlim(-3,3) + ylim(0, 5)  + ggtitle(paste("LH-Fastball", pete_lh_fb$`Usage %`,"%")) +
  theme(
    legend.position = "none",
    plot.title = element_text(color = "black", size = 15, face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(color = "black", size = 1.5, fill = NA))

## Alonso LH Offspeed heatmap

pete_lh_os <- pete_lh%>%filter(pitch_name == "Offspeed")

hm_pa_lh_os <- ggplot(pete_lh_os, mapping = aes(x=plate_x, y= plate_z)) +
  stat_density2d_filled()  +
  scale_fill_manual(values = c(heat_colors_interpolated), aesthetics = c("fill", "color")) +
  # The Box (Bottom, Top, Left, Right)
  geom_segment(x = (Left), y = (Bottom), xend = (Right), yend = (Bottom)) +
  geom_segment(x = (Left), y = (Top), xend = (Right), yend = (Top)) +
  geom_segment(x = (Left), y = (Bottom), xend = (Left), yend = (Top)) +
  geom_segment(x = (Right), y = (Bottom), xend = (Right), yend = (Top)) +
  
  # Horizontal Lines (Bottom Inner, Top Inner)
  geom_segment(x = (Left), y = (Bottom + Height), xend = (Right), yend = (Bottom + Height)) +
  geom_segment(x = (Left), y = (Top - Height), xend = (Right), yend = (Top - Height)) +
  
  # Vertical Lines (Left Inner, Right Inner)
  geom_segment(x = (Left + Width), y = (Bottom), xend = (Left + Width), yend = (Top)) +
  geom_segment(x = (Right - Width), y = (Bottom), xend = (Right - Width), yend = (Top)) +
  
  # Plate (Bottom, Left Side, Left Diagonal, Right Diagonal, Right Side)  
  geom_segment(x = (Left), y = (0), xend = (Right), yend = (0)) +
  geom_segment(x = (Left), y = (0), xend = (Left), yend = (4.25/12)) +
  geom_segment(x = (Left), y = (4.25/12), xend = (0), yend = (8.5/12)) +
  geom_segment(x = (Right), y = (4.25/12), xend = (Right), yend = (0)) +
  geom_segment(x = (0), y = (8.5/12), xend = (Right), yend = (4.25/12)) +
  
  xlim(-3,3) + ylim(0, 5)  + ggtitle(paste("LH-Offspeed", pete_lh_os$`Usage %`,"%")) +
  theme(
    legend.position = "none",
    plot.title = element_text(color = "black", size = 15, face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(color = "black", size = 1.5, fill = NA))

## Alonso LH Breakingball heatmap

pete_lh_bb <- pete_lh%>%filter(pitch_name == "Breakingball")

hm_pa_lh_bb <- ggplot(pete_lh_bb, mapping = aes(x=plate_x, y= plate_z)) +
  stat_density2d_filled()  +
  scale_fill_manual(values = c(heat_colors_interpolated), aesthetics = c("fill", "color")) +
  # The Box (Bottom, Top, Left, Right)
  geom_segment(x = (Left), y = (Bottom), xend = (Right), yend = (Bottom)) +
  geom_segment(x = (Left), y = (Top), xend = (Right), yend = (Top)) +
  geom_segment(x = (Left), y = (Bottom), xend = (Left), yend = (Top)) +
  geom_segment(x = (Right), y = (Bottom), xend = (Right), yend = (Top)) +
  
  # Horizontal Lines (Bottom Inner, Top Inner)
  geom_segment(x = (Left), y = (Bottom + Height), xend = (Right), yend = (Bottom + Height)) +
  geom_segment(x = (Left), y = (Top - Height), xend = (Right), yend = (Top - Height)) +
  
  # Vertical Lines (Left Inner, Right Inner)
  geom_segment(x = (Left + Width), y = (Bottom), xend = (Left + Width), yend = (Top)) +
  geom_segment(x = (Right - Width), y = (Bottom), xend = (Right - Width), yend = (Top)) +
  
  # Plate (Bottom, Left Side, Left Diagonal, Right Diagonal, Right Side)  
  geom_segment(x = (Left), y = (0), xend = (Right), yend = (0)) +
  geom_segment(x = (Left), y = (0), xend = (Left), yend = (4.25/12)) +
  geom_segment(x = (Left), y = (4.25/12), xend = (0), yend = (8.5/12)) +
  geom_segment(x = (Right), y = (4.25/12), xend = (Right), yend = (0)) +
  geom_segment(x = (0), y = (8.5/12), xend = (Right), yend = (4.25/12)) +
  
  xlim(-3,3) + ylim(0, 5)  + ggtitle(paste("LH-Breakingball", pete_lh_bb$`Usage %`,"%")) +
  theme(
    legend.position = "none",
    plot.title = element_text(color = "black", size = 15, face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(color = "black", size = 1.5, fill = NA))

plot_alonso <- ggarrange(

ggarrange(hm_pa_rh_fb, hm_pa_rh_os, hm_pa_rh_bb, nrow = 1, ncol = 3),
ggarrange(hm_pa_lh_fb, hm_pa_lh_os, hm_pa_lh_bb, nrow = 1, ncol = 3),

nrow = 2, ncol = 1)

# Walker pitches

walker_pitches <- cw %>%
  select(pitch_name, plate_x, plate_z, p_throws)

## Catorgorizing Pitches

walker_pitches$pitch_name[walker_pitches$pitch_name == "4-Seam Fastball"] <- "Fastball"
walker_pitches$pitch_name[walker_pitches$pitch_name == "Sinker"] <- "Fastball"
walker_pitches$pitch_name[walker_pitches$pitch_name == "Cutter"] <- "Fastball"

walker_pitches$pitch_name[walker_pitches$pitch_name == "Curveball"] <- "Breakingball"
walker_pitches$pitch_name[walker_pitches$pitch_name == "Knuckle Curve"] <- "Breakingball"
walker_pitches$pitch_name[walker_pitches$pitch_name == "Slurve"] <- "Breakingball"
walker_pitches$pitch_name[walker_pitches$pitch_name == "Slider"] <- "Breakingball"
walker_pitches$pitch_name[walker_pitches$pitch_name == "Sweeper"] <- "Breakingball"

walker_pitches$pitch_name[walker_pitches$pitch_name == "Changeup"] <- "Offspeed"
walker_pitches$pitch_name[walker_pitches$pitch_name == "Knuckleball"] <- "Offspeed"
walker_pitches$pitch_name[walker_pitches$pitch_name == "Split-Finger"] <- "Offspeed"


## Walker RH Pitches

walker_rh <- walker_pitches %>%
  filter(walker_pitches$p_throws == "R")

## Usage RH Pitches TB

walker_freq_rh <- walker_rh%>%group_by(pitch_name)%>%tally()
walker_total_rh <- as.numeric(sum(walker_freq_rh$n))
walker_freq_rh$`Usage %` <- round((walker_freq_rh$n/walker_total_rh)*100, digits = 0)
walker_freq_rh <- subset(walker_freq_rh, select = -c(n))



# Merge Main DataSet with Usage %
walker_rh <- merge(walker_rh, walker_freq_rh)

# Add in Parameters for Strike Zone / Plate
Left <- -8.5/12
Right <- 8.5/12
Bottom <- 18.29/12
Top <- 44.08/12

# This is to Make a 3x3 Strike Zone (Vertical and Horizontal Lines in Zone)
Width <- (Right - Left) / 3
Height <- (Top - Bottom) / 3

# Walker RH Fastball Heatmap

walker_rh_fb <- walker_rh%>%filter(pitch_name == "Fastball")

hm_cw_rh_fb <- ggplot(walker_rh_fb, mapping = aes(x=plate_x, y= plate_z)) +
  stat_density2d_filled()  +
  scale_fill_manual(values = c(heat_colors_interpolated), aesthetics = c("fill", "color")) +
  # The Box (Bottom, Top, Left, Right)
  geom_segment(x = (Left), y = (Bottom), xend = (Right), yend = (Bottom)) +
  geom_segment(x = (Left), y = (Top), xend = (Right), yend = (Top)) +
  geom_segment(x = (Left), y = (Bottom), xend = (Left), yend = (Top)) +
  geom_segment(x = (Right), y = (Bottom), xend = (Right), yend = (Top)) +
  
  # Horizontal Lines (Bottom Inner, Top Inner)
  geom_segment(x = (Left), y = (Bottom + Height), xend = (Right), yend = (Bottom + Height)) +
  geom_segment(x = (Left), y = (Top - Height), xend = (Right), yend = (Top - Height)) +
  
  # Vertical Lines (Left Inner, Right Inner)
  geom_segment(x = (Left + Width), y = (Bottom), xend = (Left + Width), yend = (Top)) +
  geom_segment(x = (Right - Width), y = (Bottom), xend = (Right - Width), yend = (Top)) +
  
  # Plate (Bottom, Left Side, Left Diagonal, Right Diagonal, Right Side)  
  geom_segment(x = (Left), y = (0), xend = (Right), yend = (0)) +
  geom_segment(x = (Left), y = (0), xend = (Left), yend = (4.25/12)) +
  geom_segment(x = (Left), y = (4.25/12), xend = (0), yend = (8.5/12)) +
  geom_segment(x = (Right), y = (4.25/12), xend = (Right), yend = (0)) +
  geom_segment(x = (0), y = (8.5/12), xend = (Right), yend = (4.25/12)) +
  
  xlim(-3,3) + ylim(0, 5)  + ggtitle(paste("RH-Fastball", walker_rh_fb$`Usage %`,"%")) +
  theme(
    legend.position = "none",
    plot.title = element_text(color = "black", size = 15, face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(color = "black", size = 1.5, fill = NA))


## Walker Offspeed heatmap

walker_rh_os <- walker_rh%>%filter(pitch_name == "Offspeed")

hm_cw_rh_os <- ggplot(walker_rh_os, mapping = aes(x=plate_x, y= plate_z)) +
  stat_density2d_filled()  +
  scale_fill_manual(values = c(heat_colors_interpolated), aesthetics = c("fill", "color")) +
  # The Box (Bottom, Top, Left, Right)
  geom_segment(x = (Left), y = (Bottom), xend = (Right), yend = (Bottom)) +
  geom_segment(x = (Left), y = (Top), xend = (Right), yend = (Top)) +
  geom_segment(x = (Left), y = (Bottom), xend = (Left), yend = (Top)) +
  geom_segment(x = (Right), y = (Bottom), xend = (Right), yend = (Top)) +
  
  # Horizontal Lines (Bottom Inner, Top Inner)
  geom_segment(x = (Left), y = (Bottom + Height), xend = (Right), yend = (Bottom + Height)) +
  geom_segment(x = (Left), y = (Top - Height), xend = (Right), yend = (Top - Height)) +
  
  # Vertical Lines (Left Inner, Right Inner)
  geom_segment(x = (Left + Width), y = (Bottom), xend = (Left + Width), yend = (Top)) +
  geom_segment(x = (Right - Width), y = (Bottom), xend = (Right - Width), yend = (Top)) +
  
  # Plate (Bottom, Left Side, Left Diagonal, Right Diagonal, Right Side)  
  geom_segment(x = (Left), y = (0), xend = (Right), yend = (0)) +
  geom_segment(x = (Left), y = (0), xend = (Left), yend = (4.25/12)) +
  geom_segment(x = (Left), y = (4.25/12), xend = (0), yend = (8.5/12)) +
  geom_segment(x = (Right), y = (4.25/12), xend = (Right), yend = (0)) +
  geom_segment(x = (0), y = (8.5/12), xend = (Right), yend = (4.25/12)) +
  
  xlim(-3,3) + ylim(0, 5)  + ggtitle(paste("RH-Offspeed", walker_rh_os$`Usage %`,"%")) +
  theme(
    legend.position = "none",
    plot.title = element_text(color = "black", size = 15, face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(color = "black", size = 1.5, fill = NA))


## Walker Breakingball Heatmap

walker_rh_bb <- walker_rh%>%filter(pitch_name == "Breakingball")

hm_cw_rh_bb <- ggplot(walker_rh_bb, mapping = aes(x=plate_x, y= plate_z)) +
  stat_density2d_filled()  +
  scale_fill_manual(values = c(heat_colors_interpolated), aesthetics = c("fill", "color")) +
  # The Box (Bottom, Top, Left, Right)
  geom_segment(x = (Left), y = (Bottom), xend = (Right), yend = (Bottom)) +
  geom_segment(x = (Left), y = (Top), xend = (Right), yend = (Top)) +
  geom_segment(x = (Left), y = (Bottom), xend = (Left), yend = (Top)) +
  geom_segment(x = (Right), y = (Bottom), xend = (Right), yend = (Top)) +
  
  # Horizontal Lines (Bottom Inner, Top Inner)
  geom_segment(x = (Left), y = (Bottom + Height), xend = (Right), yend = (Bottom + Height)) +
  geom_segment(x = (Left), y = (Top - Height), xend = (Right), yend = (Top - Height)) +
  
  # Vertical Lines (Left Inner, Right Inner)
  geom_segment(x = (Left + Width), y = (Bottom), xend = (Left + Width), yend = (Top)) +
  geom_segment(x = (Right - Width), y = (Bottom), xend = (Right - Width), yend = (Top)) +
  
  # Plate (Bottom, Left Side, Left Diagonal, Right Diagonal, Right Side)  
  geom_segment(x = (Left), y = (0), xend = (Right), yend = (0)) +
  geom_segment(x = (Left), y = (0), xend = (Left), yend = (4.25/12)) +
  geom_segment(x = (Left), y = (4.25/12), xend = (0), yend = (8.5/12)) +
  geom_segment(x = (Right), y = (4.25/12), xend = (Right), yend = (0)) +
  geom_segment(x = (0), y = (8.5/12), xend = (Right), yend = (4.25/12)) +
  
  xlim(-3,3) + ylim(0, 5)  + ggtitle(paste("RH-Breakingball", walker_rh_bb$`Usage %`,"%")) +
  theme(
    legend.position = "none",
    plot.title = element_text(color = "black", size = 15, face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(color = "black", size = 1.5, fill = NA))


## Walker LH Pitches

walker_lh <- walker_pitches %>%
  filter(walker_pitches$p_throws == "L")

## Usage LH Pitches TB

walker_freq_lh <- walker_lh%>%group_by(pitch_name)%>%tally()
walker_total_lh <- as.numeric(sum(walker_freq_lh$n))
walker_freq_lh$`Usage %` <- round((walker_freq_lh$n/walker_total_lh)*100, digits = 0)
walker_freq_lh <- subset(walker_freq_lh, select = -c(n))

# Merge Main DataSet with Usage %
walker_lh <- merge(walker_lh, walker_freq_lh)

## Walker LH Fastball heatmap

walker_lh_fb <- walker_lh%>%filter(pitch_name == "Fastball")

hm_cw_lh_fb <- ggplot(walker_lh_fb, mapping = aes(x=plate_x, y= plate_z)) +
  stat_density2d_filled()  +
  scale_fill_manual(values = c(heat_colors_interpolated), aesthetics = c("fill", "color")) +
  # The Box (Bottom, Top, Left, Right)
  geom_segment(x = (Left), y = (Bottom), xend = (Right), yend = (Bottom)) +
  geom_segment(x = (Left), y = (Top), xend = (Right), yend = (Top)) +
  geom_segment(x = (Left), y = (Bottom), xend = (Left), yend = (Top)) +
  geom_segment(x = (Right), y = (Bottom), xend = (Right), yend = (Top)) +
  
  # Horizontal Lines (Bottom Inner, Top Inner)
  geom_segment(x = (Left), y = (Bottom + Height), xend = (Right), yend = (Bottom + Height)) +
  geom_segment(x = (Left), y = (Top - Height), xend = (Right), yend = (Top - Height)) +
  
  # Vertical Lines (Left Inner, Right Inner)
  geom_segment(x = (Left + Width), y = (Bottom), xend = (Left + Width), yend = (Top)) +
  geom_segment(x = (Right - Width), y = (Bottom), xend = (Right - Width), yend = (Top)) +
  
  # Plate (Bottom, Left Side, Left Diagonal, Right Diagonal, Right Side)  
  geom_segment(x = (Left), y = (0), xend = (Right), yend = (0)) +
  geom_segment(x = (Left), y = (0), xend = (Left), yend = (4.25/12)) +
  geom_segment(x = (Left), y = (4.25/12), xend = (0), yend = (8.5/12)) +
  geom_segment(x = (Right), y = (4.25/12), xend = (Right), yend = (0)) +
  geom_segment(x = (0), y = (8.5/12), xend = (Right), yend = (4.25/12)) +
  
  xlim(-3,3) + ylim(0, 5)  + ggtitle(paste("LH-Fastball", walker_lh_fb$`Usage %`,"%")) +
  theme(
    legend.position = "none",
    plot.title = element_text(color = "black", size = 15, face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(color = "black", size = 1.5, fill = NA))

## Walker LH Offspeed heatmap

walker_lh_os <- walker_lh%>%filter(pitch_name == "Offspeed")

hm_cw_lh_os <- ggplot(walker_lh_os, mapping = aes(x=plate_x, y= plate_z)) +
  stat_density2d_filled()  +
  scale_fill_manual(values = c(heat_colors_interpolated), aesthetics = c("fill", "color")) +
  # The Box (Bottom, Top, Left, Right)
  geom_segment(x = (Left), y = (Bottom), xend = (Right), yend = (Bottom)) +
  geom_segment(x = (Left), y = (Top), xend = (Right), yend = (Top)) +
  geom_segment(x = (Left), y = (Bottom), xend = (Left), yend = (Top)) +
  geom_segment(x = (Right), y = (Bottom), xend = (Right), yend = (Top)) +
  
  # Horizontal Lines (Bottom Inner, Top Inner)
  geom_segment(x = (Left), y = (Bottom + Height), xend = (Right), yend = (Bottom + Height)) +
  geom_segment(x = (Left), y = (Top - Height), xend = (Right), yend = (Top - Height)) +
  
  # Vertical Lines (Left Inner, Right Inner)
  geom_segment(x = (Left + Width), y = (Bottom), xend = (Left + Width), yend = (Top)) +
  geom_segment(x = (Right - Width), y = (Bottom), xend = (Right - Width), yend = (Top)) +
  
  # Plate (Bottom, Left Side, Left Diagonal, Right Diagonal, Right Side)  
  geom_segment(x = (Left), y = (0), xend = (Right), yend = (0)) +
  geom_segment(x = (Left), y = (0), xend = (Left), yend = (4.25/12)) +
  geom_segment(x = (Left), y = (4.25/12), xend = (0), yend = (8.5/12)) +
  geom_segment(x = (Right), y = (4.25/12), xend = (Right), yend = (0)) +
  geom_segment(x = (0), y = (8.5/12), xend = (Right), yend = (4.25/12)) +
  
  xlim(-3,3) + ylim(0, 5)  + ggtitle(paste("LH-Offspeed", walker_lh_os$`Usage %`,"%")) +
  theme(
    legend.position = "none",
    plot.title = element_text(color = "black", size = 15, face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(color = "black", size = 1.5, fill = NA))

## Walker LH Breakingball heatmap

walker_lh_bb <- walker_lh%>%filter(pitch_name == "Breakingball")

hm_cw_lh_bb <- ggplot(walker_lh_bb, mapping = aes(x=plate_x, y= plate_z)) +
  stat_density2d_filled()  +
  scale_fill_manual(values = c(heat_colors_interpolated), aesthetics = c("fill", "color")) +
  # The Box (Bottom, Top, Left, Right)
  geom_segment(x = (Left), y = (Bottom), xend = (Right), yend = (Bottom)) +
  geom_segment(x = (Left), y = (Top), xend = (Right), yend = (Top)) +
  geom_segment(x = (Left), y = (Bottom), xend = (Left), yend = (Top)) +
  geom_segment(x = (Right), y = (Bottom), xend = (Right), yend = (Top)) +
  
  # Horizontal Lines (Bottom Inner, Top Inner)
  geom_segment(x = (Left), y = (Bottom + Height), xend = (Right), yend = (Bottom + Height)) +
  geom_segment(x = (Left), y = (Top - Height), xend = (Right), yend = (Top - Height)) +
  
  # Vertical Lines (Left Inner, Right Inner)
  geom_segment(x = (Left + Width), y = (Bottom), xend = (Left + Width), yend = (Top)) +
  geom_segment(x = (Right - Width), y = (Bottom), xend = (Right - Width), yend = (Top)) +
  
  # Plate (Bottom, Left Side, Left Diagonal, Right Diagonal, Right Side)  
  geom_segment(x = (Left), y = (0), xend = (Right), yend = (0)) +
  geom_segment(x = (Left), y = (0), xend = (Left), yend = (4.25/12)) +
  geom_segment(x = (Left), y = (4.25/12), xend = (0), yend = (8.5/12)) +
  geom_segment(x = (Right), y = (4.25/12), xend = (Right), yend = (0)) +
  geom_segment(x = (0), y = (8.5/12), xend = (Right), yend = (4.25/12)) +
  
  xlim(-3,3) + ylim(0, 5)  + ggtitle(paste("LH-Breakingball", walker_lh_bb$`Usage %`,"%")) +
  theme(
    legend.position = "none",
    plot.title = element_text(color = "black", size = 15, face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(color = "black", size = 1.5, fill = NA))

plot_walker <- ggarrange(
  
  ggarrange(hm_cw_rh_fb, hm_cw_rh_os, hm_cw_rh_bb, nrow = 1, ncol = 3),
  ggarrange(hm_cw_lh_fb, hm_cw_lh_os, hm_cw_lh_bb, nrow = 1, ncol = 3),
  
  nrow = 2, ncol = 1)


###Final Visual
  
plot1 <- ggarrange(
  
  
  ggarrange(pa_ba, pa_hits, pa_hrs, pa_slg, pa_obp, pa_ops, pa_k_percent, pa_bb_percent, nrow = 3, ncol = 3),
  
  ggarrange(cw_ba, cw_hits, cw_hrs, cw_slg, cw_obp, cw_ops, cw_k_percent, cw_bb_percent, nrow = 3, ncol = 3),
  
  nrow = 1, ncol = 2)

title <- expression(atop(bold("1st Baseman Offensive Percentiles from 2022-2024"), scriptstyle("MIN 200 PA")))

plot1 <- annotate_figure(plot1, top = text_grob(title))
  
plot_alonso <- ggarrange(
  
  ggarrange(hm_pa_rh_fb, hm_pa_rh_os, hm_pa_rh_bb, nrow = 1, ncol = 3),
  ggarrange(hm_pa_lh_fb, hm_pa_lh_os, hm_pa_lh_bb, nrow = 1, ncol = 3),
  
  nrow = 2, ncol = 1)

plot_walker <- ggarrange(
  
  ggarrange(hm_cw_rh_fb, hm_cw_rh_os, hm_cw_rh_bb, nrow = 1, ncol = 3),
  ggarrange(hm_cw_lh_fb, hm_cw_lh_os, hm_cw_lh_bb, nrow = 1, ncol = 3),
  
  nrow = 2, ncol = 1)

title_alonso <- expression(atop(bold("Pete Alonso 2024")))

plot_alonso <- annotate_figure(plot_alonso, top = text_grob(title_alonso))

title_walker <- expression(atop(bold("Christian Walker 2024")))

plot_walker <- annotate_figure(plot_walker, top = text_grob(title_walker))


plot2 <- ggarrange(plot_alonso, plot_walker, nrow = 1, ncol = 2)

ggarrange(plot1, plot2, nrow = 2, ncol = 1)
