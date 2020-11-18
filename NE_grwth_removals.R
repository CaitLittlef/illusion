##########################################
#### NORTHEAST GROWTH:REMOVALS & MORT ####
##########################################


## Load data
data <- read.csv(paste0(data.dir, "NE_2016_grwth-removals-mort.csv"))
data$state <- trimws(data$state, which = c("right")) %>% as.factor() %>% droplevels()  # remove extra space after states

area <- read.csv(paste0(data.dir, "NE_2017_area.csv"))
area$state <- trimws(area$state, which = c("right")) %>% as.factor() %>% droplevels() # remove extra space after states
area <- area %>% dplyr::select(state, ttl_timber_acre)

pop <- read.csv(paste0(data.dir, "NE_pop.csv"))
# pop$state <- trimws(pop$state, which = c("right")) %>% as.factor() %>% droplevels() # remove extra space after states
pop <- pop %>% dplyr::select(NAME, X2017)
colnames(pop) <- c("state", "2017")


## Select and reorder key states
# target <- c("Vermont", "New York", "New Hampshire", "Maine")
target <- c("Connecticut", "Maine", "Massachusetts", "New Hampshire", "New York", "Rhode Island", "Vermont")
data <- data %>% filter(state %in% target)
data[] <- data[] %>% droplevels()
data$state <- factor(data$state,
                     levels = c("Connecticut", "Maine", "Massachusetts", "New Hampshire", "New York", "Rhode Island", "Vermont"))

area <- area %>% filter(state %in% target)
area[] <- area[] %>% droplevels()
area$state <- factor(area$state,
                     levels = c("Connecticut", "Maine", "Massachusetts", "New Hampshire", "New York", "Rhode Island", "Vermont"))

pop <- pop %>% filter(state %in% target)
pop[] <- pop[] %>% droplevels()
pop$state <- factor(pop$state,
                     levels = c("Connecticut", "Maine", "Massachusetts", "New Hampshire", "New York", "Rhode Island", "Vermont"))

identical(data$state, area$state, pop$state)
data$ttl_timber_acre <- area$ttl_timber_acre
data$pop17 <- pop$`2017`



## Gather data for plotting; turn cat into factor with desired order
data <- data %>% gather(key = "category", value = "volume_mcf", -state, -ttl_timber_acre, -pop17)
data$category <- factor(data$category, levels = c("net_grwth_mcf", "removals_mcf", "mort_mcf"))


## Plot 
st_labels <- c("CT", "ME", "MA", "NH", "NY", "RI", "VT")
# g <- ggplot(data, aes(x = state, y = volume_mcf, fill = category)) +
# g <- ggplot(data, aes(x = state, y = volume_mcf/ttl_timber_acre, fill = category)) +
g <- ggplot(data, aes(x = state, y = volume_mcf/pop17, fill = category)) +
  geom_bar(stat = "identity", width = 0.75) +
  # labs(title = "Growth, removals, and mortality of growing stock, 2016",
  # labs(title = "Growth, removals, and mortality of growing stock by area, 2016",
  labs(title = "Growth, removals, and mortality of growing stock per capita, 2016",
       x = NULL,
       # y = "Volume (Mcf)") +
       # y = "Volume (Mcf) / acre") +
       y = "Volume (Mcf) per capita") +
  scale_x_discrete(labels = st_labels) +
  # scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE),
  #                    breaks = seq(0, 1500000, 250000)) +
  # scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE),
  #                    breaks = seq(0, 90, 10)) + # for area
  scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE),
                     breaks = seq(0, 1, 0.25)) + # for population
  scale_fill_manual(labels = c("growth", "removals", "mortality"),
                    # values = c("aquamarine4", "coral2", "aquamarine2")) +
                    values = c("#1B9E77", "#7570B3", "#66A61E")) +
  theme_bw(base_size = 14) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16), # centers title
        legend.justification=c(1,0), 
        # legend.position = c(0.99, 0.68), # for raw
        # legend.position = c(0.87, 0.73), # left/up if scaled per area
        legend.position = c(0.99, 0.68), # left/up if scaled per capita
        legend.title = element_blank(),
        legend.box = "horizontal",
        legend.text=element_text(size=12), #angle = 90),
        # legend.background = element_rect(color = "transparent", fill = "white")) #+
        legend.background = element_rect(color = "transparent", fill = "transparent")) #+
# annotate("text", x = 2018, y = 200000, label = "imports not\nincluded", size = 4)
g

dev.off()


## Save as pdf by version
# v <- 1
# pdf(paste0(out.dir, "ME-NY-NH-VT_ratios_v",v,"_", currentDate, ".pdf"),
# pdf(paste0(out.dir, "NE-all_ratios_v",v,"_", currentDate, ".pdf"),
# pdf(paste0(out.dir, "NE-all_ratios_per_area_v",v,"_", currentDate, ".pdf"),
pdf(paste0(out.dir, "NE-all_ratios_per_pop_v",v,"_", currentDate, ".pdf"),
    width = 7, height = 4) ; v <- v+1
g

dev.off()




