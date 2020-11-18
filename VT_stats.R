#######################################
#### VT PRODUCTION VS. CONSUMPTION ####
#######################################

## Load data
data <- read.csv(paste0(wd, "data/VT_mills.csv"), skip = 1) # skip first row of long names
colnames(data)



## Nix irrelevant variables
data <- data %>% dplyr::select(year, sawlogs_mbf, log_exp_mbf, log_imp_mbf, consumption_mbf)



## Fill in missing consumption for 2007 and 2008
data[data$year == 2007,]$consumption_mbf =
  data[data$year == 2007,]$sawlogs_mbf - data[data$year == 2007,]$log_exp_mbf + data[data$year == 2007,]$log_imp_mbf
data[data$year == 2008,]$consumption_mbf =
  data[data$year == 2008,]$sawlogs_mbf - data[data$year == 2008,]$log_exp_mbf + data[data$year == 2008,]$log_imp_mbf



## Remove imports
data$consumption_mbf <- (data$consumption_mbf - data$log_imp_mbf)
data <- data %>% dplyr::select(year, sawlogs_mbf, log_exp_mbf, consumption_mbf)
data[1:5,]



## Gather data for plotting; turn cat into factor with desired order
data <- data %>% gather(key = "category", value = "volume_mbf", -year)
data$category <- as.factor(data$category)
#levels(data$category) <- rev(levels(data$category))


## Plot 

g <- ggplot() +
  geom_line(data = data,
            aes(x = year, y = volume_mbf, color = category, linetype = category),
            cex = 2) +
  labs(title = "Vermont sawlog production and consumption",
       x = NULL,
       y = "Volume (Mbf)") + 
  scale_x_continuous(breaks = seq(1985, 2015, 5)) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE),
                     breaks = seq(0, 400000, 50000)) +
  scale_color_manual(labels = c("consumption", "exports", "production"),
                     # values = c("coral2", "aquamarine2", "aquamarine4")) +
                     values = c("#7570B3", "#66A61E", "#1B9E77")) +
  scale_linetype_manual(values=c("solid", "longdash", "solid"), guide = "none") +
  guides(color = guide_legend(reverse = TRUE), linetype = "none") +
  theme_bw(base_size = 14) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size = 0.25),
        plot.title = element_text(hjust = 0.5, size = 16), # centers title
        legend.justification=c(1,0), 
        legend.position = c(0.99, 0.68),
        legend.title = element_blank(),
        legend.box = "horizontal",
        legend.text=element_text(size=12), #angle = 90),
        legend.background = element_rect(color = "transparent", fill = "white")) #+
  # annotate("text", x = 2018, y = 200000, label = "imports not\nincluded", size = 4)
g

dev.off()


## Save as pdf by version
v <- 1
pdf(paste0(out.dir, "VT_logs_v",v,"_", currentDate, ".pdf"),
    width = 6, height = 4) ; v <- v+1
g
dev.off()




