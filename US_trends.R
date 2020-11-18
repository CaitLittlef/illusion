##########################################
####   US PRODUCTION VS CONSUMPTION   ####
##########################################

# source: Tbl. 5a HowardLiang_2019_USTimberProdTradeConsumption1965-2017_fpl_rp701
# incl. all industrial roundwood production and consumpion -- no fuelwood tho.

## Load data; remove commas; convert all to numeric
data <- read.csv(paste0(data.dir, "US_prodconsump_Howard19.csv"))
data[] <- lapply(data, gsub, pattern=",", replacement="")
data[] <- lapply(data, as.numeric)

## Gather
data <- data %>% gather(key = "category", value = "volume_mmcft3", -Year)
data$category <- factor(data$category, levels = c("Production", "Imports", "Exports", "Consumption"))

display.brewer.pal(8, "Dark2")


## Plot 
g <- ggplot() +
  geom_line(data = data,
            aes(x = Year, y = volume_mmcft3, color = category, linetype = category),
            cex = 2) +
  labs(title = "US production and consumption",
       x = NULL,
       y = "Volume (mm ft3)") + 
  scale_x_continuous(breaks = seq(1965, 2015, 5)) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE),
                     breaks = seq(0, 20000, 2500)) +
  scale_color_manual(labels = c("production", "exports", "imports", "consumption"),
                     # values = c("coral2", "aquamarine2", "aquamarine4", "orange")
                     values = palette[c(1,2,3,6)]) +
  scale_linetype_manual(values=c("solid", "solid", "solid", "solid"), guide = "none") +
  guides(color = guide_legend(reverse = TRUE), linetype = "none") +
  theme_bw(base_size = 14) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size = 0.25),
        plot.title = element_text(hjust = 0.5, size = 16), # centers title
        legend.justification=c(1,0), 
        legend.position = c(0.66, 0.26),
        legend.title = element_blank(),
        legend.box = "horizontal",
        legend.text=element_text(size=12), #angle = 90),
        legend.background = element_rect(color = "transparent", fill = "white")) #+
# annotate("text", x = 2018, y = 200000, label = "imports not\nincluded", size = 4)
g

dev.off()


## Save as pdf by version
# v <- 1
pdf(paste0(out.dir, "US_prodcons_v",v,"_", currentDate, ".pdf"),
    width = 6, height = 4) ; v <- v+1
g
dev.off()





