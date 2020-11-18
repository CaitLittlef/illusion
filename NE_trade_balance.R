##########################################
#### TRADE BALANCE NEW ENGLAND STATES ####
##########################################


## Load data
import <- read.csv(paste0(data.dir, "State Imports by NAICS Commodities_all_sts.csv"), skip = 3)
export <- read.csv(paste0(data.dir, "State Exports by NAICS Commodities_all_sts.csv"), skip = 3)

colnames(import)
colnames(import) <- c("state", "commodity", "origin", "year", "USD")
colnames(export)
colnames(export) <- c("state", "commodity", "destination", "year", "USD")



## Inspect variables.

# State 
levels(import$state)
levels(export$state)

target <- c("Connecticut", "Maine", "Massachusetts", "New Hampshire", "New York", "Rhode Island", "Vermont")
import <- import %>% filter(state %in% target)
export <- export %>% filter(state %in% target)


# Commodity. N.b., 4-digit are subset of 3-digit
# See descriptions in NAICS doc.
levels(import$commodity)
levels(export$commodity)

# [1] "113 Forestry Products, Nesoi"                    "1132 Forestry Products"                         
# [3] "1133 Timber & Logs"                              "321 Wood Products"                              
# [5] "3211 Sawmill & Wood Products"                    "3212 Veneer, Plywood & Engineered Wood Products"
# [7] "3219 Other Wood Products"                        "322 Paper"                                      
# [9] "3221 Pulp, Paper & Paperboard Mill Products"     "3222 Converted Paper Products" 

# Retain 1133 timber & logs but not 1132 forestry products (incl. nursery stock, some other NTFPs); p. 98
# Retain 3211 & 3212 but not 3219 (incl. wood containers, mobile home manufacturing, broom handles, etc.); p. 182
# Retain 3221 but not 3222 (incl. food containers, fiber reels, stationary, etc.) ; p. 186

comm <- c("1133 Timber & Logs",
          "3211 Sawmill & Wood Products",
          "3212 Veneer, Plywood & Engineered Wood Products",
          "3221 Pulp, Paper & Paperboard Mill Products")


# Oirigin. N.b., Mexico and Canada fall under North Amer
levels(import$origin)
levels(export$destination)


# Year
range(import$year)
range(export$year) # nix earlier exports to match imports
export <- export %>% filter(year>2007)


# USD
import$USD <- as.numeric(as.character(gsub(",","",import$USD))) # nix commas, convert to char, then num from factor
export$USD <- as.numeric(as.character(gsub(",","",export$USD))) # nix commas, convert to char, then num from factor


import[] <- import[] %>% droplevels()
export[] <- export[] %>% droplevels()



## Combine tables by state, year, and commodity
i <- import
e <- export
i <- i %>% rename("partner" = "origin", "USDi" = "USD")
e <- e %>% rename("partner" = "destination", "USDe" = "USD")


tb <- full_join(i, e, by = c("state", "year", "commodity", "partner"))
tb$USDi[is.na(tb$USDi)] <- 0 # where no matches, will be NAs. Set to zero.
tb$USDe[is.na(tb$USDe)] <- 0 # where no matches, will be NAs. Set to zero.
tb$USDtb <- tb$USDe - tb$USDi


## Filter and summarize for plotting

levels(tb$partner)
part <- c("Africa", "Asia", "Australia and Oceania", "Europe", "North America", "South/Central America")

data <- tb %>%
  filter(partner == "All Geographic Regions (World Total)",
         commodity %in% comm)

data <- data %>% group_by(state, year) %>%
  summarise(ttl_USDtb = sum(USDtb))

range(data$ttl_USDtb, na.rm = TRUE)




## Plot 
# st_labels <- c("CT", "ME", "MA, "NH", "NY", "RI", "VT")
g <- ggplot(data, aes(x = year, y = ttl_USDtb/1000, fill = state)) +
  geom_hline(yintercept=0, cex = 0.3, col = "grey") + 
  geom_col(width = 0.75, position = "dodge") +
  labs(title = "Trade balance of wood, pulp, and paper products",
       x = NULL,
       y = "Trade balance (thousands US$)") +
  scale_x_continuous(breaks = seq(2008, 2020, 1)) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE),
                     breaks = seq(-1000000000, 375000000, 250000)) +
  scale_fill_manual(labels = target, #st_labels,
                    values = palette) +
  theme_bw(base_size = 14) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16), # centers title
        # legend.justification=c(1,0), 
        # legend.position = c(0.42, 0), # for raw
        legend.position = "bottom", # left/up if scaled per capita
        legend.title = element_blank(),
        legend.box = "vertical",
        legend.text=element_text(size=12), #angle = 90),
        # legend.background = element_rect(color = "transparent", fill = "white")) #+
        legend.background = element_rect(color = "transparent", fill = "transparent")) #+
# annotate("text", x = 2018, y = 200000, label = "imports not\nincluded", size = 4)
g

dev.off()



## Save as pdf by version
# v <- 1

pdf(paste0(out.dir, "NE-all_tb_v",v,"_", currentDate, ".pdf"), 
    width = 7, height = 4) ; v <- v+1
g
dev.off()
