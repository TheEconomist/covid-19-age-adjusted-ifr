### Load population data (from the UN):
library(readxl)
pop <- read_xlsx("source-data/WPP2019_POP_F15_1_ANNUAL_POPULATION_BY_AGE_BOTH_SEXES.xlsx", skip = 16)
pop <- pop[pop$`Reference date (as of 1 July)` == "2020", ]

library(data.table)
pop <- melt(setDT(pop), id.vars = colnames(pop)[1:8], variable.name = "age")
pop$value <- as.numeric(pop$value)

# Get age range:
pop$age_min <- NA
pop$age_max <- NA
for(i in unique(pop$age)){
  if(i == "100+"){
    pop$age_min[pop$age == i] <- 100
    pop$age_max[pop$age == i] <- 125
    
  } else {
  pop$age_min[pop$age == i] <- as.numeric(strsplit(as.character(i), "-")[[1]][1])
  pop$age_max[pop$age == i] <- as.numeric(strsplit(as.character(i), "-")[[1]][2])
  }
}
pop$age_mean <- (pop$age_min + pop$age_max)/2

### Load IFR by age:
ifr <- read.csv("source-data/ifr_imperial.csv")[1:19, 1:3]
colnames(ifr) <- c("age", "ifr_no_seroreversion", "ifr_with_seroreversion")

# Append categories in UN data to facilitate merging:
ifr <- rbind(ifr, ifr[ifr$age == "90+", ], ifr[ifr$age == "90+", ])
ifr$age[(nrow(ifr)-2):nrow(ifr)] <- c("90-94", "95-100", "100+")

# We now know that seroreversion takes places, so use this estimate as our preferred estimate:
ifr$ifr <- as.numeric(sapply(ifr$ifr_with_seroreversion, FUN = function(x) strsplit(x, " ")[[1]][1]))
ifr$ifr_min <- sapply(ifr$ifr_with_seroreversion, FUN = function(x) strsplit(x, " ")[[1]][2])
ifr$ifr_max <- sapply(ifr$ifr_with_seroreversion, FUN = function(x) strsplit(x, " ")[[1]][3])
ifr$ifr_min <- gsub("\\(", "", ifr$ifr_min)
ifr$ifr_min <- as.numeric(gsub("\\,", "", ifr$ifr_min))
ifr$ifr_max <- as.numeric(gsub("\\)", "", ifr$ifr_max))

# Merge into population data:
pop <- merge(pop, ifr, by = "age")

### Alternative IFR by age:
# log_10 (IFR) = -3.27 + 0.0524*age
# Source: https://www.medrxiv.org/content/10.1101/2020.07.23.20160895v6.full.pdf
# Implies:
pop$ifr_alt <- (exp(-3.27 + 0.0524*pop$age_min) + exp(-3.27 + 0.0524*pop$age_max))/2

# Calculate IFR:
pop$year <- pop$`Reference date (as of 1 July)` 
pop$area <- pop$`Region, subregion, country or area *`

pop$area_pop <- ave(pop$value, pop$area, FUN = function(x) sum(x, na.rm = T))
pop$area_ifr <- ave(pop$value*pop$ifr, pop$area, FUN = function(x) sum(x, na.rm = T))/pop$area_pop
pop$area_ifr_alt <- ave(pop$value*pop$ifr_alt, pop$area, FUN = function(x) sum(x, na.rm = T))/pop$area_pop
summary(pop$area_ifr_alt)

pop$area_mean_age_approx <- ave(pop$value*pop$age_mean, pop$area, FUN = function(x) sum(x, na.rm = T))/pop$area_pop


# Inspect:
# View(unique(pop[, c("area", "area_ifr", "area_mean_age_approx")]))

ifr_dat <- unique(pop[pop$Type == "Country/Area", c("area", "area_ifr")])
ifr_dat <- rbind(ifr_dat[ifr_dat$area == "Denmark", ], ifr_dat)
ifr_dat$area[1] <- "Greenland"
ifr_dat$iso2c <- countrycode(ifr_dat$area, "country.name", "iso2c")

# Plot:
write.csv(ifr_dat, "ifr_by_iso2c.csv")

ifr_dat <- read.csv("ifr_by_iso2c.csv")
ifr_dat$iso2c[ifr_dat$area == "Namibia"] <- "NA"
library(countrycode)
library(ggplot2)
world <- data.table(map_data('world'))
world <- world[world$region!='Antarctica',]
world$iso2c <- countrycode(world$region, "country.name", "iso2c")

world <- merge(world, ifr_dat[!is.na(ifr_dat$iso2c), ], by="iso2c", all.x = T)

ggplot(world, aes(long, lat)) + 
  geom_polygon(aes(group=group,fill=area_ifr),colour="black",size=0.1) +
  coord_equal() + 
  scale_x_continuous(expand=c(0,0)) + 
  scale_y_continuous(expand=c(0,0)) +
  labs(x='Longitude', y='Latitude') +
  scale_fill_continuous(high = "royalblue", low = "white")+theme_minimal()+ggtitle("Expected infection fatality rate, adjusting for population age distribution*, %")+
  xlab("Source: The Economist, Brazeau et al (2020), UN \n----- *not adjusting for treatment options and underlying conditions.")
