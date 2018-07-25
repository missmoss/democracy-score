library("tidyverse")
library("readxl")

# Load excel data

p4v2017_all <- read_excel("p4v2017.xls")
fiw1973_2018_all <- read_excel("Country and Territory Ratings and Statuses FIW1973-2018.xlsx", 
                           sheet = "Country Ratings, Statuses ", 
                           col_names = FALSE, skip = 1)

# change year description to exact year

fiw1973_2018_all[1, "X__29"] <- "1981"
fiw1973_2018_all[1, "X__32"] <- "1982"
fiw1973_2018_all[1, "X__35"] <- "1983"
fiw1973_2018_all[1, "X__38"] <- "1984"
fiw1973_2018_all[1, "X__41"] <- "1985"
fiw1973_2018_all[1, "X__44"] <- "1986"
fiw1973_2018_all[1, "X__47"] <- "1987"
fiw1973_2018_all[1, "X__50"] <- "1989"

# fill NAs with correct years in FIW data

year_fiw <- as.character(fiw1973_2018_all[1,])
yearIdx <- !is.na(year_fiw)
yearVals <- c(NA, year_fiw[yearIdx])
yearFillIdx <- cumsum(yearIdx)+1
new_year_fiw <- yearVals[yearFillIdx]
cat <- c("PR", "CL", "status")
new_names_fiw <- c("country", paste(new_year_fiw[2:length(new_year_fiw)], cat, sep="_"))

# convert FIW to tidy format

names(fiw1973_2018_all) <- new_names_fiw
fiw1973_2018_all <- fiw1973_2018_all[3:nrow(fiw1973_2018_all), ]
fiw1973_2018_tidy <- fiw1973_2018_all %>%
  gather(year_cat, score, -country) %>%
  separate(year_cat, c("year", "cat")) %>%
  spread(cat, score)

# export tidy data to csv
write.csv(p4v2017_all, "p4v2017.csv")
write.csv(fiw1973_2018_tidy, "fiw2973_2018.csv")

# the list of the third waves of democratization

wave3 <- c("Portugal", 
           "Spain",
           "Greece",
           "Brazil",
           "Argentina",
           "Chile",
           "South Korea", #fiw
           "Korea", #p4v
           "Taiwan",
           "Poland",
           "Hungary",
           "Czech Republic",
           "Slovakia", #fiw
           "Slovak Republic", #p4v
           "Bulgaria",
           "Albania",
           "Romania",
           "Mongolia")

# get wave3 data

fiw1973_2018_wave3 <- filter(fiw1973_2018_tidy, country %in% wave3) %>%
  mutate(CL = as.numeric(CL), PR = as.numeric(PR), year = as.numeric(year))
p4v2017_wave3 <- filter(p4v2017_all, country %in% wave3)

# use the sames names in p4v & fiw data

p4v2017_wave3$country <- replace(as.character(p4v2017_wave3$country)
                                 , p4v2017_wave3$country == "Korea"
                                 , "South Korea")
p4v2017_wave3$country <- replace(as.character(p4v2017_wave3$country)
                                 , p4v2017_wave3$country == "Slovak Republic"
                                 , "Slovakia")

# combine 2 data into 1 dataframe
# which I didn't use in the following graphs XD

join_wave3 <- merge(x = fiw1973_2018_wave3, y = p4v2017_wave3[p4v2017_wave3$year > 1970, ], by = c("year", "country"), all = TRUE)
join_wave3_2017 <- join_wave3 %>%
  filter(year == 2017) %>%
  mutate(freedom = PR + CL)

# get Taiwan's data

fiw_tw <- filter(fiw1973_2018_wave3, country == "Taiwan")
p4v_tw <- filter(p4v2017_wave3, country == "Taiwan")

# Draw!
# p4v: Polity Score

g <- ggplot() +
  geom_line(data = p4v2017_wave3[p4v2017_wave3$year > 1970,], 
            aes(year, polity2, colour = country),
            alpha = 0.3, size = 2) +
  geom_line(data = p4v_tw[p4v_tw$year > 1970,], 
            aes(year, polity2, colour = country), size = 2) +  
  ggtitle("Polity Score") +
  ylab("Polity Score") +
  theme(plot.title = element_text(size = rel(3)))
g

# fiw: Political Rights

g <- ggplot() +
  geom_line(data = fiw1973_2018_wave3[fiw1973_2018_wave3$year > 1970,], 
            aes(year, PR, colour = country, group = country),
            alpha = 0.3, size = 2) +
  geom_line(data = fiw_tw[fiw_tw$year > 1970,], 
            aes(year, PR, colour = country, group = country), size = 2) +
  ggtitle("Political Rights") +
  ylab("<- [ 1 = Best ] _____ Political Rights _____ [ 7 = Worse ]->") +
  theme(plot.title = element_text(size = rel(3)))
g

# fiw: Civil Liberty

g <- ggplot() +
  geom_line(data = fiw1973_2018_wave3[fiw1973_2018_wave3$year > 1970,], 
            aes(year, CL, colour = country, group = country),
            alpha = 0.3, size = 2) +
  geom_line(data = fiw_tw[fiw_tw$year > 1970,], 
            aes(year, CL, colour = country, group = country), size = 2) +
  ggtitle("Civil Liberty") +
  ylab("<- [ 1 = Best ] _____ Civil Liberty _____ [ 7 = Worse ]->") +
  theme(plot.title = element_text(size = rel(3)))
g