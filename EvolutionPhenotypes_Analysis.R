################################################################################
### Evolution Phenotypes --> Analysis
################################################################################

### load packages

library(tidyverse); library(cowplot)

### load data

data <- read.csv('EvoPhenotypeData_All.csv')

### add columns for temperature and treatment

data$Temperature <- sapply(strsplit(data$evol_pops, split = "_"), "[[", 1)

data$Treatment <- sapply(strsplit(data$evol_pops, split = "_"), "[[", 2)

data$Replicate <- sapply(strsplit(data$evol_pops, split = "_"), "[[", 3)

data$Temperature <- as.numeric(paste(data$Temperature))

str(data)

unique(data$date)

summ_data <- data %>% group_by(date, Temperature, Treatment) %>% summarise_all(.funs = mean)

### now we can look at how each of the populations changed over time 

length_all <- ggplot(data = data, aes(x = as.Date(date), y = mean_major, color = as.factor(Temperature), shape = Treatment, group = evol_pops)) + 
  geom_point(size = 3) + geom_line() + scale_color_discrete(type = c('20' = 'blue', '26' = 'orange'), name = 'Temperature') + 
  xlab('Date') + ylab('Length') + theme_cowplot()

length_means <- ggplot(data = summ_data, aes(x = as.Date(date), y = mean_major, color = as.factor(Temperature), shape = Treatment)) + 
  geom_point(size = 4) + geom_line(linewidth = 1) + scale_color_discrete(type = c('20' = 'blue', '26' = 'orange'), name = 'Temperature') + 
  xlab('Date') + ylab('Length') + theme_cowplot()

length_plots <- plot_grid(length_all, length_means, nrow = 2, ncol = 1)

save_plot(filename = 'length_plots.png', plot = length_plots, ncol = 1, nrow = 2, bg = 'white')

speed_all <- ggplot(data = data, aes(x = as.Date(date), y = gross_speed, color = as.factor(Temperature), shape = Treatment, group = evol_pops)) + 
  geom_point(size = 3) + geom_line() + scale_color_discrete(type = c('20' = 'blue', '26' = 'orange'), name = 'Temperature') + 
  xlab('Date') + ylab('Speed') + theme_cowplot() 

speed_means <- ggplot(data = summ_data, aes(x = as.Date(date), y = gross_speed, color = as.factor(Temperature), shape = Treatment)) + 
  geom_point(size = 4) + geom_line(linewidth = 1) + scale_color_discrete(type = c('20' = 'blue', '26' = 'orange'), name = 'Temperature') + 
  xlab('Date') + ylab('Speed') + theme_cowplot() 

speed_plots <- plot_grid(speed_all, speed_means, nrow = 2, ncol = 1)

save_plot(filename = 'speed_plots.png', plot = speed_plots, ncol = 1, nrow = 2, bg = 'white')










