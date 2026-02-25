#load packages 
library(dplyr)

#load file 
df <- read.csv(file.choose())

#Check column names 
names(df)

#Calculate fetal growth trajectory
df <- df %>%
  mutate(
    GA_EFW = as.numeric(GA.EFW),
    GA_birth = as.numeric(GA.birth),
    EFW_SD = as.numeric(EFW.SD),
    BW_SD = as.numeric(BW.SD),
    
    # change in SD score
    delta_SD = BW_SD - EFW_SD,
    
    # time between measurements
    weeks_between = GA_birth - GA_EFW,
    
    # growth trajectory (SD change per week)
    slope_SD_per_week = delta_SD / weeks_between
  )

#Check results 
summary(df$slope_SD_per_week)
head(df)

#Create scatter plot 
library(ggplot2)

ggplot(df, aes(x = EFW_SD, y = BW_SD)) +
  geom_point(alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", colour = "red") +
  labs(
    title = "Change in Growth SD from EFW to Birth",
    x = "EFW SD",
    y = "Birthweight SD"
  ) +
  theme_minimal()t

#Plot individual trajectories 
#First reshape data 

plot_df <- bind_rows(
  df %>% transmute(Study.id, GA = GA_EFW,   SD = EFW_SD, point = "EFW"),
  df %>% transmute(Study.id, GA = GA_birth, SD = BW_SD,  point = "Birth")
) %>%
  arrange(Study.id, GA)

# Create plot 

ggplot(plot_df, aes(x = GA, y = SD, group = Study.id)) +
  geom_line(alpha = 0.3) +
  geom_point(aes(shape = point), size = 2) +
  scale_shape_manual(values = c(EFW = 16, Birth = 17)) +
  labs(
    title = "Individual Growth Trajectories (SD score) from EFW to Birth",
    x = "Gestational age (weeks)",
    y = "Weight SD score",
    shape = "Timepoint"
  ) +
  theme_minimal()