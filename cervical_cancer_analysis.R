# Project for R Course
# Cervical cancer impact factors

library(dplyr)
library(ggplot2)
library(gridExtra)
library(RColorBrewer)


density_data_prep <- function(raw_df, dens_col) {
  new_col_name <- tolower(paste0(substr(dens_col, start = 1, stop = 3), 
                                 '_time'))
  new_df <- select(raw_df,
                   !!new_col_name := dens_col,
                   cancer_diagnosis)
  
  new_df <- new_df |>
    na.omit() |>
    mutate_at(vars(new_col_name), as.numeric) |>
    mutate_at(vars(cancer_diagnosis), as.factor)
  return(new_df)
}


cancer_prob_calc <- function(raw_df, std_num) {
  ifelse(std_num < 3,
         std_df <- select(filter(raw_data, STDs..number. == std_num),
                          cancer_diagnosis),
         
         std_df <- select(filter(raw_data, STDs..number. >= std_num),
                          cancer_diagnosis))
  
  prob <- sum(std_df$cancer_diagnosis) / length(std_df$cancer_diagnosis) * 100
  return(round(prob))
}


#### Common Data Manipulation ####

# Reading and storing raw csv data
raw_data <- read.csv('data/kag_risk_factors_cervical_cancer.csv')

# Get summary statistics of raw_data
summary(raw_data)

# Replacing all the '?' values for NA
raw_data[raw_data == '?'] <- NA

# Make every value in raw_data numeric
raw_data <- mutate_all(raw_data, function(x) as.numeric(as.character(x)))

# Checking the first 10 entries of the raw_data dataframe
head(raw_data, 10)

# To know the names of the columns in raw_data
names(raw_data)

# To know the datatypes of the values in the columns of raw_data
sapply(raw_data, class)


# Check for NA values in the cancer diagnosis columns before joining
unique(is.na(c(raw_data$Hinselmann, 
               raw_data$Schiller, 
               raw_data$Citology, 
               raw_data$Biopsy)))

# Creating a new column in which '1' represents a cancer diagnosis
# independent of the test (Hinselmann, Schiller, Citology, or Biopsy) used and
# '0' represents no diagnosis.
raw_data <- mutate(raw_data, 
                    cancer_diagnosis = ifelse(raw_data$Hinselmann == 0 & 
                                                raw_data$Schiller == 0 & 
                                                raw_data$Citology == 0 & 
                                                raw_data$Biopsy == 0,
                                              0, 1))


#### Figure Contraceptives ####
# Relationship between different contraceptives and cervical cancer.

hor_cancer <- density_data_prep(raw_df = raw_data, 
                                dens_col = 'Hormonal.Contraceptives..years.')

iud_cancer <- density_data_prep(raw_df = raw_data, 
                                dens_col = 'IUD..years.')

hor_contr_plot <- ggplot(hor_cancer) +
  geom_density(aes(x = hor_time, fill = cancer_diagnosis), 
               bw = 2, alpha = 0.5) + 
  labs(title='Horm. Contr. as a Risk Factor of Cervical Cancer',
       y='Intensity',
       x='Use of Hormonal Contraceptives (years)',
       fill='Test Results') +
  theme_classic() + 
  theme(axis.text.x = element_text(size = 8, hjust = 0.5),
        axis.title.x = element_text(size = 10),
        axis.text.y = element_text(size = 8, hjust = 0.5),
        axis.title.y = element_text(size = 10),
        legend.title = element_text(size = 9),
        plot.title = element_text(size = 14, hjust = 0.5)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 35)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.2)) +
  scale_fill_manual(labels = c('Negative', 'Positive'), 
                    values = brewer.pal(3, 'Set2'))

iud_contr_plot <- ggplot(iud_cancer) +
  geom_density(aes(x = iud_time, fill = cancer_diagnosis), 
               bw = 2, alpha = 0.5) + 
  labs(title='IUD as a Risk Factor of Cervical Cancer',
       y='Intensity',
       x='Use of IUD (years)',
       fill='Test Results') +
  theme_classic() + 
  theme(axis.text.x = element_text(size = 8, hjust = 0.5),
        axis.title.x = element_text(size = 10),
        axis.text.y = element_text(size = 8, hjust = 0.5),
        axis.title.y = element_text(size = 10),
        legend.title = element_text(size = 9),
        plot.title = element_text(size = 14, , hjust = 0.5)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 35)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.2)) +
  scale_fill_manual(labels = c('Negative', 'Positive'), 
                    values = brewer.pal(3, 'Set1'))

contr_fig = grid.arrange(hor_contr_plot, iud_contr_plot, ncol = 2, nrow = 1)

ggsave('img/contr_cancer.png', contr_fig, width = 10, height = 5, dpi = 300)


#### Figure STDs ####
# Relationship between number of STDs and cervical cancer.

three_std <- cancer_prob_calc(raw_df = raw_data, std_num = 3)
two_std <- cancer_prob_calc(raw_df = raw_data, std_num = 2)
one_std <- cancer_prob_calc(raw_df = raw_data, std_num = 1)
zero_std <- cancer_prob_calc(raw_df = raw_data, std_num = 0)


std_cancer_df <- data.frame(std_num = c('0', 
                                        '1', 
                                        '2', 
                                        '3'),
                            cancer_prob = c(zero_std, 
                                            one_std, 
                                            two_std, 
                                            three_std))

std_plot <- ggplot(data=std_cancer_df, aes(x=std_num,
                                           y=cancer_prob,
                                           fill=std_num)) +
    geom_bar(stat="identity") + 
    scale_fill_brewer(palette = "Set2", name = 'Cases of STDs') + 
    labs(title='IUD as a Risk Factor of Cervical Cancer',
         y='Study Subjects Diagnosed with Cervical Cancer (%)',
         x='Number of Cases of STDs') +
    theme_classic() + 
    theme(axis.text.x = element_text(size = 7, hjust = 0.5),
          axis.title.x = element_text(size = 9),
          axis.text.y = element_text(size = 7, hjust = 0.5),
          axis.title.y = element_text(size = 9),
          legend.title = element_text(size = 9),
          plot.title = element_text(size = 14, hjust = 0.5))

ggsave('img/std_cancer.png', std_plot, width = 10, height = 5, dpi = 300)
