# Load packages
library(here)
library(tidyverse)
library(viridis)
# Import data
here::here()
df <- read_delim('data.csv',delim = ';')
glimpse(df)
# Transform dataframe
df2 <- df %>%
  select(3:12) %>%
  gather(question,score,facialaesthetics:satisfaction)
df2 <- df2 %>%
  group_by(question,score) %>%
  summarize(n=n())
# Plot
  # Labels
labels <-
  c('Pain/benefit','Occlusion','Breath','Cost/Benefit of surgery',
    'Dental Aesthetics','Facial Aesthetics','Smell perception',
    'General Satisfaction','Speak','Swallow')
# Heatmap
ggplot(df2,aes(x=score,y=question,fill=n))+
  geom_tile()+
  scale_x_continuous(name = 'Score', position = 'top',
                     expand = c(0,0))+
  scale_y_discrete(name = NULL, labels = labels,
                   expand = c(0,0))+
  scale_fill_viridis(direction=-1)+
  theme_classic()+
  labs(fill='Frequency')
ggsave('heatmap.pdf', width = 5, height = 4, units= 'in', device = 'pdf')
# Some descriptive statistics
  # Plot complications 1 week
df3 <- df %>%
  select(13:14) %>%
  mutate(complications1 = recode(complications1, '0' = 'None','1' = 'Nausea','2' = 'Swelling','3' = 'Dysphagia','4' = 'Numbness', '5' = 'Drowsiness','6' = 'Pain','7' = 'Fatigue')) %>%
  mutate(complications1 = factor(complications1,levels = c('Nausea','Drowsiness','Numbness','Fatigue','Dysphagia','Pain','Swelling','None')))
ggplot(df3,aes(x=complications1))+
  geom_bar()+
  scale_y_continuous(name='Frequency',
                     breaks = seq(0,10,2), expand = c(0,0))+
  scale_x_discrete(name=NULL)+
  coord_flip()+
  theme_classic()
ggsave('complications.pdf', width = 5, height = 4, units = 'in', device = 'pdf')
# Tables
df <- df %>%
  mutate(gender = recode(gender,'1' = 'Male', '2' = 'Female'))
recovery <- table(df$recovery,df$gender)
wait <- table(df$wait,df$gender)
