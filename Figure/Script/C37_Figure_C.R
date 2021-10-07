# Figure C

rm(list = ls())
# install.packages("pacman")
library(pacman)
p_load("tidyverse", "plotly", "readxl", "compareGroups", "ggpubr", 
       "reshape2","inegiR", "coronavirus", "lubridate", "haven")

### Base de datos de gonomas

arg <- read_excel("argentina_julio.xlsx") 
chi <- read_excel("chile_julio.xlsx") 
per <- read_excel("peru_julio.xlsx")


arg$date <- as.Date(arg$date, format = "%Y-%m-%d")
chi$date <- as.Date(chi$date, format = "%Y-%m-%d")
per$date <- as.Date(per$date, format = "%Y/%m/%d")

#arg_n <- arg %>% filter(date >= "2020-11-01" & date <= "2021-06-07")
#chi_n <- chi %>% filter(date >= "2020-11-01" & date <= "2021-06-07")
#per_n <- per %>% filter(date >= "2020-11-01" & date <= "2021-06-07")


### Base de datos de muertes

peru_df<- refresh_coronavirus_jhu() %>% 
  filter(location == "Peru") %>% filter(data_type == "deaths_new")

peru_df <- peru_df %>%
  mutate(week1 = as.Date(cut(date,breaks = "1 week",start.on.monday = FALSE)))

peru_df1 <- peru_df %>% 
  group_by(week1) %>%
  dplyr::summarise(dead = sum(value)/7) %>%
  ungroup()

rm(peru_df)

chile_df <- refresh_coronavirus_jhu() %>% 
  filter(location == "Chile") %>% filter(data_type == "deaths_new")

chile_df <- chile_df %>%
  mutate(week1 = as.Date(cut(date,breaks = "1 week",start.on.monday = FALSE)))

chile_df1 <- chile_df %>% 
  group_by(week1) %>%
  dplyr::summarise(dead = sum(value)/7) %>%
  ungroup()

rm(chile_df)

argentina_df1 <- read.table("argentina_df.tab", sep = "\t", header = T)

argentina_df1$week1 <- as.Date(argentina_df1$week1, format = "%Y-%m-%d")

### Colores

lin_col <- c("zOther" = "gray89","abP.1 (Gamma)" = "red3", "aaC.37 (Lambda)" = "#45994A", 
             "B.1" = "salmon1","B.1.1.1" = "royalblue2", "B.1.1" =  "deeppink2", 
             "B.1.1.348" = "tan3", "aB.1.1.7 (Alpha)" = "darkorchid", "C.11" = "gold1",
             "B.1.621" = "blue4")

lin_col <- c("aaC.37 (Lambda)" = "#45994A",  "aB.1.1.7 (Alpha)" = "darkorchid", 
             "abP.1 (Gamma)" = "red3", "abP.2(Zeta)" = "royalblue2",  "B.1.617.2 (Delta)" = "deeppink2", 
             "B.1.427/429 (Epsilon)" = "salmon1", "zOther" = "gray71")
## Graficos 2 ###

# Argentinca

arg_plt <- ggplot(arg, aes(x = date,  after_stat(count), colour = Lineage)) + 
  geom_density(aes(fill = Lineage, group=Lineage), 
               alpha = 0.4,size=0.1, 
               adjust=0.6, 
               bw = 5,
               position= position_fill(reverse = T)) +  #position= "fill"
  geom_line(data = argentina_df1, aes(x = week1, 
                                      y = fallecido/700), 
            color = "black", size=0.8) + 
  scale_y_continuous(name = "Frequency",
                     breaks=seq(0, 1, 0.20), 
                     sec.axis = sec_axis(trans = ~.*700, name="Weekly Excess Deaths", breaks = seq(100, 700, by = 100)),
                     expand = c(0, 0)) +
  scale_x_date(breaks = "2 week", date_labels = "%d-%b", 
               limits = as.Date(c("2020-11-01","2021-07-05")),
               expand = c(0.0,0)) + 
  theme(axis.text.x = element_text(color="black", size=10,angle = 90)) +
  theme(axis.title.x = element_text(color="black", size=12, face="bold"))+
  theme(axis.title.y = element_text(color="black", size=12, face="bold"))+
  theme(axis.text.x = element_text(color="black", size=10))+
  theme(axis.text.y = element_text(color="black", size=10))+
  scale_fill_manual(values = lin_col) +
  scale_color_manual(values = lin_col) +
  ggtitle("Argentina") + xlab("Date") + 
  theme(legend.position='bottom')#+ theme_classic() 
arg_plt  

# Chile 

chi_plt <- ggplot(chi, aes(x = date,  after_stat(count), colour = Lineage)) + 
  geom_density(aes(fill = Lineage, group=Lineage), 
               alpha = 0.4,size=0.1, 
               adjust=0.6, 
               bw = 5,
               position= position_fill(reverse = T)) +  #position= "fill"
  geom_line(data = chile_df1, aes(x = week1, 
                                  y = dead/140), 
            color = "black", size=0.8) + 
  scale_y_continuous(name = "Frequency",
                     breaks=seq(0, 1, 0.20), 
                     sec.axis = sec_axis(trans = ~.*140, name="Weekly Excess Deaths", breaks = seq(20, 140, by = 20)),
                     expand = c(0, 0),
                     limits = c(0,1)) +
  scale_x_date(breaks = "2 week", date_labels = "%d-%b", 
               limits = as.Date(c("2020-11-01","2021-07-05")),
               expand = c(0,0)) +
  theme(axis.text.x = element_text(color="black", size=10,angle = 90)) +
  theme(axis.title.x = element_text(color="black", size=12, face="bold"))+
  theme(axis.title.y = element_text(color="black", size=12, face="bold"))+
  theme(axis.text.x = element_text(color="black", size=10))+
  theme(axis.text.y = element_text(color="black", size=10))+
  scale_fill_manual(values = lin_col) +
  scale_color_manual(values = lin_col) +
  ggtitle("Chile") + xlab("Date") +
  theme(legend.position='bottom')#+ theme_classic() 
chi_plt  
# Peru

per_plt <- ggplot(per, aes(x = date,  after_stat(count), colour = Lineage)) + 
  geom_density(aes(fill = Lineage, group=Lineage), 
               alpha = 0.4,size=0.1, 
               adjust=0.6, 
               bw = 5,
               position= position_fill(reverse = T)) +  #position= "fill"
  geom_line(data = peru_df1, aes(x = week1, 
                                  y = dead/900), 
            color = "black", size=0.8) + 
  scale_y_continuous(name = "Frequency",
                     breaks=seq(0, 1, 0.20), 
                     sec.axis = sec_axis(trans = ~.*900, name="Weekly Excess Deaths", breaks = seq(100, 900, by = 100)),
                     expand = c(0, 0),
                     limits = c(0,1)) +
  scale_x_date(breaks = "2 week", date_labels = "%d-%b", 
               limits = as.Date(c("2020-11-01","2021-07-05")),
               expand = c(0,0)) +
  theme(axis.text.x = element_text(color="black", size=10,angle = 90)) +
  theme(axis.title.x = element_text(color="black", size=12, face="bold"))+
  theme(axis.title.y = element_text(color="black", size=12, face="bold"))+
  theme(axis.text.x = element_text(color="black", size=10))+
  theme(axis.text.y = element_text(color="black", size=10))+
  scale_fill_manual(values = lin_col) +
  scale_color_manual(values = lin_col) +
  ggtitle("Peru") + xlab("Date") +
  theme(legend.position='bottom')#+ theme_classic() 

per_plt 


ggarrange(arg_plt, chi_plt, per_plt,
          ncol = 3, nrow = 1,  common.legend = T, legend = "bottom") %>%
  ggsave(file="c37_16.pdf",  width=23, height=8)

# This scrip was written by Diego Cuicapuza(diego.cuicapuza@upch.pe)