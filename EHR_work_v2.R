library(knitr)
library(ggplot2)
library(tidyverse)
library(readxl) # Package to read Excel data
library(ggrepel)
library(reshape)
library(haven)
library(viridis)
library(hrbrthemes)
library(ggrepel)
library(scales)
library(gghighlight)
library(ggcorrplot)

data = read.csv('C:/Users/hp/Box/Estonia EHR/claims data prediction/data/Raw/total_patients_phc_nopii.csv')

#Summary table of pop with PHC claims by year
summ1 = data %>% select(treatment_year, ecm_patient_id, born_year, died_year)%>% group_by(treatment_year) %>%
  summarise(count_phc=length(ecm_patient_id), born=sum(born_year, na.rm = TRUE), died=sum(died_year,na.rm = TRUE)) %>%
  mutate(count_exclude_born_dead = count_phc-born- died )



## Mapped birth year--------------------------------------------------------------------------------
## Cleaning the birth field - map back birth years

birth_data <- data[!is.na(data$birth_year) & data$birth_year!=0,] #removing rows with NA and 0 in birth field
birth_data <- birth_data %>% select(ecm_patient_id, birth_year) %>% group_by(ecm_patient_id) %>% filter(row_number() == n())
data = merge(data, birth_data, by="ecm_patient_id", all.x = TRUE, all.y = FALSE) #Added birth_mod field
colnames(data) = c ("ecm_patient_id","X","treatment_year","death_year","birth_year","died_year","born_year","birth_year_mod" ) 
data$birth_year_mod[is.na(data$birth_year_mod)]<- 0



## Mapped death year --------------------------------------------------------------------------------
## Cleaning the death field - map back death years

death_data <- data[!is.na(data$death_year) & data$death_year!=0,] #removing rows with NA and 0 in birth field
death_data <- death_data %>% select(ecm_patient_id, death_year) %>% group_by(ecm_patient_id) %>% filter(row_number() == n())
data = merge(data, death_data, by="ecm_patient_id", all.x = TRUE, all.y = FALSE) #Added death_mod field
colnames(data) = c ("ecm_patient_id","X","treatment_year","death_year","birth_year","died_year","born_year","birth_year_mod",'death_year_mod' ) 
data$death_year_mod[is.na(data$death_year_mod)]<- 0









# Begin repeat ##--------------------------------------------------------------------------------------------------------------

#------------------------2009 population-------------------------------------------------------------------------------------
# All patients alive in 2009 = data -> (delete duplicates by ecm_patient_id, keep first) -> (delete patients whose birth year is > 2009)
#13 patients without birth year in 2009 - we delete them
data<- data[order(data$ecm_patient_id, data$treatment_year, decreasing=FALSE),]
data2009 = data[!duplicated(data$ecm_patient_id), ]
data2009 = data2009 %>% filter(birth_year_mod <= 2009 & (death_year_mod >=2009 | death_year_mod==0))
# 1358664 patients in 2009

data2009$Age = 2009 - data2009$birth_year_mod
data2009$Age_bins <- cut(data2009$Age, breaks=c(-1,10,20,30,40,50,60,70,80,90, 120)
                           , labels=c('under 10',"10-20","20-30", "30-40", '40-50', '50-60','60-70', '70-80','80-90','90+'))

popage2009 = data2009 %>% select(Age_bins, ecm_patient_id) %>% group_by(Age_bins) %>% count()
popage2009 =  popage2009[!is.na(popage2009$Age_bins),]
#we want both death count and death rate per 1000 residents


#Deaths by age in 2009-----------------------------------------------------------------------------------------------------
# 12269 died in 2009
death_2009 = data %>% select(treatment_year, died_year, birth_year_mod,death_year_mod, ecm_patient_id ) %>% filter(treatment_year==2009, death_year_mod==2009)
death_2009$Age = 2009 - death_2009$birth_year_mod
death_2009$Age_bins <- cut(death_2009$Age, breaks=c(-1,10,20,30,40,50,60,70,80,90, 120)
                             , labels=c('under 10',"10-20","20-30", "30-40", '40-50', '50-60','60-70', '70-80','80-90','90+'))

deathage2009 = death_2009 %>% select(Age_bins, died_year) %>% group_by(Age_bins) %>% summarise(sum(died_year))
deathage2009 =  deathage2009[!is.na(deathage2009$Age_bins),]


##Merge death and population data--------------------------------------------------------------------------------------------

deathage2009 = merge(deathage2009, popage2009, by='Age_bins', all.x = TRUE, all.y = TRUE)
colnames(deathage2009) = c('Age_bins', 'death_counts', 'pop')
deathage2009 = deathage2009 %>% mutate(mortality_1000_pop = death_counts*1000/pop)
deathage2009 = deathage2009 %>% mutate(legend_MR1000pop = round(death_counts*1000/pop))

total_deaths = sum(deathage2009$death_counts)
Pop_MR1000 = round(sum(deathage2009$death_counts)*1000/ sum(deathage2009$pop))

## Death Age histogram
ggplot(deathage2009, aes(x=Age_bins, y=mortality_1000_pop))+
  geom_bar(stat='identity', color='black', fill='#DE5A8A')+
  geom_text(aes(label=legend_MR1000pop), stat = 'identity', nudge_y = 5)+
  labs(x='Age categories', y='Mortality per 1000 residents', title = "Mortality rate per 1000 population in 2009")+
  annotate(geom = "text", x = "10-20", y = 200, colour = "black", size = 3.5, label='Population mortality rate: 9')+
  annotate(geom='text', x='10-20', y=190, colour = "black", size = 3.5, label='Total death count: 12266')
ggsave("PP1_Mortality rate per 1000 population in 2009.jpg", plot = last_plot(), device = "jpeg", path = NULL, scale = 1, dpi = 300, limitsize = TRUE)




ggplot(deathage2009, aes(x=Age_bins, y=death_counts))+
  geom_bar(stat='identity', color='black', fill='#DE5A8A')+
  geom_text(aes(label=death_counts), stat = 'identity', nudge_y = 80)+
  labs(x='Age categories', y='Death counts', title = "Age distribution of deaths in 2009")+
  annotate(geom = "text", x = "10-20", y = 3500, colour = "black", size = 3.5, label='Population mortality rate: 9')+
  annotate(geom='text', x='10-20', y=3400, colour = "black", size = 3.5, label='Total death count: 12266')
ggsave("PP2_Age distribution of deaths in 2009.jpg", plot = last_plot(), device = "jpeg", path = NULL, scale = 1, dpi = 300, limitsize = TRUE)



##--------------------------------------------------------------------------------------------------------------------------------



treatment_years = c(2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019)

for (yr in treatment_years){
  
  x = data[!duplicated(data$ecm_patient_id), ]
  x = x %>% filter(birth_year_mod <= deparse(yr))
  x$Age = yr - x$birth_year_mod
  x$Age_bins <- cut(x$Age, breaks=c(-1,10,20,30,40,50,60,70,80,90, 120)
                           , labels=c('under 10',"10-20","20-30", "30-40", '40-50', '50-60','60-70', '70-80','80-90','90+'))
  x = x %>% select(Age_bins, ecm_patient_id) %>% group_by(Age_bins) %>% count()
  x =  x[!is.na(x$Age_bins),]
  
  
  
  y = data %>% select(treatment_year, died_year, birth_year_mod,death_year_mod, ecm_patient_id ) %>% filter(treatment_year==2009, death_year_mod==2009)
  y$Age = yr - y$birth_year_mod
  y$Age_bins <- cut(y$Age, breaks=c(-1,10,20,30,40,50,60,70,80,90, 120)
                             , labels=c('under 10',"10-20","20-30", "30-40", '40-50', '50-60','60-70', '70-80','80-90','90+'))
  y = y %>% select(Age_bins, died_year) %>% group_by(Age_bins) %>% summarise(sum(died_year))
  y =  y[!is.na(y$Age_bins),]
  
  y = merge(y, x, by='Age_bins', all.x = TRUE, all.y = TRUE)
  colnames(y) = c('Age_bins', 'death_counts', 'pop')
  y = y %>% mutate(mortality_1000_pop = death_counts*1000/pop)
  y = y %>% mutate(legend_MR1000pop = round(death_counts*1000/pop))
  
  
  #assign(paste0('mortality',yr),y)
  
  total_deaths = sum(y$death_counts)
  Pop_MR1000 = round(sum(y$death_counts)*1000/ sum(y$pop))
  
  # Mortality Rate
  FIG1 = ggplot(y, aes(x=Age_bins, y=mortality_1000_pop))+
    geom_bar(stat='identity', color='black', fill='#DE5A8A')+
    geom_text(aes(label=legend_MR1000pop), stat = 'identity', nudge_y = 5)+
    labs(x='Age categories', y='Mortality per 1000 residents', title = paste0("Mortality rate per 1000 population in ",yr))
    #annotate(geom = "text", x = "10-20", y = 200, colour = "black", size = 3.5, label='Population mortality rate: 9')+
    #annotate(geom='text', x='10-20', y=190, colour = "black", size = 3.5, label='Total death count: 12266')
  print(FIG1)
  ggsave(FIG1, file=paste0("Output_MR/",yr,"Mortality rate.png"), device=png, width = 44.45, height = 27.78, units = "cm", dpi=300)
  
  
  
  ## Death counts
  #FIG2 = ggplot(y, aes(x=Age_bins, y=death_counts))+
  #  geom_bar(stat='identity', color='black', fill='#DE5A8A')+
  #  geom_text(aes(label=death_counts), stat = 'identity', nudge_y = 80)+
  # labs(x='Age categories', y='Death counts', title = paste0("Age distribution of deaths in ",yr))
    #annotate(geom = "text", x = "10-20", y = 3500, colour = "black", size = 3.5, label='Population mortality rate: 9')
    #annotate(geom='text', x='10-20', y=3400, colour = "black", size = 3.5, label='Total death count: 12266')
  #print(FIG2)
  #ggsave(FIG2, file=paste0(yr,".png"), device=png, width = 44.45, height = 27.78, units = "cm", dpi=300)
  #ggsave(FIG2, paste0("Output_DC/",yr,"Age distribution of deaths.png"), device=png, width = 44.45, height = 27.78, units = "cm", dpi=300)
  
  
}

















#Age as of 2009----------------------------------------------------------------------------------------------------------------
data2009_pop = data2009 %>% select(birth_year_mod, ecm_patient_id) %>% group_by(birth_year_mod) %>% count()
data2009_pop$Age = 2009 - data2009_pop$birth_year_mod
data2009_pop$Age_bins <- cut(data2009_pop$Age, breaks=c(-1,20,55,70, 90, 110)
                         , labels=c('under 20',"21-55","56-70", "70-90", '90+'))
data2009_pop$axis2 = ''
data2009_pop$axis1 = ''
data2009_pop %>% filter(birth_year_mod %in% c() )


data2009_pop[99,'axis2'] ='under 20' 
data2009_pop[69,'axis2'] ="21-55" 
data2009_pop[44,'axis2'] ="56-70"
data2009_pop[24,'axis2'] ="70-90"
data2009_pop[9,'axis2'] ='90+'


data2009_pop[108,'axis1'] ='2009'
data2009_pop[88,'axis1'] ='1989'
data2009_pop[53,'axis1'] ='1954'
data2009_pop[38,'axis1'] ='1939'
data2009_pop[18,'axis1'] ='1919'
data2009_pop[3,'axis1'] ='1904'

breaks.major = c(2000,1970,1945,1925,1910)
breaks.minor=c(2009,1989,1954,1939,1919,1904)
labels.minor = c('under 20',"21-55","56-70", "70-90", '90+')
lims=c(2009,1903)

ggplot(data2009_pop, aes(x=birth_year_mod, y=n),label=TRUE, group=Age_bins)+
  geom_bar(aes(fill=Age_bins),stat = 'identity',position = position_dodge(), show.legend = FALSE, width = 0.7)+
  theme_bw()+
  coord_cartesian(clip = "off")+
  scale_x_reverse(limits=c(2009,1903), expand=c(0,0),breaks=c(2009, 1989, 1954,1939,1919,1904))+
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),axis.title.x = element_blank(),axis.text.x = element_blank())+ 
  labs(x="Birth year", y="patient population", title = "Population in 2009 by birth year")+
  #scale_x_reverse(limits=c(2009,1903), expand=c(0,0),breaks=c(2009, 1989, 1954,1939,1919,1904))+
  #scale_x_reverse(expand=c(0,0), limit=lims, minor_breaks=breaks.minor, breaks=breaks.major,labels=labels.minor)+
  annotate(geom = "text", x = 2000, y = 20000, colour = "black", size = 3.5, label='Russian emigration\n after independence')+
  annotate(geom = "text", x = 1970, y = 21000, colour = "black", size = 3.5, label='Russian immigration')+
  annotate(geom = "text", x = 1945, y = 20000, colour = "black", size = 3.5, label='1939-1949\nMass Deportations\n(women and elderly)')+
  annotate(geom = "text", x = 1923, y = 12000, colour = "black", size = 3.5, label='Baltic Germans Emigration')+
  annotate(geom = "text", x = 1958, y = 29500, colour = "black", size = 4.5, label='Median age: 39 years')+
  annotate(geom = "text", x = 1957.5, y = 28000, colour = "black", size = 4.5, label='Population: 1.35 Million')+
  annotate(geom = "text", x = 1989, y = 22500, colour = "black", size = 3.5, label='FR: 2.1')+
  annotate(geom = "text", x = 1998, y = 13500, colour = "black", size = 3.5, label='FR:\n1.3')+
  annotate(geom = "text", x = 2007, y = 17000, colour = "black", size = 3.5, label='FR:1.6')+
  annotate(geom = "text", x = data2009_pop$birth_year_mod,y = 0,label = data2009_pop$axis2,vjust = 5)+
  annotate(geom = "text", x = 2009 ,y = 0,label = "Age", vjust=5, hjust=2)+
  annotate(geom = "text", x = 2009 ,y = 0,label = "Year", vjust=1.5,hjust=2)+
  annotate(geom='text', x=1910, y=29000, label="FR: Fertility Rate")+
  annotate(geom = "text",x = data2009_pop$birth_year_mod,y = 0,label = data2009_pop$axis1,vjust = 1.5, size=3.5)+
  scale_fill_discrete(name = "Age bins")+
  #scale_fill_manual(values=c('87CE9F','4AC071','23C659', '0CBF47','03A539'))+
  ylim(c(0,30000))+
  geom_vline(xintercept = 1970)
#facet_grid(.~group, scales = "free", switch = "x", space = "free_x")
#theme(strip.placement = "outside")

ggsave("P1_Population in 2009 by birth year.jpg", plot = last_plot(), device = "jpeg", path = NULL, scale = 1, dpi = 300, limitsize = TRUE)

  
  




#------------------------2014 population-------------------------------------------------------------------------------------


data2014 = data[!duplicated(data$ecm_patient_id), ]
data2014 = data2014 %>% filter(birth_year_mod <= 2014 & (death_year_mod >=2014 | death_year_mod ==0 ))
# 1362541 patients in 2014


#Age as of 2009
data2014_pop = data2014 %>% select(birth_year_mod, ecm_patient_id) %>% group_by(birth_year_mod) %>% count()
data2014_pop$Age = 2014 - data2014_pop$birth_year_mod


data2014_pop$Age_bins <- cut(data2014_pop$Age, breaks=c(-1,25,60,75, 90, 110)
                            , labels=c('under 25',"25-60","60-75", "75-90", '90+'))

data2014_pop = merge(data2014_pop, data2009_pop, by='birth_year_mod',all.x=TRUE, all.y=FALSE)
data2014_pop = subset(data2014_pop, select=-c(Age.y,Age_bins.y))
colnames(data2014_pop) = c("birth_year_mod", "n","Age","Age_bins", "n_2009")
data2014_pop = data2014_pop[!is.na(data2014_pop$Age_bins),]

#After 1990, Estonia lost about 15% of its population (230,000 people). 
#The population decreased to 1,294,455 by December 2011, a figure lower than that recorded in 1970
#Decreasing population pressures are explained by a higher death than birth rate and periods of an excess of emigrants over immigrants
#Since 2015 the country has experienced population growth.[13] 
#The population mainly increased as a result of net immigration of European Union citizens
#IMR, FR

median(data2014$birth_year_mod)
sum(data2014_pop$n)

ggplot(data2014_pop)+
  geom_col(aes(x=birth_year_mod, y=n_2009), color='grey', fill='grey',stat = 'identity',position = position_dodge(),width = 0.7)+
  geom_bar(aes(x=birth_year_mod, y=n,fill=Age_bins),stat = 'identity',position = position_dodge(), show.legend = FALSE, width = 0.7)+
  theme_bw()+
  labs(x="Birth year", y="patient population", title = "Population in 2014 by birth year")+
  scale_x_reverse(limits=c(2014,1903), expand=c(0,0),breaks=c(2014,2008,1998, 1989, 1954,1939,1919,1904))+
  annotate(geom = "text", x = 2000, y = 20000, colour = "black", size = 3.5, label='Russian emigration\n after independence')+
  annotate(geom = "text", x = 1970, y = 21000, colour = "black", size = 3.5, label='Russian immigration')+
  annotate(geom = "text", x = 1945, y = 20000, colour = "black", size = 3.5, label='1939-1949\nMass Deportations\n(women and elderly)')+
  annotate(geom = "text", x = 1923, y = 12000, colour = "black", size = 3.5, label='Baltic Germans Emigration')+
  annotate(geom = "text", x = 1958, y = 29500, colour = "black", size = 4.5, label='Median age: 41 years')+
  annotate(geom = "text", x = 1957.5, y = 28000, colour = "black", size = 4.5, label='Population: 1.36 Million')+
  annotate(geom = "text", x = 1989, y = 22500, colour = "black", size = 3.5, label='FR: 2.1')+
  annotate(geom = "text", x = 1998, y = 13500, colour = "black", size = 3.5, label='FR:\n1.3')+
  annotate(geom = "text", x = 2008, y = 17000, colour = "black", size = 3.5, label='FR:1.6')+
  scale_fill_discrete(name = "Age")+
  #scale_fill_manual(values=c('87CE9F','4AC071','23C659', '0CBF47','03A539'))+
  ylim(c(0,30000))+
  geom_vline(xintercept = 1973)

#1989 FR - 2.085, 1998 FR:1.331, 2008:1.662 2014:1.59, 2019:1.597
#facet_grid(.~group, scales = "free", switch = "x", space = "free_x")
#theme(strip.placement = "outside")

ggsave("P2_Population in 2014 by birth year.jpg", plot = last_plot(), device = "jpeg", path = NULL, scale = 1, dpi = 300, limitsize = TRUE)












#------------------------2014 population-------------------------------------------------------------------------------------


data2019 = data[!duplicated(data$ecm_patient_id), ]
data2019 = data2019 %>% filter(birth_year_mod <= 2019 & (death_year_mod >=2019 | death_year_mod ==0 ))
# 1353331 patients in 2019


#Age as of 2009
data2019_pop = data2019 %>% select(birth_year_mod, ecm_patient_id) %>% group_by(birth_year_mod) %>% count()
data2019_pop$Age = 2019 - data2019_pop$birth_year_mod


data2019_pop$Age_bins <- cut(data2019_pop$Age, breaks=c(-1,30,65,80, 90, 120)
                             , labels=c('under 30',"31-65","65-80", "80-90", '90+'))

data2019_pop = merge(data2019_pop, data2009_pop, by='birth_year_mod',all.x=TRUE, all.y=FALSE)
data2019_pop = subset(data2019_pop, select=-c(Age.y,Age_bins.y))
colnames(data2019_pop) = c("birth_year_mod", "n","Age","Age_bins", "n_2009")
data2019_pop = data2019_pop[!is.na(data2019_pop$Age_bins),]

#After 1990, Estonia lost about 15% of its population (230,000 people). 
#The population decreased to 1,294,455 by December 2011, a figure lower than that recorded in 1970
#Decreasing population pressures are explained by a higher death than birth rate and periods of an excess of emigrants over immigrants
#Since 2015 the country has experienced population growth.[13] 
#The population mainly increased as a result of net immigration of European Union citizens
#IMR, FR

median(data2019$birth_year_mod) #1977
sum(data2019_pop$n) #1.35 Million

ggplot(data2019_pop)+
  geom_col(aes(x=birth_year_mod, y=n_2009), color='grey', fill='grey',stat = 'identity',position = position_dodge(),width=0.7)+
  geom_bar(aes(x=birth_year_mod, y=n,fill=Age_bins),stat = 'identity',position = position_dodge(), show.legend = FALSE, width = 0.7)+
  theme_bw()+
  labs(x="Birth year", y="patient population", title = "Population in 2019 by birth year")+
  scale_x_reverse(limits=c(2019,1903), expand=c(0,0),breaks=c(2019,2014,2008,1998, 1989, 1954,1939,1919,1904))+
  annotate(geom = "text", x = 2000, y = 20000, colour = "black", size = 3.5, label='Russian emigration\n after independence')+
  annotate(geom = "text", x = 1970, y = 21000, colour = "black", size = 3.5, label='Russian immigration')+
  annotate(geom = "text", x = 1945, y = 20000, colour = "black", size = 3.5, label='1939-1949\nMass Deportations\n(women and elderly)')+
  annotate(geom = "text", x = 1923, y = 12000, colour = "black", size = 3.5, label='Baltic Germans Emigration')+
  annotate(geom = "text", x = 1961, y = 29500, colour = "black", size = 4.5, label='Median age: 42 years')+
  annotate(geom = "text", x = 1960.5, y = 28000, colour = "black", size = 4.5, label='Population: 1.35 Million')+
  annotate(geom = "text", x = 1989, y = 22500, colour = "black", size = 3.5, label='FR: 2.1')+
  annotate(geom = "text", x = 1998, y = 13500, colour = "black", size = 3.5, label='FR:\n1.3')+
  annotate(geom = "text", x = 2008, y = 17000, colour = "black", size = 3.5, label='FR:1.6')+
  scale_fill_discrete(name = "Age")+
  #scale_fill_manual(values=c('87CE9F','4AC071','23C659', '0CBF47','03A539'))+
  ylim(c(0,30000))+
  geom_vline(xintercept = 1977)

#1989 FR - 2.085, 1998 FR:1.331, 2008:1.662 2014:1.59, 2019:1.597
#facet_grid(.~group, scales = "free", switch = "x", space = "free_x")
#theme(strip.placement = "outside")

ggsave("P3_Population in 2019 by birth year.jpg", plot = last_plot(), device = "jpeg", path = NULL, scale = 1, dpi = 300, limitsize = TRUE)










#P4 Population by gender and age group---------------------------------------------------------------------------------------

data = read.csv("data_edited2.csv")
data["gender"][data["gender"] == "M"] <- "Male"
data["gender"][data["gender"] == "N"] <- "Female"
data['Age']= data$treatment_year - data$birth_mod

data$Age_bins<- cut(data$Age, breaks=c(17,30,40,50,60,70,80,90,116),
                    labels=c("18-30","30-40","40-50", "50-60","60-70", "70-80","80-90", "90+"))

P4data = data %>% filter(treatment_year==2009) %>% select(ecm_patient_id,Age_bins,gender) %>% group_by( gender,Age_bins) %>% count() %>% mutate(n_perc = round(n*100/768315))
P4data = P4data[!is.na(P4data$Age_bins),]
P4data$n_perc = as.character(P4data$n_perc)
P4data$n_perc<- paste0(P4data$n_perc, "%")
  
  
#Population break up - 60% female and 40% male
P4data %>% group_by(gender) %>% summarise(pop=sum(n)) %>% mutate(gender_perc = pop*100/768315 )

ggplot(P4data, aes(x=Age_bins, y=n, fill=gender))+
  geom_bar(stat="identity", position = "dodge", color="black" )+
  labs(x="Age categories", y="population", title = "Population by gender and age group (2009)")+
  theme_bw()+
  annotate(geom = 'text',x='80-90',y=80000,label='Total Population:\n60% Female, 40% Male')+
  geom_text(label=P4data$n_perc,hjust =0,vjust=2, size=3,position = position_dodge(width= 1))

ggsave("P4_Population by gender and age group.jpg", plot = last_plot(), device = "jpeg", path = NULL, scale = 1, dpi = 300, limitsize = TRUE)








#P5 Mortality by gender-----------------------------------------------------------------------------------------------------------

gender_map = data[data$gender!='',] %>% select(ecm_patient_id,gender) %>% group_by(ecm_patient_id) %>% filter(row_number() == n())
colnames(gender_map)= c('ecm_patient_id', 'gender_mod')

#mapped gender
data = merge(data,gender_map,by='ecm_patient_id', all.x = TRUE, all.y = FALSE)

P5data = data %>% filter(treatment_year==death_mod) %>% select(death_mod, ecm_patient_id, gender_mod) %>% group_by(death_mod, gender_mod)   %>% count()


dby = ggplot( P5data, aes(x=death_mod, y=n, color=as.factor(gender_mod)), show.legend=FALSE) +
  geom_line(lwd=1.2,show.legend=FALSE) +
  geom_point()+
  theme_bw() +
  labs(title = "Death counts by gender")+
  ylab("Death counts") +
  xlab("Year")+
  scale_fill_manual(values= c("#79b47c","#808080"))+
  scale_x_continuous(breaks=seq(2009, 2020, 1))+
  coord_cartesian(ylim = c(0, 13000))+
  scale_y_continuous(breaks=seq(0, 13000, 1000))+
  annotate(geom = 'text',x=2019.5,y=6000,label='Male')+
  annotate(geom = 'text',x=2019.5,y=4500,label='Female')+
  geom_text(label=P5data$n, nudge_y = -450, check_overlap=T, size=3)+
  theme(legend.position = "none") 
print(dby)

ggsave("P5_Death counts by gender.jpg", plot = last_plot(), device = "jpeg", path = NULL, scale = 1, dpi = 300, limitsize = TRUE)
















