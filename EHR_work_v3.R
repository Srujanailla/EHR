
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
library(reshape2)
library(gridExtra)

data = read.csv('C:/Users/hp/Box/Estonia EHR/claims data prediction/data/Constructed/Diagnosis_utilization_data.csv')
data$Gender[data$Gender=='N'] ='Female'
data$Gender[data$Gender=='M'] ='Male'


# 2009 ------------------------------------



data2009_pop = data %>% filter(treatment_year==2009) %>%select(birth_year, ecm_patient_id) %>% group_by(birth_year) %>% count()
data2009_pop$Age = 2009 - data2009_pop$birth_year
data2009_pop$Age_mod = if_else(data2009_pop$Age >= 90,90,data2009_pop$Age)
data2009_pop$Age_bins <- cut(data2009_pop$Age_mod, breaks=c(17,18,20,54,69, 89, 110)
                                           , labels=c('18',"19-20","21-54","55-69", "70-89", '90+'))
data2009_pop$birth_year_mod = if_else(data2009_pop$birth_year < 1919,1919,as.double(data2009_pop$birth_year))
data2009_pop = data2009_pop %>% select(birth_year_mod, Age_bins, n) %>% group_by(birth_year_mod, Age_bins) %>% summarise(pop = sum(n))

data2009_pop$axis2 = ''
data2009_pop$axis1 = ''
data2009_pop$axis2[data2009_pop$birth_year_mod == 1990] ='under 20'
data2009_pop$axis2[data2009_pop$birth_year_mod == 1970] ='21-54'
data2009_pop$axis2[data2009_pop$birth_year_mod == 1945] ='55-69'
data2009_pop$axis2[data2009_pop$birth_year_mod == 1930] ='70-89'
data2009_pop$axis2[data2009_pop$birth_year_mod == 1919] ='90+'

data2009_pop$axis1[data2009_pop$birth_year_mod==1991] ='91'
data2009_pop$axis1[data2009_pop$birth_year_mod==1989] ='89'
data2009_pop$axis1[data2009_pop$birth_year_mod==1940] ='1940'
data2009_pop$axis1[data2009_pop$birth_year_mod==1955] ='1955'
data2009_pop$axis1[data2009_pop$birth_year_mod==1919] ='1919'

#2014------------------------------------


data2014_pop = data %>% filter(treatment_year==2014) %>%select(birth_year, ecm_patient_id) %>% group_by(birth_year) %>% count()
data2014_pop$Age = 2014 - data2014_pop$birth_year
data2014_pop$Age_mod = if_else(data2014_pop$Age >= 90,90,data2014_pop$Age)
data2014_pop$birth_year_mod = if_else(data2014_pop$birth_year < 1924,1924,as.double(data2014_pop$birth_year))
data2014_pop$Age_bins <- cut(data2014_pop$Age_mod, breaks=c(17,28,30,64,79, 89, 90)
                             , labels=c('18-23',"23-25","26-60","61-74", "74-89", '90+'))
data2014_pop = data2014_pop %>% select(birth_year_mod, Age_bins, n) %>% group_by(birth_year_mod, Age_bins) %>% summarise(pop = sum(n))


data2014_pop = merge(data2014_pop, data2009_pop, by='birth_year_mod',all.x=TRUE, all.y=FALSE)
data2014_pop = data %>% filter(treatment_year==2014) %>%select(birth_year, ecm_patient_id) %>% group_by(birth_year) %>% count()
data2014_pop$Age = 2014 - data2014_pop$birth_year
data2014_pop$Age_mod = if_else(data2014_pop$Age >= 90,90,data2014_pop$Age)
data2014_pop$birth_year_mod = if_else(data2014_pop$birth_year < 1924,1924,as.double(data2014_pop$birth_year))
data2014_pop$Age_bins <- cut(data2014_pop$Age_mod, breaks=c(17,23,25,59,74, 89, 120)
                                           , labels=c('18-23',"23-25","26-60","61-74", "74-89", '90+'))
data2014_pop = data2014_pop %>% select(birth_year_mod, Age_bins, n) %>% group_by(birth_year_mod, Age_bins) %>% summarise(pop = sum(n))
data2014_pop = merge(data2014_pop, data2009_pop, by='birth_year_mod',all.x=TRUE, all.y=FALSE)
data2014_pop = subset(data2014_pop, select=-c(Age_bins.y))
colnames(data2014_pop) = c("birth_year_mod","Age_bins",'n', "n_2009","axis2","axis1")

data2014_pop$n_2009[data2014_pop$birth_year_mod==1924] =0

data %>% filter(treatment_year==2014) %>% summarise(median(Age))

ggplot(data2014_pop)+
  geom_col(aes(x=birth_year_mod, y=n_2009,color='grey'),fill='grey',stat = 'identity',position = position_dodge(),width = 0.5)+
  scale_color_manual(name="",breaks=c("2009-2014 deaths"), values = c("2009-2014 deaths"='grey'))+
  theme_bw()+
  theme(legend.position = c(0.9,0.5))+
  geom_bar(aes(x=birth_year_mod, y=n,fill=Age_bins),stat = 'identity',position = position_dodge(), show.legend = FALSE, width = 0.5)+
  #theme_bw()+
  labs(x="Birth year", y="patient population", title = "Population in 2014 by birth year")+
  scale_x_reverse(limits=c(1997,1923), expand=c(0,0),breaks=c(1996,1991,1989, 1955,1940,1924))+
  annotate(geom = "text", x = 1993, y = 20000, colour = "black", size = 3.5, label='Independence\n(1991)')+
  annotate(geom = "text", x = 1989, y = 26000, colour = "black", size = 3.5, label='Fall of\nBerlin Wall\n(1989)')+
  annotate(geom = "text", x = 1971, y = 20000, colour = "black", size = 3.5, label='Warsaw Pact (1955)')+
  annotate(geom = "text", x = 1945, y = 20000, colour = "black", size = 3.5, label='Russian Invasion(1940)')+
  annotate(geom = "text", x = 1959, y = 29500, colour = "black", size = 4.5, label='Median age: 48 years')+
  annotate(geom = "text", x = 1953.5, y = 28000, colour = "black", size = 4.5, label='Population: 1.1 Million (excluding under 18)')+
  annotate(geom = "text", x = 1989, y = 22500, colour = "black", size = 3.5, label='FR: 2.1')+
  annotate(geom = "text", x = 1996, y = 15000, colour = "black", size = 3.5, label='FR:\n1.37')+
  annotate(geom = 'text', x=1929, y=30000, colour='black', size=3.5, label='FR: Fertility rate\n(births per woman)')+
  #annotate(geom = "text", x = 2008, y = 17000, colour = "black", size = 3.5, label='FR:1.6')+
  scale_fill_discrete(name = "Age")+
  #scale_fill_manual(values=c('87CE9F','4AC071','23C659', '0CBF47','03A539'))+
  ylim(c(0,30000))+
geom_vline(xintercept = 1966)

ggsave("P1.2_Population in 2014 by birth year.jpg", plot = last_plot(), device = "jpeg", path = NULL, scale = 1, dpi = 300, limitsize = TRUE)






#2019------------------------------------


data2019_pop = data %>% filter(treatment_year==2019) %>%select(birth_year, ecm_patient_id) %>% group_by(birth_year) %>% count()
data2019_pop$Age = 2019 - data2019_pop$birth_year
data2019_pop$Age_mod = if_else(data2019_pop$Age >= 90,90,data2019_pop$Age)
data2019_pop$birth_year_mod = if_else(data2019_pop$birth_year < 1929,1929,as.double(data2019_pop$birth_year))
data2019_pop = data2019_pop %>% select(birth_year_mod, Age_bins, n) %>% group_by(birth_year_mod, Age_bins) %>% summarise(pop = sum(n))
data2019_pop$axis2 = ''
data2019_pop$axis1 = ''

data2019_pop = merge(data2019_pop, data2009_pop, by='birth_year_mod',all.x=TRUE, all.y=FALSE)
#data2019_pop = data %>% filter(treatment_year==2019) %>%select(birth_year, ecm_patient_id) %>% group_by(birth_year) %>% count()
data2019_pop$Age = 2019 - data2019_pop$birth_year
data2019_pop$Age_mod = if_else(data2019_pop$Age >= 90,90,data2019_pop$Age)
data2019_pop$birth_year_mod = if_else(data2019_pop$birth_year < 1929,1929,as.double(data2019_pop$birth_year))
data2019_pop$Age_bins <- cut(data2019_pop$Age_mod, breaks=c(17,28,30,64,79, 89, 90)
                             , labels=c('18-23',"23-25","26-60","61-74", "74-89", '90+'))
data2019_pop = data2019_pop %>% select(birth_year_mod, Age_bins, n) %>% group_by(birth_year_mod, Age_bins) %>% summarise(pop = sum(n))
data2019_pop = merge(data2019_pop, data2009_pop, by='birth_year_mod',all.x=TRUE, all.y=FALSE)
data2019_pop = subset(data2019_pop, select=-c(Age_bins.y))
colnames(data2019_pop) = c("birth_year_mod","Age_bins",'n', "n_2009","axis2","axis1")


data %>% filter(treatment_year==2019) %>% summarise(median(Age))
data %>% filter(treatment_year==2019) %>% count()

data2019_pop$n_2009[data2019_pop$birth_year_mod==1929] =0

ggplot(data2019_pop)+
  geom_col(aes(x=birth_year_mod, y=n_2009,color='grey'),fill='grey',stat = 'identity',position = position_dodge(),width = 0.5)+
  scale_color_manual(name="",breaks=c("2009-2019 deaths"), values = c("2009-2019 deaths"='grey'))+
  theme_bw()+
  theme(legend.position = c(0.9,0.6))+
  geom_bar(aes(x=birth_year_mod, y=n,fill=Age_bins),stat = 'identity',position = position_dodge(), show.legend = FALSE, width = 0.5)+
  #theme_bw()+
  labs(x="Birth year", y="patient population", title = "Population in 2019 by birth year")+
  scale_x_reverse(limits=c(2001,1927), expand=c(0,0),breaks=c(2001,1991,1989, 1955,1940,1929))+
  annotate(geom = "text", x = 1996, y = 20000, colour = "black", size = 3.5, label='Independence\n(1991)')+
  annotate(geom = "text", x = 1989, y = 26000, colour = "black", size = 3.5, label='Fall of\nBerlin Wall\n(1989)')+
  annotate(geom = "text", x = 1971, y = 20000, colour = "black", size = 3.5, label='Warsaw Pact (1955)')+
  annotate(geom = "text", x = 1945, y = 20000, colour = "black", size = 3.5, label='Russian Invasion(1940)')+
  annotate(geom = "text", x = 1963, y = 29500, colour = "black", size = 4.5, label='Median age: 49 years')+
  annotate(geom = "text", x = 1957.5, y = 28000, colour = "black", size = 4.5, label='Population: 1.09 Million (excluding under 18)')+
  annotate(geom = "text", x = 1989, y = 22500, colour = "black", size = 3.5, label='FR: 2.1')+
  annotate(geom = "text", x = 1996, y = 15000, colour = "black", size = 3.5, label='FR:\n1.37')+
  annotate(geom = 'text', x=1934, y=30000, colour='black', size=3.5, label='FR: Fertility rate\n(births per woman)')+
  #annotate(geom = "text", x = 2008, y = 17000, colour = "black", size = 3.5, label='FR:1.6')+
  scale_fill_discrete(name = "Age")+
  #scale_fill_manual(values=c('87CE9F','4AC071','23C659', '0CBF47','03A539'))+
  ylim(c(0,30000))+
  geom_vline(xintercept = 1970)

ggsave("P1.3_Population in 2019 by birth year.jpg", plot = last_plot(), device = "jpeg", path = NULL, scale = 1, dpi = 300, limitsize = TRUE)




##-------------------Mortality Rate----------------------------------------------------------------------------


data$died_ty = if_else(data$treatment_year==data$death_year,1,0)
data$Age_bins <- cut(data$Age, breaks=c(17,30,40,50,60,70,80,90,110)
                             , labels=c('18-29','30-39','40-49', '50-59','60-69','70-79','80-89','90+'))


# Total year pop
pop_yr = data %>% select(treatment_year, ecm_patient_id) %>% group_by(treatment_year) %>% count() 
colnames(pop_yr) = c('treatment_year', 'total_pop')

#Gender pop by Age bins
pop_AGY = data %>% filter(Age !=9999) %>% select(treatment_year, Age_bins,ecm_patient_id,Gender) %>% group_by(treatment_year,Age_bins, Gender) %>% count()
colnames(pop_AGY) = c("treatment_year",'Age_bins' ,'gender', 'gender_pop')

pop_AGY = merge(pop_AGY, pop_yr, by='treatment_year', all.x = TRUE, all.y = TRUE)
pop_AGY$pop_perc = round(pop_AGY$gender_pop*100/pop_AGY$total_pop,1)
pop_AGY$pop_perc <- paste0(pop_AGY$pop_perc, "%")
colnames(pop_AGY) = c("treatment_year","Age_bins","Sex","gender_pop","total_pop","pop_perc")

#Median age of Male and Female pop in 2009 
Gender_median_age_yr = data %>% filter(Age !=9999) %>% select(treatment_year,Age,Gender) %>% group_by(treatment_year,Gender) %>% summarise(median_age=median(Age)) 

ggplot(pop_AGY[pop_AGY$treatment_year==2009,], aes(x=Age_bins, y=gender_pop, fill=gender))+
  geom_bar(stat='identity', position = 'dodge')+
  theme_bw()+
  theme(legend.position=c(0.8,0.7))+
  scale_y_continuous(breaks =seq(0,160000,25000))+
  labs(x="Age categories", y='Population', title = "Population by gender and age groups (2009)")+
  geom_text(aes(label=pop_perc),hjust =0.5,vjust=2, size=3,position = position_dodge(width= 1))+
  annotate(geom='text', x='80-89', y=125000, label="Total Population: 1.1 Million\nMale:45% (Median Age - 43 yrs)\nFemale:55% (Median Age - 49 yrs)")

ggsave("P2.1_Population by gender and age groups(2009).jpg", plot = last_plot(), device = "jpeg", path = NULL, scale = 1, dpi = 300, limitsize = TRUE)


ggplot(pop_AGY[pop_AGY$treatment_year==2019,], aes(x=Age_bins, y=gender_pop, fill=Sex))+
  geom_bar(stat='identity', position = 'dodge')+
  theme_bw()+
  theme(legend.position=c(0.8,0.7))+
  scale_y_continuous(breaks =seq(0,160000,25000))+
  labs(x="Age categories", y='Population', title = "Population by gender and age groups (2019)")+
  geom_text(aes(label=pop_perc),hjust =0.5,vjust=2, size=3,position = position_dodge(width= 1))+
  annotate(geom='text', x='80-89', y=100000, label="Total Population: 1.09 Million\nMale:46% (Median Age - 46 yrs)\nFemale:54% (Median Age - 52 yrs)")

ggsave("P2.2_Population by gender and age groups(2019).jpg", plot = last_plot(), device = "jpeg", path = NULL, scale = 1, dpi = 300, limitsize = TRUE)



#--------------------------------------------------------------------------------------------------------------

#population by year and gender
pop_yr_gender = data %>% select(treatment_year, ecm_patient_id, Gender) %>% group_by(treatment_year,Gender) %>% count()
colnames(pop_yr_gender) = c('treatment_year','gender','pop_gender')

pop_yr_gender = merge(pop_yr, pop_yr_gender, by="treatment_year", all.x = TRUE, all.y = TRUE)
pop_yr_gender$pop_gender = pop_yr_gender$pop_gender/1000
pop_yr_gender$gender_perc = round(pop_yr_gender$pop_gender*100000/pop_yr_gender$total_pop, )
pop_yr_gender$gender_perc<- paste0(pop_yr_gender$gender_perc, "%")

#deaths by year and gender
deaths_yr_gender = data %>% select(treatment_year, died_ty, Gender) %>%  group_by(treatment_year,Gender) %>% summarise(deaths=sum(died_ty))
colnames(deaths_yr_gender) = c('treatment_year','gender','deaths')

pop_ygd = merge(pop_yr_gender,deaths_yr_gender, by=c("treatment_year", 'gender'), all.x = TRUE, all.y = TRUE)
pop_ygd$MR_100K = pop_ygd$deaths*100/pop_ygd$pop_gender
pop_ygd$MR_100K_label = round(pop_ygd$MR_100K,)

colnames(pop_ygd) = c("treatment_year","Sex","total_pop","pop_gender", "gender_perc","deaths","MR_100K","MR_100K_label" )


ggplot(pop_ygd, aes(x=treatment_year, y=MR_100K, fill=Sex))+
  geom_line(aes(color=Sex), lwd=1.5)+
  geom_point()+
  ylim(c(0,1500))+
  theme_bw()+
  theme(legend.position = c(0.9,0.7))+
  scale_x_continuous(breaks = seq(2009,2019,1))+
  labs(x="Years", y="Mortality rate per 100K population", title='Crude Mortality Rate by Gender')+
  geom_text(label=pop_ygd$MR_100K_label, vjust=-0.75 ,size=3, check_overlap = TRUE)

ggsave("P2.3_Crude Mortality Rate by Gender.jpg", plot = last_plot(), device = "jpeg", path = NULL, scale = 1, dpi = 300, limitsize = TRUE)















#P3.3---------------------------------------------------------------------------------------------------------------------

#no of patients without county information - 44,453
length(unique(data$ecm_patient_id[data$ResidenceNameLocalGovLevel=='0']))
Counties = read_excel('C:/Users/hp/Desktop/DSPP - GU/Estonia_HealthCare_ImpactEvaluation/Code/estonia-ehr/EHR_data_checks/Estonias_counties.xlsx')

dim(data) #12163045

data = merge(data, Counties, by="ResidenceNameLocalGovLevel", all.x = TRUE,all.y = TRUE)
dim(data) #12163045 - Merged right

write.csv(data,'C:/Users/hp/Box/Estonia EHR/claims data prediction/data/Constructed/Diagnosis_utilization_data.csv')


phc_U = data %>% select(treatment_year,Gender,Age_bins,phcvisits) %>% group_by(treatment_year,Gender,Age_bins) %>% summarise(mean(phcvisits))
phc_U = phc_U[!is.na(phc_U$Age_bins),]
colnames(phc_U) = c("treatment_year","Gender","Age_bins","mean_phcvisits")

phc_U$label = round(phc_U$mean_phcvisits,1)
phc_U$Age_bins = as.factor(phc_U$Age_bins)


#P3.4 Average number of phcvisits by year-------------------------------------------------------------------------------

phc_year = data %>% select(treatment_year, Age_bins, phcvisits) %>% group_by(treatment_year, Age_bins) %>% summarise(mean_phcvisits=mean(phcvisits))
phc_year = phc_year[!is.na(phc_year$Age_bins),]
phc_year$label = round(phc_year$mean_phcvisits,1)

ggplot(phc_year, aes(x=treatment_year,y=mean_phcvisits, color=Age_bins), show.legend = FALSE)+
  geom_line(lwd=1, show.legend = FALSE)+
  geom_point(show.legend = FALSE)+
  theme_bw()+
  #theme(legend.title= element_text("Age bins"))+
  scale_x_continuous(breaks=seq(2009, 2020, 1))+
  ylim(c(0,7.5))+
  labs(x='year',y='PHC visits per capita', title='PHC visits per capita by Age group')+
  geom_text(aes(label=label),nudge_y = 0.3, check_overlap = TRUE, show.legend=FALSE)+
  annotate(geom='text', x= 2019.5, y=5.2, label="80-89 yrs", size=3.5)+
  annotate(geom='text', x= 2019.5, y=4.9, label="90+ yrs", size=3.5)+
  annotate(geom='text', x= 2019.5, y=4.5, label="70-79 yrs", size=3.5)+
  annotate(geom='text', x= 2019.5, y=3.6, label="60-69 yrs", size=3.5)+
  annotate(geom='text', x= 2019.5, y=2.7, label="50-59 yrs", size=3.5)+
  annotate(geom='text', x= 2019.5, y=2, label="40-49 yrs", size=3.5)+
  annotate(geom='text', x= 2019.5, y=1.6, label="30-39 yrs", size=3.5)+
  annotate(geom='text', x= 2019.5, y=1.3, label="18-29 yrs", size=3.5)

ggsave("P3.1 visits per capita over the years by age.jpg", plot = last_plot(), device = "jpeg", path = NULL, scale = 1, dpi = 300, limitsize = TRUE)






#average number of phc visits by locale---------------------------------------------------------------------------------
phc_locale = data %>% select(treatment_year,County,phcvisits) %>% group_by(treatment_year,County) %>% summarise(visits = mean(phcvisits))
phc_locale$phc_label = round(phc_locale$visits,1)
phc_locale= phc_locale[!is.na(phc_locale$County),]


ggplot(phc_locale[phc_locale$treatment_year==2019,],aes(x=County, y=visits))+
  geom_bar(stat = "identity",fill="darkgreen")+
  theme_bw()+
  labs(x="County", y="Average PHC visits", title='PHC visits per capita by County (2019)')+
  geom_text(aes(label=phc_label),hjust =0.5,vjust=1, size=3.5,color="white")
  #theme(axis.text.x = element_text(angle = 45, hjust=1))


ggsave("P3.2_PHC visist by County 2019.jpg", plot = last_plot(), device = "jpeg", path = NULL, scale = 1, dpi = 300, limitsize = TRUE)



#Hospitalizations by County-----------------------------------------------------------------------------------------

hosp_locale = data %>% select(treatment_year,County,total_hospitalizations) %>% group_by(treatment_year,County) %>% summarise(hosp_per1000 = mean(total_hospitalizations))
hosp_locale$hosp_per1000 = 1000*hosp_locale$hosp_per1000
hosp_locale$hosp_label = round(hosp_locale$hosp_per1000,1)
hosp_locale= hosp_locale[!is.na(hosp_locale$County),]


ggplot(hosp_locale[hosp_locale$treatment_year==2019,],aes(x=County, y=hosp_per1000))+
  geom_bar(stat = "identity",fill="darkgreen")+
  theme_bw()+
  labs(x="County", y="Average PHC visits", title='Hospitalizations per 1000 residents by County (2019)')+
  geom_text(aes(label=hosp_label),hjust =0.5,vjust=1, size=3.5,color="white")
#theme(axis.text.x = element_text(angle = 45, hjust=1))


county_util = merge(hosp_locale,phc_locale, by=c('County','treatment_year'), all.x=TRUE, all.y = TRUE)
#county_util = county_util %>% select(County,treatment_year,hosp_per1000,visits)
#county_util = reshape2::melt(county_util, id.vars = c("County", "treatment_year"))

county_pop = data %>% select(treatment_year, County,ecm_patient_id) %>% group_by(treatment_year, County) %>% count()
colnames(county_pop) = c('treatment_year','County', 'pop') 
county_pop = county_pop[!is.na(county_pop$County),]

county_util = merge(county_util,county_pop, by=c('County','treatment_year'), all.x=TRUE, all.y = TRUE)

ggplot(county_util[county_util$treatment_year==2019,],aes(x=hosp_per1000, y=visits, color=County))+
  geom_point(aes(size=pop), alpha=0.6, show.legend = FALSE)+
  scale_size_continuous(range=c(7,25))+
  scale_x_continuous(breaks = seq(80,180,10))+
  scale_y_continuous(breaks = seq(0,4,0.25))+
  labs(x='Hospitalizations per 1000 population', y='Per capita PHC visits', title = 'Utilization of services by County: PHC visits Vs Hospitalizations')+
  geom_text(aes(label=County),color='black', check_overlap = TRUE, hjust=0.5)+
  annotate(geom='text',x=149,y=2.9,label='Voru')+
  annotate(geom='text',x=125,y=3.1,label='Laane-Viru')+
  #xlim(c(0,200))+
  #ylim(c(0,4))+
  theme_bw()
  
ggsave("P3.3_Utilization of services by County(2019).jpg", plot = last_plot(), device = "jpeg", path = NULL, scale = 1, dpi = 300, limitsize = TRUE)




#Counties median age and SD ------------------------------------

Age_dist_labels = data %>% filter(treatment_year==2019 & Age!=9999 & Age<90) %>% select(County,Age) %>% group_by(County) %>% summarise(quantile(Age,c(.25, .50, .75)))
percentitle = rep(c('25th','50th', '75th'),16)
Age_dist_labels$percentile=percentitle
colnames(Age_dist_labels) = c('County','Age_value','percentile')
fill=c('cyan','deepskyblue2','deepskyblue4','deepskyblue2','deepskyblue2','deepskyblue2','deepskyblue2','deepskyblue2',
       'deepskyblue2','deepskyblue2','deepskyblue2','cyan','deepskyblue2','deepskyblue2','deepskyblue2')
quantile(data$Age, c(.25, .50, .75)) 

pop_locale = data %>% filter(treatment_year==2019 & Age<90) %>% select(County,ecm_patient_id) %>% group_by(County) %>% count()
pop_locale = pop_locale[!is.na(pop_locale$County),]
pop_locale$pop_perc = round(pop_locale$n*100/1050280,1)
colnames(pop_locale) = c('County', 'count', '% of population')

pop_locale_plot = pop_locale %>% select(County, `% of population`)
pop_locale_plot =  arrange(pop_locale_plot, desc(`% of population`))
pop_locale_plot$`% of population` = paste0(pop_locale_plot$`% of population`,"%")

ggplot( data[!is.na(data$County) & data$Age !=9999 & data$Age<90,], aes(x=County, y=Age)) +
  geom_boxplot(fill=fill)+
  ylim(c(10,150))+
  scale_y_continuous(c(10,150), breaks=seq(10,150,10), expand=c(0.8,0))+
  scale_x_discrete(expand = c(0.1, 0))+
  geom_text(data=Age_dist_labels[Age_dist_labels$percentile=='50th',], aes(x=County, y=Age_value, label=Age_value), hjust=-0.5, size=3.5, color='black')+
  labs(x="County", y="Age", title = 'Population Age Distribution by County (2019)')+
  theme_bw()+
  coord_flip()+
  annotation_custom(tableGrob(pop_locale_plot, rows = NULL), ymin=100,ymax=149, xmin = "Harju", xmax="Voru")
  #scale_y_continuous(breaks =seq(0,121,10))
ggsave("P2.5_Population Age Distribution by County (2019).jpg", plot = last_plot(), device = "jpeg", path = NULL, scale = 1, dpi = 300, limitsize = TRUE)



#---Crude Mortality Rate by County-------------------------------------------------------------------------------------

County_MR = data %>% select(treatment_year, County,died_ty,ecm_patient_id) %>% group_by(treatment_year, County) %>% summarise(death_counts = sum(died_ty), pop=length(ecm_patient_id))

County_MR$MR_100K = County_MR$death_counts*100000/County_MR$pop
County_MR$MR_100K_label = round(County_MR$MR_100K,) 
County_MR = County_MR[!is.na(County_MR$County),]

ggplot(County_MR[(County_MR$treatment_year==2009) |(County_MR$treatment_year==2019),], aes(x=County, y=MR_100K, fill=as.factor(treatment_year)))+
  geom_bar(stat='identity',position = "dodge",width=0.6)+
  theme_bw()+
  scale_fill_discrete(name = "Year")+
  labs(x='County', y='Mortality Rate per 100K population', title='Crude Mortality Rate by County (2009 vs 2019)')+
  geom_text(aes(label=MR_100K_label),hjust =0.5,vjust=2, size=3,position = position_dodge(width= 1))

ggsave("P2.6_Crude Mortality Rate by County(2009 vs 2019).jpg", plot = last_plot(), device = "jpeg", path = NULL, scale = 1, dpi = 300, limitsize = TRUE)


