library(readr)
library(dplyr)

### For outbreaks and seroprevalence visualizations

isocodes <- read_delim("isocodes.csv", delim = ";", 
                       escape_double = FALSE, trim_ws = TRUE)

Outbreaks<-left_join(Outbreaks,isocodes) #outbreak.csv data - to read again if not in R project

Africa <- Outbreaks[Outbreaks$region == 'Africa', ]

Africa_arbo <- Africa[Africa$icd10n == "Arthropod-borne viral fevers and viral haemorrhagic fevers", ]
Africa_arbo <- Africa[Africa$icd11l2 == "Certain arthropod-borne viral fevers" | Africa$icd11l2 == "Dengue", ]

Africa_cholera<-Africa[Africa$icd103n == "Cholera", ]

#Africa_2022 <- Africa[Africa$Year == 2021, ]



diseases_progress<-ggplot()+
  theme_minimal()+
  geom_count(data=subset(Africa_arbo,!is.na(icd104n)),aes(as.numeric(Year),icd104n,colour=icd104n),alpha=0.8)+
  theme(plot.margin = margin(1,1,1,1, "cm"), plot.title = element_text(size=10), legend.position='bottom',legend.direction = 'vertical')+
  #geom_density(stat='count',data=subset(Africa_arbo,Year>1999),aes(as.numeric(Year),group=icd104n,colour=icd104n))+
  scale_colour_manual(values=c('#001219', '#005f73', '#0a9396', '#94d2bd', '#e9d8a6', '#ee9b00', '#ca6702', '#bb3e03', '#ae2012', '#9b2226'),name='Diseases')+
  scale_size_continuous(name='Count',breaks = c(1,3,5,5),
                        labels = c(1,3,5,5))+
  ylab("No. of Outbreaks")+
  xlab("Year")+
  ggtitle('Arbovirus Disease Otbreaks in Africa (By Diseases)')
diseases_progress


diseases_progress2<-ggplot()+
  theme_minimal()+
  geom_count(data=subset(Africa_arbo,!is.na(icd104n)),aes(as.numeric(Year),Country,colour=icd104n),size=4,alpha=0.8)+
  theme(plot.margin = margin(1,1,1,1, "cm"), plot.title = element_text(size=10), legend.position='none')+
  #geom_density(stat='count',data=subset(Africa_arbo,Year>1999),aes(as.numeric(Year),group=icd104n,colour=icd104n))+
  scale_colour_manual(values=c('#001219', '#005f73', '#0a9396', '#94d2bd', '#e9d8a6', '#ee9b00', '#ca6702', '#bb3e03', '#ae2012', '#9b2226'))+
  ylab("No. of Outbreaks")+
  xlab("Year")+
  ggtitle('Arbovirus Disease Otbreaks in Africa (By Country)')
diseases_progress2

plot_grid(diseases_progress,diseases_progress2,labels=c("A","B"),rel_widths = c(0.4,0.6))


chik_sero<-read_excel('../VIPR_chikungunya/pntd.0010069.s006.xlsx')

chik_sero_p<-ggplot(chik_sero,aes(`Date of sample collection`,as.numeric(`Seroprevalence (%)`),fill=Country), show.legend=FALSE)+
  theme_minimal()+
  geom_point(shape=21,aes(size=`Sample size`))+
  scale_fill_brewer(palette = 'Set3')+
  geom_spatial_text_repel(aes(label=Country))+
  ylab('Seroprevalence (%)')+
  ggtitle("Chikungunya Seroprevalence in Africa")
chik_sero_p
