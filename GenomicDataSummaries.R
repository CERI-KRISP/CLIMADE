library(ggplot2)
library(readxl)
library(rworldmap)
library(cowplot)

#Functions to annotate location info
world_data<-getMap(resolution='low')@data

country2continent = function(x)
{  
  country_data = subset(world_data, SUBUNIT==x)
  
  return (as.character(country_data[['REGION']]))   # returns the continent (7 continent model)
}


country2continent_region = function(x)
{  
  country_data = subset(world_data, SUBUNIT==x)
  
  return (as.character(country_data[['IMAGE24']]))  
}


country2lat = function(x)
{  
  country_data = subset(world_data, SUBUNIT==x)
  
  return (as.numeric(country_data[['LAT']]))  
}


country2long = function(x)
{  
  country_data = subset(world_data, SUBUNIT==x)
  
  return (as.numeric(country_data[['LON']]))  
}

chik_genome_length<-12000 #approximately
dengue_genome_length<-11000 #approximately
zika_genome_length<-11000 #approximately
rvf_genome_length<-12000 #approximately
yellowfever_genome_length<-10000 #approximately

genome<-rvf_genome_length

#Read whichever virus genomic data by just navigating the file location below
chik_data<-read_excel("VIPR_chikungunya/148117604240-Results.xls")
dengue_data<-read_excel("VIPR_dengue/959033147672-Results.xls")
zika_data<-read_excel("VIPR_zika/28966063777-Results.xls")
riftvalley_data<-read_excel("VIPR_riftvalleyfever/40460646054-Results.xls")
yellowfever_data<-read_excel("VIPR_yellowfever/704972492015-Results.xls")

genomic_data<-riftvalley_data
genomic_data$seq_length<-as.numeric(genomic_data$`Sequence Length`)
genomic_data$seq_coverage<-as.numeric(genomic_data$seq_length)/genome
genomic_data$cov_categories <- cut(genomic_data$seq_coverage, breaks = c(-0.5,0,0.5, 0.9, Inf),
                  labels = c(">90%","0-50%", "50-90%", ">90%"),
                  right = FALSE)

table(genomic_data$cov_categories)

genomic_data$days<-as.Date(cut(genomic_data$`Collection Date`,breaks = "day",start.on.monday = FALSE))
genomic_data$date<-as.Date(cut(genomic_data$days,breaks = "week",start.on.monday = FALSE))
genomic_data$date2<-as.Date(cut(genomic_data$days,breaks = "2 week",start.on.monday = FALSE))
genomic_data$date4<-as.Date(cut(genomic_data$days,breaks = "4 week",start.on.monday = FALSE))
genomic_data$date5<-as.Date(cut(genomic_data$days,breaks = "year",start.on.monday = FALSE))


genomic_data$Country[genomic_data$Country == "Virgin Islands"] <- "United States Virgin Islands"
genomic_data$Country[genomic_data$Country == "Viet Nam"] <- "Vietnam"
genomic_data$Country[genomic_data$Country == "USA"] <- "United States of America"
genomic_data$Country[genomic_data$Country == "Micronesia"] <- "Federated States of Micronesia"
genomic_data$Country[genomic_data$Country == "Mayotte"] <- "French Southern and Antarctic Lands"
genomic_data$Country[genomic_data$Country == "Martinique"] <- "French Southern and Antarctic Lands"
genomic_data$Country[genomic_data$Country == "Guadeloupe"] <- "French Southern and Antarctic Lands"
genomic_data$Country[genomic_data$Country == "Borneo"] <- "Brunei"
genomic_data$Country[genomic_data$Country == "Reunion"] <- "French Southern and Antarctic Lands"
genomic_data$Country[genomic_data$Country == "Tokelau"] <- "New Zealand"
genomic_data$Country[genomic_data$Country == "Timor-Leste"] <- "East Timor"
genomic_data$Country[genomic_data$Country == "Cote dIvoire"] <- "Ivory Coast"

genomic_data$Country[genomic_data$Country == "Hong Kong"] <- "Hong Kong S.A.R."
genomic_data$Country[genomic_data$Country == "Bahamas"] <- "The Bahamas"
genomic_data$Country[genomic_data$Country == "Saint Martin"] <- "Sint Maarten"
genomic_data$Country[genomic_data$Country == "-N/A-"] <- NA

genomic_data$Continent<-lapply(genomic_data$Country,country2continent)
genomic_data$Continent[genomic_data$Continent == 'character(0)'] <- NA

genomic_data$Continent_Region<-lapply(genomic_data$Country,country2continent_region)
genomic_data$Continent_Region[genomic_data$Continent_Region == 'character(0)'] <- NA

genomic_data$`Subtype/Genotype (ViPR)`[genomic_data$`Subtype/Genotype (ViPR)` == '-N/A-'] <- NA



global_distribution<-ggplot(genomic_data,mapping= aes(x=date5,fill=unlist(Continent)))+
  theme_bw()+
  geom_bar(stat='count',colour='black',size=0.2)+
  xlab('Collection Date')+
  ylab('Count')+
  scale_fill_brewer(palette = 'RdBu',name='Location')+
  theme(legend.position = 'bottom')+
  ggtitle('Global genomic sequencing')
global_distribution


africa_distribution<-ggplot(subset(genomic_data,Continent=='Africa'),mapping= aes(x=date5,fill=unlist(Continent_Region)))+
  theme_bw()+
  geom_bar(stat='count',colour='black',size=0.2)+
  xlab('Collection Date')+
  ylab('Count')+
  scale_fill_brewer(palette = 'Spectral',name='Region')+
  theme(legend.position = 'bottom')+
  ggtitle('Genomic sequencing in Africa')
africa_distribution



africa_genotypes<-ggplot(subset(genomic_data,Continent=='Africa'),mapping= aes(x=date5,fill=`Subtype/Genotype (ViPR)`))+
  theme_bw()+
  geom_bar(stat='count',colour='black',size=0.2)+
  xlab('Collection Date')+
  ylab('Count')+
  scale_fill_brewer(palette = 'Dark2',name='Genotype')+
  theme(legend.position = 'bottom')+
  ggtitle('Genotypes in Africa')
africa_genotypes



africa_denvtypes<-ggplot(subset(genomic_data,Continent=='Africa'),mapping= aes(x=date5,fill=`Virus Type`))+
  theme_bw()+
  geom_bar(stat='count',colour='black',size=0.2)+
  xlab('Collection Date')+
  ylab('Count')+
  scale_fill_brewer(palette = 'Paired',name='Genotype')+
  theme(legend.position = 'bottom')+
  ggtitle('Dengue Types in Africa')
africa_denvtypes


africa_genotypes2<-ggplot(subset(genomic_data,Continent=='Africa'),mapping= aes(x=date5,fill=`Subtype/Genotype (ViPR)`))+
  theme_bw()+
  geom_bar(stat='count',position='fill',colour='black',size=0.2)+
  xlab('Collection Date')+
  ylab('Count')+
  scale_fill_brewer(palette = 'Dark2',name='Genotype')+
  theme(legend.position = 'bottom')+
  ggtitle('Genotypes in Africa')
africa_genotypes2


africa_denvtypes2<-ggplot(subset(genomic_data,Continent=='Africa'),mapping= aes(x=date5,fill=`Virus Type`))+
  theme_bw()+
  geom_bar(stat='count',colour='black',size=0.2,position='fill')+
  xlab('Collection Date')+
  ylab('Count')+
  scale_fill_brewer(palette = 'Paired',name='Genotype')+
  theme(legend.position = 'bottom')+
  ggtitle('Dengue Types in Africa')
africa_denvtypes2


plot_grid(global_distribution,africa_distribution,africa_denvtypes,africa_denvtypes2,africa_genotypes,africa_genotypes2,ncol=2)
plot_grid(global_distribution,africa_distribution,africa_genotypes,africa_genotypes2,ncol=2)

#ggsave("RVF_summary_figures.pdf", width = 30, height = 20, units = "cm")

genomic_data$Continent<-unlist(genomic_data$Continent)
genomic_data$Continent_Region<-unlist(genomic_data$Continent_Region)
genomic_data_africa<-subset(genomic_data,Continent=='Africa')
genomic_data_above_50<-subset(genomic_data,seq_coverage>=0.5)
genomic_data_above_50_africa<-subset(genomic_data_above_50,Continent=='Africa')

write.csv(x=genomic_data_africa,file='rvf_Africa_genomes_all.csv')
write.csv(x=genomic_data_above_50,file='rvf_global_genomes_above50.csv')
