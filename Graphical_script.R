pacman::p_load(tidyverse,haven,devtools,here,openxlsx,sjlabelled,stringi,stringr,janitor,lubridate,ggpattern,hrbrthemes,ggpubr)



plots_2023  <- readxl::read_xlsx(here("data/inventaire_placettes_2023_VF2.xlsx"))%>%
  mutate(ile=recode(ile,"mare"="Maré","lifou"="Lifou","ouvea"="Ouvéa"))

plots_2012 <- readxl::read_xlsx(here("data/inventaire_placettes_2012_selection_2023_fin.xlsx"))%>%
  mutate(ile=recode(ile,"mare"="Maré","lifou"="Lifou","ouvea"="Ouvéa"))

plots_2003 <- readxl::read_xlsx(here("data/inventaire_placettes_2003_selection_2023_fin.xlsx"))%>%
  mutate(ile=recode(ile,"mare"="Maré","lifou"="Lifou","ouvea"="Ouvéa"))

indiv_2023  <- readxl::read_xlsx(here("data/inventaire_santals_2023_VF2.xlsx"))%>%
  left_join(plots_2023%>%select(enq_id,ile,massif))%>%
  mutate(   classe_circo=cut(Circonf50cm,
                             include.lowest = TRUE,
                             right = T,
                             dig.lab = 4,
                             breaks = c(0, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85)
  ),
  classe_circo=as.numeric(as.factor(classe_circo))*5,
  classe_circo=ifelse( classe_circo==5,"From 0 to 10 cm",
                       paste("From",as.numeric(classe_circo),"to",as.numeric(classe_circo)+5,"cm")) ,
  
  
  classe_circo = fct_relevel(classe_circo,
                             "From 0 to 10 cm", "From 10 to 15 cm", "From 15 to 20 cm", "From 20 to 25 cm",
                             "From 25 to 30 cm", "From 30 to 35 cm", "From 35 to 40 cm", "From 40 to 45 cm",
                             "From 45 to 50 cm", "From 50 to 55 cm", "From 55 to 60 cm", "From 60 to 65 cm",
                             "From 65 to 70 cm", "From 70 to 75 cm", "From 75 to 80 cm", "From 80 to 85 cm"
  ))

nb_hectare_per_island <- readxl::read_xlsx("data/nb_hectare_par_ile.xlsx")%>%
  pivot_longer(2:4,names_to = "annee",values_to = "nb_hectares",names_transform = list(annee=as.numeric))

counting_sandalwood_2003 <- plots_2003%>%
  mutate(ile=recode(ile,"mare"="Maré","lifou"="Lifou","ouvea"="Ouvéa"))%>%
  pivot_longer(c(`0-10_viv`:`95-100_viv`,`0-10_mort`:`80-85_mort`),names_to = "classe_circo")%>%
  separate(classe_circo,sep = "_",into = c("classe_circo","type"))%>%
  mutate(classe_circo  = fct_recode( classe_circo,
                                     "From 0 to 10 cm"="0-10",
                                     "From 10 to 20 cm" = "10-15",
                                     "From 10 to 20 cm" = "15-20",
                                     "From 20 to 30 cm" = "20-25",
                                     "From 20 to 30 cm" = "25-30",
                                     "From 30 to 40 cm" = "30-35",
                                     "From 30 to 40 cm" = "35-40",
                                     "From 40 to 50 cm" = "40-45",
                                     "From 40 to 50 cm" = "45-50",
                                     "From 50 to 60 cm" = "50-55",
                                     "From 50 to 60 cm" = "55-60",
                                     "From 60 to 70 cm" = "60-65",
                                     "From 60 to 70 cm" = "65-70",
                                     "From 70 to 80 cm" = "70-75",
                                     "From 70 to 80 cm" = "75-80",
                                     "From 80 to 90 cm" = "80-85",
                                     "From 80 to 90 cm" = "85-90",
                                     "From 90 to 100 cm" = "90-95",
                                     "From 90 to 100 cm" = "95-100"))%>%
  filter(is.na(value)==F)%>%
  group_by(ile,classe_circo,type)%>%
  summarise(n=sum(value,na.rm = T))

counting_sandalwood_2012 <- plots_2012%>%
  mutate(ile=recode(ile,"mare"="Maré","lifou"="Lifou","ouvea"="Ouvéa"))%>%
  pivot_longer(c(`0-10_viv`:`95-100_viv`,`0-10_mort`:`80-85_mort`),names_to = "classe_circo")%>%
  separate(classe_circo,sep = "_",into = c("classe_circo","type"))%>%
  mutate(classe_circo  = fct_recode( classe_circo,
                                     "From 0 to 10 cm"="0-10",
                                     "From 10 to 20 cm" = "10-15",
                                     "From 10 to 20 cm" = "15-20",
                                     "From 20 to 30 cm" = "20-25",
                                     "From 20 to 30 cm" = "25-30",
                                     "From 30 to 40 cm" = "30-35",
                                     "From 30 to 40 cm" = "35-40",
                                     "From 40 to 50 cm" = "40-45",
                                     "From 40 to 50 cm" = "45-50",
                                     "From 50 to 60 cm" = "50-55",
                                     "From 50 to 60 cm" = "55-60",
                                     "From 60 to 70 cm" = "60-65",
                                     "From 60 to 70 cm" = "65-70",
                                     "From 70 to 80 cm" = "70-75",
                                     "From 70 to 80 cm" = "75-80",
                                     "From 80 to 90 cm" = "80-85",
                                     "From 80 to 90 cm" = "85-90",
                                     "From 90 to 100 cm" = "90-95",
                                     "From 90 to 100 cm" = "95-100"))%>%
  filter(is.na(value)==F)%>%
  group_by(ile,classe_circo,type)%>%
  summarise(n=sum(value,na.rm = T))

counting_sandalwood_2023 <- plots_2023%>%
  mutate(ile=recode(ile,"mare"="Maré","lifou"="Lifou","ouvea"="Ouvéa"))%>%
  pivot_longer(c(`0-10_viv`:`95-100_viv`,`0-10_mort`:`80-85_mort`),names_to = "classe_circo")%>%
  separate(classe_circo,sep = "_",into = c("classe_circo","type"))%>%
  mutate(classe_circo  = fct_recode( classe_circo,
                                     "From 0 to 10 cm"="0-10",
                                     "From 10 to 20 cm" = "10-15",
                                     "From 10 to 20 cm" = "15-20",
                                     "From 20 to 30 cm" = "20-25",
                                     "From 20 to 30 cm" = "25-30",
                                     "From 30 to 40 cm" = "30-35",
                                     "From 30 to 40 cm" = "35-40",
                                     "From 40 to 50 cm" = "40-45",
                                     "From 40 to 50 cm" = "45-50",
                                     "From 50 to 60 cm" = "50-55",
                                     "From 50 to 60 cm" = "55-60",
                                     "From 60 to 70 cm" = "60-65",
                                     "From 60 to 70 cm" = "65-70",
                                     "From 70 to 80 cm" = "70-75",
                                     "From 70 to 80 cm" = "75-80",
                                     "From 80 to 90 cm" = "80-85",
                                     "From 80 to 90 cm" = "85-90",
                                     "From 90 to 100 cm" = "90-95",
                                     "From 90 to 100 cm" = "95-100"))%>%
  filter(is.na(value)==F)%>%
  group_by(ile,classe_circo,type)%>%
  summarise(n=sum(value,na.rm = T))

comparaisons_island_type_10 <- counting_sandalwood_2023%>%
  mutate(annee=2023)%>%
  bind_rows(counting_sandalwood_2012%>%
              mutate(annee=2012))%>%
  bind_rows(counting_sandalwood_2003%>%
              mutate(annee=2003))%>%
  mutate(value= n)%>%select(-c(n))%>%
  mutate(type=fct_recode(type,"alive"="viv"))

ggplot(comparaisons_island_type_10%>%
         filter(classe_circo!="From 0 to 10 cm")%>%
         left_join(nb_hectare_per_island)%>%
         mutate(value=value/nb_hectares)%>%
         mutate(type=fct_recode(type,"dead"="mort")),
       aes(y=value,x=classe_circo,fill=type))+
  geom_bar(stat = "identity",position = position_stack(),color="black",linewidth=0.1,alpha=0.6)+
  scale_fill_brewer(palette = "Set1")+
  geom_vline(xintercept = 4.5)+
  theme_bw()+ 
  theme(axis.text.x = element_text(vjust = 0.45,angle = 65))+
  facet_grid(ile~as.character(annee))+
  theme(strip.text.x = element_text(size = 12),strip.text.y = element_text(size = 12))+
  labs(x="Circumference at 0.5meter",fill="",y="Number of sandalwood  per hectare")+
  theme(axis.title=element_text(size = 12),legend.text=element_text(size = 12))

ggsave(here("output/Density per hectare of 10cm classes of 0,5m circumferences of the sandalwood per island and year without juveniles (deads AND alives).png"),height = 8,width = 10)


indiv_2023%>%filter(is.na(Circonf50cm)==F)%>%
  mutate( rejet=ifelse(Origine_santal %in% c("Cépée (fourche &lt; 50cm du sol)","Drageon","Drageon du pied mère","Rejet d'une coupe","Rejet"),"Offshoots","Seeds" ))%>%
  mutate(classe_circo  = fct_recode( classe_circo,
                                     "From 10 to 20 cm" = "From 10 to 15 cm",
                                     "From 10 to 20 cm" = "From 15 to 20 cm",
                                     "From 20 to 30 cm" = "From 20 to 25 cm",
                                     "From 20 to 30 cm" = "From 25 to 30 cm",
                                     "From 30 to 40 cm" = "From 30 to 35 cm",
                                     "From 30 to 40 cm" = "From 35 to 40 cm",
                                     "From 40 to 50 cm" = "From 40 to 45 cm",
                                     "From 40 to 50 cm" = "From 45 to 50 cm",
                                     "From 50 to 60 cm" = "From 50 to 55 cm",
                                     "From 50 to 60 cm" = "From 55 to 60 cm",
                                     "From 60 to 70 cm" = "From 60 to 65 cm",
                                     "From 60 to 70 cm" = "From 65 to 70 cm",
                                     "From 70 to 80 cm" = "From 70 to 75 cm",
                                     "From 70 to 80 cm" = "From 75 to 80 cm",
                                     "From 80 to 90 cm" = "From 80 to 85 cm"))%>%
  count(ile,classe_circo, rejet)%>%
  group_by(ile,classe_circo)%>%
  left_join(nb_hectare_per_island%>%filter(annee==2023))%>%
  mutate(total=ifelse(ile=="Lifou","45% from offshoots",ifelse(ile=="Maré","36% from offshoots","45% from offshoots")))%>%
  ggplot(aes(y=n/nb_hectares,x=classe_circo,fill=rejet))+
  geom_bar(stat = "identity",position = position_stack(),color="black",linewidth=0.1,alpha=0.6)+
  scale_fill_brewer(palette = "Set2")+
  theme_bw()+ 
  geom_vline(xintercept = 5.5)+
  geom_text(aes(x=5.4,y=7,label=total),size=5)+
  theme(axis.text.x = element_text(vjust = 0.45,angle = 65))+
  facet_grid(~ile)+
  theme(strip.text.x = element_text(size = 12),strip.text.y = element_text(size = 12))+
  labs(x="",fill="",y="Number of sandalwood per hectare")+
  theme(axis.title=element_text(size = 12),legend.text=element_text(size = 12))

ggsave(here("output/Density per hectare and per breeding origin of sandalwoods with 10cm classes of 0,5m circumferences per island.png"),height = 4,width = 10)

indiv_2023_10  <- indiv_2023%>%
  mutate( classe_circo=cut(Circonf50cm,
                           include.lowest = TRUE,
                           right = T,
                           dig.lab = 4,
                           breaks = c(0, 10,  20,  30, 40, 50,  60,  70, 80, 90)
  ),
  classe_circo_n=as.numeric(as.factor(classe_circo))*10,
  classe_circo=ifelse( classe_circo_n==10,"From 0 to 10 cm",
                       paste("From",as.numeric(classe_circo_n)-10,"to",as.numeric(classe_circo_n),"cm")))

counting_liv_sandalwood_2023 <-  indiv_2023_10%>%filter(is.na(Circonf50cm)==F)%>%
  count(ile,classe_circo)

counting_liv_sandalwood_2012 <- plots_2012%>%
  mutate(ile=recode(ile,"mare"="Maré","lifou"="Lifou","ouvea"="Ouvéa"))%>%
  pivot_longer(`0-10_viv`:`95-100_viv`,names_to = "classe_circo")%>%
  mutate(classe_circo  = fct_recode( classe_circo,
                                     "From 0 to 10 cm"="0-10_viv",
                                     "From 10 to 20 cm" = "10-15_viv",
                                     "From 10 to 20 cm" = "15-20_viv",
                                     "From 20 to 30 cm" = "20-25_viv",
                                     "From 20 to 30 cm" = "25-30_viv",
                                     "From 30 to 40 cm" = "30-35_viv",
                                     "From 30 to 40 cm" = "35-40_viv",
                                     "From 40 to 50 cm" = "40-45_viv",
                                     "From 40 to 50 cm" = "45-50_viv",
                                     "From 50 to 60 cm" = "50-55_viv",
                                     "From 50 to 60 cm" = "55-60_viv",
                                     "From 60 to 70 cm" = "60-65_viv",
                                     "From 60 to 70 cm" = "65-70_viv",
                                     "From 70 to 80 cm" = "70-75_viv",
                                     "From 70 to 80 cm" = "75-80_viv",
                                     "From 80 to 90 cm" = "80-85_viv",
                                     "From 80 to 90 cm" = "85-90_viv",
                                     "From 90 to 100 cm" = "90-95_viv",
                                     "From 90 to 100 cm" = "95-100_viv"))%>%
  filter(is.na(value)==F)%>%
  group_by(ile,classe_circo)%>%
  summarise(n=sum(value,na.rm = T))

counting_liv_sandalwood_2003 <- plots_2003%>%
  mutate(ile=recode(ile,"mare"="Maré","lifou"="Lifou","ouvea"="Ouvéa"))%>%
  pivot_longer(`0-10_viv`:`95-100_viv`,names_to = "classe_circo")%>%
  mutate(classe_circo  = fct_recode( classe_circo,
                                     "From 0 to 10 cm"="0-10_viv",
                                     "From 10 to 20 cm" = "10-15_viv",
                                     "From 10 to 20 cm" = "15-20_viv",
                                     "From 20 to 30 cm" = "20-25_viv",
                                     "From 20 to 30 cm" = "25-30_viv",
                                     "From 30 to 40 cm" = "30-35_viv",
                                     "From 30 to 40 cm" = "35-40_viv",
                                     "From 40 to 50 cm" = "40-45_viv",
                                     "From 40 to 50 cm" = "45-50_viv",
                                     "From 50 to 60 cm" = "50-55_viv",
                                     "From 50 to 60 cm" = "55-60_viv",
                                     "From 60 to 70 cm" = "60-65_viv",
                                     "From 60 to 70 cm" = "65-70_viv",
                                     "From 70 to 80 cm" = "70-75_viv",
                                     "From 70 to 80 cm" = "75-80_viv",
                                     "From 80 to 90 cm" = "80-85_viv",
                                     "From 80 to 90 cm" = "85-90_viv",
                                     "From 90 to 100 cm" = "90-95_viv",
                                     "From 90 to 100 cm" = "95-100_viv"))%>%
  filter(is.na(value)==F)%>%
  group_by(ile,classe_circo)%>%
  summarise(n=sum(value,na.rm = T))


counting_liv_sandalwood<-counting_liv_sandalwood_2023%>%mutate(classe_circo_n=as.numeric(str_extract(classe_circo,"\\d+"))+10)%>%
  mutate(annee=2023)%>%
  bind_rows(counting_liv_sandalwood_2012%>%mutate(classe_circo_n=as.numeric(str_extract(classe_circo,"\\d+"))+10)%>%
              mutate(annee=2012))%>%
  bind_rows(counting_liv_sandalwood_2003%>%mutate(classe_circo_n=as.numeric(str_extract(classe_circo,"\\d+"))+10)%>%
              mutate(annee=2003))

half_normal_density <- function(x, sigma) {
  sqrt(2 / pi) * (1 / sigma) * exp(-(x^2) / (2 * sigma^2))
}

sigma_island <- counting_liv_sandalwood%>%
  filter(classe_circo_n!=10)%>%
  group_by(ile,annee)%>%
  mutate(sigma=n*(classe_circo_n)^2,
         percent=n/sum(n))%>%
  summarise(sigma = sqrt(sum(sigma)/sum(n)))


comparison_islands_year <- counting_liv_sandalwood%>%
  left_join(sigma_island)%>%
  group_by(ile,annee)%>%
  mutate(percent=n/sum(n),
         percent2= (half_normal_density(classe_circo_n,sigma = sigma)*sum(n))/
           sum(half_normal_density(classe_circo_n,sigma = sigma)*sum(n)))%>%
  left_join(indiv_2023_10%>%select(classe_circo,classe_circo_n)%>%unique())


ggplot(comparison_islands_year)+
  geom_bar(aes(y=percent,x=classe_circo,fill="Observed repartition"),stat = "identity",alpha=0.5,linewidth=0.1,color="black")+    
  geom_bar(aes(y=percent2,x=classe_circo,fill="Half-normal\nmodelized repartition"),stat = "identity",color="#117733",linewidth=0.5)+
  theme_bw()+ 
  geom_vline(xintercept = 5.5)+
  scale_y_percent(accuracy = 1)+
  theme(axis.text.x = element_text(vjust = 0.55,angle = 80))+
  facet_grid(as.character(annee)~ile)+
  theme(strip.text.x = element_text(size = 12),strip.text.y = element_text(size = 12))+
  labs(x="",fill="",y="Percentage of the population (%)")+
  theme(axis.title=element_text(size = 12),legend.text=element_text(size = 12))+
  scale_fill_manual(values = c("transparent","#377EB8"))+
  guides(fill=guide_legend(override.aes = list(color=c("#117733","black"),
                                               linewidth =c(0.1,0.1) )))

ggsave(here("output/sn/Comparison of the observed and half-normal modelized repartitions per island and year with regeneration (only livings) with 10cm classes.png"),height = 7,width =8)
