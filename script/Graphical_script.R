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

clean_names <- function(x){
  x <- str_remove_all(x,"\\,")
  x <-str_to_lower(x)
  x <-str_replace(x, "amakal,bole","amakalbole")
  x <-str_replace(x, "jewiceltis","jewi celtis")
  x <-str_replace(x, "3 meyö diff","3_meyö_diff")
  x <-str_replace(x, "tropoia baga","tropoia_baga")
  x <-str_replace(x, "tulipe jaune","tulipe_jaune")
  x <-str_replace(x, "dreni xapo","dreni_xapo")
  x <-str_replace(x, "mexa baga","mexa_baga")
  x <-str_replace(x, "hneö theth","hneö_theth")
  x <-str_replace(x, "grande fougère","grande_fougère")
  x <-str_replace(x, "keti ne hnitre","keti_ne_hnitre")
  x <-str_replace(x, "hnë atheth","hnë_atheth")
  x <-str_replace(x, "megech ?","megech_?")
  x <-str_replace(x, "bois pétrole","bois_pétrole")
  x <-str_replace(x, "bois de fer","bois_de_fer")
  x <-str_replace(x, "hala.ed","halaned")
  x <-str_replace(x, "hnë thet","hnë_thet")
  x <-str_replace(x,"mexa moomo","mexa_moomo")
  x <-str_replace(x,"faux manguier","faux_manguier")
  x <-str_replace(x,  "meyö baga",  "meyö_baga")
  x <-str_replace(x,  "meyo baga",  "meyo_baga")
  x <-str_replace(x,  "meyö moomo",  "meyö_moomo")
  x <-str_replace(x,  "banian wasu",  "banian_wasu")
  x <-str_replace(x,  "et baga",  "et_baga")
  x <-str_replace(x,  "chu keli",  "chu_keli")
  x <-str_replace(x,  "hmu mort",  "hmu_mort")
  x <-str_replace(x,  "hmë mort",  "hmë_mort")
  x <-str_replace(x,  "hmeli mort",  "hmeli_mort")
  x <-str_replace(x,  "bois de rose",  "bois_de_rose")
  x <-str_replace(x,  "bois rose",  "bois_rose")
  x <-str_replace(x,  "banian mana",  "banian_mana")
  x <-str_replace(x,  "banian hmana",  "banian_hmana")
  x <-str_replace(x,  "tropoia baga",  "tropoia_baga")
  x <-str_replace(x,  "mako (lifou)",  "mako_(lifou)")
  x <-str_replace(x,  "et moomo",  "et_moomo")
  x <-str_replace(x,  "bous petrole",  "bous_petrole")
  x <-str_replace(x,  "grandes fougères",  "grandes_fougères")
  x <-str_replace(x,  "amakalbole",  "amakal bole")
  x <-str_replace(x,  "bolehalaned",  "bole halaned")
  x <-str_replace(x,  "mako (f)",  "mako_(f)")
  x <-str_replace(x,  "mako\\_\\(f\\)icus",  "mako ficus")
  x <-str_replace(x,  "begaiac",  "be gaiac")
  x <-str_replace(x,  "bois petrole",  "bois_petrole")
  x <-str_replace(x,  "puja puja",  "puja_puja")
  x <-str_replace(x,  "boi pétrole",  "boi_pétrole")
  x <-str_replace(x,  "chou begen",  "chou_begen")
  x <-str_replace(x,  "chou beguen",  "chou_beguen")
  x <-str_replace(x,  "fougère labgue",  "fougère_labgue")
  x <-str_replace(x,  "tija tija",  "tija_tija")
  x <-str_replace(x,  "meyo baga",  "meyo_baga")
  x <-str_replace(x,  "pin colonnaire",  "pin_colonnaire")
  x <-str_replace(x,  "pulo uto",  "pulo_uto")
  x <-str_replace(x,  "petit khad",  "petit_khad")
  x <-str_replace(x,  "kuc=petit khad",  "kuc=petit_khad")
  x <-str_replace(x,  "goyave chinois",  "goyave_chinois")
  x <-str_replace(x,  "goyave e chine",  "goyave_e_chine")
  x <-str_replace(x,  "goyave de chine",  "goyave_de_chine")
  x <-str_replace(x,  "goyave chine",  "goyave_chine")
  x <-str_replace(x,  "faux figuier",  "faux_figuier")
  x <-str_replace(x,  "grande fougere",  "grande_fougere")
  x <-str_replace(x,  "grande_fougère royale",  "grande_fougère_royale")
  x <-str_replace(x,  "fougère labgue",  "fougère_labgue")
  x <-str_replace(x,  "chou keli",  "chou_keli")
  x <-str_replace(x,  "bois noir",  "bois_noir")
  x <-str_replace(x,  "faux mimosa",  "faux_mimosa")
  x <-str_replace(x,  "hu (lifou)",  "hu_(lifou)")
  x <-str_replace(x,  "hwë (hu)",  "hwë_(hu)")
  x <-str_replace(x,  "hwe (hu)",  "hwe_(hu)")
  x <-str_replace(x,  "fougère ked",  "fougère_ked")
  x <-str_replace(x,  "nikei=prunier kayak",  "nikei=prunier_kayak")
  x <-str_replace(x,  "hneö theth",  "hneö_theth")
  x <-str_replace(x,  "hnë atheth",  "hnë_atheth")
  x <-str_replace(x,  "hnē theth",  "hnē_theth")
  x <-str_replace(x,  "mexabaga (pas mako)",  "mexabaga_(pas_mako)")
  x <-str_replace(x,  "pandanus tect",  "pandanus_tect")
  x <-str_replace(x,  "sica (f)",  "sica_(f)")
  x <-str_replace(x,  "banian yawa",  "banian_yawa")
  x <-str_replace(x,  "psychotria= fechajachi",  "psychotria=_fechajachi")
  x <-str_replace(x,  "solarium nigrum",  "solarium_nigrum")
  x <-str_replace(x,  "fetra met",  "fetra_met")
  x <-str_replace(x,  "cyca=wene eku",  "cyca=wene_eku")
  x <-str_replace(x,  "cyca=longo longo",  "cyca=longo_longo")
  x <-str_replace(x,  "jehe göti",  "jehe_göti")
  x <-str_replace(x,  "dem (f)",  "dem_(f)")
  x <-str_replace(x,  "hnë sipa",  "hnë_sipa")
  x <-str_replace(x,  "hne sipa",  "hne_sipa")
  x <-str_replace(x,  "3 meyö diff",  "3_meyö_diff")
  x <-str_replace(x,  "3 meyö diff",  "3_meyö_diff")
  x <-str_replace(x,  "meyo baga",  "meyo_baga")
  x <-str_replace(x,  "meyöbaga",  "meyö_baga")
  x <-str_replace(x,  "hatre",  "hatrë")
  x <-str_replace(x,  "puje puja",  "puje_puja")
  x <-str_replace(x,  "xetrmedez",  "xetr medez")
  x <-str_replace(x,  "haptrathepe",  "haptra thepe")
  x <-str_replace(x,  "xuenicse",  "xuenic se")
  x <-str_replace(x,  "menë pë",  "menë_pë")
  x <-str_replace(x,  "cryptocarya lifu",  "cryptocarya_lifu")
  x <-str_replace(x,  "jeni xapo",  "jeni_xapo")
  x <-str_replace(x,  "hnê theth",  "hnê_theth")
  x <-str_replace(x,  "hne theth",  "hne_theth")
  x <- str_replace(x,"eo \\(liane\\)" , "eo_\\(liane\\)")
  x <-str_replace(x,  "banian awa",  "banian_awa")
  x <-str_replace(x,  "fleurs jaunes",  "fleurs_jaunes")
  x <-str_replace(x,  "fleur jaune",  "fleur_jaune")
  x <-str_replace(x,  "palétuviers blanc",  "palétuviers_blanc")
  x <-str_replace(x,  "sinu=palétuviers blanc",  "sinu=palétuviers_blanc")
  x <-str_replace(x,  "sija\\(dreni_xapo\\)",  "sija dreni_xapo")
  x <-str_replace(x,  "longo longo",  "longo_longo")
  x <-str_replace(x,  "cyca= ",  "cyca=")
  x <-str_replace(x,  "bahootsâ",  "bahoot sâ")
  x <-str_replace(x,  "meio baga",  "meio_baga")
  x <-str_replace(x,  "fleurs jaune",  "fleur_jaune")
  x <-str_replace(x,  "mako_(f)aux_manguier",  "mako_(f) faux_manguier")
  x <-str_replace(x,  "nikenigenmoomo",  "nikenigen moomo")
  x <-str_replace(x,  "hmëju",  "hmë ju")
  x <-str_replace(x,  "sikahmë",  "sika hmë")
  x <-str_replace(x,  "dinigo uch",  "dinigo_uch")
}

local_names <- readxl::read_xlsx(here("data/noms_locaux.xlsx"))%>%
  janitor::clean_names()%>%
  pivot_longer(2:35,names_to = "langue",values_to = "nom")%>%
  filter(is.na(nom)==F & is.na(nom_scientifique)==F&nom!="")%>%
  select(-c(langue))%>%
  unique()%>%
  mutate(nom=clean_names(nom))%>%
  unique()

indiv_2012<- readxl::read_xlsx(here("data/inventaire_santals_total2012.xlsx"))%>%
  select(-C_50,-C_100,-C_130)

######Graphic about density evolution by island#######


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

######Graphic about sandalwood origin in 2023 according to circumference classes######


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

######Graphic about comparison with half normal model using proportion of circumference classes######

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


#######Graphic survival rates#######

indiv2023_10_survival<- indiv_2023%>%
  left_join(plots_2023%>%select(enq_id,ile,massif))%>%
  mutate(circonf=Circonf50cm+1,
         classe_circo=cut(circonf,
                          include.lowest = TRUE,
                          right = T,
                          dig.lab = 4,
                          breaks = c(0, 10,  20,  30, 40, 50,  60,  70, 80, 90)),
         classe_circo=as.numeric(as.factor(classe_circo))*10,
         classe_circo=ifelse(classe_circo==10,"From 0 to 10 cm",
                              paste("From",as.numeric(classe_circo)-10,"to",as.numeric(classe_circo),"cm"))) 

mortality_table <- indiv2023_10_survival%>%
  filter(is.na(circonf)==F&circonf>10)%>%
  count(ile,classe_circo)%>%
  group_by(ile)%>%
  mutate(deadrate=lead(n)/n)%>%
  dplyr::select(-c(n))

mortality_table_ouvea<-mortality_table%>%filter(ile=="Ouvéa")%>%
  bind_rows(mortality_table%>%filter(ile=="Ouvéa")%>%mutate(ile=ifelse(ile=="Ouvéa","Lifou",ile)))%>%
  bind_rows(mortality_table%>%filter(ile=="Ouvéa")%>%mutate(ile=ifelse(ile=="Ouvéa","Maré",ile)))

data <- indiv2023_10_survival%>%
  filter(is.na(circonf)==F&circonf>10)%>%
  count(ile,classe_circo)%>%   
  complete(classe_circo,nesting(ile), fill = list(n=0))%>%
  left_join(mortality_table_ouvea)%>%
  arrange(ile)%>%
  group_by(ile)%>%
  mutate(n2=ifelse(classe_circo=="From 10 to 20 cm",NA,lag(n)*lag(deadrate)))%>%
  mutate(n3=ifelse(is.na(n2)==T,n,n2))


iles_proportion_ouvea <- data%>%
  group_by(ile)%>%
  mutate(percent1=n/sum(n),
         percent2=n3/sum(n3))


unique(data$classe_circo)

iles<-c("Maré","Lifou","Ouvéa")

#nb_hectares
#total santals
#nombre placettes

nb_hectare <- nb_hectare_per_island%>%
  filter(annee==2023)%>%
  select(-c(annee))%>%
  mutate_if(is.numeric,function(x){ifelse(is.na(x)==T,0,x)})

ile_nb_plots <- indiv2023_10_survival%>%
  group_by(ile)%>%
  summarise(nb_sandals=n())%>%
  left_join(plots_2023%>%
              group_by(ile)%>%
              summarise(nb_placettes=n_distinct(enq_id)))


plus_50 <- data%>%
  filter( classe_circo%in% c( "From 50 to 60 cm" ,"From 60 to 70 cm", "From 70 to 80 cm", "From 80 to 90 cm"))%>%
  group_by(ile)%>%
  summarise(total=sum(n2))%>%
  left_join(nb_hectare)%>%
  left_join(ile_nb_plots)%>%
  mutate(total=paste(round(total/nb_hectares,2), "sandalwoods > 50cm per hectare \nin 10 years"))

ggplot(data%>%
         left_join(plus_50)%>%
         left_join(nb_hectare))+
  geom_bar(aes(y=n/nb_hectares,x=classe_circo,fill="2023 observed values"),stat = "identity",alpha=0.5,linewidth=0.1,color="black")+    
  geom_bar(aes(y=n2/nb_hectares,x=classe_circo,fill="2033 forecasted values"),stat = "identity",color="red",linewidth=0.5)+
  geom_vline(xintercept = 4.5, linetype="dashed")+
  geom_text(aes(x=5,y=8.5,label=total),size=3)+
  geom_text(aes(x=5,y=9.5,label=paste(nb_placettes,"plots")),size=3,color="red4")+
  geom_text(aes(x=5,y=9,label=paste("Total number of sandalwoods measured:",nb_sandals)),size=3,color="blue4")+
  theme_bw()+
  #coord_flip()+
  expand_limits(x=c(0,10),y=c(0,6.5))+
  theme(axis.text.x = element_text(vjust = 0.5,angle = 85))+
  facet_wrap(~ile)+
  labs(x="",fill="",y="Number of sandalwoods per hectare")+
  scale_fill_manual(values = c("#377EB8","transparent"))+
  guides(fill=guide_legend(override.aes = list(color=c("black","red"),
                                               linewidth =c(0.1,0.1) )))


ggsave(paste(here("output/tables_mortalite/Comparison between observed and forecasted values without juveniles (only living).png")),height = 6,width = 10)

########Sapwood thickness correlation########

sandalwood_inventory <- indiv2023_10_survival%>%
  left_join(indiv2023_10_survival%>%
              mutate(nom=clean_names(Spp_prox) ,
                     nom=ifelse(nom=="mako_(f)aux_manguier hurok","mako faux_manguier hurok",nom))%>%
              separate_rows(nom,sep = " ")%>%
              filter(nom!="")%>%
              left_join(local_names)%>%
              filter(is.na(nom_scientifique)==F)%>%
              group_by(enq_id,Pied_santal__id)%>%
              summarise(Liste_sp_scient=paste(nom_scientifique,collapse = ",")))

data <-sandalwood_inventory%>%
  filter(is.na(Mesure_aubier)==F)%>%
  left_join(plots_2023%>%
              select(enq_id,harbre))%>%
  select(Mesure_aubier,Circonf50cm, hauteur_fut,harbre)%>%
  rename(Sapwood_thickness=Mesure_aubier,Circumf_05m=Circonf50cm ,branching_height=hauteur_fut, general_height=harbre)

heatmaply::heatmaply_cor(cor(data))%>%
  plotly::layout(font = list(size = 16))

ggsave(paste(here("output/Collinearity between sapwood thickness and tree's dimensions quantitative variables studied.png")),height = 4,width = 5.3)

dev.off()

data <-indiv2023_10_survival%>%filter(is.na(Mesure_aubier)==F)%>%
  left_join(plots_2023 %>%select(enq_id,harbre,roche,terre))%>%
  select(ile,Mesure_aubier,roche,terre)%>%
  rename(Sapwood_thickness=Mesure_aubier,island=ile ,Rocks=roche, Soil_color=terre)%>%
  mutate(Soil_color = recode(Soil_color,
                          "Terre noire" = "Black soil",
                          "Terre noire&Terre rouge" = "Brown soil",
                          "Terre rouge"="Red soil"),
         Rocks=recode(Rocks,
                      "Pas de roche affleurante"="No rock outcrop",
                      "Pas de roche affleurante&Quelques affleurements"="Some rock outcrop",
                      "Quelques afflaurements"="Several rock outcrop",
                      "Roche très affleurante (sol disséqué)"="Only rocks"))

ggplot(data,aes(y=Sapwood_thickness,x=Soil_color,fill=Soil_color))+
  geom_jitter()+
  geom_boxplot(alpha=0.5)+
  theme(axis.text.x = element_text(size=8, angle=45,vjust=0.5), legend.position="none")

ggsave(paste(here("output/Correlation between sapwood thickness and soil qualitative variable.png")),height = 4,width = 5.3)

ggplot(data,aes(y=Sapwood_thickness,x=Rocks,fill=Rocks))+
  geom_jitter()+
  geom_boxplot(alpha=0.5)+
  theme(axis.text.x = element_text(size=8, angle=45,vjust=0.5), legend.position="none")

ggsave(paste(here("output/Correlation between sapwood thickness and rock outcrops variable.png")),height = 4,width = 5.3)

#######Relation between C0.2m and C0.5m####### 

data<-indiv_2012%>%
  filter(!is.na(C_50_cm) & !is.na(C_20_cm))#%>%

mod1<-lm(C_50_cm~C_20_cm, data=data)
predicted <- data.frame(C_20_cm = seq(0, 150, length.out = 1000))
predicted$C_20_cm <- predict(mod1,predicted)
predicted$ci_binf <- predict(mod1,predicted, interval="confidence",level = 0.95)[,2]
predicted$ci_bsup <- predict(mod1,predicted, interval="confidence",level = 0.95)[,3]

ggplot(data,aes(x=C_20_cm,y=C_50_cm))+
  geom_point()+
  geom_smooth(method="lm",col = "red")+
  theme_bw()+
  theme(axis.text.x = element_text(vjust = 0.75,angle = 35),
        axis.text=element_text(size=14),
        axis.title=element_text(size=16))+
  labs(x="Circumference at 0.2m",y="Circumference at 0.5m",fill="")+
  stat_regline_equation(aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),size=8,label.x=4, label.y=1, size=3, colour="red")#+
  #geom_line(data=predicted, aes(y=ci_binf, x=C_20_cm), col="red", linetype="dashed")+ 
  #geom_line(data=predicted, aes(y=ci_bsup, x=C_20_cm), col="red", linetype="dashed")

ggsave(paste(here("output/Correlation with equation between cO.5m and C0.2m.png")),height = 6,width = 9)
