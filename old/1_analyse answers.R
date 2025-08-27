# This script is to ???????????????

#### Load packages ####
library(dplyr)
library(ggplot2)
library(reshape2)
library(tidyr)
library(stringr)


#### Import data ####
raw_data <- read.csv(here::here("results-survey676868_prefilter.csv"))
names(raw_data)


# Femmes VS hommes :
fem_hom <- raw_data$G21Q00001  %>%
  as.data.frame() %>%
  na.omit()

names(fem_hom) <- "type"
fem_hom$type[grepl(pattern="Une femme", fem_hom$type)] <- "Femme"
fem_hom$type[grepl(pattern="Un homme", fem_hom$type)] <- "Homme"


fem_hom <- fem_hom %>%
  group_by(type) %>%
  summarise(count=n())  


ggplot(fem_hom, aes(x=type, y=count)) +
  geom_col()






# Education :
educ <- raw_data  %>%
  select(all_of(names(select(raw_data, matches("^G21Q00003.*"))))) %>%
  as.data.frame()

names(educ) <- c("type", "autre_detail")

educ$type[is.na(educ$type) & !is.na(educ$autre_detail)] <- "Autre"

educ_simpl <- educ %>%
  group_by(type) %>%
  summarise(count=n())  

educ_simpl$type <- factor(educ_simpl$type, levels=c("Sans diplôme",
                                      "Collège",
                                      "Lycée",
                                      "Bac+2 / Bac+3",
                                      "Bac+5 et plus",
                                      "Autre"))

ggplot(educ_simpl, aes(x=type, y=count)) +
  geom_col()


# Catégorie socio-pro :
socio_pro <- raw_data  %>%
  select(all_of(names(select(raw_data, matches("^G21Q00005.*"))))) %>%
  as.data.frame()

names(socio_pro) <- c("type", "autre_detail")

socio_pro$type[is.na(socio_pro$type) & !is.na(socio_pro$autre_detail)] <- "Autre"


socio_pro_simpl <- socio_pro %>%
  group_by(type) %>%
  summarise(count=n())  


ggplot(socio_pro_simpl, aes(x=type, y=count)) +
  geom_col() +
  theme(axis.text.x=element_text(angle=45, hjust=1))


# Age :
age <- raw_data  %>%
  select(all_of(names(select(raw_data, matches("^G21Q00002.*"))))) %>%
  as.data.frame()

names(age) <- "type"


age <- age %>%
  group_by(type) %>%
  summarise(count=n())  

age$type <- factor(age$type, levels=c("Moins de 26 ans",
                                      "26 à 35 ans",
                                      "36 à 50 ans",
                                      "Plus de 50 ans"))

ggplot(age, aes(x=type, y=count)) +
  geom_col()


# Cueilleurs amateurs vs pros :
amat_pro <- raw_data  %>%
  select(all_of(names(select(raw_data, matches("^G21Q00009.*"))))) %>%
  as.data.frame()

names(amat_pro) <- "type"


names(amat_pro) <- "type"
amat_pro$type[grepl(pattern="amateur", amat_pro$type)] <- "Amateur"
amat_pro$type[grepl(pattern="pro", amat_pro$type)] <- "Professionel"


amat_pro <- amat_pro %>%
  group_by(type) %>%
  summarise(count=n())  
  
  
ggplot(amat_pro, aes(x=type, y=count)) +
  geom_col()


# Groupement de cueilleurs :
groupe <- raw_data$G21Q00011  %>%
  as.data.frame() %>%
  na.omit()


names(groupe) <- "type"


groupe <- groupe %>%
  group_by(type) %>%
  summarise(count=n())  


ggplot(groupe, aes(x=type, y=count)) +
  geom_col() +
  theme(axis.text.x=element_text(angle=45, hjust=1))


# Groupement de cueilleurs :
groupe_nom <- raw_data$G21Q00012  %>%
  as.data.frame() %>%
  na.omit()


names(groupe_nom) <- "type"


groupe_nom <- groupe_nom %>%
  group_by(type) %>%
  summarise(count=n())  


ggplot(groupe_nom, aes(x=type, y=count)) +
  geom_col() +
  theme(axis.text.x=element_text(angle=45, hjust=1))



# Organisme :
orga <- raw_data$G21Q00013  %>%
  as.data.frame() %>%
  na.omit()


names(orga) <- "type"


orga <- orga %>%
  group_by(type) %>%
  summarise(count=n())  


ggplot(orga, aes(x=type, y=count)) +
  geom_col() +
  theme(axis.text.x=element_text(angle=45, hjust=1))


# Organisme, nom :
orga_nom <- raw_data$G21Q00014  %>% 
  as.data.frame() %>%
  na.omit()


names(orga_nom) <- "type"


orga_nom <- orga_nom %>%
  group_by(type) %>%
  summarise(count=n())  


ggplot(orga_nom, aes(x=type, y=count)) +
  geom_col() +
  theme(axis.text.x=element_text(angle=45, hjust=1)) #######Nettoyer !!




# Départements touchés :
list_dpts <- read.csv("raw_data/corresp_dpts_sq.csv")


dpts <- raw_data %>%
  select(list_dpts$Code_question) %>%
  na.omit() %>%
  summarise(across(everything(), ~ sum(. == "Oui"))) %>% # Count "Yes" for each column
  pivot_longer(cols = everything(), names_to = "Column", values_to = "Count") %>%
  full_join(list_dpts, join_by(Column==Code_question))


ggplot(dpts, aes(x=Nom, y=Count)) +
  geom_col() +
  theme(axis.text.x=element_text(angle=45, hjust=1))

mean(dpts$Count)


# Régions touchées :
list_regions <- read.csv("raw_data/corresp_regions_sq.csv")

regions <- raw_data %>%
  select(list_regions$Code_question) %>%
  na.omit() %>%
  summarise(across(everything(), ~ sum(. == "Oui"))) %>% # Count "Yes" for each column
  pivot_longer(cols = everything(), names_to = "Column", values_to = "Count") %>%
  full_join(list_regions, join_by(Column==Code_question))

# Join with dpt data :
dpts_reg <- dpts %>%
  select(-Nom)
names(dpts_reg)[3] <- "Nom"

regions <- rbind(dpts_reg, regions) %>%
  group_by(Nom) %>%
  summarise(Count=sum(Count))


ggplot(regions, aes(x=Nom, y=Count)) +
  geom_col() +
  theme(axis.text.x=element_text(angle=45, hjust=1))

mean(regions$Count)
quantile(regions$Count)


# Evol nb reponses :
# Calculer la somme cumulée
data_time <- raw_data$startdate %>%
  as.data.frame()

names(data_time) <- "date"
data_time$value <- 1
data_time <- data_time %>%
  mutate(cumulative_value = cumsum(value))

data_time$date <- as.Date(word(data_time$date,1,1))

# Tracer la courbe d'accumulation
ggplot(data_time, aes(x = date, y = cumulative_value)) +
  geom_line(color = "blue", size = 1) +
  labs(
    title = "Courbe d'accumulation",
    x = "Date",
    y = "Valeur cumulative"
  ) +
  theme_minimal()
