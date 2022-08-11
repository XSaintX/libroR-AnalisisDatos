library(dplyr)
library(ggplot2)
library(scales)
#install.packages("papeR")
library(papeR)

survey_base <- read.delim("C:\\D\\data science\\Libros\\Analisis de datos con R\\Codigo R\\survey_base.csv", sep=';')
class(survey_base)
survey_base[1:10,1:5]

papeR::summarise(survey_base)

survey_base <- survey_base %>%
  dplyr::select(-starts_with('P'), which(colSums(is.na(.))<41)) %>%
  dplyr::select(Establecimiento,starts_with('P'),starts_with('C'))
survey_base[] <- lapply(
  survey_base, function(x) ifelse(is.na(x), as.integer(median(x, na.rm=TRUE)), x)
)

survey_base$Establecimiento <- factor(survey_base$Establecimiento, labels=c('carrefour', 'dia', 'mercadona'))

dplyr::glimpse(survey_base)
max(survey_base$C1.Edad, na.rm = TRUE)
#unique(survey_base[c("C1.Edad")])
#unique(survey_base[c("C4.CompraMedia")])
#unique(survey_base[c("C5.IngMes")])
survey_base$RC1.Edad <- dplyr::recode(survey_base$C1.Edad,21,30,40,50,60,65)
survey_base$RC2.Sexo <- dplyr::recode(survey_base$C2.Sexo, 0,1)
survey_base$RC4.CompraMedia <- dplyr::recode(survey_base$C4.CompraMedia,15,23,38,53,68,83,103,120)
survey_base$RC5.IngMes <- dplyr::recode(survey_base$C5.IngMes, 600,800,1250,1750,2500,3750,4500)
head(dplyr::select(survey_base,RC1.Edad,RC2.Sexo,RC4.CompraMedia,RC5.IngMes))
count(survey_base,'C1.Edad')

survey_base$C1.Edad <- factor(survey_base$C1.Edad, labels =c('de 18 a 24','de 25 a 35', 'de 35 a 44',
                                                             'de 45 a 54','de 55 a 64','mas de 64'))
survey_base$C2.Sexo <- factor(survey_base$C2.Sexo, labels=c('hombre','mujer'))
survey_base$C3.EstadoCivil <- factor(survey_base$C3.EstadoCivil, labels=c('soltero/a','casado/a','unido/a','separado/a','viudo/a'))
survey_base$C4.CompraMedia <- factor(survey_base$C4.CompraMedia, labels=c('menos de 15','entre 15 y 30','entre 31 y 45',
                                                                          'entre 46 y 60','entre 61 y 75','entre 76 y 90','entre 91 y 120','mas de 120'))
survey_base$C5.IngMes <- factor(survey_base$C5.IngMes, labels=c('menos de 600','entre 601 y 1000',
                                                                'entre 1001 y 1500','entre 1501 y 2000',
                                                                'entre 2001 y 3000','entre 3001 y 4500',
                                                                'mas de 4500'))
dplyr::glimpse(dplyr::select(survey_base,starts_with('C')))
write.table(survey_base, 'survey2.csv', sep=';', row.names=FALSE)
survey<-read.delim("survey2.csv",sep=';')
head(dplyr::select(survey,starts_with(('R'))))
levels(survey$C4.CompraMedia)
dplyr::glimpse(survey)
survey$C4.CompraMedia <- as.factor(survey$C4.CompraMedia)
survey$C3.EstadoCivil <- as.factor(survey$C3.EstadoCivil)
survey$C2.Sexo <- as.factor(survey$C2.Sexo)
survey$C1.Edad <- as.factor(survey$C1.Edad)
survey$C5.IngMes <- as.factor(survey$C5.IngMes)
levels(survey$C5.IngMes)
survey$C4.CompraMedia<-factor(survey$C4.CompraMedia, levels=c('menos de 15','entre 15 y 30','entre 31 y 45',
                                                              'entre 46 y 60','entre 61 y 75','entre 76 y 90','entre 91 y 120','mas de 120'))
survey$C5.IngMes<-factor(survey$C5.IngMes, levels=c('menos de 600','entre 601 y 1000',
                                                    'entre 1001 y 1500','entre 1501 y 2000',
                                                    'entre 2001 y 3000','entre 3001 y 4500',
                                                    'mas de 4500'))
round(prop.table(table(survey$C5.IngMes, survey$C1.Edad), 2) * 100, 2).
round(prop.table(table(survey$C5.IngMes, survey$C1.Edad), 2) * 100, 2)
round(prop.table(table(survey$C1.Edad,survey$C5.IngMes),2)*100,2)
install.packages("ggplot2")
library(ggplot2)
ggplot(survey, aes(C1.Edad, fill=C1.Edad))+
  geom_bar(aes(y=..count../sum(..count..)))+
  theme(legend.position="none")+
  ggtitle('Edad')+
  labs(x='Edad', y='Frecuencia')+
  scale_y_continuous(labels=scales::percent)+
  coord_flip()

ggplot(survey, aes(C1.Edad,fill=C1.Edad))+
  geom_bar(aes(y=(..count..)/sum(..count..)))+
  facet_grid(Establecimiento ~ .)+
  theme(legend.position="none")+
  ggtitle('Edad')+
  labs(x='Edad',y='Frecuencia')+
  scale_y_continuous(labels=scales::percent)+
  coord_flip()

ggplot(survey, aes(C5.IngMes, fill=C5.IngMes))+
  geom_bar(aes(y=(..count..)/sum(..count..)))+
  theme(legend.position = 'none')+
  ggtitle('Ingresos mensuales')+
  labs(x='Ing. mens.', y='Frecuencia')+
  scale_y_continuous(labels=scales::percent)+
  coord_flip()

ggplot(survey,aes(C5.IngMes, fill=C5.IngMes))+
  geom_bar(aes(y=(..count..)/sum(..count..)))+
  facet_grid(Establecimiento ~ .)+
  theme(legend.position='none')+
  ggtitle('Ingresos mensuales')+
  labs(x='Ingresos mensuales',y='Frecuencia')+
  scale_y_continuous(labels=scales::percent)+
  coord_flip()

ggplot(survey, aes(C5.IngMes, fill=C5.IngMes))+
  geom_bar(aes(y=(..count..)/sum(..count..)))+
  facet_wrap(~ C1.Edad, ncol=3)+
  theme(legend.position="none")+
  ggtitle('Ingresos mensuales totales por edades')+
  labs(x='Ingresos mensuales totales',y='')+
  scale_y_continuous(breaks=scales::pretty_breaks(), labels=scales::percent)+
  coord_flip()

ggplot(survey, aes(C1.Edad, fill=C1.Edad))+
  geom_bar(aes(y=(..count..)/sum(..count..)))+
  facet_wrap(~ C5.IngMes, ncol=3)+
  theme(legend.position="none")+
  ggtitle('Edades por Ingresos mensuales')+
  labs(x='Edades', y='')+
  scale_y_continuous(breaks=scales::pretty_breaks(),labels=scales::percent)+
  coord_flip()
