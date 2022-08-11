library(dplyr)
library(ggplot2)
library(reshape2)
library(scales)
#install.packages("corrplot")
library(corrplot)

file.choose()

survey <- read.delim("C:\\D\\data science\\Libros\\Analisis de datos con R\\Codigo R\\survey.csv", sep=';')
dia <- dplyr::filter(survey, Establecimiento=='dia')
carrefour <- dplyr::filter(survey, Establecimiento=='carrefour')
mercadona <- dplyr::filter(survey, Establecimiento =='mercadona')
dia

medias <- survey %>%
  dplyr::group_by(Establecimiento) %>%
  dplyr::summarise_at(vars(starts_with('P')),mean)
medias[,1:5]
medias
medias$Establecimiento
c(levels(medias$Establecimiento))
unique(medias$Establecimiento)
medias_establecimiento <- dplyr::select(medias, starts_with('P'))
medias_establecimiento
medias_establecimiento <- data.frame(t(medias_establecimiento))
medias_establecimiento
medias_establecimiento$variables <-row.names(medias_establecimiento)
medias_establecimiento
str(medias)
medias$Establecimiento <- as.factor(medias$Establecimiento)
medias_establecimiento <- setNames(medias_establecimiento, c(levels(medias$Establecimiento),'Variables'))
medias_establecimiento
dplyr::arrange(medias_establecimiento,desc(mercadona))
str(medias_establecimiento)
dplyr::glimpse(medias_establecimiento)
medias_grafico <- reshape2::melt(medias, id.vars='Establecimiento',
                                 value.name='value', variable.name='preguntas')
head(medias_grafico)
ggplot(medias_grafico,
       aes(reorder(preguntas,value),value,
           group=Establecimiento, color=Establecimiento))+
  geom_line(size=0.75)+
  geom_point(size=1)+
  theme(legend.position="bottom", legend.title=element_blank())+
  theme(panel.grid.major.x=element_line(colour="grey",size=0.5))+
  ggtitle('Comparativa respuestas medias')+
  labs(x='', y="Medias de las respuestas")+
  coord_flip()


dia_media <- survey %>%
  dplyr::filter(Establecimiento == 'dia') %>%
  dplyr::summarise_at(vars(starts_with('P')),mean)
#Dado que obtenemos valores por filas
head(dia_media[,1:4])
#transponemos y pasamos los nombres a una columna
dia_media <- as.data.frame(t(dia_media))
dia_media
rownames(dia_media)
dia_media$variables <- rownames(dia_media)
dia_media
row.names(dia_media) <- NULL #transformamos los nombres de las filas a numeros
head(dia_media[1:4,])
#Realizamos el grafico de las medias, ordenando de mayor a menor
#En ggplot para poder realizar el grafico de linea de una sola variable
#en aes, hemos de añadir group=12 para poder dibujarla
ggplot(dia_media, aes(reorder(variables,V1),V1, group=1))+
  geom_line(size=0.75, color='blue')+
  geom_point(size=1,color='blue')+
  theme(legend.position="none")+
  theme(panel.grid.major.x = element_line(colour="grey",size=0.5))+
  ggtitle('Respuestas medias para dia')+
  labs(x='',y="Medias de las respuestas")+
  coord_flip()
carrefour_media <- survey %>%
  dplyr::filter(Establecimiento=='carrefour') %>%
  dplyr::summarise_at(vars(starts_with('P')),mean)
head(carrefour_media[,1:4])
carrefour_media <- as.data.frame(t(carrefour_media))
carrefour_media
carrefour_media$variables <- rownames(carrefour_media)
carrefour_media
row.names(carrefour_media) <- NULL
head(carrefour_media[1:4,])
ggplot(carrefour_media,
       aes(reorder(variables,V1),V1,group=1))+
  geom_line(size=0.75, color='blue')+
  geom_point(size=1,color='blue')+
  theme(legend.position="none")+
  theme(panel.grid.major.x = element_line(colour="grey", size=0.5))+
  ggtitle('Respuestas medias para Carrefour')+
  labs(x='', y="Medias de las respuestas")+
  coord_flip()

mercadona_media <- survey %>%
  dplyr::filter(Establecimiento=='mercadona') %>%
  dplyr::summarise_at(vars(starts_with('P')),mean)
head(mercadona_media[,1:4])                      
mercadona
mercadona_media <- as.data.frame(t(mercadona_media))
mercadona_media$variables <- rownames(mercadona_media)
row.names(mercadona_media) <- NULL
head(mercadona_media[1:4,])
ggplot(mercadona_media,
       aes(reorder(variables,V1),V1,group=1))+
  geom_line(size=0.75,color='blue')+
  geom_point(size=1,color='blue')+
  theme(legend.position="none")+
  theme(panel.grid.major.x = element_line(colour="grey",size=0.5))+
  ggtitle('Respuestas medias para Mercadona')+
  labs(x='',y="Medias de las respuestas")+
  coord_flip()
survey_Pc1_Estab_medias <- aov(Pc1.Actitud ~ Establecimiento, survey)
print(model.tables(survey_Pc1_Estab_medias, "means"), digits=2)
summary(survey_Pc1_Estab_medias)

survey_Pc1_Estab_medias_Tukey <- data.frame(TukeyHSD(
  survey_Pc1_Estab_medias, 'Establecimiento', ordered=TRUE)$Establecimiento)
print(survey_Pc1_Estab_medias_Tukey, digits=2)

row.names(survey_Pc1_Estab_medias_Tukey)

survey_Pc1_Estab_medias_Tukey$comparacion <- row.names(survey_Pc1_Estab_medias_Tukey)
survey_Pc1_Estab_medias_Tukey$comparacion <- gsub('-',' vs. ', survey_Pc1_Estab_medias_Tukey$comparacion)
print(survey_Pc1_Estab_medias_Tukey, digits=2)

ggplot(survey_Pc1_Estab_medias_Tukey,
       aes(comparacion,y=diff,ymin=lwr,ymax=upr,color=comparacion))+
  geom_pointrange()+
  labs(title='Diferencia de medias Actitud',
       subtitle='Intervalo de confianza al 95%', y='Diferencias', x='')+
  theme(legend.position = 'none')+
  coord_flip()

survey_Pc1_Estab_chi <- table(survey$Pc1.Actitud, survey$Establecimiento)
print(prop.table(survey_Pc1_Estab_chi,2), digits=2)

survey_Pc1_Estab_chi_prop <- data.frame(prop.table(survey_Pc1_Estab_chi,2))
print(survey_Pc1_Estab_chi_prop, digits=2)

colnames(survey_Pc1_Estab_chi_prop) <- c('grupo','centro','proporcion')
survey_Pc1_Estab_chi_prop
ggplot(survey_Pc1_Estab_chi_prop, aes(grupo, proporcion, fill=centro))+
  geom_bar(stat='identity', position='dodge')+
  labs(title='Proporcion respuestas Actitud',y='',x='Valoracion')+
  theme(legend.position = "bottom",legend.direction = "horizontal")+
  theme(legend.title = element_blank())+
  scale_y_continuous(labels=scales::percent)

chisq.test(survey_Pc1_Estab_chi)
xlabs <- paste0(levels(survey$Establecimiento),
                "\n(N=", table(survey$Establecimiento),")")
str(survey)
levels(survey$Establecimiento)
survey$Establecimiento <- as.factor(survey$Establecimiento)
xlabs

ggplot(survey, aes(Establecimiento, Pc1.Actitud))+
  geom_boxplot()+
  stat_summary(fun.y = mean, geom="point", colour="red", size=1)+
  theme(legend.position = "bottom",legend.title = element_blank())+
  ggtitle('Valoracion amabilidad de los empleados')+
  scale_x_discrete(labels=xlabs)+
  labs(x='Centro',y='Valoracion del 1 al 10')

#comparacion similitud de respuestas
dia_C3_Ph1_Calid_medias <- aov(Ph1.CalidadPrecio ~ C3.EstadoCivil, dia)
print(model.tables(dia_C3_Ph1_Calid_medias, "means"), digits=2)
#anova
summary(dia_C3_Ph1_Calid_medias)
#Tukey
dia_C3_Ph1_Calid_medias_Tukey <- data.frame(TukeyHSD(
  dia_C3_Ph1_Calid_medias, 'C3.EstadoCivil', ordered=TRUE
  )$C3.EstadoCivil)
#Analisis
print(dia_C3_Ph1_Calid_medias_Tukey, digits=2)

dia_C3_Ph1_Calid_medias_Tukey$comparacion <- row.names(dia_C3_Ph1_Calid_medias_Tukey)
dia_C3_Ph1_Calid_medias_Tukey
dia_C3_Ph1_Calid_medias_Tukey$comparacion <- gsub('-',' vs. ',dia_C3_Ph1_Calid_medias_Tukey$comparacion)

#Grafico
ggplot(dia_C3_Ph1_Calid_medias_Tukey,
       aes(comparacion, y=diff, ymin=lwr, ymax=upr, color= comparacion))+
  geom_pointrange()+
  labs(title='Diferencia de medias calidad precio',
       subtitle='Intervalo de confianza al 95%', y='Diferencias', x='')+
  theme(legend.position = 'none')+
  coord_flip()

#Proporciones
dia_C3_Ph1_Calid_chi <- table(dia$Ph1.CalidadPrecio, dia$C3.EstadoCivil)
print(prop.table(dia_C3_Ph1_Calid_chi,2), digits=2)

#Grafico
dia_C3_Ph1_Calid_chi_prop <- data.frame(prop.table(dia_C3_Ph1_Calid_chi,2))
dia_C3_Ph1_Calid_chi_prop
colnames(dia_C3_Ph1_Calid_chi_prop) <- c('grupo','centro','proporcion')
ggplot(dia_C3_Ph1_Calid_chi_prop, aes(grupo, proporcion, fill=centro))+
  geom_bar(stat='identity', position='dodge')+
  labs(title = 'Proporcion respuestas calidad precio', y='',x='Valoracion')+
  theme(legend.position = "bottom",legend.direction = "horizontal")+
  theme(legend.title = element_blank())+
  scale_y_continuous(labels = scales::percent)

#chi cuadrado
chisq.test(dia_C3_Ph1_Calid_chi)

#Boxplot
#Reorganizamos los niveles de la variable
dia$C3.EstadoCivil <- 
  factor(dia$C3.EstadoCivil,
         labels=c('soltero/a','casado/a','unido/a',
                    'separado/a','viudo/a'))

#grafico
xlabs <- paste0(levels(dia$C3.EstadoCivil),"\n(N=",table(dia$C3.EstadoCivil),")")

ggplot(dia, aes(C3.EstadoCivil, Ph1.CalidadPrecio))+
  geom_boxplot()+
  stat_summary(fun.y=mean,geom="point",colour="red",size=1)+
  scale_x_discrete(labels=xlabs)+
  theme(legend.position = "bottom",legend.title=element_blank())+
  ggtitle('Relacion calidad precio')+
  labs(x='Estado civil',y='Valoracion de 1 a 10')

color1 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "white", "cyan", "#007FFF", "blue", "#00007F"))
corrplot::corrplot(cor(dplyr::select(survey, starts_with('P'))),
                   title = "Matriz de correlacion", mar=c(0,0,1,0),
                   method="color", outline=T, addgrid.col = "darkgray",
                   order = "hclust",addrect = 8,col=color1(100),
                   tl.col='black',tl.cex = .75)

#Dia
corrplot(cor(dplyr::select(dia, starts_with('P'))),
         title="Matriz de correlacion Dia", mar=c(0,0,1,0),
         method = "color",outline = T,addgrid.col="darkgray",
         order="hclust", addrect=8, col=color1(100),
         tl.col='black', tl.cex = .75)

#Carrefour
corrplot(cor(dplyr::select(carrefour,starts_with('P'))),
         title = "Matriz de correlacion Carrefour",mar=c(0,0,1,0),
         method = "color",outline=T,addgrid.col="darkgray",
         order="hclust",addrect=8,col=color1(100),
         tl.col='black',tl.cex=.75)
#Mercadona
corrplot(cor(dplyr::select(mercadona, starts_with('P'))),
         title = "Matriz de correlacion Mercadona", mar=c(0,0,1,0),
         method = "color",outline = T,addgrid.col = "darkgray",
         order="hclust",addrect = 8,col = color1(100),
         tl.col = 'black', tl.cex = .75)
