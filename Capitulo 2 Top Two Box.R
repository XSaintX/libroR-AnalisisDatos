library(dplyr)
library(ggplot2)
library(scales)
#install.packages("stringr")
#install.packages("reshape2")
library(stringr)
library(reshape2)
file.choose()
survey <- read.delim("C:\\D\\data science\\Libros\\Analisis de datos con R\\Codigo R\\survey.csv", sep=';')
dia <- dplyr::filter(survey, Establecimiento =='dia')
carrefour <- dplyr::filter(survey, Establecimiento=='carrefour')
mercadona <- dplyr::filter(survey, Establecimiento=='mercadona')
dia_P <- dplyr::select(dia, starts_with('P'))
carrefour_P <- dplyr::select(carrefour, starts_with('P'))
mercadona_P <- dplyr::select(mercadona, starts_with('P'))
dia_T2B<-list()
dia_T2B[1]<-data.frame(apply(dia_P, 2, function(x) sum(x>=8 & x<=10)))
dia_T2B[2]<-data.frame(apply(dia_P, 2, function(x) sum(x>=5 & x<=7)))
dia_T2B[3]<-data.frame(apply(dia_P, 2, function(x) sum(x<=4)))
dplyr::glimpse(dia_P)
dplyr::glimpse(dia_T2B)
dia_T2B <- data.frame(dia_T2B)
dia_T2B <- data.frame(t(apply(dia_T2B,1,function(x) x/sum(x))))*100
dplyr::glimpse(dia_T2B)
colnames(dia_T2B) <- c('Alto','Medio','Bajo')
print(head(dia_T2B), digits=3)
dia_T2B$variables <- names(dia_P)
dia_T2B$centro <- 'dia'
print(head(dia_T2B), digits=3)
dia_T2B_melt <- reshape2::melt(subset(dia_T2B, select=-centro),id.vars='variables')
print(head(dia_T2B_melt), digits=3)

ggplot(dia_T2B_melt, aes(variables, value, fill=variable))+
  geom_bar(position=position_stack(), stat="identity")+
  geom_text(aes(label = paste0(round(value,0),"%")),
            position = position_stack(vjust=0.5),size=3)+
  theme(legend.position="bottom",legend.direction = "horizontal")+
  theme(legend.title = element_blank())+
  theme(axis.title.x = element_blank())+
  theme(axis.title.y=element_blank())+
  ggtitle('Valoraciones Dia')+
  guides(fill=guide_legend(reverse=TRUE))+
  coord_flip()

dplyr::glimpse(dia_T2B)
ggplot(dia_T2B, aes(reorder(variables, Alto), Alto, fill = variables)) +
  geom_bar(stat = 'identity') +
  theme(legend.position = "none") +
  theme(axis.title.x = element_blank())+
  theme(axis.title.y = element_blank())+
  ggtitle('Top three box Dia (%)')+
  coord_flip()

#aspectos peor valorados, Bottom three box B3B
ggplot(dia_T2B, aes(reorder(variables, Bajo), Bajo, fill=variables))+
  geom_bar(stat="identity")+
  theme(legend.position = "none")+
  theme(axis.title.x = element_blank())+
  theme(axis.title.y = element_blank())+
  ggtitle('Bottom three box Dia(%)')+
  coord_flip()

ggplot(dplyr::filter(dia_T2B, stringr::str_detect(variables, 'Pc1')),
  aes(reorder(variables, Alto), Alto, fill=variables))+
  geom_bar(stat="identity")+
  theme(legend.position = "none")+
  theme(axis.title.x = element_blank())+
  theme(axis.title.y = element_blank())+
  ggtitle('Top three box Dia (%)')+
  coord_flip()
##Carrefour
carrefour_T2B <- list()
carrefour_T2B[1] <- as.data.frame(apply(carrefour_P,2, function(x) sum(x>=8 & x<=10)))
carrefour_T2B[2] <- as.data.frame(apply(carrefour_P,2, function(x) sum(x>=5 & x<=7)))
carrefour_T2B[3] <- as.data.frame(apply(carrefour_P,2, function(x) sum(x<=4)))
carrefour_T2B <- data.frame(carrefour_T2B)
dplyr::glimpse(carrefour_T2B)
#pasamos a porcentaje
carrefour_T2B <- as.data.frame(
  t(apply(carrefour_T2B,1,function(x) x/sum(x))))*100
dplyr::glimpse(carrefour_T2B)
#ponemos nombre a cada grupo
colnames(carrefour_T2B) <- c('Alto','Medio','Bajo')
#añadimos una columna con los nombres de las variables
carrefour_T2B$variables <- (names(carrefour_P))
carrefour_T2B$centro <- 'carrefour'
dplyr::glimpse(carrefour_T2B)
carrefour_T2B_melt <- melt(
  subset(carrefour_T2B, select=-centro), id.vars = 'variables')
dplyr::glimpse(carrefour_T2B_melt)
#grafico
ggplot(carrefour_T2B_melt, aes(variables, value, fill=variable))+
  geom_bar(position=position_stack(), stat="identity")+
  geom_text(aes(label=paste0(round(value,0),"%")),
            position=position_stack(vjust=0.5), size=3)+
  theme(legend.position="bottom", legend.direction = "horizontal")+
  theme(legend.title = element_blank())+
  theme(axis.title.x = element_blank())+
  theme(axis.title.y = element_blank())+
  guides(fill=guide_legend(reverse=TRUE))+
  ggtitle('Valoraciones Carrefour')+
  coord_flip()
##Mercadona
mercadona_T2B <- list()
mercadona_T2B[1] <- as.data.frame(
  apply(mercadona_P,2, function(x) sum(x>=8 & x<=10)))
mercadona_T2B[2] <- as.data.frame(
  apply(mercadona_P, 2, function(x) sum(x>=5 & x<=7)))
mercadona_T2B[3] <- as.data.frame(
  apply(mercadona_P,2,function(x) sum(x<=4)))
mercadona_T2B <- data.frame(mercadona_T2B)
dplyr::glimpse(mercadona_T2B)
#pasamos a porcentaje
mercadona_T2B <- as.data.frame(t(apply(mercadona_T2B,1,function(x) x/sum(x))))*100
dplyr::glimpse(mercadona_T2B)
#ponemos nombre a cada grupo
colnames(mercadona_T2B) <- c('Alto','Medio','Bajo')
#añadimos una columna con los nombres de las variables
dplyr::glimpse(mercadona_T2B)
mercadona_T2B$variables <- (names(mercadona_P))
mercadona_T2B$centro <- 'Mercadona'
mercadona_T2B_melt <- melt(
  subset(mercadona_T2B, select = -centro), id.vars='variables')
dplyr::glimpse(mercadona_T2B)
#grafico
ggplot(mercadona_T2B_melt, aes(variables, value, fill=variable))+
  geom_bar(position = position_stack(),stat="identity")+
  geom_text(aes(label=paste0(round(value,0),"%")),
            position=position_stack(vjust=0.5), size=3)+
  theme(legend.position = "bottom",legend.direction = "horizontal")+
  theme(legend.title = element_blank())+
  theme(axis.title.x = element_blank())+
  theme(axis.title.y = element_blank())+
  guides(fill=guide_legend(reverse=TRUE))+
  ggtitle('Valoraciones Mercadona')+
  coord_flip()

dia_T2B_melt <- reshape2::melt(dia_T2B, id.vars=c('variables', 'centro'))
dplyr::glimpse(dia_T2B_melt)
carrefour_T2B_melt <- reshape2::melt(carrefour_T2B, id.vars=c('variables', 'centro'))
dplyr::glimpse(carrefour_T2B_melt)
mercadona_T2B_melt <- reshape2::melt(mercadona_T2B, id.vars=c('variables', 'centro'))
dplyr::glimpse(mercadona_T2B_melt)
resumen_T2B_melt <- dplyr::bind_rows(dia_T2B_melt, carrefour_T2B_melt, mercadona_T2B_melt)
dplyr::glimpse(resumen_T2B_melt)
head(resumen_T2B_melt)
tail(resumen_T2B_melt)
#ordenar siguiendo el orden deseado
resumen_T2B_melt$centro <- factor(resumen_T2B_melt$centro, levels=c('dia','carrefour','mercadona'))
dplyr::glimpse(resumen_T2B_melt)
head(resumen_T2B_melt)

ggplot(resumen_T2B_melt, aes(variables, value, fill=variable))+
  geom_bar(position = position_stack(), stat="identity")+
  geom_text(aes(label=paste0(round(value,0),"%")),
            position=position_stack(vjust=0.5),size=3)+
  coord_flip()+
  facet_grid(. ~ centro)+
  theme(legend.position="bottom",legend.direction = "horizontal")+
  theme(legend.title = element_blank())+
  theme(axis.title.x = element_blank())+
  theme(axis.title.y = element_blank())+
  guides(fill=guide_legend(reverse=TRUE))+
  ggtitle('Comparativa valoraciones T3B y B3B')
head(dia_P)
#frecuencias absolutas del numero de respuestas
table(dia_P$Ph1.Satisfaccion)
dia_P_prop <- as.data.frame(prop.table(table(dia_P$Ph1.Satisfaccion)))
dia_P_prop
#veo que tipos de columnas tengo
dplyr::glimpse(dia_P_prop)
str(dia_P_prop)
#convertir este dato en numero entero
dia_P_prop$Var1 <- as.integer(dia_P_prop$Var1)
str(dia_P_prop)
dia_nps <- (sum(dia_P_prop$Freq[dia_P_prop$Var1>=9])
            -sum(dia_P_prop$Freq[dia_P_prop$Var1<=6]))
dia_nps
#-0.5577191
#quiere decir que existen mas detractores que promotores
dia_P_prop$color <- ifelse(dia_P_prop$Var1 <= 6, 'Detractor', ifelse(dia_P_prop$Var1<=8,'Pasivo','Promotor'))
dia_P_prop

ggplot(dia_P_prop,aes(as.factor(Var1), Freq, fill=color))+
         geom_bar(stat='identity')+
          geom_text(
            aes(x = Var1, y = Freq, label = paste0(round(Freq*100,2),'%.')), 
            hjust = +0.5, vjust=2.5,size = 4,
            position = position_dodge(width = 1),
            inherit.aes = TRUE
          )+
         scale_fill_manual(values=c(Detractor='red',
                                    Pasivo='yellow',Promotor='green'))+
         scale_y_continuous(labels=scales::percent)+
         theme(plot.title=element_text(size=12))+
         theme(legend.position="bottom", legend.title = element_blank())+
         labs(title=paste0('Dia NPS ', round(dia_nps*100,2),'%. Con Satisfacción Global'),
              x='Valoracion', y='Porcentaje')
#carrefour
#calculo de proporciones y NPS
carrefour_P_prop <- as.data.frame(prop.table(table(carrefour_P$Ph1.Satisfaccion)))
carrefour_P_prop$Var1 <- as.integer(carrefour_P_prop$Var1)
carrefour_nps <- (sum(carrefour_P_prop$Freq[carrefour_P_prop$Var1 >= 9])
                  -sum(carrefour_P_prop$Freq[carrefour_P_prop$Var1 <=6]))
carrefour_nps
carrefour_P_prop$color <- ifelse(carrefour_P_prop$Var1 <=6, 'Detractor', ifelse(carrefour_P_prop$Var1 <=8, 'Pasivo', 'Promotor'))
carrefour_P_prop
ggplot(carrefour_P_prop, aes(as.factor(Var1), Freq, fill=color))+
  geom_bar(stat='identity')+
  geom_text(
    aes(x = Var1, y = Freq, label = paste0(round(Freq*100,2),'%.')), 
    hjust = +0.5, vjust=-0.3,size = 4,
    position = position_dodge(width = 1),
    inherit.aes = TRUE
  )+
  scale_fill_manual(values=c(Detractor='red',
                             Pasivo='yellow', Promotor='green'))+
  scale_y_continuous(labels = scales::percent)+
  theme(legend.position = "bottom",legend.title = element_blank())+
  theme(plot.title = element_text(size=12))+
  labs(title=paste0('Carrefour NPS ',round(carrefour_nps*100,2),
                    '%. Con Satisfaccion Total'),
       x='Valoracion',y='Porcentaje')

##Mercadona
#Calculo de las proporciones y NPS
mercadona_P_prop <- as.data.frame(prop.table(table(mercadona_P$Ph1.Satisfaccion)))
mercadona_P_prop
str(mercadona_P_prop)
mercadona_P_prop$Var1 <- as.integer(mercadona_P_prop$Var1)
mercadona_nps <- (sum(mercadona_P_prop$Freq[mercadona_P_prop$Var1 >=9])
                  -sum(mercadona_P_prop$Freq[mercadona_P_prop$Var1 <= 6]))
mercadona_nps
#creacion de las categorias
mercadona_P_prop$color <- ifelse(mercadona_P_prop$Var1 <=6,'Detractor', ifelse(mercadona_P_prop$Var1 <=8,'Pasivo','Promotor'))
#Grafico
#aqui hay mas promotores que detractores
ggplot(mercadona_P_prop, aes(as.factor(Var1),Freq,fill=color))+
  geom_bar(stat='identity')+
  geom_text(
    aes(x = Var1, y = Freq, label = paste0(round(Freq*100,2),'%.')), 
    hjust = +0.5, vjust=-0.3,size = 4,
    position = position_dodge(width = 1),
    inherit.aes = TRUE
  )+
  scale_fill_manual(values=c(Detractor='red',
                             Pasivo='yellow',
                             Promotor='green'))+
  scale_y_continuous(labels=scales::percent)+
  theme(legend.position = "bottom", legend.title = element_blank())+
  theme(plot.title = element_text(size=12))+
  labs(title=paste0('Mercadona NPS ',round(mercadona_nps*100,2),
                    '%. Con Satisfaccion Global'),
       x='Valoracion',y='Porcentaje')

#grafico donde se unan los 3
dia_P_prop$centro <- paste0('Dia, NPS ', round(dia_nps*100,2),'%')
carrefour_P_prop$centro <- paste0('Carrefour, NPS ',round(carrefour_nps*100,2),'%')
mercadona_P_prop$centro <- paste0('Mercadona, NPS ',round(mercadona_nps*100,2),'%')
unique(dia_P_prop[c("centro")])
unique(carrefour_P_prop[c("centro")])
unique(mercadona_P_prop[c("centro")])
dia_P_prop
carrefour_P_prop
mercadona_P_prop

resumen_prop <- dplyr::bind_rows(dia_P_prop, carrefour_P_prop, mercadona_P_prop)
str(resumen_prop)
unique(resumen_prop[c("centro")])
resumen_prop$centro <- factor(resumen_prop$centro,
                              levels=c(paste0('Dia, NPS ', round(dia_nps*100,2), '%'),
                                       paste0('Carrefour, NPS ', round(carrefour_nps*100,2), '%'),
                                       paste0('Mercadona, NPS ',round(mercadona_nps*100,2),'%')))

str(resumen_prop)
unique(resumen_prop[c("centro")])
resumen_prop
ggplot(resumen_prop, aes(as.factor(Var1), Freq, fill=color))+
  geom_bar(stat='identity')+
  geom_text(
    aes(x = Var1, y = Freq, label = paste0(round(Freq*100,2),'%.')), 
    hjust = +0.5, vjust=+0.3,size = 3,
    position = position_dodge(width = 1),
    inherit.aes = TRUE
  )+
  facet_wrap(~ centro, ncol=1)+
  scale_fill_manual(
    values=c(Detractor='red',Pasivo='yellow',Promotor='green'))+
  scale_y_continuous(labels=scales::percent)+
  theme(legend.position = "bottom", legend.title = element_blank())+
  labs(x='Valoracion', y='Porcentaje')
