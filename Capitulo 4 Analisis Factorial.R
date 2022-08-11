library(dplyr)
library(ggplot2)
library(corrplot)
library(psych)
install.packages("psych")

file.choose()
survey <- read.delim("C:\\D\\data science\\Libros\\Analisis de datos con R\\Codigo R\\survey.csv", sep=';')
dia <- filter(survey, Establecimiento =='dia')
carrefour <- filter(survey,Establecimiento=='carrefour')
mercadona <- filter(survey, Establecimiento =='mercadona')
dia

#matriz de correlacion de toidos los establecimientos
color1 <- colorRampPalette(c("#7F0000","red","#FF7F00","yellow","white",
                           "cyan","#007FFF","blue","#00007F"))
corrplot(cor(dplyr::select(survey,starts_with('P'))),
         title="Matriz de correlacion",mar=c(0,0,1,0),
         method="color",outline = T,addgrid.col = "darkgray",
         order = "hclust",addrect = 8,col=color1(100),
         tl.col = 'black',tl.cex = .75)

survey_KMO <- psych::KMO(dplyr::select(survey, starts_with('P')))
str(survey_KMO)
survey_KMO$MSA

survey_principal <- psych::principal(dplyr::select(survey, starts_with('P')),
                                     nfactors=4, rotate='none')
survey_principal
print(survey_principal$loadings, digits=2)

#Creamos el data.frame con los valores de la comunalidad
survey_comunalidad <- data.frame(comunalidad=survey_principal$communality)
survey_comunalidad
#Con los nombres de las filas, que son las variables creamos 1 columna
survey_comunalidad$variables <-rownames(survey_comunalidad)
#Ordenamos de mayor a menor los valores
survey_comunalidad <- dplyr::arrange(survey_comunalidad, desc(comunalidad))
#Mostramos los valores
print(survey_comunalidad,digits = 2)
rownames(survey_comunalidad) <- 1:nrow(survey_comunalidad)
head(survey_comunalidad)

deportividad <- c(2,4,7,9,8)
comodidad <- c(10,6,7,5.5,3.5)
sin_rotar <- data.frame(x=deportividad, 
                        y=comodidad, modelo=c('A','B','C','D','E'))

ggplot(sin_rotar, aes(x,y))+
  geom_hline(yintercept = 1.5, colour="gray70")+
  geom_vline(xintercept=1.5,colour="gray70")+
  geom_point(fill=NA,alpha=0)+
  geom_text(aes(label=modelo))+
  geom_smooth(method=lm, se=FALSE)+
  labs(x='deportividad',y='comodidad')

deportividad_horizontal <- c(10,9.6,7,4,0)
comodidad_horizontal <- c(0.5,0.5,0.5,0.5,0.5)
rotado <- data.frame(x=deportividad_horizontal, y=comodidad_horizontal,
                     rot='rotado',modelo=c('D','E','C','B','A'))
ggplot(rotado, aes(x,y))+
  geom_hline(yintercept=0.45, colour="blue")+
  geom_point(fill=NA, alpha=0)+
  geom_text(aes(label=modelo))+
  theme(axis.line=element_blank(), axis.ticks = element_blank(),
        axis.text.x = element_blank(), axis.text.y = element_blank())+
  annotate("text",x=0.8,y=0.6, label="Comodidad")+
  annotate("text",x=9.0,y=0.6,label="Deportividad")+
  scale_y_continuous(limit=c(0.25,0.75))+
  labs(x='',y='')

survey_principal_rot <- psych::principal(dplyr::select(survey,starts_with('P')),
                                         nfactors = 4,rotate = 'varimax')
print(survey_principal_rot$loadings,cutoff = 0.4,digits = 2)

#Representacion Grafica del analisis factrorial
survey_principal_cargas_rot <- data.frame(unclass(survey_principal_rot$loadings))
survey_principal_cargas_rot$variables <- rownames(survey_principal_cargas_rot)
rownames(survey_principal_cargas_rot) <- NULL
survey_principal_cargas_rot <- dplyr::select(survey_principal_cargas_rot,variables,starts_with('R'))
print(head(survey_principal_cargas_rot), digits = 2)
survey_principal_scores_rot <- data.frame(unclass(survey_principal_rot$scores))
survey_principal_scores_rot
survey_principal_scores_rot$Establecimiento <- survey$Establecimiento
survey_principal_scores_rot

ggplot(data=survey_principal_scores_rot,aes(RC1,RC2))+
  geom_hline(yintercept=0,colour="gray70")+
  geom_vline(xintercept=0,colour="gray70")+
  geom_point(aes(colour=factor(Establecimiento)),alpha=0.4)+
  geom_density2d(colour="gray80")+
  theme(legend.position="bottom",legend.direction="horizontal")+
  theme(legend.title = element_blank())+
  labs(title = "Analisis factorial por centros",
       x='Actitud de los empleados', y='Establecimiento')+
  scale_colour_discrete(name="Variable")

##Creamos variables temporales
#Agregamos las columnas a la nueva variable, pero dandoles nombre x o y
a <- dplyr::select(survey_principal_scores_rot, x=RC1, y=RC2, Establecimiento)
a$factor <- 'Empleados y Establecimiento'
b <- dplyr::select(survey_principal_scores_rot, x=RC1, y=RC3, Establecimiento)
b$factor <- 'Empleados y Tiempos'
c <- dplyr::select(survey_principal_scores_rot, x=RC1, y=RC4, Establecimiento)
c
c$factor <- 'Empleados y Gama Productos'
d <- dplyr::bind_rows(a,b,c)
ggplot(data = d, aes(x,y))+
  geom_hline(yintercept=0,colour="gray70")+
  geom_vline(xintercept = 0,colour="gray70")+
  geom_point(aes(colour=factor(Establecimiento)),alpha=0.4)+
  geom_density2d(colour="gray80")+
  facet_grid(~ factor)+
  theme(legend.position = "bottom",legend.direction = "horizontal")+
  theme(legend.title = element_blank())+
  labs(title = "Analisis factorial por centros",x='',y='')+
  scale_colour_discrete(name="Variable")
