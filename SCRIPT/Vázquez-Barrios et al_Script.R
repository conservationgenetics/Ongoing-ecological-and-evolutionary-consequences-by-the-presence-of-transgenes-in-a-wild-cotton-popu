###Escritorio de trabajo

#### Libraries 

library(multcomp)
library(dplyr)
library ( ggplot2 )
library("wesanderson")

#### Data 

setwd("C:/Users/Valeria Vázquez/Desktop/ARTICLE/Scientific Reports LV/EFN Treatment/")

#### Effect of trangenic on EFN protuction in each genotypes ####

#### Wild

#### Tratamiento + Fecha

Wild<-read.csv("Wild_EFN_FV.csv")
names(Wild)


WildAllVar<- glm(Wild$Volumen.en.ug~Treatment, data = Wild, family = "quasipoisson")
summary(WildAllVar)
anova(WildAllVar, test= "Chi") # test anova  glht(my.glm, mcp(Temperature="Tukey"
Tukey_glmw<- glht(WildAllVar, mcp(Treatment= "Tukey")) # Only Treatment is significative
summary(Tukey_glmw) # summary glht
cld(Tukey_glmw, Letters=letters) #Differents letters


meW<-with(Wild,tapply(Wild$Volumen.en.ug,Treatment,mean)) ### mean of Trangenic variable
meW
SEW<-with(Wild,tapply(Wild$Volumen.en.ug,Treatment,sd)/sqrt(summary(Treatment)))
SEW
SDW<-with(Wild,tapply(Wild$Volumen.en.ug,Treatment,sd))
SDW

#### Cry 

Cry<-read.csv("Cry_EFN_FV.csv")

#### Interaction Treatment*Date

CryTra<- glm(Cry$Volumen.en.ug~Treatment, data = Cry, family = "quasipoisson")
summary(CryTra)
anova(CryTra, test= "Chi") # test anova
summary(CryTra) # summary

meCry<-with(Cry,tapply(Cry$Volumen.en.ug,Treatment,mean)) ### mean of Trangenic variable
meCry
SECry<-with(Cry,tapply(Cry$Volumen.en.ug,Treatment,sd)/sqrt(summary(Treatment)))
SECry


#### Cp4-epsps

Cp4<-read.csv("Cp4-epsps_EFN_FV.csv")

#### Interactions Treatment*Date and individual effect

Cp4Int<- glm(Cp4$Volumen.of.EFN~Treatment,data = Cp4, family = "quasipoisson")
summary(Cp4Int)
anova(Cp4Int, test= "Chi") # test anova
summary(Cp4Int) # summary

meCp4<-with(Cp4,tapply(Cp4$Volumen.of.EFN,Treatment,mean)) ### mean of Trangenic variable
meCp4
SECp4<-with(Cp4,tapply(Cp4$Volumen.of.EFN,Treatment,sd)/sqrt(summary(Treatment)))
SECp4

#### cry control and negative induction treatment ####

CN<-read.csv("cry_wild_comparative_IT_FV.csv")

#### Interaction Treatment Induction wild and Control cry 

CNI<- glm(CN$EFN.volume.ug~CN$Treatment, data = CN, family = "quasipoisson")
summary(CNI)
anova(CNI, test= "Chi") # test anova
summary(CNI) # summary

#### Colours

# Crear paleta apta para daltónicos:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


##### Nectar treatment figure 

nt<-data.frame(
  "Transgene"= c("S", "SCry", "SCp4-epsps", "S", "SCry", "SCp4-epsps"),
  "Treatment"= c("Control", "Control", "Control", "MeJa","MeJa","MeJa"),
  "Mean"=c(9.147619,19.910223,5.612791,26.078571,30.36818,7.195349),
  "SE"=c(1.94124,6.014835,1.420689,12.21541,13.695886,1.17479)
)




#BARPLOT error bar
as.factor(nt$Transgene)
nt$Transgene = factor(nt$Transgene, levels=c("S", "SCry","SCp4-epsps"))
levels(nt$Transgene)

jpeg("Nectar_treatment_FV.jpeg", width = 15, height = 10,
     units = "cm", res = 300, pointsize = 0.25)



p1<-ggplot(nt, aes(nt$Transgene, Mean, fill=Treatment)) +
  scale_fill_manual(values = cbPalette)+
  geom_bar(stat = "identity", position = "dodge")+
  geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), position = position_dodge(0.9), width=0.2)+
  labs(x = "Genotypes",y = "Extrafloral nectar (µg µL-1)")+theme_classic()

p2<-p1+  theme(axis.title.x = element_text(size=18, colour="black"))+
  theme(axis.title.y = element_text(size=18, colour = "black")) + 
  theme(axis.text.x = element_text(size=15, colour = "black"))+ 
  theme(axis.text.y = element_text(size=15, colour = "black")) +  
  theme(legend.position = "right")+
  theme(panel.background=element_rect(fill="transparent",colour=NA),
        plot.background=element_rect(fill="transparent",colour=NA),
        legend.key = element_rect(fill = "transparent", colour = "transparent"))+
  annotate("text", x=c(1,2,3), y=c(43,50,15), label= c("*","n.s","n.s"),size=c(13,6,6))

p2

dev.off()

#### Herbivory deskbook ####

#### Libraries 

library(multcomp)
library(dplyr)
library ( ggplot2 )

#### Data
setwd("C:/Users/Valeria Vázquez/Desktop/ARTICLE/Scientific Reports LV/Herbivory/")

H<-read.csv("Herbivory_FV.csv")

her<- glm(Herbivory~Genotype, data = H, family = "quasipoisson")
summary(her)
anova(her, test= "Chi") # test anova
summary(her) # summary
Tukey_glm<- glht(her, linfct = mcp(Genotype= "Tukey")) 
summary(Tukey_glm) # summary glht
cld(Tukey_glm, Letters=letters) #Differents letters
plot(Tukey_glm) # plot glht


###media y error estandar

H

mediaher<-with(H,tapply(H$Herbivory,Genotype,mean)) ### mean of Trangenic variable
mediaher
SEher<-with(H,tapply(H$Herbivory,Genotype,sd)/sqrt(summary(Genotype)))
SEher


hb<-data.frame(
  "Genotype"= c("W", "Wcry", "Wcp4-epsps"),
  "Mean"=c(2.368357,1.491786,4.402077),
  "SE"=c(0.4112395,0.3051965,0.8637594))

hb

#BARPLOT error bar
as.factor(hb$Transgene)
hb$Transgene = factor(hb$Genotype, levels=c("W", "Wcry","Wcp4-epsps"))
levels(hb$Transgene)

jpeg("Herbivory_damage_FV.jpeg", width = 15, height = 10,
     units = "cm", res = 300, pointsize = 0.25)


hb1<-ggplot(hb, aes(hb$Genotype, Mean, fill=Genotype))+
  scale_fill_manual(values = cbPalette)+
  geom_bar(stat = "identity", position = "dodge")+
  geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), position = position_dodge(0.9), width=0.2)+
  labs(x = "Genotypes",y = "Herbivory (%) ± ESM")+theme_classic()
hb1

hb2<-hb1+  theme(axis.title.x = element_text(size=18, colour="black"))+
  theme(axis.title.y = element_text(size=18, colour = "black")) + 
  theme(axis.text.x = element_text(size=15, colour = "black"))+ 
  theme(axis.text.y = element_text(size=15, colour = "black")) +  
  theme(legend.position = "right")+
  theme(panel.background=element_rect(fill="transparent",colour=NA),
        plot.background=element_rect(fill="transparent",colour=NA),
        legend.key = element_rect(fill = "transparent", colour = "transparent"))+
  annotate("text", x=c(1,2,3), y=c(3.5,6,2.5), label= c("b","a","b"),size=c(6,6,6))

hb2
dev.off()

##### Abundance of ants ####

setwd("C:/Users/Valeria Vázquez/Desktop/ARTICLE/Scientific Reports LV/ANTS/")

Aw<-read.csv("cry_ants_FV.csv")

Awild<- glm(Aw$Abundance_mean~Aw$Treatment, data = Aw, family = "quasipoisson")
summary(Awild)
anova(Awild, test= "Chi") # test anova
summary(Awild) # summary

Acry<-read.csv("cry_ants_FV.csv")

Ac<- glm(Acry$Abundance_mean~Acry$Treatment, data = Acry, family = "quasipoisson")
summary(Ac)
anova(Ac, test= "Chi") # test anova
summary(Ac) # summary

Acp<-read.csv("cp4-epsps_ants_FV.csv")

Acp4<- glm(Acp$Abundance_mean~Acp$Treatment, data = Acp, family = "quasipoisson")
summary(Acp4)
anova(Acp4, test= "Chi") # test anova
summary(Acp4) # summary


### Ants abundance figure

ab<-data.frame(
  "Transgene"= c("W", "WCry", "WCp4-epsps","W", "WCry", "WCp4-epsps"),
  "Treatment"=c("Control","Control","Control","MeJa","MeJa","MeJa"),
  "Mean"=c(0.7142857,0.2857143,0.4642857,1,0.9642857,0.5714286),
  "SE"=c(0.3247186,0.0583212,0.0683877,0.1010153,0.15838256,0.21028))
ab

#BARPLOT error bar
as.factor(ab$Transgene)
ab$Transgene = factor(ab$Transgene, levels=c("W", "WCry","WCp4-epsps"))
levels(ab$Transgene)

jpeg("ants_abundance_FV.jpeg", width = 15, height = 10,
     units = "cm", res = 300, pointsize = 0.25)



ab1<-ggplot(ab, aes(ab$Transgene, Mean, fill=Treatment))+
  scale_fill_manual(values =cbPalette)+
  geom_bar(stat = "identity", position = "dodge")+
  geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), position = position_dodge(0.9), width=0.2)+
  labs(x = "Genotypes",y = "Number of ants ± SE")+theme_classic()

ab1

ab2<-ab1+ theme(axis.title.x = element_text(size=18, colour="black"))+
  theme(axis.title.y = element_text(size=18, colour = "black")) + 
  theme(axis.text.x = element_text(size=15, colour = "black"))+ 
  theme(axis.text.y = element_text(size=15, colour = "black")) +  
  theme(legend.position = "right")+
  theme(panel.background=element_rect(fill="transparent",colour=NA),
        plot.background=element_rect(fill="transparent",colour=NA),
        legend.key = element_rect(fill = "transparent", colour = "transparent"))+
  annotate("text", x=c(1,2,3), y=c(1.25,1.2,1), label= c("n.s","***","n.s"),size=c(6,13,6))

ab2
dev.off()

##### Leaf age ####

setwd("C:/Users/Valeria Vázquez/Desktop/ARTICLE/Scientific Reports LV/Leaf age/")


#### Wild ####

# wild control

agwc<-read.csv("leaf.age_control_wild.csv")

agwc$Leaf<- as.factor(agwc$Leaf)

Agwc<- glm(agwc$Volumen.of.EFN.in.ug.mean~Leaf, data = agwc, family = "quasipoisson")
summary(Agwc)
anova(Agwc, test= "Chi") # test anova  glht(my.glm, mcp(Temperature="Tukey"
Tukey_glmw<- glht(Agwc, mcp(Leaf= "Tukey")) # Only Treatment is significative
summary(Tukey_glmw) # summary glht
cld(Tukey_glmw, Letters=letters) #Differents letters

# wild induction 

agwi<-read.csv("leaf.age_induction_wild.csv")

agwi$Leaf<- as.factor(agwc$Leaf)

Agwi<- glm(agwi$Volumen.of.EFN.in.ug.mean~Leaf, data = agwi, family = "quasipoisson")
summary(Agwi)
anova(Agwi, test= "Chi") # test anova  glht(my.glm, mcp(Temperature="Tukey"
Tukey_glmw<- glht(Agwc, mcp(Leaf= "Tukey")) # Only Treatment is significative
summary(Tukey_glmw) # summary glht
cld(Tukey_glmw, Letters=letters) #Differents letters

#### cry ####

# cry control

agcc<-read.csv("leaf.age_control_cry.csv")

agcc$Leaf<- as.factor(agcc$Leaf)

Agcc<- glm(agcc$Volumen.of.EFN.in.ug.mean~Leaf, data = agcc, family = "quasipoisson")
summary(Agcc)
anova(Agcc, test= "Chi") # test anova  glht(my.glm, mcp(Temperature="Tukey"
Tukey_glmw<- glht(Agcc, mcp(Leaf= "Tukey")) # Only Treatment is significative
summary(Tukey_glmw) # summary glht
cld(Tukey_glmw, Letters=letters) #Differents letters

# wild induction 

agci<-read.csv("leaf.age_induction_cry.csv")

agci$Leaf<- as.factor(agci$Leaf)

Agci<- glm(agci$Volumen.of.EFN.in.ug.mean~Leaf, data = agci, family = "quasipoisson")
summary(Agci)
anova(Agci, test= "Chi") # test anova  glht(my.glm, mcp(Temperature="Tukey"
Tukey_glmw<- glht(Agci, mcp(Leaf= "Tukey")) # Only Treatment is significative
summary(Tukey_glmw) # summary glht
cld(Tukey_glmw, Letters=letters) #Differents letters


####cp4.epsps ####

#cp4.epsps control

agcp4c<-read.csv("leaf.age_control_cp4epsps.csv")
agcp4c$Leaf<- as.factor(agcp4c$Leaf)

Agcp4c<- glm(agcp4c$Volumen.of.EFN.in.ug.mean~Leaf, data = agcp4c, family = "quasipoisson")
summary(Agcp4c)
anova(Agcp4c, test= "Chi") # test anova  glht(my.glm, mcp(Temperature="Tukey"
Tukey_glmw<- glht(Agcp4c, mcp(Leaf= "Tukey")) # Only Treatment is significative
summary(Tukey_glmw) # summary glht
cld(Tukey_glmw, Letters=letters) #Differents letters

#cp4.epsps induction

agcp4i<-read.csv("leaf.age_induction_cp4epsps.csv")
ag
agcp4i$Leaf<- as.factor(agcp4i$Leaf)

Agcp4i<- glm(agcp4i$Volumen.of.EFN.in.ug.mean~Leaf, data = agcp4i, family = "quasipoisson")
summary(Agcp4i)
anova(Agcp4i, test= "Chi") # test anova  glht(my.glm, mcp(Temperature="Tukey"
Tukey_glmw<- glht(Agcp4i, mcp(Leaf= "Tukey")) # Only Treatment is significative
summary(Tukey_glmw) # summary glht
cld(Tukey_glmw, Letters=letters) #Differents letters



####### END ######

