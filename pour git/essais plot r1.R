library(ggplot2)
ggplot(data = tresetrevpaca,aes (x = med, y = X..Voix.Exp))+
geom_point()+
labs(
  title = "Une tendence qui se",
  subtitle = "Pourcentage de voix exprimees pour Emmanuel Marcon en fonction du revenu median en 2022 en region PACA",
  x = "revenu median",
  y= "Pourcentage de voix exprimees pour Emmanuel Macron",
  caption = "Source : Ministere de l'interieur et Insee") + theme_bw() + theme(
    plot.title = element_text( face = "bold"),
    panel.grid.major = element_line(color = "grey", linetype = "dashed"),
    panel.grid.minor = element_blank())

ggplot(data = tresetrevpaca,aes (x = med, y = X..Voix.Exp...25))+
  geom_point()+
labs(
  title = "Les tres hauts revenus votent pour Emmanuel Macron",
  subtitle = "Pourcentage de voix exprimees pour Emmanuel Marcon en fonction du revenu median en 2017 en region PACA",
  x = "revenu median",
  y= "Pourcentage de voix exprimees pour Emanuel Macron",
  caption = "Source : Ministere de l'interieur et Insee") + theme_bw() + theme(
    plot.title = element_text( face = "bold"),
    panel.grid.major = element_line(color = "grey", linetype = "dashed"),
    panel.grid.minor = element_blank())


ggplot(data = tresetrevpaca,aes (x = med, y = (..count..)/sum(..count..)))+
  geom_histogram(color="white", fill = "black")+
  labs(
    title = "Une region avec moins de valeurs extreme ",
    subtitle = "histogramme du revenu median en region PACA",
    x = "revenu median",
    y= "Frequence",
    caption = "Source :  Insee") + theme_bw() + theme(
      plot.title = element_text( face = "bold"),
      panel.grid.major = element_line(color = "grey", linetype = "dashed"),
      panel.grid.minor = element_blank())+
  scale_x_continuous(limits = c(10000, 60000))


ggplot(data = revenu,aes (x = med, y = (..count..)/sum(..count..)))+
  geom_histogram(color="white", fill = "black")+
  labs(
    title = "De fortes inegalites au niveau du revenu en France",
    subtitle = "histogramme du revenu median en France",
    x = "revenu median",
    y= "Frequence",
    caption = "Source :  Insee") + theme_bw() + theme(
      plot.title = element_text( face = "bold"),
      panel.grid.major = element_line(color = "grey", linetype = "dashed"),
      panel.grid.minor = element_blank())+
  scale_x_continuous(limits = c(10000, 60000))



ggplot(data = tresetrevFR,aes (x = med, y = tresetrevFR[,30]))+
  geom_point()+
  labs(
    title = "Les tres hauts revenus votent pour Emanuel Macron",
    subtitle = "Pourcentage de voix exprimees pour Emanuel Marcon en fonction du revenu median en 2022 en France",
    x = "revenu median",
    y= "Pourcentage de voix exprimees pour Emanuel Macron",
    caption = "Source : Ministere de l'interieur et Insee") + theme_bw() + theme(
      plot.title = element_text( face = "bold"),
      panel.grid.major = element_line(color = "grey", linetype = "dashed"),
      panel.grid.minor = element_blank())

ggplot()+
  geom_point(data = tresetrevFR,aes (x = med, y = tresetrevFR[,31], color = "France"), alpha = 0.5)+
  geom_point(data = tresetrevpaca,aes (x = med, y = X..Voix.Exp, color = "PACA"), alpha = 0.8)+
  labs(
    title = "Le vote pour M. Macron en baisse",
    subtitle = "Pourcentage de voix exprimees en fonction du revenu median en 2022 en France",
    x = "revenu median",
    y= "Pourcentage de voix exprimees",
    caption = "Source : Ministere de l'interieur et Insee") + 
  theme_bw() + 
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "grey", linetype = "dashed"),
    panel.grid.minor = element_blank()) +
  scale_color_manual(values = c("France" = "grey", "PACA" = "black"))


ggplot()+
  geom_point(data = tresetrevFR,aes (x = med, y = tresetrevFR[,30], color = "France"), alpha = 0.5)+
  geom_point(data = tresetrevpaca,aes (x = med, y = X..Voix.Exp...25, color = "PACA"), alpha = 0.8)+
  labs(
    title = "la region PACA, une France miniature",
    subtitle = "Pourcentage de voix exprimees en fonction du revenu median en 2017 en France",
    x = "revenu median",
    y= "Pourcentage de voix exprimees",
    caption = "Source : Ministere de l'interieur et Insee") + 
  theme_bw() + 
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "grey", linetype = "dashed"),
    panel.grid.minor = element_blank()) +
  scale_color_manual(values = c("France" = "grey", "PACA" = "black"))

ggplot(data = tresetrevpaca,aes (x = med, y = X..Abs.Ins))+geom_point()
ggplot(data = tresetrevpaca,aes (x = X..Voix.Ins, y = X..Abs.Ins, color = med))+geom_point()
ggplot(data = tresetrevpaca,aes (x = X..Voix.Exp, y = X..Abs.Ins, color = med))+geom_point()
?scale_alpha_discrete
