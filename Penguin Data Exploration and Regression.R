library(readr)
library(ggplot2)
library(plotly)


#Getting the current dir and setting the working dir
dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dir)

penguins_size <- read.csv("penguins_size.csv")
View(penguins_size)

cor(penguins_size[3:6], use="complete.obs")
summary(penguins_size)

table(penguins_size$sex)

ggplotly(
  ggplot(data=penguins_size) + 
    geom_point(mapping=aes(x=body_mass_g, y=culmen_length_mm, color=species)))
#The correlation is different for looking at just the species, vs penguins as a whole

chinstrap <- penguins_size[penguins_size$species=="Chinstrap",]
cor(chinstrap[3:6], use="complete.obs")

#The correlation in the entire dataset is positive, 
# but some species have a negative correlation when you go further into looking into the data

# Linear model function

gentoo <- penguins_size[penguins_size$species=="Gentoo",]

lm(culmen_depth_mm ~ body_mass_g, data=gentoo)
ggplotly(
  ggplot(data=penguins_size) +
    geom_point(mapping=aes(x=body_mass_g, y=culmen_depth_mm, color=species)) +
    geom_abline(intercept=7.8775, slope =0.0014))

#Creating the linear model
bodymass_to_depth <-lm(formula= culmen_depth_mm ~ body_mass_g, data=gentoo)

#Getting the summary stats from the model
summary(bodymass_to_depth)

#You can use predict to fill in missing data
#predict(bodymass_to_depth,gentoo)

# We can save this to get a look at the difference between the prediciton and the real data
gentoo$predicted_depth<-predict(bodymass_to_depth,gentoo)
View(gentoo)

#Multiple Regression with more than one variable
cor(gentoo[3:6], use="complete.obs")
#adding flipper length may add better performance from the model
bodymass_and_length_to_depth <-lm(formula= culmen_depth_mm ~ body_mass_g + flipper_length_mm, data=gentoo)
summary(bodymass_and_length_to_depth)
gentoo$predicted_depth_2<-predict(bodymass_and_length_to_depth,gentoo)
View(gentoo)

#Showing the data in a 3D space
plot_ly(data=gentoo, x=gentoo$body_mass_g,y=gentoo$culmen_length_mm,
        z=gentoo$culmen_depth_mm,mode="markers",type="scatter3d", color=gentoo$culmen_depth_mm)
#Showing how our predictions lie on a 3D plane
plot_ly(data=gentoo, x=gentoo$body_mass_g,y=gentoo$culmen_length_mm,
        z=gentoo$predicted_depth_2,mode="markers",type="scatter3d", color=gentoo$culmen_depth_mm)

