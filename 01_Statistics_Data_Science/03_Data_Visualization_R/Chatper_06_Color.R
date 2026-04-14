setwd("/Users/ceci_3110/visualization")
getwd()

# 1. Enter the color name directly as text
library(scales)
show_col(c("red"))
show_col(c("red", "orange", "yellow", "green"))

# 2. hex code
show_col(c("#FF0000", "#FFFF00", "#00FF00",
           "#FF00FF", "#FFFFFF", "#00FFFF", 
           "#0000FF", "#000000", "#F0F0F0"))

# 3. rgb code
show_col(c(rgb(0,0,1), rgb(0, 0, 0.5), 
           rgb(0, 0, 0.3), rgb(0, 0, 0)))

#4. by number (1~8)
show_col(1:9)


# 2) colormap 
# grDevices package - colormap function ~ rainbow(n), heat.colors(n), topo.color(n)
show_col(rainbow(10), ncol=1, cex_label = 0.7, borders = NA)

#RColorBrewer package 
install.packages("RColorBrewer")
library(RColorBrewer)
display.brewer.all()
# top: sequential colormap, mid: qualitative colormap, bottom: diverging colormap 
brewer.pal(7, "BuPu")
display.brewer.pal(7, "BuPu")

#scico package 
# Easy to recognize for those who are colorblind or have color weaknesses. 
# It also displays well in black and white printing (24 color maps).
install.packages("scico")
library(scico)
scico_palette_show()
scico(7, palette = "acton")
show_col(scico(7, palette = "acton"))

#viridis colormap - ggplot2 


# 3) How to use colormaps in ggplot2
# **use scale() function**
# -> scale_color_XXXX : dot or line 
# -> scale_fill_XXXX : face

# Categorical Variables
# Directly Enter Color Names : manual
# RColorBrewer's color map : brewer

# Continuous Variables
# Directly Enter Color Names : gradient 
# RColorBrewer's color map : distiller
# viridis's color map : viridis
# scico's color map : scico 

setwd("/Users/ceci_3110/visualization")
load("sah.RData")
library(ggplot2)
ggplot(sah, aes(age, ldl, color=BMI.cat)) + geom_point()

# scale_color_manual(values=...)
ggplot(sah, aes(age, ldl, color=BMI.cat)) + geom_point() + 
  scale_color_manual(values = c(c("tomato", "orange", "skyblue", "forestgreen")))

# scale_color_brewer
ggplot(sah, aes(age, ldl, color=BMI.cat)) + geom_point() + 
  scale_color_brewer(palette="Set2")

#
