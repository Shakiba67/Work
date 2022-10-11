
install.packages("reshape2")

install.packages("ggplot2")

setwd("/Users/shakiba/Desktop/Covid-19")

#F1_male=read.csv("sixmnts_F0.csv")
F1_male = read.csv("sixmnts_F2.csv", fileEncoding = "Latin1", check.names = F)
#F1_male=read.csv("sixmnts_F0.csv",fileEncoding = "UCS-2LE")

#F1_male= read.csv("sixmnts_F2.csv",check.names = F)

write.csv (F1_male, "/Users/shakiba/Desktop/Covid-19/F1_latin.csv")




boys_bfff<-F1_male[,c('Acetaminophen sulfate','2-hydroxyacetaminophen sulfate','Acetaminophen glucuronide','N-acetylcysteine conjugate','Acetaminophen','Cysteine conjugate')]

boys_bfff<-F1_male[,c('1.15_231.0197n','1.07_247.0146n','7.41_327.0946n','5.11_313.0846m/z','1.15_152.0705m/z','6.97_271.0741m/z')]



#Correlation matrix can be created using the R function cor()

corr <- cor(boys_bfff, use="complete.obs", method="spearman")


corr_boys_bfff<-round(corr, 2)


head(corr_boys_bfff)




#corrplot(corr_boys_bfff, diag = FALSE, order = "FPC", tl.pos = "td", tl.cex = 0.5, method = "color", type = "upper")

#corrplot(corr_boys_bfff, type = "upper")



#melt and Create the correlation heatmap with ggplot2

library(reshape2)


melted_corr_boys_bfff <- melt(corr_boys_bfff)

head(melted_corr_boys_bfff)



library(ggplot2)

ggplot(data = melted_corr_boys_bfff, aes(x=Var1, y=Var2, fill=value)) +
  
  geom_tile()





# Get lower triangle of the correlation matrix

get_lower_tri_boysbfff<-function(corr_boys_bfff){
  
  corr_boys_bfff[upper.tri(corr_boys_bfff)] <- NA
  
  return(corr_boys_bfff)
  
}

# Get upper triangle of the correlation matrix

get_upper_tri_boysbfff <- function(corr_boys_bfff){
  
  corr_boys_bfff[lower.tri(corr_boys_bfff)]<- NA
  
  return(corr_boys_bfff)
  
}

upper_tri_boys_bfff<- get_upper_tri_boysbfff(corr_boys_bfff)

upper_tri_boys_bfff



#Finished correlation matrix heatmap

library(reshape2)

melted_corr_boys_bfff <- melt(upper_tri_boys_bfff, na.rm = TRUE)

# Heatmap

library(ggplot2)

ggheatmap_boysbfff<-ggplot(data = melted_corr_boys_bfff, aes(Var2, Var1, fill = value))+
  
  geom_tile(color = "white")+
  
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       
                       name="Spearman\nCorrelation") +
  
  theme_minimal()+
  
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   
                                   size = 12, hjust = 1))+
  
  coord_fixed()





ggheatmap_boysbfff +
  
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 5) +
  
  theme(
    
    axis.title.x = element_blank(),
    
    axis.title.y = element_blank(),
    
    panel.grid.major = element_blank(),
    
    panel.border = element_blank(),
    
    panel.background = element_blank(),
    
    axis.ticks = element_blank(),
    
    legend.justification = c(1, 0),
    
    legend.position = c(0.6, 0.7),
    
    legend.direction = "horizontal")+
  
  guides(fill = guide_colorbar(barwidth = 5, barheight = 1,
                               
                               title.position = "top", title.hjust = 0.5))
