library(maptools)
library(ggmap)
library(sp)
library(RColorBrewer)
library(foreign)
library(nlme)
library(xtable)
library(ggplot2)
library(gridExtra)
library(moments)
library(stringr)
library(haven)
theme_set(theme_gray(base_size = 10))

#library(gpclip)
#rm(list=ls())
#--------------------------
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

Karte <- readShapePoly("./MEX_adm/MEX_adm2.shp")
#unique(Karte@data$NAME_1)
Karte <- Karte[Karte@data$NAME_1 == "Guanajuato", ]
plot(Karte)
Karte@data$ID_2

Karte.fort = fortify(Karte, region = "ID_2")
Karte.fort = merge(Karte.fort, Karte@data, by.x="id", by.y ="ID_2")
Karte.centroids.f <- data.frame(long = coordinates(Karte)[, 1], 
                                lat = coordinates(Karte)[, 2], 
                                id= str_sub(as.character(Karte@data$ID_2), -3), 
                                dist= Karte@data$NAME_2) 

distri <- ggplot(Karte.fort, aes(long, lat, group = group, fill = id)) + 
  ggtitle("") + geom_polygon(colour = "azure3") + coord_equal() +
  theme(axis.ticks=element_blank(), axis.text = element_blank(), 
        legend.position="none") + geom_path(color="azure3") +
  labs(x = "", y = "") +
  geom_text(data = Karte.centroids.f, aes(label = id, x = long, y = lat, group = dist),
            size=3)
#ggsave(filename = "Municipalities.pdf",device = "pdf",width=12,height=8)
#+
 # scale_fill_manual("District", values = 
  #                    colorRampPalette(brewer.pal(9, "Set3"))(16), labels = dist)
rownames(Karte.centroids.f) <- NULL
print(xtable(Karte.centroids.f[,3:4]), booktabs = T, 
      include.rownames = F,
      hline.after=c(-1,1,nrow(dist)))

#run firstblood till line 164 first :)

opt_df <- data.frame(mun = names(optpars), parameter = optpars)

Karte <- readShapePoly("C:/Users/lexuanson/Desktop/Xuan/estee/MEX_adm/MEX_adm1.shp")
plot(Karte)

ord  <- match(Karte@data$NAME_1, opt_df$mun)
#Karte@data$NAME_1[is.na(ord)]
#[1] Coahuila  Michoacán Veracruz 
ord[is.na(ord)] <- c(5, 16, 30)

Karte@data[["full_name"]] <- opt_df$mun[ord] 
Karte@data[["opt_par"]] <- opt_df$parameter[ord]

Karte.fort=fortify(Karte, region = "ID_1")
Karte.fort=merge(Karte.fort,Karte@data,by.x="id", by.y ="ID_1")
library(scales)

opt_map <- ggplot(Karte.fort, aes(long, lat, group = group, fill = opt_par)) + 
  ggtitle("") + geom_polygon(colour = "azure3") + coord_equal() +
  theme(axis.ticks=element_blank(), axis.text = element_blank(), 
        legend.position="right") + 
  geom_path(color="azure3") +   labs(x = "", y = "")  +
  guides(fill=guide_legend(title="Optimal Parameter"))

opt_box <- qplot(x = as.factor(1), y = parameter, data = opt_df, geom=c("boxplot"),
                 fill = 1, main = "", xlab="", ylab="Optimal Parameter") + 
  theme(legend.position="none")

map_legend <- get_legend(opt_map)
opt_map <- opt_map +  theme(legend.position="none")

theme_set(theme_gray(base_size = 12))
opt_map
ggsave(filename = "box_map_opt.pdf", device = "pdf", width = 12, height = 6)
opt_box + scale_x_discrete(breaks = "")
ggsave(filename = "opt_box.pdf", device = "pdf", width = 3, height = 6)


sum <- summary(optpars)
sum_df <- as.data.frame(t(as.numeric(sum)))
colnames(sum_df) <- names(sum)
xtable(sum_df)

grid.arrange(opt_box, opt_map, map_legend, ncol = 3, 
             widths = c(1,18,2))


ggsave(filename = "box_map_opttot.pdf" , arrangeGrob(opt_box, opt_map, map_legend, ncol = 3, 
       widths = c(1,18,2), padding  = 0), device = "pdf",width = 14, height = 7)



ggsave(filename = "opt_par_map.pdf",device = "pdf",width=12,height=8)





ggsave(filename = "opt_par_box.pdf",device = "pdf",width=12,height=8)


#####################################################################################
#####################################################################################
#####################################################################################
#########
#########  EBP Karten
#########
#####################################################################################
#####################################################################################
#####################################################################################
# ########
load(file = "ebp16_full_inclPop.RData")
#load(file = "ebp56.RData")

ebp_data <- ebp16
#ebp_data <- ebp56


Karte <- readShapePoly("../MEX_adm/MEX_adm2.shp")
Karte <- Karte[Karte@data$NAME_1 == "Guerrero", ]
plot(Karte)
Karte@data$ID_2

ebp_data$POV$dom

base_2donivel <- read_dta("../base_2donivel_Nacional_2010.dta")

base_2donivel <- base_2donivel[(base_2donivel$nom_ent == "Guerrero"),]

matcher <- match(Karte@data$NAME_2, base_2donivel$nom_mun)
Karte@data$NAME_2[is.na(matcher)] 
matcher[is.na(matcher)] <- c(11, 28, 16, 38, 68) 
Karte@data[["ebp_id"]] <- base_2donivel$cve_mun[matcher]
matcher2 <- match(Karte@data[["ebp_id"]], as.character(ebp_data$POV$dom))
Karte@data[["HCR"]] <- ebp_data$POV$hcr[matcher2]
Karte@data[["QSR"]] <- ebp_data$POV$qsr64[matcher2]
Karte@data[["PGAP"]] <- ebp_data$POV$pgap[matcher2]

# [1] Atoyac de Alvarez  Chilapa de Alvarez     Erongarícuaro     
# [4] José Azueta        La Union    

Karte.fort=fortify(Karte, region = "ID_2")
Karte.fort=merge(Karte.fort,Karte@data,by.x="id", by.y ="ID_2")
library(scales)

map_hcr <- ggplot(Karte.fort, aes(long, lat, group = group, fill = HCR)) + 
  geom_polygon() + coord_equal() + labs(x = "", y = "", fill = "HCR") + 
  ggtitle("") + scale_fill_gradient2(low = "green", mid = "yellow", high = "red"
                       ,midpoint = median(Karte.fort$HCR)#,
                       # limits=range(c(Karte.fort[c("mean_non","mean_log","mean_box")]))
                       ) +
  theme(axis.ticks = element_blank(), axis.text = element_blank())
  
map_pg <- ggplot(Karte.fort, aes(long, lat, group = group, fill = PGAP)) + 
  geom_polygon() + coord_equal() + labs(x = "", y = "", fill = "PGAP") + 
  ggtitle("") + scale_fill_gradient2(low = "green", mid = "yellow", high = "red"
                       ,midpoint = median(Karte.fort$PG)#,
                       # limits=range(c(Karte.fort[c("mean_non","mean_log","mean_box")]))
  ) + theme(axis.ticks = element_blank(), axis.text = element_blank())

Karte.fort$QSR <- squish(Karte.fort$QSR, range = c(10, 60),only.finite = T)
map_qsr <- ggplot(Karte.fort, aes(long, lat, group = group, fill = QSR)) + 
  geom_polygon() + coord_equal() + labs(x = "", y = "", fill = "QSR") + 
  ggtitle("") + scale_fill_gradient2(low = "green", mid = "yellow", high = "red"
                       , midpoint = 35
                       , limits = range(c(10, 60))
  ) +
  theme(axis.ticks = element_blank(), axis.text = element_blank(), 
        legend.position = "right", legend.direction = "vertical")
map_qsr

mrg <- unit(c(0, 0, 0, -0.7), "cm")
grid.arrange(map_hcr + theme(plot.margin = mrg)
             , map_pg + theme(plot.margin = mrg)
             , map_qsr + theme(plot.margin = mrg)
             , ncol = 3, widths = c(4,4,4))



mrg <- unit(c(0, -5, 0, 0), "cm")
ggsave("ebp_map.pdf", arrangeGrob(
                                    map_hcr + theme(plot.margin = mrg)
                                  , map_pg + theme(plot.margin = mrg)
                                  , map_qsr + theme(plot.margin = mrg) ,                           
                              ncol = 3 #,
                              #widths = c(4,4,4), 
                              #heights = 4
                              ),
       width = 12, height = 2)

#########
#########

load("ebp16.RData")
load("ebp26.RData")
load("ebp46.RData")
load("ebp56.RData")
load("ebp96.RData")

la_lista <- list(non       = ebp26,
                 log       = ebp56,
                 log_shift = ebp16,
                 box_cox   = ebp46,
                 dual      = ebp96)
lapply(la_lista, function(x){ summary(x$POV$hcr) } )

lapply(la_lista, function(x){ summary(x$POV$pgap) } )

lapply(la_lista, function(x){ summary(x$POV$qsr) } )













