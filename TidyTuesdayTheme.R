library("ggplot2")
library("ggthemes")
library(ggridges)
library(extrafont)

TidyTues_theme<- function () { 
  ggpubr::theme_pubr(base_size=15, base_family="Avenir") %+replace% 
    theme(strip.background = element_blank(), 
          legend.position = "right",
          legend.key = element_blank())
  # 
  # theme_bw(base_size=13, base_family="Avenir") %+replace% 
  #   theme(
  #     panel.background  = element_blank(),
  #     plot.background = element_rect(fill="gray96", colour=NA), 
  #     legend.background = element_rect(fill="transparent", colour=NA),
  #     legend.key = element_rect(fill="transparent", colour=NA)
  #   )
}


TidyTues_Divergent_Palette <- c("#f72585","#b5179e","#7209b7","#560bad","#480ca8","#3a0ca3","#3f37c9","#4361ee","#4895ef","#4cc9f0")
TidyTues_Qualitative_Palette <- c("#ff9494","#ffc38a","#fdf3a0","#bdf3a5","#90e6ef","#96adef","#b49cf2","#ffacf4","#CABFA3")
TidyTues_Sequential_Palette <- c("#f94144","#f3722c","#f8961e","#f9844a","#f9c74f","#90be6d","#43aa8b","#4d908e","#577590","#277da1")




ttues_palettes<-list(
  Divergent=TidyTues_Divergent_Palette,
  Qualitative=TidyTues_Qualitative_Palette,
  Sequential=TidyTues_Sequential_Palette
)

ttues_pal<-function (palette = "Qualitative", reverse = FALSE, ...) 
{
  pal <- ttues_palettes[[palette]]
  if (reverse) {
    pal <- rev(pal)
  }
  grDevices::colorRampPalette(pal,interpolate = "spline",space = "Lab", ...)
}

scale_color_tidyTues<-function (palette = "Qualitative", discrete = TRUE, reverse = FALSE, 
                              ...) 
{
  pal <- ttues_pal(palette = palette, reverse = reverse)
  if (discrete) {
    ggplot2::discrete_scale("colour", paste0("ttues_", palette), 
                            palette = pal, ...)
  }
  else {
    ggplot2::scale_color_gradientn(colours = pal(256), ...)
  }
}

scale_fill_tidyTues<-function (palette = "Qualitative", discrete = TRUE, reverse = FALSE, 
                             ...) 
{
  pal <- ttues_pal(palette = palette, reverse = reverse)
  if (discrete) {
    ggplot2::discrete_scale("fill", paste0("ttues_", palette), 
                            palette = pal, ...)
  }
  else {
    ggplot2::scale_fill_gradientn(colours = pal(256), ...)
  }
}

theme_set(TidyTues_theme())
