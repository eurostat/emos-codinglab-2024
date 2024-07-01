clrs<-read.csv("estat_colour_themes.txt",sep="\t",stringsAsFactors = F)
image(c(1:6),c(1:nrow(clrs)),matrix(1:(6*nrow(clrs)),6,nrow(clrs)),col=as.vector(t(apply(as.matrix(clrs[,c(3:8)]),2,rev))), xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty = "n",
      main = "Eurostat colour palettes")
text(c(0.5), c(1:nrow(clrs)), adj=c(0,1),rev(clrs$theme_desc), col = clrs$text_light)
text(c(1.5), c(1:nrow(clrs)), adj=c(0,1),rev(clrs$theme_desc), col = clrs$text_dark)
