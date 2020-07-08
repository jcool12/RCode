setwd("directory/path")
sentimentposts <- read.csv("sentimentposts.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

sentimentposts$Datetime <- dmy_hm(sentimentposts$Datetime)

## 1st change point graph for Negative
r7 <- sentimentposts %>%
  filter(Day == c(4,5,6), Cat == "Negative") 

plotpos2 <- r7
plotpos2$label <- c(paste(round(plotpos2$Count,digits=2)))

binseg.meancpt = cpt.mean(rev(r7$Percent), method="BinSeg",Q=3)
meancpt.point = cpts(binseg.meancpt)

p5 <- ggplot(data=r7, aes(x=Datetime, y = Percent)) + 
  geom_line(col="blue") + 
  geom_vline(xintercept = rev(r7$Datetime)[meancpt.point], col="red",lwd=0.9,linetype="dotted") +
  geom_text_repel(data = plotpos2, aes(label = label),  
                  vjust = 0.2, hjust = 0.4, size = 2) +
  scale_x_datetime(breaks = date_breaks("6 hours"), labels = date_format("%D %H:%M")) +
  ylab("Posts")+
  ggtitle("ChangePoint of mean with BinSeg: Negative") 

## 2nd change point graph for Neutral
r8 <- sentimentposts %>%
  filter(Day == c(4,5,6), Cat == "Neutral") 

plotpos2 <- r8
plotpos2$label <- c(paste(round(plotpos2$Count,digits=2)))

binseg.meancpt = cpt.mean(r8$Percent, method="BinSeg",Q=3)
meancpt.point = cpts(binseg.meancpt)

p6 <- ggplot(data=r8, aes(x=Datetime, y = Percent)) + 
  geom_line(col="blue") + 
  geom_vline(xintercept = rev(r8$Datetime)[meancpt.point], col="red",lwd=0.9,linetype="dotted") +
  geom_text_repel(data = plotpos2, aes(label = label),  
                  vjust = 0.2, hjust = 0.4, size = 2) +
  scale_x_datetime(breaks = date_breaks("6 hours"), labels = date_format("%D %H:%M")) +
  ylab("Posts")+
  ggtitle("ChangePoint of mean with BinSeg: Neutral") 

## 3rd change point graph for Positive
r9 <- sentimentposts %>%
  filter(Day == c(4,5,6), Cat == "Positive") 

plotpos2 <- r9
plotpos2$label <- c(paste(round(plotpos2$Count,digits=2)))

binseg.meancpt = cpt.mean(r9$Percent, method="BinSeg",Q=2)
meancpt.point = cpts(binseg.meancpt)

p7 <- ggplot(data=r9, aes(x=Datetime, y = Percent)) + 
  geom_line(col="blue") + 
  geom_vline(xintercept = rev(r9$Datetime)[meancpt.point], col="red",lwd=0.9,linetype="dotted") +
  geom_text_repel(data = plotpos2, aes(label = label),  
                  vjust = 0.2, hjust = 0.4, size = 2) +
  scale_x_datetime(breaks = date_breaks("6 hours"), labels = date_format("%D %H:%M")) +
  ylab("Posts")+
  ggtitle("ChangePoint of mean with BinSeg: Positive") 


#Combine graphs into one single output
grid.arrange(p5,p6,p7, ncol = 1, nrow = 3)


