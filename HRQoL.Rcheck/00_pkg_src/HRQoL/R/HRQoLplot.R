HRQoLplot <- function(data,legend=FALSE){
  
maxmin <- data.frame(
  RP=c(4,0),
  RE=c(3,0),
  PF=c(20,0),
  VT=c(20,0),
  SF=c(8,0),
  BP=c(9,0),
  GH=c(20,0),
  MH=c(13,0))
 
names(data) <- names(maxmin)


dat <- rbind(maxmin,data)

#radarchart(dat, axistype=1, seg=8, plty=1, vlabels=c("RP", "RE","PF", "VT", "SF","BP","GH","MH"), 
#           title="Short Form-36 Health Survey",pcol=brewer.pal(8,"Set1"),col=brewer.pal(8,"Set1"))

#radarchart(dat, axistype=2, pcol=brewer.pal(8,"Set1"), plty=1, pdensity=30, pfcol=brewer.pal(8,"Set1"),
#           title="Short Form-36 Health Survey")

#radarchart(dat, axistype=3, pty=32, plty=1, axislabcol="grey", na.itp=FALSE,
#           title="Short Form-36 Health Survey")

#radarchart(dat, axistype=1, plwd=1:8, pcol=1, centerzero=TRUE, 
#           seg=4, caxislabels=c("worst", "", "", "", "best"),
#           title="Short Form-36 Health Survey")

radarchart(dat, axistype=0, seg=4, pty=32, plty=1,plwd=3, na.itp=FALSE,cglcol="black",
           title="Short Form-36 Health Survey",pcol=brewer.pal(8,"Set1"))
if (legend==TRUE){
  legend("topright", legend=c(rownames(dat[-c(1,2),])),text.col=brewer.pal(8,"Set1"),bty="n")
}
box()
}