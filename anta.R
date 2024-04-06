library(fmsb)
library(plotly)

###############################################################
#wyznaczenie min zasiegu dla wszystkich katow
data0<-read.csv("daily_ice_edge.csv",header=TRUE,encoding="UTF-8")
data<-data0[,-1] #pomijamy kolumne z datami

data_m <- matrix(, nrow = length(data), ncol = 1) #pusta macierz (361 x 1)
for (i in 1:length(data_m)) {
  data_m[i] <- min(data[,i]) #minimalna wartosc z kazdej kolumny
}

#przeliczanie na wspolrzedne biegunowe
r <- 90 + (data_m) + 30; #90 pozwala "wywrocic na lewa strone"; +30 (okolo) by powiekszyc model
x <- matrix(, nrow = length(r), ncol = 1)
y <- matrix(, nrow = length(r), ncol = 1)
 
for(i in 1:(length(r))){
  x[i] <- r[i] * cos(pi*(length(r)-(i-1)+90)/180) #pi * kat / 180 = radiany
  y[i] <- r[i] * sin(pi*(length(r)-(i-1)+90)/180) #+90 by przeniesc kat 0 stopni na gore (tak jak w radiochart'cie)
}

png(filename="D:/plot_min.png", width = 550, height = 550)
par(mar = c(4,3,4,3))
plot(x,y, type='l', col="black", main="Minimalny zasi?gu lodu morskiego dla ka?dego z k?t?w", xlim = c(-80,80), ylim = c(-80,80), xlab="", ylab="", axes=FALSE, frame.plot=TRUE)
#lines(x1,y1, col="red")
#legend("topright", legend=c("minima zasi?g lodu", "wymodelowany zasi?g lodu"), col=c("black", "red"), lty=c(1,1), cex=0.8)
dev.off()


#to samo przy uzyciu radiocharta
data_m_r <- as.data.frame(t(data_m))
rb_data <- rbind(rep(0,361) , rep(-90,361), data_m_r) #data_m bylo wektorem 360x1 => zmieniamy na 1x360
colnames(rb_data) <- c(0:360)

radarchart(rb_data[,c(1, ncol(rb_data):2)]) #kod w [] odwraca kolejnosc stopni na zgodna ze wskazowkami zegara
##################################################################

everyTwoDays <- data0[0:1590,] #uciecie danych gdzie pomiar wykonywany jest co 2 dni
everyDay <- data0[1592:nrow(data),] #uciecie danych gdzie pomiar wykonywany jest co 1 dzien
everyDay <- everyDay[seq(1, nrow(everyDay), 2), ] #pominiecie czesci danych
allDataEveryTwoDays0 <- rbind(everyTwoDays, everyDay) #polaczenie danych
rownames(allDataEveryTwoDays0) <- 1:nrow(allDataEveryTwoDays0) #nadanie wierszom nowych nazw
allDataEveryTwoDays<-allDataEveryTwoDays0[,-1]

#/////////////////////////////////////////////////
#test
# ydata <- allDataEveryTwoDays[,181]
# t <- 1:nrow(allDataEveryTwoDays)
# plot(ydata~t)
# 
# ssp <- spectrum(ydata,plot=FALSE)
# per <- 1/ssp$freq[ssp$spec==max(ssp$spec)]
# reslm <- lm(ydata ~ sin(2*pi/per*t)+cos(2*pi/per*t))
# summary(reslm)
# 
# ydata_min <- min(ydata)
# func <- fitted(reslm)
# func[func<=ydata_min] <- ydata_min
# plot(ydata~t)
# lines(func~t,col=4,lty=2)

#wykres 3D
testData<-allDataEveryTwoDays[1:1000,]

#szerokosc geogr
x3d <- testData[,1]
for (i in 2:ncol(testData)){
  x3d <- append(x3d, testData[,i])
}

#czas
ss <- seq(1,nrow(testData),1)
y3d <- list()
for (j in 1:ncol(testData)){
  y3d <- append(y3d, ss)
}

#dlugosc geogr
num <- 0
z3d <- list()
for (i in 1:ncol(testData)){
  z3d <- append(z3d, rep(num, nrow(testData)))
  num <- num + 1
}

lista <- list(y3d, z3d, x3d)
d3d <- matrix(unlist(lista), ncol = 3)
sd3d <- d3d[order(d3d[, 1]), ]
dd3d <- data.frame(x=sd3d[,1],y=sd3d[,2],z=sd3d[,3])

# p<- plot_ly() %>% 
#   add_trace(data = dd3d,  x=dd3d$x, y=dd3d$y, z=dd3d$z, type="mesh3d" )
# p

p <- plot_ly(data=dd3d, x=dd3d$x, y=dd3d$y, z=dd3d$z, color = dd3d$z, colors = c('#BF382A','#0C4B8E'), size = 6)
p <- p %>% add_markers()
p <- p %>% layout(scene = list(xaxis = list(title = 'czas'),
                               yaxis = list(title = 'dlugos? geograficzna'),
                               zaxis = list(title = 'szerokos? geograficzna')))
p

#///////////////////////////////////////////////


expected <- matrix(, nrow = nrow(allDataEveryTwoDays), ncol = ncol(allDataEveryTwoDays))

for (i in 1:ncol(allDataEveryTwoDays)) {
  ydata <- allDataEveryTwoDays[,i]
  t <- 1:nrow(allDataEveryTwoDays)
  #plot(ydata~t)
  
  ssp <- spectrum(ydata,plot=FALSE)
  per <- 1/ssp$freq[ssp$spec==max(ssp$spec)]
  reslm <- lm(ydata ~ sin(2*pi/per*t)+cos(2*pi/per*t))
  summary(reslm)
  
  func <- fitted(reslm)
  func[func<=data_m[i]] <- data_m[i]
  
  for (j in 1:nrow(allDataEveryTwoDays)){
    expected[j,i] <- func[j]
  }
}
#############################################################
for (k in 1:nrow(allDataEveryTwoDays))
{
  #przeliczanie na wspolrzedne biegunowe
  r <- 90 + (allDataEveryTwoDays[k,]) + 30; #90 pozwala "wywrocic na lewa strone"; +30 (okolo) by powiekszyc model
  x <- matrix(, nrow = length(r), ncol = 1)
  y <- matrix(, nrow = length(r), ncol = 1)
  
  for(i in 1:(length(r))){
    x[i] <- r[i] * cos(pi*(length(r)-(i-1)+90)/180) #pi * kat / 180 = radiany
    y[i] <- r[i] * sin(pi*(length(r)-(i-1)+90)/180) #+90 by przeniesc kat 0 stopni na gore (tak jak w radiochart'cie)
  }
  
  r1 <- 90 + (expected[k,]) + 30; #90 pozwala "wywrocic na lewa strone"; +30 (okolo) by powiekszyc model
  x1 <- matrix(, nrow = length(r1), ncol = 1)
  y1 <- matrix(, nrow = length(r1), ncol = 1)
  
  for(i in 1:(length(r1))){
    x1[i] <- r1[i] * cos(pi*(length(r1)-(i-1)+90)/180) #pi * kat / 180 = radiany
    y1[i] <- r1[i] * sin(pi*(length(r1)-(i-1)+90)/180) #+90 by przeniesc kat 0 stopni na gore (tak jak w radiochart'cie)
  }
  
  plotpath<- file.path("D:","rplots",paste("plot_",k,".png",sep=""))
  png(filename=plotpath, width = 550, height = 550)
  
  par(mar = c(4,3,4,3))
  plot(x,y, type='l', col="black", main="Zmiana zasięgu lodu morskiego na Antarktydzie w latach 1978 - 2009", xlim = c(-80,80), ylim = c(-80,80), xlab="", ylab="", axes=FALSE, frame.plot=TRUE)
  lines(x1,y1, col="red")
  legend("topright", legend=c("rzeczywisty zasięg lodu", "wymodelowany zasięg lodu"),
         col=c("black", "red"), lty=c(1,1), cex=0.8)
  text(-68,79,allDataEveryTwoDays0[k,1])
  dev.off()
}