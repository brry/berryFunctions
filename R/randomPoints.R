
# Berry Boessenkool

randomPoints <- function(
         xmin,
         xmax,
         ymin,
         ymax,
         number,
         mindist,
         plot=TRUE,
         ...)
{
# benotigte Abstandsfunktion definieren:
distance <- function(xpt,ypt, xref,yref) sqrt((xref-xpt)^2 + (yref-ypt)^2)
# Zielvektoren fuer zufaellig verteilte Punkte erstellen
x <- y <- rep(NA, number)
# Ersten Wert reinschreiben
x[1] <- runif(1, xmin, xmax) ; y[1] <- runif(1, ymin, ymax)
# number-1  Punkte hinzufuegen
for(i in 2:number)
   {x[i] <- runif(1, xmin, xmax) ; y[i] <- runif(1, ymin, ymax)
   # Wenn minimale Distanz nicht gehalten, Punkt ersetzen
   while(   min(distance(x[i], y[i], x[-i],y[-i]), na.rm=TRUE) < mindist  )
   {x[i] <- runif(1, xmin, xmax) ; y[i] <- runif(1, ymin, ymax) }
   }
# wenn gewollt, Ergebnis plotten
if(plot) plot(x,y,las=1,pch=16, ...)
# Ergebnis ausgeben
return(data.frame(x,y))
}
