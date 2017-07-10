
library(ncdf4)
library(fields)
library(kali)


####**Promedio de temperatura durante los años 1959 y 2005**

nc_sst=nc_open("datos/sst.nc4")
sst = ncvar_get(nc_sst, "to")
tiempo= ncvar_get(nc_sst, "time")
sst_promedio=apply(sst, MARGIN=3,FUN=mean, na.rm=TRUE)
plot(tiempo, sst_promedio,type="l", col="blue",las=1, xlab="tiempo",ylab="temperatura promedio(°C)")
abline(v=c(385:396),lty=4,col="red")

#### **Análisis de la temperatura superficial del mar, salinidad y fitoplancton durante el año 1991.**

nc_sst=nc_open("datos/sst.nc4")
nc_sss=nc_open("datos/sss.nc4")
nc_lphy=nc_open("datos/lphy.nc4")
nc_sphy=nc_open("datos/sphy.nc4")
sst = ncvar_get(nc_sst, "to")
sss = ncvar_get(nc_sss, "so")
lphy = ncvar_get(nc_lphy, "intpp")
sphy = ncvar_get(nc_sphy, "intpp")
par(mfrow=c(2,2))
tiempo= ncvar_get(nc_sst, "time")
#Grafico promedio de SST
tiempo=c(385:396)
promsst=apply(sst[,,385:396], MARGIN=3,FUN=mean, na.rm=TRUE)
promsss=apply(sss[,,385:396], MARGIN=3,FUN=mean, na.rm=TRUE)
promlphy=apply(lphy[,,385:396], MARGIN=3,FUN=mean, na.rm=TRUE)
promsphy=apply(sphy[,,385:396], MARGIN=3,FUN=mean, na.rm=TRUE)
plot(tiempo,promsst,type="l", col="blue",las=1, xlab="tiempo",ylab="temperatura promedio(°C)")
plot(tiempo,promsss,type="l", col="red",las=1, xlab="tiempo",ylab="salinidad promedio(PSU)")
plot(tiempo,promlphy,type="l", col="orange",las=1, xlab="tiempo",ylab="Fitoplancton grande(molC. m-2.s-1)")
plot(tiempo,promsphy,type="l", col="orange",las=1, xlab="tiempo",ylab="Fitoplancton pequeño(molC. m-2.s-1)")


####**Temperatura promedio y anomalia para año 1991**

#variable sst
nc_sst=nc_open("datos/sst.nc4")
sst = ncvar_get(nc_sst, "to")
lat = ncvar_get(nc_sst, "latitude")
lon = ncvar_get(nc_sst, "longitude") - 360
tiempo= ncvar_get(nc_sst, "time")
par(mfrow=c(1,2))
#Grafico promedio de SST
sst_promedio=apply(sst[,,1:552], MARGIN = c(1,2), FUN=mean, na.rm = TRUE)
image.map(lon, lat, sst_promedio)
title("PROMEDIO SST (°C) 1959-2005")

#Grafico promedio de año 1991
sst_ano1991=apply(sst[,,385:396], MARGIN = c(1,2), FUN = mean, na.rm = TRUE)
anomalia_sst=sst_promedio-sst_ano1991
image.map(lon, lat, anomalia_sst)
title("Anomalías SST (°C) año 1991")

###**Salinidad promedio y anomalia para año 1991**
##variable sss
nc_sss=nc_open("datos/sss.nc4")
sss=ncvar_get(nc_sss,"so")
lat=ncvar_get(nc_sss,"latitude")
lon=ncvar_get(nc_sss,"longitude") - 360
par(mfrow=c(1,2))
#Grafico promedio de SSS

sss_promedio=apply(sss[,,1:552], MARGIN = c(1,2), FUN=mean, na.rm = TRUE)
image.map(lon, lat, sss_promedio)
title("PROMEDIO SALINIDAD (PSU) 1959-2005")

#Grafico promedio de año SSS 1991

sss_ano1991=apply(sss[,,384:396], MARGIN = c(1,2), FUN = mean, na.rm = TRUE)
anomalia_sss=sss_promedio-sss_ano1991
image.map(lon, lat, anomalia_sss)
title("Anomalías salinidad (PSU) año 1991")


###**Fitoplancton promedio y anomalia para año 1991**.

nc_lphy=nc_open("datos/lphy.nc4")
nc_sphy=nc_open("datos/sphy.nc4")
lphy= ncvar_get(nc_lphy, "intpp")
sphy= ncvar_get(nc_sphy, "intpp")
lat_sphy = ncvar_get(nc_sphy, "latitude")
lon_sphy= ncvar_get(nc_sphy, "longitude") - 360
lat_lphy = ncvar_get(nc_lphy, "latitude")
lon_lphy= ncvar_get(nc_lphy, "longitude") - 360
par(mfrow=c(2,2))

#Grafico promedio total de phyto grande y pequeño

sphy_promedio=apply(sphy[,,1:552],MARGIN = c(1,2), FUN=mean, na.rm = TRUE)
image.map(lon_sphy, lat_sphy, sphy_promedio)
title("Fito pequeño 1959-2005(molC. m-2.s-1)")
lphy_promedio=apply(lphy[,,1:552],MARGIN = c(1,2), FUN=mean, na.rm = TRUE)
image.map(lon_sphy, lat_sphy, sphy_promedio)
title("Fito grande 1959-2005(molC. m-2.s-1)")

#Grafico promedio de año 1991 phyto grande y pequeño
sphy_ano1991=apply(sphy[,,384:396],MARGIN = c(1,2), FUN=mean, na.rm = TRUE)
image.map(lon_sphy, lat_sphy, sphy_ano1991)
title("Fito pequeño año 1991(molC. m-2.s-1)")

lphy_ano1991=apply(lphy[,,384:396],MARGIN = c(1,2), FUN=mean, na.rm = TRUE)
image.map(lon_lphy, lat_lphy, lphy_ano1991)
title("Fito grande año 1991(molC. m-2.s-1)")


