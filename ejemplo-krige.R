library("automap", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.3")
library("raster", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.3")
library("rgeos", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.3")
library("rgdal", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.3")


# Leo los datos como CSV desde el WFS de GeoINTA
url_puntos="http://geointa.inta.gov.ar/geoserver/geointa/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=geointa:lluvias_hoy&maxFeatures=50&outputFormat=csv"
puntos_lluvia<-read.csv(url(url_puntos))

prj="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# Creo un array de SpatialPointDataframe
point.sp=SpatialPointsDataFrame(readWKT(puntos_lluvia$the_geom[1]), 
                                data.frame(OBJECTID=puntos_lluvia$id_estacion[1], 
                                           mm=puntos_lluvia$precip_mm[1]))

for (n in 2:nrow(puntos_lluvia)) {
  # El array se crea concatenando lo DF
  point.sp <- rbind(point.sp, SpatialPointsDataFrame(readWKT(puntos_lluvia$the_geom[n]), 
                                 data.frame(OBJECTID=puntos_lluvia$id_estacion[n], 
                                            mm=puntos_lluvia$precip_mm[n])))
}
head(point.sp)
str(point.sp)


grilla = GridTopology(c(-73,-52), c(0.25, 0.25), c(80,128))

grid.sp=SpatialPoints( cbind(coordinates(grilla)[,1],
                            coordinates(grilla)[,2]))

# Ordinary kriging

kriging_result = autoKrige(mm~1, point.sp, grid.sp)
plot(kriging_result)

df.out=data.frame(lon=kriging_result$krige_output@coords[,1],
                  lat=kriging_result$krige_output@coords[,2],
                  mm=kriging_result$krige_output@data$var1.pred)

raster.grid=rasterFromXYZ(df.out,  crs=CRS(prj))
plot(raster.grid)

# Corte
shape=readOGR(dsn = "/home/santiago/datos/INTA/Contorno_Argentina/",layer="limite-contorno")
recorte=mask(raster.grid, shape)
#png('recorte.png')

breakpoints <- c(seq(0,200,25),250)
colors <- brewer.pal(10, "Spectral")

plot(recorte, xlab="Longitud", ylab="Latitud", breaks=breakpoints, col=colors)
contour(recorte, add=T)
points(x=point.sp@coords[,1],y=point.sp@coords[,2],type = 'p', col = 'red', pch=16 )
#dev.off()
