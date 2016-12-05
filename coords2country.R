library(sp)
library(rworldmap)

# The single argument to this function, points, is a data.frame in which:
#   - column 1 contains the longitude in degrees
#   - column 2 contains the latitude in degrees
coords2country = function(points)
{  
        countriesSP <- getMap(resolution='low')
        
        
        #setting CRS directly to that from rworldmap
        pointsSP = SpatialPoints(points, 
                proj4string=CRS(proj4string(countriesSP)))  
        
        
        # use 'over' to get indices of the Polygons object 
        # containing each point 
        indices = over(pointsSP, countriesSP)
        
        # return the ADMIN names of each country
        indices$ADMIN  
        
}
