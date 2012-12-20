# I had trouble reproducing the Houston Crime Maps from 
# https://github.com/hadley/ggplot2/wiki/Crime-in-Downtown-Houston,-Texas-:-Combining-ggplot2-and-Google-Maps
# And the file 'HoustonCrime.Rdata' from:
# https://sites.google.com/site/davidkahle/online-resources/ggplot2-2010-case-study

# I beleive that some of the functions in the program are old.  I couldn't get the script to run.
# I'm running R 2.15.2 in R-Studio 0.96.331 in a 64-bit Mac running OSX 10.8.2

# The problems are:

# 1. The ggooglemap function crashes because the GetMap function returns an error when 
# asked to save the map as a .jpg.  Note that GetMap still saves the map as a .jpg
# even though it returs the error message.  A quick get around for this problem is to 
# split the ggooglemap function into two parts with the same arguments, as done below.

# 2. R doesn't like the two following lines:
# site   <- strsplit(site, '\[')[[1]][2]
# site   <- strsplit(site, ',')[[1]][1:2]
# To fix this I've removed the 'location' argument from the function.  Because of this,
# users must specify spatial coordinates for the center of the map (which the code does
# anyways, making the location argument unnecessary).

# 3. Google no longer requires an API, so that argument is removed from the function.

# 4. The 'opts' option is outdated:
#   opts(title = 'Violent Crime Contour Map of Downtown Houston, 2010') +
# Need to use 'labs' instead
#   labs(title = 'Violent Crime Contour Map of Downtown Houston, 2010') +

# 5. The 'train' function for the weather map is outdated:
# den_fill_scale$train(kde_df$density, T)
# This should now be:
# den_fill_scale$range$train(kde_df$density)

# 6. The '$map' function for the weather map is outdated:
# kde_df$density_s <- den_fill_scale$map(kde_df$density)
# This should now be:
# kde_df$density_s <- ggplot2:::scale_map(den_fill_scale,kde_df$density)

# 7. The 'scale_alpha(to = c(0, .9))' option in the weather map doesn't work.
# I haven't found a replacement, so it is commented out.

library(ggplot2)
library(ReadImages)
library(RgoogleMaps)
library(MASS)
library(reshape) # Note I added this for the use for the use of the 'melt' function
theme_set(theme_bw())


setwd("/Users/tiernay/Desktop/spatial")

################################################################################
#################### preload functions for later use        ####################
################################################################################

ggimage <- function(image){
  require(ggplot2)
  
  if(length(dim(image)) == 2){
    message('creating black and white image...')
    image <- melt(image)
    names(image) <- c('row','column','fill')
    plot <- qplot(column, -row, data = image, geom = 'tile', fill = fill) +
      scale_fill_gradient(low = 'black', high = 'white')
  }
  
  if(length(dim(image)) == 3){
    message('creating color image...')
    image <- apply(image, 1:2, function(v) rgb(v[1], v[2], v[3]))
    image <- melt(image)
    names(image) <- c('row', 'column', 'fill')
    plot <- qplot(column, -row, data = image, geom = 'tile', fill = fill) +
      scale_fill_identity()    
  }
  
  #return(plot) # remove first pound for the image in the case study
  plot +
    opts(      
      axis.line = theme_blank(), axis.ticks = theme_blank(),
      axis.text.x = theme_blank(), axis.text.y = theme_blank(), 
      axis.title.x = theme_blank(), axis.title.y = theme_blank(),
      axis.ticks.length = unit(0, "lines"), 
      axis.ticks.margin = unit(0, "lines"),
      legend.position = "none", 
      panel.background = theme_blank(), 
      panel.border = theme_blank(), 
      panel.grid.major = theme_blank(), 
      panel.grid.minor = theme_blank(), 
      panel.margin = unit(0, "lines"), 
      plot.background = theme_blank(), 
      plot.title = theme_blank(), 
      plot.margin = unit(c(-1, -1, -1.5, -1.5), "lines")
    )
}



# Note that this is now ggooglemap1
ggooglemap1 <- function(
  center = c(lat = 0, lon = 0), 
  type = c('color','bw')[1], rgbcoefs = c(0, 1, 0), zoom = zoom, 
  maptype = 'terrain',
  destfile = 'TemporaryMap.jpg', n_pix = 640)
{
  require(ggplot2)  
  require(RgoogleMaps)
  require(ReadImages)  
  
  # get map
  GetMap( center = center[c('lat','lon')], 
          size = c(n_pix, n_pix), zoom = zoom, format = 'jpg', 
          maptype = maptype, destfile = destfile)
}

# ggooglemap1 will return an error message:
# Error in readPNG(destfile, native = native) : file is not in PNG format
# It's ok, ignore the error and run ggooglemap2

ggooglemap2 <- function(
  center = c(lat = 0, lon = 0), 
  type = c('color','bw')[1], rgbcoefs = c(0, 1, 0), zoom = zoom, 
  maptype = 'terrain',
  destfile = 'TemporaryMap.jpg', n_pix = 640)
{
  # load map  
  map <- read.jpeg(destfile)
  
  # deal with color
  if(type == 'color'){
    map <- apply(map, 1:2, function(v) rgb(v[1], v[2], v[3]))     
  } else if(type == 'bw') {
    nrow <- nrow(map)
    ncol <- ncol(map)    
    map <- grey(rgb2grey(map, coefs = rgbcoefs))
    map <- matrix(map, nrow = nrow, ncol = ncol)
  } else {
    stop('type must be either 50aab105c5a133889b1aeb4f8dc967bc28c77661#39;color\' or 50aab105c5a133889b1aeb4f8dc967bc28c77661#39;bw\'', call. = FALSE)
  }
  
  # reshape map for plotting
  m_map <- melt(map)
  names(m_map) <- c('x','y','fill')
  m_map <- within(m_map,{
    x <- x - n_pix/2 - 1
    y <- y - n_pix/2 - 1
  })     
  
  mapInfo <- list(lat = center['lat'], lon = center['lon'], zoom = zoom, map)
  XY_cent <- LatLon2XY.centered(mapInfo, center['lat'], center['lon'])

  
  # geocode pixel references
  s <- (-n_pix/2) : (n_pix/2 - 1)  
  lat_wrapper <- function(x) XY2LatLon(mapInfo, -n_pix/2, x)[1]
  lats <- apply(data.frame(s), 1, lat_wrapper)  
  lon_wrapper <- function(y) XY2LatLon(mapInfo, y, -n_pix/2)[2]
  lons <- apply(data.frame(s), 1, lon_wrapper)
  
  # merge colors to latlons and return
  df_xy   <- expand.grid(x = s, y = s)
  df_ll   <- expand.grid(lat = rev(lats), lon = lons)
  df_xyll <- data.frame(df_xy, df_ll)
  df <- suppressMessages(join(df_xyll, m_map, type = 'right'))
  df <- df[,c('lon','lat','fill')]
  df
}





theme_nothing <- function (base_size = 12){
  structure(list(
    axis.line = theme_blank(), 
    axis.text.x = theme_blank(), axis.text.y = theme_blank(),
    axis.ticks = theme_blank(), 
    axis.title.x = theme_blank(), axis.title.y = theme_blank(), 
    axis.ticks.length = unit(0, "lines"), axis.ticks.margin = unit(0, "lines"), 
    legend.position = "none", 
    panel.background = theme_rect(fill = 'white'), 
    panel.border = theme_blank(), 
    panel.grid.major = theme_blank(), panel.grid.minor = theme_blank(), 
    panel.margin = unit(0, "lines"), 
    plot.background = theme_rect(colour = 'white'), 
    plot.title = theme_text(size = base_size * 1.2), 
    plot.margin = unit(c(-1, -1, -1.5, -1.5), "lines")
  ), class = "options")
}




vplayout <- function(x, y)  viewport(layout.pos.row = x, layout.pos.col = y)






################################################################################
#################### making a map                           ####################
################################################################################

CityHall_latlon <- c(lon = -95.369318, lat = 29.760210) 
DowntownMap <-ggooglemap1(center = CityHall_latlon, zoom = 14)
DowntownMap <-ggooglemap2(center = CityHall_latlon, zoom = 14)

qplot(lon, lat, data = DowntownMap, geom = 'tile', fill = fill) +
  scale_fill_identity() +
  scale_x_continuous('Longitude') + 
  scale_y_continuous('Latitude') +  
  coord_equal() +
  labs(title = 'Terrain Map of Downtown Houston')





################################################################################
#################### houston crime                          ####################
################################################################################

load('HoustonCrime.Rdata')  # variable name : crime_df

# restrict to violent crimes
violent_crimes <- subset(crime_df, 
                         offense != 'Auto Theft' & offense != 'Theft' & offense != 'Burglary'
)

# restrict to year 2010 (january - august)
violent_crimes <- subset(violent_crimes, 
                         time >= ISOdatetime(2010, 1, 1, 0, 0, 0)
)

# grab downtown google map
CityHall_latlon <- c(lon = -95.369318, lat = 29.760210) 
DowntownMap <-ggooglemap1(center = CityHall_latlon, zoom = 14)
DowntownMap <-ggooglemap2(center = CityHall_latlon, zoom = 14)
lat_range <- range(DowntownMap$lat)
lon_range <- range(DowntownMap$lon)

# contour plot
ggplot() +
  geom_tile(aes(x = lon, y = lat, fill = fill), data = DowntownMap) +
  scale_fill_identity() +
  geom_density2d(aes(x = lon, y = lat, colour = ..level..), 
                 bins = I(10), fill = NA, alpha = I(1/2), size = I(.75), data = violent_crimes) +
                   scale_colour_gradient2('Violent\nCrime\nDensity', 
                                          low = 'darkblue', mid = 'orange', high = 'red', midpoint = 900) + 
                                            scale_x_continuous('Longitude', limits = lon_range) + 
                                            scale_y_continuous('Latitude', limits = lat_range) +
                                            labs(title = 'Violent Crime Contour Map of Downtown Houston, 2010') +
                                            coord_equal()   




# point plot
violent_crimes <- subset(violent_crimes,
                         lon_range[1] <= lon & lon <= lon_range[2] &
                           lat_range[1] <= lat & lat <= lat_range[2] 
)  # cuts out the warning for missing values in geom_point

ggplot() +
  geom_tile(aes(x = lon, y = lat, fill = fill), data = DowntownMap) +
  scale_fill_identity() +
  geom_jitter(aes(x = lon, y = lat, colour = offense, size = offense), 
              fill = NA, alpha = I(3/4), data = violent_crimes,
              position = position_jitter(width = .001, height = .001)) +
                scale_x_continuous('Longitude', limits = lon_range) + 
                scale_y_continuous('Latitude', limits = lat_range) +
                scale_colour_discrete('') +
                scale_size_manual('', values = c(
                  'Robbery' = 2, 'Aggravated Assault' = 2.5, 'Rape' = 3, 'Murder' = 4
                )) +
                  labs(title = 'Violent Crime Map of Downtown Houston, 2010') +
                  coord_equal()   













# weather map
load('HoustonCrime.Rdata')  # variable name : crime_df

violent_crimes <- subset(crime_df, 
                         offense != 'Auto Theft' & offense != 'Theft' & offense != 'Burglary'
)

violent_crimes <- subset(violent_crimes, 
                         time >= ISOdatetime(2010, 1, 1, 0, 0, 0)
) 

CityHall_latlon <- c(lon = -95.369318, lat = 29.760210) 
DowntownMap <-ggooglemap1(center = CityHall_latlon, zoom = 14, maptype = 'hybrid')
DowntownMap <-ggooglemap2(center = CityHall_latlon, zoom = 14, maptype = 'hybrid')
lat_range <- range(DowntownMap$lat)
lon_range <- range(DowntownMap$lon)

vclatlon <- violent_crimes[,c('lon','lat')]
vclatlon <- na.omit(violent_crimes[,c('lon','lat')])
vclatlon <- subset(vclatlon,
                   lon_range[1] <= lon & lon <= lon_range[2] &
                     lat_range[1] <= lat & lat <= lat_range[2] 
)
den <- kde2d(vclatlon$lon, vclatlon$lat, n = 320, 
             lims = c(lon_range, lat_range))

kde_df <- expand.grid(
  lon = seq.int(lon_range[1], lon_range[2], length.out = 320),
  lat = seq.int(lat_range[1], lat_range[2], length.out = 320)
)
kde_df$density <- melt(den$z)$value

summary(kde_df$density)
den_fill_scale <- scale_colour_gradient2(low = 'white', mid = 'darkgreen', 
                                         high = 'red', midpoint = 225)

 den_fill_scale$range$train(kde_df$density)

kde_df$density_s <- ggplot2:::scale_map(den_fill_scale,kde_df$density)

kde_df$density_zeroone <- pmin(kde_df$density / max(kde_df$density), .9)


big_plot <- ggplot() +
  geom_tile(aes(x = lon, y = lat, fill = fill), data = DowntownMap) +
  geom_tile(aes(x = lon, y = lat, fill = density_s, alpha = density_zeroone), 
            data = kde_df) +
              scale_x_continuous('Longitude', limits = lon_range) + 
              scale_y_continuous('Latitude', limits = lat_range) +
              #scale_alpha(to = c(0, .9)) +
              scale_fill_identity() +
              labs(
                legend.position = 'none',
                title = 'Violent Crime Weather Map of Downtown Houston, 2010'
              ) +
                coord_equal()















