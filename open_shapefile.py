#On ouvre le shapefile
shapefile_init = gpd.read_file('/cnrm/ville/USERS/corneillel/data/Communes/COMMUNE_SPF.shp')
shapefile_init=shapefile_init[shapefile_init['INSEE_REG']=='11']
shapefile_init.crs = "EPSG:3857"
shapefile_init=shapefile_init.reset_index()
shapefile_init=shapefile_init.drop(columns='index')
shapefile = shapefile_init.to_crs(epsg=4326)
