df = pd.read_csv('/cnrm/ville/USERS/corneillel/data/IRIS/zones_IPR.csv')
df['Insee'] = df['Insee'].astype(str)

hyper = list(df['Insee'][df['EntiteGeo_SDRIF-E']=='Hypercentre'])
rural = list(df['Insee'][df['EntiteGeo_SDRIF-E']=='Communes rurales'])
petit = list(df['Insee'][df['EntiteGeo_SDRIF-E']=='Petites villes'])
moyen = list(df['Insee'][df['EntiteGeo_SDRIF-E']=='Villes moyennes'])
agglo = list(df['Insee'][df['EntiteGeo_SDRIF-E']=="Couronne d'agglomération"])
coeur_agglo = list(df['Insee'][df['EntiteGeo_SDRIF-E']=="Cœur d'agglomération"])


iris_hyper = shapefile[shapefile['INSEE_COM'].isin(hyper)]
iris_rural =shapefile[shapefile['INSEE_COM'].isin(rural)]
iris_petit = shapefile[shapefile['INSEE_COM'].isin(petit)]
iris_moyen =shapefile[shapefile['INSEE_COM'].isin(moyen)]
iris_agglo = shapefile[shapefile['INSEE_COM'].isin(agglo)]
iris_coeur_agglo =shapefile[shapefile['INSEE_COM'].isin(coeur_agglo)]
