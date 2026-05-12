#%% (1) CALCUL DU NOMBRE DE NUITS TROP

import numpy as np
import xarray as xr
import pandas as pd
import geopandas as gpd

simu='NFR010D'
tn = xr.open_mfdataset('./data/'+simu+'/tn_comSPF_'+simu+'*.nc')
tn = tn.sel(date=tn.date.dt.month.isin([6,7,8]))


nuit_trop_COM=[]

for i in tn.COM.values:
    list_nuit_trop = []
    seuil_tn = 20
    tn_i = tn.sel(COM=i)
    nb_nuit_trop = len(tn_i.tn[tn_i.tn>20])
    
    print(i, nb_nuit_trop)
    nuit_trop_COM.append(nb_nuit_trop)
    
shapefile['nuit_trop_18'] = nuit_trop_COM 	#ajout au shapefile, faire /23 si on veut nb moyen de nuits trop / an sur 2000-2022

#%% version chtgpt
shapefile["nuit_trop_18"] = (
    (tn.tn > 20)
    .sum(dim="date")
    .to_series()
    .reindex(shapefile["COM"])
    .values
)


#%% (2) CALCUL DU NB DE PERS. EXPOSÉES LORS DE NUITS TROPICALES
var='tn'
ee=xr.open_mfdataset('./data/NFR010C/'+var+'_comSPF_NFR010C_*.nc')
ff=xr.open_mfdataset('./data/NFR010D/'+var+'_comSPF_NFR010D_*.nc')
ee = ee.sel(date=(ee.date.dt.month.isin([5,6,7,8,9])))
ff = ff.sel(date=(ff.date.dt.month.isin([5,6,7,8,9])))

#Pré-sélection des dates de NT pour ne pas boucler sur l'entièreté des jours d'été
dates_nt_90=np.unique(ee.tn[np.where(ee.tn>20)[0],np.where(ee.tn>20)[1]].date)
dates_nt_18=np.unique(ff.tn[np.where(ff.tn>20)[0],np.where(ff.tn>20)[1]].date)
ee=ee.sel(date=ee.date.isin(dates_nt_90))
ff=ff.sel(date=ff.date.isin(dates_nt_90))   #ff.date.isin(date_nt_18)

#Création de dictionnaires à remplir avec chaque date de nuit tropicale (key) et la liste des communes exposées à cette nuit trop
dic_NT_1990={}
dic_NT_2018={}

#avec LULC 1990
for i in ee.date.values:
    tn_i=ee.sel(date=i)
    list_COM=np.asarray(tn_i.COM[np.where(tn_i.tn>20)])
    dic_NT_1990[i]=list_COM
    
#avec LULC 2018
for i in ee.date.values:      #ou ff si on veut prendre les dates de NT de 2018, il y en a 3 en plus qu'en 1990
    tn_i=ff.sel(date=i)
    list_COM=np.asarray(tn_i.COM[np.where(tn_i.tn>20)])
    dic_NT_2018[i]=list_COM
    

# (3) INDICATEURS DE TEMPÉRATURE ESTIVALE MOYENNE
simu='NFR010D'
simu2='NFR010C'

for var in ['tn','tm','tx']:
    aa2=xr.open_mfdataset('./data/'+simu+'/'+var+'_comSPF_'+simu+'_*.nc')
    bb2=xr.open_mfdataset('./data/'+simu2+'/'+var+'_comSPF_'+simu2+'_*.nc')

    aa2 = aa2.sel(date=aa2.date.dt.month.isin([5,6,7,8,9]))  #sélection période d'été étendue
    bb2 = bb2.sel(date=bb2.date.dt.month.isin([5,6,7,8,9]))

    shapefile[var+'_mean_2018']=aa2[var].mean(dim='date')
    shapefile[var+'_mean_1990']=bb2[var].mean(dim='date')
    shapefile[var+'_mean_diff_2018-1990']= (aa2[var]-bb2[var]).mean(dim='date')
