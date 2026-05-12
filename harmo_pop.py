#Opening population data files
df2 = pd.read_excel('pop_sexe_age_communes_1990.xlsx', header=0)
df3=pd.read_excel('pop_sexe_age_communes_2018.xlsx', header=0)

#Sum to form age class
dic_age_df2={'65-H':['ageq_rec01s1rpop1990','ageq_rec02s1rpop1990','ageq_rec03s1rpop1990','ageq_rec04s1rpop1990','ageq_rec05s1rpop1990','ageq_rec06s1rpop1990',
                  'ageq_rec07s1rpop1990','ageq_rec08s1rpop1990','ageq_rec09s1rpop1990','ageq_rec10s1rpop1990','ageq_rec11s1rpop1990','ageq_rec12s1rpop1990',
                  'ageq_rec13s1rpop1990'],
          '65+H':['ageq_rec14s1rpop1990','ageq_rec15s1rpop1990','ageq_rec16s1rpop1990','ageq_rec17s1rpop1990','ageq_rec18s1rpop1990',
                'ageq_rec19s1rpop1990','ageq_rec20s1rpop1990'],
          '65-F':['ageq_rec01s2rpop1990','ageq_rec02s2rpop1990','ageq_rec03s2rpop1990','ageq_rec04s2rpop1990','ageq_rec05s2rpop1990','ageq_rec06s2rpop1990',
                  'ageq_rec07s2rpop1990','ageq_rec08s2rpop1990','ageq_rec09s2rpop1990','ageq_rec10s2rpop1990','ageq_rec11s2rpop1990','ageq_rec12s2rpop1990',
                  'ageq_rec13s2rpop1990'],
          '65+F':['ageq_rec14s2rpop1990','ageq_rec15s2rpop1990','ageq_rec16s2rpop1990','ageq_rec17s2rpop1990','ageq_rec18s2rpop1990',
                'ageq_rec19s2rpop1990','ageq_rec20s2rpop1990']}

dic_age_df3={'65-H':['SEXE1_AGEPYR1000','SEXE1_AGEPYR1003','SEXE1_AGEPYR1006','SEXE1_AGEPYR1011','SEXE1_AGEPYR1018',
                 'SEXE1_AGEPYR1025','SEXE1_AGEPYR1040','SEXE1_AGEPYR1055'],
          '65+H':['SEXE1_AGEPYR1065','SEXE1_AGEPYR1080'],
          '65-F':['SEXE2_AGEPYR1000','SEXE2_AGEPYR1003','SEXE2_AGEPYR1006','SEXE2_AGEPYR1011','SEXE2_AGEPYR1018',
                 'SEXE2_AGEPYR1025','SEXE2_AGEPYR1040','SEXE2_AGEPYR1055'],
          '65+F':['SEXE2_AGEPYR1065','SEXE2_AGEPYR1080']}

for f in dic_age_df2.keys():
    df2[f]=df2[dic_age_df2[f][0]]
    for i in dic_age_df2[f][1:]:
        df2[f]+=df2[i]
        
for f in dic_age_df3.keys():
    df3[f]=df3[dic_age_df3[f][0]]
    for i in dic_age_df3[f][1:]:
        df3[f]+=df3[i]
        
df2['65+']=df2['65+H']+df2['65+F']
df2['65-']=df2['65-H']+df2['65-F']

df3['65+']=df3['65+H']+df3['65+F']
df3['65-']=df3['65-H']+df3['65-F']
df3['popTOT']=df3['65+']+df3['65-']

#Harmonization of municipalities (13 more in 1990 that merged around 2018)
com_equi={77028:77433,77149:77109,77166:77316,77170:77316,77299:77316,77399:77504,77491:77316,
 78251:78551,78503:78320,78524:78158,91182:91228,91222:91390,95259:95040}

for i in com_equi.keys():
    print(i)
    df2.loc[df2['INSEE']==com_equi[i],'65+']+=float(df2.loc[df2['INSEE']==i,'65+'])
    df2.loc[df2['INSEE']==com_equi[i],'65-']+=float(df2.loc[df2['INSEE']==i,'65-'])
    df2.loc[df2['INSEE']==com_equi[i],'65+H']+=float(df2.loc[df2['INSEE']==i,'65+H'])
    df2.loc[df2['INSEE']==com_equi[i],'65+F']+=float(df2.loc[df2['INSEE']==i,'65+F'])
    df2.loc[df2['INSEE']==com_equi[i],'65-H']+=float(df2.loc[df2['INSEE']==i,'65-H'])
    df2.loc[df2['INSEE']==com_equi[i],'65-F']+=float(df2.loc[df2['INSEE']==i,'65-F'])
    df2=df2.drop(df2[df2['INSEE']==i].index)
df2['popTOT']=df2['65+']+df2['65-']
df2=df2.reset_index(drop=True)

#Calculation of aged people fraction (+65) for both years and application on other
df2['frac_age_90']=df2['65+']/df2['popTOT']
df3['frac_age_18']=df3['65+']/df3['popTOT']
df2['65+_inv']=df2['popTOT']*df3['frac_age_18']
df3['65+_inv']=df3['popTOT']*df2['frac_age_90']
df2['65-_inv']=df2['popTOT']*(1-df3['frac_age_18'])
df3['65-_inv']=df3['popTOT']*(1-df2['frac_age_90'])

