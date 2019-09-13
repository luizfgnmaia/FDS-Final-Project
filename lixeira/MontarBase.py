# -*- coding: utf-8 -*-
"""
Created on Tue Sep  3 21:18:28 2019

@author: Flavio Fontanella
"""

import numpy as np
import pandas as pd

#lê os .csv e gera um dataframe para cada país
df_arg = pd.read_csv("data/Argentina.csv")
df_bol = pd.read_csv("data/Bolivia.csv")
df_bra = pd.read_csv("data/Brasil.csv")
df_chi = pd.read_csv("data/Chile.csv")
df_col = pd.read_csv("data/Colombia.csv")
df_ecu = pd.read_csv("data/Equador.csv")
df_par = pd.read_csv("data/Paraguai.csv")
df_per = pd.read_csv("data/Peru.csv")
df_uru = pd.read_csv("data/Uruguai.csv")
df_ven = pd.read_csv("data/Venezuela.csv")

dfs = [df_arg,df_bol,df_bra,df_chi,df_col,df_ecu,df_par,df_per,df_uru,df_ven]
        
for df in dfs:
    dates = []
    for i in range(df['date'].size):        
        item = df['date'].get(i)
        date = item.split('.')
        if df['club_1'][0] == 'Defensa y Justicia' or df['club_1'][0] == 'Puerto Cabello':
            if date[1]>'06':
                date[2]=date[2][0:4]
            else:
                date[2]=date[2][-4:]
        item = date[2]+'.'+date[1]+'.'+date[0]
        dates.append(item)
    df['date']=dates
    df.sort_values(by="date", inplace=True)

#cria o cadastro de times argentinos
df_teste = df_arg[df_arg['date']>='2002.01.01'][df_arg['tournament']>='Primera Division'][df_arg['tournament']!='Super Cup']
clubs = df_teste['club_1'].values, df_teste['club_2'].values
clubs = np.unique(clubs)
clubes=[]
for i in range(len(clubs)):
    clubes.append([i+1, clubs[i], 'Argentina',1500])
df_clubarg = pd.DataFrame(clubes)

#cria o cadastro de times bolivianos
df_teste = df_bol[df_bol['date']>='2002.01.01']
clubs = df_teste['club_1'].values, df_teste['club_2'].values
clubs = np.unique(clubs)
clubes=[]
for i in range(len(clubs)):
    clubes.append([i+101, clubs[i], 'Bolivia',1500])
df_clubbol = pd.DataFrame(clubes)

#cria o cadastro de times brasileiros 
df_teste = df_bra[df_bra['date']>='2002.01.01'][df_bra['tournament']=='Série A']
clubs = df_teste['club_1'].values, df_teste['club_2'].values
clubs = np.unique(clubs)
clubes=[]
for i in range(len(clubs)):
    clubes.append([i+201, clubs[i], 'Brasil',1500])
df_clubbra = pd.DataFrame(clubes)

#cria o cadastro de times chilenos 
df_teste = df_chi[df_chi['date']>='2002.01.01'][df_chi['tournament']=='Primera Division']
clubs = df_teste['club_1'].values, df_teste['club_2'].values
clubs = np.unique(clubs)
clubes=[]
for i in range(len(clubs)):
    clubes.append([i+301, clubs[i], 'Chile',1500])
df_clubchi = pd.DataFrame(clubes)

#cria o cadastro de times colombianos 
df_teste = df_col[df_col['date']>='2002.01.01'][df_col['tournament']>='Liga'][df_col['tournament']<'M']
clubs = df_teste['club_1'].values, df_teste['club_2'].values
clubs = np.unique(clubs)
clubs = np.delete(clubs,0)
clubes=[]
for i in range(len(clubs)):
    clubes.append([i+401, clubs[i], 'Colombia',1500])
df_clubcol = pd.DataFrame(clubes)

#cria o cadastro de times equatorianos 
df_teste = df_ecu[df_ecu['date']>='2002.01.01'][df_ecu['tournament']>='Liga']
clubs = df_teste['club_1'].values, df_teste['club_2'].values
clubs = np.unique(clubs)
clubes=[]
for i in range(len(clubs)):
    clubes.append([i+501, clubs[i], 'Equador',1500])
df_clubecu = pd.DataFrame(clubes)

#cria o cadastro de times paraguaios 
df_teste = df_par[df_par['date']>='2002.01.01'][df_par['tournament']=='Primera Division']
clubs = df_teste['club_1'].values, df_teste['club_2'].values
clubs = np.unique(clubs)
clubs = np.delete(clubs,3)
clubs = np.delete(clubs,3)
clubes=[]
for i in range(len(clubs)):
    clubes.append([i+601, clubs[i], 'Paraguai',1500])
df_clubpar = pd.DataFrame(clubes)

#cria o cadastro de times peruanos 
df_teste = df_per[df_per['date']>='2002.01.01'][df_per['tournament']>='Liga 1']
clubs = df_teste['club_1'].values, df_teste['club_2'].values
clubs = np.unique(clubs)
clubs = np.delete(clubs,1)
clubes=[]
for i in range(len(clubs)):
    clubes.append([i+701, clubs[i], 'Peru',1500])
df_clubper = pd.DataFrame(clubes)

#cria o cadastro de times uruguaios 
df_teste = df_uru[df_uru['date']>='2002.01.01'][df_uru['tournament']=='Primera Division']
clubs = df_teste['club_1'].values, df_teste['club_2'].values
clubs = np.unique(clubs)
clubs = np.delete(clubs,0)
clubes=[]
for i in range(len(clubs)):
    clubes.append([i+801, clubs[i], 'Uruguai',1500])
df_cluburu = pd.DataFrame(clubes)

#cria o cadastro de times venezuelanos 
df_teste = df_ven[df_ven['date']>='2002.01.01'][df_ven['tournament']=='Primera Division']
clubs = df_teste['club_1'].values, df_teste['club_2'].values
clubs = np.unique(clubs)
clubes=[]
for i in range(len(clubs)):
    clubes.append([i+901, clubs[i], 'Venezuela',1500])
df_clubven = pd.DataFrame(clubes)

#cria um dataframe para cadastrar os times
cols = ['id','nome','pais','rate']
join = [df_clubarg,df_clubbol,df_clubbra,df_clubchi,df_clubcol,df_clubecu,df_clubpar,df_clubper,df_cluburu,df_clubven]
df_teams = pd.concat(join)
df_teams.columns=cols
df_teams

#salva o cadastro num .csv
df_teams.to_csv('data/teams1500.csv')