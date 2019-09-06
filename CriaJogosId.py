# -*- coding: utf-8 -*-
"""
Created on Fri Sep  6 13:13:22 2019

@author: Flavio Fontanella
"""

import numpy as np
import pandas as pd

#lê os .csv e gera um dataframe para cada país
df_jarg = pd.read_csv("data/jogosarg.csv")
df_jbol = pd.read_csv("data/jogosbol.csv")
df_jbra = pd.read_csv("data/jogosbra.csv")
df_jchi = pd.read_csv("data/jogoschi.csv")
df_jcol = pd.read_csv("data/jogoscol.csv")
df_jecu = pd.read_csv("data/jogosecu.csv")
df_jpar = pd.read_csv("data/jogospar.csv")
df_jper = pd.read_csv("data/jogosper.csv")
df_juru = pd.read_csv("data/jogosuru.csv")
df_jven = pd.read_csv("data/jogosven.csv")

df_jogosid = pd.read_csv("data/jogossulaid.csv")
df_teams = pd.read_csv('data/teams.csv')

dfs = [df_jarg,df_jbol,df_jbra,df_jchi,df_jcol,df_jecu,df_jpar,df_jper,df_juru,df_jven]


#cria colunas para inserir os ids dos times em cada partida

for i in range(len(dfs)):
    mandantes = dfs[i]['t1']
    visitantes = dfs[i]['t2']
    idm = []
    idv = []
    idlims = [100*i+j for j in range(100)]
    df_times=df_teams[df_teams["id"].isin(idlims)]
    
    #alimenta as colunas de id considerando os times duplicados:
    for time in mandantes:
        idm.append(int(df_times[df_times.nome==time].id.values))
    
    for time in visitantes:
        idv.append(int(df_times[df_times.nome==time].id.values))
    
    #adiciona as colunas de ids ao dataframe
    dfs[i] = dfs[i].assign(id1=idm,id2=idv)[['date', 't1', 'id1', 'g1', 'g2',  'id2', 't2', 'tourn']]
    df_jogosid = df_jogosid.append(dfs[i],ignore_index=True)
    print(i,dfs[i].index.size,df_jogosid.index.size)
    
df_jogosid = df_jogosid.sort_values(by=["date"])
df_jogosid = df_jogosid[['date', 't1', 'id1', 'g1', 'g2',  'id2', 't2', 'tourn']]
    
#exporta o dataframe para um .csv
df_jogosid.to_csv('data/jogosid.csv')
