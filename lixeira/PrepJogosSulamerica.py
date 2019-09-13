#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Sep  4 14:21:38 2019

@author: mac11
"""

import numpy as np
import pandas as pd

#lê os .csv e gera um dataframe para cada país
df_sula = pd.read_csv("data/continentais.csv")

df_teams = pd.read_csv('data/teams.csv')

dates = []
for i in range(df_sula['date'].size):        
    item = df_sula['date'].get(i)
    date = item.split('.')
    item = date[2]+'.'+date[1]+'.'+date[0]
    dates.append(item)    
df_sula['date']=dates
df_sula.sort_values(by="date", inplace=True)


cols = ['date','t1','g1','g2','t2','tourn']

#cria o dataframe de jogos das competições sul americanas
df_jogossula = pd.DataFrame(columns = cols)
j=0
for i in range(df_sula['date'].size):
    jog = df_sula.iloc[[i]]
    if jog.iat[0,0]>='2002.01.01': #descarta as partidas anteriores a 2002
        if jog.iat[0,3]>=0 and jog.iat[0,6]>=0: #ajeita o placar das partidas que foram decididas nos pênaltis
            jog.iat[0,2]=int(jog.iat[0,3])
            jog.iat[0,5]=int(jog.iat[0,6])    
        jogo = [jog.iat[0,0],jog.iat[0,1],jog.iat[0,2],jog.iat[0,5],jog.iat[0,4], jog.iat[0,7]]
        if jogo[1] in (df_teams['nome'].values): #descarta as partidas de times mexicanos ou que não jogaram primeira divisão desde 2002
            if jogo[4] in (df_teams['nome'].values):                   
                df_jogossula.loc[j] = jogo
                j+=1
            #else:
                #print(jogo)
        #else:
            #print(jogo)
                
#exporta o dataframe para um .csv
df_jogossula.to_csv('data/jogossula.csv')