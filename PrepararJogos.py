#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Sep  4 08:15:19 2019

@author: mac11
"""

import numpy as np
import pandas as pd

#lê os .csv e gera um dataframe para cada país
df_arg = pd.read_csv("data/argentina.csv")
df_bol = pd.read_csv("data/bolivia.csv")
df_bra = pd.read_csv("data/brasil.csv")
df_chi = pd.read_csv("data/chile.csv")
df_col = pd.read_csv("data/colombia.csv")
df_ecu = pd.read_csv("data/equador.csv")
df_par = pd.read_csv("data/paraguai.csv")
df_per = pd.read_csv("data/peru.csv")
df_uru = pd.read_csv("data/uruguai.csv")
df_ven = pd.read_csv("data/venezuela.csv")

df_teams = pd.read_csv('data/teams.csv')

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


cols = ['date','t1','g1','g2','t2','tourn']

#cria o dataframe de jogos argentinos
df_jogosarg = pd.DataFrame(columns = cols)
j=0
for i in range(df_arg['date'].size):
    jog = df_arg.iloc[[i]]
    if jog.iat[0,0]>='2002.01.01':
        if jog.iat[0,3]>=0 and jog.iat[0,6]>=0:
            jog.iat[0,2]=int(jog.iat[0,3])
            jog.iat[0,5]=int(jog.iat[0,6])    
        jogo = [jog.iat[0,0],jog.iat[0,1],jog.iat[0,2],jog.iat[0,5],jog.iat[0,4], jog.iat[0,7]]
        if jogo[1] in (df_teams[df_teams.pais == 'Argentina']['nome'].values):
            if jogo[4] in (df_teams[df_teams.pais == 'Argentina']['nome'].values):                   
                df_jogosarg.loc[j] = jogo
                j+=1

#exporta o dataframe para um .csv         
df_jogosarg.to_csv('data/jogosarg.csv')

#cria o dataframe de jogos bolivianos
df_jogosbol = pd.DataFrame(columns = cols)
j=0
for i in range(df_bol['date'].size):
    jog = df_bol.iloc[[i]]
    if jog.iat[0,0]>='2002.01.01':
        if jog.iat[0,3]>=0 and jog.iat[0,6]>=0:
            jog.iat[0,2]=int(jog.iat[0,3])
            jog.iat[0,5]=int(jog.iat[0,6])    
        jogo = [jog.iat[0,0],jog.iat[0,1],jog.iat[0,2],jog.iat[0,5],jog.iat[0,4], jog.iat[0,7]]
        if jogo[1] in (df_teams[df_teams.pais == 'Bolivia']['nome'].values):
            if jogo[4] in (df_teams[df_teams.pais == 'Bolivia']['nome'].values):                   
                df_jogosbol.loc[j] = jogo
                j+=1

#exporta o dataframe para um .csv  
df_jogosbol.to_csv('data/jogosbol.csv')


#cria o dataframe de jogos brasileiros
df_jogosbra = pd.DataFrame(columns = cols)
j=0
for i in range(df_bra['date'].size):
    jog = df_bra.iloc[[i]]
    if jog.iat[0,0]>='2002.01.01':
        if jog.iat[0,3]>=0 and jog.iat[0,6]>=0:
            jog.iat[0,2]=int(jog.iat[0,3])
            jog.iat[0,5]=int(jog.iat[0,6])    
        jogo = [jog.iat[0,0],jog.iat[0,1],jog.iat[0,2],jog.iat[0,5],jog.iat[0,4], jog.iat[0,7]]
        if jogo[1] in (df_teams[df_teams.pais == 'Brasil']['nome'].values):
            if jogo[4] in (df_teams[df_teams.pais == 'Brasil']['nome'].values):                   
                df_jogosbra.loc[j] = jogo
                j+=1

#exporta o dataframe para um .csv  
df_jogosbra.to_csv('data/jogosbra.csv')

#cria o dataframe de jogos chilenos
df_jogoschi = pd.DataFrame(columns = cols)
j=0
for i in range(df_chi['date'].size):
    jog = df_chi.iloc[[i]]
    if jog.iat[0,0]>='2002.01.01':
        if jog.iat[0,3]>=0 and jog.iat[0,6]>=0:
            jog.iat[0,2]=int(jog.iat[0,3])
            jog.iat[0,5]=int(jog.iat[0,6])    
        jogo = [jog.iat[0,0],jog.iat[0,1],jog.iat[0,2],jog.iat[0,5],jog.iat[0,4], jog.iat[0,7]]
        if jogo[1] in (df_teams[df_teams.pais == 'Chile']['nome'].values):
            if jogo[4] in (df_teams[df_teams.pais == 'Chile']['nome'].values):                   
                df_jogoschi.loc[j] = jogo
                j+=1
            
#exporta o dataframe para um .csv 
df_jogoschi.to_csv('data/jogoschi.csv')

#cria o dataframe de jogos colombianos
df_jogoscol = pd.DataFrame(columns = cols)
j=0
for i in range(df_col['date'].size):
    jog = df_col.iloc[[i]]
    if jog.iat[0,0]>='2002.01.01':
        if jog.iat[0,3]>=0 and jog.iat[0,6]>=0:
            jog.iat[0,2]=int(jog.iat[0,3])
            jog.iat[0,5]=int(jog.iat[0,6])    
        jogo = [jog.iat[0,0],jog.iat[0,1],jog.iat[0,2],jog.iat[0,5],jog.iat[0,4], jog.iat[0,7]]
        if jogo[1] in (df_teams[df_teams.pais == 'Colombia']['nome'].values):
            if jogo[4] in (df_teams[df_teams.pais == 'Colombia']['nome'].values):                   
                df_jogoscol.loc[j] = jogo
                j+=1

#exporta o dataframe para um .csv 
df_jogoscol.to_csv('data/jogoscol.csv')

#cria o dataframe de jogos equatorianos
df_jogosecu = pd.DataFrame(columns = cols)
j=0
for i in range(df_ecu['date'].size):
    jog = df_ecu.iloc[[i]]
    if jog.iat[0,0]>='2002.01.01':
        if jog.iat[0,3]>=0 and jog.iat[0,6]>=0:
            jog.iat[0,2]=int(jog.iat[0,3])
            jog.iat[0,5]=int(jog.iat[0,6])    
        jogo = [jog.iat[0,0],jog.iat[0,1],jog.iat[0,2],jog.iat[0,5],jog.iat[0,4], jog.iat[0,7]]
        if jogo[1] in (df_teams[df_teams.pais == 'Equador']['nome'].values):
            if jogo[4] in (df_teams[df_teams.pais == 'Equador']['nome'].values):                   
                df_jogosecu.loc[j] = jogo
                j+=1

#exporta o dataframe para um .csv 
df_jogosecu.to_csv('data/jogosecu.csv')

#cria o dataframe de jogos paraguaios
df_jogospar = pd.DataFrame(columns = cols)
j=0
for i in range(df_par['date'].size):
    jog = df_par.iloc[[i]]
    if jog.iat[0,0]>='2002.01.01':
        if jog.iat[0,3]>=0 and jog.iat[0,6]>=0:
            jog.iat[0,2]=int(jog.iat[0,3])
            jog.iat[0,5]=int(jog.iat[0,6])    
        jogo = [jog.iat[0,0],jog.iat[0,1],jog.iat[0,2],jog.iat[0,5],jog.iat[0,4], jog.iat[0,7]]
        if jogo[1] in (df_teams[df_teams.pais == 'Paraguai']['nome'].values):
            if jogo[4] in (df_teams[df_teams.pais == 'Paraguai']['nome'].values):                   
                df_jogospar.loc[j] = jogo
                j+=1

 
df_jogospar.to_csv('data/jogospar.csv')

#cria o dataframe de jogos peruanos
df_jogosper = pd.DataFrame(columns = cols)
j=0
for i in range(df_per['date'].size):
    jog = df_per.iloc[[i]]
    if jog.iat[0,0]>='2002.01.01':
        if jog.iat[0,3]>=0 and jog.iat[0,6]>=0:
            jog.iat[0,2]=int(jog.iat[0,3])
            jog.iat[0,5]=int(jog.iat[0,6])    
        jogo = [jog.iat[0,0],jog.iat[0,1],jog.iat[0,2],jog.iat[0,5],jog.iat[0,4], jog.iat[0,7]]
        if jogo[1] in (df_teams[df_teams.pais == 'Peru']['nome'].values):
            if jogo[4] in (df_teams[df_teams.pais == 'Peru']['nome'].values):                   
                df_jogosper.loc[j] = jogo
                j+=1

#exporta o dataframe para um .csv
df_jogosper.to_csv('data/jogosper.csv')

#cria o dataframe de jogos uruguaios
df_jogosuru = pd.DataFrame(columns = cols)
j=0
for i in range(df_uru['date'].size):
    jog = df_uru.iloc[[i]]
    if jog.iat[0,0]>='2002.01.01':
        if jog.iat[0,3]>=0 and jog.iat[0,6]>=0:
            jog.iat[0,2]=int(jog.iat[0,3])
            jog.iat[0,5]=int(jog.iat[0,6])    
        jogo = [jog.iat[0,0],jog.iat[0,1],jog.iat[0,2],jog.iat[0,5],jog.iat[0,4], jog.iat[0,7]]
        if jogo[1] in (df_teams[df_teams.pais == 'Uruguai']['nome'].values):
            if jogo[4] in (df_teams[df_teams.pais == 'Uruguai']['nome'].values):                   
                df_jogosuru.loc[j] = jogo
                j+=1

#exporta o dataframe para um .csv
df_jogosuru.to_csv('data/jogosuru.csv')

#cria o dataframe de jogos venezuelanos
df_jogosven = pd.DataFrame(columns = cols)
j=0
for i in range(df_ven['date'].size):
    jog = df_ven.iloc[[i]]
    if jog.iat[0,0]>='2002.01.01':
        if jog.iat[0,3]>=0 and jog.iat[0,6]>=0:
            jog.iat[0,2]=int(jog.iat[0,3])
            jog.iat[0,5]=int(jog.iat[0,6])    
        jogo = [jog.iat[0,0],jog.iat[0,1],jog.iat[0,2],jog.iat[0,5],jog.iat[0,4], jog.iat[0,7]]
        if jogo[1] in (df_teams[df_teams.pais == 'Venezuela']['nome'].values):
            if jogo[4] in (df_teams[df_teams.pais == 'Venezuela']['nome'].values):                   
                df_jogosven.loc[j] = jogo
                j+=1

#exporta o dataframe para um .csv
df_jogosven.to_csv('data/jogosven.csv')
