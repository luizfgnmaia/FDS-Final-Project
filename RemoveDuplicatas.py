# -*- coding: utf-8 -*-
"""
Created on Fri Sep  6 08:29:15 2019

@author: Flavio Fontanella
"""

import numpy as np
import pandas as pd

df_jogossula = pd.read_csv("data/jogossula.csv")
df_teams = pd.read_csv('data/teams.csv')

#encontra nomes duplicados de times na base
print(df_teams[df_teams.nome.duplicated(keep=False)])
nomes_duplicados = df_teams[df_teams.nome.duplicated()].nome.values
print(nomes_duplicados)

#verifica quantos jogos têm times com nome duplicados
print(df_jogossula[df_jogossula['t1'].isin(nomes_duplicados)]['t1'].size, 'mandantes entre os duplicados')
print(df_jogossula[df_jogossula['t2'].isin(nomes_duplicados)]['t2'].size, 'visitantes entre os duplicados')

#cria colunas para inserir os ids dos times em cada partida
mandantes = df_jogossula['t1']
visitantes = df_jogossula['t2']
idm = []
idv = []

#identifica as partidas disputadas pela U. Catolica do Equador
#print(df_jogossula[df_jogossula['t1']=='U. Catolica'][df_jogossula['tourn']=='Copa Sudamericana'][df_jogossula['date']>='2014']['date'])

for num in [2194,2249,2402,2633,2859,2898,3320,3397,3461]:
    df_jogossula.at[num,'t1']='U. Catolica2'

for num in [2210,2242,2418,2621,2763,2874,3375,3418,3446]:
    df_jogossula.at[num,'t2']='U. Catolica2'
#print(df_jogossula[df_jogossula['t1']=='U. Catolica2'])
#print(df_jogossula[df_jogossula['t2']=='U. Catolica2'])
    

#identifica as partidas disputadas pelo River Plate do Uruguai
for num in [1018,1215,1224,1241,1249,1393,1996,2029,2218,2253,2490,2500,2547,2558,3247,3399]:
    df_jogossula.at[num,'t1']='River Plate2'

for num in [1024,1199,1232,1244,1251,1383,1986,2018,2202,2245,2495,2523,2531,2572,3264,3416]:
    df_jogossula.at[num,'t2']='River Plate2'
#print(df_jogossula[df_jogossula['t1']=='River Plate2'])
#print(df_jogossula[df_jogossula['t2']=='River Plate2'])

#alimenta as colunas de id considerando os times duplicados:
for time in mandantes:
    if time=='Independiente':
        idm.append(22)
    elif time=='San Lorenzo':
        idm.append(33)
    elif time=='Santa Cruz':
        idm.append(235)
    elif time=='Guarani':
        idm.append(611)
    elif time=='Sport Boys':
        idm.append(125)
    elif time=='Portuguesa':
        idm.append(234)
    elif time=='River Plate':
        idm.append(31)
    elif time=='River Plate2':
        idm.append(826)
    elif time=='U. Catolica':
        idm.append(329)
    elif time=='U. Catolica2':
        idm.append(525)
    else:
        idm.append(int(df_teams[df_teams.nome==time].id.values))

for time in visitantes:
    if time=='Independiente':
        idv.append(22)
    elif time=='San Lorenzo':
        idv.append(33)
    elif time=='Santa Cruz':
        idv.append(235)
    elif time=='Guarani':
        idv.append(611)
    elif time=='Sport Boys':
        idv.append(125)
    elif time=='Portuguesa':
        idv.append(234)
    elif time=='River Plate':
        idv.append(31)
    elif time=='River Plate2':
        idv.append(826)
    elif time=='U. Catolica':
        idv.append(329)
    elif time=='U. Catolica2':
        idv.append(525)
    else:
        idv.append(int(df_teams[df_teams.nome==time].id.values))

#adiciona as colunas de ids ao dataframe
df_jogossula = df_jogossula.assign(id1=idm,id2=idv)[['date', 't1', 'id1', 'g1', 'g2',  'id2', 't2', 'tourn']]

#exporta o dataframe para um .csv
df_jogossula.to_csv('data/jogossulaid.csv')


