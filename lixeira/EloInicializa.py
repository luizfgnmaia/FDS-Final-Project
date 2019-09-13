# -*- coding: utf-8 -*-
"""
Created on Sat Jul  6 08:44:18 2019

@author: Flavio Fontanella
"""

import numpy as np
import pandas as pd


def rate_table(entrada):    
    ratings = []
    i=1
    for line in entrada:
        while i!=line[0]:
            ratings.append([i,0.0])
            i+=1
        rate = [0, 0.0]
        s = [line[0],line[1]]
        #s = line.split()
        rate[0]=int(s[0])
        rate[1]=float(s[1])
        ratings.append(rate)
        i+=1
    print(ratings)
    return ratings


def elo(Pb,W,We,I,dif):
    return Pb + I*(W-We)*dif**(1/2)


def elo_rate(ratings, jogos):
    for line in jogos:
        s = [line[0],line[1],line[2],line[3]]
        #divide o registro de um jogo em suas informações: time1, gols1, gols2, time2, torneio e importância
        jogo = [0,0,0,0]        
        jogo[0] = int(s[0])
        jogo[1] = int(s[1])  
        jogo[2] = int(s[2])  
        jogo[3] = int(s[3])
        I=20       
        t1 = jogo[0]
        t2 = jogo[3]
        Pb1 = ratings[t1-1][1]
        Pb2 = ratings[t2-1][1]
        W1 = 0.5
        W2 = 0.5
        dif = jogo[1]-jogo[2]
        if dif>0:
            W1,W2 = 1,0
        elif dif<0:
            W1,W2,dif = 0,1,-dif
        else:
            dif=1
        We1 = 1/(10**((Pb2-Pb1+100)/400)+1)
        We2 = 1/(10**((Pb1-Pb2-100)/400)+1)
        x1 = elo(Pb1,W1,We1,I,dif)
        x2 = elo(Pb2,W2,We2,I,dif)
        ratings[t1-1][1]=x1
        ratings[t2-1][1]=x2
    return ratings



#dataframes dos resultados das Libertadores de 1999 a 2001
df_jogosid = pd.read_excel("data/lib_9901MEX.xlsx")
df_teams = pd.read_csv("data/teams99.csv")
df_teams02 = pd.read_csv("data/teams1500.csv")

entrada = list(zip(df_teams.id,df_teams.rate))
ratings = rate_table(entrada) 

jogos = list(zip(df_jogosid.id1,df_jogosid.g1,df_jogosid.g2,df_jogosid.id2))
ratings = elo_rate (ratings, jogos)

#calcula o Elo rate por país referente às Libertadores de 1999 a 2001
rates = [ratings[i][1] for i in range(len(ratings))]
ratings=[]
for rate in rates:
    if rate!=0:
        ratings.append(rate)
df_teams.rate = ratings
print(df_teams.sort_values(by=['rate'],ascending=False)[['id','nome','pais','rate']])

#renormaliza os ratings para média 1500 sem os times mexicanos
for i in range(len(ratings)-1):
    ratings[i]+=(ratings[10]-1500)/10
ratings[10]=1500
print(ratings, np.mean(ratings))

#calcula os número de times cadastrados por país
timesporpais = []
for i in range(10):
    qtd = df_teams02[df_teams02['id']>=100*i][df_teams02['id']<100*(i+1)].id.size
    timesporpais.append(qtd)
    
print(timesporpais)

#pondera o rating de cada país pelo número de times daquele país
ratings_ini = [round(1500+(ratings[i]-1500)*np.min(timesporpais)/timesporpais[i],8) for i in range(10)]
rates_ini = []
for i in range(10):
    for j in range(timesporpais[i]):
        rates_ini.append(ratings_ini[i])

#atribui o respectivo valor inicial encontrado para todos os times da base
df_teams02.rate = rates_ini
cols = ['id','nome','pais','rate']
df_teams02 = df_teams02[cols]
print(df_teams02, round(np.mean(df_teams02.rate),8))
 
#salva o dataframe num .csv
df_teams02.to_csv('data/teams.csv')

