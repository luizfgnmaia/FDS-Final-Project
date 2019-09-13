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


df_jogosid = pd.read_csv("data/jogosid.csv")
df_teams = pd.read_csv("data/teams.csv")
df_tercas = pd.read_csv("data/tercas.csv")

datual = '2002.01.01'
df_vez = df_teams
dates = [datual]*df_vez['id'].size
df_vez = df_vez.assign(date = dates)  
df_vez = df_vez[['id','nome','pais','rate','date']]
df_todo = df_vez
df_vez.to_csv('data/rankings/rank{}.csv'.format(datual))

for date in df_tercas['2002.01.01'].values:    
    danterior = datual    
    datual = date
    df_vez = pd.read_csv('data/rankings/rank{}.csv'.format(danterior))
    entrada = list(zip(df_vez.id,df_vez.rate))
    ratings = rate_table(entrada)    
    df_jogosmes = df_jogosid[df_jogosid['date']>=danterior][df_jogosid['date']<datual]
    jogos = list(zip(df_jogosmes.id1,df_jogosmes.g1,df_jogosmes.g2,df_jogosmes.id2))
    ratings = elo_rate (ratings, jogos)        
    rates = [ratings[i][1] for i in range(len(ratings))]
    ratings=[]
    for rate in rates:
        if rate!=0:
            ratings.append(round(rate,8))
    df_vez.rate = ratings
    dates = [date]*df_vez['id'].size
    df_vez.date = dates  
    df_vez = df_vez[['id','nome','pais','rate','date']]    
    df_vez.to_csv('data/rankings/rank{}.csv'.format(datual))
    df_todo = df_todo.append(df_vez,ignore_index=True)
    print(datual)
    print(df_vez.sort_values(by=['rate'],ascending=False)[['id','nome','pais','rate']].head())
    print(df_vez.sort_values(by=['rate'],ascending=False)[['id','nome','pais','rate']].tail())
    
print(df_vez.sort_values(by=['rate'],ascending=False)[['id','nome','pais','rate']])
df_todo = df_todo[['id','nome','pais','rate','date']]
print(df_todo)
df_todo.to_csv('data/rankings/ranktodos.csv')


