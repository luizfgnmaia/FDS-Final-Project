import numpy as np
import pandas as pd
from selenium import webdriver # http://pythonclub.com.br/selenium-parte-1.html
from selenium.webdriver.firefox.options import Options # https://stackoverflow.com/questions/53134306/deprecationwarning-use-setter-for-headless-property-instead-of-set-headless-opt
import time # https://www.tutorialspoint.com/python/time_sleep
import re # https://www.w3schools.com/python/python_regex.asp

def scrape_flashscore(*hrefs):
    
    lst_df = []
    
    # Iniciando o Selenium
    opt = Options() # https://datarebellion.com/blog/using-headless-firefox-with-selenium-in-python/
    opt.headless = True
    wd = webdriver.Firefox(executable_path = "./geckodriver.exe", options = opt)
    
    for href in hrefs:
    
        # Indo até a página
        wd.get("https://www.flashscore.com" + href + "results/")
    
        # Clicando no mostrar mais até que não dê mais
        stop = False
        while stop == False:
            try:
                time.sleep(1)
                show_more = wd.find_element_by_css_selector(".event__more--static") 
                show_more.click()
                time.sleep(3)
            except:
                stop = True        
        
        new_page = wd.page_source
    
        # Coletando as linhas
        rows = []
        we = wd.find_elements_by_css_selector(".event__match--oneLine")
        for i in we:
            rows.append(i.text)
        
        # Coletando data da partida e nome do torneio
        we = wd.find_element_by_css_selector("div.teamHeader__text")
        year = we.text
        date = [row[0:5]+"."+year for row in rows]
        we = wd.find_element_by_css_selector("div.teamHeader__name")
        tournament = we.text
    
        # Separando as informações das linhas e preparando o output
        #rows = [re.sub("\s\([A-Za-z]*\)", "", row) for row in rows] # Flamengo RJ (Bra) -> Flamengo RJ na Libertadores
        rows = [re.sub("\\nAwarded", "", row) for row in rows]
        rows = [re.sub("\((?=[^0-9])", "[", row) for row in rows]
        rows = [re.sub("(?<=[^0-9])\)", "]", row) for row in rows] 
        
        for i in range(len(rows)):
            last_par = rows[i].rfind("(")
            if last_par != -1 and rows[i][-1] == ")":
                rows[i] = rows[i][13:last_par-1].replace("\n", " ") 
            else:
                rows[i] = rows[i][13:].replace("\n", " ") 
    
        club_1 = []
        club_2 = []
        goals_1 = []
        goals_2 = []
        par_1 = []
        par_2 = []
    
        for i in range(len(rows)):
            if rows[i].find('(') != -1:
                parenthesis = re.search("\(.*\)", rows[i]).group()[1:-1]
                par_1.append(int(re.search(".*(?=-)", parenthesis).group()))
                par_2.append(int(re.search("(?<=-).*", parenthesis).group()))
            else:
                par_1.append(None)
                par_2.append(None)
            rows[i] = re.sub("\(.*\)", "", rows[i])
            tmp_1 = re.search(".*(?=\s\s-)", rows[i])
            if tmp_1 != None: # Se não tiver o resultado, não pegar
                tmp_1 = tmp_1.group()
                goals_1.append(int(tmp_1[tmp_1.rfind(" ")+1:]))
                club_1.append(tmp_1[:tmp_1.rfind(" ")])
            tmp_2 = re.search("(?<=-\s\s).*", rows[i])
            if tmp_2 != None:
                tmp_2 = tmp_2.group()
                goals_2.append(int(tmp_2[:tmp_2.find(" ")]))
                club_2.append(tmp_2[tmp_2.find(" "):].strip())
            
        # Salvando o data frame na lista
        df = pd.DataFrame(list(zip(date, club_1, goals_1, par_1, club_2, goals_2, par_2)), 
                           columns = ['date', 'club_1', 'goals_1', 'par_1', 'club_2', 'goals_2', 'par_2'])
        df['tournament'] = tournament
        lst_df.append(df)
    
    # Unindo os data frames da lista
    ret = pd.DataFrame(columns = df.columns)    
    for i in range(len(lst_df)):
        ret = ret.append(lst_df[i], ignore_index = True)  
    ret.replace(to_replace = [None], value = np.nan, inplace = True) # https://stackoverflow.com/questions/23743460/replace-none-with-nan-in-pandas-dataframe
    
    wd.quit()
    
    return(ret)