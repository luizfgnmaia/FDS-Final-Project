from bs4 import BeautifulSoup
import requests

def get_hrefs(*urls):
    ret = []
    for url in urls:
        page = requests.get(url)
        soup = BeautifulSoup(page.text, "html.parser")
        elems = soup.select('.leagueTable__season a')
        hrefs = [elem.attrs["href"] for elem in elems]
        ret.extend(hrefs)
    return ret 

