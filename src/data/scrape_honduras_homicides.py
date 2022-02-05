"""
Scrapes homicide counts for each municipality and annual period in Honduras
from the National Police database located at:
https://www.sepol.hn/sepol-estadisticas-incidencia-municipio.php
"""

from bs4 import BeautifulSoup
import os
import pandas as pd
from selenium import webdriver
from selenium.webdriver.support.select import Select
from selenium.webdriver.common.by import By
from time import sleep
from tqdm import tqdm
from webdriver_manager.chrome import ChromeDriverManager


# Helper functions
def load_municipality_list(dep):
    # Set up a headless Chrome browser
    options = webdriver.ChromeOptions()
    options.add_argument("headless")
    options.add_argument('log-level=2')

    # Set up driver
    driver = webdriver.Chrome(ChromeDriverManager().install(), options=options)
    driver.maximize_window()
    url = 'https://www.sepol.hn/sepol-estadisticas-incidencia-municipio.php'
    driver.get(url)
    sleep(2)

    # Find municipality list
    driver.find_element(By.XPATH, "//select[@name='dep_cod']/option[text()='{}']".format(dep)).click()
    sleep(2)
    muns = Select(driver.find_element(By.ID, "mun_cod"))
    mun_list = [o.text for o in muns.options]
    return mun_list


def query_municipality(dep, mun, year):
    # Set up a headless chrome browser
    options = webdriver.ChromeOptions()
    options.add_argument("headless")
    options.add_argument('log-level=2')

    # Set up driver
    driver = webdriver.Chrome(ChromeDriverManager().install(), options=options)
    driver.maximize_window()
    url = 'https://www.sepol.hn/sepol-estadisticas-incidencia-municipio.php'
    driver.get(url)
    sleep(2)

    # Define departamento, municipality and year
    driver.find_element(By.XPATH, "//select[@name='dep_cod']/option[text()='{}']".format(dep)).click()
    sleep(1)
    driver.find_element(By.XPATH, "//select[@name='mun_cod']/option[text()='{}']".format(mun)).click()
    sleep(1)
    driver.find_element(By.XPATH, "//select[@name='anio']/option[text()='{}']".format(year)).click()
    sleep(1)

    # Define calendar dates (start and end)
    driver.find_element(By.ID, 'fch_inicio').click()
    sleep(1)
    driver.find_element(By.XPATH, "//select[@class='ui-datepicker-month']/option[text()='Ene']").click()
    driver.find_element(By.XPATH, "//a[text()='1']").click()
    sleep(1)

    driver.find_element(By.ID, 'fch_fin').click()
    sleep(1)
    driver.find_element(By.XPATH, "//select[@class='ui-datepicker-month']/option[text()='Dic']").click()
    driver.find_element(By.XPATH, "//a[text()='31']").click()
    sleep(1)

    # Generate table
    driver.find_element(By.CSS_SELECTOR, '.but_table').click()
    sleep(2)

    # Scrape table
    soup = BeautifulSoup(driver.page_source, features="html.parser")
    table = soup.find_all('table')[0]

    # Verify dep, mun, year and calendar period
    heading = [[cell.text for cell in row.find_all(["th"])] for row in table.find_all("tr")][0][0]
    query_dep = heading.split('Departamento de ')[1].split(' del')[0]
    query_mun = heading.split('Municipio de ')[1].split(',')[0]
    query_date_st = heading.split('del ')[1].split(' a')[0]
    query_date_end = heading.split(' a ')[1][:11]
    if query_dep != dep or query_mun != mun:
        raise Exception('[ERROR] Queried dep, mun: {} {} vs {} {}'.format(query_dep, query_mun, dep, mun))
    if query_date_st != '01-ENE-{}'.format(year) or query_date_end != '31-DIC-{}'.format(year):
        raise Exception('[ERROR] Queried dates: {} - {} for year: {}'.format(
            query_date_st, query_date_end, year))

    # Parse table
    tab_data = [[cell.text for cell in row.find_all(["th", "td"])] for row in table.find_all("tr")]
    homicidios = None
    for t in tab_data:
        if len(t) == 3:
            id, crime, count = t
            if crime == 'HOMICIDIOS':
                homicidios = int(count)
                break
    if homicidios is None:
        print('[WARNING] Missing homicide value for {} {} {}'.format(dep, mun, year))

    return homicidios


#query_municipality('COMAYAGUA', 'EL ROSARIO', '2013')
#load_municipality_list('COMAYAGUA')

# Departamento list
DEPS = ['ATLÁNTIDA', 'COLON', 'COMAYAGUA', 'COPAN', 'CORTES', 'CHOLUTECA',
        'EL PARAÍSO', 'FRANCISCO MORAZÁN', 'GRACIAS A DIOS', 'INTIBUCÁ',
        'ISLAS DE LA BAHÍA', 'LA PAZ', 'LEMPIRA', 'OCOTEPEQUE', 'OLANCHO',
        'SANTA BÁRBARA', 'VALLE', 'YORO']
YEARS = range(2013, 2021)
OUTPUT_PATH = os.path.join(
    '..', '..', 'Data', 'Raw', 'honduras_departamento_year_2013_2020.csv')

# Set up homicide table and query each departamento
if os.path.exists(OUTPUT_PATH):
    homicide_table = pd.read_csv(OUTPUT_PATH)
    last_queried_dep = homicide_table['Dep'].unique().tolist()[-1]
    deps = DEPS[DEPS.index(last_queried_dep):]
else:
    homicide_table = pd.DataFrame({'Dep': [], 'Mun': [], 'Year': [], 'Count': []})
    deps = DEPS

# Set up municipality dictionary
mun_dict = {}

# Iterate
for dep in tqdm(deps):
    print('[INFO] Querying departamento: {}'.format(dep))
    # Get municipalities
    mun_list = load_municipality_list(dep)
    mun_dict[dep] = mun_list

    for mun in mun_list:
        for year in YEARS:
            hom_count = query_municipality(dep, mun, year)
            homicide_table = homicide_table.append(
                {
                    'Dep': dep, 'Mun': mun, 'Year': year, 'Count': hom_count
                }, ignore_index=True
            )
        homicide_table.to_csv('honduras_homicides.csv', index=False)

# Remove duplicates
homicide_table.drop_duplicates(inplace=True)

# Verify completeness
for dep in DEPS:
    for mun in mun_dict[dep]:
        subset = homicide_table.loc[(homicide_table['Dep'] == dep) & (homicide_table['Mun'] == mun)]
        if len(subset) != len(list(YEARS)):
            print('[WARNING] Departamentp {}, Municipality {}'.format(dep, mun))

# Save final version
homicide_table.to_csv('honduras_homicides.csv', index=False)
