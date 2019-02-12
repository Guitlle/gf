from selenium import webdriver
import time

options = webdriver.ChromeOptions()
# options.headless = True
driver = webdriver.Chrome(chrome_options=options)
driver.get('https://sicoin.minfin.gob.gt')

def login(driver):
    user = driver.find_element_by_id("TxtUsuario")
    passw = driver.find_element_by_id("TxtPassword")
    user.send_keys("prensa")
    passw.send_keys("prensa")
    passw.submit()

def setYear(driver, year):
    frames = driver.find_elements_by_tag_name("frame")
    print(len(frames))
    driver.switch_to.frame(frames[0])
    try:
        preselector = driver.find_element_by_id("lblEjercicioActual")
        preselector.click()
    except:
        pass
    selector = driver.find_element_by_id("ddlListaEjercicios")
    selector.send_keys(str(year))
    driver.switch_to.parent_frame()

def resultsMng(year, filters):
    
