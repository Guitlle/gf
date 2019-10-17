from selenium import webdriver
import time
import sys
import os

# Put the folder were you have downloaded the chrome driver for selenium in the path variable, for instance:
# Windows:  set PATH=%PATH%;C:\chromedriver_32
# Linux:   export PATH=$PATH:~/chromedriver/

def login(driver):
    user = driver.find_element_by_id("TxtUsuario")
    passw = driver.find_element_by_id("TxtPassword")
    user.send_keys("prensa")
    passw.send_keys("prensa")
    passw.submit()

def findHeading(driver):
    for i in range(1,5):
        driver.switch_to.parent_frame()
    attempts = 10
    while (attempts > 0):
        attempts -= 1
        frame = None
        try:
            frame = driver.find_element_by_tag_name("frame")
            if frame.get_attribute("name") == "heading":
                driver.switch_to.frame(frame)
                form = driver.find_element_by_tag_id("Form1")
                if form is not None:
                    break
        except:
            pass

def setYear(driver, year):
    frames = driver.find_elements_by_tag_name("frame")
    driver.switch_to.frame(frames[0])
    try:
        preselector = driver.find_element_by_id("lblEjercicioActual")
        preselector.click()
    except:
        pass
    selector = driver.find_element_by_id("ddlListaEjercicios")
    try:
        ix = [x[-4:] for x in selector.get_attribute("innerHTML").split("</option>")].index(str(year))
        
    except:
        print("Year not found " + year)
        return False
    selector.click()
    selector.find_elements_by_tag_name("option")[ix].click()
    driver.switch_to.parent_frame()
    return True


def getReportGUI(driver, clicks_sequence):
    findHeading(driver)
    menu = driver.find_element_by_id("MenuNavegacion")
    driver.switch_to.frame(menu)
    try:
        for item in clicks_sequence:
            driver.find_element_by_id(item).click()
        
    except:
        return None
    
# Reports following a typical pattern found in SICOIN, where filters are set in an upper frame
# while month option is put in the lower frame and then a form is submitted to download an excel file
def reportTypeA(driver, clicks_sequence, filters, mes_inicio = "ENERO", mes_final = "DICIEMBRE"):
    getReportGUI(driver, clicks_sequence)
    findHeading(driver)
    pags = driver.find_element_by_id("paginas")
    driver.switch_to.frame(pags)
    frames = driver.find_elements_by_tag_name("frame")
    driver.switch_to.frame(frames[0])
    for filter in filters:
        variable = driver.find_element_by_id("UCFiltroReporte1_DDLEstructura")
        operador = driver.find_element_by_id("UCFiltroReporte1_DDLOperador")
        valor = driver.find_element_by_id("UCFiltroReporte1_TXTValor")
        variable.send_keys(filter["variable"])
        operador.send_keys(filter["operador"])
        valor.send_keys(filter["valor"])
        driver.find_element_by_id("UCFiltroReporte1_Button1").click()
    driver.switch_to.parent_frame()
    driver.switch_to.frame(frames[1])
    driver.find_element_by_id("P_MES_INI").send_keys(mes_inicio)
    driver.find_element_by_id("P_MES_FIN").send_keys(mes_final)
    driver.find_elements_by_name("opExport")[1].click()
    driver.find_element_by_id("sbtContinuar").click()
    driver.refresh()

    
