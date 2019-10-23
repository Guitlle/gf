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

def getReportGUI(driver, clicks_sequence):
    for attempts in range(1,10):
        try:
            findHeading(driver)
            menu = driver.find_element_by_id("MenuNavegacion")
            driver.switch_to.frame(menu)
            for item in clicks_sequence:
                driver.find_element_by_id(item).click()
            return True
        except:
            print("failed to find report GUI, attempt ", attempts)
    return None

# Reports following a typical pattern found in SICOIN, where filters are set in an upper frame
# while month option is put in the lower frame and then a form is submitted to download an excel file
def reportTypeA(driver, clicks_sequence, filters, mes_inicio = "ENERO", mes_final = "DICIEMBRE"):
    time.sleep(2)
    getReportGUI(driver, clicks_sequence)
    time.sleep(2)
    findHeading(driver)
    pags = driver.find_element_by_id("paginas")
    driver.switch_to.frame(pags)
    frames = driver.find_elements_by_tag_name("frame")
    while (len(frames) == 0):
        print("waiting for page load...")
        time.sleep(10)
        driver.switch_to.parent_frame()
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
        time.sleep(2)
    driver.switch_to.parent_frame()
    driver.switch_to.frame(frames[1])
    driver.find_element_by_id("P_MES_INI").send_keys(mes_inicio)
    driver.find_element_by_id("P_MES_FIN").send_keys(mes_final)
    driver.find_elements_by_name("opExport")[1].click()
    print("downloading")
    driver.find_element_by_id("sbtContinuar").click()
    time.sleep(2)
    driver.refresh()

def setYear(driver, year):
    driver.switch_to.parent_frame()
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
    selector.send_keys(str(year))
    selector.send_keys(webdriver.common.keys.Keys.ENTER)
    driver.switch_to.parent_frame()
    return True

def screenShot(output = "/tmp/tmp.png"):
    scshot = driver.get_screenshot_as_png()
    with open(output, "wb") as f:
        f.write(scshot)
     