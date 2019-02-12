from selenium import webdriver
import time
import sys
import os

options = webdriver.ChromeOptions()
options.headless = True
# options.add_argument("download.default_directory="+os.getcwd())
options.add_experimental_option("prefs", {
  "download.default_directory": os.getcwd()+"\\..\\..\\..\\Resource Tracking\\SICOIN Scraped\\",
  "download.prompt_for_download": False,
})
driver = webdriver.Chrome(chrome_options=options)
driver.get('https://sicoin.minfin.gob.gt')

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
def reportTypeA(driver, clicks_sequence filters):
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
    driver.find_element_by_id("P_MES_FIN").send_keys("DICIEMBRE")
    driver.find_elements_by_name("opExport")[1].click()
    driver.find_element_by_id("sbtContinuar").click()
    driver.refresh()
    
login(driver)
setYear(driver, 2015)
# Gestion por resultados report
reportTypeA(driver,
                     clicks_sequence = ["itemTextLink99", "itemTextLink102"], 
                     [
                        {
                            "variable": "ENTIDAD",
                            "operador": "igual",
                            "valor"   : "11130009"
                        },
                        {
                            "variable": "PROGRAMA",
                            "operador": "igual",
                            "valor"   : "17"
                        }
                     ])