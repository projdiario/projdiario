""" Download de PDFs do sítio da Imprensa Nacional.
    Alvo: DOU 1, 2, 3, Extra e Suplementar completos.
    ** NOTA: Não faz o download de arquivos já existentes.
    ** NOTA: DOU Extra e Suplementar somente serão baixados no dia
             seguinte à sua data de publicação.
    Versão do Python: 3.6
    Pacotes nativos utilizados:
        os | locale | urllib | datetime | calendar | re
    Pacotes não nativos necessários:
        1. BeautifulSoup 4 | Instalação: pip install beautifulsoup
"""

import os
from locale import setlocale
from locale import LC_TIME
from urllib.request import urlopen
from datetime import date
from datetime import timedelta
from calendar import day_abbr
from time import sleep
from random import random
from re import findall as refindall
from bs4 import BeautifulSoup
import paths

print("Inicializando...")

def check_dou_dirs(dou_path_, day_, month_, year_):
    """ Checagem/Criação da estrutura de diretórios dos downloads.
    """
    if not os.path.exists(dou_path_):
        os.makedirs(dou_path_)
    dou_path_ = os.path.join(dou_path_, year_)
    if not os.path.exists(dou_path_):
        os.makedirs(dou_path_)
    dou_path_ = os.path.join(dou_path_, month_)
    if not os.path.exists(dou_path_):
        os.makedirs(dou_path_)
    dou_path_ = os.path.join(dou_path_, day_)
    if not os.path.exists(dou_path_):
        os.makedirs(dou_path_)
    return dou_path_

def ult_dia_semana(dia_, mes_, ano_):
    """ Retorna o primeiro dia anterior ao input que é dia de semana.
        Tipo do retorno: <class 'datatime.date'>
    """
    day_before = 1
    tday = date(int(ano_), int(mes_), int(dia_))
    while True:
        yday = tday - timedelta(day_before)
        yday_abbr = yday.strftime("%a")
        if yday_abbr not in WEEKDAYS:
            day_before += 1
        else:
            return yday

# Definição da língua de criação de pastas para a padrão da máquina
setlocale(LC_TIME, "")

# Constantes gerais
HOJE = date.today()
# HOJE = date(2017, 9, 7)
DT_HOJE = HOJE.strftime("%d/%m/%Y")
NUM_ANO = HOJE.strftime("%Y")
NM_MES = HOJE.strftime("%B")
NUM_MES = HOJE.strftime("%m")
NUM_DIA = HOJE.strftime("%d")
DT_FILE = HOJE.strftime("%Y_%m_%d")
JORNAIS = 3

# Verificação se é dia da semana
WEEKDAYS = list(day_abbr)[:-2]
WEEKEND_DAYS = list(day_abbr)[-2:]

# URLs - Constantes
MAINDOWN_URL = "http://pesquisa.in.gov.br/imprensa/servlet/INPDFViewer?"
MAINVISU_URL = "http://pesquisa.in.gov.br/imprensa/jsp/visualiza/index.jsp?"
DAY_URL = "data=" + DT_HOJE
END_URL = "captchafield=firstAccess"

if HOJE.strftime("%a") in WEEKDAYS:
    for i in range(1, JORNAIS+1):

        print("Download DOU%s iniciado..." % str(i), end='\r')

        # Verificação/Montagem dos paths de download (DOU, ANO, MÊS, DIA)
        dou_dir = os.path.join(paths.PDF_DIR, "DOU"+str(i))

        # URL - Número do Jornal
        jornal_url = "jornal=" + str(i)

        # Definição do número máximo de páginas do DOU.
        pag_num = 1
        pag_url = "pagina=" + str(pag_num)
        pag_url = MAINVISU_URL + jornal_url + chr(38) + pag_url + chr(38) + DAY_URL

        url = urlopen(pag_url)
        soup = BeautifulSoup(url, "html5lib")
        soup = soup.find("frame", {"name":"controlador"})

        pag_max = str(soup).split(";")[-1]
        pag_max = refindall(r"\d+", pag_max)
        pag_max = int(pag_max[0])

        dou_dir = check_dou_dirs(dou_dir, NUM_DIA, NM_MES, NUM_ANO)

        # Lista dos arquivos no diretório alvo (evitar duplicidade de download)
        files_lst = os.listdir(dou_dir)

        for j in range(1, pag_max+1):
            # URLs - Número da página e endereço completo
            pag_url = "pagina=" + str(j)
            down_url = (MAINDOWN_URL + jornal_url + chr(38) + pag_url +
                        chr(38) + DAY_URL + chr(38) + END_URL)

            # Montagem do nome do arquivo
            num_pag = "00" + str(j)
            num_pag = num_pag[-3:]
            pdfname = "DOU" + str(i) + "_" + DT_FILE + "_pg" + num_pag + ".pdf"

            if pdfname in files_lst:
                # Download ignorado
                print("Progresso: DOU%s -- %s de %s | Download ignorado (duplicidade)"
                      %(str(i), str(j), str(pag_max)), end="\r")
            else:
                download_status = False
                while download_status is False:
                    try:
                        # Download do arquivo
                        pdf_path = dou_dir + "\\" + pdfname
                        with open(pdf_path, "wb") as pdf_file:
                            pdf_file.write(urlopen(down_url).read())
                        print("Progresso: DOU%s -- %s de %s | Download feito                    "
                              %(str(i), str(j), str(pag_max)), end="\r")
                        download_status = True
                        sleep(round(random()*3, 2))
                    except:
                        print("Progresso: DOU%s -- %s de %s | Erro download: Tentando novamente..."
                              %(str(i), str(j), str(pag_max)), end="\r")
                        sleep(round(random()*5, 2))

        print("Download DOU%s completo! \n" % str(i), end='\n')
else:
    print("Não há diários no dia de hoje \n")

# Verificação/Download de diário extra no dia anterior.

# Constantes gerais
ONTEM = ult_dia_semana(NUM_DIA, NUM_MES, NUM_ANO)
DT_ONTEM = ONTEM.strftime("%d/%m/%Y")
NUM_ANO = ONTEM.strftime("%Y")
NM_MES = ONTEM.strftime("%B")
NUM_MES = ONTEM.strftime("%m")
NUM_DIA = ONTEM.strftime("%d")
DT_FILE = ONTEM.strftime("%Y_%m_%d")
JORNAIS = [1000, 2000, 3000]

print("Verificando a existência de Edição Extra no dia %s..." % DT_ONTEM)

for i in JORNAIS:

    # URL - Número do Jornal
    jornal_url = "jornal=" + str(i)

    jornal = str(i)[0]

    # Verificação de existência da edição extra
    pag_num = 1
    pag_url = "pagina=" + str(pag_num)
    pag_url = MAINVISU_URL + jornal_url + chr(38) + pag_url + chr(38) + DAY_URL

    url = urlopen(pag_url)
    soup = BeautifulSoup(url, "html5lib")
    soup = soup.find("frame", {"name":"controlador"})

    if soup != None:
        # Definição do número máximo de páginas do DOU.
        pag_max = str(soup).split(";")[-1]
        pag_max = refindall(r"\d+", pag_max)
        pag_max = int(pag_max[0])

        print("Download DOU%s Edição Extra iniciado..." % jornal, end='\r')

        # Verificação/Montagem dos paths de download (DOU, ANO, MÊS, DIA)
        dou_dir = os.path.join(paths.PDF_DIR, "DOU"+jornal+"_EXTRA")
        dou_dir = check_dou_dirs(dou_dir, NUM_DIA, NM_MES, NUM_ANO)

        # Lista dos arquivos no diretório alvo (evitar duplicidade de download)
        files_lst = os.listdir(dou_dir)

        for j in range(1, pag_max+1):
            # URLs - Número da página e endereço completo
            pag_url = "pagina=" + str(j)
            down_url = (MAINDOWN_URL + jornal_url + chr(38) + pag_url +
                        chr(38) + DAY_URL + chr(38) + END_URL)

            # Montagem do nome do arquivo
            num_pag = "00" + str(j)
            num_pag = num_pag[-3:]
            pdfname = "DOU" + jornal + "_EXTRA_" + DT_FILE + "_pg" + num_pag + ".pdf"

            if pdfname in files_lst:
                # Download ignorado
                print("Progresso: DOU%s -- %s de %s | Download ignorado (duplicidade)"
                      %(str(i), str(j), str(pag_max)), end="\r")
            else:
                download_status = False
                while download_status is False:
                    try:
                        # Download do arquivo
                        pdf_path = dou_dir + "\\" + pdfname
                        with open(pdf_path, "wb") as pdf_file:
                            pdf_file.write(urlopen(down_url).read())
                        print("Progresso: DOU%s -- %s de %s | Download feito                      "
                              %(str(i), str(j), str(pag_max)), end="\r")
                        download_status = True
                        sleep(round(random()*3, 2))
                    except Exception as e:
                        # Falha no download
                        print("Progresso: DOU%s -- %s de %s | Erro download: Tentando novamente..."
                              %(str(i), str(j), str(pag_max)), end="\r")
                        sleep(round(random()*5, 2))

        print("Download DOU%s Edição Extra completo!" % jornal, end='\n')

    else:
        print("DOU%s Edição Extra não encontrado!" % jornal, end='\n')
