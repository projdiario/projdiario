""" Script destinado à criação do log de armazenamento dos dados
    de início e fim da seção do Mapa em cada dia do DOU. """

import os
import re
import paths

def criar_log_pags_mapa():
    """ Cria arquivo log listando todos os arquivos txt (pag. 001)
        com o início e fim da seção referente ao Mapa """
    # Arquivo padrão
    log_file = os.path.join(paths.LOG_DIR, "pags-mapa.log")
    with open(log_file, "w") as logf:
        logf.write("")

    # Obtenção de todos os caminhos possíveis nas pastas de txt.
    paths_txt = [x[0] for x in os.walk(paths.TXT_DIR) if
                 len(x[0].split("\\")) > 6
                 and x[0].split("\\")[-6] == "dados"
                ]

    for txtpath in paths_txt:
        # Retira arquivos tmps incorretos.
        list_txt = [x for x in os.listdir(txtpath) if not x.startswith("~")]

        # Filtra apenas as páginas nº 1 (sumários).
        list_txt = [x for x in os.listdir(txtpath) if x.endswith("pg001.txt")]

        for txt_file in list_txt:
            # Criação do path do arquivo.
            filepath = os.path.join(txtpath, txt_file)

            # Leitura do arquivo.
            with open(filepath, "r") as file_txt:
                texto_pag001 = file_txt.readlines()

            # Filtragem e detecção do Mapa no sumário.
            sumario = list(filter(lambda x: re.findall(r'\.\.\.*? *?[0-9]+', x), texto_pag001))
            sumario = [x.replace('.', '').replace('\n', '') for x in sumario]
            sumario = [list(filter(None, re.split(r'(\d+)', x))) for x in sumario]
            sumario = [x.strip() for j in sumario for x in j]
            try:
                index_agri = [sumario.index(x) for x in sumario
                              if "agric" in x.lower()][0]
                mapa_start = int(sumario[index_agri+1])
                try:
                    mapa_end = int(sumario[index_agri+3])
                except ValueError:
                    mapa_end = int(sumario[index_agri+4])
                with open(log_file, "a") as logf:
                    logf.write(txt_file + "\t" + str(mapa_start) + "\t" + str(mapa_end) + "\n")

            # Caso o não seja encontrado o Mapa no sumário.
            except IndexError:
                with open(log_file, "a") as logf:
                    logf.write(txt_file + "\tx\tx\n")
