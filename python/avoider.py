"""
----- English -----
Script to list all pages 001 that are not converted.
----- Português (BR) -----
Script para listar as páginas 001 que ainda não encontram-se convertidas.
"""

# Packages
import os
import paths

def list_conversao_p1():
    """ Retorna os arquivos PDF referentes às páginas 1 que necessitam
        ser convertidas. Tipo do retorno: list """
    # Listar todos os PDF referentes às págs. 1
    pdf_001 = []
    for files in os.walk(paths.PDF_DIR):
        for files1 in files[2]:
            if files1.startswith("DOU") and files1.endswith("pg001.pdf"):
                pdf_001.append(files1)

    # Listar todos os TXT referentes às págs. 1
    txt_001 = []
    for files in os.walk(paths.TXT_DIR):
        for files1 in files[2]:
            if files1.startswith("DOU") and files1.endswith("pg001.txt"):
                txt_001.append(files1)

    # Listar PDFs (001) que necessitam ser convertidos em TXT
    pdf_conversao = []
    for files in pdf_001:
        files1 = files.replace("pdf", "txt")
        if files1 not in txt_001:
            pdf_conversao.append(files)
            print("%s.pdf: Adicionado à lista de conversão" % files)
    return pdf_conversao

def list_conversao_mapa():
    """ Retorna os arquivos PDF referentes às páginas 1 que necessitam
        ser convertidas. Tipo do retorno: list """
    # Listar todos os PDF referentes às págs. 1
    all_pdf = []
    for files in os.walk(paths.PDF_DIR):
        for files1 in files[2]:
            if files1.startswith("DOU"):
                all_pdf.append(files1)

    # Listar todos os TXT referentes às págs. 1
    all_txt = []
    for files in os.walk(paths.TXT_DIR):
        for files1 in files[2]:
            if files1.startswith("DOU"):
                all_txt.append(files1)

        # Listar PDFs do Mapa que necessitam ser convertidos em TXT
    # setlocale(LC_TIME, "")
    pdf_conversao = []
    pylog_mapa = os.path.join(paths.LOG_DIR, "pags-mapa.log")
    with open(pylog_mapa, "r") as logf:
        log_pags = logf.readlines()
    for line in log_pags:
        files, mapa_ini, mapa_fim = line.split("\t")
        if mapa_ini != "x" and mapa_fim != "x":
            for num_ in range(int(mapa_ini), int(mapa_fim)+1):
                filename_conv = files[:-7] + "{:03}".format(num_)
                if filename_conv+".txt" not in all_txt and filename_conv+".pdf" in all_pdf:
                    pdf_conversao.append(filename_conv+".pdf")
                    print("%s.pdf: Adicionado à lista de conversão" % filename_conv)
        else:
            print("%s: Mapa não encontrado" % files[:-10])
    return pdf_conversao
