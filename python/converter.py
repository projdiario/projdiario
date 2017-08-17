"""
----- English -----
Conversion script to transform PDF files to TXT and HTML files using Microsoft Word.
    Python Version: 3.6.1
    External Packages:
        1. PyWin32 (220)
           (PIP Installation: pip install pypiwin32)
    Recommendations:
        1. Change to 0 (zero) Microsoft Word option to show recent Documents.
           (Options > Advanced > Display > Show this number of Recent Documents).
           -------- Avoid memory issues.
        2. When converting large numbers of PDF files, avoid running the script
           with Windows Explorer opened. -------- Avoid memory issues.
        3. Read the documentation from all scripts used/imported for the convertion.
    Addons:
        1. Log creation on 'pylog' directory.

----- Português (BR) -----
Script para conversão de arquivos .PDF para .TXT e .HTML utilizando o Microsoft Word.
    Versão do Python: 3.6.1
    Pacotes Externos:
        1. PyWin32 (220)
           (Instalação PIP: pip install pypiwin32)
    Recomendações:
        1. Alterar para 0 (zero) a opção de mostrar documentos recentes no Word.
           (Opções > Avançado > Exibir > Mostrar este número de
            Documentos Recentes). -------- Evitar problemas de memória.
        2. Em conversões de grandes números de PDFs, evitar rodar o script com o
           Windows Explorer aberto. -------- Evitar problemas de memória.
        3. Ler a documentação de todos os scripts usados/importados na conversão.
    Adendos:
        1. Criação de log no diretório 'pylog'.
"""

# Import Packages
import os
import re
import sys
import time
import pywintypes
import paths
import avoider
import win32com.client as win32

# Word App
WORD = win32.gencache.EnsureDispatch('Word.Application')
WORD.Visible = True
TXT_FORMAT = win32.constants.wdFormatText
WORD.DisplayAlerts = win32.constants.wdAlertsNone

# Def
class AtributteTables(object):
    def __init__(self, num_table, qtd_col, qtd_row):
        self.num_table = num_table
        self.qtd_col = qtd_col
        self.qtd_row = qtd_row

def get_width(table_gw, row_gw, col_gw):
    """ Get the width of each cell (row, col).
    Atributes:
        1. table_gw: Number of the table.
        2. row_gw: Number of the row.
        3. col_gw: Number of the Column.
    """
    try:
        wc = int(active_doc.Tables(table_gw).Cell(Row=row_gw, Column=col_gw).Width)
    except:
        wc = 0
    return wc

def join_inner_list(lst_to_join):
    """ Join list inside of list to form only one list.
    WARNING! Works only for one set of list insede of list.
    Exemple: [[0, 0, 0, 1], 1, [1, 3]] - Yes
             [[0, [0, 0], 1], 1, [1, [3]]] - No
    Argument:
        1. lst_to_join: List
    """
    joined_list = []
    for bigger_list in lst_to_join:
        for strings_list in bigger_list:
            joined_list.append(strings_list)
    return joined_list

def mapa_index(lst_to_search):
    """ Search the string 'agric' in the given list.
    Argument:
        1. lst_to_search: List.
    """
    for ministerio in range(len(lst_to_search)):
        if 'agric' in lst_to_search[ministerio].lower():
            return ministerio

def log_writer(logfile_path, *args_to_write, sep=";", end_line=None):
    """ Write log entries (arguments) to a given log file with an
        specified separator.
        Arguments:
            1. logfile_path: Full path of the log file (txt).
            2. *args_to_write: Any argument that is needed to write in the log file.
            3. sep="separator": Separator used between each argument in log. [Default: ";"]
            4. end_line="separator": Determine whether or not there is a diferent separator
               at the end of each log string. [Default=None]
               End line example: New lines (\n); Tab (\t); etc.
    """
    log_string = ""
    for log_argument in args_to_write:
        log_argument_str = str(log_argument)
        if log_argument == args_to_write[0]:
            log_string = log_argument_str
        else:
            log_string = log_string + sep + log_argument_str
        if log_argument == args_to_write[len(args_to_write)-1]:
            str_end_line = str(end_line)
            log_line = log_string + str_end_line
            with open(logfile_path, 'a') as log_control:
                log_control.write(log_line)

def row_to_col(lst_rows, num_cols):
    len_dict_row = len(lst_rows)
    dict_col2 = []
    for cn in range(num_cols):
        col_atr = []
        for rn in range(len_dict_row):
            try:
                col_atr.append(lst_rows[rn][cn])
            except:
                col_atr.append(0)
        dict_col2.append(col_atr)
    return dict_col2

def col_to_row(lst_cols, num_rows):
    len_dict_cols = len(lst_cols)
    dict_row2 = []
    for rn in range(num_rows):
        row_atr = []
        for cn in range(len_dict_cols):
            row_atr.append(lst_cols[cn][rn])
        dict_row2.append(row_atr)
    return dict_row2

# Count PDF conversion list
PDF_CONVERTION_COUNT = len(avoider.PDF_CONVERSION_LIST)

# Creating Log file
TIME_LOG_FILE = time.strftime('%d-%m-%Y_%H-%M')
LOG_FILE = os.path.join(paths.LOG_DIR, 'log_'+TIME_LOG_FILE+'.txt')

# Create PDF, TXT and HTML path list
PDF_PATH_LIST = []
TXT_PATH_LIST = []

# Path creation loop
for i in range(PDF_CONVERTION_COUNT):
    FILENAME = avoider.PDF_CONVERSION_LIST[i]
    PDF_PATH = os.path.join(paths.PDF_DIR, FILENAME+'.pdf')
    TXT_PATH = os.path.join(paths.TXT_DIR, FILENAME+'.txt')
    PDF_PATH_LIST.append(PDF_PATH)
    TXT_PATH_LIST.append(TXT_PATH)

# Count PDF conversion list
PDF_PATH_COUNT = len(PDF_PATH_LIST)
PDF_PATH_LIST = [w.replace('\\', '\\\\') for w in PDF_PATH_LIST]
TXT_PATH_LIST = [w.replace('\\', '\\\\') for w in TXT_PATH_LIST]

# Conversion Loop TXT
for n in range(PDF_PATH_COUNT):

    # Loop: PAGE 001
    TIME_CONVERSION = time.strftime('%H:%M:%S')
    PDF_PG001 = PDF_PATH_LIST[n]
    TXT_PG001 = TXT_PATH_LIST[n]
    FILEWEX_PG001 = avoider.PDF_CONVERSION_LIST[n]
    FILENAME = FILEWEX_PG001
    WORD.Documents.Open(PDF_PG001)
    try:
        active_doc = WORD.ActiveDocument
        active_doc.SaveAs(TXT_PG001, FileFormat=TXT_FORMAT)
        active_doc.Close(False)
        log_writer(LOG_FILE, FILENAME, "OK", "NOT VERIFIED", TIME_CONVERSION,
                   sep="...", end_line="\n")
        try:
            with open(TXT_PG001) as file_txt:
                texto_pag001 = file_txt.readlines()
            sumario = list(filter(lambda x: re.findall(r'\.\.\.*? *?[0-9]+', x), texto_pag001))
            sumario_cleaned = [x.replace('.', '').replace('\n', '') for x in sumario]
            sumario_split = [list(filter(None, re.split(r'(\d+)', x))) for x in sumario_cleaned]
            sumario_split = join_inner_list(sumario_split)
            index_agri = mapa_index(sumario_split)
            mapa_start = int(sumario_split[index_agri+1])
            mapa_end = int(sumario_split[index_agri+3])
            print(FILENAME, mapa_start, mapa_end)
        except:
            pass

    # Loop: PAGE MAPA
        for pg_number in range(mapa_start, mapa_end+1):
            mapa_pdf = FILEWEX_PG001[:18] + "{0:03}".format(pg_number)
            mapa_pdf_path = os.path.join(paths.PDF_DIR, mapa_pdf+'.pdf')
            mapa_txt_path = os.path.join(paths.TXT_DIR, mapa_pdf+'.txt')
            LIST_REAL_TABLES = []
            TIME_CONVERSION = time.strftime('%H:%M:%S')
            try:
                FILENAME = mapa_pdf
                print(FILENAME)
                FILEPATH = mapa_pdf_path
                TXTPATH = mapa_txt_path
                WORD.Documents.Open(FILEPATH)
                active_doc = WORD.ActiveDocument

                active_doc.ShowGrammaticalErrors = False
                active_doc.ShowSpellingErrors = False
                active_doc.Paragraphs.LineSpacingRule = win32.constants.wdLineSpaceSingle
                active_doc.Paragraphs.SpaceBefore = 0
                active_doc.Paragraphs.SpaceAfter = 0

                ALL_TABLES = active_doc.Tables.Count
                for i in range(1, ALL_TABLES+1):
                    active_doc.Tables(i).Columns.PreferredWidth = 50
                    sum_col = active_doc.Tables(i).Columns.Count
                    sum_row = active_doc.Tables(i).Rows.Count
                    sum_cell = active_doc.Tables(i).Range.Cells.Count
                    if sum_cell > 1:
                        for k in range(1, sum_row+1):
                            for l in range(1, sum_col+1):
                                try:
                                    textCells = active_doc.Tables(i).Cell(Row=k, Column=l).Range.Text
                                    s_textCells = textCells.strip()
                                    textLength = len(s_textCells)
                                    if textLength < 500 and textLength > 2 and l > 1:
                                        LIST_REAL_TABLES.append(i)
                                except:
                                    pass
                try:
                    LIST_REAL_TABLES = list(set(LIST_REAL_TABLES))
                except:
                    LIST_REAL_TABLES = []

                for i in LIST_REAL_TABLES:

                    act_table = active_doc.Tables(i)
                    qtd_linha = act_table.Rows.Count
                    qtd_coluna = act_table.Columns.Count
                    atr = AtributteTables(i, qtd_coluna, qtd_linha)
                    dict_row = []
                    for j in range(1, atr.qtd_row+1):
                        lst_row = []
                        for k in range(1, atr.qtd_col+1):
                            tam = get_width(i, j, k)
                            lst_row.append(tam)
                        dict_row.append(lst_row)


                    matriz = []
                    word_col = 0
                    float_cols = atr.qtd_col

                    while word_col < float_cols:

                        add_float = True

                        matriz_da_coluna = []

                        teste = row_to_col(dict_row, float_cols)
                        coluna = teste[word_col]

                        # Achar o tamanho mínimo maior que zero da coluna!
                        # Try para ignorar tabelas que ganham colunas transitórias.
                        try:
                            sem_zero = [x for x in coluna if x != 0]
                            minimo = min(sem_zero)

                            # Rodar célula a célula na coluna
                            for linha in range(atr.qtd_row):
                                celula = coluna[linha]
                                if celula == minimo:
                                    matriz_da_coluna.append('<td>')

                                elif celula > minimo:
                                    if celula == 66666666:
                                        matriz_da_coluna.append(matriz[word_col-1][linha])

                                    else:

                                        cont_rowspan = 2
                                        add_cell = 1
                                        soma_rowspan = minimo
                                        see_next_col = True

                                        while see_next_col is True:

                                            if word_col+add_cell > float_cols-1:
                                                see_next_col = False

                                            else:

                                                celula_retirada = teste[word_col+add_cell].pop(linha)

                                                # Achar o tamanho mínimo maopr que zero da próxima coluna
                                                proxima_col_sem_zero = [x for x in teste[word_col+add_cell] if x != 0]
                                                minimo_proxima_col = min(proxima_col_sem_zero)

                                                teste[word_col+add_cell].insert(linha, celula_retirada)

                                                if soma_rowspan + minimo_proxima_col in range(celula-2, celula+3):
                                                    see_next_col = False

                                                else:
                                                    cont_rowspan += 1
                                                    add_cell += 1
                                                    soma_rowspan += minimo_proxima_col

                                            text_rowspan = '<td colspan="' + str(cont_rowspan) + '">'

                                        matriz_da_coluna.append(text_rowspan)
                                        dict_row[linha][word_col] = minimo

                                    # Modifica o dado da base da linha para corrigir o erro das próximas comparações!
                                        for ajst_rowspan in range(1, cont_rowspan):
                                            dict_row[linha].insert(word_col+ajst_rowspan, 66666666)
                                            if dict_row[linha][-1] == 0 and float_cols == atr.qtd_col:
                                                dict_row[linha] = dict_row[linha][:-1]
                                            else:
                                                if add_float is True:
                                                    float_cols += 1
                                                    add_float = False

                                else:        # Celula é mesclada verticalmente
                                    if coluna[linha-1] == 0:
                                        matriz_da_coluna.append(matriz_da_coluna[linha-1])
                                    else:
                                        cont_colspan = 2
                                        add_cell = 1
                                        see_next_cell = True
                                        while see_next_cell is True:
                                            index_next = linha + add_cell
                                            if index_next > atr.qtd_row-1:
                                                see_next_cell = False
                                            elif coluna[index_next] == 0:
                                                cont_colspan += 1
                                                add_cell += 1
                                            else:
                                                see_next_cell = False
                                            text_colspan = '<td rowspan="' + str(cont_colspan) + '">'
                                        matriz_da_coluna.append(text_colspan)
                                        matriz_da_coluna[linha-1] = text_colspan

                            matriz.append(matriz_da_coluna)
                            word_col += 1
                        
                        except:
                            word_col += 1

                    tags_to_rows = col_to_row(matriz, atr.qtd_row)
                    len_tags_rows = len(tags_to_rows)
                    for k in range(len_tags_rows):
                        rows_tags = tags_to_rows[k]
                        len2 = len(tags_to_rows[k])
                        delete_itens = []
                        for j in range(len2):
                            try:
                                if 'colspan' in rows_tags[j]:
                                    del_range = [int(x) for x in rows_tags[j].split(sep='"') if x.isdigit()]
                                    cont = 1
                                    while cont < del_range[0]:
                                        del[rows_tags[j+1]]
                                        cont += 1
                            except:
                                pass
                            
                    tr_index = []
                    for k in range(atr.qtd_row):
                        tr_row = dict_row[k]
                        tr_len = len(tr_row)
                        add_tr = 0
                        sub_tr = -1
                        check_next = True
                        check_before = True
                        tr_beg_end = []
                        while check_next is True:
                            tr_cell = tr_row[add_tr]
                            if tr_cell == 0:
                                add_tr += 1
                            else:
                                tr_beg_end.append(add_tr+1)
                                check_next = False
                        while check_before is True:
                            tr_end = [x for x in tr_row if x != 66666666]
                            tr_cell = tr_end[sub_tr]
                            if tr_cell == 0:
                                sub_tr -= 1
                            else:
                                tr_beg_end.append(len(tr_end)+sub_tr+1)
                                check_before = False
                        tr_index.append(tr_beg_end)

                    for k in range(1, atr.qtd_row+1):
                        for j in range(1, atr.qtd_col+1):
                            try:
                                act_table.Cell(Row=k, Column=j).Range.InsertBefore(tags_to_rows[k-1][j-1])
                                act_table.Cell(Row=k, Column=j).Range.InsertAfter('</td>')
                                if j == tr_index[i-1][0]:
                                    act_table.Cell(Row=k, Column=j).Range.InsertBefore('<tr>')
                                    if k == 1:
                                        act_table.Cell(Row=k, Column=j).Range.InsertBefore('<table>')
                                elif j == tr_index[k-1][1]:
                                    act_table.Cell(Row=k, Column=j).Range.InsertAfter('</tr>')
                                    if k == atr.qtd_row:
                                        act_table.Cell(Row=k, Column=j).Range.InsertAfter('</table><br>')

                            except:
                                pass

                active_doc.SaveAs(TXTPATH, FileFormat=TXT_FORMAT)
                active_doc.Close(False)

    # Log documentation:
                if len(LIST_REAL_TABLES) > 0:
                    log_writer(LOG_FILE, FILENAME, "OK", "TABLES",
                               TIME_CONVERSION, sep="...", end_line="\n")
                else:
                    log_writer(LOG_FILE, FILENAME, "OK", "NO TABLES",
                               TIME_CONVERSION, sep="...", end_line="\n")
            except pywintypes.com_error as com_error:
                try:
                    active_doc.SaveAs(TXTPATH, FileFormat=TXT_FORMAT)
                    active_doc.Close(False)
                    COM_ERROR_LOG = com_error.excepinfo[2].replace('\n', '')
                    if len(LIST_REAL_TABLES) > 0:
                        log_writer(LOG_FILE, FILENAME, "OK:"+COM_ERROR_LOG,
                                   "TABLES", TIME_CONVERSION, sep="...", end_line="\n")
                    else:
                        log_writer(LOG_FILE, FILENAME, "OK:"+COM_ERROR_LOG,
                                   "NO TABLES", TIME_CONVERSION, sep="...", end_line="\n")
                except:
                    COM_ERROR_LOG = com_error.excepinfo[2].replace('\n', '')
                    if len(LIST_REAL_TABLES) > 0:
                        log_writer(LOG_FILE, FILENAME, "ERRO:"+COM_ERROR_LOG,
                                   "TABLES", TIME_CONVERSION, sep="...", end_line="\n")
                    else:
                        log_writer(LOG_FILE, FILENAME, "ERRO:"+COM_ERROR_LOG,
                                   "NO TABLES", TIME_CONVERSION, sep="...", end_line="\n")
            except:
                try:
                    active_doc.SaveAs(TXTPATH, FileFormat=TXT_FORMAT)
                    active_doc.Close(False)
                    ERROR_TYPE = sys.exc_info()[0].__name__
                    if len(LIST_REAL_TABLES) > 0:
                        log_writer(LOG_FILE, FILENAME, "OK:"+ERROR_TYPE,
                                   "TABLES", TIME_CONVERSION, sep="...", end_line="\n")
                    else:
                        log_writer(LOG_FILE, FILENAME, "OK:"+ERROR_TYPE,
                                   "NO TABLES", TIME_CONVERSION, sep="...", end_line="\n")
                except:
                    ERROR_TYPE = sys.exc_info()[0].__name__
                    if len(LIST_REAL_TABLES) > 0:
                        log_writer(LOG_FILE, FILENAME, "ERRO:"+ERROR_TYPE,
                                   "TABLES", TIME_CONVERSION, sep="...", end_line="\n")
                    else:
                        log_writer(LOG_FILE, FILENAME, "ERRO:"+ERROR_TYPE,
                                   "NO TABLES", TIME_CONVERSION, sep="...", end_line="\n")
    except:
        COM_ERROR_LOG = com_error.excepinfo[2].replace('\n', '')
        log_writer(LOG_FILE, FILENAME, "ERRO:"+COM_ERROR_LOG,
                   "NOT VERIFIED", TIME_CONVERSION, sep="...", end_line="\n")

    # Restart Word Application (Memory rest)
    if n % 200 == 0 and n > 0:
        WORD.Quit()
        time.sleep(1)
        WORD = win32.gencache.EnsureDispatch('Word.Application')

try:
    active_doc.Close(False)
except:
    pass

WORD.Quit()