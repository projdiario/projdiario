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
from statistics import mode
import pywintypes
import paths
import avoider
import win32com.client as win32

# Word App
WORD = win32.gencache.EnsureDispatch('Word.Application')
WORD.Visible = False
TXT_FORMAT = win32.constants.wdFormatText

# Def
def dict_col_width(sum_cols, sum_rows, table):
    """
    Create a dictionary with the integer mode width for all columns in table.
    Format: {column_number: [mode_width],
             column_number: [mode_width],
             (...)}
    Arguments:
        1. sum_col: Sum of all columns in the desired table.
        2. sum_row: Sum of all rows in the desired table.
        3. table: Number of the active table in Word.
    """
    col_width = {}
    for col_n in range(1, sum_cols+1):
        col_nor = []
        for row_n in range(1, sum_rows+1):
            try:
                width_a = active_doc.Tables(table).Cell(Row=row_n, Column=col_n).Width
                col_nor.append(int(width_a))
            except:
                width_a = 0
                col_nor.append(width_a)
        col_nor = [item for item in col_nor if item != 0]
        try:
            col_nor = mode(col_nor)
        except:
            col_nor = min(col_nor)
        col_width[col_n] = col_nor
    return col_width

def merged_cells(sum_cols, sum_rows, table):
    """
    Create a dictionary with all integer widths from the lines with merged cells.
    Format: {row_with_merged_cells: [col1_width, col2_width, col3_width],
             row_with_merged_cells: [col1_width, col2_width, col3_width],
             (...)}
    Arguments:
        1. sum_col: Sum of all columns in the desired table.
        2. sum_row: Sum of all rows in the desired table.
        3. table: Number of the active table in Word.
    """
    dict_width = {}
    for row_nu in range(1, sum_rows+1):
        widths_row = []
        for col_nu in range(1, sum_cols+1):
            try:
                width_b = int(active_doc.Tables(table).Cell(Row=row_nu, Column=col_nu).Width)
                widths_row.append(width_b)
            except:
                width_b = 0
                widths_row.append(width_b)
        if 0 in widths_row:
            dict_width[row_nu] = widths_row
        else:
            pass
    return dict_width

def table_tags(row, col, table, sum_rows, sum_cols):
    """
    Insert HTML table tags (table, tr and td) in the selected cell.
    Arguments:
        1. row: Number of the row where the tag will be inserted.
        2. column: Number of the column where the tag will be inserted.
        3. table: Number of the active table in Word.
        4. sum_col: Sum of all columns in the active table.
        5. sum_row: Sum of all rows in the active table.
    """
    active_doc.Tables(table).Cell(Row=row, Column=col).Range.InsertBefore("<td>")
    active_doc.Tables(table).Cell(Row=row, Column=col).Range.InsertAfter("</td>")
    if col == 1:
        active_doc.Tables(table).Cell(Row=row, Column=1).Range.InsertBefore("<tr>")
        if row == 1:
            active_doc.Tables(table).Cell(Row=1, Column=1).Range.InsertBefore("<table>")
    if col == sum_cols:
        active_doc.Tables(table).Cell(Row=row, Column=sum_col).Range.InsertAfter("</tr>")
        if row == sum_rows:
            active_doc.Tables(table).Cell(Row=sum_rows, Column=sum_cols).Range.InsertAfter("</table>")

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

# Count PDF conversion list
PDF_CONVERTION_COUNT = len(avoider.PDF_CONVERSION_LIST)

# Creating Log file
TIME_LOG_FILE = time.strftime('%d-%m-%Y_%H-%M')
LOG_FILE = os.path.join(paths.LOG_DIR, 'log_'+TIME_LOG_FILE+'.txt')

# Create PDF, TXT and HTML path list
PDF_PATH_LIST = []
TXT_PATH_LIST = []
# HTML_PATH_LIST = []

# Path creation loop
for i in range(PDF_CONVERTION_COUNT):
    FILENAME = avoider.PDF_CONVERSION_LIST[i]
    PDF_PATH = os.path.join(paths.PDF_DIR, FILENAME+'.pdf')
    TXT_PATH = os.path.join(paths.TXT_DIR, FILENAME+'.txt')
    # HTML_PATH = os.path.join(paths.HTML_DIR, FILENAME+'.html')
    PDF_PATH_LIST.append(PDF_PATH)
    TXT_PATH_LIST.append(TXT_PATH)
    # HTML_PATH_LIST.append(HTML_PATH)

# Count PDF conversion list
PDF_PATH_COUNT = len(PDF_PATH_LIST)
PDF_PATH_LIST = [w.replace('\\', '\\\\') for w in PDF_PATH_LIST]
TXT_PATH_LIST = [w.replace('\\', '\\\\') for w in TXT_PATH_LIST]
# HTML_PATH_LIST = [w.replace('\\', '\\\\') for w in HTML_PATH_LIST]

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
            print(mapa_start, mapa_end)
        except:
            pass

    # Loop: PAGE MAPA
        for pg_number in range(mapa_start, mapa_end+1):
            mapa_pdf = FILEWEX_PG001[:18] + "{0:03}".format(pg_number)
            mapa_pdf_path = os.path.join(paths.PDF_DIR, mapa_pdf+'.pdf')
            mapa_txt_path = os.path.join(paths.TXT_DIR, mapa_pdf+'.txt')
            LIST_REAL_TABLES = []
            try:
                FILENAME = mapa_pdf
                FILEPATH = mapa_pdf_path
                TXTPATH = mapa_txt_path
                WORD.Documents.Open(FILEPATH)
                active_doc = WORD.ActiveDocument
                ALL_TABLES = active_doc.Tables.Count
                for i in range(1, ALL_TABLES+1):
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
                if len(LIST_REAL_TABLES) > 0:
                    for i in LIST_REAL_TABLES:
                        sum_col = active_doc.Tables(i).Columns.Count
                        sum_row = active_doc.Tables(i).Rows.Count
                        sum_cell = active_doc.Tables(i).Range.Cells.Count
                        if sum_cell < (sum_col * sum_row):
                            def_width = dict_col_width(sum_col, sum_row, i)
                            target_rows = merged_cells(sum_col, sum_row, i)
                            for j in range(1, sum_row+1):
                                if j in target_rows:
                                    for k in range(1, sum_col+1):
                                        try:
                                            width = target_rows[j][k-1]
                                            if width > def_width[k]:
                                                active_doc.Tables(i).Cell(Row=j, Column=k).Split(NumRows=1, NumColumns=2)
                                                table_tags(j, k, i, sum_row, sum_col)
                                                target_rows[j].insert(k, def_width[k+1])
                                                target_rows[j].remove(0)
                                            else:
                                                table_tags(j, k, i, sum_row, sum_col)
                                        except:
                                            try:
                                                active_doc.Tables(i).Cell(Row=j-1, Column=k).Split(NumRows=2, NumColumns=1)
                                                table_tags(j, k, i, sum_row, sum_col)
                                            except:
                                                pass
                                else:
                                    for k in range(1, sum_col+1):
                                        table_tags(j, k, i, sum_row, sum_col)
                        else:
                            for j in range(1, sum_row+1):
                                for k in range(1, sum_col+1):
                                    table_tags(j, k, i, sum_row, sum_col)
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
