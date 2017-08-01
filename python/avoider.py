"""
----- English -----
Script made to avoid the conversion of PDF files that are already converted.
----- Português (BR) -----
Script para evitar a conversão de arquivos .PDF que já encontram-se convertidos.
"""

# Packages
import os
import paths

# Detect files within directorys: PDF, TXT and HTML (WITH EXTENTIONS)
LIST_PDF_FILES = os.listdir(paths.PDF_DIR)
LIST_TXT_FILES = os.listdir(paths.TXT_DIR)
# LIST_HTML_FILES = listdir(paths.HTML_DIR)

# Clean files ~
CLEAN_LIST_PDF_FILES = [x for x in LIST_PDF_FILES if not x.startswith('~')]
CLEAN_LIST_TXT_FILES = [x for x in LIST_TXT_FILES if not x.startswith('~')]
# CLEAN_LIST_HTML_FILES = [x for x in LIST_HTML_FILES if not x.startswith('~')]

# Find DOU pg001
LIST_PDF_PG001 = [x for x in CLEAN_LIST_PDF_FILES if x.find('pg001') != -1]

# Files within directorys: PDF, TXT and HTML (WITHOUT EXTENTIONS)
LIST_PDF_PG001_NO_EXT = [w.replace('.pdf', '') for w in LIST_PDF_PG001]
LIST_TXT_FILES_NO_EXT = [w.replace('.txt', '') for w in CLEAN_LIST_TXT_FILES]
# LIST_HTML_FILES_NO_EXT = [w.replace('.html', '') for w in CLEAN_LIST_HTML_FILES]

# Count files within directorys: PDF
PDF_PG001_COUNT = len(LIST_PDF_PG001_NO_EXT)

# Checker
PDF_CONVERSION_LIST = [LIST_PDF_PG001_NO_EXT[i] for i in range(PDF_PG001_COUNT)
                       if LIST_PDF_PG001_NO_EXT[i] not in LIST_TXT_FILES_NO_EXT]
