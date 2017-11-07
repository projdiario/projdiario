"""
----- English -----
Script objective: Stablish paths to be used on the project.
----- Português (BR) -----
Objetivo do script: Estabelecer os endereços dos diretórios a serem
utilizados no projeto.
"""

# Packages
import os

def checkdir(dir_path):
    """ Verifica a existência da pasta,
        caso não exista cria a pasta designada
    """
    if not os.path.exists(dir_path):
        os.makedirs(dir_path)

# Python scripts directory
SCRIPT_DIR = os.path.dirname(__file__)

# Main SISLEGAL directory
MAIN_DIR = SCRIPT_DIR.replace('\\python', '')
checkdir(MAIN_DIR)

# PDF directory
PDF_DIR = os.path.join(MAIN_DIR, 'dados\\pdf')
checkdir(PDF_DIR)

# TXT directory
TXT_DIR = os.path.join(MAIN_DIR, 'dados\\txt')
checkdir(TXT_DIR)

# Log directory
LOG_DIR = os.path.join(SCRIPT_DIR, 'pylog')
checkdir(LOG_DIR)
