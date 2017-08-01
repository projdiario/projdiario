"""
----- English -----
Script objective: Stablish paths to be used on the project.
----- Português (BR) -----
Objetivo do script: Estabelecer os endereços dos diretórios a serem
utilizados no projeto.
"""

# Packages
import os

# Python scripts directory
SCRIPT_DIR = os.path.dirname(__file__)

# Main SISLEGAL directory
MAIN_DIR = SCRIPT_DIR.replace('\\python', '')

# PDF directory
PDF_DIR = os.path.join(MAIN_DIR, 'dados\\pdf')

# TXT directory
TXT_DIR = os.path.join(MAIN_DIR, 'dados\\txt')

# HTML directory
HTML_DIR = os.path.join(MAIN_DIR, 'dados\\html')

# Log directory
LOG_DIR = os.path.join(SCRIPT_DIR, 'pylog')

# os.chdir(PDF_DIR)
