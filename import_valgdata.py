## Henter xml-data med Python

import pandas as pd
import numpy as np
import requests
import xmltodict

# Læser funktion
exec(open("Hentdata_python_funktion.py").read())

valg_url = "https://www.dst.dk/valg/Valg1968094/xml/fintal.xml"
landsdele = hent_data(xml_url = valg_url, geo_kreds = "Landsdele", stemmer = "Parti")
storkredse_parti = hent_data(xml_url = valg_url, geo_kreds = "storkredse", stemmer = "parti")
storkredse_person = hent_data(xml_url = valg_url, geo_kreds = "storkredse", stemmer = "person")


