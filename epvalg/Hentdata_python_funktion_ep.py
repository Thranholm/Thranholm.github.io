def hent_data_ep(xml_url, geo_kreds = "Storkredse", stemmer = "parti", valg_type = "fintælling"):
  
  geo_kreds = geo_kreds.lower().capitalize()
  stemmer = stemmer.lower().capitalize()
  
  ## Tjekker at værdier er gyldige
  if stemmer not in ["Parti", "Person"]:
    raise TypeError("Stemmer skal være enten 'Parti' eller 'Person'.")
  
  if geo_kreds not in ["Land", "Landsdele", "Storkredse", "Opstillingskredse", "Afstemningsomraader"]:
    raise TypeError("Der er angivet ugyldig geo_kreds, det skal være en af: 'Land', 'Landsdele', 'Storkredse', 'Opstillingskredse', 'Afstemningsomraader'.")
  
  # if stemmer == "Person" and geo_kreds not in ["Storkredse", "Opstillingskredse", "Afstemningsomraader"]:
  #   raise TypeError("Der kan kun hentes personlige stemmer for geo_kreds: 'Storkredse', 'Opstillingskredse' eller 'Afstemningsomraader'.")
  
  ## Henter xml
  response = requests.get(xml_url)
  data = xmltodict.parse(response.content)
  
  ## Geo_kreds muligheder: Landsdele, Storkredse, Opstillingskredse, Afstemningsomraader, Land
  temp_geo_kreds = geo_kreds[:-1] if geo_kreds != "Land" else ""
  
  ## TODO: Virker ikke for land
  #geo_xml = data["Data"][geo_kreds][temp_geo_kreds]
  geo_xml = data["Data"][geo_kreds] if geo_kreds == "Land" else data["Data"][geo_kreds][temp_geo_kreds]
  
  if (valg_type == "valgaften"):
    xml = [item["@filnavn"] for item in geo_xml] if geo_kreds != "Land" else geo_xml[1]["@filnavn"]
  else:
    xml = [item["@filnavn"] for item in geo_xml] if geo_kreds != "Land" else geo_xml["@filnavn"]

  ## Laver xml til dict
  resp = list(map(requests.get, xml)) if geo_kreds != "Land" else requests.get(xml)
  xml_dict = [xmltodict.parse(resp[item].content) for item in range(len(resp))] if geo_kreds != "Land" else xmltodict.parse(resp.content)

  if (stemmer == "Person"): 
    temp_stemmer = "Personer" 
  elif (stemmer == "Parti"): 
    temp_stemmer = "Stemmer"
  else:
    raise TypeError("Ikke gyldig værdi i stemmer, skal være 'Parti' eller 'Person'")
  exit

  if (xml_dict["Data"]["Status"]["#text"] == "Resultatet foreligger endnu ikke."):
    text = "Resultat ikke klar endnu"
    return(text)


  if geo_kreds != "Land":
    data_liste = [pd.Series(xml_dict[item]["Data"]["Sted"]).to_frame().transpose().join(pd.json_normalize(pd.Series(xml_dict[item]["Data"][temp_stemmer]["Parti"])), rsuffix = "Parti", how = "cross") for item in range(len(xml_dict))]
  else:
    data_liste = [pd.Series(xml_dict["Data"]["Sted"]).to_frame().transpose().join(pd.json_normalize(pd.Series(xml_dict["Data"][temp_stemmer]["Parti"])), rsuffix = "Parti", how = "cross")]
# pd.concat(data_liste)

  if stemmer == "Parti":
    data_stemmer = pd.concat(data_liste)
  elif stemmer == "Person":
    data_frame = pd.concat(data_liste).explode("Person", ignore_index = True)
    data_stemmer = data_frame.join(pd.json_normalize(data_frame["Person"]), lsuffix = "Parti")
  exit
  
  data_stemmer.columns = data_stemmer.columns.str.replace("#|@|\\.", "", regex = True)

  # Fjerner variable
  data_stemmer.drop(["Person"], axis = 1, inplace = True, errors = "ignore")

  data_stemmer.rename(columns={"text": "Storkreds", "navn": "Parti", "StemmerIAlt": "PersonligeStemmerIAlt"}, inplace = True, errors = "ignore")
  return(data_stemmer)
