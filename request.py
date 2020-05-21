import requests
import json
url = 'http://localhost:5000/predict_api'
r = requests.post(url,json={'BM':2, 'Cement_CaO':9, 'Cl_C3A_x':6,'Cl_C3s':2, 'GA_Dosage_x':9, 'C_Blaine':6,'RP_Power':2, 'PA_prop':9, 'Gypsum_Purity':6,'Cement_So3_x':2})

print(r.json())