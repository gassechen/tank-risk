import socket
import json
import pandas as pd
import numpy as np
from sklearn.ensemble import RandomForestClassifier
from sklearn.preprocessing import LabelEncoder
from sklearn.impute import SimpleImputer
import joblib
import os

# Definir paths para guardar el modelo y preprocesadores
MODEL_PATH = "corrosion_model.pkl"
PREPROCESSOR_PATH = "corrosion_preprocessor.pkl"
DATA_PATH = "corrosion_data.pkl"

# Cargar o preparar la base de datos
if os.path.exists(MODEL_PATH) and os.path.exists(PREPROCESSOR_PATH):
    print("Cargando modelo y preprocesadores de corrosión existentes...")
    model = joblib.load(MODEL_PATH)
    label_encoders, imputer = joblib.load(PREPROCESSOR_PATH)
else:
    print("Preparando datos y entrenando nuevo modelo de corrosión...")
    data = pd.read_csv('CORR-DATA_Database.csv', low_memory=False)
    features = ['Environment', 'Material Group', 'Material Family', 'Material', 
                'Concentration (Vol %)', 'Temperature (deg C)']
    target = 'Rate (mm/yr) or Rating'

    data = data[data[target].isin(['A (Resistant)', 'B (Good)', 'C (Questionable)', 'D (Poor)'])]
    data[target] = data[target].map({
        'A (Resistant)': 0, 'B (Good)': 1, 'C (Questionable)': 2, 'D (Poor)': 3
    })

    label_encoders = {}
    for col in ['Environment', 'Material Group', 'Material Family', 'Material']:
        le = LabelEncoder()
        data[col] = le.fit_transform(data[col].astype(str))
        label_encoders[col] = le

    data['Concentration (Vol %)'] = pd.to_numeric(data['Concentration (Vol %)'], errors='coerce')
    data['Temperature (deg C)'] = pd.to_numeric(data['Temperature (deg C)'], errors='coerce')
    imputer = SimpleImputer(strategy='mean')
    data[features] = imputer.fit_transform(data[features])

    X = data[features]
    y = data[target]
    model = RandomForestClassifier(n_estimators=50, random_state=42)
    model.fit(X, y)

    joblib.dump(model, MODEL_PATH)
    joblib.dump((label_encoders, imputer), PREPROCESSOR_PATH)
    joblib.dump((X, y), DATA_PATH)

def predict_corrosion(data):
    try:
        inputs = pd.DataFrame([[
            data["environment"], 
            data["material_group"], 
            data["material_family"], 
            data["material"], 
            data["concentration"], 
            data["temperature"]
        ]], columns=['Environment', 'Material Group', 'Material Family', 'Material', 
                     'Concentration (Vol %)', 'Temperature (deg C)'])
        print(f"Entradas crudas: {inputs.to_dict(orient='records')}")  # Log de entradas
        for col in ['Environment', 'Material Group', 'Material Family', 'Material']:
            try:
                inputs[col] = label_encoders[col].transform([inputs[col].iloc[0]])
            except:
                inputs[col] = 0  # Valor por defecto
        # Aplicar imputación manteniendo DataFrame
        inputs = pd.DataFrame(imputer.transform(inputs), columns=inputs.columns)
        print(f"Entradas preprocesadas: {inputs.to_dict(orient='records')}")  # Log de preprocesadas
        pred = model.predict(inputs)[0]
        confidence = model.predict_proba(inputs).max()
        rating_map = {0: "A (Resistant)", 1: "B (Good)", 2: "C (Questionable)", 3: "D (Poor)"}
        corrosion_level = "alta" if pred >= 2 else "baja"
        print(f"Predicción: {rating_map[pred]}, Confianza: {confidence}")  # Log de predicción
        return {
            "rating": rating_map[pred],
            "corrosion_level": corrosion_level,
            "confidence": float(confidence),
            "fact": f"(hecho (corrosion {corrosion_level}) (material {data['material']}) (entorno {data['environment']}))"
        }
    except Exception as e:
        print(f"Error en predicción: {str(e)}")  # Log de errores
        return {"error": str(e)}

def start_corrosion_server(host="127.0.0.1", port=5001):
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
        s.bind((host, port))
        s.listen()
        print(f"Servidor de corrosión activo en {host}:{port}")
        while True:
            conn, addr = s.accept()
            with conn:
                print(f"Conexión desde {addr}")
                data = conn.recv(1024).decode()
                try:
                    parsed = json.loads(data)
                    result = predict_corrosion(parsed)
                except json.JSONDecodeError:
                    result = {"error": "JSON inválido"}
                conn.sendall((json.dumps(result) + "\n").encode())

if __name__ == "__main__":
    start_corrosion_server()
