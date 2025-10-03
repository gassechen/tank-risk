import socket
import json
import numpy as np
from sklearn.ensemble import RandomForestClassifier
import joblib
import os

# Definir paths para guardar el modelo
MODEL_PATH = "risk_model.pkl"
DATA_PATH = "risk_data.pkl"

# Datos de entrenamiento (mismos que en tu código)
X = np.array([
    [10.0, 50, 5, 0, 15, 1, 1],
    [8.0, 60, 10, 1, 20, 0, 0],
    [12.0, 40, 3, 0, 10, 1, 1],
    [7.0, 70, 15, 1, 25, 0, 0],
    [9.0, 55, 8, 0, 18, 1, 0],
    [6.0, 65, 12, 1, 22, 0, 0],
    [11.0, 45, 4, 0, 12, 1, 1],
    [5.0, 75, 20, 1, 30, 0, 0],
    [9.5, 50, 7, 0, 15, 1, 1],
    [7.5, 62, 13, 1, 20, 0, 1],
    [10.5, 48, 6, 0, 14, 1, 0],
    [6.5, 68, 18, 1, 28, 0, 0]
])
y = np.array([0, 1, 0, 2, 1, 2, 0, 2, 0, 1, 0, 2])

# Cargar o entrenar el modelo
if os.path.exists(MODEL_PATH):
    print("Cargando modelo de riesgo existente...")
    model = joblib.load(MODEL_PATH)
else:
    print("Entrenando nuevo modelo de riesgo...")
    model = RandomForestClassifier(n_estimators=100, random_state=42)
    model.fit(X, y)
    joblib.dump(model, MODEL_PATH)
    joblib.dump((X, y), DATA_PATH)  # Guardar datos para referencia

risk_map = {0: "bajo", 1: "medio", 2: "alto"}

def predict_risk(data):
    try:
        features = np.array([[data["espesor"], data["temperatura"], data["anos"],
                              data["tipo_fluido"], data["presion"],
                              data["recubrimiento"], data["proteccion_catodica"]]])
        risk_level = model.predict(features)[0]
        confidence = model.predict_proba(features).max()
        return {
            "riesgo": risk_map[risk_level],
            "confianza": float(confidence),
            "espesor": float(data["espesor"])
        }
    except Exception as e:
        return {"error": str(e)}

def start_server(host="127.0.0.1", port=5000):
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
        s.bind((host, port))
        s.listen()
        print(f"Servidor de riesgo activo en {host}:{port}")
        while True:
            conn, addr = s.accept()
            with conn:
                print(f"Conexión desde {addr}")
                data = conn.recv(1024).decode()
                try:
                    parsed = json.loads(data)
                    result = predict_risk(parsed)
                except json.JSONDecodeError:
                    result = {"error": "JSON inválido"}
                conn.sendall((json.dumps(result) + "\n").encode())

if __name__ == "__main__":
    start_server()
