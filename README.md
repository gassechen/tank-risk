Sistema Experto Híbrido: Evaluación de Integridad de Tanques (Tank-Integrity AI)
🎯 Descripción del Proyecto
Tank-Integrity AI es un Sistema Experto Híbrido diseñado para la toma de decisiones en tiempo real sobre la integridad mecánica de tanques de almacenamiento y recipientes a presión en la industria de hidrocarburos y química. Este sistema combina la precisión de modelos de Machine Learning (ML) con la auditabilidad y rigurosidad de la lógica basada en reglas (Sistemas Expertos/IA Simbólica), implementando un mecanismo de "Rule Gap" (Vacío de Conocimiento) que garantiza la cobertura total de casos.

El Problema que Resuelve
Los sistemas de inspección tradicionales a menudo fallan en escenarios de baja confianza o datos contradictorios (casos de borde), donde la recomendación debe ir más allá de las normas estándar (API 653, API 510). Este proyecto asegura que cada caso de incertidumbre detectado por los modelos de ML sea resuelto mediante la generación dinámica de conocimiento utilizando un Large Language Model (LLM) como componente de inferencia de respaldo.

🏗️ Arquitectura Neurosimbólica
El proyecto está diseñado bajo una arquitectura de "cerebro dual" que orquesta tres componentes principales a través de Common Lisp/LISA:

1. Núcleo Simbólico (Motor de Reglas LISA)
Tecnología: Common Lisp (CL) utilizando el shell de sistemas expertos LISA.

Función: Almacena y ejecuta las reglas de decisión de ingeniería basadas en normas (API 653/510, ASME). Es el componente de auditoría y decisión primaria.

2. Componentes Neuro (Modelos de Machine Learning)
Tecnología: Servidores Microservice (Python/Flask) que exponen endpoints (Puertos 5000, 5001).

Función: Proporcionan hechos de entrada al sistema (LISA):

Modelo de Riesgo (Risk Model): Predice la categoría de riesgo ("alto", "medio", "bajo") y, crucialmente, la Confianza (?c) de esa predicción.

Modelo de Corrosión (Corrosion Model): Predice la agresividad de la corrosión.

3. Mecanismo de Inferencia Híbrida (El LLM)
Tecnología: Servidor Microservice que aloja un Large Language Model (LLM) configurado con ingeniería de prompts.

Función: Es el mecanismo de fallback. Solo se activa si el motor LISA no puede asertar un CONCLUSION-FOUND debido a que la confianza de los modelos de ML es demasiado baja (el Rule Gap). El LLM genera y devuelve una nueva regla LISA, cerrando el vacío.

⚙️ Características Técnicas y Funcionalidad
Flujo Operacional (Inferencia Híbrida)
Activación: El frontend (LISA) recibe los inputs del tanque (evaluar-tanque ...).

Consulta ML: LISA consulta los Modelos de ML para obtener los hechos TANK-RISK y CORROSION-FACT.

Evaluación Simbólica: LISA ejecuta sus reglas de ingeniería (?c>0.6).

Detección de Vacío (Rule Gap):

Si Confianza ML >0.6: Una regla estándar se dispara, aserta CONCLUSION-FOUND y se finaliza.

Si Confianza ML ≤0.6 (Duda): El motor detecta la falta del hecho CONCLUSION-FOUND y suspende la ejecución.

Generación de Conocimiento: Se llama al LLM con los hechos y inputs del tanque. El LLM infiere una solución (acción + norma) y genera una nueva defrule LISA.

Actualización: La nueva regla se integra en la base de conocimientos, resolviendo permanentemente el punto ciego para futuras consultas.

Reglas Clave Generadas (Ejemplos de Vacíos Encontrados)
El sistema ha demostrado capacidad para generar reglas para escenarios complejos y de baja confianza, como:

API-580-BAJA-CONF: Riesgo Bajo, pero Confianza extremadamente baja (?c<0.4), requiriendo validación de datos (API 580).

API-510-ALTO-RIESGO-BAJA-CONF: Riesgo Alto, pero Confianza dudosa (?c=0.52), requiriendo re-inspección inmediata y recalcular el riesgo (API 510/580).

API-581-SCC-ALTO: Alto Riesgo por Condición Metalúrgica (Cloruros/Temperatura), requiriendo técnicas avanzadas de NDT para agrietamiento (API 581).

🛠️ Tecnologías Utilizadas
Motor de Reglas: LISA (Lisp-based Intelligent Software Agent)

Lenguaje Principal: Common Lisp

Microservicios ML/LLM: Python / Flask

Estándares de Ingeniería: API 653, API 510, API 580, ASME B31.3.

🚀 Próximos Pasos (Hoja de Ruta)
Integración Final: Implementar las correcciones del prompt y las reglas generadas en la base de conocimientos de producción.

Pruebas de Cierre: Realizar pruebas de regresión para asegurar que las reglas recién generadas no disparen falsos negativos o positivos.

Monitoreo del LLM: Implementar un registro detallado de las reglas generadas por el LLM para auditoría continua y refinamiento del prompt.
