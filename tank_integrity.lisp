;; Cargar dependencias
(ql:quickload :lisa)
(ql:quickload :usocket)
(ql:quickload :yason)
(ql:quickload :uiop)


;; Definir paquete
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (not (find-package "TANK-INTEGRITY"))
    (defpackage "TANK-INTEGRITY"
      (:use "LISA-LISP")
      (:export "EVALUAR-TANQUE"))))

(in-package "TANK-INTEGRITY")

;; Inicializar motor de inferencia
(make-inference-engine)

;; Definir template para hechos
(deftemplate tank-risk ()
  (slot riesgo)
  (slot confianza)
  (slot espesor))

(deftemplate corrosion-fact ()
  (slot corrosion)
  (slot material)
  (slot entorno)
  (slot confianza))

(deftemplate conclusion-found ()
 (slot source))



;; Función para calcular espesor mínimo según API 653 (Sección 9.2, simplificado)
(defun calcular-espesor-minimo (altura diametro gravedad-especifica)
  ;; t_min = (2.6 * H * D * G) / (S * E), asumimos S=24800 psi (acero), E=1.0
  (/ (* 2.6 altura diametro gravedad-especifica) (* 24800 1.0)))

;; Reglas según API 653
(defrule riesgo-bajo ()
  (tank-risk (riesgo ?r) (confianza ?c) (espesor ?t))
  (test (string= ?r "bajo"))
  (test (> ?c 0.6))
  (test (> ?t (calcular-espesor-minimo 40 100 0.85))) ; Altura=40ft, Diámetro=100ft, G=0.85
  =>
  (format t "Riesgo bajo. Continuar operación normal. Inspección externa cada 5 años (API 653, Sección 6.4.2).~%")
  (assert (conclusion-found (source "API-653-BAJO"))))

(defrule riesgo-medio-inspeccion ()
  (tank-risk (riesgo ?r) (confianza ?c) (espesor ?t))
  (test (string= ?r "medio"))
  (test (> ?c 0.6))
  (test (> ?t (calcular-espesor-minimo 40 100 0.85)))
  =>
  (format t "Riesgo medio. Programar inspección visual y ultrasónica en 6 meses (API 653, Anexo F - RBI).~%")
  (assert (conclusion-found (source "API-653-MEDIO-INSP"))))


(defrule riesgo-medio-espesor-critico ()
  (tank-risk (riesgo ?r) (confianza ?c) (espesor ?t))
  (test (string= ?r "medio"))
  (test (> ?c 0.6))
  (test (<= ?t (calcular-espesor-minimo 40 100 0.85)))
  =>
  (format t "Riesgo medio, espesor crítico. Programar inspección interna inmediata (API 653, Sección 4.3).~%")
  (assert (conclusion-found (source "API-653-MEDIO-CRITICO"))))

(defrule riesgo-alto ()
  (tank-risk (riesgo ?r) (confianza ?c) (espesor ?t))
  (test (string= ?r "alto"))
  (test (> ?c 0.6))
  =>
  (format t "Riesgo alto. Suspender operación y reparar inmediatamente (API 653, Sección 9.2).~%")
  (assert (conclusion-found (source "API-653-ALTO"))))


;; Reglas para corrosión (ASME B31.3 y API 510)
(defrule corrosion-baja ()
  (corrosion-fact (corrosion ?c) (material ?m) (entorno ?e) (confianza ?conf))
  (test (string= ?c "baja"))
  (test (> ?conf 0.6))
  =>
  (format t "Corrosión baja (~a en ~a). Monitoreo anual (ASME B31.3).~%" ?m ?e)
  (assert (conclusion-found (source "ASME-B31.3-BAJA-CORR"))))

(defrule corrosion-alta ()
  (corrosion-fact (corrosion ?c) (material ?m) (entorno ?e) (confianza ?conf))
  (test (string= ?c "alta"))
  (test (> ?conf 0.6))
  =>
  (format t "Corrosión alta (~a en ~a). Inspección inmediata (API 510, Sección 5.5).~%" ?m ?e)
  (assert (conclusion-found (source "ASME-B31.3-BAJA-CORR"))))


(defrule riesgo-medio-y-corrosion-alta ()
  (tank-risk (riesgo ?r) (confianza ?c1) (espesor ?t))
  (corrosion-fact (corrosion ?c) (material ?m) (entorno ?e) (confianza ?c2))
  (test (string= ?r "medio"))
  (test (string= ?c "alta"))
  (test (> ?c1 0.6))
  (test (> ?c2 0.6))
  =>
  (format t "Riesgo medio y corrosión alta (~a en ~a). Inspección inmediata y evaluación RBI (API 653 y API 510).~%" ?m ?e)
  (assert (conclusion-found (source "API-510-ALTA-CORR"))))




(defun start-backend-servers ()
  "Ejecuta el script start_servers.sh y espera un momento para que los servicios estén listos."
  (format t "~% Iniciando los servidores Python de Machine Learning y LLM...~%")
  (handler-case 
      (uiop:run-program "./start_servers.sh" 
                        :output :nil
                        :error :interactive 
                        :wait nil)
    (error (e)
      (format t " Error al intentar ejecutar el script de inicio: ~A~%" e)
      (return-from start-backend-servers nil)))
  (format t "⏳ Esperando 5 segundos para que los servidores estén completamente activos...~%")
  (sleep 5) 
  
  (format t "✅ Servidores iniciados y LISP listo para la evaluación.~%")
  t) 


;; Función para hacer la solicitud TCP al servidor Python
(defun client-tcp (host port json-string)
  (let ((socket (usocket:socket-connect host port :element-type 'character)))
    (unwind-protect
         (let ((stream (usocket:socket-stream socket)))
           ;; Enviar JSON más newline
           (format stream "~A~%" json-string)
           (force-output stream)

           ;; Leer respuesta hasta newline
           (let ((line (read-line stream nil nil)))
             (when line
               (yason:parse line :object-as :hash-table))))
      (usocket:socket-close socket))))


(defun llamar-modelo-riesgo (espesor temperatura anos tipo-fluido presion recubrimiento proteccion-catodica)
  (let ((payload (with-output-to-string (out)
                   (let ((obj (make-hash-table :test 'equal)))
                     (setf (gethash "espesor" obj) espesor
                           (gethash "temperatura" obj) temperatura
                           (gethash "anos" obj) anos
                           (gethash "tipo_fluido" obj) tipo-fluido
                           (gethash "presion" obj) presion
                           (gethash "recubrimiento" obj) recubrimiento
                           (gethash "proteccion_catodica" obj) proteccion-catodica)
                     (yason:encode obj out)))))
    ;;(format t "~%Payload enviado: ~A~%" payload)
    (client-tcp "127.0.0.1" 5000 payload)))


;; Llamar al servidor de corrosión
(defun llamar-modelo-corrosion (environment material-group material-family material concentration temperature)
  (let ((payload (with-output-to-string (out)
                   (let ((obj (make-hash-table :test 'equal)))
                     (setf (gethash "environment" obj) environment
                           (gethash "material_group" obj) material-group
                           (gethash "material_family" obj) material-family
                           (gethash "material" obj) material
                           (gethash "concentration" obj) concentration
                           (gethash "temperature" obj) temperature)
                     (yason:encode obj out)))))
    (client-tcp "127.0.0.1" 5001 payload)))

(defun llamar-modelo-llm (eval-input fallos-output)
  (let ((payload (with-output-to-string (out)
                   (let ((obj (make-hash-table :test 'equal)))
                     (setf (gethash "eval_input" obj) eval-input)
                     (setf (gethash "fallos_output" obj) 
                           (let ((fo-obj (make-hash-table :test 'equal)))
                             (setf (gethash "riesgo" fo-obj) (gethash "riesgo" fallos-output)
                                   (gethash "confianza" fo-obj) (gethash "confianza" fallos-output)
                                   (gethash "espesor" fo-obj) (gethash "espesor" fallos-output))
                             fo-obj))
                     (yason:encode obj out)))))
    
    (format t "~% RULE GAP DETECTED: Calling LLM Inference Service (Port 5002)...~%")
    (client-tcp "127.0.0.1" 5002 payload)))



(defun get-facts-output-string ()
  "Redirige la salida impresa de (facts) a una cadena de texto."
  
  (with-output-to-string (s)
    ;; Temporalmente redirige *standard-output* (donde se imprime facts) a la cadena 's'
    (let ((*standard-output* s))
      ;; La función FACTS se ejecuta e imprime su contenido en 's'
      (facts))))


(defun fact-of-template-exists (template-name-string)
  "Busca el tag del template (ej. '#<CONCLUSION-FOUND') en la salida de (facts)."
  (let ((facts-string (get-facts-output-string))
        ;; Prepara la etiqueta de búsqueda, ej: "#<CONCLUSION-FOUND"
        (search-tag (format nil "#<~A" template-name-string)))
    ;; Utiliza SEARCH para ver si el tag existe en el output de facts
    (if (search search-tag facts-string :test #'string=)
        t
        nil)))





(defun evaluar-tanque (espesor temperatura anos tipo-fluido presion recubrimiento proteccion-catodica
		       environment material-group material-family material concentration)
  (reset)
  
  (let* ((riesgo-resultado (llamar-modelo-riesgo espesor temperatura anos tipo-fluido presion recubrimiento proteccion-catodica))
         (corrosion-resultado (llamar-modelo-corrosion environment material-group material-family material concentration temperatura))
         (eval-input-list (list espesor temperatura anos tipo-fluido presion recubrimiento proteccion-catodica
                                environment material-group material-family material concentration))
         (fallos-output-ht (make-hash-table :test 'equal)))

    ;; Asertar hechos de riesgo y corrosión
    (assert (tank-risk (riesgo (gethash "riesgo" riesgo-resultado))
                       (confianza (gethash "confianza" riesgo-resultado))
                       (espesor (gethash "espesor" riesgo-resultado))))
                       
    (assert (corrosion-fact (corrosion (gethash "corrosion_level" corrosion-resultado))
                            (material (gethash "material" corrosion-resultado material))
                            (entorno (gethash "environment" corrosion-resultado environment))
                            (confianza (gethash "confidence" corrosion-resultado))))

    ;; 4. Disparar el motor de reglas
    (run)
    
    ;; 5. DETECCIÓN DE VACÍO DE CONOCIMIENTO (Rule Gap)
    ;; Si no se ha asertado ningún hecho 'conclusion-found' después de 'run', llama al LLM.
    (unless (fact-of-template-exists "CONCLUSION-FOUND")
        (format t "~%--- 🧠 VACÍO DE CONOCIMIENTO DETECTADO. INFERENCIA NEUROSIMBÓLICA ---~%")
        
        ;; Llenar el Hash-Table fallos-output-ht
        (setf (gethash "riesgo" fallos-output-ht) (gethash "riesgo" riesgo-resultado)
              (gethash "confianza" fallos-output-ht) (gethash "confianza" riesgo-resultado)
              (gethash "espesor" fallos-output-ht) (gethash "espesor" riesgo-resultado))
              
        ;; Llamar al servidor de inferencia del LLM (Port 5002)
        (let ((llm-response (llamar-modelo-llm eval-input-list fallos-output-ht)))
            
            (if (string= (gethash "status" llm-response) "success")
                (progn
                    (format t "✨ LLM Generó Regla: ~A~%" (gethash "rule_name" llm-response))
                    (format t "--------------------------------------------------------~%")
                    (format t "~A~%" (gethash "lisa_rule" llm-response))
                    (format t "--------------------------------------------------------~%")
                    (format t "➡️ ¡NUEVA REGLA LISTA PARA AÑADIR A LA BASE DE CONOCIMIENTO!~%"))
                (format t "❌ LLM Error: ~A~%" (gethash "message" llm-response)))))))


;; ;; Función para evaluar un tanque
;; (defun evaluar-tanque (espesor temperatura anos tipo-fluido presion recubrimiento proteccion-catodica
;;                       environment material-group material-family material concentration)
;;   (reset)
;;   ;; Consultar modelo de riesgo
;;   (let ((riesgo-resultado (llamar-modelo-riesgo espesor temperatura anos tipo-fluido presion recubrimiento proteccion-catodica)))
    
;;     (assert (tank-risk (riesgo (gethash "riesgo" riesgo-resultado))
;;                        (confianza (gethash "confianza" riesgo-resultado))
;;                        (espesor (gethash "espesor" riesgo-resultado)))))
;;   ;; Consultar modelo de corrosión
;;   (let ((corrosion-resultado (llamar-modelo-corrosion environment material-group material-family material concentration temperatura)))
    
;;     (assert (corrosion-fact (corrosion (gethash "corrosion_level" corrosion-resultado))
;;                             (material (gethash "material" corrosion-resultado material))
;;                             (entorno (gethash "environment" corrosion-resultado environment))
;;                             (confianza (gethash "confidence" corrosion-resultado)))))
;;   (run))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;DEBUG;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun evaluar-tanque-debug (espesor temperatura anos tipo-fluido presion recubrimiento proteccion-catodica
                      environment material-group material-family material concentration)
  (reset)
  (let ((riesgo-resultado (llamar-modelo-riesgo espesor temperatura anos tipo-fluido presion recubrimiento proteccion-catodica)))
    (format t "Resultado riesgo: ~a~%" riesgo-resultado)
    (maphash (lambda (k v) (format t "  ~a: ~a~%" k v)) riesgo-resultado) ; Añadir esto
    (assert (tank-risk (riesgo (gethash "riesgo" riesgo-resultado))
                       (confianza (gethash "confianza" riesgo-resultado))
                       (espesor (gethash "espesor" riesgo-resultado)))))
  (let ((corrosion-resultado (llamar-modelo-corrosion environment material-group material-family material concentration temperatura)))
    (format t "Resultado corrosión: ~a~%" corrosion-resultado)
    (maphash (lambda (k v) (format t "  ~a: ~a~%" k v)) corrosion-resultado) ; Añadir esto
    (assert (corrosion-fact (corrosion (gethash "corrosion_level" corrosion-resultado))
                            (material (gethash "material" corrosion-resultado material))
                            (entorno (gethash "environment" corrosion-resultado environment))
                            (confianza (gethash "confidence" corrosion-resultado))))
  (run)))




(uiop:launch-program "./start_servers.sh")

;; Prueba
(evaluar-tanque 8.0 60 10 1 20 0 0 "Acetic Acid" "Stainless steels" "Austenitic" "316" 90)
(evaluar-tanque 8.0 180 10 1 20 0 0 "Sulfuric Acid" "Carbon steels" "Ferritic" "A36" 80)
(evaluar-tanque 1.0 180 50 1 200 1 0 "Sulfuric Acid" "Carbon steels" "Ferritic" "A36" 80)

(evaluar-tanque 5.0 100 40 3 50 1 1 "Kerosene" "Carbon steels" "Ferritic" "A36" 50)

(evaluar-tanque 30.0 60 5 1 5 0 0 "Acetic Acid" "Stainless steels" "Austenitic" "316" 90)

(evaluar-tanque 7.5 150 25 1 80 0 0 "Sulfuric Acid" "Carbon steels" "Ferritic" "A36" 50)

(evaluar-tanque 20.0 40 2 1 10 0 0 "Cloruro de Magnesio" "Carbon steels" "Ferritic" "A36" 95)

;; crea llm rule
(evaluar-tanque 30.0 60 5 1 500 0 0 "Acetic Acid" "Stainless steels" "Austenitic" "316" 90)
(evaluar-tanque 6.0 200 35 1 10 1 1 "Water" "Carbon steels" "Ferritic" "A516" 50)
(evaluar-tanque 7.0 300 5 1 10 0 0 "Chlorides" "Stainless steels" "Austenitic" "304L" 99)


;;(fact-of-template-exists "CONCLUSION-FOUND")
