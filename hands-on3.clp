(deftemplate disease
   (slot name) ; Nombre de la enfermedad
   (slot category) ; Viral o Bacterial
   (slot symptoms) ; Lista de síntomas
)

; Base inicial de hechos con las enfermedades
(deffacts initial-diseases
   (disease (name "Influenza") (category "Viral") (symptoms "Fever, cough, sore throat, fatigue, muscle aches"))
   (disease (name "Pneumonia") (category "Bacterial") (symptoms "Cough with phlegm, high fever, chest pain, shortness of breath"))
   (disease (name "Tuberculosis") (category "Bacterial") (symptoms "Persistent cough, fever, night sweats, weight loss"))
   (disease (name "COVID-19") (category "Viral") (symptoms "Fever, dry cough, fatigue, shortness of breath, loss of taste or smell"))
   (disease (name "Chickenpox") (category "Viral") (symptoms "Itchy rash, fever, fatigue, loss of appetite"))
   (disease (name "Diphtheria") (category "Bacterial") (symptoms "Sore throat, fever, breathing difficulty, weakness"))
   (disease (name "Salmonellosis") (category "Bacterial") (symptoms "Diarrhea, fever, abdominal pain, vomiting"))
   (disease (name "Hepatitis A") (category "Viral") (symptoms "Jaundice, fever, nausea, appetite loss, fatigue"))
   (disease (name "Tetanus") (category "Bacterial") (symptoms "Muscle stiffness, spasms, fever, swallowing difficulty"))
   (disease (name "Measles") (category "Viral") (symptoms "Rash, high fever, cough, conjunctivitis, Koplik spots"))
)

; List all diseases
(defrule list-diseases
   ?d <- (disease (name ?disease-name) (category ?type) (symptoms ?disease-symptoms))
   =>
   (printout t "Disease: " ?disease-name ", Type: " ?type ", Symptoms: " ?disease-symptoms crlf)
)

; Add a new disease
(deffunction add-disease (?new-name ?new-type ?new-symptoms)
   (assert (disease (name ?new-name) (category ?new-type) (symptoms ?new-symptoms)))
   (printout t "Disease added: " ?new-name crlf)
)

; Update an existing disease
(deffunction update-disease (?existing-name ?updated-type ?updated-symptoms)
   (bind ?disease-fact (find-fact ((?fact disease)) (eq ?fact:name ?existing-name)))
   (if ?disease-fact then
      (retract ?disease-fact)
      (assert (disease (name ?existing-name) (category ?updated-type) (symptoms ?updated-symptoms)))
      (printout t "Disease updated: " ?existing-name crlf)
   else
      (printout t "Error: Disease not found: " ?existing-name crlf))
)

; Delete an existing disease
(deffunction delete-disease (?disease-to-delete)
   (bind ?disease-fact (find-fact ((?fact disease)) (eq ?fact:name ?disease-to-delete)))
   (if ?disease-fact then
      (retract ?disease-fact)
      (printout t "Disease deleted: " ?disease-to-delete crlf)
   else
      (printout t "Error: Disease not found: " ?disease-to-delete crlf))
)

; Search diseases by symptoms
(defrule search-disease-by-symptoms
   (declare (salience 20)) ; Higher priority for searching
   (disease (name ?disease-name) (symptoms $?disease-symptoms))
   =>
   (printout t "Disease found: " ?disease-name crlf)
)

; Example of searching diseases by a specific symptom
(deffunction search-by-symptom (?input-symptom)
   (printout t "Searching diseases with symptom: " ?input-symptom crlf)
   (run)
)
