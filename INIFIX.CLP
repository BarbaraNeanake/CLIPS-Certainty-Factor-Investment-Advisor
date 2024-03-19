;; Barbara Neanake A.
;; 22/494495/TK/54328

;; DEFFUNCTIONS
(deffunction next-question (?question ?allowed-values)
   (printout t ?question crlf)
   (printout t "Jawaban: ")
   (bind ?reply (read))
   (printout t crlf)
   (if (numberp ?reply) then (bind ?reply (min 1.0 (max -1.0 ?reply))) else (bind ?reply ""))
   (while (not (numberp ?reply)) do
      (printout t ?question crlf)
      (printout t "Balasan (masukkan angka desimal di antara -1 dan 1): ")
      (bind ?reply (read))
      (printout t crlf)
      (if (numberp ?reply) then (bind ?reply (min 1.0 (max -1.0 ?reply))) else (bind ?reply "")))
   ?reply)

;; INIT
(defrule start
  (declare (salience 10000))
  =>
  (set-fact-duplication TRUE)
  (printout t "" crlf)
  (printout t "************************************" crlf)
  (printout t "              Hello!^^              " crlf)
  (printout t "Welcome to Nea's Consultant Academy!" crlf)
  (printout t "************************************" crlf crlf)
  (printout t "Sistem ini akan membantu teman-teman mahasiswa dalam menganalisis keuangan dan instrumen investasi yang sesuai." crlf crlf))

;; Form of the facts
(deftemplate a-fact
   (slot name)
   (slot cf (default 0.0)))

;; Certainty factor combination rules
(defrule combine-certainties-1
  ?fact1 <- (a-fact (name ?id) (cf ?cf1))
  ?fact2 <- (a-fact (name ?id) (cf ?cf2))
  (test (neq ?fact1 ?fact2))
  (test (> ?cf1 0.0))
  (test (> ?cf2 0.0))
  =>
  (retract ?fact1)
  (modify ?fact2 (cf (+ ?cf1 (* 0.8 (- 1.0 ?cf1 ?cf2))))))

(defrule combine-certainties-2
  ?fact1 <- (a-fact (name ?id) (cf ?cf1))
  ?fact2 <- (a-fact (name ?id) (cf ?cf2))
  (test (neq ?fact1 ?fact2))
  (test (< ?cf1 0.0))
  (test (< ?cf2 0.0))
  =>
  (retract ?fact1)
  (modify ?fact2 (cf (+ ?cf1 (* 0.8 (+ 1.0 ?cf1 ?cf2))))))

(defrule combine-certainties-3
  ?fact1 <- (a-fact (name ?id) (cf ?cf1))
  ?fact2 <- (a-fact (name ?id) (cf ?cf2))
  (test (neq ?fact1 ?fact2))
  (test (> ?cf1 0.0))
  (test (< ?cf2 0.0))
  =>
  (retract ?fact1)
  (modify ?fact2 (cf (/ (+ ?cf1 ?cf2) (- 1.0 (min (abs ?cf1) (abs ?cf2)))))))

;; Form of the questions
(deftemplate questionnaire
   (slot a-fact (default ?NONE))
   (slot the-questionnaire (default ?NONE))
   (slot already-asked (default FALSE)))

;; Rule for asking a question
(defrule ask-a-questionnaire
   ?f <- (questionnaire (already-asked FALSE)
                        (the-questionnaire ?the-questionnaire)
                        (a-fact ?the-fact))
   =>
   (modify ?f (already-asked TRUE))
   (bind ?accepted (create$ -1.0 -0.8 -0.6 -0.4 0.0 0.4 0.6 0.8 1.0))
      (assert (a-fact (name ?the-fact) (cf (next-question ?the-questionnaire ?accepted)))))

;; List of questions
(deffacts questionnaire-facts
  (questionnaire (a-fact risk-knowledge)
                 (the-questionnaire "Seberapa baik pengetahuan Anda tentang risiko investasi? (skala -1 hingga 1)"))
  (questionnaire (a-fact risk-tolerance)
                 (the-questionnaire "Seberapa nyaman Anda mengambil risiko dalam investasi? (skala -1 hingga 1)"))
  (questionnaire (a-fact investment-size)
                 (the-questionnaire "Berapa besar dana yang akan Anda investasikan? (skala -1 hingga 1)"))
  (questionnaire (a-fact willingness-to-learn)
                 (the-questionnaire "Apakah Anda bersedia belajar lebih tentang investasi? (skala -1 hingga 1)"))
  (questionnaire (a-fact financial-confidence)
                 (the-questionnaire "Seberapa percaya diri Anda dalam mengambil keputusan finansial? (skala -1 hingga 1)")))

;; Inference rules for investment recommendations
(defrule recommend-investment
  (declare (salience -10))
  ?risk-knowledge <- (a-fact (name risk-knowledge) (cf ?knowledge))
  ?risk-tolerance <- (a-fact (name risk-tolerance) (cf ?tolerance))
  ?investment-size <- (a-fact (name investment-size) (cf ?size))
  ?willingness-to-learn <- (a-fact (name willingness-to-learn) (cf ?learn))
  ?financial-confidence <- (a-fact (name financial-confidence) (cf ?confidence))
  =>
  (bind ?reksadana-cf
         (+ (* 0.2 ?knowledge)
            (* 0.3 ?tolerance)
            (* 0.2 ?size)
            (* 0.1 ?learn)
            (* 0.2 ?confidence)))
  (bind ?saham-cf
         (+ (* 0.3 ?knowledge)
            (* 0.2 ?tolerance)
            (* 0.1 ?size)
            (* 0.2 ?learn)
            (* 0.2 ?confidence)))
  (bind ?obligasi-cf
         (+ (* 0.1 ?knowledge)
            (* 0.1 ?tolerance)
            (* 0.3 ?size)
            (* 0.2 ?learn)
            (* 0.3 ?confidence)))
  (bind ?emas-cf
         (+ (* 0.1 ?knowledge)
            (* 0.2 ?tolerance)
            (* 0.1 ?size)
            (* 0.3 ?learn)
            (* 0.3 ?confidence)))
  (bind ?properti-cf
         (+ (* 0.1 ?knowledge)
            (* 0.1 ?tolerance)
            (* 0.1 ?size)
            (* 0.3 ?learn)
            (* 0.4 ?confidence)))
  (printout t "Okay! terima kasih sudah mengisi data profil keuangan anda." crlf)
  (printout t "Berikut merupakan rekomendasi instrumen investasi bagi kondisi anda:" crlf crlf)
  (if (> ?reksadana-cf 0.5)
    then
    (printout t "----------------------------" crlf)
    (printout t "REKOMENDASI REKSADANA: Anda memiliki profil yang cocok untuk menggunakan instrumen investasi Reksadana." crlf)
    (printout t "Kecocokanmu untuk menggunakan instrumen investasi Reksadana adalah sebesar = " ?reksadana-cf crlf)
    (printout t "Ini menunjukkan tingkat kesesuaian yang sangat tinggi, yang berarti Anda memiliki pemahaman yang baik tentang risiko investasi, toleransi risiko yang tinggi, dan ukuran investasi yang signifikan." crlf)
    (printout t "----------------------------" crlf crlf))
  (if (> ?saham-cf 0.3)
    then
    (printout t "----------------------------" crlf)
    (printout t "REKOMENDASI SAHAM: Anda memiliki profil yang cocok untuk menggunakan instrumen investasi Saham." crlf)
    (printout t "Kecocokanmu untuk menggunakan instrumen investasi Saham adalah sebesar = " ?saham-cf crlf)
    (printout t "Ini menunjukkan tingkat kesesuaian yang cukup tinggi, yang berarti Anda memiliki pemahaman yang baik tentang risiko investasi dan toleransi risiko yang moderat." crlf)
    (printout t "----------------------------" crlf crlf ))
  (if (> ?obligasi-cf 0)
    then
    (printout t "----------------------------" crlf)
    (printout t "REKOMENDASI OBLIGASI: Anda memiliki profil yang cocok untuk menggunakan instrumen investasi Obligasi." crlf)
    (printout t "Kecocokanmu untuk menggunakan instrumen investasi Obligasi adalah sebesar = " ?obligasi-cf crlf)
    (printout t "Ini menunjukkan tingkat kesesuaian yang cukup, yang berarti Anda memiliki pemahaman yang cukup tentang risiko investasi dan toleransi risiko yang rendah." crlf)
    (printout t "----------------------------" crlf crlf))
  (if (> ?emas-cf -0.3)
    then
    (printout t "----------------------------" crlf)
    (printout t "REKOMENDASI EMAS: Anda memiliki profil yang cocok untuk menggunakan instrumen investasi Emas." crlf)
    (printout t "Kecocokanmu untuk menggunakan instrumen investasi Emas adalah sebesar = " ?emas-cf crlf)
    (printout t "Ini menunjukkan tingkat kesesuaian yang rendah, yang berarti Anda memiliki pemahaman yang terbatas tentang risiko investasi dan toleransi risiko yang rendah." crlf)
    (printout t "----------------------------" crlf crlf))
  (if (> ?properti-cf -0.5)
    then
    (printout t "----------------------------" crlf)
    (printout t "REKOMENDASI PROPERTI: Anda memiliki profil yang cocok untuk menggunakan instrumen investasi Properti." crlf)
    (printout t "Kecocokanmu untuk menggunakan instrumen investasi Properti adalah sebesar = " ?properti-cf crlf)
    (printout t "Ini menunjukkan tingkat kesesuaian yang sangat rendah, yang berarti Anda memiliki pemahaman yang sangat terbatas tentang risiko investasi dan toleransi risiko yang sangat rendah." crlf)
    (printout t "----------------------------" crlf crlf)))

;; Final rule to end the program
(defrule finish
  (declare (salience -100))
  =>
  (printout t "Terima kasih telah menggunakan layanan Nea's Consultant Academy, pakar keuangan dan instrumen investasi." crlf)
  (printout t "Goodluck dengan instrumen investasi pilihanmu!^^" crlf crlf)
  (printout t "**********************************************" crlf)
  (halt))

