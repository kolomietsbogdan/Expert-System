(defrule start (initial-fact)
=>
(printout t crlf)
(printout t "======================================================")
(printout t crlf)
(printout t "Vacation Country Support System")
(printout t crlf)
(printout t "Answer questions yes/no or numbers when it necessary.")
(printout t crlf)
(printout t "======================================================")
(printout t crlf crlf))

;;****************
;;* DEFFUNCTIONS *
;;****************


(deffunction ask-question (?question $?allowed-values)
   (printout t ?question)
   (bind ?answer (read))
   (if (lexemep ?answer) 
       then (bind ?answer (lowcase ?answer)))
   (while (not (member$ ?answer ?allowed-values)) do
      (printout t ?question)
      (bind ?answer (read))
      (if (lexemep ?answer) 
          then (bind ?answer (lowcase ?answer))))
   ?answer)

(deffunction yes_no (?question)
   (bind ?response (ask-question ?question yes no y n))
   (if (or (eq ?response yes) (eq ?response y))
       then yes 
       else no))

(deffunction cost (?question)
   (bind ?response (ask-question ?question 500 100))
   (if (eq ?response 500)
       then true 
       else false))

;;;***************************
;;;* START *
;;;***************************

(defrule climate ""
   (not (warm ?))
   =>
   (assert (warm (yes_no "Do you prefer warm climate? (yes/no) "))))

;;;***************************
;;;* LEFT WING ANSWERS *
;;;***************************

(defrule money_at_all ""
   (warm yes)
   =>
   (assert (money_at_all (yes_no "Do you have money for a vacation at all? (yes/no) "))))

(defrule check_money_at_all ""
   (money_at_all no)
   =>
   (assert (country "backyard at granny's dacha.")))

(defrule like_guitars ""
   (money_at_all yes)
   =>
   (assert (like_guitars (yes_no "Do you like guitar? (yes/no) "))))

(defrule check_like_guitars ""
   (like_guitars yes)
   =>
   (assert (country "Spain.")))

(defrule like_football ""
   (like_guitars no)
   =>
   (assert (like_football (yes_no "Do you like play football? (yes/no) "))))

(defrule check_like_football ""
   (like_football yes)
   =>
   (assert (country "Portugal.")))

(defrule like_history ""
   (like_football no)
   =>
   (assert (like_history (yes_no "Do you like history? (yes/no) "))))
;;;***************************
;;;* LEFT-LEFT WING ANSWERS *
;;;***************************
(defrule pax_romana ""
   (like_history yes)
   =>
   (assert (pax_romana (yes_no "Pax Romana? (yes/no) "))))

(defrule check_pax_romana_yes ""
   (pax_romana yes)
   =>
   (assert (country "Italy.")))

(defrule check_pax_romana_no ""
   (pax_romana no)
   =>
   (assert (country "Greece.")))
;;;***************************
;;;* LEFT-RIGHT WING ANSWERS *
;;;***************************
(defrule vive_la_france ""
   (like_history no)
   =>
   (assert (vive_la_france (yes_no "Vive la France? (yes/no) "))))

(defrule check_vive_la_france_yes ""
   (vive_la_france yes)
   =>
   (assert (country "France.")))

(defrule spend_a_day ""
   (vive_la_france no)
   =>
   (assert (spend_a_day (cost "How much can you afford to spend on a vacation a day in USD: 100 or 500? (number) "))))

(defrule check_spend_a_day_rich ""
   (spend_a_day true)
   =>
   (assert (country "Monaco.")))

(defrule check_spend_a_day_normal ""
   (spend_a_day false)
   =>
   (assert (country "Croatia.")))

;;;***************************
;;;* RIGHT WING ANSWERS *
;;;***************************

(defrule love_hockey ""
   (warm no)
   =>
   (assert (love_hockey (yes_no "Do you like hockey? (yes/no) "))))

(defrule check_love_hockey_yes ""
   (love_hockey yes)
   =>
   (assert (country "Canada.")))

(defrule love_horses ""
   (love_hockey no)
   =>
   (assert (love_horses (yes_no "Do you like horses? (yes/no) "))))

(defrule check_love_horses_yes ""
   (love_horses yes)
   =>
   (assert (country "Mongolia.")))

(defrule love_anime ""
   (love_horses no)
   =>
   (assert (love_anime (yes_no "But you definitely love anime, don't you? (yes/no) "))))

(defrule check_love_anime_yes ""
   (love_anime yes)
   =>
   (assert (country "Japan.")))

(defrule love_asia ""
   (love_anime no)
   =>
   (assert (love_asia (yes_no "Do you like Asia at all? (yes/no) "))))

;;;***************************
;;;* RIGHT-LEFT WING ANSWERS *
;;;***************************

(defrule like_eastern_dragons ""
   (love_asia yes)
   =>
   (assert (like_eastern_dragons (yes_no "Do you like eastern image of the dragons? (yes/no) "))))

(defrule check_like_eastern_dragons_yes ""
   (like_eastern_dragons yes)
   =>
   (assert (country "China.")))

(defrule love_bts ""
   (like_eastern_dragons no)
   =>
   (assert (love_bts (yes_no "Do you know what BTS is? (yes/no) "))))

(defrule check_love_bts_yes ""
   (love_bts yes)
   =>
   (assert (country "South Korea.")))

(defrule check_love_bts_no ""
   (love_bts no)
   =>
   (assert (country "KPDR.")))

;;;***************************
;;;* RIGHT-RIGHT WING ANSWERS *
;;;***************************

(defrule like_alpinism ""
   (love_asia no)
   =>
   (assert (like_alpinism (yes_no "Do you like alpinism? (yes/no) "))))

(defrule check_like_alpinism_yes ""
   (like_alpinism yes)
   =>
   (assert (country "Switzerland.")))

(defrule like_snow_and_mountains ""
   (like_alpinism no)
   =>
   (assert (like_snow_and_mountains (yes_no "Maybe you just like snow and mountains? (yes/no) "))))

(defrule check_like_snow_and_mountains_yes ""
   (like_snow_and_mountains yes)
   =>
   (assert (country "Norway.")))

(defrule like_lego ""
   (like_snow_and_mountains no)
   =>
   (assert (like_lego (yes_no "Do you like LEGO? (yes/no) "))))

(defrule check_like_lego_yes ""
   (like_lego yes)
   =>
   (assert (country "Denmark.")))

(defrule check_like_lego_no ""
   (like_lego no)
   =>
   (assert (country "Netherlands.")))

;;;********************************
;;;* DECIDED COUNTRY *
;;;********************************

(defrule decided_country ""
  (declare (salience 10))
  (country ?item)
  =>
  (printout t crlf)
  (printout t "I think the best place for you is " ?item crlf))