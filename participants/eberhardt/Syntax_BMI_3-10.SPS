* Encoding: UTF-8.



Bewertung:
1= stark untergewichtig
2= untergewichtig
3= normalgewicht
4= übergewicht
5= adipös



DO IF (Geschlecht=1 & alter = 3) .
RECODE
  BMI
  (Lowest thru 13.55=1)  (13.56 thru 14.13=2)  (14.14 thru 17.61=3)  (17.62
  thru 18.81=4)  (18.82 thru Highest=5)  INTO  KategorieBMI .
END IF .
VARIABLE LABELS BMI 'KategoerieBMI'.
EXECUTE .

DO IF (Geschlecht=1 & alter = 4) .
RECODE
  BMI
  (Lowest thru 13.36=1)  (13.37 thru 13.94=2)  (13.95 thru 17.53=3)  (17.54
  thru 18.82=4)  (18.83 thru Highest=5)  INTO  KategorieBMI .
END IF .
VARIABLE LABELS BMI 'KategoerieBMI'.
EXECUTE .

DO IF (Geschlecht=1 & alter = 5) .
RECODE
  BMI
  (Lowest thru 13.24=1)  (13.25 thru 13.83=2)  (13.84 thru 17.60=3)  (17.61
  thru 19.01=4)  (19.02 thru Highest=5)  INTO  KategorieBMI .
END IF .
VARIABLE LABELS BMI 'KategoerieBMI'.
EXECUTE .

DO IF (Geschlecht=1 & alter = 6) .
RECODE
  BMI
  (Lowest thru 13.18=1)  (13.19 thru 13.79=2)  (13.80 thru 17.85=3)  (17.86
  thru 19.43=4)  (19.44 thru Highest=5)  INTO  KategorieBMI .
END IF .
VARIABLE LABELS BMI 'KategoerieBMI'.
EXECUTE .
	
DO IF (Geschlecht=1 & alter = 7) .
RECODE
  BMI
  (Lowest thru 13.23=1)  (13.24 thru 13.88=2)  (13.89 thru 18.33=3)  (18.34
  thru 20.14=4)  (20.15 thru Highest=5)  INTO  KategorieBMI .
END IF .
VARIABLE LABELS BMI 'KategoerieBMI'.
EXECUTE .	

DO IF (Geschlecht=1 & alter = 8) .
RECODE
  BMI
  (Lowest thru 13.37=1)  (13.38 thru 14.07=2)  (14.08 thru 19.00=3)  (19.01
  thru 21.10=4)  (21.11 thru Highest=5)  INTO  KategorieBMI .
END IF .
VARIABLE LABELS BMI 'KategoerieBMI'.
EXECUTE .	

DO IF (Geschlecht=1 & alter = 9) .
RECODE
  BMI
  (Lowest thru 13.56=1)  (13.57 thru 14.31=2)  (14.32 thru 19.77=3)  (19.78
  thru 22.20=4)  (22.21 thru Highest=5)  INTO  KategorieBMI .
END IF .
VARIABLE LABELS BMI 'KategoerieBMI'.
EXECUTE .

DO IF (Geschlecht=1 & alter = 10) .
RECODE
  BMI
  (Lowest thru 13.80=1)  (13.81 thru 14.60=2)  (14.61 thru 20.59=3)  (20.60
  thru 23.34=4)  (23.35 thru Highest=5)  INTO  KategorieBMI .
END IF .
VARIABLE LABELS BMI 'KategoerieBMI'.
EXECUTE .

DO IF (Geschlecht=2 & alter = 3) .
RECODE
  BMI
  (Lowest thru 13.29=1)  (13.30 thru 13.93=2)  (13.94 thru 17.63=3)  (17.64
  thru 18.83=4)  (18.84 thru Highest=5)  INTO  KategorieBMI .
END IF .
VARIABLE LABELS BMI 'KategoerieBMI'.
EXECUTE .

DO IF (Geschlecht=2 & alter = 4) .
RECODE
  BMI
  (Lowest thru 13.06=1)  (13.07 thru 13.69=2)  (13.70 thru 17.53=3)  (17.54
  thru 18.84=4)  (18.85 thru Highest=5)  INTO  KategorieBMI .
END IF .
VARIABLE LABELS BMI 'KategoerieBMI'.
EXECUTE .

DO IF (Geschlecht=2 & alter = 5) .
RECODE
  BMI
  (Lowest thru 12.97=1)  (12.98 thru 13.61=2)  (13.62 thru 17.68=3)  (17.69
  thru 19.15=4)  (19.16 thru Highest=5)  INTO  KategorieBMI .
END IF .
VARIABLE LABELS BMI 'KategoerieBMI'.
EXECUTE .

DO IF (Geschlecht=2 & alter = 6) .
RECODE
  BMI
  (Lowest thru 12.92=1)  (12.93 thru 13.59=2)  (13.60 thru 17.98=3)  (17.99
  thru 19.66=4)  (19.67 thru Highest=5)  INTO  KategorieBMI .
END IF .
VARIABLE LABELS BMI 'KategoerieBMI'.
EXECUTE .

DO IF (Geschlecht=2 & alter = 7) .
RECODE
  BMI
  (Lowest thru 12.98=1)  (12.99 thru 13.69=2)  (13.70 thru 18.50=3)  (18.51
  thru 20.43=4)  (20.44 thru Highest=5)  INTO  KategorieBMI .
END IF .
VARIABLE LABELS BMI 'KategoerieBMI'.
EXECUTE .	

DO IF (Geschlecht=2 & alter = 8) .
RECODE
  BMI
  (Lowest thru 13.16=1)  (13.17 thru 13.92=2)  (13.93 thru 19.24=3)  (19.25
  thru 21.46=4)  (21.47 thru Highest=5)  INTO  KategorieBMI .
END IF .
VARIABLE LABELS BMI 'KategoerieBMI'.
EXECUTE .	

DO IF (Geschlecht=2 & alter = 9) .
RECODE
  BMI
  (Lowest thru 13.38=1)  (13.39 thru 14.19=2)  (14.20 thru 20.03=3)  (20.04
  thru 22.53=4)  (22.54 thru Highest=5)  INTO  KategorieBMI .
END IF .
VARIABLE LABELS BMI 'KategoerieBMI'.
EXECUTE .

DO IF (Geschlecht=2 & alter = 10) .
RECODE
  BMI
  (Lowest thru 13.61=1)  (13.62 thru 14.48=2)  (14.49 thru 20.79=3)  (20.80
  thru 23.53=4)  (23.54 thru Highest=5)  INTO  KategorieBMI .
END IF .
VARIABLE LABELS BMI 'KategoerieBMI'.
EXECUTE .

	




