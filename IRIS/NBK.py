import random
import csv
import math

# Datensatz einlesen, dabei gleich Datentypen anpassen
with   open("iris.csv") as f:
    daten = [[float(x) for x in datenzeile[:-1]] + [datenzeile[-1]] for datenzeile in csv.reader(f)]

# Datensatz mischen
random.shuffle(daten)

# Datensatz in Trainingsdaten und <testdaten aufteilen (80% bzw. 20%)
trainingsdaten = daten[:int(len(daten)*0.8)]
testdaten = daten[int(len(daten) * 0.8):]

def gauss(x, mu, sigma):
    vorfaktor = 1 / (sigma * math.sqrt(2 * math.pi))
    exponent = -1/2 * ((x - mu) / sigma) ** 2
    return vorfaktor * math.e ** exponent

def mittelwert(daten):
    return sum(daten) / len(daten)

def standartabweichung(daten, mu):
    return sum((datum - mu) ** 2 for datum in daten) / (len(daten) - 1)

klassen = set(trainingsdatenzeile[-1] for trainingsdatenzeile in trainingsdaten)

trainingsdaten_pro_klasse = {klasse: [trainingsdatenzeile[:-1] for trainingsdatenzeile in trainingsdaten if trainingsdatenzeile[-1] == klasse] for klasse in klassen} 

trainingsdaten_pro_klasse_t  = {klasse: list (zip(*trainingsdaten_pro_klasse[klasse]))for klasse in klassen}

parameter = {klasse: list((mittelwert(spalte), standartabweichung(spalte, mittelwert(spalte))) for spalte in trainingsdaten_pro_klasse_t[klasse]) for klasse in klassen}

bewertung = 0

# Vorhersage für alle testdatenzeilen treffen
for testdatenzeile in testdaten:
    klasse_max = None
    wahrscheinlichkeit = 0

    for klasse in klassen:
        wahrscheinlichkeit = 1
        for i in range(len(testdatenzeile)-1):
            wert = testdatenzeile[i]        
            wahrscheinlichkeit *= gauss(wert, parameter[klasse][i][0], parameter[klasse][i][1]) 
        wahrscheinlichkeit *= len(trainingsdaten_pro_klasse[klasse]) / len(trainingsdaten)

vorhersage = ...
   
print(bewertung)   
   
   
   
   # wahrscheinlichkeiten = {klasse: parameter[klasse] for klasse in klassen}
   # vorhersage = 


