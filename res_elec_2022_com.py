# -*- coding: utf-8 -*-
"""
Created on Sat Oct 29 11:53:45 2022

@author: louis

création d'un fichier csv dans le répertoire dans lequel, le programme est enregistré. Le nom du fichier est : reselections2022communes.csv
"""

import requests
import re

import csv

def donne_url_id():
    file = open("communes-departement-region.csv", 'r')

    text = file.read()

    ligne = text.split('\n')

    tableau = []

    for e in ligne[1:] :
        tableau.append((e.split(',')))


    communes_paca =[]

    for element in tableau :
        if len(element)>12:
            if  '93' == (element[13]):
                communes_paca.append(element)


    tab_nom_id_dep = []
    for el in communes_paca :
        tab_nom_id_dep.append([el[1],el[0], el[11]])


    tab_nom_url = []

    for ell in tab_nom_id_dep :
        url = 'https://www.interieur.gouv.fr/Elections/Les-resultats/Presidentielles/elecresult__presidentielle-2022/(path)/presidentielle-2022/093/' + ell[2].rjust(3, '0') + '/' + ( ell[1].rjust(3, '0')).rjust(6, '0') +'.html'
        tab_nom_url.append([ell[0], url])
    return tab_nom_url



def donne_res_m_m(url):
    test = requests.get(str(url))

    page = test.text
    deb = page.find("Marine")
    end = page.find('Emmanuel')
    if deb <= end :
        marine = page[deb:end]
        l = marine.split('\n')
        inte= l[:4]

        t = []

        for e in inte :
            t.append(e.strip('</td>'))
        res_marine = []
        if len(t)>2:
            t1 = t[1].strip(' style="text-align:right">')
            t1 = re.sub(r"\s+", '',t1)
            t2 = t[2].strip(' style="text-align:center">')
            t2 = re.sub(r",", '.',t2)
            t3 = t[3].strip(' style="text-align:center">')
            t3 = re.sub(r",", '.',t3)
            res_marine =[t[0], int(t1), float(t2), float(t3)]

        deba = page.find('Emmanuel')
        enda = page.find('<br><br><table class="table table-bordered tableau-mentions"')
        macron = page[deba:enda]
        l = macron.split('\n')
        inte= l[:4]

        t = []

        for e in inte :
            t.append(e.strip('</td>'))
        res_macron =[]
        if len(t)>2:
            t1 = t[1].strip(' style="text-align:right">')
            t1 = re.sub(r"\s+", '',t1)
            t2 = t[2].strip(' style="text-align:center">')
            t2 = re.sub(r",", '.',t2)
            t3 = t[3].strip(' style="text-align:center">')
            t3 = re.sub(r",", '.',t3)
            res_macron =[t[0], int(t1), float(t2), float(t3)]
    else:
        deb = page.find("Emmanuel")
        end = page.find('Marine')
        macron = page[deb:end]
        l = macron.split('\n')
        inte= l[:4]

        t = []

        for e in inte :
            t.append(e.strip('</td>'))
        res_macron = []
        if len(t)>2:
            t1 = t[1].strip(' style="text-align:right">')
            t1 = re.sub(r"\s+", '',t1)
            t2 = t[2].strip(' style="text-align:center">')
            t2 = re.sub(r",", '.',t2)
            t3 = t[3].strip(' style="text-align:center">')
            t3 = re.sub(r",", '.',t3)
            res_macron =[t[0], int(t1), float(t2), float(t3)]

        deba = page.find('Marine')
        enda = page.find('<br><br><table class="table table-bordered tableau-mentions"')
        marine = page[deba:enda]
        l = marine.split('\n')
        inte= l[:4]

        t = []

        for e in inte :
            t.append(e.strip('</td>'))
        res_marine =[]
        if len(t)>2:
            t1 = t[1].strip(' style="text-align:right">')
            t1 = re.sub(r"\s+", '',t1)
            t2 = t[2].strip(' style="text-align:center">')
            t2 = re.sub(r",", '.',t2)
            t3 = t[3].strip(' style="text-align:center">')
            t3 = re.sub(r",", '.',t3)
            res_marine =[t[0], int(t1), float(t2), float(t3)]


    return res_macron, res_marine


def final():
    t = donne_url_id()
    l = [['id_com_insee','res_macron ( nb voix, pourcentage d inscrits, pourcentage d exprimés)', 'res_lepen ( nb voix, pourcentage d inscrits, pourcentage d exprimés)']]
    for u in t :
        a,b = donne_res_m_m(u[1])
        if len(a) != 0 and len(b) != 0 :
            c,d,e,f = a
            g,h,i,j = b
            l.append([u[0],c,d,e,f,g,h,i,j])
    return l

res_f = final()
ttttttt=[]
tttttttt=[]




m = ['MARSEILLE', 'Emmanuel Macron', 202968 , 40.58, 64.42,'Marine LE PEN', 112098, 22.41, 35.58]
ff= []
for e in res_f:
    if len(e[1]) != 0 :
        ff.append(e)

for e in ff:
    if e[1] == []:
        ttttttt.append(e)
    if e[2] == []:
        tttttttt.append(e)
ff.append(m)

with open('reselections2022communes.csv', 'w', newline='') as file:
    writer = csv.writer(file, quoting=csv.QUOTE_ALL,delimiter=';')
    writer.writerows(ff)
