Teilnehmer:

- Konstantin Grabmann
- Jonas Gunz
- Paul Trojahn



### management frontend für ansible



##### Beschreibung

[**Ansible** ist ein Open-Source Automatisierungs-Werkzeug zur Orchestrierung und allgemeinen Konfiguration und Administration von Computern.](https://de.wikipedia.org/wiki/Ansible) Beispielsweise sorgen wir damit für einheitliche Konfiguration auf Servern. Basispakete werden installiert, Nutzer werden erstellt, aber auch komplexere Aufgaben wie die Konfiguration von Webservern sind automatisierbar. 

Momentan wird das Ansible Management unserer Serverstruktur mit Jenkins verwaltet. Da Jenkins nicht dafür ausgelegt ist, handelt es sich nur um eine Übergangslösung, die von Shell Scripten zusammengehalten wird. 

Wir wollen ein für diesen Zweck zugeschnittenes CI System programmieren. Dafür werden wir ein funktionales Web Frontend implementieren. Die Ansible- , sowie die Systemkonfiguration sollten aus einer Git Repo entnommen werden. Die Ergebnisse der Ansible Durchläufe werden im Webinterface anschaulich aufbereitet. 



##### Vertiefende Themen

- Webentwicklung für das Webinterface
- Datenbankanbindung für persistente Speicherung von Konfigurations- und Anwendungsdaten
- Monadentransformer für gleichzeitiges Schreiben in die Datenbank und Liveausgabe 