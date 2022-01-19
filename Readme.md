# Hansible

Teilnehmer:

- Konstantin Grabmann
- Jonas Gunz
- Paul Trojahn


### management frontend für ansible


##### Beschreibung

Momentan wird das Ansible Management unserer Serverstruktur mit Jenkins verwaltet. Da Jenkins nicht dafür ausgelegt ist, handelt es sich nur um eine Übergangslösung, die von Shell Scripten zusammengehalten wird. 

Wir wollen ein für diesen Zweck zugeschnittenes CI System programmieren. Dafür werden wir ein funktionales Web Frontend implementieren. Die Ansible- , sowie die Systemkonfiguration sollten aus einen SCM entnommen werden. Die Ergebnisse der Ansible Durchläufe werden im Webinterface anschaulich aufbereitet. 



##### Vertiefende Themen

- Webentwicklung für das Webinterface
- Datenbankanbindung für persistente Speicherung von Konfigurations- und Anwendungsdaten
- Monadentransformer für gleichzeitiges Schreiben in die Datenbank und Liveausgabe 
