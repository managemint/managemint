# hansible

![hansible](static/logo.png "hansible")

## Verwendung

### Compilieren

Neben Stack und den damit installierten dependencies benötigt hansible `python` und `libgit2`.
Da CPython seine Header und Library in versionsspezifischen Ordnern hinterlegt, muss diese in der `package.yml` entsprechend angegeben werden:

```yml
...

c-sources: csrc/*.c
cc-options:
  - '-D_PYINCLUDE=<python[PYTHON VERSION]/Python.h>'
include-dirs: csrc/
extra-libraries:
  - python[PYTHON VERSION]
  - git2

...
```

Getestete Versionen:

* `python` 3.9, 3.10
* `libgit2` 1.1, 1.4

### Setup

Neben dem basis hansible-Projekt werden außerdem die python-Module `hansible_glue` und `ansible`,
sowie das Ansible-Galaxy Modul `hansible_modules` benötigt.
Die installation erfolgt im jeweiligen Quellcode-Ordner mit `pip install .` bzw. `ansible-galaxy collection install .`.
Hierfür empfiehlt sich ein Virtualenv.

Zur Ausführung wird weiter eine MySQL-Datenbank benötigt (getestet mit MariaDB 10.5).
Die Zugansdaten müssen in `app/Main.hs` angepasst werden.

Das Webinterface ist nach korrekter Konfiguration auf port `3000` zu erreichen.

### Projekte

Ausgangspunkt ist eine Repository mit Ansible-Struktur, siehe `ansible-example`.
In dessen Wurzel wird nun eine `hansible.conf`-Datei angelegt.

Diese kann wie folgt aussehen:

```toml
[run.createUsers]
file = "pb_users.yml"
schedule = "20:00"

[run.createBackups]
file = 'pb_backups.yml'
schedule = "mon..fri,sun /04:00"
```

In diesem Beispiel werden zwei Runs definiert.
Der erste mit Namen `createUsers` führt das Playbook `pb_users.yml` jeden Tag um 20:00h aus.
`createBackups` startet `pb_backups.yml` Werktags und Sonntags alle vier Stunden.

### Schedule Format

Das Format ist inspiriert vom ProxMox Backup Schedule Format, siehe [ProxMox Dokumentation](https://pve.proxmox.com/pve-docs/pve-admin-guide.html#chapter_calendar_events)

Grammatik siehe Quellcode. Hier ein paar Beispiele:

| Schedule Format     | Alternativ      | Bedeutung |
| --------            | --------        | -------- |
| mon,tue,wed,thu,fri | mon..fri        | Werktags um 0:00 |
| mon,tue,wed,sun     | mon..wed,sun    | Montags - Mittwochs und Sonntags um 0:00 |
| 12:05               | 12:05           | Jeden Tag um 12:05 |
| fri 12:00/20        | fri 12:00/00:20 | Freitag um 12:00, 12:20 und 12:40 |
| 24                  | -               | 24 Minuten nach jeder vollen Stunde |
| 14:00/02:10         | -               | ab 14:00 alle zwei Stunden und zehn Minuten bis 22:40 |
| /5                  | 0/5             | Alle fünf Minuten |
