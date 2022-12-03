# managemint

![managemint](static/logo.png "managemint")

managemint is a purpose-built CI for ansible.
It aims to give users a better insight of the state of their Playbook runs.

managemint is in early development and not yet stable.

## Usage

### Building

managemint is written in Haskell and uses the stack build-system to install Haskell dependencies.
On top of those, managemint depends on `python` and `libgit2`.
Because of CPython's hardcoded version in the library name, it is necessary to specify the exact version in `package.yml`:

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

Tested versions:

* `python` 3.9, 3.10
* `libgit2` 1.1, 1.4

### Setup

This is an experimental development setup only.

managemint depends on a python-package and a Ansible-Galaxy collection,
`manageint_glue` and `managemint_modules`.

Install them with running `pip install .` and `ansible-galaxy collection install .`
in the respective source folders.
A virtaulenv for python packages is recommended.

A MySQL Database (tested with MariaDB 10.5) is required to run managemint.
Connection parameters are set in `managemint.conf`.
See `managemint.conf.example` for details.

By default listens on port `3000`.

### Projects

A managemint-ready repository contains a normal Ansible project structure
and a `managemint.conf` file (look at the `ansible-example` repo).

An example `managemint.conf` could look like this:

```toml
[run.createUsers]
file = "pb_users.yml"
schedule = "20:00"

[run.createBackups]
file = 'pb_backups.yml'
schedule = "mon..fri,sun /04:00"
```

This example defines two Runs: `createUsers` and `runBackups`.
The first runs the playbook `pb_users.yml` every day at 8:00PM,
the latter runs `pb_backups.yml` every working day and Sunday every four hours.

### Schedule Format

The format was inspired by the ProxMox schedule format: [ProxMox Documentation](https://pve.proxmox.com/pve-docs/pve-admin-guide.html#chapter_calendar_events)

The Grammar is explained in the source code, some Examples:

| Schedule Format     | Alternative     | Meaning                                                 |
| --------            | --------        | --------                                                |
| mon,tue,wed,thu,fri | mon..fri        | Workdays at 0:00                                        |
| mon,tue,wed,sun     | mon..wed,sun    | Monday to Wednesday and Sunday at 0:00                  |
| 12:05               | -               | Every day at 12:05                                      |
| fri 12:00/20        | fri 12:00/00:20 | Friday at 12:00, 12:20 and 12:40                        |
| 24                  | -               | 24 minutes after every full hour                        |
| 14:00/02:10         | -               | from 14:00 every two hours and ten mintues, until 22:40 |
| /5                  | 0/5             | every five minutes                                      |
