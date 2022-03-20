/* csrc/ansible.h
 *
 *  Copyright (C) 2022 Jonas Gunz, Konstantin Grabmann, Paul Trojahn
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 3 as
 * published by the Free Software Foundation.
 *
*/

/**
 * Run Ansible Playbook.
 * Chnaged to directory _path, then runs _playbook
 */
int ansible( char* _path, char *_playbook, char *_limit, char *_tag );
