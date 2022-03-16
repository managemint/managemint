/* csrc/ansible.h
 *
 *  Copyright (C) 2022 Jonas Gunz, Konstantin Grabmann, Paul Trojahn
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 3 as
 * published by the Free Software Foundation.
 *
*/

#pragma once

#define ANSIBLE_OK		0
#define ANSIBLE_ERROR		1
#define ANSIBLE_FAILED_HOSTS	2
#define ANSIBLE_UNREACH_HOSTS	4
#define ANSIBLE_BREAK_PLAY	8
#define ANSIBLE_UNKNOWN		255

int ansible( char* _path, char *_playbook, char *_limit, char *_tag );
