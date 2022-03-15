/* app/git.h
 *
 *  Copyright (C) 2022 Jonas Gunz, Konstantin Grabmann, Paul Trojahn
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 3 as
 * published by the Free Software Foundation.
 *
*/

#pragma once

#define HSGIT_OK		0
#define HSGIT_CALL_FAILED	-1
#define HSGIT_INCORRECT_REMOTE	-2
#define HSGIT_REFSPEC_NOT_MERGEABLE -3

#define HSGIT_NOT_A_REPO	-1
#define HSGIT_FETCH_FAILED	-4
#define HSGIT_CHECKOUT_FAILED	-5
#define HSGIT_MERGE_FAILED	-6

char *get_last_merge_oid();

char *get_last_error();

int is_repo(char* _path, char* _remote);

int do_git_clone(char* _url, char* _path, char* _refspec);

int do_git_pull(char* _path, char* _refspec);
