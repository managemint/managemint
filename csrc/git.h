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
#define HSGIT_CALL_FAILED	1
#define HSGIT_INCORRECT_REMOTE	2
#define HSGIT_REFSPEC_NOT_MERGEABLE 3
#define HSGIT_NO_FF_POSSIBLE	4

/**
 * returns last merge OID. Only valid directly afer successful do_git_pull() call
 * user needs to free()
 */
char *get_last_merge_oid();

/**
 * returns last libgit error string.
 * Errstring is NOT cleared after successful calls.
 * Only call, if a function returns HSGIT_CALL_FAILED
 */
char *get_last_error();

/**
 * Check, whether _path is a valid repo and has _remote as a remote set.
 */
int is_repo(char* _path, char* _remote);

/**
 * Perform a git clone operation of _url to path _path,
 * checkout _refspec
 */
int do_git_clone(char* _url, char* _path, char* _refspec);

/**
 * Perform a Fast-Forward only pull of repo in _path on branch _refspec.
 * No merge is performed. If a FF is not possible,
 * do_git_pull() fails with HSGIT_NO_FF_POSSIBLE.
 */
int do_git_pull(char* _path, char* _refspec);
