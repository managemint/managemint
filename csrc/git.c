/* csrc/git.c
 *
 *  Copyright (C) 2022 Jonas Gunz, Konstantin Grabmann, Paul Trojahn
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 3 as
 * published by the Free Software Foundation.
 *
*/

#include "git.h"

#include <alloca.h>
#include <git2.h>
#include <git2/annotated_commit.h>
#include <git2/branch.h>
#include <git2/credential.h>
#include <git2/global.h>
#include <git2/merge.h>
#include <git2/object.h>
#include <git2/refs.h>
#include <git2/remote.h>
#include <git2/repository.h>
#include <git2/strarray.h>

#include <git2/types.h>
#include <string.h>


#define ASSERT_GIT_CALL(a) {if(a!=0){ret=HSGIT_CALL_FAILED; update_last_error(); goto error;}}

#define _GIT_DEFAULT_REMOTE "origin"
#define _GIT_ERRSTR_LEN 512

// https://libgit2.org/docs/guides/101-samples/

/* TODO
 * - Sanitize paths
 */

git_oid glbl__merge_oid;
int	glbl__merge_oid_set;
char	glbl__errmsg[_GIT_ERRSTR_LEN];

static int auth_callback(git_cred **out, const char *url, const char *username_from_url, unsigned int allowed_types, void *payload) {
	// TODO other stuff
	return git_credential_ssh_key_from_agent(out, username_from_url);
}

// TODO ugly AF...
/**
 * check, if refspecs match.
 *
 */
static int refspecs_match(const char *_name, const char *_ref) {
	size_t len_ref = 0;
	size_t len_name = 0;

	if (!_name || !_ref)
		return 0;
	
	len_ref  = strlen(_ref);
	len_name = strlen(_name);

	if( len_name <= len_ref)
		return 0;

	for (int i = 0; i < len_name; i++) {
		int i_n = len_name - i - 1;
		int i_r = len_ref  - i - 1;

		if (i >= len_ref) {
			if (_name[i_n] == '/')
				return 1;
			return 0;
		}

		if ( _name[i_n] != _ref[i_r] )
			return 0;
	}

	return 0;
}

/**
 * Take the first remote OID that matches the target one.
 */
static int fetchhead_ref_callback(const char *_name, const char *_url, const git_oid *_oid, unsigned int _is_merge, void *_payload) {
	if (!refspecs_match(_name, (char*)_payload))
		return 0;

	if ( _is_merge ) {
		memcpy( &glbl__merge_oid, _oid, sizeof( git_oid ) );
		glbl__merge_oid_set = 1;
	}

	return 0;
}

/**
 * Since we want to retrieve errstrings from outside the libgit-context,
 * we need to copy them when they occur.
 */
static void update_last_error() {
	char* err = git_error_last()->message;
	strncpy( glbl__errmsg, err, _GIT_ERRSTR_LEN-1 );
}

char *get_last_error() {
	// Be paranoid!
	glbl__errmsg[_GIT_ERRSTR_LEN-1] = '\0';
	return glbl__errmsg;
}

/*
 * Exported functions below. See header for docs.
 */

char *get_last_merge_oid() {
	char *ret = NULL;
	size_t len = GIT_OID_HEXSZ+1;

	if(! (ret = malloc(len)) )
		return ret;

	git_oid_tostr(ret, len, &glbl__merge_oid);

	return ret;
}

int is_repo(char* _path, char* _remote) {
	int ret = -2;
	git_repository *repo = NULL;
	git_strarray remotes = {0};
	git_remote *remote = NULL;


	git_libgit2_init();

	ASSERT_GIT_CALL( git_repository_open_ext(&repo, _path, GIT_REPOSITORY_OPEN_NO_SEARCH, NULL) );
	ASSERT_GIT_CALL( git_remote_list(&remotes, repo) );

	if (remotes.count <= 0) {
		ret = HSGIT_INCORRECT_REMOTE;
		goto error;
	}

	for (unsigned int i = 0; i < remotes.count; i++) {
		ASSERT_GIT_CALL( git_remote_lookup(&remote, repo, remotes.strings[i]) );

		if (strcmp(git_remote_url(remote), _remote) == 0) {
			ret = GIT_OK;
			break;
		}
	}

end:
	git_repository_free(repo);
	git_strarray_free(&remotes);
	git_remote_free(remote);

	git_libgit2_shutdown();
	return ret;
error:
	goto end;
}

int do_git_clone(char* _url, char* _path, char* _refspec) {
	int ret = -1;
	git_repository* repo = NULL;
	git_clone_options clone_opts = GIT_CLONE_OPTIONS_INIT;

	clone_opts.checkout_branch = _refspec;
	clone_opts.fetch_opts.callbacks.credentials = auth_callback;

	git_libgit2_init();

	ASSERT_GIT_CALL(git_clone(&repo, _url, _path, &clone_opts));

	ret = HSGIT_OK;

end:
	git_repository_free(repo);
	git_libgit2_shutdown();
	return ret;
error:
	goto end;
}

int do_git_pull(char* _path, char* _refspec) {
	int ret = -1;
	git_repository *repo = NULL;
	git_remote *remote = NULL;
	git_object *ff_target = NULL;

	git_annotated_commit *heads[ 1 ] = {NULL};

	git_reference *current_head = NULL;
	git_reference *new_head = NULL;

	git_merge_analysis_t merge_analysis;
	git_merge_preference_t merge_pref;

	git_fetch_options fetch_opts = GIT_FETCH_OPTIONS_INIT;
	git_checkout_options checkout_opts = GIT_CHECKOUT_OPTIONS_INIT;
	git_merge_options merge_opts = GIT_MERGE_OPTIONS_INIT;

	fetch_opts.callbacks.credentials = auth_callback;
	checkout_opts.checkout_strategy = GIT_CHECKOUT_SAFE;

	git_libgit2_init();

	ASSERT_GIT_CALL( git_repository_open_ext(&repo, _path, GIT_REPOSITORY_OPEN_NO_SEARCH, NULL) );

	ASSERT_GIT_CALL( git_remote_lookup(&remote, repo, _GIT_DEFAULT_REMOTE) );
	ASSERT_GIT_CALL( git_remote_fetch(remote, NULL, &fetch_opts, NULL) );

	glbl__merge_oid_set = 0;
	git_repository_fetchhead_foreach( repo, fetchhead_ref_callback, _refspec );

    // _refspec does not track any remote HEAD
	if(!glbl__merge_oid_set) {
		ret = HSGIT_REFSPEC_NOT_MERGEABLE;
		goto error;
	}

	ASSERT_GIT_CALL( git_annotated_commit_lookup(&heads[0], repo, &glbl__merge_oid) );
	ASSERT_GIT_CALL( git_merge_analysis(&merge_analysis, &merge_pref, repo, (const git_annotated_commit **)heads, 1) );

    // We dont't need to FF if nothing changed
	if( merge_analysis & GIT_MERGE_ANALYSIS_UP_TO_DATE ) {
		ret = HSGIT_OK;
		goto end;
	}

    // Check, if FF is possible, Abort if not
	if( ! (merge_analysis & GIT_MERGE_ANALYSIS_FASTFORWARD) ) {
		ret = HSGIT_NO_FF_POSSIBLE;
		goto error;
	}

	ASSERT_GIT_CALL( git_object_lookup(&ff_target, repo, &glbl__merge_oid, GIT_OBJECT_COMMIT) );
	ASSERT_GIT_CALL( git_checkout_tree(repo, ff_target, &checkout_opts) );

	ASSERT_GIT_CALL( git_repository_head(&current_head, repo) );
	ASSERT_GIT_CALL( git_reference_set_target(&new_head, current_head, &glbl__merge_oid, NULL) );

	ret = HSGIT_OK;

error:
end:
	git_repository_free(repo);
	git_remote_free(remote);
	git_annotated_commit_free(heads[0]);
	git_reference_free(current_head);
	git_reference_free(new_head);

	git_libgit2_shutdown();
	return ret;
}
