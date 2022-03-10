#include "git.h"

#include <alloca.h>
#include <git2.h>
#include <git2/branch.h>
#include <git2/global.h>
#include <git2/object.h>
#include <git2/refs.h>
#include <git2/remote.h>
#include <git2/repository.h>
#include <git2/strarray.h>

#include <string.h>

#define ASSERT_GIT_CALL(a,r) {if(a!=0){ret=r; goto error;}}

#define _GIT_DEFAULT_REMOTE "origin"

// https://libgit2.org/docs/guides/101-samples/

/* TODO
 * - Sanitize paths
 */

int is_repo(char* _path, char* _remote) {
	int ret = -2;
	git_repository *repo = NULL;
	git_strarray remotes = {0};
	git_remote *remote = NULL;


	git_libgit2_init();

	ASSERT_GIT_CALL( git_repository_open_ext(&repo, _path, GIT_REPOSITORY_OPEN_NO_SEARCH, NULL), GIT_NOT_A_REPO );
	ASSERT_GIT_CALL( git_remote_list(&remotes, repo), -1 );

	if (remotes.count <= 0) {
		ret = GIT_INCORRECT_REMOTE;
		goto error;
	}

	for (unsigned int i = 0; i < remotes.count; i++) {
		ASSERT_GIT_CALL( git_remote_lookup(&remote, repo, remotes.strings[i]), GIT_INCORRECT_REMOTE );

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

	git_libgit2_init();

	ASSERT_GIT_CALL(git_clone(&repo, _url, _path, &clone_opts), GIT_CALL_FAILED);

end:
	git_repository_free(repo);
	git_libgit2_shutdown();
	return ret;
error:
	// TODO delete directory if failed?
	goto end;
}

int do_git_pull(char* _path, char* _refspec) {
	int ret = -1;
	git_repository *repo = NULL;
	git_remote *remote = NULL;
	//git_reference *ref = NULL;
	git_oid oid;
	git_object *obj = NULL;

	git_libgit2_init();

	ASSERT_GIT_CALL( git_repository_open_ext(&repo, _path, GIT_REPOSITORY_OPEN_NO_SEARCH, NULL), GIT_NOT_A_REPO );

	ASSERT_GIT_CALL( git_remote_lookup(&remote, repo, _GIT_DEFAULT_REMOTE), GIT_INCORRECT_REMOTE );
	ASSERT_GIT_CALL( git_remote_fetch(remote, NULL, NULL, NULL), GIT_FETCH_FAILED );

	// Get object
	// ASSERT_GIT_CALL( git_branch_lookup(&ref, repo, _refspec, GIT_BRANCH_REMOTE), GIT_CHECKOUT_FAILED);
	ASSERT_GIT_CALL( git_reference_name_to_id(&oid, repo, _refspec), GIT_CHECKOUT_FAILED );
	ASSERT_GIT_CALL( git_object_lookup(&obj, repo, &oid, GIT_OBJECT_ANY), GIT_CHECKOUT_FAILED );

	ASSERT_GIT_CALL( git_checkout_tree(repo, obj, NULL), GIT_CHECKOUT_FAILED);

error:
end:
	git_repository_free(repo);
	git_remote_free(remote);
	//git_reference_free(ref);
	git_object_free(obj);
	git_libgit2_shutdown();
	return ret;
}
