#include "git.h"

#include <git2.h>
#include <git2/global.h>
#include <git2/repository.h>

// https://libgit2.org/docs/guides/101-samples/

int is_repo(char* _url, char* _path) {
	if (git_repository_open_ext(NULL, _path, GIT_REPOSITORY_OPEN_NO_SEARCH, NULL) != 0) {
		return -1;
	}
	return -1;
}

int do_git_clone(char* _url, char* _path, char* _refspec) {
	int ret = -1;
	git_repository* repo = NULL;

	git_libgit2_init();

	ret = git_clone(&repo, _url, _path, NULL);

end:
	git_repository_free(repo);
	git_libgit2_shutdown();
	return ret;
}
