#pragma once

#define GIT_OK			0
#define GIT_NOT_A_REPO		-1
#define GIT_INCORRECT_REMOTE	-2
#define GIT_CALL_FAILED		-3
#define GIT_FETCH_FAILED	-4
#define GIT_CHECKOUT_FAILED	-5

int is_repo(char* _path, char* _remote);

int do_git_clone(char* _url, char* _path, char* _refspec);

int do_git_pull(char* _path, char* _refspec);
