/* csrc/ansible.c
 *
 *  Copyright (C) 2022 Jonas Gunz, Konstantin Grabmann, Paul Trojahn
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 3 as
 * published by the Free Software Foundation.
 *
*/

#include "ansible.h"

#ifndef _PYINCLUDE
#warning "Assuming python3.10"
#define _PYINCLUDE <python3.10/Python.h>
#endif

#define PY_SSIZE_T_CLEAN
#include _PYINCLUDE

#include <errno.h>
#include <stdio.h>

#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

PyObject *import_name(const char *_modname, const char *_symbol)
{
	PyObject *u_name, *module;
	u_name = PyUnicode_FromString(_modname);
	module = PyImport_Import(u_name);
	Py_DECREF(u_name);

	if (!module)
		return NULL;

	return PyObject_GetAttrString(module, _symbol);
}

static void run_ansible( char* _path, char *_playbook, char *_limit, char *_tag ) {
	PyObject *run_func, *args, *kwargs, *result = NULL;
	PyGILState_STATE state;

	int ret = -1;

	if (!_playbook)
		exit(-1);

	// TODO Yeah we should sanitize here...
	if(chdir(_path)) {
		printf("Couldn't chdir() to '%s': %s\n", _path, strerror(errno));
		exit(-1);
	}

	putenv("ANSIBLE_STDOUT_CALLBACK=ffpp.hansible_modules.hansible_export");

	/* TODO: Use those! */
	if (!_limit)
		_limit = "\0";
	if (!_tag)
		_tag = "\0";

	Py_Initialize();

	run_func = import_name("hansible_glue", "run_playbook");
	state = PyGILState_Ensure();

	if (!run_func || !PyCallable_Check(run_func)) {
		printf("No Callable!\n");
		if(PyErr_Occurred())
			PyErr_Print();
		goto end;
	}

	args = Py_BuildValue("(s)", _playbook);
	kwargs = NULL;

	result = PyObject_Call(run_func, args, kwargs);
	Py_DECREF(args);
	Py_XDECREF(kwargs);

	if(PyErr_Occurred()) {
		PyErr_Print();
		goto end;
	}

	ret = PyLong_AsLong(result);

	end:
	Py_XDECREF(result);
	PyGILState_Release(state);

	Py_Finalize();
	exit(ret);
}

/**
 * We need this, because Py_Finalize() is appearantly buggy.
 * Re-Running ansible causes weird Class inheritance errors.
 * So we need a fork here.
 */
int ansible( char* _path, char *_playbook, char *_limit, char *_tag ) {
	pid_t pid = fork();
	int ret;

	if(pid == 0)
		run_ansible(_path, _playbook, _limit, _tag);


	if(waitpid(pid, &ret, 0) < 0)
		return 99; // TODO Error
	
	return ret;
}
