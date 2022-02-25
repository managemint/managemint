#include "ansible.h"

#ifndef _PYINCLUDE
#warning "Assuming python3.10"
#define _PYINCLUDE <python3.10/Python.h>
#endif

#define PY_SSIZE_T_CLEAN
#include _PYINCLUDE

#include <errno.h>
#include <stdio.h>

PyObject *import_name(const char *modname, const char *symbol)
{
	PyObject *u_name, *module;
	u_name = PyUnicode_FromString(modname);
	module = PyImport_Import(u_name);
	Py_DECREF(u_name);

	if (!module)
		return NULL;

	return PyObject_GetAttrString(module, symbol);
}

int ansible( char* _path, char *_playbook, char *_limit, char *_tag ) {
	// TODO Yeah we should sanitize here...
	if(chdir(_path)) {
		printf("Couldn't chdir() to '%s': %s\n", _path, strerror(errno));
		return -1;
	}

	putenv("ANSIBLE_STDOUT_CALLBACK=ffpp.hansible_modules.hansible_export");

	PyObject *run_func, *args, *kwargs, *result = NULL;
	PyGILState_STATE state;

	int ret = -1;

	if (!_playbook)
		return -1;

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
	return ret;
}
