#include <clang-c/Index.h>
#include <stdio.h>
#include <string.h>

#include <erl_nif.h>

static ERL_NIF_TERM walk_cursor(ErlNifEnv* env, CXTranslationUnit t, CXCursor c);
static enum CXChildVisitResult visitor_cb(CXCursor Cursor, CXCursor Parent, CXClientData ClientData);
static void print_fails(CXTranslationUnit t);
static ERL_NIF_TERM cparse(ErlNifEnv* env, int argc, char *argv[]);
static char* term2string(ErlNifEnv* env, ERL_NIF_TERM s);

static ERL_NIF_TERM
cparse(ErlNifEnv* env, int argc, char *argv[]) {
	CXIndex Index = clang_createIndex(0,0);
	CXTranslationUnit TU = clang_parseTranslationUnit(Index, 0, (const char**)(argv), argc, 0,0, CXTranslationUnit_None);
	ERL_NIF_TERM retval;

	if (!clang_getNumDiagnostics(TU)) {
		retval = walk_cursor(env, TU, clang_getTranslationUnitCursor(TU));
	} else {
		retval = enif_make_atom(env, "fail");
		print_fails(TU);
	}
	clang_disposeTranslationUnit(TU);
	clang_disposeIndex(Index);
	return retval;
}

typedef struct _clientd {
	ErlNifEnv* env;
	ERL_NIF_TERM payload;
} Data;


static ERL_NIF_TERM
walk_cursor(ErlNifEnv* env, CXTranslationUnit t, CXCursor c) {
	Data* data = enif_alloc(sizeof(Data));
	ERL_NIF_TERM retval;
	data->env = env;
	data->payload = enif_make_list(env, 0);
	CXCursorVisitor visitor = visitor_cb;
	clang_visitChildren(c, visitor, (CXClientData)data);
	enif_make_reverse_list(env, data->payload, &retval);
	return retval;
}

static enum CXChildVisitResult
visitor_cb(CXCursor cursor, CXCursor parent, CXClientData client_data)
{
	char* ctmp;
	CXString tmp;
	CXType type;
	ERL_NIF_TERM data = ((Data*)client_data)->payload;
	ErlNifEnv* env = ((Data*)client_data)->env;

	switch (clang_getCursorKind(cursor)) {
		case CXCursor_FunctionDecl: {
			data = enif_make_list_cell(env, enif_make_string(env, "FUNCTION", ERL_NIF_LATIN1), data);

			tmp = clang_getCursorSpelling(cursor);
			data = enif_make_list_cell(env, enif_make_string(env, clang_getCString(tmp), ERL_NIF_LATIN1), data);
			clang_disposeString(tmp);

			type = clang_getCursorType(cursor);
			tmp = clang_getTypeSpelling(type);
			data = enif_make_list_cell(env, enif_make_string(env, clang_getCString(tmp), ERL_NIF_LATIN1), data);
			clang_disposeString(tmp);

			((Data*)client_data)->payload = data;
			return CXChildVisit_Recurse;
		}
		case CXCursor_StructDecl: {
			if ((clang_getCursorKind(parent)==CXCursor_FieldDecl) ||
				(clang_getCursorKind(parent)==CXCursor_TypedefDecl) ||
				(clang_getCursorKind(parent)==CXCursor_TranslationUnit) ||
				(clang_getCursorKind(parent)==CXCursor_FunctionDecl) ||
				(clang_getCursorKind(parent)==CXCursor_ParmDecl)) 
			{
				data = enif_make_list_cell(env, enif_make_string(env, "STRUCT", ERL_NIF_LATIN1), data);
				tmp = clang_getCursorSpelling(cursor);
				ctmp = (char*)clang_getCString(tmp);
				if ((clang_getCursorKind(parent)==CXCursor_TranslationUnit) && (!strlen(ctmp))) {
					clang_disposeString(tmp);
					return CXChildVisit_Continue;
				}
				data = enif_make_list_cell(env, enif_make_string(env, ctmp, ERL_NIF_LATIN1), data);
				clang_disposeString(tmp);

				((Data*)client_data)->payload = data;
				return CXChildVisit_Recurse;
			} else {
				return CXChildVisit_Continue;
			}
		}
		case CXCursor_FieldDecl: {
			data = enif_make_list_cell(env, enif_make_string(env, "FIELD", ERL_NIF_LATIN1), data);

			type = clang_getCursorType(parent);
			tmp = clang_getTypeSpelling(type);
			data = enif_make_list_cell(env, enif_make_string(env, clang_getCString(tmp), ERL_NIF_LATIN1), data);
			clang_disposeString(tmp);

			tmp = clang_getCursorSpelling(cursor);
			data = enif_make_list_cell(env, enif_make_string(env, clang_getCString(tmp), ERL_NIF_LATIN1), data);
			clang_disposeString(tmp);

			type = clang_getCursorType(cursor);
			tmp = clang_getTypeSpelling(type);
			data = enif_make_list_cell(env, enif_make_string(env, clang_getCString(tmp), ERL_NIF_LATIN1), data);
			clang_disposeString(tmp);

			((Data*)client_data)->payload = data;
			return CXChildVisit_Recurse;
		}
		case CXCursor_TypedefDecl: {
			data = enif_make_list_cell(env, enif_make_string(env, "TYPEDEF", ERL_NIF_LATIN1), data);
			tmp = clang_getCursorSpelling(cursor);
			data = enif_make_list_cell(env, enif_make_string(env, clang_getCString(tmp), ERL_NIF_LATIN1), data);
			clang_disposeString(tmp);

			type =  clang_getTypedefDeclUnderlyingType(cursor);
			tmp = clang_getTypeSpelling(type);
			data = enif_make_list_cell(env, enif_make_string(env, clang_getCString(tmp), ERL_NIF_LATIN1), data);
			clang_disposeString(tmp);

			((Data*)client_data)->payload = data;
			return CXChildVisit_Recurse;
		}
		case CXCursor_ParmDecl: {
			data = enif_make_list_cell(env, enif_make_string(env, "PARAMETER", ERL_NIF_LATIN1), data);
			tmp = clang_getCursorSpelling(cursor);
			data = enif_make_list_cell(env, enif_make_string(env, clang_getCString(tmp), ERL_NIF_LATIN1), data);
			clang_disposeString(tmp);
			
			type = clang_getCursorType(cursor);
			tmp = clang_getTypeSpelling(type);
			data = enif_make_list_cell(env, enif_make_string(env, clang_getCString(tmp), ERL_NIF_LATIN1), data);
			clang_disposeString(tmp);

// 			tmp = clang_getCursorSpelling(parent);
// 			data = enif_make_list_cell(env, enif_make_string(env, clang_getCString(tmp), ERL_NIF_LATIN1), data);
// 			clang_disposeString(tmp);

			((Data*)client_data)->payload = data;
			return CXChildVisit_Recurse;
		}
		default: {
			return CXChildVisit_Continue;
		}
	}
}

static void print_fails(CXTranslationUnit t)
{
	for (unsigned i = 0, n = clang_getNumDiagnostics(t); i!=n; i++) {
		CXDiagnostic diag = clang_getDiagnostic(t, i);
		CXString s = clang_formatDiagnostic(diag, clang_defaultDiagnosticDisplayOptions());
		fprintf(stderr, "%s\r\n", clang_getCString(s));
		clang_disposeString(s);
	}
}

static char*
term2string(ErlNifEnv* env, ERL_NIF_TERM s)
{
	char* ret;
	unsigned int n;
	int atom=0;
	if (!enif_get_list_length(env, s, &n))
	{
		if (!enif_get_atom_length(env, s, &n, ERL_NIF_LATIN1))
			return NULL;
		else
			atom=1;
	}
	ret = enif_alloc(sizeof(char)*(n+1));
	if (!atom)
	{
		if (!enif_get_string(env, s, ret, n+1, ERL_NIF_LATIN1)) {
			enif_free(ret);
			return NULL;
		}
	} else {
		if (!enif_get_atom(env, s, ret, n+1, ERL_NIF_LATIN1)) {
			enif_free(ret);
			return NULL;
		}
	}
	return ret;
}

static ERL_NIF_TERM
parse_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	char **cargv, *arg;
	unsigned arg_count, i;
	ERL_NIF_TERM head, tail, list, retval;
	int errorval=0;

	list = argv[0];
	if (!enif_get_list_length(env, list, &arg_count)) {
		errorval = 1;
		goto error;
	}

	cargv = enif_alloc(sizeof(char*)*arg_count);

	for (i = 0; i<arg_count; i++) {
		if (!enif_get_list_cell(env, list, &head, &tail)) {
			errorval = 1;
			goto free_cells;
		}
		list = tail;
		if (!(cargv[i] = term2string(env, head))) {
			errorval = 1;
			goto free_cells;
		}
	}
	retval = cparse(env, arg_count, cargv);

free_cells:
	arg_count = i;
	for (i=0; i<arg_count; i++) {
		enif_free(cargv[i]);
	}
	enif_free(cargv);

error:
	if (errorval)
		return enif_make_badarg(env);
	else
		return retval;
}

static ErlNifFunc nif_funcs[] = {
	{"cparse", 1, parse_impl},
};

int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info)
{
	return 0;
}


ERL_NIF_INIT(clang_parse, nif_funcs, NULL, NULL, upgrade, NULL);