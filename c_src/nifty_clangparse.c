#include <clang-c/Index.h>
#include <stdio.h>
#include <string.h>

#include <erl_nif.h>

static ERL_NIF_TERM walk_cursor(ErlNifEnv* env, CXTranslationUnit t, CXCursor c);
static enum CXChildVisitResult visitor_cb(CXCursor Cursor, CXCursor Parent, CXClientData ClientData);
static enum CXChildVisitResult visitor_function_cb(CXCursor cursor, CXCursor parent, CXClientData client_data);
static enum CXChildVisitResult visitor_struct_cb(CXCursor cursor, CXCursor parent, CXClientData client_data);
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
  ERL_NIF_TERM func_file;
  ERL_NIF_TERM types;
  ERL_NIF_TERM symbol_table;
  ERL_NIF_TERM constr_table;
} Data;

typedef struct _subdata {
  ErlNifEnv* env;
  ERL_NIF_TERM types;
  ERL_NIF_TERM data;
} SubData;


static ERL_NIF_TERM
walk_cursor(ErlNifEnv* env, CXTranslationUnit t, CXCursor c) {
  Data* data = enif_alloc(sizeof(Data));
  data->env = env;
  data->func_file = enif_make_list(env, 0);
  data->types = enif_make_list(env, 0);
  data->symbol_table = enif_make_list(env, 0);
  data->constr_table = enif_make_list(env, 0);
  CXCursorVisitor visitor = visitor_cb;
  clang_visitChildren(c, visitor, (CXClientData)data);
  return enif_make_tuple4(env, 
			  data->func_file,
			  data->types,
			  data->symbol_table,
			  data->constr_table);
}

static enum CXChildVisitResult
visitor_cb(CXCursor cursor, CXCursor parent, CXClientData client_data)
{
  char* ctmp;
  CXString tmp;
  CXType type;

  CXFile file;
  unsigned line;
  unsigned column;
  unsigned offset;
  CXSourceLocation loc;

  Data* data = (Data*)client_data;

  ERL_NIF_TERM ff_l = data->func_file;
  ERL_NIF_TERM fn, funcname;
  ERL_NIF_TERM etmp;
  ErlNifEnv* env = data->env;

  SubData* subd;

  switch (clang_getCursorKind(cursor)) {
  case CXCursor_FunctionDecl: {
    tmp = clang_getCursorSpelling(cursor);
    funcname = enif_make_string(env, clang_getCString(tmp), ERL_NIF_LATIN1);
    clang_disposeString(tmp);

    loc = clang_getCursorLocation(cursor);
    clang_getFileLocation(loc,
    			  &file,
    			  &line,
    			  &column,
    			  &offset);
    
    tmp=clang_getFileName(file);
    fn = enif_make_string(env, clang_getCString(tmp), ERL_NIF_LATIN1);
    clang_disposeString(tmp);
    ff_l = enif_make_list_cell(env, enif_make_tuple2(env, funcname, fn), ff_l);
    data->func_file = ff_l;

    subd = enif_alloc(sizeof(SubData));
    clang_visitChildren(cursor, visitor_function_cb, (CXClientData)subd);

    return CXChildVisit_Continue;
  }
  case CXCursor_StructDecl: {
    tmp = clang_getCursorSpelling(cursor);
    ctmp = (char*)clang_getCString(tmp);
    if ((clang_getCursorKind(parent) == CXCursor_TranslationUnit) && (!strlen(ctmp))) {
      clang_disposeString(tmp);
      return CXChildVisit_Continue;
    } else {
      subd = enif_alloc(sizeof(SubData));
      subd->types = data->types;
      subd->data = enif_make_list(env, 0);
      subd->env = env;
      clang_visitChildren(cursor, visitor_struct_cb, (CXClientData)subd);
      etmp = enif_make_tuple2(env, enif_make_atom(env, "struct"), 
			      enif_make_string(env, ctmp, ERL_NIF_LATIN1));
      clang_disposeString(tmp);
      etmp = enif_make_tuple2(env, etmp, subd->data);
      data->constr_table =
	enif_make_list_cell(env, etmp, data->constr_table);
      data->types = subd->types;
      return CXChildVisit_Continue;
    }
  }
  case CXCursor_TypedefDecl: {
    return CXChildVisit_Continue;
  }
  default: {
    return CXChildVisit_Continue;
  }
  }
}


static enum CXChildVisitResult
visitor_function_cb(CXCursor cursor, CXCursor parent, CXClientData client_data) {
  switch (clang_getCursorKind(cursor)) {
  case CXCursor_ParmDecl: {
    printf("Argument\n\r");
    return CXChildVisit_Continue;
  }
  default: {
    return CXChildVisit_Continue;
  }
  }
}

static enum CXChildVisitResult
visitor_struct_cb(CXCursor cursor, CXCursor parent, CXClientData client_data) {
  ERL_NIF_TERM name;
  ERL_NIF_TERM typename;

  CXString tmp;
  CXType type;
  ERL_NIF_TERM etmp;
  unsigned len;
  SubData *data = (SubData*)client_data;
  ErlNifEnv *env = data->env;

  switch (clang_getCursorKind(cursor)) {
  case CXCursor_FieldDecl: {
    enif_get_list_length(env, data->data, &len);

    tmp = clang_getCursorSpelling(cursor);
    name = enif_make_string(env, clang_getCString(tmp), ERL_NIF_LATIN1);
    clang_disposeString(tmp);

    type = clang_getCursorType(cursor);
    tmp = clang_getTypeSpelling(type);
    typename = enif_make_string(env, clang_getCString(tmp), ERL_NIF_LATIN1);
    data->types = enif_make_list_cell(env, typename, data->types);
    clang_disposeString(tmp);    

    etmp = enif_make_tuple4(env,
			    enif_make_atom(env, "field"),
			    name,
			    typename,
			    enif_make_uint(env, len));
    data->data = enif_make_list_cell(env, etmp, data->data);
    return CXChildVisit_Continue;
  }
  default: {
    return CXChildVisit_Continue;
  }
  }
}

static void print_fails(CXTranslationUnit t)
{
  unsigned i, n;
  for (i = 0, n = clang_getNumDiagnostics(t); i!=n; i++) {
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
  char **cargv;
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

ERL_NIF_INIT(nifty_clangparse, nif_funcs, NULL, NULL, upgrade, NULL);
