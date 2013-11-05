#include <clang-c/Index.h>
#include <stdio.h>
#include <string.h>

static void walk_cursor(CXTranslationUnit t, CXCursor c);
static enum CXChildVisitResult visitor_cb(CXCursor Cursor, CXCursor Parent, CXClientData ClientData);
static void print_fails(CXTranslationUnit t);

int
main(int argc, char *argv[]) {
	CXIndex Index = clang_createIndex(0,0);
	CXTranslationUnit TU = clang_parseTranslationUnit(Index, 0, (const char**)(argv+1), argc-1, 0,0, CXTranslationUnit_None);

	if (!clang_getNumDiagnostics(TU)) {
		walk_cursor(TU, clang_getTranslationUnitCursor(TU));
	} else {
		// print errors
		print_fails(TU);
	}
	clang_disposeTranslationUnit(TU);
	clang_disposeIndex(Index);
	return 0;
}

static void
walk_cursor(CXTranslationUnit t, CXCursor c) {
	CXCursorVisitor visitor = visitor_cb;
	clang_visitChildren(c, visitor, t);
}

static enum CXChildVisitResult
visitor_cb(CXCursor cursor, CXCursor parent, CXClientData client_data)
{
	char* ctmp;
	CXString tmp;
	CXType type;
	switch (clang_getCursorKind(cursor)) {
		case CXCursor_FunctionDecl: {
			tmp = clang_getCursorSpelling(cursor);
			printf("FUNCTION name: %s", clang_getCString(tmp));
			clang_disposeString(tmp);
			
			type = clang_getCursorType(cursor);
			tmp = clang_getTypeSpelling(type);
			printf("\ttype: %s\n", clang_getCString(tmp));
			clang_disposeString(tmp);

			return CXChildVisit_Recurse;
		}
		case CXCursor_StructDecl: {
			if ((clang_getCursorKind(parent)==CXCursor_FieldDecl) ||
				(clang_getCursorKind(parent)==CXCursor_TypedefDecl) ||
				(clang_getCursorKind(parent)==CXCursor_TranslationUnit) ||
				(clang_getCursorKind(parent)==CXCursor_FunctionDecl) ||
				(clang_getCursorKind(parent)==CXCursor_ParmDecl))
			{
				tmp = clang_getCursorSpelling(cursor);
				ctmp = (char*)clang_getCString(tmp);
				if ((clang_getCursorKind(parent)==CXCursor_TranslationUnit) && (!strlen(ctmp))) {
					clang_disposeString(tmp);
					return CXChildVisit_Continue;
				}
				printf("STRUCT name: %s\n", ctmp);
				clang_disposeString(tmp);
				return CXChildVisit_Recurse;
			} else {
				printf("skip: %d\n", clang_getCursorKind(parent));
				return CXChildVisit_Continue;
			}
		}
		case CXCursor_FieldDecl: {
			tmp = clang_getCursorSpelling(cursor);
			printf("FIELD name: %s", clang_getCString(tmp));
			clang_disposeString(tmp);

			type = clang_getCursorType(parent);
			tmp = clang_getTypeSpelling(type);
			printf("\tof %s", clang_getCString(tmp));
			clang_disposeString(tmp);
			
			type = clang_getCursorType(cursor);
			tmp = clang_getTypeSpelling(type);
			printf("\ttype: %s\n", clang_getCString(tmp));
			clang_disposeString(tmp);

			return CXChildVisit_Recurse;
		}
		case CXCursor_TypedefDecl: {
			tmp = clang_getCursorSpelling(cursor);
			printf("TYPEDEF name: %s", clang_getCString(tmp));
			clang_disposeString(tmp);

			type =  clang_getTypedefDeclUnderlyingType(cursor);
			tmp = clang_getTypeSpelling(type);
			printf("\ttype: %s \n", clang_getCString(tmp));
			clang_disposeString(tmp);

			return CXChildVisit_Recurse;
		}
// 		case CXCursor_TypeRef: {
// 			if (clang_getCursorKind(parent)==CXCursor_TypedefDecl) {
// 				tmp = clang_getCursorSpelling(cursor);
// 				printf("\ttype: %s\n", clang_getCString(tmp));
// 				clang_disposeString(tmp);
// 				return CXChildVisit_Recurse;
// 			}
// 			
// // 			CXCursor rc =  clang_getCursorDefinition(cursor);
// // 			tmp = clang_getCursorSpelling(rc);
// // 			printf("\treferenced: %s\n", clang_getCString(tmp));
// // 			clang_disposeString(tmp);
// 			return CXChildVisit_Continue;
// 		}
		case CXCursor_ParmDecl: {
			tmp = clang_getCursorSpelling(cursor);
			printf("PARAMETER name: %s", clang_getCString(tmp));
			clang_disposeString(tmp);
			
			type = clang_getCursorType(cursor);
			tmp = clang_getTypeSpelling(type);
			printf("\ttype: %s ", clang_getCString(tmp));
			clang_disposeString(tmp);

			tmp = clang_getCursorSpelling(parent);
			printf(" of %s\n", clang_getCString(tmp));
			clang_disposeString(tmp);

			return CXChildVisit_Recurse;
		}
		default: {
// 			tmp = clang_getCursorSpelling(cursor);
// 			printf("DEFL name: %s type: %d\n", clang_getCString(tmp), clang_getCursorKind(cursor));
// 			clang_disposeString(tmp);
// 			return CXChildVisit_Recurse;
			return CXChildVisit_Continue;
	}
	}
}

static void print_fails(CXTranslationUnit t)
{
	for (unsigned i = 0, n = clang_getNumDiagnostics(t); i!=n; i++) {
		CXDiagnostic diag = clang_getDiagnostic(t, i);
		CXString s = clang_formatDiagnostic(diag, clang_defaultDiagnosticDisplayOptions());
		fprintf(stderr, "%s\n", clang_getCString(s));
		clang_disposeString(s);
	}
}
