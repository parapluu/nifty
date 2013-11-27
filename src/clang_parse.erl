-module(clang_parse).
-export([parse/1, build_vars/1]).
-on_load(init/0).

init() ->
    ok = erlang:load_nif("clang_parse", 0).

cparse(_) ->
    exit(nif_library_not_loaded).

parse(Args) ->
    {cparse(Args), Args}.

build_vars(Token) ->
    build_vars(Token, {dict:new(), dict:new(), dict:new()}).

build_vars(L, false) ->
    {fail, L};
build_vars([], Definitions) ->
    Definitions;
build_vars([H|T], Definitions) ->
    case H of
	"FUNCTION" ->
	    {NewT, Defs} = build_function(T, Definitions);
	"STRUCT" ->
	    {NewT, Defs} = build_struct(T, Definitions);
	"TYPEDEF" ->
	    {NewT, Defs} = build_typedef(T, Definitions);
	%% Defs = Definitions,
	%% [_|[_|NewT]] = T;
	_ ->
	    Defs = false,
	    NewT = [H|T]
    end,
    build_vars(NewT, Defs).

build_type([Type|T], {Functions, TypeDefs, Structs}) ->
    {NT, Defs} = 
	case (string:str(Type, "<anonymous")>0) of
	    true-> 
		[_,_|RestToken] = T,
		build_named_struct(RestToken, {Functions, TypeDefs, Structs}, string:substr(Type, 8));
	    false ->
		{T, {Functions, TypeDefs, Structs}}
	end,
    {NT, Defs, Type}.

build_function([FuncName|T], {Functions, TypeDefs, Structs}) ->
    {PT, PD, Rettype} = build_rettype(T, {Functions, TypeDefs, Structs}),
    {NT, {FD, TD, SD}, Params} = build_params(PT, PD),
    NFD = dict:append(FuncName, {Rettype, Params}, FD),
    {NT, {NFD, TD, SD}}.

build_rettype([Type|T], Definitions) ->
    PureType = string:strip(string:substr(Type, 1, string:str(Type, "(")-1)),
    build_type([PureType|T], Definitions).

build_param([Ident|T], Definitions) ->
    case Ident of
	"PARAMETER" ->
	    [Name|TT] = T,
	    {NT, Defs, Type} = build_type(TT, Definitions),
	    {NT, Defs, {Name, Type}};
	_ -> {[Ident|T], Definitions, stop}
    end.

build_params(T, Definitions) ->
    {NT, Data, Params} = build_params(T, Definitions, []),
    {NT, Data, lists:reverse(Params)}.

build_params([], Definitions, Params) ->
    {[], Definitions, Params};
build_params(T, Definitions, Params) ->
    {NT, Defs, Param} = build_param(T, Definitions),
    case Param of
	stop -> {NT, Defs, Params};
	_ -> build_params(NT, Defs, [Param|Params])
    end.

build_field([Name|T], Definitions) ->
    {NT, Defs, Type} = build_type(T, Definitions),
    {NT, Defs, {Name, Type}}.

build_field_save([], Definitions, _) ->
    {[], Definitions, stop};
build_field_save([Ident|T], Definitions, ParentName) ->
    {_, TypeDefs, _} = Definitions,
    case Ident of
	"FIELD" -> 
	    [Parent|NT] = T,
	    case string:substr(resolve_type(strip_type_name(Parent), TypeDefs),8)=:=strip_type_name(ParentName) of
		true ->
		    build_field(NT, Definitions);
		false ->
		    {[Ident|T], Definitions, stop}
	    end;
	_ -> 
	    {[Ident|T], Definitions, stop}
    end.

build_fields(T, Definitions, Name) ->
    build_fields(T, Definitions, Name, []).

build_fields(T, Definitions, Name, Fields) ->
    {NT, Defs, Field} = build_field_save(T, Definitions, Name),
    case Field of
	stop -> 
	    {NT, Defs, Fields};
	_ -> build_fields(NT, Defs, Name, [Field|Fields])
    end.

build_named_struct(T, Definitions, Name) ->
    {NT, {Functions, TypeDefs, Structs}, Fields} = build_fields(T, Definitions, Name),
    NewStructs = dict:append(Name, Fields, Structs),
    {NT, {Functions, TypeDefs, NewStructs}}.

build_struct([Name|T], Definitions) ->
    build_named_struct(T, Definitions, Name).

build_typedef([Name| T], Definitions) ->
						% special case with anonymous structs
    {NT, {Functions, TypeDefs, Structs}, Type} = build_type(T, Definitions),
    Expr = ((string:str(Type, "struct")>0) andalso (not (dict:is_key(Type, Structs)))),
    case Expr of
	true ->
	    TypeName = string:substr(Type, 8),
	    [_|[_|RestToken]] = NT,
	    build_named_struct(RestToken, {Functions, dict:append(Name, Type, TypeDefs), Structs}, TypeName);
	false ->
	    {NT, {Functions, dict:append(Name, Type, TypeDefs), Structs}}
    end.

strip_type_name(Name) ->
    Index = string:str(Name, "::"),
    Temp = case Index of
	       0 -> Name;
	       _ -> 
		   case string:str(Name, "struct") of
		       1 -> "struct " ++ string:substr(Name, Index+2);
		       _ -> string:substr(Name, Index+2)
		   end
	   end,
    case string:str(Temp, "struct") of
	12 -> string:substr(Temp, 1, 11) ++ string:substr(Temp, 19);
	_ -> Temp
    end.


resolve_type(Name, TypeDefs) -> 
    case dict:is_key(Name, TypeDefs) of
	true ->
	    [ResolvedName|_] = dict:fetch(Name, TypeDefs),
	    resolve_type(ResolvedName, TypeDefs);
	false ->
	    Name
    end.

