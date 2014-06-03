%%% -------------------------------------------------------------------
%%% Copyright (c) 2014, Andreas Löscher <andreas.loscher@it.uu.se> and
%%%                     Konstantinos Sagonas <kostis@it.uu.se>
%%% All rights reserved.
%%%
%%% This file is distributed under the Simplified BSD License.
%%% Details can be found in the LICENSE file.
%%% -------------------------------------------------------------------

-define(NEW_CONFIG, []).
-define(ADD_SOURCES(S, C), nifty_utils:merge_nif_spec(C, {".*", "$NIF", S, [{env, []}]})).
-define(ADD_CFLAG(F, C), nifty_utils:merge_nif_spec(C, {".*", "$NIF", [], [{env, [{"CFLAGS", "$CFLAGS "++ F}]}]})).
-define(ADD_LDFLAG(F, C), nifty_utils:merge_nif_spec(C, {".*", "$NIF", [], [{env, [{"LDFLAGS", "$LDFLAGS "++ F}]}]})).
