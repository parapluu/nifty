{erl_opts, [debug_info]}.
{deps, []}.

{plugins, [{ pc, {git, "https://github.com/blt/port_compiler.git", {branch, "master"}}}]}.

{provider_hooks,
 [{pre,[{compile, {pc, compile}},
        {clean, {pc, clean}}]}]}.

{% for E in config %}
{{E}}.
{% endfor %}
