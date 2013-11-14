{#
 # forced buildins
 #}
{% if type=="char *" %}{% include "from_erl/char *.tpl" %}{% else %}
{#
 # structures
 #}
{% if "struct" in type %}
{% include "from_erl/struct.tpl" %}
{% else %}
{#
 # lists (or pointer)
 #}
{% if "*" in type or "[" in type %}
{% include "from_erl/list.tpl" %}
{% else %}
{#
 # simple types
 #}
{% with tplname="from_erl/"|add:type|add:".tpl" %}{% include tplname %}{% endwith %}
{% endif %}{% endif %}{% endif %}