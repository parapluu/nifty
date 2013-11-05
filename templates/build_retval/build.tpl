{#
 # registrate types here
 #}
{% for rettype, args in data %}
{% include "build_retval/int.tpl" %}
{% endfor %}