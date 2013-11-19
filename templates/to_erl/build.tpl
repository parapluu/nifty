{% if "struct" in type %}
{% include "to_erl/struct.tpl" %}
{% else %}
{% with tplname="to_erl/"|add:type|add:".tpl" %}
{% include tplname %}
{% endwith %}
{% endif %}