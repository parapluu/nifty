{% for rettype, args in data %}
{% with tplname="build_retval/"|add:rettype|add:".tpl" %}
{% include tplname %}
{% endwith %}
{% endfor %}