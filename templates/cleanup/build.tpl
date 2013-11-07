{% with tplname="cleanup/"|add:type|add:".tpl" %}
{% if "*" in tplname %}{% include tplname %}{% endif %}
{% endwith %}