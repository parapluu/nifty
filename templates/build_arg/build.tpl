{% for rettype, args in data %}{% for erlname, atype in args %}
{% with cname="c_arg_"|add:erlname %}
{% with arg="argv["|add:forloop.counter0|add:"]" %}
{% with tplname="build_arg/"|add:atype|add:".tpl" %}
{% include tplname %}
{% endwith %}{% endwith %}{% endwith %}
{% endfor %}{% endfor %}