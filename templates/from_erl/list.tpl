	if (enif_is_list(env, {{erlname}})) {
		enif_get_list_length(env, {{erlname}}, &l);
		{{cname}} = sizeof({{type|dereference_type}}*l);
		{% with lc=type|loopcounter:cname %}
		unsigned int {{lc}}=0;
		while (enif_get_list_cell(env, &head, &tail)) {
			{% with type=type|dereference_type %}
			{% with tplname="from_erl/"|add:type|add:".tpl" %}
		{% include tplname with cname="("|add:cname|add:"["|add:lc|add:"]" erlname="head" %}
			{% endwith %}{% endwith %}
			{{lc}}++;
			}
		{% endwith %}
	} else {
		/* pointer to datatype, build dereferenced type */
		{% with type=type|dereference_type %}{% with tplname="from_erl/"|add:type|add:".tpl" %}{% include tplname with cname="(*"|add:cname|add:")" %}{% endwith %}{% endwith %}
	}