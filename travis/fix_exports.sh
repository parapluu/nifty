if grep -q "nowarn_export_all" _build/default/lib/erlware_commons/src/ec_semver_parser.erl
then
    echo "ec_semver_parser.erl ... ok"
else
    echo "Supress export_all warning in ec_semver_parser.erl"
    sed -i "s/-compile(export_all)\./-compile(export_all)\.\n-compile(nowarn_export_all)\./g" _build/default/lib/erlware_commons/src/ec_semver_parser.erl
fi

if grep -q "nowarn_export_all" _build/default/lib/relx/src/rlx_goal.erl
then
    echo "rlx_goal.erl         ... ok"
else
    echo "Supress export_all warning in rlx_goal.erl"
    sed -i "s/-compile(export_all)\./-compile(export_all)\.\n-compile(nowarn_export_all)\./g" _build/default/lib/relx/src/rlx_goal.erl
fi
