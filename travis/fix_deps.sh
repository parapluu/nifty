if [ "$TRAVIS_OTP_RELEASE"=="19.prerelease" ] ; then
	echo "Hacking ErlyDTL to compile"
	sed 's/|18/|18|19/' deps/erlydtl/rebar.config -i
fi
