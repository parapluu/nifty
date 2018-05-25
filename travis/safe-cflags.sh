#!/bin/sh
UNSAFE_CFLAGS=`$1 --cflags`
FLAGS_LIST=$(echo $UNSAFE_CFLAGS | tr " " "\n" )
SAFE_FLAGS=""
for FLAG in $FLAGS_LIST
do
	case $FLAG in
		"-Wcovered-switch-default")
		;;
		"-Werror=date-time")
		;;
		"-Wdelete-non-virtual-dtor")
		;;
		"-Wstring-conversion")
                ;;
                "-Werror=unguarded-availability-new")
		;;
		-D*)
		;;
		?*)
			SAFE_FLAGS="$SAFE_FLAGS $FLAG"
		;;
	esac
done
echo $SAFE_FLAGS
