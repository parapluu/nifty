#!/bin/sh
mkdir -p cache

# LLVM
LLVM_URL="http://llvm.org/releases/$LLVM_VERSION/clang+llvm-$LLVM_VERSION-x86_64-linux-gnu-ubuntu-14.04.tar.xz"
LLVM_DEP="cache/$LLVM_VERSION.tar.xz"
LLVM_DIR="cache/clang+llvm-$LLVM_VERSION"

echo $LLVM_URL
echo $LLVM_DEP
echo $LLVM_DIR

# download and install
if [ ! -f $LLVM_DEP ] ; then
	echo "Downloading LLVM"
	wget $LLVM_URL -O $LLVM_DEP
	if [ ! $? -eq 0 ]; then
		echo "Error: Download failed"
		rm $LLVM_DEP || true
		exit 1
	fi
else
	echo "Found Cached Archive"
fi

if [ ! -d $LLVM_DIR ] ; then
	echo "Extracting LLVM"
	tar -xf $LLVM_DEP -C cache/
	if [ ! $? -eq 0 ]; then
		echo "Error: Extraction Failed"
		rm $LLVM_DEP || true
		exit 1
	fi
	case $LLVM_VERSION in
		"3.5.2")
			mv cache/clang+llvm-3.5.2-x86_64-linux-gnu cache/clang+llvm-3.5.2
		;;
		?*)
			mv cache/clang+llvm-$LLVM_VERSION-x86_64-linux-gnu-ubuntu-14.04 cache/clang+llvm-$LLVM_VERSION
		;;
	esac
else
	echo "Found Cached Installation"
fi

# Erlang OTP from GIT
if [ "$TRAVIS_OTP_RELEASE" = "19.prerelease" ] ; then
	echo "OTP 19  ($TRAVIS_OTP_RELEASE)"
	OTP_SRC="cache/OTP_19_SRC"
	OTP_DIR="$PWD/cache/OTP_19_INSTALL"
	OTP_GIT="https://github.com/erlang/otp.git"
	OTP_REF="2ce17d4"
	if [ ! -d $OTP_DIR ] ; then
		rm -rf $OTP_SRC || true
		git clone $OTP_GIT -b master $OTP_SRC
		if [ ! $? -eq 0 ] ; then
			echo "Error: could not clone OTP repo"
			rm -rf $OTP_SRC || true
			exit 1
		fi
		export ROOT=$PWD
		cd $OTP_SRC
		export ERL_TOP=`pwd`
		git checkout $OTP_REF
		./otp_build autoconf
		./otp_build configure --enable-dirty-schedulers --prefix=$OTP_DIR
		make
		make install
		if [ ! $? -eq 0 ] ; then
			echo "Error: could not build OTP"
			cd $ROOT
			rm -rf $OTP_DIR || true
			exit 1
		fi
		cd $ROOT
	else
		echo "OPT 19 Pre-Release Found"
	fi
fi

# PLT
cp cache/plt-$TRAVIS_OTP_RELEASE .nifty_plt || true
