#!/usr/bin/env bash

export PREFIX=/opt/hare

gitenter() {
	local package="$1"
	local repo="$2"

	echo "### $package ###"

	local src="$PREFIX/build/$1"
	if [[ -d "$src" ]]; then
		git -C "$src" pull
	else
		git clone "$repo" "$src"
	fi
	pushd "$src"
}

gitenter qbe git://c9x.me/qbe.git
	make PREFIX=$PREFIX
	make install PREFIX=$PREFIX
popd

gitenter scdoc https://git.sr.ht/~sircmpwn/scdoc
	make PREFIX=$PREFIX
	make install PREFIX=$PREFIX
popd

gitenter harec https://git.sr.ht/~sircmpwn/harec
	mkdir build
	cd build
	QBE=/opt/hare/bin/qbe ../configure --prefix=$PREFIX
	make
	make install
popd

gitenter hare https://git.sr.ht/~sircmpwn/hare
	cat config.example.mk \
		| sed 's:/usr/local:/opt/hare:' \
		| sed 's:harec:/opt/hare/bin/harec:' \
		| sed 's:qbe:/opt/hare/bin/qbe:' \
		| sed 's:scdoc:/opt/hare/bin/scdoc:' \
		> config.mk
	make
	make install
popd

for bin in $PREFIX/bin/*; do
	local linkname="$HOME/.local/bin/$(basename "$bin")"
	if [[ ! -e "$linkname" ]]; then
		ln -s "$bin" "$linkname"
	fi
done
