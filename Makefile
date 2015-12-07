
.PHONY: clean test mount cabal-setup

dist/build/locatefs/locatefs: cabal-setup src/locatefs.hs
	cabal build

locatefs: dist/build/locatefs/locatefs
	ln -s ./dist/build/locatefs/locatefs locatefs

cabal-setup:
	cabal sandbox init
	cabal install

clean:
	- rm -rf dist .cabal-sandbox locatefs cabal.sandbox.config

watch:
	axe $(shell find src -type f -name '*.hs') - make

mount:
	- sudo umount /locate
	./locatefs /locate

test: mount
	@ sleep 1
	ls -la /locate
	ls -la /locate/猫
	ls -la /locate/pacman.log/var_-log_-pacman.log
	head -n 3 /locate/pacman.log/var_-log_-pacman.log
