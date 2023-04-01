.PHONY:
ghcid:
	@ghcid -c 'stack ghci'

.PHONY:
ghcid-test:
	@ghcid -c 'stack ghci gfs:lib gfs:test:gfs-test --ghci-options=-fobject-code' -T main
