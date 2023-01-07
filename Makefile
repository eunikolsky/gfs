.PHONY:
ghcid:
	@ghcid -c 'stack ghci'

.PHONY:
ghcid-test:
	@ghcid -c 'stack ghci tscleaner:lib tscleaner:test:tscleaner-test --ghci-options=-fobject-code' -T main
