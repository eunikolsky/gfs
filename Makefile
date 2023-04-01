MAIN_TEST_TARGET = gfs:test:gfs-test

.PHONY:
check: check-build check-test
# FIXME add check-hlint

.PHONY:
check-build:
	stack --verbosity error build --fast

.PHONY:
check-test:
	stack --verbosity error test --fast --ta='-f silent' $(MAIN_TEST_TARGET)

.PHONY:
ghcid:
	@ghcid -c 'stack ghci'

.PHONY:
ghcid-test:
	@ghcid -c 'stack ghci $(MAIN_TEST_TARGET) --ghci-options=-fobject-code' -T main

.PHONY:
install-precommit-hook:
	# GNU ln supports the `-r` option to create a relative symlink
	gln -srv .git-pre-commit .git/hooks/pre-commit
