# SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

.PHONY: xrefcheck test lint stylish clean bats all

# Build target from the common utility Makefile
MAKEU = $(MAKE) -C make/

MAKE_PACKAGE = $(MAKEU) PACKAGE=xrefcheck

xrefcheck:
	$(MAKE_PACKAGE) dev

# Runs all tests.
# Usage:
#  * make test
#  * make test BATSFILTER='test name' TEST_ARGUMENTS='--match \"test name\"'
test:
	make test-unit
	make test-ftp
	make bats

test-dumb-term:
	$(MAKE_PACKAGE) test-dumb-term

test-hide-successes:
	$(MAKE_PACKAGE) test-hide-successes

clean:
	$(MAKE_PACKAGE) clean

lint:
	hlint .

stylish:
	find . -name '.stack-work' -prune -o -name '.dist-newstyle' -prune -o -name '*.hs' -exec stylish-haskell -i '{}' \;

####################################
# Individual test suites

test-unit:
	$(MAKEU) test PACKAGE="xrefcheck:test:xrefcheck-tests"

test-ftp:
	vsftpd \
    -orun_as_launching_user=yes \
    -olisten_port=2221 \
    -olisten=yes \
    -oftp_username=$(shell whoami) \
    -oanon_root=./ftp-tests/ftp_root \
    -opasv_min_port=2222 \
    -ohide_file='{.*}' \
    -odeny_file='{.*}' \
    -oseccomp_sandbox=no \
    -olog_ftp_protocol=yes \
    -oxferlog_enable=yes \
    -ovsftpd_log_file=/tmp/vsftpd.log &

	$(MAKEU) test PACKAGE="xrefcheck:test:ftp-tests" \
		TEST_ARGUMENTS="--ftp-host ftp://127.0.0.1:2221"

# Usage:
#   * make bats
#   * make bats BATSFILTER="test name"
#   * make bats BATSFILTER="regex to match several tests"
bats:
	stack install --fast xrefcheck:exe:xrefcheck
	git submodule update --init --recursive
	bats ./tests/golden/** -f $(if $(BATSFILTER),"$(BATSFILTER)",".*")
