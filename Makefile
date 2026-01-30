ifeq ($(origin FEDORA_RELEASE), undefined)
else
  FEDORA_RELEASE_ARGS = --release=${FEDORA_RELEASE}
endif

ifeq ($(origin CLIPPY_FIX), undefined)
  CLIPPY_OPTS = --all-targets --no-deps
else
  CLIPPY_OPTS = --fix
endif

ifeq ($(origin MINIMAL), undefined)
  BUILD = build
  TEST = test
else
  BUILD = minimal-versions build --direct
  TEST = minimal-versions test --direct
endif

IGNORE_ARGS ?=

audit:
	cargo audit -D warnings

check-typos:
	typos

test-compare-fedora-versions:
	echo "Testing that COMPARE_FEDORA_VERSIONS environment variable is set to a valid path"
	test -e "${COMPARE_FEDORA_VERSIONS}"

check-fedora-versions: test-compare-fedora-versions
	${COMPARE_FEDORA_VERSIONS} ${FEDORA_RELEASE_ARGS} ${IGNORE_ARGS}

fmt:
	cargo fmt
	cd devicemapper-rs-sys && cargo fmt

fmt-ci:
	cargo fmt -- --check
	cd devicemapper-rs-sys && cargo fmt -- --check

build:
	cargo ${BUILD}

build-tests:
	cargo ${TEST} --no-run

test:
	RUST_BACKTRACE=1 cargo test -- --skip sudo_ --skip loop_

sudo_test:
	RUST_BACKTRACE=1 RUST_TEST_THREADS=1 CARGO_TARGET_X86_64_UNKNOWN_LINUX_GNU_RUNNER='sudo -E' cargo test

clippy:
	cargo clippy --all-features ${CLIPPY_OPTS}
	(cd devicemapper-rs-sys && cargo clippy --all-features ${CLIPPY_OPTS})

docs:
	cargo doc --no-deps --document-private-items

yamllint:
	yamllint --strict .github/workflows/*.yml .packit.yaml .yamllint.yaml

.PHONY:
	audit
	build
	check-fedora-versions
	check-fedora-versions-sys
	check-typos
	clippy
	docs
	fmt
	fmt-ci
	sudo_test
	test
	test-compare-fedora-versions
	yamllint
