ifeq ($(origin FEDORA_RELEASE), undefined)
else
  FEDORA_RELEASE_ARGS = --release=${FEDORA_RELEASE}
endif

ifeq ($(origin MANIFEST_PATH), undefined)
else
  MANIFEST_PATH_ARGS = --manifest-path=${MANIFEST_PATH}
endif

IGNORE_ARGS ?=

DENY = -D warnings -D future-incompatible -D unused -D rust_2018_idioms -D nonstandard_style

${HOME}/.cargo/bin/cargo-tree:
	cargo install cargo-tree

${HOME}/.cargo/bin/cargo-audit:
	cargo install cargo-audit

${HOME}/.cargo/bin/cargo-tarpaulin:
	cargo install cargo-tarpaulin

tree: ${HOME}/.cargo/bin/cargo-tree
	PATH=${HOME}/.cargo/bin:${PATH} cargo tree

audit: ${HOME}/.cargo/bin/cargo-audit
        # --ignore=RUSTSEC-2021-0139 is required due to old loopdev dependency
	PATH=${HOME}/.cargo/bin:${PATH} cargo audit -D warnings \
        --ignore=RUSTSEC-2021-0139

tarpaulin: ${HOME}/.cargo/bin/cargo-tarpaulin
	PATH=${HOME}/.cargo/bin:${PATH}  \
	RUST_BACKTRACE=1 \
	cargo tarpaulin -v -- --test-threads=1

SET_LOWER_BOUNDS ?=
test-set-lower-bounds:
	echo "Testing that SET_LOWER_BOUNDS environment variable is set to a valid path"
	test -e "${SET_LOWER_BOUNDS}"

verify-dependency-bounds: test-set-lower-bounds
	RUSTFLAGS="${DENY}" cargo build ${MANIFEST_PATH_ARGS}
	${SET_LOWER_BOUNDS} ${MANIFEST_PATH_ARGS}
	RUSTFLAGS="${DENY}" cargo build ${MANIFEST_PATH_ARGS}

test-compare-fedora-versions:
	echo "Testing that COMPARE_FEDORA_VERSIONS environment variable is set to a valid path"
	test -e "${COMPARE_FEDORA_VERSIONS}"

check-fedora-versions: test-compare-fedora-versions
	${COMPARE_FEDORA_VERSIONS} ${MANIFEST_PATH_ARGS} ${FEDORA_RELEASE_ARGS} ${IGNORE_ARGS}

fmt:
	cargo fmt
	cd devicemapper-rs-sys && cargo fmt

fmt-ci:
	cargo fmt -- --check
	cd devicemapper-rs-sys && cargo fmt -- --check

build:
	RUSTFLAGS="${DENY}" cargo build

build-tests:
	RUSTFLAGS="${DENY}" cargo test --no-run

test:
	RUSTFLAGS="${DENY}" RUST_BACKTRACE=1 cargo test -- --skip sudo_ --skip loop_

sudo_test:
	RUSTFLAGS="${DENY}" RUST_BACKTRACE=1 RUST_TEST_THREADS=1 cargo test

clippy:
	RUSTFLAGS="${DENY}" \
        cargo clippy --all-targets --all-features -- \
        -D clippy::cargo \
        -D clippy::all
	cd devicemapper-rs-sys && RUSTFLAGS="${DENY}" \
        cargo clippy --all-targets --all-features -- \
        -D clippy::cargo \
        -D clippy::all

docs:
	cargo doc --no-deps

yamllint:
	yamllint --strict .github/workflows/*.yml

.PHONY:
	audit
	build
	check-fedora-versions
	check-fedora-versions-sys
	clippy
	docs
	fmt
	fmt-ci
	sudo_test
	test
	test-compare-fedora-versions
	test-set-lower-bounds
	tree
	verify-dependency-bounds
	yamllint
