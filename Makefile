RUST_2018_IDIOMS = -D bare-trait-objects  \
                   -D ellipsis-inclusive-range-patterns \
                   -D unused-extern-crates

DENY = -D warnings -D future-incompatible -D unused ${RUST_2018_IDIOMS}

${HOME}/.cargo/bin/cargo-tree:
	cargo install cargo-tree

${HOME}/.cargo/bin/cargo-audit:
	cargo install cargo-audit

tree: ${HOME}/.cargo/bin/cargo-tree
	PATH=${HOME}/.cargo/bin:${PATH} cargo tree

audit: ${HOME}/.cargo/bin/cargo-audit
	PATH=${HOME}/.cargo/bin:${PATH} cargo audit -D

fmt:
	cargo fmt

travis_fmt:
	cargo fmt -- --check

build:
	RUSTFLAGS="${DENY}" cargo build

build-tests:
	RUSTFLAGS="${DENY}" cargo test --no-run

test:
	RUSTFLAGS="${DENY}" RUST_BACKTRACE=1 cargo test -- --skip sudo_ --skip loop_

sudo_test:
	RUSTFLAGS="${DENY}" RUST_BACKTRACE=1 RUST_TEST_THREADS=1 cargo test

clippy:
	cargo clippy --all-targets --all-features -- -D warnings -D clippy::needless_borrow

docs:
	cargo doc --no-deps

yamllint:
	yamllint --strict .github/workflows/main.yml

.PHONY:
	audit
	build
	clippy
	docs
	fmt
	sudo_test
	test
	travis_fmt
	tree
	yamllint
