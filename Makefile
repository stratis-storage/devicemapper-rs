RUST_2018_IDIOMS = -D bare-trait-objects  \
		   -D ellipsis-inclusive-range-patterns \
		   -D unused-extern-crates

DENY = -D warnings -D future-incompatible -D unused ${RUST_2018_IDIOMS}

${HOME}/.cargo/bin/cargo-expand:
	cargo install cargo-expand

${HOME}/.cargo/bin/cargo-tree:
	cargo install cargo-tree

expand: ${HOME}/.cargo/bin/cargo-expand
	PATH=${HOME}/.cargo/bin:${PATH} cargo expand core::errors
	PATH=${HOME}/.cargo/bin:${PATH} cargo expand core::types
	PATH=${HOME}/.cargo/bin:${PATH} cargo expand units

tree: ${HOME}/.cargo/bin/cargo-tree
	PATH=${HOME}/.cargo/bin:${PATH} cargo tree

fmt:
	cargo fmt

travis_fmt:
	cargo fmt -- --check

build:
	RUSTFLAGS="${DENY}" cargo build

test:
	RUSTFLAGS="${DENY}" RUST_BACKTRACE=1 cargo test -- --skip sudo_ --skip loop_

sudo_test:
	sudo env "PATH=${PATH}" RUSTFLAGS="${DENY}" RUST_BACKTRACE=1 RUST_TEST_THREADS=1 cargo test

clippy:
	cargo clippy --all-targets --all-features -- -D warnings

docs:
	cargo doc --no-deps

.PHONY:
	build
	clippy
	docs
	fmt
	sudo_test
	test
	travis_fmt
	tree
