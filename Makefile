DENY = "-D warnings -D future-incompatible -D unused"

${HOME}/.cargo/bin/cargo-tree:
	cargo install cargo-tree

tree: ${HOME}/.cargo/bin/cargo-tree
	PATH=${HOME}/.cargo/bin:${PATH} cargo tree

fmt:
	cargo fmt

travis_fmt:
	cargo fmt -- --check

build:
	RUSTFLAGS=${DENY} cargo build

test:
	RUSTFLAGS=${DENY} RUST_BACKTRACE=1 cargo test -- --skip sudo_ --skip loop_

sudo_test:
	sudo env "PATH=${PATH}" RUSTFLAGS=${DENY} RUST_BACKTRACE=1 RUST_TEST_THREADS=1 cargo test

clippy:
	cargo clippy --all-targets --all-features -- -D warnings

.PHONY:
	build
	clippy
	fmt
	sudo_test
	test
	travis_fmt
	tree
