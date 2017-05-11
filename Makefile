check: fmt build test

${HOME}/.cargo/bin/cargo-fmt:
	cargo install rustfmt --vers 0.8.3

fmt: ${HOME}/.cargo/bin/cargo-fmt
	PATH=${HOME}/.cargo/bin:${PATH} cargo fmt -- --write-mode=diff

build:
	RUSTFLAGS='-D warnings' cargo build --verbose

test:
	sudo env "PATH=${PATH}" RUSTFLAGS='-D warnings' RUST_BACKTRACE=1 cargo test --verbose

.PHONY:
	check
	fmt
	build
	test
