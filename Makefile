check: build test

build:
	RUSTFLAGS='-D warnings' cargo build --verbose

test:
	sudo env "PATH=${PATH}" RUSTFLAGS='-D warnings' RUST_BACKTRACE=1 cargo test --verbose

.PHONY:
	build
	check
	test
