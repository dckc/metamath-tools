# rust-lang.org
RUST=rustc

RUST_THREADS=1
RUST_LOG=
#RUST_LOG=mmparse::basic_syntax,mmparse::test_basic_syntax
#RUST_LOG=mmparse::basic_syntax,mmparse::preprocessing,rparse::parsers,mmparse::test_preliminaries
#...

check: mmparse
	RUST_THREADS=$(RUST_THREADS) RUST_LOG=$(RUST_LOG) ./mmparse

mmutil: lib mmutil.rs
	$(RUST) -L . mmutil.rs

# TODO: build libmmparse-*.so only when necessary
lib: mmparse.rs
	$(RUST) --lib mmparse.rs

mmparse: mmparse.rs
	$(RUST) mmparse.rs --test


clean:
	$(RM) -f libmmparse.*.so mmparse mmutil
