# rust-lang.org
RUST=rustc

check: mmparse
	./mmparse

mmutil: lib mmutil.rs
	$(RUST) -L . mmutil.rs

#RUST_THREADS=1 RUST_LOG=mmparse::test_basic_syntax ./mmparse
#RUST_THREADS=1 RUST_LOG=mmparse::preprocessing,mmparse::test_preliminaries ./mmparse

# TODO: build libmmparse-*.so only when necessary
lib: mmparse.rs
	$(RUST) --lib mmparse.rs

mmparse: mmparse.rs
	$(RUST) mmparse.rs --test
