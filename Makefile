# rust-lang.org
RUST=rustc

check: mmparse
	./mmparse

#RUST_THREADS=1 RUST_LOG=mmparse::test_basic_syntax ./mmparse
#RUST_THREADS=1 RUST_LOG=mmparse::preprocessing,mmparse::test_preliminaries ./mmparse

mmparse: mmparse.rs
	$(RUST) mmparse.rs --test
