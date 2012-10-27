# rust-lang.org
RUST=rustc

check: mmparse
	./mmparse
	#RUST_THREADS=1 RUST_LOG=mmparse::test_basic_syntax ./mmparse
	#RUST_THREADS=1 RUST_LOG=mmparse::test_basic_syntax,mmparse::preprocessing,mmparse::test_preliminaries ./mmparse

mmutil: lib mmutil.rs
	$(RUST) -L . mmutil.rs

# TODO: build libmmparse-*.so only when necessary
lib: mmparse.rs
	$(RUST) --lib mmparse.rs

mmparse: mmparse.rs
	$(RUST) mmparse.rs --test


clean:
	$(RM) -f libmmparse.*.so mmparse mmutil
