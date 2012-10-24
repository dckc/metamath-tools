# rust-lang.org
RUST=rustc

check: mmparse
	./mmparse

mmparse: mmparse.rs
	$(RUST) mmparse.rs --test
