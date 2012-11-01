# rust-lang.org
RUST=rustc
#RUST=RUST_LOG=rustc=0,::rt::backtrace rustc

RUST_THREADS=1
RUST_LOG=
RUST_LOG=mmfsm::test
#RUST_LOG=mmparse::basic_syntax,mmparse::preprocessing,rparse::parsers,mmparse::test_preliminaries
#...

check2: mmfsm
	RUST_THREADS=$(RUST_THREADS) RUST_LOG=$(RUST_LOG) ./mmfsm

mmfsm: mmfsm.rs
	$(RUST) mmfsm.rs --test


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
