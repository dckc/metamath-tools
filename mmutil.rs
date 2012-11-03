extern mod std;
use mod std::rope;

extern mod mmfsm;

fn main() {
    let argv = os::args();

    let usage = ~"Usage: mmutil -c|-s input_filename";
    if (argv.len() != 3) {
        fail usage;
    }

    enum Production { Statements };
    let production_flags = ~[(Statements, "-s")];

    let production = match (
        production_flags.find(
            |pf| match pf { (_, f) => argv[1] == f.to_unique() })) {
      Some((p, _)) => p,
      None => fail usage
    };

    let input_fn = Path(argv[2]);

    match io::read_whole_file_str(&input_fn) {
      Ok(txt) => {
        match production {
          Statements => do_statements(@input_fn.to_str(), txt),
        }
      }
      Err(msg) => {
        fail msg;
      }
    }
}

fn do_statements(_input_fn: @~str, txt: &str) {
    debug!("%s: %u bytes", *_input_fn, txt.len());

    let r = do mmfsm::each_statement(txt) |label, kw, expr, pf| {
        io::println("thunk!");
        show_doc1(label, kw, expr, pf)
    };
    match r {
      Ok(()) => (),
      Err(x) => fail x
    }
}


fn show_doc1(label: &Option<mmfsm::Label>,
             kw: mmfsm::Keyword,
             _expr: &[mmfsm::Symbol],
             _pf: &Option<mmfsm::Proof>) {
    match *label {
      Some(l) => io::println(fmt!("%s %? ...", l, kw)),
      _ => ()
    }
}
