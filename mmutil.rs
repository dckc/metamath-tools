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

    let r = do mmfsm::each_statement(txt) |st| {
        io::println("thunk!");
        show_doc1(st)
    };
    match r {
      Ok(()) => (),
      Err(x) => fail x
    }
}


fn show_doc1(st: &mmfsm::Statement) {
    match *st {
      mmfsm::Axiom(mmfsm::Label(label), _) => {
        io::println(fmt!("%s: ...", label))
      }
      _ => ()
    }
}
        
