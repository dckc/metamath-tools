extern mod std;

extern mod rparse;
use rparse::*;  //{StringParsers};

extern mod mmparse;
use mmparse::basic_syntax::{Statement,KeywordStatement};

fn main() {
    let argv = os::args();

    let usage = ~"Usage: mmutil -c|-s input_filename";
    if (argv.len() != 3) {
        fail usage;
    }

    enum Production { Comments, Statements };
    let production_flags = ~[(Comments, "-c"), (Statements, "-s")];

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
          Comments => do_comments(@input_fn.to_str(), txt),
          Statements => do_kw(@input_fn.to_str(), txt)
        }
      }
      Err(msg) => {
        fail msg;
      }
    }
}

fn do_kw(input_fn: @~str, txt: &str) {
    let actual = mmparse::basic_syntax::statements()
        .parse(input_fn, txt);
    io::println(fmt!("%?", actual));

    match actual {
      Ok(sts) => show_doc(sts),
      Err(x) => fail fmt!("%?", x)
    }
}

fn show_doc(sts: @~[@~Statement]) {
    for sts.each() |st| {
        match ***st {
          KeywordStatement(Some(doc), p) => {
            match p.label {
              Some(l) => io::println(fmt!("%s: %s", *l, *doc)),
              _ => ()
            }
          }
          _ => ()
        }
    }
}

fn do_comments(input_fn: @~str, txt: &str) {
    let actual = mmparse::preliminaries::comments()
        .parse(input_fn, txt);
    io::println(fmt!("%?", actual))
}
        
