extern mod std;

extern mod rparse;
use rparse::*;  //{StringParsers};

extern mod mmparse;

fn main() {
    let argv = os::args();

    let usage = ~"Usage: mmutil -c|-k input_filename";
    if (argv.len() != 3) {
        fail usage;
    }

    enum Production { Comments, KeywordStatement };
    let production_flags = ~[(Comments, "-c"), (KeywordStatement, "-k")];

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
          Comments => {
            let actual = mmparse::preliminaries::comments()
                .parse(@input_fn.to_str(), txt);
            io::println(fmt!("%?", actual))
          }
        
          KeywordStatement => {
            let actual = mmparse::basic_syntax::keyword_statement()
                .parse(@input_fn.to_str(), txt);
            io::println(fmt!("%?", actual))
          }
        }
      }

      Err(msg) => {
        fail msg;
      }
    }
}
