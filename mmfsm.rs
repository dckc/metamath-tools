
extern mod std;
use std::rope;

fn each_statement(text: rope::Rope) -> bool {
    for rope::loop_chars(text) |ch| {
        debug!("hi: %c", ch);
    }
}

#[cfg(test)]
mod test {
    #[test]
    fn each_statement1() {
        let doc = rope::of_str(@~"axiom.1 $a |- x = x $.");
        each_statement(doc);
    }
}
