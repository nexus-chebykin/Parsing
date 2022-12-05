#![allow(non_snake_case)]
#![allow(dead_code)]
#![allow(unused_imports)]

use std::io;
use std::path::Path;

// use log::trace;
use env_logger;

use crate::early_parse::earley_parse;
use crate::grammar::{AdvancedGrammar, BasicGrammar};
use crate::LR_parse::LRParser;

mod LR_parse;
mod early_parse;
mod grammar;

#[cfg(test)]
mod tests {
    use super::*;

    fn setup(path: &Path) -> AdvancedGrammar {
        AdvancedGrammar::from_basic_grammar(BasicGrammar::from_file(path))
    }

    const CBS_INPUTS: [(&str, bool); 12] = [
        ("aabb", true),
        ("abab", true),
        ("abba", false),
        ("aaabbb", true),
        ("aabbb", false),
        ("abbb", false),
        ("ab", true),
        ("a", false),
        ("b", false),
        ("", true),
        ("ababaababb", true),
        ("ababaabab", false),
    ];

    const AEQB_INPUTS: [(&str, bool); 13] = [
        ("aabb", true),
        ("abab", true),
        ("abba", true),
        ("aaabbb", true),
        ("aabbb", false),
        ("bbbaaa", true),
        ("bbaaba", true),
        ("ab", true),
        ("a", false),
        ("b", false),
        ("", true),
        ("ababaababb", true),
        ("abbabbbbaaa", false),
    ];

    const LR0_INPUTS: [(&str, bool); 4] = [("ab", true), ("a", false), ("b", false), ("", false)];

    const LR1_INPUTS: [(&str, bool); 4] = [("b", true), ("bb", true), ("bbb", false), ("", false)];

    const LR2_INPUTS: [(&str, bool); 5] = [
        ("bb", true),
        ("bbb", true),
        ("bbbb", false),
        ("b", false),
        ("", false),
    ];

    fn do_earley_test(grammar: &AdvancedGrammar, inputs: &[(&str, bool)]) {
        for (i, (input, is)) in inputs.iter().enumerate() {
            assert_eq!(
                earley_parse(grammar.clone(), &input.to_string()),
                *is,
                "Test {} failed",
                i
            );
        }
    }

    fn do_LR_test<const K: usize>(parser: &LRParser<K>, inputs: &[(&str, bool)]) {
        for (i, (input, is)) in inputs.iter().enumerate() {
            assert_eq!(parser.parse(input).is_ok(), *is, "Test {} failed", i);
        }
    }

    #[test]
    fn test_earley_cbs_1() {
        let grammar = setup(Path::new("test_grammars/correct_bracket_sequence/1.bnf"));
        do_earley_test(&grammar, &CBS_INPUTS);
    }

    #[test]
    fn test_earley_cbs_2() {
        let grammar = setup(Path::new("test_grammars/correct_bracket_sequence/2.bnf"));
        do_earley_test(&grammar, &CBS_INPUTS);
    }

    #[test]
    fn test_earley_cbs_3() {
        let grammar = setup(Path::new("test_grammars/correct_bracket_sequence/3.bnf"));
        do_earley_test(&grammar, &CBS_INPUTS);
    }
    #[test]
    fn test_earley_aeqb_1() {
        let grammar = setup(Path::new("test_grammars/a_eq_b.bnf"));
        do_earley_test(&grammar, &AEQB_INPUTS);
    }

    #[test]
    fn test_lr_cbs_1() {
        let grammar = setup(Path::new("test_grammars/correct_bracket_sequence/1.bnf"));
        assert!(LRParser::<1>::try_from_advanced_grammar(grammar).is_err());
    }

    #[test]
    fn test_lr_cbs_2() {
        let grammar = setup(Path::new("test_grammars/correct_bracket_sequence/2.bnf"));
        let LR = LRParser::<1>::try_from_advanced_grammar(grammar);
        assert!(LR.is_ok());
        do_LR_test(&LR.unwrap(), &CBS_INPUTS);
    }

    #[test]
    fn test_lr_cbs_3() {
        let grammar = setup(Path::new("test_grammars/correct_bracket_sequence/3.bnf"));
        let LR = LRParser::<1>::try_from_advanced_grammar(grammar);
        assert!(LR.is_ok());
        do_LR_test(&LR.unwrap(), &CBS_INPUTS);
    }

    #[test]
    fn test_lr_aeqb_1() {
        let grammar = setup(Path::new("test_grammars/a_eq_b.bnf"));
        assert!(LRParser::<1>::try_from_advanced_grammar(grammar).is_err());
    }
    #[test]
    fn test_lr_0_lr_0() {
        let grammar = setup(Path::new("test_grammars/LR0.bnf"));
        let LR = LRParser::<1>::try_from_advanced_grammar(grammar);
        assert!(LR.is_ok());
        do_LR_test(&LR.unwrap(), &LR0_INPUTS);
    }

    #[test]
    fn test_lr_0_lr_1() {
        let grammar = setup(Path::new("test_grammars/LR1_min.bnf"));
        let LR = LRParser::<0>::try_from_advanced_grammar(grammar);
        assert!(LR.is_err());
    }

    #[test]
    fn test_lr_1_lr_1() {
        let grammar = setup(Path::new("test_grammars/LR1_min.bnf"));
        let LR = LRParser::<1>::try_from_advanced_grammar(grammar);
        assert!(LR.is_ok());
        do_LR_test(&LR.unwrap(), &LR1_INPUTS);
    }

    #[test]
    fn test_lr_1_lr_2() {
        let grammar = setup(Path::new("test_grammars/LR2_min.bnf"));
        let LR = LRParser::<1>::try_from_advanced_grammar(grammar);
        assert!(LR.is_err());
    }

    #[test]
    fn test_lr_2_lr_2() {
        let grammar = setup(Path::new("test_grammars/LR2_min.bnf"));
        let LR = LRParser::<2>::try_from_advanced_grammar(grammar);
        assert!(LR.is_ok());
        do_LR_test(&LR.unwrap(), &LR2_INPUTS);
    }
}

fn main() {
    // To log earley parsing process, uncomment the following line
    // env_logger::Builder::from_env(Env::default().default_filter_or("trace")).format(|buf, record| {
    //     writeln!(
    //         buf, "{}", record.args()
    //     )
    // }).init();

    let grammar = AdvancedGrammar::from_basic_grammar(BasicGrammar::from_file(Path::new(
        "test_grammars/LR1_min.bnf",
    )));
    let LR = LRParser::<1>::try_from_advanced_grammar(grammar.clone()).unwrap();
    let word = "aabb".to_string();
    let result_earley = earley_parse(grammar, &word);
    let result_lr = LR.parse(&word);
    assert_eq!(result_earley, result_lr.is_ok());
}
