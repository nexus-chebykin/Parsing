use std::cell::Cell;
use std::cmp::min;
use std::collections::hash_map::Entry;
use std::collections::{HashMap, HashSet, VecDeque};
use std::fs::File;
use std::io::Read;
use std::path::{Iter, Path};
use std::vec;

const EPSILON_SYMBOL: Terminal = Terminal { char: 'ε' };
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct Terminal {
    pub char: char,
}

#[derive(Clone, Debug, Eq, PartialEq, Copy, Hash)]
pub struct NonTerminal {
    pub index: usize,
}

#[derive(Debug, Clone, Eq, PartialEq, Copy, Hash)]
pub enum Symbol {
    Terminal(Terminal),
    NonTerminal(NonTerminal),
}

impl Symbol {
    pub fn to_string(&self, grammar: &BasicGrammar) -> String {
        match self {
            Symbol::Terminal(t) => t.char.to_string(),
            Symbol::NonTerminal(nt) => {
                grammar.nonterminals_mapping.index_to_human_name[nt.index].clone()
            }
        }
    }
    pub fn is_nonterminal(&self) -> bool {
        match self {
            Symbol::Terminal(_) => false,
            Symbol::NonTerminal(_) => true,
        }
    }
}
#[derive(Debug, Clone)]
pub struct Rule {
    pub lhs: NonTerminal,
    pub rhs: Vec<Symbol>,
}
impl Rule {
    pub fn to_parts(&self, grammar: &BasicGrammar) -> (String, String) {
        let lhs = grammar.nonterminals_mapping.index_to_human_name[self.lhs.index].clone();
        let rhs = self
            .rhs
            .iter()
            .map(|symbol| match symbol {
                Symbol::Terminal(terminal) => terminal.char.to_string(),
                Symbol::NonTerminal(nonterminal) => {
                    grammar.nonterminals_mapping.index_to_human_name[nonterminal.index].clone()
                }
            })
            .collect::<Vec<String>>()
            .join("");
        (lhs, rhs)
    }
    pub fn to_string(&self, grammar: &BasicGrammar) -> String {
        let (lhs, mut rhs) = self.to_parts(grammar);
        if rhs.is_empty() {
            rhs = "ε".to_string();
        }
        format!("{} -> {}", lhs, rhs)
    }
}
#[derive(Debug, Clone, Default)]
pub struct NonterminalsMapping {
    pub human_name_to_index: HashMap<String, usize>,
    pub index_to_human_name: Vec<String>,
}

impl NonterminalsMapping {
    pub fn insert_nonterminal(&mut self, human_name: String) -> NonTerminal {
        let index = match self.human_name_to_index.get(&human_name) {
            Some(index) => *index,
            None => {
                let index = self.human_name_to_index.len();
                self.human_name_to_index.insert(human_name.clone(), index);
                self.index_to_human_name.push(human_name);
                index
            }
        };
        NonTerminal { index }
    }
}

#[derive(Debug, Default, Clone)]
pub struct AdvancedGrammar {
    pub basic_grammar: BasicGrammar,
    pub rules_by_nonterminal: Vec<Vec<usize>>,
}

#[derive(Debug, Default, Clone)]
pub struct BasicGrammar {
    pub nonterminals_mapping: NonterminalsMapping,
    pub rules: Vec<Rule>,
}

impl BasicGrammar {
    pub fn from_string(contents: &str) -> BasicGrammar {
        let mut result = BasicGrammar::default();
        for mut line in contents.lines() {
            line = line.trim();
            if line.starts_with("//") || line.is_empty() {
                continue;
            }
            let parts = line.split_once("::=").unwrap();
            let lhs = parts
                .0
                .trim()
                .strip_prefix('<')
                .unwrap()
                .strip_suffix('>')
                .unwrap()
                .to_string();

            let mut possible_rhs = Vec::new();
            let lhs = result.nonterminals_mapping.insert_nonterminal(lhs);
            for mut part in parts.1.split("|") {
                possible_rhs.push(Vec::<Symbol>::new());
                part = part.trim();
                if part == "ε" {
                    continue;
                } else {
                    let mut current_nonterminal = String::new();
                    let mut started = false;
                    for c in part.chars() {
                        if started {
                            if c == '>' {
                                possible_rhs.last_mut().unwrap().push(
                                    if current_nonterminal == "SPACE" {
                                        Symbol::Terminal(Terminal { char: ' ' })
                                    } else {
                                        Symbol::NonTerminal(
                                            result
                                                .nonterminals_mapping
                                                .insert_nonterminal(current_nonterminal),
                                        )
                                    },
                                );
                                current_nonterminal = String::new();
                                started = false;
                            } else {
                                current_nonterminal.push(c);
                            }
                        } else {
                            if c == '<' {
                                started = true;
                            } else {
                                possible_rhs
                                    .last_mut()
                                    .unwrap()
                                    .push(Symbol::Terminal(Terminal { char: c }));
                            }
                        }
                    }
                }
            }
            for rhs in possible_rhs {
                result.rules.push(Rule {
                    lhs: lhs.clone(),
                    rhs,
                });
            }
        }
        result
    }
    pub fn from_file(filename: &Path) -> BasicGrammar {
        let mut file = File::open(filename).unwrap();
        let mut contents = String::new();
        file.read_to_string(&mut contents).unwrap();
        Self::from_string(&contents)
    }
    pub fn add_fake_rule(&mut self) {
        let starting_nonterminal = self
            .nonterminals_mapping
            .insert_nonterminal("S'".to_string());
        self.rules.push(Rule {
            lhs: starting_nonterminal,
            rhs: vec![Symbol::NonTerminal(NonTerminal { index: 0 })],
        });
    }
}

impl AdvancedGrammar {
    // After this, the last rule in grammar will be S' -> S
    pub fn from_basic_grammar(mut basic_grammar: BasicGrammar) -> AdvancedGrammar {
        basic_grammar.add_fake_rule();
        let mut result = AdvancedGrammar {
            basic_grammar,
            rules_by_nonterminal: Vec::new(),
        };
        result
            .basic_grammar
            .rules
            .sort_unstable_by(|a, b| a.lhs.index.cmp(&b.lhs.index));
        result.rules_by_nonterminal.resize(
            result
                .basic_grammar
                .nonterminals_mapping
                .human_name_to_index
                .len(),
            Vec::new(),
        );
        for (index, rule) in result.basic_grammar.rules.iter().enumerate() {
            result.rules_by_nonterminal[rule.lhs.index].push(index);
        }
        result
    }
}
