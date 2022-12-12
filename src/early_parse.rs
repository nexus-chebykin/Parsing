use std::collections::{HashMap, VecDeque};

use log::trace;

use crate::grammar::{AdvancedGrammar, BasicGrammar, NonTerminal, Rule, Symbol, Terminal};

#[allow(dead_code)]
const EPSILON: Symbol = Symbol::Terminal(Terminal { char: 'ε' });
const CONTAINS: char = '∈';

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct EarleySituation {
    pub rule_index: usize,
    pub dot: usize,
    pub asker: usize,
}

impl EarleySituation {
    pub fn to_string(&self, grammar: &BasicGrammar) -> String {
        let (lhs, mut rhs) = grammar.rules[self.rule_index].to_parts(grammar);
        // Medium sized dot char:
        rhs.insert(self.dot, '•');
        format!("({} -> {}, {})", lhs, rhs, self.asker)
    }
}

pub(crate) fn earley_parse(grammar: AdvancedGrammar, word: &String) -> bool {
    // Add new starting nonterminal
    trace!("New parse, parsing word: {}", word);
    trace!("The rules are:");
    for (i, rule) in grammar.basic_grammar.rules.iter().enumerate() {
        trace!("{}) {}", i, rule.to_string(&grammar.basic_grammar));
    }
    // Not completed - completed
    let mut situations_by_scanned_symbols: Vec<HashMap<Symbol, Vec<EarleySituation>>> =
        vec![HashMap::new(); word.len() + 1];

    let mut situations_to_process = VecDeque::new();
    let mut was = Vec::with_capacity(grammar.basic_grammar.rules.len());
    for i in 0..grammar.basic_grammar.rules.len() {
        was.push(vec![
            vec![-1; word.len() + 1]; grammar.basic_grammar.rules[i].rhs.len() + 1]);
    }
    situations_to_process.push_back(EarleySituation {
        rule_index: grammar.basic_grammar.rules.len() - 1,
        dot: 0,
        asker: 0,
    });

    trace!("Starting the parsing");
    trace!(
        "{} {CONTAINS} D0: Initial situation",
        situations_to_process[0].to_string(&grammar.basic_grammar)
    );

    let mut add_to_processing_queue =
        |situation: EarleySituation, current_layer: i32, queue: &mut VecDeque<EarleySituation>| {
            if was[situation.rule_index][situation.dot][situation.asker] != current_layer {
                was[situation.rule_index][situation.dot][situation.asker] = current_layer;
                queue.push_back(situation);
                return true;
            }
            return false;
        };
    let mut current_layer = 0;
    loop {
        while let Some(situation) = situations_to_process.pop_front() {
            let rule = &grammar.basic_grammar.rules[situation.rule_index];
            if situation.dot == rule.rhs.len() {
                // Situation is complete
                for to_complete in situations_by_scanned_symbols[situation.asker].get(&Symbol::NonTerminal(rule.lhs)).unwrap_or(&vec![]) {
                    let to_complete_rule = &grammar.basic_grammar.rules[to_complete.rule_index];
                    if to_complete.dot < to_complete_rule.rhs.len()
                        && to_complete_rule.rhs[to_complete.dot]
                        == Symbol::NonTerminal(rule.lhs.clone())
                    {
                        let new_situation = EarleySituation {
                            rule_index: to_complete.rule_index,
                            dot: to_complete.dot + 1,
                            asker: to_complete.asker,
                        };
                        let added = add_to_processing_queue(new_situation, current_layer as i32, &mut situations_to_process);
                        if added {
                            trace!(
                                "{} {CONTAINS} D{current_layer}: was completed by {} {CONTAINS} D{current_layer} from D{}",
                                new_situation.to_string(&grammar.basic_grammar),
                                situation.to_string(&grammar.basic_grammar),
                                to_complete.asker
                            );
                        }
                    }
                }
                situations_by_scanned_symbols[current_layer].entry(EPSILON).or_default().push(situation);
            } else {
                // Maybe we can be completed further?
                for completer in situations_by_scanned_symbols[current_layer].get(&EPSILON).unwrap_or(&vec![]) {
                    let completer_rule = &grammar.basic_grammar.rules[completer.rule_index];
                    assert_eq!(completer.dot, completer_rule.rhs.len());
                    if Symbol::NonTerminal(completer_rule.lhs.clone()) == rule.rhs[situation.dot] {
                        let new_situation = EarleySituation {
                            rule_index: situation.rule_index,
                            dot: situation.dot + 1,
                            asker: completer.asker,
                        };
                        let added = add_to_processing_queue(new_situation, current_layer as i32, &mut situations_to_process);
                        if added {
                            trace!(
                                "{} {CONTAINS} D{}: was completed by {} {CONTAINS} D{current_layer} from D{}",
                                new_situation.to_string(&grammar.basic_grammar),
                                current_layer,
                                completer.to_string(&grammar.basic_grammar),
                                current_layer
                            );
                        }
                    }
                }
                match rule.rhs[situation.dot] {
                    Symbol::NonTerminal(NonTerminal { index }) => {
                        for (index, rule) in grammar.rules_by_nonterminal[index].iter().enumerate() {
                            let new_situation = EarleySituation {
                                rule_index: *rule,
                                dot: 0,
                                asker: current_layer,
                            };
                            let added = add_to_processing_queue(new_situation, current_layer as i32, &mut situations_to_process);
                            if added {
                                trace!(
                                    "{} {CONTAINS} D{}: was predicted from {} {CONTAINS} D{current_layer} using rule {}",
                                    new_situation.to_string(&grammar.basic_grammar),
                                    current_layer,
                                    situation.to_string(&grammar.basic_grammar),
                                    index
                                );
                            }
                        }
                    }
                    Symbol::Terminal(_) => {}
                }
                situations_by_scanned_symbols[current_layer].entry(rule.rhs[situation.dot]).or_default().push(situation);
            }
        }
        if current_layer == word.len() {
            break;
        }
        let char_to_scan = word.as_bytes()[current_layer] as char;
        current_layer += 1;
        for situation in situations_by_scanned_symbols[current_layer - 1].get(
            &Symbol::Terminal(Terminal { char: char_to_scan })).unwrap_or(&vec![]) {
            let new_situation = EarleySituation {
                rule_index: situation.rule_index,
                dot: situation.dot + 1,
                asker: situation.asker,
            };
            let added = add_to_processing_queue(
                EarleySituation {
                    rule_index: situation.rule_index,
                    dot: situation.dot + 1,
                    asker: situation.asker,
                },
                current_layer as i32,
                &mut situations_to_process,
            );
            if added {
                trace!(
                    "{} {CONTAINS} D{}: was scanned from {} {CONTAINS} D{}",
                    new_situation.to_string(&grammar.basic_grammar),
                    current_layer,
                    situation.to_string(&grammar.basic_grammar),
                    current_layer - 1
                );
            }
        }
    }
    situations_by_scanned_symbols.last().unwrap().get(&EPSILON).unwrap_or(&vec![]).contains(&EarleySituation {
        rule_index: grammar.basic_grammar.rules.len() - 1,
        dot: 1,
        asker: 0,
    })
}
