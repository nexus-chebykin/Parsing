use crate::grammar::{AdvancedGrammar, NonTerminal, Rule, Symbol, Terminal, BasicGrammar};
use std::collections::VecDeque;
use log::trace;

#[allow(dead_code)]
const EPSILON: char = 'ε';
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
    #[allow(non_snake_case)]
    // Not completed - completed
    let mut D: Vec<(Vec<EarleySituation>, Vec<EarleySituation>)> =
        vec![(Vec::new(), Vec::new()); word.len() + 1];

    let mut q = VecDeque::new();
    let mut was = Vec::with_capacity(grammar.basic_grammar.rules.len());
    for i in 0..grammar.basic_grammar.rules.len() {
        was.push(vec![
            vec![-1; word.len() + 1];
            grammar.basic_grammar.rules[i].rhs.len() + 1
        ]);
    }
    q.push_back(EarleySituation {
        rule_index: grammar.basic_grammar.rules.len() - 1,
        dot: 0,
        asker: 0,
    });

    trace!("Starting the parsing");
    trace!(
        "{} {CONTAINS} D0: Initial situation",
        q[0].to_string(&grammar.basic_grammar)
    );

    let mut add_to_q =
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
        while let Some(situation) = q.pop_front() {
            let rule = &grammar.basic_grammar.rules[situation.rule_index];
            if situation.dot == rule.rhs.len() {
                // Situation is complete
                for to_complete in &D[situation.asker].0 {
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
                        let added = add_to_q(new_situation, current_layer as i32, &mut q);
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
                D[current_layer].1.push(situation);
            } else {
                // Maybe we can be completed further?
                for completer in &D[current_layer].1 {
                    let completer_rule = &grammar.basic_grammar.rules[completer.rule_index];
                    if completer.dot == completer_rule.rhs.len()
                        && Symbol::NonTerminal(completer_rule.lhs.clone())
                            == rule.rhs[situation.dot]
                    {
                        let new_situation = EarleySituation {
                            rule_index: situation.rule_index,
                            dot: situation.dot + 1,
                            asker: completer.asker,
                        };
                        let added = add_to_q(new_situation, current_layer as i32, &mut q);
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
                            let added = add_to_q(new_situation, current_layer as i32, &mut q);
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
                D[current_layer].0.push(situation);
            }
        }
        if current_layer == word.len() {
            break;
        }
        current_layer += 1;
        for situation in &D[current_layer - 1].0 {
            let rule = &grammar.basic_grammar.rules[situation.rule_index];
            if situation.dot < rule.rhs.len() {
                match rule.rhs[situation.dot] {
                    Symbol::NonTerminal(_) => {}
                    Symbol::Terminal(Terminal { char: c }) => {
                        if c == word.as_bytes()[current_layer - 1] as char {
                            let new_situation = EarleySituation {
                                rule_index: situation.rule_index,
                                dot: situation.dot + 1,
                                asker: situation.asker,
                            };
                            let added = add_to_q(
                                EarleySituation {
                                    rule_index: situation.rule_index,
                                    dot: situation.dot + 1,
                                    asker: situation.asker,
                                },
                                current_layer as i32,
                                &mut q,
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
                }
            }
        }
    }
    D.last().unwrap().1.contains(&EarleySituation {
        rule_index: grammar.basic_grammar.rules.len() - 1,
        dot: 1,
        asker: 0,
    })
}
