use crate::grammar::{AdvancedGrammar, NonTerminal, Rule, Symbol, Terminal};
use std::cmp::min;
use std::collections::hash_map::Entry;
use std::collections::{HashMap, HashSet, VecDeque};

type AheadString<const K: usize> = [Terminal; K];

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct LRSituation<const K: usize> {
    pub rule_index: usize,
    pub dot: usize,
    pub ahead: AheadString<K>,
}
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum Action {
    Shift,
    Reduce(usize),
    Accept,
}
impl<const K: usize> LRSituation<K> {
    pub fn get_first(&self, parser: &LRParser<K>) -> HashSet<AheadString<K>> {
        parser.get_first(
            [
                &parser.grammar.basic_grammar.rules[self.rule_index].rhs[self.dot + 1..],
                &self.ahead.map(|t| Symbol::Terminal(t)),
            ]
            .concat(),
        )
    }
    pub fn is_finished(&self, grammar: &AdvancedGrammar) -> bool {
        self.dot == grammar.basic_grammar.rules[self.rule_index].rhs.len()
    }
    pub fn get_next_symbol(&self, grammar: &AdvancedGrammar) -> Option<Symbol> {
        grammar.basic_grammar.rules[self.rule_index]
            .rhs
            .get(self.dot)
            .cloned()
    }

    pub fn get_symbols_after_dot(&self, grammar: &AdvancedGrammar) -> Vec<Symbol> {
        if self.dot >= grammar.basic_grammar.rules[self.rule_index].rhs.len()
            || grammar.basic_grammar.rules[self.rule_index].rhs[self.dot].is_nonterminal()
        {
            return vec![];
        }
        let mut res = grammar.basic_grammar.rules[self.rule_index].rhs[self.dot..].to_vec();
        res.extend(&self.ahead.map(|t| Symbol::Terminal(t)));
        res
    }
    pub fn get_next_situation(&self) -> Self {
        LRSituation {
            rule_index: self.rule_index,
            dot: self.dot + 1,
            ahead: self.ahead,
        }
    }
    pub fn ahead_to_string(&self) -> String {
        self.ahead
            .iter()
            .map(|t| t.char.to_string())
            .collect::<String>()
    }
    pub fn to_string(&self, grammar: &AdvancedGrammar) -> String {
        let (lhs, mut rhs) =
            grammar.basic_grammar.rules[self.rule_index].to_parts(&grammar.basic_grammar);
        // Medium sized dot char:
        rhs.insert(self.dot, '•');
        format!("{} -> {}, {}", lhs, rhs, self.ahead_to_string())
    }
}
#[derive(Debug)]
pub struct LRParser<const K: usize> {
    pub grammar: AdvancedGrammar,
    pub automaton: Vec<HashMap<Symbol, usize>>,
    pub action: Vec<HashMap<AheadString<K>, Action>>,
    pub first: Vec<HashSet<AheadString<K>>>,
    pub index_to_situation: Vec<LRSituation<K>>,
    pub situation_to_index: HashMap<LRSituation<K>, usize>,
    pub situations_subset_to_index: HashMap<Vec<usize>, usize>,
    pub index_to_situations_subset: Vec<Vec<usize>>,
}

impl<const K: usize> LRParser<K> {
    const EOF_SYMBOL: Terminal = Terminal { char: '¥' };
    const EOF_AHEAD: AheadString<K> = [Self::EOF_SYMBOL; K];

    fn recursive_build_first(
        // It would be nice to have both recursive functions merged together, but idc
        rule: &Rule,
        position_in_rule: usize,
        position_in_fill: usize,
        to_fill: &mut AheadString<K>,
        current_first: &[HashSet<AheadString<K>>],
        new_first: &mut [HashSet<AheadString<K>>],
    ) -> bool {
        if position_in_rule == rule.rhs.len() || position_in_fill == K {
            return new_first[rule.lhs.index].insert(*to_fill);
        }
        let mut result = false;
        match rule.rhs[position_in_rule] {
            Symbol::Terminal(terminal) => {
                to_fill[position_in_fill] = terminal;
                result |= Self::recursive_build_first(
                    rule,
                    position_in_rule + 1,
                    position_in_fill + 1,
                    to_fill,
                    current_first,
                    new_first,
                );
                to_fill[position_in_fill] = Self::EOF_SYMBOL;
            }
            Symbol::NonTerminal(nonterminal) => {
                for possible_ahead in current_first[nonterminal.index].iter() {
                    let mut new_position_in_fill = position_in_fill;
                    for (i, c) in possible_ahead.iter().enumerate() {
                        if *c == Self::EOF_SYMBOL || position_in_fill + i >= K {
                            new_position_in_fill = position_in_fill + i;
                            break;
                        }
                        to_fill[position_in_fill + i] = *c;
                    }
                    result |= Self::recursive_build_first(
                        rule,
                        position_in_rule + 1,
                        new_position_in_fill,
                        to_fill,
                        current_first,
                        new_first,
                    );
                    for i in position_in_fill..new_position_in_fill {
                        to_fill[i] = Self::EOF_SYMBOL;
                    }
                }
            }
        }
        result
    }

    pub fn recursive_get_first(
        alpha: &[Symbol],
        position_in_fill: usize,
        to_fill: &mut AheadString<K>,
        first: &[HashSet<AheadString<K>>],
    ) -> HashSet<AheadString<K>> {
        let mut result = HashSet::new();
        if alpha.is_empty() || position_in_fill == K {
            result.insert(*to_fill);
            return result;
        }
        match alpha[0] {
            Symbol::Terminal(terminal) => {
                to_fill[position_in_fill] = terminal;
                result.extend(Self::recursive_get_first(
                    &alpha[1..],
                    position_in_fill + 1,
                    to_fill,
                    first,
                ));
                to_fill[position_in_fill] = Self::EOF_SYMBOL;
            }
            Symbol::NonTerminal(nonterminal) => {
                for possible_ahead in first[nonterminal.index].iter() {
                    let mut new_position_in_fill = position_in_fill;
                    for (i, c) in possible_ahead.iter().enumerate() {
                        if *c == Self::EOF_SYMBOL || position_in_fill + i >= K {
                            new_position_in_fill = position_in_fill + i;
                            break;
                        }
                        to_fill[position_in_fill + i] = *c;
                    }
                    result.extend(Self::recursive_get_first(
                        &alpha[1..],
                        new_position_in_fill,
                        to_fill,
                        first,
                    ));
                    for i in position_in_fill..new_position_in_fill {
                        to_fill[i] = Self::EOF_SYMBOL;
                    }
                }
            }
        }
        result
    }

    pub fn build_first(grammar: &AdvancedGrammar) -> Vec<HashSet<AheadString<K>>> {
        let mut first = vec![
            HashSet::<AheadString<K>>::new();
            grammar
                .basic_grammar
                .nonterminals_mapping
                .name_to_index
                .len()
        ];
        let mut changed = true;

        while changed {
            changed = false;
            let mut new_first = first.clone();
            for rule in grammar.basic_grammar.rules.iter() {
                let mut to_fill = [Self::EOF_SYMBOL; K];
                changed |=
                    Self::recursive_build_first(rule, 0, 0, &mut to_fill, &first, &mut new_first);
            }
            first = new_first;
        }
        first
    }
    pub fn get_first(&self, string: Vec<Symbol>) -> HashSet<AheadString<K>> {
        Self::recursive_get_first(&string,  0, &mut [Self::EOF_SYMBOL; K], &self.first)
    }

    fn add_situation(&mut self, situation: LRSituation<K>) -> (usize, bool) {
        if let Some(index) = self.situation_to_index.get(&situation) {
            return (*index, false);
        }
        let index = self.situation_to_index.len();
        self.situation_to_index.insert(situation.clone(), index);
        self.index_to_situation.push(situation);
        self.automaton.push(HashMap::new());
        self.action.push(HashMap::new());
        (index, true)
    }
    // Should pass sorted situations
    fn add_situations_subset(&mut self, situations_subset: Vec<usize>) -> (usize, bool) {
        if let Some(index) = self.situations_subset_to_index.get(&situations_subset) {
            return (*index, false);
        }
        let index = self.situations_subset_to_index.len();
        self.situations_subset_to_index
            .insert(situations_subset.clone(), index);
        self.index_to_situations_subset.push(situations_subset);
        self.automaton.push(HashMap::new());
        (index, true)
    }

    //Returns sorted vector of situations
    fn closure(&mut self, situations_subset: Vec<usize>) -> Vec<usize> {
        // A queue of situation indices to expand
        let mut queue = VecDeque::new();
        let mut new_situations: HashSet<usize> = situations_subset.iter().cloned().collect();
        queue.extend(situations_subset);
        while !queue.is_empty() {
            let situation_index = queue.pop_front().unwrap();
            let situation = &self.index_to_situation[situation_index];
            if situation.is_finished(&self.grammar) {
                continue;
            }
            match self.grammar.basic_grammar.rules[situation.rule_index].rhs[situation.dot] {
                Symbol::Terminal(_) => {}
                Symbol::NonTerminal(NonTerminal {
                    index: nonterminal_index,
                }) => {
                    let new_first = situation.get_first(self);
                    for i in 0..self.grammar.rules_by_nonterminal[nonterminal_index].len() {
                        let rule_index = self.grammar.rules_by_nonterminal[nonterminal_index][i];
                        for possible_ahead in new_first.iter().cloned() {
                            let new_situation = LRSituation {
                                rule_index,
                                dot: 0,
                                ahead: possible_ahead,
                            };
                            let (new_situation_index, _) = self.add_situation(new_situation);
                            let inserted = new_situations.insert(new_situation_index);
                            if inserted {
                                queue.push_back(new_situation_index);
                            }
                        }
                    }
                }
            }
        }
        let mut result: Vec<usize> = new_situations.into_iter().collect();
        result.sort_unstable();
        result
    }

    fn update_action(
        &mut self,
        vertex: usize,
        ahead: &AheadString<K>,
        new_action: Action,
    ) -> Result<(), String> {
        let new_is_shift = match new_action {
            Action::Shift => true,
            _ => false,
        };
        // let ptr = self as *const Self;
        match self.action[vertex].entry(*ahead) {
            Entry::Occupied(entry) => match entry.get() {
                Action::Reduce(_) => {
                    // unsafe {
                        // println!("{}", (*ptr).to_string());
                    // }
                    return Err(format!(
                        "{:?} / {:?} conflict at vertex {vertex} with ahead {ahead:#?}",
                        new_action,
                        entry.get()
                    ));
                }
                _ => {
                    if !new_is_shift {
                        // unsafe {
                            // println!("{}", (*ptr).to_string());
                        // }
                        return Err(format!(
                            "{:?} / {:?} conflict at vertex {vertex} with ahead {ahead:#?}",
                            new_action,
                            entry.get()
                        ));
                    }
                }
            },
            Entry::Vacant(entry) => {
                entry.insert(new_action);
            }
        }
        Ok(())
    }

    fn situations_subset_to_string(&self, situations_subset: &[usize]) -> String {
        let mut result = String::from("{\n");
        for situation_index in situations_subset {
            let situation = &self.index_to_situation[*situation_index];
            result += &format!("{}\n", situation.to_string(&self.grammar))
        }
        result += "}";
        result
    }
    pub fn try_from_advanced_grammar(grammar: AdvancedGrammar) -> Result<Self, String> {
        let first = Self::build_first(&grammar);
        let mut result = LRParser {
            grammar,
            automaton: Vec::new(),
            action: Vec::new(),
            first,
            index_to_situation: Vec::new(),
            situation_to_index: HashMap::new(),
            situations_subset_to_index: HashMap::new(),
            index_to_situations_subset: Vec::new(),
        };
        let mut situations_subset_to_process: VecDeque<usize> = VecDeque::new();

        let starting_situation = LRSituation {
            rule_index: result.grammar.basic_grammar.rules.len() - 1,
            dot: 0,
            ahead: Self::EOF_AHEAD.clone(),
        };
        let starting_situations_subset: Vec<usize> =
            vec![(result.add_situation(starting_situation).0).clone()];
        let starting_situations_subset = result.closure(starting_situations_subset);
        situations_subset_to_process.push_back(result.add_situations_subset(starting_situations_subset).0);
        
        while !situations_subset_to_process.is_empty() {
            let current_subset_index = situations_subset_to_process.pop_front().unwrap();
            result.process_subset(current_subset_index, &mut situations_subset_to_process)?;
        }
        Ok(result)
    }
    fn process_subset(&mut self, current_subset_index: usize, situations_subset_to_process: &mut VecDeque<usize>) -> Result<(), String> {
        let current_subset_ptr =
            &self.index_to_situations_subset[current_subset_index] as *const Vec<usize>;
        let current_subset = unsafe { &*current_subset_ptr };
        // I dont really like it, but do not know any better solution
        let mut possible_moves = HashMap::new();
        for situation in current_subset.iter().cloned() {
            let current_situation = &self.index_to_situation[situation].clone();
            let finished = current_situation.is_finished(&self.grammar);
            if finished {
                if current_situation.rule_index == self.grammar.basic_grammar.rules.len() - 1
                {
                    self.update_action(
                        current_subset_index,
                        &current_situation.ahead,
                        Action::Accept,
                    )?;
                } else {
                    self.update_action(
                        current_subset_index,
                        &current_situation.ahead,
                        Action::Reduce(current_situation.rule_index),
                    )?;
                }
                continue;
            }
            let next_symbol = current_situation.get_next_symbol(&self.grammar).unwrap();
            possible_moves
                .entry(next_symbol)
                .or_insert_with(Vec::new)
                .push(
                    self
                        .add_situation(current_situation.get_next_situation())
                        .0,
                );
            let beta2_v = current_situation.get_symbols_after_dot(&self.grammar);
            if beta2_v.is_empty() {
                continue;
            }
            let first_beta2_v = self.get_first(beta2_v);
            for possible_ahead in first_beta2_v {
                self.update_action(current_subset_index, &possible_ahead, Action::Shift)?;
            }
        }
        for (symbol, next_situations) in possible_moves {
            let next_situations = self.closure(next_situations);
            let (next_subset_index, inserted) = self.add_situations_subset(next_situations);
            if inserted {
                situations_subset_to_process.push_back(next_subset_index);
            }
            self.automaton[current_subset_index].insert(symbol, next_subset_index);
        }
        Ok(())
    }
    pub fn to_string(&self) -> String {
        let mut result = String::new();
        result += "First:\n";
        for (nonterminal_index, starts) in self.first.iter().enumerate() {
            result += &format!(
                "{}: {:?}\n",
                self.grammar
                    .basic_grammar
                    .nonterminals_mapping
                    .index_to_name[nonterminal_index],
                starts
            );
        }
        result += "Automaton:\n";
        for i in 0..self.index_to_situations_subset.len() {
            result += &format!(
                "{i}: {}\n",
                self.situations_subset_to_string(&self.index_to_situations_subset[i])
            )
        }
        result += "\n";
        for i in 0..self.automaton.len() {
            for (symbol, next_subset_index) in self.automaton[i].iter() {
                result += &format!(
                    "{} -{}-> {}\n",
                    i,
                    symbol.to_string(&self.grammar.basic_grammar),
                    next_subset_index
                );
            }
        }
        result += "\n";
        for i in 0..self.action.len() {
            for (ahead, action) in self.action[i].iter() {
                result += &format!("{}, {:#?} -> {:?}\n", i, ahead, action);
            }
        }
        result += "\n";
        result
    }
    pub fn parse(&self, input: &str) -> Result<(), String> {
        // println!("{:#?}", &self);
        let mut stack = Vec::new();
        let input: Vec<Terminal> = input.chars().map(|c| Terminal { char: c }).collect();
        let mut input_index = 0;
        stack.push(0);
        while !stack.is_empty() {
            // let current_ahead: AheadString =
            //     input.get(input_index).unwrap_or(&Self::EOF_SYMBOL).clone();
            let current_ahead: AheadString<K> = input
                [input_index..min(input_index + K, input.len())]
                .iter()
                .cloned()
                .chain(std::iter::repeat(&Self::EOF_SYMBOL).cloned())
                .take(K)
                .collect::<Vec<_>>()
                .try_into()
                .unwrap();
            let current_state = *stack.last().unwrap();
            match self.action[current_state].get(&current_ahead) {
                Some(action) => match action {
                    Action::Shift => {
                        let new_state =
                            self.automaton[current_state].get(&Symbol::Terminal(current_ahead[0]));
                        match new_state {
                            Some(new_state) => {
                                stack.push(*new_state);
                            }
                            None => {
                                return Err(format!("Unexpected symbol {:?}", current_ahead));
                            }
                        }
                        input_index += 1;
                    }
                    Action::Reduce(rule_index) => {
                        let rule = &self.grammar.basic_grammar.rules[*rule_index];
                        for _ in 0..rule.rhs.len() {
                            stack.pop();
                        }
                        let new_state = *self.automaton[*stack.last().unwrap()]
                            .get(&Symbol::NonTerminal(rule.lhs))
                            .unwrap();
                        stack.push(new_state);
                    }
                    Action::Accept => {
                        return Ok(());
                    }
                },
                None => {
                    return Err(format!("Unexpected symbol {:?}", current_ahead));
                }
            }
        }
        Err("Unexpected end of input".to_string())
    }
}
