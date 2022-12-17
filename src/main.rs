use std::{
    cmp::{Eq, Ord, Ordering},
    collections::{BinaryHeap, HashMap},
    env,
    fs::{self, OpenOptions},
    io::Write,
    ops::Add,
    path,
};

const USAGE_HINT: &str = "
USAGE:

    hf -c PATH
    hf -d PATH
    hf --help

For more information try --help
";

const VERBOSE: &str = "
    TODO: verbose
";

#[derive(Clone)]
struct BitSet {
    bytes: Vec<u8>,
    pos: u64,
}

impl BitSet {
    fn new() -> Self {
        BitSet {
            bytes: Vec::<u8>::from([0]),
            pos: 0,
        }
    }

    fn from(bytes: &[u8]) -> Self {
        BitSet {
            bytes: Vec::<u8>::from(bytes),
            pos: 8 * bytes.len() as u64,
        }
    }

    fn len(&self) -> usize {
        self.bytes.len()
    }

    fn push_bit(&mut self, bit: u8) {
        let i = (self.pos / 8) as usize;
        let r = self.pos % 8;
        if i == self.bytes.len() {
            self.bytes.push(0);
        }
        if bit == 1 {
            self.bytes[i] |= 1 << r;
        }
        self.pos += 1;
    }

    fn push_u32(&mut self, x: u32) {
        for i in 0..32 {
            let mask = 1 << i;
            let bit = (x | mask).min(1) as u8;
            self.push_bit(bit)
        }
    }

    fn raw_bytes(self) -> Vec<u8> {
        self.bytes
    }

    fn get_bit(&self, index: usize) -> u8 {
        let i = index / 8;
        let r = index % 8;
        let mask = 1 << r;
        (self.bytes[i] & mask).min(1)
    }

    fn push_back(&mut self, rhs: BitSet) {
        for i in 0..rhs.pos {
            self.push_bit(rhs.get_bit(i as usize))
        }
    }
}

impl ToString for BitSet {
    fn to_string(&self) -> String {
        let mut s = String::new();
        for i in 0..self.pos {
            s.push(match self.get_bit(i as usize).min(1) {
                1 => '1',
                0 => '0',
                _ => panic!(""),
            });
        }
        return s;
    }
}

#[derive(Clone, Copy, Debug)]
struct Pair(u64, char);

impl Pair {
    fn new(k: u64, v: char) -> Self {
        return Self { 0: k, 1: v };
    }
}

impl Add for Pair {
    type Output = Pair;
    fn add(self, other: Pair) -> Pair {
        Pair {
            0: self.0 + other.0,
            1: char::default(),
        }
    }
}

impl PartialEq for Pair {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }

    fn ne(&self, other: &Self) -> bool {
        self.0 != other.0
    }
}

impl PartialOrd for Pair {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self.lt(other), self.eq(other), self.gt(other)) {
            (true, _, _) => Some(Ordering::Less),
            (_, true, _) => Some(Ordering::Equal),
            (_, _, true) => Some(Ordering::Greater),
            _ => None,
        }
    }
    fn lt(&self, other: &Self) -> bool {
        self.0 > other.0
    }
    fn le(&self, other: &Self) -> bool {
        self.0 >= other.0
    }
    fn gt(&self, other: &Self) -> bool {
        self.0 < other.0
    }
    fn ge(&self, other: &Self) -> bool {
        self.0 <= other.0
    }
}

impl Eq for Pair {}

impl Ord for Pair {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap()
    }

    fn max(self, other: Self) -> Self {
        match self.cmp(&other) {
            Ordering::Less => other,
            Ordering::Equal => other,
            Ordering::Greater => self,
        }
    }
    fn min(self, other: Self) -> Self {
        match self.cmp(&other) {
            Ordering::Less => self,
            Ordering::Equal => self,
            Ordering::Greater => other,
        }
    }
    fn clamp(self, min: Self, max: Self) -> Self {
        match (self.cmp(&min), self.cmp(&max)) {
            (Ordering::Less, _) => min,
            (_, Ordering::Greater) => max,
            _ => self,
        }
    }
}

#[derive(Debug)]
struct Node<T: Eq + Ord> {
    left: Option<Box<Node<T>>>,
    right: Option<Box<Node<T>>>,
    data: T,
}

impl Node<Pair> {
    fn new(v: Pair) -> Self {
        Self {
            left: None,
            right: None,
            data: v,
        }
    }

    fn combine(self, other: Self) -> Self {
        let data = self.data.clone() + other.data.clone();
        Self {
            left: Some(Box::new(self)),
            right: Some(Box::new(other)),
            data,
        }
    }

    fn encode_chars(&self) -> HashMap<char, BitSet> {
        let mut encoded = HashMap::<char, BitSet>::new();
        self.visit(BitSet::new(), &mut |data: Pair, code: BitSet| {
            encoded.insert(data.1, code);
        });
        encoded
    }

    fn encode_tree(&self) -> BitSet {
        let mut encoded_tree = BitSet::new();
        self.encode_node(&mut encoded_tree, &mut |data: Pair, code: &mut BitSet| {
            code.push_back(BitSet::from(&[(data.1 as u8)]));
        });
        encoded_tree
    }

    fn visit(&self, code: BitSet, visitor_fn: &mut impl FnMut(Pair, BitSet)) {
        if self.right == None && self.left == None {
            visitor_fn(self.data.clone(), code.clone());
            return;
        }
        if let &Some(ref left_node) = &self.left {
            let mut left_node_code = code.clone();
            left_node_code.push_bit(0);
            left_node.visit(left_node_code, visitor_fn);
        };
        if let &Some(ref right_node) = &self.right {
            let mut right_node_code = code.clone();
            right_node_code.push_bit(1);
            right_node.visit(right_node_code, visitor_fn);
        };
    }

    fn encode_node(&self, code: &mut BitSet, visitor_fn: &mut impl FnMut(Pair, &mut BitSet)) {
        if self.right == None && self.left == None {
            code.push_bit(1);
            visitor_fn(self.data.clone(), code);
            return;
        }
        code.push_bit(0);
        if let &Some(ref left_node) = &self.left {
            left_node.encode_node(code, visitor_fn);
        };
        if let &Some(ref right_node) = &self.right {
            right_node.encode_node(code, visitor_fn);
        };
    }
}

impl<T> PartialEq for Node<T>
where
    T: Eq + Ord,
{
    fn eq(&self, other: &Self) -> bool {
        self.data == other.data
    }

    fn ne(&self, other: &Self) -> bool {
        self.data != other.data
    }
}

impl<T> PartialOrd for Node<T>
where
    T: Eq + Ord,
{
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        return self.data.partial_cmp(&other.data);
    }
    fn lt(&self, other: &Self) -> bool {
        return self.data.lt(&other.data);
    }
    fn le(&self, other: &Self) -> bool {
        return self.data.le(&other.data);
    }
    fn gt(&self, other: &Self) -> bool {
        return self.data.gt(&other.data);
    }
    fn ge(&self, other: &Self) -> bool {
        return self.data.ge(&other.data);
    }
}

impl<T> Eq for Node<T> where T: Eq + Ord {}

impl<T> Ord for Node<T>
where
    T: Eq + Ord,
{
    fn cmp(&self, other: &Self) -> Ordering {
        self.data.cmp(&other.data)
    }

    fn max(self, other: Self) -> Self {
        match self.data.cmp(&other.data) {
            Ordering::Less => other,
            Ordering::Equal => other,
            Ordering::Greater => self,
        }
    }
    fn min(self, other: Self) -> Self {
        match self.data.cmp(&other.data) {
            Ordering::Less => self,
            Ordering::Equal => self,
            Ordering::Greater => other,
        }
    }
    fn clamp(self, min: Self, max: Self) -> Self {
        match (self.cmp(&min), self.cmp(&max)) {
            (Ordering::Less, _) => min,
            (_, Ordering::Greater) => max,
            _ => self,
        }
    }
}

fn count_frequency(data: &String) -> HashMap<char, u64> {
    let mut char_freq_table: HashMap<char, u64> = HashMap::new();
    for c in data.chars() {
        char_freq_table
            .entry(c)
            .and_modify(|counter| *counter += 1)
            .or_insert(1);
    }
    char_freq_table
}

fn build_huffman_tree(char_freq_table: HashMap<char, u64>) -> Option<Node<Pair>> {
    let mut heap = BinaryHeap::<Node<Pair>>::new();
    for (k, v) in char_freq_table {
        heap.push(Node::new(Pair::new(v, k)))
    }
    while heap.len() != 1 {
        let fst = heap.pop();
        let snd = heap.pop();
        match (fst, snd) {
            (Some(fst_node), Some(snd_node)) => {
                let new_node = fst_node.combine(snd_node);
                heap.push(new_node);
            }
            _ => panic!("unexpected element in heap was found"),
        }
    }
    heap.pop()
}

fn encode_data(text: &String, huffman_codes: HashMap<char, BitSet>) -> BitSet {
    let mut text_bit_set = BitSet::new();
    for c in text.chars() {
        text_bit_set.push_back(huffman_codes[&c].clone());
    }
    text_bit_set
}

struct CompressedFile {
    compressed_file_size: u32,
    header_size: u32,
    uncomporessed_file_size: u32,
    tree: BitSet,
    data: BitSet,
}

impl CompressedFile {
    fn new(original_file_size: u32, tree: BitSet, data: BitSet) -> Self {
        return CompressedFile {
            compressed_file_size: 12 + (tree.len() as u32) + (data.len() as u32),
            header_size: 12 + (tree.len() as u32),
            uncomporessed_file_size: original_file_size,
            tree,
            data,
        };
    }

    fn write(self, file: &path::Path) {
        let mut file = OpenOptions::new()
            .write(true)
            .create(true)
            .open(file)
            .unwrap();
        let mut encoded_data = BitSet::new();
        encoded_data.push_u32(self.compressed_file_size);
        encoded_data.push_u32(self.header_size);
        encoded_data.push_u32(self.uncomporessed_file_size);
        encoded_data.push_back(self.tree);
        encoded_data.push_back(self.data);
        file.write_all(&encoded_data.raw_bytes()).unwrap();
    }
}

fn compress(from: &path::Path, to: &path::Path) {
    let ok = fs::read_to_string(from);
    let data: String;
    match ok {
        Ok(str) => data = str,
        Err(msg) => panic!("{msg}"),
    }
    let char_freq_table = count_frequency(&data);
    let huffman_codes: HashMap<char, BitSet>;
    let encoded_tree: BitSet;
    let encoded_data: BitSet;
    match build_huffman_tree(char_freq_table) {
        Some(root) => {
            huffman_codes = root.encode_chars();
            encoded_tree = root.encode_tree();
            encoded_data = encode_data(&data, huffman_codes);
        }
        None => panic!("binary heap is empty"),
    }
    let original_file_size = data.len() as u32;
    let compressed_file = CompressedFile::new(original_file_size, encoded_tree, encoded_data);
    compressed_file.write(to);
}

fn decompress(_: &path::Path, _: &path::Path) {
    println!("TODO: make implementation");
}

fn main() {
    let args: Vec<String> = env::args().collect();
    match args.len() {
        4 => match args[1].as_str() {
            "-c" => compress(&path::Path::new(&args[2]), &path::Path::new(&args[3])),
            "-d" => decompress(&path::Path::new(&args[2]), &path::Path::new(&args[3])),
            _ => println!("{USAGE_HINT}"),
        },
        2 => match args[1].as_str() {
            "--help" => println!("{VERBOSE}"),
            _ => println!("{USAGE_HINT}"),
        },
        _ => println!("{USAGE_HINT}"),
    }
}
