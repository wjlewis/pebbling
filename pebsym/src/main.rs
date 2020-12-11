use std::env;
use std::fmt;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 3 {
        println!("Usage: fastpebsym <depth> <total-amt>");
        std::process::exit(1);
    }

    let depth = args[1].parse::<u32>();
    let height = args[2].parse::<u32>();

    if depth.is_err() {
        println!(
            r#"Depth must be a positive integer (received "{}")"#,
            args[1]
        );
    }
    if height.is_err() {
        println!(
            r#"Height must be a positive integer (received "{}")"#,
            args[2]
        );
    }

    let pebbleable = Tree::candidates(depth.unwrap(), height.unwrap())
        .filter(|tree| tree.is_pebbleable())
        .collect::<Vec<Tree>>();

    if pebbleable.len() == 0 {
        println!("No pebbleable solutions");
        return;
    }

    println!(" r");
    for tree in pebbleable {
        println!("{}", tree);
    }
}

#[derive(Debug)]
struct Tree(Vec<u32>);

impl Tree {
    fn candidates(depth: u32, total: u32) -> Box<dyn Iterator<Item = Tree>> {
        Tree::gen_candidates(depth, total, depth as usize)
    }

    fn gen_candidates(depth: u32, total: u32, tree_depth: usize) -> Box<dyn Iterator<Item = Tree>> {
        if depth == 0 {
            let items = vec![0; tree_depth];
            Box::new(std::iter::once(Tree(items)))
        } else {
            Box::new(root_amts(total).flat_map(move |root_amt| {
                let residue = (total - root_amt) / 2;
                Tree::gen_candidates(depth - 1, residue, tree_depth).map(move |mut subtree| {
                    subtree.0[tree_depth - depth as usize] = root_amt;
                    subtree
                })
            }))
        }
    }

    fn is_pebbleable(&self) -> bool {
        let mut from_above = 0;

        for (i, amt) in self.0.iter().enumerate() {
            let rest = &self.0[i + 1..];
            let from_below = muster(rest);
            let total_at_node = amt + from_above / 2 + from_below / 2;

            if total_at_node == 0 {
                return false;
            }

            from_above = total_at_node;
        }

        true
    }
}

impl fmt::Display for Tree {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

fn muster(items: &[u32]) -> u32 {
    let mut acc = 0;

    for amt in items.iter().rev() {
        acc = amt + 2 * (acc / 2);
    }

    acc
}

fn root_amts(total: u32) -> impl Iterator<Item = u32> {
    if total % 2 == 0 {
        (0..total + 1).rev().step_by(2)
    } else {
        (1..total + 1).rev().step_by(2)
    }
}
