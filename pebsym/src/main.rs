use std::fmt;
use std::rc::Rc;

fn main() {
    let pebbleable = Tree::candidates(10, 687).filter(|tree| tree.is_valid());
    println!("{:?}", pebbleable.collect::<Vec<Tree>>());
}

#[derive(Debug)]
struct Tree(List<u32>);

impl Tree {
    fn candidates(depth: u32, total: u32) -> Box<dyn Iterator<Item = Tree>> {
        if depth == 0 {
            Box::new(std::iter::once(Tree(List::from(total))))
        } else {
            Box::new(root_amts(total).flat_map(move |at_root| {
                let residue = (total - at_root) / 2;
                Tree::candidates(depth - 1, residue).map(move |candidate_tree| {
                    let Tree(subtree) = candidate_tree;
                    Tree(subtree.cons(at_root))
                })
            }))
        }
    }

    fn is_valid(&self) -> bool {
        self.0.check_valid(0)
    }
}

fn root_amts(total: u32) -> impl Iterator<Item = u32> {
    if total % 2 == 0 {
        // We'd like to write this as an inclusive range, like
        // `0..=total`, but `rev` requires a `DoubleEndedIterator`.
        (0..total + 1).rev().step_by(2)
    } else {
        (1..total + 1).rev().step_by(2)
    }
}

impl List<u32> {
    fn check_valid(&self, from_above: u32) -> bool {
        self.items.check_valid(from_above)
    }
}

impl ListItems<u32> {
    fn muster(&self) -> u32 {
        match self {
            ListItems::Empty => 0,
            ListItems::Cons(amt, rest) => amt + 2 * (rest.muster() / 2),
        }
    }

    fn check_valid(&self, from_above: u32) -> bool {
        match self {
            ListItems::Empty => true,
            ListItems::Cons(amt, rest) => {
                let mustered = rest.muster();
                let at_root = amt + mustered / 2 + from_above / 2;

                at_root > 0 && rest.check_valid(at_root)
            }
        }
    }
}

#[derive(Debug)]
struct List<T> {
    items: Rc<ListItems<T>>,
}

enum ListItems<T> {
    Empty,
    Cons(T, Rc<ListItems<T>>),
}

impl<T> List<T> {
    fn empty() -> Self {
        List {
            items: Rc::new(ListItems::Empty),
        }
    }

    fn cons(&self, item: T) -> List<T> {
        List {
            items: Rc::new(ListItems::Cons(item, Rc::clone(&self.items))),
        }
    }
}

impl<T> From<T> for List<T> {
    fn from(item: T) -> List<T> {
        List::empty().cons(item)
    }
}

impl<T: fmt::Debug> fmt::Debug for ListItems<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(")?;
        let mut current = self;
        while let ListItems::Cons(head, tail) = current {
            write!(f, "{:?}", head)?;
            if let ListItems::Cons(_, _) = **tail {
                write!(f, " ")?;
            }

            current = tail;
        }
        write!(f, ")")
    }
}
