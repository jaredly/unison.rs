// enum Primitive {
//     Int(i64),
//     Nat(u64),
//     Float(f64),
// }

// enum Value {
//     Primitive(Primitive)
// }

// fn eval_builtins(env: &mut Env, name: &str, body: &[&Term])

// ("Int.-", [Term::Int(a)], Term::Int(b)) => Term::Int(a - b),
// ("Int.*", [Term::Int(a)], Term::Int(b)) => Term::Int(a * b),
// ("Int./", [Term::Int(a)], Term::Int(b)) => Term::Int(a / b),
// ("Int.<", [Term::Int(a)], Term::Int(b)) => Term::Boolean(*a < *b),
// ("Int.<=", [Term::Int(a)], Term::Int(b)) => Term::Boolean(*a <= *b),
// ("Int.>", [Term::Int(a)], Term::Int(b)) => Term::Boolean(*a > *b),
// ("Int.>=", [Term::Int(a)], Term::Int(b)) => Term::Boolean(*a >= *b),
// ("Int.==", [Term::Int(a)], Term::Int(b)) => Term::Boolean(*a == *b),
// ("Int.and", [Term::Int(a)], Term::Int(b)) => Term::Int(a & b),
// ("Int.or", [Term::Int(a)], Term::Int(b)) => Term::Int(a | b),
// ("Int.xor", [Term::Int(a)], Term::Int(b)) => Term::Int(a ^ b),
// ("Int.mod", [Term::Int(a)], Term::Int(b)) => Term::Int(a % b),
// ("Int.pow", [Term::Int(a)], Term::Nat(b)) => {
//     Term::Int(a.pow(*b as u32))
// }
// ("Int.shiftLeft", [Term::Int(a)], Term::Nat(b)) => {
//     Term::Int(a >> *b as u32)
// }
// ("Int.shiftRight", [Term::Int(a)], Term::Nat(b)) => {
//     Term::Int(a << *b as u32)
// }

// ("Nat.+", [Term::Nat(a)], Term::Nat(b)) => Term::Nat(a + b),
// ("Nat.*", [Term::Nat(a)], Term::Nat(b)) => Term::Nat(a * b),
// ("Nat./", [Term::Nat(a)], Term::Nat(b)) => Term::Nat(a / b),
// ("Nat.>", [Term::Nat(a)], Term::Nat(b)) => Term::Boolean(*a > *b),
// ("Nat.>=", [Term::Nat(a)], Term::Nat(b)) => Term::Boolean(*a >= *b),
// ("Nat.<", [Term::Nat(a)], Term::Nat(b)) => Term::Boolean(*a < *b),
// ("Nat.<=", [Term::Nat(a)], Term::Nat(b)) => Term::Boolean(*a <= *b),
// ("Nat.==", [Term::Nat(a)], Term::Nat(b)) => Term::Boolean(*a == *b),
// ("Nat.and", [Term::Nat(a)], Term::Nat(b)) => Term::Nat(a & b),
// ("Nat.or", [Term::Nat(a)], Term::Nat(b)) => Term::Nat(a | b),
// ("Nat.xor", [Term::Nat(a)], Term::Nat(b)) => Term::Nat(a ^ b),
// ("Nat.mod", [Term::Nat(a)], Term::Nat(b)) => Term::Nat(a % b),
// ("Nat.pow", [Term::Nat(a)], Term::Nat(b)) => {
//     Term::Nat(a.pow(*b as u32))
// }
// ("Nat.shiftLeft", [Term::Nat(a)], Term::Nat(b)) => {
//     Term::Nat(a >> *b as u32)
// }
// ("Nat.shiftRight", [Term::Nat(a)], Term::Nat(b)) => {
//     Term::Nat(a << *b as u32)
// }

// // , ("Nat.drop", 2, DropN (Slot 1) (Slot 0))
// // , ("Nat.sub", 2, SubN (Slot 1) (Slot 0))
// // , ("Nat.mod", 2, ModN (Slot 1) (Slot 0))
// // , ("Nat.pow", 2, PowN (Slot 1) (Slot 0))
// ("Float.+", [Term::Float(a)], Term::Float(b)) => Term::Float(a + b),
// ("Float.-", [Term::Float(a)], Term::Float(b)) => Term::Float(a - b),
// ("Float.*", [Term::Float(a)], Term::Float(b)) => Term::Float(a * b),
// ("Float./", [Term::Float(a)], Term::Float(b)) => Term::Float(a / b),
// ("Float.<", [Term::Float(a)], Term::Float(b)) => Term::Boolean(*a < *b),
// ("Float.<=", [Term::Float(a)], Term::Float(b)) => {
//     Term::Boolean(*a <= *b)
// }
// ("Float.>", [Term::Float(a)], Term::Float(b)) => Term::Boolean(*a > *b),
// ("Float.>=", [Term::Float(a)], Term::Float(b)) => {
//     Term::Boolean(*a >= *b)
// }
// ("Float.==", [Term::Float(a)], Term::Float(b)) => {
//     Term::Boolean(*a == *b)
// }

// ("Universal.==", [one], two) => Term::Boolean(one == two),
// ("Universal.>", [one], two) => Term::Boolean(one > two),
// ("Universal.<", [one], two) => Term::Boolean(one < two),
// ("Universal.>=", [one], two) => Term::Boolean(one >= two),
// ("Universal.<=", [one], two) => Term::Boolean(one <= two),
// ("Universal.compare", [one], two) => Term::Int(if one < two {
//     -1
// } else if one > two {
//     1
// } else {
//     0
// }),

// ("Int.increment", Term::Int(i)) => Term::Int(i + 1),
// ("Int.negate", Term::Int(i)) => Term::Int(-i),
// ("Int.isEven", Term::Int(i)) => Term::Boolean(i % 2 == 0),
// ("Int.isOdd", Term::Int(i)) => Term::Boolean(i % 2 == 1),
// ("Nat.increment", Term::Nat(i)) => Term::Nat(i + 1),
// ("Nat.isEvent", Term::Nat(i)) => Term::Boolean(i % 2 == 0),
// ("Nat.isOdd", Term::Nat(i)) => Term::Boolean(i % 2 == 1),
// ("Nat.toInt", Term::Nat(i)) => Term::Int(i as i64),
// ("Boolean.not", Term::Boolean(i)) => Term::Boolean(!i),

// // , ("Int.complement", 1, ComplementI (Slot 0))
// // , ("Int.signum", 1, SignumI (Slot 0))
// // , ("Int.truncate0", 1, Truncate0I (Slot 0))
// // , ("Int.leadingZeros", 1, LeadZeroI (Slot 0))
// // , ("Int.trailingZeros", 1, TrailZeroI (Slot 0))
// // , ("bug", 1, Bug (Slot 0))
// // , ("todo", 1, Todo (Slot 0))
// // , ("Nat.complement", 1, ComplementN (Slot 0))
// // , ("Nat.leadingZeros", 1, LeadZeroN (Slot 0))
// // , ("Nat.trailingZeros", 1, TrailZeroN (Slot 0))
