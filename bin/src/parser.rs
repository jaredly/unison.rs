use byteorder::{BigEndian, ByteOrder};
use log::info;
use std::collections::HashMap;
use std::fs::File;
use std::io::Read;

use shared::types::*;

pub trait FromBuffer {
    fn get(buf: &mut Buffer) -> Self;
}

pub trait FromBufferWithEnv {
    fn get(buf: &mut Buffer, env: &Vec<Symbol>, fvs: &Vec<Symbol>) -> Self;
}

pub struct Buffer {
    buf: Vec<u8>,
    idx: usize,
    pub indent: usize,
}

impl Buffer {
    pub fn from_file(path: &std::path::Path) -> std::io::Result<Self> {
        let mut buf = vec![];
        let mut file = File::open(path)?;
        file.read_to_end(&mut buf)?;
        Ok(Buffer {
            buf,
            idx: 0,
            indent: 0,
        })
    }

    pub fn get_n(&mut self, count: usize) -> &[u8] {
        self.idx += count;
        let res = &self.buf[self.idx - count..self.idx];
        res
    }

    pub fn get<T: FromBuffer>(&mut self) -> T {
        info!(
            "{}Getting {:?} @ {}",
            indent(self.indent),
            std::any::type_name::<T>(),
            self.idx
        );
        self.indent += 1;
        let res = T::get(self);
        self.indent -= 1;
        res
    }

    pub fn get_with_env<T: FromBufferWithEnv>(
        &mut self,
        env: &Vec<Symbol>,
        fvs: &Vec<Symbol>,
    ) -> T {
        info!(
            "{}Getting with env {:?} @ {}",
            indent(self.indent),
            std::any::type_name::<T>(),
            self.idx
        );
        T::get(self, env, fvs)
    }

    pub fn get_branch(&mut self) -> crate::branch::Causal<crate::branch::RawBranch> {
        self.get()
    }

    pub fn get_type(&mut self) -> TypeDecl {
        // println!("Getting type");
        self.get()
    }

    pub fn get_term_type(&mut self) -> ABT<Type> {
        self.get()
    }

    pub fn get_term(&mut self) -> ABT<Term> {
        self.get()
    }
}

fn indent(n: usize) -> String {
    let mut res = "".to_owned();
    for _ in 0..n {
        res += "|  ";
    }
    res
}

impl FromBuffer for String {
    fn get(buf: &mut Buffer) -> Self {
        let length = usize::get(buf);
        let raw = buf.get_n(length);
        let res = String::from_utf8(raw.to_owned()).unwrap();
        info!("{}String: {}", indent(buf.indent), res);
        res
    }
}

impl FromBuffer for char {
    fn get(buf: &mut Buffer) -> Self {
        // String::from_utf8(buf.get::<usize>().to_owned())
        buf.get_n(1)[0] as char
    }
}

impl FromBuffer for u8 {
    fn get(buf: &mut Buffer) -> Self {
        buf.get_n(1)[0]
    }
}

impl FromBuffer for i64 {
    fn get(buf: &mut Buffer) -> Self {
        let data = buf.get_n(8);
        let value = BigEndian::read_u64(&data);
        return value as i64;
    }
}

impl FromBuffer for u64 {
    fn get(buf: &mut Buffer) -> Self {
        let data = buf.get_n(8);
        return BigEndian::read_u64(&data);
    }
}

impl FromBuffer for f64 {
    fn get(buf: &mut Buffer) -> Self {
        let data = buf.get_n(8);
        use std::convert::TryInto;
        return f64::from_be_bytes(data.try_into().unwrap());
    }
}

impl FromBuffer for bool {
    fn get(buf: &mut Buffer) -> Self {
        let data: u8 = buf.get();
        return data == 1;
    }
}

impl FromBuffer for usize {
    fn get(buf: &mut Buffer) -> Self {
        // Varint, described here https://developers.google.com/protocol-buffers/docs/encoding#varints
        let word8 = u8::get(buf);
        info!("{}Getting usize buffer {:08b}", indent(buf.indent), word8);
        let indicator = word8 >> 7;

        let res = if indicator == 0 {
            word8 as usize
        } else {
            let word8 = word8 ^ 128; // clear the indicator
            let rest = usize::get(buf);
            (rest << 7) + word8 as usize
        };

        info!("{}usize: {}", indent(buf.indent), res);
        res
    }
}

impl<T: FromBuffer> FromBuffer for Vec<T> {
    fn get(buf: &mut Buffer) -> Self {
        let mut res = vec![];
        let length = usize::get(buf);
        info!("{}vec ({} elements)", indent(buf.indent), length);
        for _ in 0..length {
            res.push(T::get(buf));
        }
        res
    }
}

impl FromBuffer for Symbol {
    fn get(buf: &mut Buffer) -> Self {
        Symbol {
            num: buf.get(),
            text: buf.get(),
            unique: 0,
        }
    }
}

impl FromBuffer for ConstructorType {
    fn get(buf: &mut Buffer) -> Self {
        let tag: u8 = buf.get();
        match tag {
            0 => ConstructorType::Data,
            1 => ConstructorType::Effect,
            _ => unreachable!(),
        }
    }
}

impl FromBuffer for Hash {
    fn get(buf: &mut Buffer) -> Self {
        let len: usize = buf.get();
        let res = Hash(crate::base32hex::encode(buf.get_n(len)));
        info!("{}Hash: #{:?}", indent(buf.indent), res);
        res
    }
}

impl FromBuffer for Id {
    fn get(buf: &mut Buffer) -> Self {
        Id(buf.get(), buf.get(), buf.get())
    }
}

impl FromBuffer for Reference {
    fn get(buf: &mut Buffer) -> Self {
        let tag = buf.get();
        match tag {
            0_u8 => Reference::Builtin(buf.get()),
            1 => Reference::DerivedId(buf.get()),
            _ => unreachable!(),
        }
    }
}

impl FromBuffer for Referent {
    fn get(buf: &mut Buffer) -> Self {
        let tag = buf.get();
        info!("{}Referent tag : {}", indent(buf.indent), tag);
        match tag {
            0_u8 => Referent::Ref(buf.get()),
            1 => Referent::Con(buf.get(), buf.get(), buf.get()),
            _ => unreachable!(),
        }
    }
}

impl FromBufferWithEnv for MatchCase {
    fn get(buf: &mut Buffer, env: &Vec<Symbol>, fvs: &Vec<Symbol>) -> Self {
        MatchCase(
            buf.get(),
            buf.get_with_env(env, fvs),
            buf.get_with_env(env, fvs),
        )
    }
}

impl FromBuffer for Kind {
    fn get(buf: &mut Buffer) -> Self {
        let tag: u8 = buf.get();
        match tag {
            0 => Kind::Star,
            1 => Kind::Arrow(buf.get(), buf.get()),
            _ => unreachable!("Kind tag {}", tag),
        }
    }
}

impl FromBuffer for Pattern {
    fn get(buf: &mut Buffer) -> Self {
        let tag: u8 = buf.get();
        info!("Pattern branch {}", tag);
        match tag {
            0 => Pattern::Unbound,
            1 => Pattern::Var,
            2 => Pattern::Boolean(buf.get()),
            3 => Pattern::Int(buf.get()),
            4 => Pattern::Nat(buf.get()),
            5 => Pattern::Float(buf.get()),
            6 => Pattern::Constructor(buf.get(), buf.get(), buf.get()),
            7 => Pattern::As(buf.get()),
            8 => Pattern::EffectPure(buf.get()),
            9 => Pattern::EffectBind(buf.get(), buf.get(), buf.get(), buf.get()),
            10 => Pattern::SequenceLiteral(buf.get()),
            11 => Pattern::SequenceOp(buf.get(), buf.get(), buf.get()),
            12 => Pattern::Text(buf.get()),
            13 => Pattern::Char(buf.get()),
            _ => unreachable!("Kind tag {}", tag),
        }
    }
}

impl FromBuffer for SeqOp {
    fn get(buf: &mut Buffer) -> Self {
        let tag: u8 = buf.get();
        match tag {
            0 => SeqOp::Cons,
            1 => SeqOp::Snoc,
            2 => SeqOp::Concat,
            _ => unreachable!("Kind tag {}", tag),
        }
    }
}

impl<T: FromBuffer> FromBuffer for Box<T> {
    fn get(buf: &mut Buffer) -> Self {
        T::get(buf).into()
    }
}

impl<T: FromBufferWithEnv> FromBufferWithEnv for Box<T> {
    fn get(buf: &mut Buffer, env: &Vec<Symbol>, fvs: &Vec<Symbol>) -> Self {
        T::get(buf, env, fvs).into()
    }
}

impl<T: FromBufferWithEnv> FromBufferWithEnv for Option<T> {
    fn get(buf: &mut Buffer, env: &Vec<Symbol>, fvs: &Vec<Symbol>) -> Self {
        let tag = buf.get();
        match tag {
            0_u8 => None,
            1 => Some(T::get(buf, env, fvs).into()),
            _ => unreachable!("Option tag {}", tag),
        }
    }
}

impl<T: FromBufferWithEnv> FromBufferWithEnv for Vec<T> {
    fn get(buf: &mut Buffer, env: &Vec<Symbol>, fvs: &Vec<Symbol>) -> Self {
        let mut res = vec![];
        let length = usize::get(buf);
        info!("{}env vec ({} elements)", indent(buf.indent), length);
        for _ in 0..length {
            res.push(T::get(buf, env, fvs));
        }
        res
    }
}

impl FromBufferWithEnv for Type {
    fn get(buf: &mut Buffer, env: &Vec<Symbol>, fvs: &Vec<Symbol>) -> Self {
        let tag = u8::get(buf);
        info!("{}Type ({})", indent(buf.indent), tag);
        match tag {
            0 => Type::Ref(buf.get()),
            1 => Type::Arrow(buf.get_with_env(env, fvs), buf.get_with_env(env, fvs)),
            2 => Type::Ann(buf.get_with_env(env, fvs), buf.get()),
            3 => Type::App(buf.get_with_env(env, fvs), buf.get_with_env(env, fvs)),
            4 => Type::Effect(buf.get_with_env(env, fvs), buf.get_with_env(env, fvs)),
            5 => Type::Effects(buf.get_with_env(env, fvs)),
            6 => Type::Forall(buf.get_with_env(env, fvs)),
            7 => Type::IntroOuter(buf.get_with_env(env, fvs)),
            _ => unreachable!("Type tag {}", tag),
        }
    }
}

impl FromBufferWithEnv for Term {
    fn get(buf: &mut Buffer, env: &Vec<Symbol>, fvs: &Vec<Symbol>) -> Self {
        let tag = u8::get(buf);
        info!("{}Term {}", indent(buf.indent), tag);
        match tag {
            0 => Term::Int(buf.get()),
            1 => Term::Nat(buf.get()),
            2 => Term::Float(buf.get()),
            3 => Term::Boolean(buf.get()),
            4 => Term::Text(buf.get()),
            5 => Term::Ref(buf.get()),
            6 => Term::Constructor(buf.get(), buf.get()),
            7 => Term::Request(buf.get(), buf.get()),
            8 => Term::Handle(buf.get_with_env(env, fvs), buf.get_with_env(env, fvs)),
            9 => Term::App(buf.get_with_env(env, fvs), buf.get_with_env(env, fvs)),
            10 => Term::Ann(buf.get_with_env(env, fvs), buf.get()),
            11 => Term::Sequence(buf.get_with_env(env, fvs)),
            12 => Term::If(
                buf.get_with_env(env, fvs),
                buf.get_with_env(env, fvs),
                buf.get_with_env(env, fvs),
            ),
            13 => Term::And(buf.get_with_env(env, fvs), buf.get_with_env(env, fvs)),
            14 => Term::Or(buf.get_with_env(env, fvs), buf.get_with_env(env, fvs)),
            15 => Term::Lam(buf.get_with_env(env, fvs), vec![]),
            16 => Term::LetRec(
                false,
                buf.get_with_env(env, fvs),
                buf.get_with_env(env, fvs),
            ),
            17 => Term::Let(
                false,
                buf.get_with_env(env, fvs),
                buf.get_with_env(env, fvs),
            ),
            18 => Term::Match(buf.get_with_env(env, fvs), buf.get_with_env(env, fvs)),
            19 => Term::Char(buf.get()),
            20 => Term::TermLink(buf.get()),
            21 => Term::TypeLink(buf.get()),
            _ => panic!("Failed {}", tag),
        }
    }
}

impl<Inner: FromBufferWithEnv + std::fmt::Debug> FromBufferWithEnv for ABT<Inner> {
    fn get(buf: &mut Buffer, env: &Vec<Symbol>, fvs: &Vec<Symbol>) -> Self {
        let tag: u8 = buf.get();
        info!("{}ABT ({})", indent(buf.indent), tag);
        buf.indent += 1;
        let res = match tag {
            0 => {
                let tag = u8::get(buf);
                match tag {
                    0 => ABT::Var(env[buf.get::<usize>()].clone(), 0),
                    1 => ABT::Var(fvs[buf.get::<usize>()].clone(), 0),
                    _ => unreachable!(),
                }
            }
            1 => ABT::Tm(Inner::get(buf, env, fvs)),
            2 => {
                let v: Symbol = buf.get();
                let mut nw = env.to_owned();
                nw.insert(0, v.clone());
                ABT::Abs(v, 0, buf.get_with_env(&nw, fvs))
            }
            3 => ABT::Cycle(buf.get_with_env(env, fvs)),
            _ => unreachable!("ABT {}", tag),
        };
        info!("{}ABT: {:?}", indent(buf.indent), res);
        buf.indent -= 1;
        res
    }
}

impl FromBuffer for ABT<Type> {
    fn get(buf: &mut Buffer) -> Self {
        let names = Vec::<Symbol>::get(buf);
        info!("{}FVS: {:?}", indent(buf.indent), names);
        buf.get_with_env(&vec![], &names)
    }
}

impl FromBuffer for ABT<Term> {
    fn get(buf: &mut Buffer) -> Self {
        let names = Vec::<Symbol>::get(buf);
        info!("{}FVS: {:?}", indent(buf.indent), names);
        buf.get_with_env(&vec![], &names)
    }
}

impl<T: FromBuffer> FromBuffer for crate::branch::Causal<T> {
    fn get(buf: &mut Buffer) -> Self {
        use crate::branch::Causal::*;
        let tag = buf.get();
        match tag {
            0_u8 => One(buf.get()),
            1 => Cons(buf.get(), buf.get()),
            2 => Merge(buf.get(), buf.get()),
            _ => unreachable!("Causal tag {}", tag),
        }
    }
}

impl<K: FromBuffer, V: FromBuffer> FromBuffer for (K, V) {
    fn get(buf: &mut Buffer) -> Self {
        (buf.get(), buf.get())
    }
}

impl<K: FromBuffer + std::hash::Hash + std::cmp::Eq + Clone, V: FromBuffer> FromBuffer
    for Star<K, V>
{
    fn get(buf: &mut Buffer) -> Self {
        Star {
            fact: buf.get(),
            d1: buf.get(),
            d2: buf.get(),
            d3: buf.get(),
        }
    }
}

impl FromBuffer for crate::branch::RawBranch {
    fn get(buf: &mut Buffer) -> Self {
        crate::branch::RawBranch {
            terms: buf.get(),
            types: buf.get(),
            children: buf.get(),
            edits: buf.get(),
        }
    }
}

impl<K: FromBuffer + std::hash::Hash + std::cmp::Eq, V: FromBuffer> FromBuffer for HashMap<K, V> {
    fn get(buf: &mut Buffer) -> Self {
        use std::iter::FromIterator;
        let items: Vec<(K, V)> = buf.get();
        HashMap::from_iter(items.into_iter())
    }
}

impl<K: FromBuffer + std::hash::Hash + std::cmp::Eq> FromBuffer for std::collections::HashSet<K> {
    fn get(buf: &mut Buffer) -> Self {
        use std::iter::FromIterator;
        let items: Vec<K> = buf.get();
        std::collections::HashSet::from_iter(items.into_iter())
    }
}

impl FromBuffer for NameSegment {
    fn get(buf: &mut Buffer) -> Self {
        NameSegment { text: buf.get() }
    }
}

impl FromBuffer for Modifier {
    fn get(buf: &mut Buffer) -> Self {
        let tag = buf.get();
        match tag {
            0_u8 => Modifier::Structural,
            1_u8 => Modifier::Unique(buf.get()),
            _ => unreachable!(),
        }
    }
}

impl FromBuffer for DataDecl {
    fn get(buf: &mut Buffer) -> Self {
        // println!("getting a datadecl");
        DataDecl {
            modifier: buf.get(),
            bound: buf.get(),
            constructors: buf.get(),
        }
        // let tag = buf.get();
        // match tag {
        //     0_u8 => TypeDecl::Effect(buf.get()),
        //     1_u8 => TypeDecl::Data(buf.get()),
        //     _ => unreachable!()
        // }
    }
}

impl FromBuffer for TypeDecl {
    fn get(buf: &mut Buffer) -> Self {
        let tag = buf.get();
        match tag {
            0_u8 => TypeDecl::Effect(buf.get()),
            1_u8 => TypeDecl::Data(buf.get()),
            _ => unreachable!(),
        }
    }
}

// (V1.getEither
//   (V1.getEffectDeclaration getV getA)
//   (V1.getDataDeclaration getV getA))
// (declPath root h)

// getDataDeclaration :: (MonadGet m, Ord v) => m v -> m a -> m (DataDeclaration v a)
// getDataDeclaration getV getA = DataDeclaration.DataDeclaration <$>
//   getModifier <*>
//   getA <*>
//   getList getV <*>
//   getList (getTuple3 getA getV (getType getV getA))

// getEffectDeclaration :: (MonadGet m, Ord v) => m v -> m a -> m (EffectDeclaration v a)
// getEffectDeclaration getV getA =
//   DataDeclaration.EffectDeclaration <$> getDataDeclaration getV getA

// data Modifier = Structural | Unique Text --  | Opaque (Set Reference)
//   deriving (Eq, Ord, Show)

// data DataDeclaration v a = DataDeclaration {
//   modifier :: Modifier,
//   annotation :: a,
//   bound :: [v],
//   constructors' :: [(a, v, Type v a)]
// } deriving (Eq, Show, Functor)

// newtype EffectDeclaration v a = EffectDeclaration {
//   toDataDecl :: DataDeclaration v a
// } deriving (Eq,Show,Functor)
