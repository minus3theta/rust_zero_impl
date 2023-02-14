use std::{cmp::Ordering, collections::BTreeMap, mem};

use anyhow::bail;

use crate::{helper::safe_add, parser};

type VarToType = BTreeMap<String, Option<parser::TypeExpr>>;

#[derive(Debug, Clone, Eq, PartialEq, Default)]
struct TypeEnvStack {
    vars: BTreeMap<usize, VarToType>,
}

impl TypeEnvStack {
    fn new() -> Self {
        Default::default()
    }

    fn push(&mut self, depth: usize) {
        self.vars.insert(depth, BTreeMap::new());
    }

    fn pop(&mut self, depth: usize) -> Option<VarToType> {
        self.vars.remove(&depth)
    }

    fn insert(&mut self, key: String, value: parser::TypeExpr) {
        if let Some(last) = self.vars.values_mut().next_back() {
            last.insert(key, Some(value));
        }
    }

    fn get_mut(&mut self, key: &str) -> Option<(usize, &mut Option<parser::TypeExpr>)> {
        for (depth, elm) in self.vars.iter_mut().rev() {
            if let Some(e) = elm.get_mut(key) {
                return Some((*depth, e));
            }
        }
        None
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeEnv {
    env_lin: TypeEnvStack,
    env_un: TypeEnvStack,
}

impl TypeEnv {
    pub fn new() -> Self {
        Self {
            env_lin: TypeEnvStack::new(),
            env_un: TypeEnvStack::new(),
        }
    }

    fn push(&mut self, depth: usize) {
        self.env_lin.push(depth);
        self.env_un.push(depth);
    }

    fn pop(&mut self, depth: usize) -> (Option<VarToType>, Option<VarToType>) {
        let t1 = self.env_lin.pop(depth);
        let t2 = self.env_un.pop(depth);
        (t1, t2)
    }

    fn insert(&mut self, key: String, value: parser::TypeExpr) {
        if value.qual == parser::Qual::Lin {
            self.env_lin.insert(key, value);
        } else {
            self.env_un.insert(key, value);
        }
    }

    fn get_mut(&mut self, key: &str) -> Option<&mut Option<parser::TypeExpr>> {
        match (self.env_lin.get_mut(key), self.env_un.get_mut(key)) {
            (Some((d1, t1)), Some((d2, t2))) => match d1.cmp(&d2) {
                Ordering::Less => Some(t2),
                Ordering::Equal => Some(t1),
                Ordering::Greater => panic!("invalid type environment"),
            },
            (Some((_, t1)), None) => Some(t1),
            (None, Some((_, t2))) => Some(t2),
            _ => None,
        }
    }
}

type TResult = anyhow::Result<parser::TypeExpr>;

pub fn typing(expr: &parser::Expr, env: &mut TypeEnv, depth: usize) -> TResult {
    use parser::Expr;
    match expr {
        Expr::App(e) => typing_app(e, env, depth),
        Expr::QVal(e) => typing_qval(e, env, depth),
        Expr::Free(e) => typing_free(e, env, depth),
        Expr::If(e) => typing_if(e, env, depth),
        Expr::Split(e) => typing_split(e, env, depth),
        Expr::Var(e) => typing_var(e, env),
        Expr::Let(e) => typing_let(e, env, depth),
    }
}

fn typing_qval(expr: &parser::QValExpr, env: &mut TypeEnv, depth: usize) -> TResult {
    use parser::ValExpr;
    let p = match &expr.val {
        ValExpr::Bool(_) => parser::PrimType::Bool,
        ValExpr::Pair(e1, e2) => {
            let t1 = typing(e1, env, depth)?;
            let t2 = typing(e2, env, depth)?;

            if expr.qual == parser::Qual::Un
                && (t1.qual == parser::Qual::Lin || t2.qual == parser::Qual::Lin)
            {
                bail!("un型のペア内でlin型を利用している");
            }

            parser::PrimType::Pair(Box::new(t1), Box::new(t2))
        }
        ValExpr::Fun(e) => {
            let env_prev = (expr.qual == parser::Qual::Un).then(|| mem::take(&mut env.env_lin));

            let mut depth = depth;
            safe_add(&mut depth, &1, || {
                anyhow::anyhow!("変数スコープのネストが深すぎる")
            })?;
            env.push(depth);
            env.insert(e.var.clone(), e.ty.clone());

            let t = typing(&e.expr, env, depth)?;

            let (elin, _) = env.pop(depth);
            for (k, v) in elin.unwrap().iter() {
                if v.is_some() {
                    bail!("関数定義内でlin型の変数\"{k}\"を消費していない");
                }
            }

            if let Some(ep) = env_prev {
                env.env_lin = ep;
            }

            parser::PrimType::Arrow(Box::new(e.ty.clone()), Box::new(t))
        }
    };

    Ok(parser::TypeExpr {
        qual: expr.qual,
        prim: p,
    })
}

fn typing_var(expr: &str, env: &mut TypeEnv) -> TResult {
    let ret = env.get_mut(expr);
    if let Some(it) = ret {
        if let Some(t) = it {
            if t.qual == parser::Qual::Lin {
                let eret = t.clone();
                *it = None;
                return Ok(eret);
            } else {
                return Ok(t.clone());
            }
        }
    }
    bail!("\"{expr}\"という変数は定義されていないか、利用済みか、キャプチャできない")
}

fn typing_if(expr: &parser::IfExpr, env: &mut TypeEnv, depth: usize) -> TResult {
    let t1 = typing(&expr.cond_expr, env, depth)?;
    if t1.prim != parser::PrimType::Bool {
        bail!("ifの条件式がboolでない");
    }

    let mut e = env.clone();
    let t2 = typing(&expr.then_expr, &mut e, depth)?;
    let t3 = typing(&expr.else_expr, env, depth)?;

    if t2 != t3 || e != *env {
        bail!("ifのthenとelseの式の型が異なる");
    }

    Ok(t2)
}

fn typing_split(expr: &parser::SplitExpr, env: &mut TypeEnv, depth: usize) -> TResult {
    if expr.left == expr.right {
        bail!("splitの2つの変数名が同じ");
    }

    let texpr = typing(&expr.expr, env, depth)?;
    let (tleft, tright) = if let parser::PrimType::Pair(tleft, tright) = texpr.prim {
        (tleft, tright)
    } else {
        bail!("splitの分割する型がpairでない")
    };

    let mut depth = depth;
    safe_add(&mut depth, &1, || {
        anyhow::anyhow!("変数スコープのネストが深すぎる")
    })?;

    env.push(depth);
    env.insert(expr.left.clone(), *tleft);
    env.insert(expr.right.clone(), *tright);

    let tbody = typing(&expr.body, env, depth)?;

    let (env_lin, _) = env.pop(depth);
    for (k, v) in env_lin.unwrap().iter() {
        if v.is_some() {
            bail!("split内でlin型の変数\"{k}\"を消費していない");
        }
    }

    Ok(tbody)
}

fn typing_let(expr: &parser::LetExpr, env: &mut TypeEnv, depth: usize) -> TResult {
    let t_def = typing(&expr.expr1, env, depth)?;
    let mut depth = depth;
    safe_add(&mut depth, &1, || {
        anyhow::anyhow!("変数スコープのネストが深すぎる")
    })?;

    env.push(depth);
    env.insert(expr.var.clone(), t_def);

    let t_body = typing(&expr.expr2, env, depth)?;

    let (env_lin, _) = env.pop(depth);
    for (k, v) in env_lin.unwrap().iter() {
        if v.is_some() {
            bail!("let内でlin型の変数\"{k}\"を消費していない");
        }
    }

    Ok(t_body)
}

fn typing_app(expr: &parser::AppExpr, env: &mut TypeEnv, depth: usize) -> TResult {
    let t_fun = typing(&expr.expr1, env, depth)?;
    let (t_dom, t_cod) = if let parser::PrimType::Arrow(t_dom, t_cod) = t_fun.prim {
        (*t_dom, *t_cod)
    } else {
        bail!("関数適用の関数がarrow型でない");
    };

    let t_arg = typing(&expr.expr2, env, depth)?;
    if t_arg != t_dom {
        bail!("関数の引数の型が一致しない");
    }

    Ok(t_cod)
}

fn typing_free(expr: &parser::FreeExpr, env: &mut TypeEnv, depth: usize) -> TResult {
    if let Some((_, t)) = env.env_lin.get_mut(&expr.var) {
        if t.is_some() {
            *t = None;
            return typing(&expr.expr, env, depth);
        }
    }
    bail!(
        "既にfreeしたか、lin型ではない変数\"{}\"をfreeしている",
        &expr.var
    );
}
