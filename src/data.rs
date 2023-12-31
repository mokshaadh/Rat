use std::{collections::{HashMap, HashSet, VecDeque}, fmt, rc::Rc};

use crate::parser::Ast;


// VALUES

#[derive(Clone)]
pub enum Value {
    Empty,
    Identifier(usize, String),
    Number(usize, String),
    Lambda((usize, usize), String, Box<Value>, Context),
    Application((usize, usize), Box<Value>, Box<Value>),
    Let((usize, usize), String, Box<Value>, Box<Value>, Context)
}
impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Identifier(_, name) => write!(f, "{}", name),
            Value::Number(_, num) => write!(f, "{}", num),
            Value::Lambda(_, parameter, body, ctx) => write!(f, "λ{}. {} | {}", parameter, body, ctx),
            Value::Application(_, func, arg) => write!(f, "({}) {}", func, arg),
            Value::Let(_, name, var_val, body, ctx) => write!(f, "let {} = {} in {} | {}", name, var_val, body, ctx),
            Value::Empty => write!(f, ""),
        }
    }
}


// CONTEXTS

#[derive(Clone)]
pub struct Context {
    bindings: HashMap<String, PolyType>
}

impl Context {
    pub fn new() -> Self {
        Self { bindings: HashMap::new() }
    }

    pub fn add(&mut self, name: &str, ty: &PolyType) {
        self.bindings.insert(name.to_string(), ty.clone());
    }

    fn get(&self, string: &str) -> Option<&PolyType> {
        self.bindings.get(string)
    }
}

impl Substitutable for Context {
    fn apply(&self, sub: &Substitution) -> Self {
        Self { bindings:
            self.bindings.clone()
            .into_iter()
            .map(| (k, v) | (k, v.apply(sub)))
            .collect()
        }
    }
}

impl FreeVars for Context {
    fn free_vars(&self) -> HashSet<String> {
        self.bindings.values().flat_map(| v | v.free_vars()).collect()
    }
}

impl fmt::Display for Context {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut retr = String::new();
        for b in self.bindings.clone() {
            retr.push_str(&format!("{}: {}, ", b.0, b.1));
        }
        write!(f, "{}", retr)
    }
}


// ENVIRONMENT

#[derive(Clone)]
pub struct Environment {
    ctxs: VecDeque<Context>,
    pub num: usize,
}

impl Environment {
    pub fn new() -> Self {
        Self { ctxs: VecDeque::new(), num: 0 }
    }

    pub fn from(other: &Environment) -> Self {
        Self { ctxs: other.ctxs.clone(), num: other.num }
    }

    pub fn push_ctx(&mut self, ctx: &Context) {
        self.ctxs.push_back(ctx.clone());
    }

    pub fn pop_ctx(&mut self) {
        self.ctxs.pop_back();
    }

    pub fn get(&self, name: &str) -> Option<&PolyType> {
        for ctx in self.ctxs.iter().rev() {
            if let Some(pt) = ctx.get(name) {
                return Some(pt);
            }
        }
        None
    }

    pub fn generate_type_var(&mut self) -> MonoType {
        // '\u{03B1}'
        let letter = ((self.num % 26) as u8 + 'a' as u8) as char;
        let num = self.num / 26;

        self.num += 1;
        if num > 0 {
            MonoType::Var(format!("{}{}", letter, num))
        }
        else {
            MonoType::Var(format!("{}", letter))
        }
    }
}

impl Substitutable for Environment {
    fn apply(&self, sub: &Substitution) -> Self {
        Self {
            ctxs: self.ctxs.clone().into_iter().map(| el | el.apply(sub)).collect(),
            num: self.num
        }
    }
}

impl FreeVars for Environment {
    fn free_vars(&self) -> HashSet<String> {
        self.ctxs.iter().flat_map(| ctx | ctx.free_vars()).collect()
    }
}

#[derive(Clone)]
pub struct DeclarationInformation {
    val: Value,
    ty: MonoType,
    sub: Substitution,
    env: Environment,
}

impl DeclarationInformation {
    pub fn new(val: &Value, ty: &MonoType, sub: &Substitution, env: &Environment) -> Self {
        Self { val: val.clone(), ty: ty.clone(), sub: sub.clone(), env: env.clone() }
    }
}

impl fmt::Display for DeclarationInformation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // write!(f, "value: {}\ntype: {}", self.val, self.ty.simplify(&mut HashMap::new()))
        write!(f, "value: {}\ntype: {}", self.val, self.ty)
    }
}


// NAMESPACE

pub struct Namespace {
    declarations: HashMap<String, DeclarationInformation>,
    // envs: VecDeque<Environment>,
}

impl Namespace {
    pub fn new() -> Self {
        Self { declarations: HashMap::new() }
    }

    pub fn add_decl(&mut self, name: &str, decl_info: DeclarationInformation) {
        self.declarations.insert(name.to_string(), decl_info);
    }

    pub fn get_decl(&self, name: &str) -> Option<&DeclarationInformation> {
        self.declarations.get(name)
    }

    fn ast_lambda_desugar(&mut self, pos: (usize, usize), index: usize, parameters: Vec<String>, body: Rc<Ast>) -> Result<Value, (String, (usize, usize))> {
        if index >= parameters.len() - 1 {
            Ok(Value::Lambda(pos, parameters[parameters.len() - 1].clone(), self.from_ast(body)?.into(), Context::new()))
        }
        else {
            Ok(Value::Lambda(pos, parameters[index].clone(), self.ast_lambda_desugar(pos, index + 1, parameters, body)?.into(), Context::new()))
        }
    }

    fn ast_application_desugar(&mut self, pos: (usize, usize), function: Box<Value>, index: usize, arguments: Vec<Rc<Ast>>) -> Result<Value, (String, (usize, usize))> {
        let ln = arguments.len();

        if index >= ln - 1{
            Ok(Value::Application(pos, function, self.from_ast(arguments[index].clone())?.into()))
        }
        else {
            let rest = self.from_ast(arguments[index].clone())?.into();
            self.ast_application_desugar(pos, Value::Application(pos, function, rest).into(), index + 1, arguments)
        }
    }

    pub fn from_ast(&mut self, ast: Rc<Ast>) -> Result<Value, (String, (usize, usize))> {
        match (*ast).to_owned() {
            Ast::Identifier(pos, name) => Ok(Value::Identifier(pos, name)),
            Ast::Number(pos, num) => Ok(Value::Number(pos, num)),
            Ast::Lambda(pos, parameters, body) =>
                self.ast_lambda_desugar(pos, 0, parameters, body),

            Ast::FunctionApplication(pos, function, arguments) => {
                let temp = self.from_ast(function)?.into();
                self.ast_application_desugar(pos, temp, 0, arguments)
            },

            Ast::Let(pos, name, parameters, var_val, body) => {
                if parameters.len() > 0 {
                    let lambda_var_val = self.ast_lambda_desugar(pos, 0, parameters, var_val)?;
                    Ok(Value::Let(pos, name, lambda_var_val.into(), self.from_ast(body)?.into(), Context::new()))
                }
                else {
                    Ok(Value::Let(pos, name, self.from_ast(var_val)?.into(), self.from_ast(body)?.into(), Context::new()))
                }
            }

            Ast::VariableBinding(pos, name, body) => {
                if self.declarations.contains_key(&name) {
                    Err((format!("Attempting to rebind `{}`", name), pos))
                }
                else {
                    let mut body_val = self.from_ast(body)?;
                    let mut env = Environment::new();
                    let (sub, end_ty) = self.alg_w(&mut body_val, &mut env)?;

                    self.add_decl(
                        &name,
                        DeclarationInformation::new(
                            &body_val.clone(),
                            &end_ty.apply(&sub),
                            &sub,
                            &env
                        ));

                    Ok(body_val)
                }
            }

            Ast::FunctionBinding(pos, name, parameters, body) => {
                if self.declarations.contains_key(&name) {
                    Err((format!("Attempting to rebind `{}`", name), pos))
                }
                else {
                    let mut lambda_val = self.ast_lambda_desugar(pos, 0, parameters, body)?;
                    let mut env = Environment::new();
                    let (sub, end_ty) = self.alg_w(&mut lambda_val, &mut env)?;

                    self.add_decl(
                        &name,
                        DeclarationInformation::new(
                            &lambda_val.clone(),
                            &end_ty.apply(&sub),
                            &sub,
                            &env
                        )
                    );

                    Ok(lambda_val)
                }
            }
        }
    }

    pub fn alg_w(&mut self, val: &mut Value, env: &mut Environment) -> Result<(Substitution, MonoType), (String, (usize, usize))>{
        match val {
            Value::Identifier(pos, name) => {
                match env.clone().get(name) {
                    Some(pt) => Ok((Substitution::new(), pt.instantiate(&mut HashMap::new(), env))),
                    None => {
                        match self.get_decl(name) {
                            Some(decl) =>
                                Ok((Substitution::new(), generalize(env, decl.ty.clone()).instantiate(&mut HashMap::new(), env))),
                            None =>
                                Err((format!("Value `{}` was not found in the current environment or namespace", name), (*pos, *pos)))
                        }
                    }
                }
            }

            Value::Application(pos, function, argument) => {
                let (sub1, tao1) = self.alg_w(function, env)?;

                *env = env.apply(&sub1);

                let (sub2, tao2) = self.alg_w(argument, env)?;

                let beta = env.generate_type_var();
                let sub3 =
                    match unify(&tao1.apply(&sub2), &MonoType::Arrow(tao2.into(), beta.clone().into())) {
                        Ok(sub) => sub,
                        Err(e) => return Err((e, *pos))
                    };

                Ok((sub3.combine(&sub2.combine(&sub1)), beta.apply(&sub3)))
            }

            Value::Lambda(_, parameter, body, ctx) => {
                let beta =  env.generate_type_var();

                ctx.add(parameter, &PolyType::Mono(beta.clone()));
                env.push_ctx(ctx);

                let (sub1, tao1) = self.alg_w(body, env)?;

                env.pop_ctx();

                let retrty = MonoType::Arrow(beta.into(), tao1.into()).apply(&sub1);
                Ok((sub1, retrty))
            }

            Value::Let(_, var_name, var_val, body, ctx) => {
                let (sub1, tao1) = self.alg_w(var_val, env)?;

                *env = env.apply(&sub1);

                ctx.add(var_name, &generalize(env, tao1));
                env.push_ctx(ctx);

                let (sub2, tao2) = self.alg_w(body, env)?;

                env.pop_ctx();

                Ok((sub2.combine(&sub1), tao2))
            }

            Value::Number(_, _) => Ok((Substitution::new(), MonoType::Num)),

            Value::Empty => unimplemented!()
        }
    }

    // pub fn alg_m(&mut self, val: &mut Value, env: &mut Environment, ty: &MonoType) -> Result<Substitution, (String, (usize, usize))> {
    //     match val {
    //         Value::Identifier(pos, name) => {
    //             match env.clone().get(name) {
    //                 Some(pt) =>
    //                     match unify(ty, &pt.instantiate(&mut HashMap::new(), env)) {
    //                         Ok(sub) => Ok(sub),
    //                         Err(e) => Err((e, (*pos, *pos)))
    //                     },

    //                 None => match self.get_decl(name) {
    //                     Some(decl) =>
    //                         match unify(ty, &generalize(env, decl.ty.clone()).instantiate(&mut HashMap::new(), env)) {
    //                             Ok(sub) => Ok(sub),
    //                             Err(e) => Err((e, (*pos, *pos)))
    //                         }
    //                     None =>
    //                         Err((format!("Value `{}` was not found in the current environment or namespace", name), (*pos, *pos)))
    //                 }
    //             }
    //         }

    //         Value::Application(_, function, argument) => {
    //             let beta = env.generate_type_var();

    //             let sub1 = self.alg_m(function, env, &MonoType::Arrow(beta.clone().into(), ty.clone().into()))?;

    //             *env = env.apply(&sub1);

    //             let sub2 = self.alg_m(argument, env, &beta.apply(&sub1))?;


    //             Ok(sub2.combine(&sub1))
    //         }


    //         Value::Lambda(pos, parameter, body, ctx) => {
    //             let beta1 = env.generate_type_var();
    //             let beta2 = env.generate_type_var();

    //             let sub1 =
    //                 match unify(ty, &MonoType::Arrow(beta1.clone().into(), beta2.clone().into())) {
    //                     Ok(sub) => sub,
    //                     Err(e) => return Err((e, *pos))
    //                 };

    //             *env = env.apply(&sub1);

    //             ctx.add(parameter, &PolyType::Mono(beta1.apply(&sub1)));
    //             env.push_ctx(ctx);

    //             let sub2 = self.alg_m(body, env, &beta2.apply(&sub1))?;

    //             env.pop_ctx();

    //             Ok(sub2.combine(&sub1))
    //         }

    //         Value::Let(_, var_name, var_val, body, ctx) => {
    //             let beta = env.generate_type_var();

    //             let sub1 = self.alg_m(var_val, env, &beta)?;

    //             *env = env.apply(&sub1);

    //             ctx.add(&var_name, &generalize(env, beta.apply(&sub1)));
    //             env.push_ctx(ctx);

    //             let sub2 = self.alg_m(body, env, &ty.apply(&sub1))?;

    //             env.pop_ctx();

    //             Ok(sub2.combine(&sub1))
    //         }

    //         Value::Number(pos, _) => {
    //             match unify(ty, &MonoType::Num) {
    //                 Ok(res) => Ok(res),
    //                 Err(e) => Err((e, (*pos, *pos)))
    //             }
    //         }

    //         Value::Empty => unimplemented!()
    //     }
    // }

}


// SUBSTITUTIONS

#[derive(Clone)]
pub struct Substitution {
    raw: HashMap<String, MonoType>,
}

impl Substitution {
    pub fn new() -> Self {
        Self { raw: HashMap::new() }
    }

    pub fn add(&mut self, name: &str, ty: &MonoType) {
        self.raw.insert(name.to_string(), ty.clone());
    }

    pub fn get(&self, string: &str) -> Option<&MonoType> {
        self.raw.get(string)
    }

    pub fn combine(&self, other: &Substitution) -> Substitution {
        Substitution { raw:
            self.raw.clone()
            .into_iter()
            .chain(
                other.raw.clone()
                .into_iter()
                .map(| (k, v) | (k, v.apply(self))))
            .collect()
        }
    }
}

impl fmt::Display for Substitution {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut retr = String::new();

        for (pos, (name, ty)) in self.raw.clone().iter().enumerate() {
            retr.push_str(&format!("{} |-> {}", name, ty));
            if pos < self.raw.len() - 1 {
                retr.push('\n');
            }
        }

        write!(f, "{}", retr)
    }
}


// MONOTYPES

#[derive(Clone, PartialEq, Eq)]
pub enum MonoType {
    Num, Bool,
    Var(String),
    Arrow(Box<MonoType>, Box<MonoType>),
}

impl MonoType {
    pub fn simplify(&self, used: &mut HashMap<String, String>) -> MonoType {
        match self {
            MonoType::Arrow(from, to) => MonoType::Arrow(from.simplify(used).into(), to.simplify(used).into()),
            MonoType::Var(name) => {
                match used.get(name) {
                    Some(s) => MonoType::Var(s.clone()),
                    None => {
                        let mut gen_num: usize = 0;
                        loop {
                            let letter = ((gen_num % 26) as u8 + 'a' as u8) as char;
                            let num = gen_num / 26;

                            let total_str = if num > 0 { format!("{}{}", letter, num) } else { format!("{}", letter) };

                            let mut found = false;
                            for (_, val) in used.iter() {
                                if *val == total_str {
                                    gen_num += 1;
                                    found = true;
                                    break;
                                }
                            }

                            if !found {
                                used.insert(name.clone(), total_str.clone());
                                return MonoType::Var(total_str);
                            }
                        }
                    }
                }

            }
            _ => { self.clone() }
        }
    }
}

impl Substitutable for MonoType {
    fn apply(&self, sub: &Substitution) -> Self {
        match self {
            MonoType::Var(tyv) => {
                match sub.get(tyv) {
                    Some(mono) => mono.clone(),
                    None => self.clone(),
                }
            }
            MonoType::Arrow(from, to) => {
                MonoType::Arrow(from.apply(sub).into(), to.apply(sub).into())
            }
            _ => self.clone()
        }
    }
}

impl Instantiable for MonoType {
    fn instantiate(&self, mappings: &mut HashMap<String, MonoType>, env: &mut Environment) -> MonoType {
        match self {
            MonoType::Var(tyv) => {
                match mappings.get(tyv) {
                    Some(mono) => mono.clone(),
                    None => self.clone()
                }
            }
            MonoType::Arrow(from, to) => {
                MonoType::Arrow(from.instantiate(mappings, env).into(), to.instantiate(mappings, env).into())
            },
            _ => self.clone()
        }
    }
}

impl FreeVars for MonoType {
    fn free_vars(&self) -> HashSet<String> {
        match self {
            MonoType::Var(tyv) => [tyv.clone()].into(),
            MonoType::Arrow(from, to) =>
                from.free_vars().into_iter().chain(to.free_vars()).collect(),
            _ => Default::default()
        }
    }
}

impl fmt::Display for MonoType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MonoType::Var(tyv) => write!(f, "{}", tyv),
            MonoType::Arrow(from, to) => write!(f, "{} -> {}", from, to),
            MonoType::Num => write!(f, "Num"),
            MonoType::Bool => write!(f, "Bool"),
        }
    }
}


// POLYTYPES

#[derive(Clone)]
pub enum PolyType {
    Mono(MonoType),
    Quantifier(String, Box<PolyType>)
}

impl Substitutable for PolyType {
    fn apply(&self, sub: &Substitution) -> Self {
        match self {
            PolyType::Mono(_) => self.clone(),
            PolyType::Quantifier(alpha, sigma) => PolyType::Quantifier(alpha.clone(), sigma.apply(sub).into())
        }
    }
}

impl Instantiable for PolyType {
    fn instantiate(&self, mappings: &mut HashMap<String, MonoType>, env: &mut Environment) -> MonoType {
        match self {
            PolyType::Mono(mono) => mono.instantiate(mappings, env),
            PolyType::Quantifier(alpha, sigma) => {
                mappings.insert(alpha.clone(), env.generate_type_var());
                sigma.instantiate(mappings, env)
            }
        }
    }
}

impl FreeVars for PolyType {
    fn free_vars(&self) -> HashSet<String> {
        match self {
            PolyType::Mono(mono) => mono.free_vars(),
            PolyType::Quantifier(alpha, sigma) =>
                sigma.free_vars().into_iter().filter(| v | *v != *alpha).collect()
        }
    }
}

impl fmt::Display for PolyType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PolyType::Mono(mono) => write!(f, "{}", mono),
            PolyType::Quantifier(alpha, sigma) => write!(f, "∀{}. {}", alpha, sigma)
        }
    }
}


// Helper functions for HM

pub fn generalize(env: &Environment, mono: MonoType) -> PolyType {
    mono
    .free_vars()
    .difference(&env.free_vars())
    .fold(
        PolyType::Mono(mono),
        | acc, el | PolyType::Quantifier(el.clone(), acc.into())
    )
}

fn contains(ty1: MonoType, ty2var: &str) -> bool {
    match ty1 {
        MonoType::Var(ty1var) => ty1var == ty2var,
        MonoType::Arrow(from, to) =>
            contains(*from, ty2var) || contains(*to, ty2var),
        _ => false
    }
}

pub fn unify(ty1: &MonoType, ty2: &MonoType) -> Result<Substitution, String> {
    match ty1 {
        MonoType::Var(ty1tyv) => {
            if ty1 == ty2 { Ok(Substitution::new()) }
            else if contains(ty2.clone(), &ty1tyv) {
                Err(format!("Infinite type created {} {}", ty1, ty2))
            }
            else {
                let mut retr = Substitution::new();
                retr.add(ty1tyv, ty2);
                Ok(retr)
            }
        }
        MonoType::Arrow(fromty1, toty1) => {
            match ty2 {
                MonoType::Var(_) => unify(ty2, ty1),
                MonoType::Arrow(fromty2, toty2) => {
                    let mut s = Substitution::new();
                    s = unify(&fromty1.apply(&s), &fromty2.apply(&s))?.combine(&s);
                    s = unify(&toty1.apply(&s), &toty2.apply(&s))?.combine(&s);

                    Ok(s)
                }
                _ => Err(format!("Expected type `{}`, but got type `{}`", ty1, ty2))
            }
        }
        _ => {
            match ty2 {
                MonoType::Var(_) => unify(ty2, ty1),
                _ => {
                    if ty1 == ty2 { Ok(Substitution::new()) }
                    else {
                        Err(format!("Expected type `{}`, but got type `{}`", ty1, ty2))
                    }
                }
            }
        }
    }
}


// TRAITS

pub trait Substitutable {
    fn apply(&self, sub: &Substitution) -> Self;
}

pub trait Instantiable {
    fn instantiate(&self, mappings: &mut HashMap<String, MonoType>, env: &mut Environment) -> MonoType;
}

pub trait FreeVars {
    fn free_vars(&self) -> HashSet<String>;
}