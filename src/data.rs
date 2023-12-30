use std::{collections::{HashMap, HashSet, VecDeque}, fmt, rc::Rc};

use crate::parser::Ast;


// VALUES

#[derive(Clone)]
pub enum Value {
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

    pub fn from(other: &Context) -> Self {
        Self { bindings: other.bindings.clone() }
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

    pub fn add(&mut self, name: &str, ty: &PolyType) {
        match self.ctxs.back_mut() {
            Some(ctx) => ctx.add(name, ty),
            None => {
                let mut new_ctx = Context::new();
                new_ctx.add(name, ty);
                self.push_ctx(&new_ctx);
            }
        }
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
        write!(f, "value: {}, type: {}", self.val, self.ty)
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

            Ast::Let(pos, name, var_val, body) =>
                Ok(Value::Let(pos, name, self.from_ast(var_val)?.into(), self.from_ast(body)?.into(), Context::new())),

            Ast::VariableBinding(_, name, body) => {
                let mut body_val = self.from_ast(body)?;
                let mut env = Environment::new();
                let end_ty = env.generate_type_var();
                let sub = self.alg_m(&mut body_val, &mut env, &end_ty)?;

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

            Ast::FunctionBinding(pos, name, parameters, body) => {
                let mut lambda_val = self.ast_lambda_desugar(pos, 0, parameters, body)?;
                let mut env = Environment::new();
                let end_ty = env.generate_type_var();
                let sub = self.alg_m(&mut lambda_val, &mut env, &end_ty)?;

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

    pub fn alg_m(&mut self, val: &mut Value, env: &mut Environment, ty: &MonoType) -> Result<Substitution, (String, (usize, usize))> {
        match val {
            Value::Identifier(pos, name) => {
                // try to get name of variable from environment
                match env.clone().get(name) {
                    Some(pt) => {
                        // now, we try to unify that name's associated polytype (instantiated -> monotype) with the passed-in monotype
                        match unify(ty, &pt.instantiate(&mut HashMap::new(), env)) {
                            Ok(res) => Ok(res),
                            Err(e) => Err((e, (*pos, *pos)))
                        }
                    }
                    None => {
                        // look in the namespace list of declarations
                        match self.declarations.get(name) {
                            Some(decl) => {
                                match unify(ty, &generalize(env, decl.ty.clone()).instantiate(&mut HashMap::new(), env)) {
                                    Ok(res) => Ok(res),
                                    Err(e) => Err((e, (*pos, *pos)))
                                }
                            }
                            // name not found
                            None => Err((format!("Value `{}` was not found in the current namespace or environment", name), (*pos, *pos)))
                        }
                    }
                }
            }

            Value::Number(pos, _) => {
                match unify(ty, &MonoType::Num) {
                    Ok(res) => Ok(res),
                    Err(e) => Err((e, (*pos, *pos)))
                }
            }

            Value::Lambda(pos, parameter, body, ctx) => {
                let beta1 = env.generate_type_var();
                let beta2 = env.generate_type_var();

                // we try to unify the inputed monotype with the newly generated function application type "a -> b"
                // in order to create subsitution one
                let sub1 = match unify(ty, &MonoType::Arrow(beta1.clone().into(), beta2.clone().into())) {
                    Ok(res) => res,
                    Err(e) => return Err((e, *pos))
                };

                // next, we populate the lambda's context (because we haven't done so already
                // - (for now) I find it easier to populate right here in one step rather than seperating it out into multiple)
                ctx.add(parameter, &PolyType::Mono(beta1.apply(&sub1)));

                // add the lambda context to the environment
                env.push_ctx(ctx);

                // get the substitution for the type of the lambda body (beta2)
                let sub2 = self.alg_m(body, env, &beta2.apply(&sub1))?;

                // pop the lambda's context from the environment
                env.pop_ctx();

                // finally, return sub2 (lambda body) combined with sub1 (lambda parameter)
                Ok(sub2.combine(&sub1))
            }
            Value::Application(_, function, argument) => {
                let beta = env.generate_type_var();

                // beta -> input type
                let sub1 = self.alg_m(function, env, &MonoType::Arrow(beta.clone().into(), ty.clone().into()))?;

                // create a new environment and apply sub1 to it. the reason we don't just directly pass this into sub2 as the env is
                // because of an annoying problem with the typevar generation. Because constraining and environment with a substitution creates a
                // fresh new environment (albiet, with the same generator number as the original environment), when we pass that environment into the alg_m function,
                // we need to then grab the generation number at the end and use it as the original environment's generation number
                let mut new_env = Environment::from(env).apply(&sub1);

                let sub2 = self.alg_m(argument, &mut new_env, &beta.apply(&sub1))?;

                // see above comment
                env.num = new_env.num;

                Ok(sub2.combine(&sub1))
            }
            Value::Let(_, name, var_val, body, ctx) => {
                let beta = env.generate_type_var();

                let sub1 = self.alg_m(var_val, env, &beta)?;

                // this new environment is simply for generalization
                // it doesn't need to mutable because generalize doesn't need a mutable environment (it simply gets the free variables in an environment)
                // so we don't need to do env.num = new_env_1.num :)
                let new_env_1 = Environment::from(env).apply(&sub1);

                ctx.add(name, &generalize(&new_env_1, beta.apply(&sub1)));
                env.push_ctx(ctx);

                let mut new_env_2 = Environment::from(env).apply(&sub1);

                let sub2 = self.alg_m(body, &mut new_env_2, &ty.apply(&sub1))?;

                env.pop_ctx();

                env.num = new_env_2.num;

                Ok(sub2.combine(&sub1))
            }
        }
    }
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