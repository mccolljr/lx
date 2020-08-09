use bincode;
use path_clean::PathClean;
use pathdiff;
use serde::{
    Deserialize,
    Serialize,
};

use crate::ast::{
    File,
    Scope,
};
use crate::check::Checker;
use crate::compiler::compile;
use crate::error::SyntaxError;
use crate::parser::Parser;
use crate::runtime::inst::Inst;
use crate::source::Code;

use std::collections::HashMap;
use std::fs::read_to_string;
use std::path::{
    Path,
    PathBuf,
};
use std::rc::Rc;

#[derive(Debug)]
pub struct Context {
    global:    Rc<Scope>,
    files:     HashMap<String, File>,
    importing: Vec<String>,
}

impl Context {
    fn new(globals: Vec<String>) -> Self {
        Context {
            global:    Scope::global(globals),
            files:     HashMap::new(),
            importing: Vec::new(),
        }
    }

    fn resolve(&mut self, path: impl AsRef<Path>) -> Option<PathBuf> {
        let import_path = path.as_ref();
        let working_dir = std::env::current_dir()
            .expect("PANIC: unable to access working directory");
        let current_path = self
            .importing
            .last()
            .map_or(working_dir.as_ref(), |v| Path::new(v).parent().unwrap());
        let resolved_path =
            working_dir.join(current_path).join(import_path).clean();
        return pathdiff::diff_paths(resolved_path, working_dir);
    }

    fn import(&mut self, path: &str) -> Result<String, SyntaxError> {
        // get the string of the full path
        let full_path = self
            .resolve(path)
            .ok_or(SyntaxError::UnresolvedImport { path: path.into() })?
            .to_string_lossy()
            .to_string();

        // disallow circular import chains
        if self.importing.contains(&full_path) {
            return Err(SyntaxError::CircularImport {
                path: format!(
                    "{} -> {}",
                    self.importing.join(" -> "),
                    full_path
                ),
            });
        }

        // don't re-import a file we've already imported
        if self.files.contains_key(&full_path) {
            return Ok(full_path);
        }

        // mark that we're currently importing this file
        self.importing.push(full_path.clone());
        let src = read_to_string(&full_path).map_err(|_| {
            SyntaxError::UnresolvedImport {
                path: full_path.clone(),
            }
        })?;

        // add the file to our list of imports
        let file = Parser::parse_file(
            full_path.clone(),
            &Code::new(full_path.as_ref(), src),
            Some(self.global.clone()),
            &mut |path| self.import(path),
        )?;
        self.files.insert(full_path.clone(), file);

        // mark that we're done importing the file,
        // and return
        self.importing.pop().expect("PANIC: bad import stack");
        return Ok(full_path);
    }

    pub fn compile(
        main_file: &str,
        globals: Vec<String>,
    ) -> Result<Program, SyntaxError> {
        // let start = std::time::Instant::now();
        // get the string of the full path
        let mut ctx = Context::new(globals);
        let full_path = ctx.import(main_file)?;

        match Checker::check_file(ctx.files.get(&full_path).unwrap()) {
            Err(e) => {
                println!("ERROR!: {}", e);
                panic!();
            }
            Ok(_) => {}
        }

        let mut compiled = HashMap::<String, Rc<[Inst]>>::new();
        for (name, file) in ctx.files {
            compiled.insert(name, Rc::from(compile(file.stmts).unwrap()));
        }
        // round trip the program for now so we know our encoding works
        let program: Program = bincode::deserialize(
            &bincode::serialize(&Program {
                files: compiled,
                main:  full_path,
            })
            .expect("unable to serialize bytecode"),
        )
        .expect("unable to deserialize bytecode");
        // let done = std::time::Instant::now();
        // println!(
        //     "compiled {:?} in {:?}",
        //     program.files.keys().collect::<Vec<&String>>(),
        //     done - start
        // );
        return Ok(program);
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Program {
    files: HashMap<String, Rc<[Inst]>>,
    main:  String,
}

impl Program {
    pub fn main(&self) -> Rc<[Inst]> { self.file(self.main.clone()) }

    pub fn file(&self, name: String) -> Rc<[Inst]> {
        Rc::clone(self.files.get(&name).expect("FATAL: bad compilation"))
    }
}
