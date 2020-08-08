use serde::{
    Deserialize,
    Serialize,
};

use crate::ast::{
    File,
    Scope,
};
use crate::compiler::compile;
use crate::error::SyntaxError;
use crate::parser::Parser;
use crate::runtime::inst::Inst;
use crate::source::Code;

use std::collections::HashMap;
use std::collections::HashSet;
use std::fs::{
    canonicalize,
    read_to_string,
};
use std::path::PathBuf;
use std::rc::Rc;

#[derive(Debug)]
pub struct Context {
    global:    Rc<Scope>,
    files:     HashMap<String, File>,
    importing: HashSet<String>,
}

impl Context {
    fn new(globals: Vec<String>) -> Self {
        Context {
            global:    Scope::global(globals),
            files:     HashMap::new(),
            importing: HashSet::new(),
        }
    }

    fn import(&mut self, path: String) -> Result<(), SyntaxError> {
        // get the string of the full path
        let full_path = abs_path(path.as_ref())
            .map_err(|_| SyntaxError::FileNotFound { path: path.clone() })?;

        // disallow circular import chains
        if self.importing.contains(&full_path) {
            return Err(SyntaxError::CircularImport { path: full_path });
        }

        // don't re-import a file we've already imported
        if self.files.contains_key(&full_path) {
            return Ok(());
        }

        // mark that we're currently importing this file
        self.importing.insert(full_path.clone());
        let src = read_to_string(&full_path)
            .map_err(|_| SyntaxError::FileNotFound { path })?;

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
        self.importing.remove(&full_path);
        return Ok(());
    }

    pub fn compile(
        main_file: &str,
        globals: Vec<String>,
    ) -> Result<Program, SyntaxError> {
        // get the string of the full path
        let full_path = abs_path(main_file)?;
        let mut ctx = Context::new(globals);
        ctx.import(full_path.clone())?;
        let mut compiled = HashMap::<String, Rc<[Inst]>>::new();
        for (name, file) in ctx.files {
            compiled.insert(name, Rc::from(compile(file.stmts).unwrap()));
        }
        return Ok(Program {
            files: compiled,
            main:  full_path,
        });
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Program {
    files: HashMap<String, Rc<[Inst]>>,
    main:  String,
}

impl Program {
    pub fn main(&self) -> Result<Rc<[Inst]>, SyntaxError> {
        self.file(self.main.clone())
    }

    pub fn file(&self, abs_or_rel: String) -> Result<Rc<[Inst]>, SyntaxError> {
        // get the string of the full path
        let full_path = abs_path(abs_or_rel.as_ref())?;
        match self.files.get(&full_path) {
            Some(insts) => Ok(Rc::clone(insts)),
            None => {
                Err(SyntaxError::FileNotFound {
                    path: full_path.into(),
                })
            }
        }
    }
}

fn abs_path(abs_or_rel: &str) -> Result<String, SyntaxError> {
    // get absolute path of file to import
    let abs_pathbuf =
        canonicalize(PathBuf::from(abs_or_rel)).map_err(|_| {
            SyntaxError::FileNotFound {
                path: abs_or_rel.into(),
            }
        })?;

    // we don't import directories (yet)
    if abs_pathbuf.is_dir() {
        return Err(SyntaxError::FileNotFound {
            path: abs_or_rel.into(),
        });
    }

    // get the string of the full path
    return Ok(abs_pathbuf.to_string_lossy().to_string());
}
