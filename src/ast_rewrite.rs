use crate::ast::{
    Expr,
    Node,
    ObjField,
    ObjKey,
};
use crate::runtime::value::{
    Array,
    Object,
    Value,
};
use crate::source::Pos;

fn const_val(x: &Expr) -> Option<Value> {
    match x {
        Expr::LitNull { .. } => Some(Value::Null),
        Expr::LitInt { val, .. } => Some(Value::Int(*val)),
        Expr::LitFlt { val, .. } => Some(Value::Flt(*val)),
        Expr::LitStr { val, .. } => Some(Value::Str(val.clone())),
        Expr::LitBool { val, .. } => Some(Value::Bool(*val)),
        Expr::LitArr { elements, .. } => {
            let arr = Array::new();
            for elt in elements.iter() {
                arr.push_back(const_val(elt)?);
            }
            Some(Value::Array(arr))
        }
        Expr::LitObj { fields, .. } => {
            let obj = Object::new();
            for field in fields.iter() {
                let field_val = const_val(field.val.as_ref())?;
                match &field.key {
                    ObjKey::Static(val, ..) => {
                        obj.index_set(val.clone(), field_val);
                    }
                    ObjKey::Dynamic(expr) => {
                        obj.index_set(
                            const_val(expr.as_ref())?.to_string(),
                            field_val,
                        );
                    }
                }
            }
            Some(Value::Object(obj))
        }
        Expr::Unary { expr, op_typ, .. } => {
            Value::op_unary(&const_val(expr.as_ref())?, *op_typ)
                .map_or(None, |v| Some(v))
        }
        Expr::Binary {
            lhs, rhs, op_typ, ..
        } => {
            Value::op_binary(
                &const_val(lhs.as_ref())?,
                &const_val(rhs.as_ref())?,
                *op_typ,
            )
            .map_or(None, |v| Some(v))
        }
        Expr::Ternary {
            cond, pass, fail, ..
        } => {
            match const_truthy(cond.as_ref()) {
                Some(truthy) => {
                    if truthy {
                        const_val(pass.as_ref())
                    } else {
                        const_val(fail.as_ref())
                    }
                }
                None => None,
            }
        }
        Expr::Selector { expr, selector, .. } => {
            const_val(expr.as_ref())?
                .op_index(&Value::Str(selector.name.clone()))
                .map_or(None, |v| Some(v))
        }
        Expr::Index { expr, index, .. } => {
            const_val(expr.as_ref())?
                .op_index(&const_val(index.as_ref())?)
                .map_or(None, |v| Some(v))
        }
        Expr::Paren { expr, .. } => const_val(expr.as_ref()),
        Expr::Call { .. } => None,
        Expr::Ident { .. } => None,
        Expr::LitFunc { .. } => None,
    }
}

pub fn const_truthy(x: &Expr) -> Option<bool> {
    match x {
        Expr::LitFunc { .. } => Some(true),
        Expr::LitArr { elements, .. } => Some(elements.len() != 0),
        Expr::LitObj { fields, .. } => Some(fields.len() != 0),
        _ => Some(const_val(x)?.truthy()),
    }
}

pub fn simplify_expr(x: Expr) -> Expr {
    let orig_pos = x.pos();
    match x {
        Expr::LitNull { .. }
        | Expr::LitInt { .. }
        | Expr::LitFlt { .. }
        | Expr::LitBool { .. }
        | Expr::LitStr { .. }
        | Expr::LitFunc { .. }
        | Expr::Ident { .. }
        | Expr::Selector { .. } => x,
        Expr::LitArr {
            elements,
            osquare,
            csquare,
        } => {
            Expr::LitArr {
                osquare,
                elements: elements.into_iter().map(simplify_expr).collect(),
                csquare,
            }
        }
        Expr::LitObj {
            fields,
            obrace,
            cbrace,
        } => {
            Expr::LitObj {
                obrace,
                fields: fields
                    .into_iter()
                    .map(|mut f| {
                        if let ObjKey::Dynamic(ref mut expr) = f.key {
                            **expr = simplify_expr((**expr).clone());
                        }
                        *f.val = simplify_expr(*f.val);
                        f
                    })
                    .collect(),
                cbrace,
            }
        }
        Expr::Paren {
            mut expr,
            oparen,
            cparen,
        } => {
            *expr = simplify_expr(*expr);
            Expr::Paren {
                expr,
                oparen,
                cparen,
            }
        }
        Expr::Unary {
            mut expr,
            op_typ,
            op_pos,
        } => {
            *expr = simplify_expr(*expr);
            let base = Expr::Unary {
                expr,
                op_pos,
                op_typ,
            };
            let result = const_val(&base);
            if result.is_none() {
                return base;
            }

            match value_to_expr(result.unwrap(), orig_pos) {
                Some(expr) => expr,
                None => base,
            }
        }
        Expr::Binary {
            mut rhs,
            mut lhs,
            op_pos,
            op_typ,
        } => {
            *lhs = simplify_expr(*lhs);
            *rhs = simplify_expr(*rhs);
            let base = Expr::Binary {
                lhs,
                rhs,
                op_typ,
                op_pos,
            };
            let result = const_val(&base);
            if result.is_none() {
                return base;
            }

            match value_to_expr(result.unwrap(), orig_pos) {
                Some(expr) => expr,
                None => base,
            }
        }
        Expr::Ternary {
            mut cond,
            question,
            mut pass,
            colon,
            mut fail,
        } => {
            *cond = simplify_expr(*cond);
            *pass = simplify_expr(*pass);
            *fail = simplify_expr(*fail);
            let base = Expr::Ternary {
                cond,
                pass,
                fail,
                question,
                colon,
            };
            let result = const_val(&base);
            if result.is_none() {
                return base;
            }

            match value_to_expr(result.unwrap(), orig_pos) {
                Some(expr) => expr,
                None => base,
            }
        }
        Expr::Index {
            mut expr,
            osquare,
            mut index,
            csquare,
        } => {
            *expr = simplify_expr(*expr);
            *index = simplify_expr(*index);
            let base = Expr::Index {
                expr,
                osquare,
                index,
                csquare,
            };
            let result = const_val(&base);
            if result.is_none() {
                return base;
            }

            match value_to_expr(result.unwrap(), orig_pos) {
                Some(expr) => expr,
                None => base,
            }
        }
        Expr::Call {
            mut expr,
            oparen,
            args,
            cparen,
        } => {
            *expr = simplify_expr(*expr);
            Expr::Call {
                expr,
                oparen,
                args: args.into_iter().map(simplify_expr).collect(),
                cparen,
            }
        }
    }
}

fn value_to_expr(v: Value, pos: Pos) -> Option<Expr> {
    match v {
        Value::Int(v) => Some(Expr::LitInt { val: v, pos }),
        Value::Flt(v) => Some(Expr::LitFlt { val: v, pos }),
        Value::Str(v) => Some(Expr::LitStr { val: v, pos }),
        Value::Bool(v) => Some(Expr::LitBool { val: v, pos }),
        Value::Array(v) => {
            let mut elements = Vec::<Expr>::with_capacity(v.len());
            let iter = v.value_iter();
            while let Some(ev) = iter.next() {
                elements.push(value_to_expr(ev, pos)?);
            }
            Some(Expr::LitArr {
                osquare: Pos::one(pos.offset),
                elements,
                csquare: Pos::one(pos.offset + pos.length - 1),
            })
        }
        Value::Object(v) => {
            let mut fields = Vec::<ObjField>::with_capacity(v.len());
            for (fk, fv) in v.rust_iter() {
                fields.push(ObjField {
                    key:   ObjKey::Static(fk, pos),
                    colon: pos,
                    val:   Box::new(value_to_expr(fv, pos)?),
                });
            }
            Some(Expr::LitObj {
                obrace: Pos::one(pos.offset),
                fields,
                cbrace: Pos::one(pos.offset + pos.length - 1),
            })
        }
        _ => None,
    }
}
