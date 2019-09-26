use crate::expr::Expr;
use crate::expr::Type;

pub fn get_type(expr: &Expr) -> Result<Type, String> {
    match expr {
        Expr::Number(_) => Ok(Type::Int),
        Expr::Boolean(_) => Ok(Type::Bool),
        Expr::Epsilon => Ok(Type::Stage),
        // _ => Ok(Type::Bool)
    }
}

