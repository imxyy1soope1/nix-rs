pub use proc_macros::*;

#[macro_export]
macro_rules! ir {
    ($(
        $(#[$($x:tt)*])*
        $ty:ident
        =>
        {$($name:ident : $elemtype:ty),*$(,)?}
    ),*$(,)?) => {
        #[derive(Clone, Debug)]
        pub enum Ir {
            $(
                $ty($ty),
            )*
        }

        impl Ir {
            fn boxed(self) -> Box<Self> {
                Box::new(self)
            }
            fn ok(self) -> Result<Self> {
                Ok(self)
            }
        }

        impl Compile for Ir {
            fn compile(self, state: &mut CompileState) {
                match self {
                    $(Ir::$ty(ir) => ir.compile(state),)*
                }
            }
        }

        trait Downcast<T> {
            fn downcast_ref(&self) -> Option<&T>;
            fn downcast_mut(&mut self) -> Option<&mut T>;
        }

        $(
            $(
                #[$($x)*]
            )*
            #[derive(Clone, Debug)]
            pub struct $ty {
                $(
                    pub $name : $elemtype,
                )*
            }

            impl $ty {
                pub fn ir(self) -> Ir {
                    Ir::$ty(self)
                }
            }

            impl TryFrom<Ir> for $ty {
                type Error = anyhow::Error;
                fn try_from(value: Ir) -> Result<Self> {
                    match value {
                        Ir::$ty(value) => Ok(value),
                        _ => Err(anyhow!("")),
                    }
                }
            }

            impl Downcast<$ty> for Ir {
                fn downcast_ref(&self) -> Option<&$ty> {
                    match self {
                        Ir::$ty(value) => Some(value),
                        _ => None,
                    }
                }
                fn downcast_mut(&mut self) -> Option<&mut $ty> {
                    match self {
                        Ir::$ty(value) => Some(value),
                        _ => None,
                    }
                }
            }
        )*
    }
}

