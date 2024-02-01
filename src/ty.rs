#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Type {
    Int(IntType),
    Ptr,
    Label,
    Void,
}

const _: () = assert!(usize::BITS == u64::BITS);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct IntType(u8);

impl Type {
    /// Size in bytes
    pub const fn size(&self) -> usize {
        (self.width() + 1) / 8
    }

    /// Size in bits
    pub const fn width(&self) -> usize {
        match self {
            Type::Int(IntType(width)) => *width as usize,
            Type::Ptr => 64,
            Type::Label => 0,
            Type::Void => 0,
        }
    }

    pub const fn max(&self) -> u64 {
        match self {
            Type::Int(IntType(width)) => (1 << (*width as u64 - 1)) - 1,
            Type::Ptr => u64::MAX,
            Type::Label => 0,
            Type::Void => 0,
        }
    }

    pub const fn from_width(w: u8) -> Self {
        assert!(w != 0);
        Type::Int(IntType(w))
    }
}

impl std::fmt::Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Int(IntType(l)) => write!(f, "i{l}"),
            Type::Ptr => write!(f, "ptr"),
            Type::Label => write!(f, "label"),
            Type::Void => write!(f, "void"),
        }
    }
}

macro_rules! ty_alias {
    (@inner $var:ident($inner:ident) $bitn:expr, $mk_fn:ident, $is_fn:ident) => {
        pub const fn $mk_fn() -> Self {
            Self::$var($inner($bitn))
        }
        pub const fn $is_fn(&self) -> bool {
            todo!()
            // &Self::$mk_fn() == self
        }
    };
    ($ty:ident::$var:ident($inner:ident) { $($bitn:expr, $mk_fn:ident, $is_fn:ident;)* }) => {
        impl $ty {
            $(ty_alias!(@inner $var($inner) $bitn, $mk_fn, $is_fn);)*
        }
    };
}

ty_alias!(Type::Int(IntType) {
    1, i1, is_i1;
    8, i8, is_i8;
    16, i16, is_i16;
    32, i32, is_i32;
    64, i64, is_i64;
});

macro_rules! ty_unit {
    ($ty:ident {$($var:ident: $mk_fn:ident, $is_fn:ident;)*}) => {
        impl $ty {
            $(ty_unit!(@inner $var, $mk_fn, $is_fn);)*
        }
    };
    (@inner $var:ident, $mk_fn:ident, $is_fn:ident) => {
        pub const fn $mk_fn() -> Self {
            Self::$var
        }
        pub const fn $is_fn(&self) -> bool {
            todo!()
            // &Self::$var == self
        }
    };
}

ty_unit!(Type {
    Void: void, is_void;
    Ptr: ptr, is_ptr;
    Label: label, is_label;
});
