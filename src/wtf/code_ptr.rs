use std::marker::PhantomData;

pub struct CodePtr<PtrTag> {
    value: *mut u8,
    marker: PhantomData<PtrTag>,
}

impl<PtrTag> CodePtr<PtrTag> {
    pub fn new(value: *mut u8) -> Self {
        CodePtr {
            value,
            marker: PhantomData,
        }
    }

    pub fn get(&self) -> *mut u8 {
        self.value
    }

    pub fn set(&mut self, value: *mut u8) {
        self.value = value;
    }

    pub fn as_usize(&self) -> usize {
        self.value as usize
    }

    pub fn from_usize(value: usize) -> Self {
        CodePtr {
            value: value as *mut u8,
            marker: PhantomData,
        }
    }

    pub fn as_ptr(&self) -> *const u8 {
        self.value
    }

    pub fn as_mut_ptr(&mut self) -> *mut u8 {
        self.value
    }

    pub fn as_ptr_tag(&self) -> *const PtrTag {
        self.value as *const PtrTag
    }

    pub fn as_mut_ptr_tag(&mut self) -> *mut PtrTag {
        self.value as *mut PtrTag
    }

    pub fn is_null(&self) -> bool {
        self.value.is_null()
    }

    pub fn is_not_null(&self) -> bool {
        !self.value.is_null()
    }

    pub fn data_location(&self) -> *mut u8 {
        self.value
    }
}

impl<PtrTag> Clone for CodePtr<PtrTag> {
    fn clone(&self) -> Self {
        CodePtr {
            value: self.value,
            marker: PhantomData,
        }
    }
}

impl<PtrTag> Copy for CodePtr<PtrTag> {}

impl<PtrTag> PartialEq for CodePtr<PtrTag> {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

impl<PtrTag> Eq for CodePtr<PtrTag> {}

impl<PtrTag> std::fmt::Debug for CodePtr<PtrTag> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "CodePtr({:p})", self.value)
    }
}

impl<PtrTag> std::fmt::Pointer for CodePtr<PtrTag> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:p}", self.value)
    }
}

impl<PtrTag> std::hash::Hash for CodePtr<PtrTag> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.value.hash(state);
    }
}

impl<PtrTag> std::ops::Add<usize> for CodePtr<PtrTag> {
    type Output = Self;

    fn add(self, rhs: usize) -> Self::Output {
        CodePtr {
            value: unsafe { self.value.offset(rhs as isize) },
            marker: PhantomData,
        }
    }
}

impl<PtrTag> std::ops::AddAssign<usize> for CodePtr<PtrTag> {
    fn add_assign(&mut self, rhs: usize) {
        self.value = unsafe { self.value.offset(rhs as isize) };
    }
}

impl<PtrTag> std::ops::Sub<usize> for CodePtr<PtrTag> {
    type Output = Self;

    fn sub(self, rhs: usize) -> Self::Output {
        CodePtr {
            value: unsafe { self.value.offset(-(rhs as isize)) },
            marker: PhantomData,
        }
    }
}

impl<PtrTag> std::ops::SubAssign<usize> for CodePtr<PtrTag> {
    fn sub_assign(&mut self, rhs: usize) {
        self.value = unsafe { self.value.offset(-(rhs as isize)) };
    }
}

impl<PtrTag> std::ops::Sub<CodePtr<PtrTag>> for CodePtr<PtrTag> {
    type Output = usize;

    fn sub(self, rhs: CodePtr<PtrTag>) -> Self::Output {
        (self.value as usize).wrapping_sub(rhs.value as usize)
    }
}

impl<PtrTag> std::ops::Add<CodePtr<PtrTag>> for CodePtr<PtrTag> {
    type Output = CodePtr<PtrTag>;

    fn add(self, rhs: CodePtr<PtrTag>) -> Self::Output {
        CodePtr {
            value: unsafe { self.value.offset(rhs.value as isize) },
            marker: PhantomData,
        }
    }
}

impl<PtrTag> std::ops::AddAssign<CodePtr<PtrTag>> for CodePtr<PtrTag> {
    fn add_assign(&mut self, rhs: CodePtr<PtrTag>) {
        self.value = unsafe { self.value.offset(rhs.value as isize) };
    }
}


impl<PtrTag> std::ops::SubAssign<CodePtr<PtrTag>> for CodePtr<PtrTag> {
    fn sub_assign(&mut self, rhs: CodePtr<PtrTag>) {
        self.value = unsafe { self.value.offset(-(rhs.value as isize)) };
    }
}

impl<PtrTag> std::ops::Add<&CodePtr<PtrTag>> for CodePtr<PtrTag> {
    type Output = CodePtr<PtrTag>;

    fn add(self, rhs: &CodePtr<PtrTag>) -> Self::Output {
        CodePtr {
            value: unsafe { self.value.offset(rhs.value as isize) },
            marker: PhantomData,
        }
    }
}

impl<PtrTag> std::ops::AddAssign<&CodePtr<PtrTag>> for CodePtr<PtrTag> {
    fn add_assign(&mut self, rhs: &CodePtr<PtrTag>) {
        self.value = unsafe { self.value.offset(rhs.value as isize) };
    }
}

