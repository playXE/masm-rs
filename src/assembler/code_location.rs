use std::ops::{Deref, DerefMut};

use crate::wtf::code_ptr::CodePtr;

pub struct CodeLocationCommon<PtrTag> {
    pub value: CodePtr<PtrTag>
}

impl<PtrTag> Deref for CodeLocationCommon<PtrTag> {
    type Target = CodePtr<PtrTag>;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<PtrTag> DerefMut for CodeLocationCommon<PtrTag> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.value
    }
}

impl<PtrTag> CodeLocationCommon<PtrTag> {
    pub fn new(value: CodePtr<PtrTag>) -> Self {
        CodeLocationCommon {
            value
        }
    }

    pub fn instruction_at_offset(&self, offset: isize) -> CodeLocationInstruction<PtrTag> {
        CodeLocationInstruction::new(CodePtr::from_usize(self.value.as_usize() + offset as usize))
    }

    pub fn label_at_offset(&self, offset: isize) -> CodeLocationLabel<PtrTag> {
        CodeLocationLabel::new(CodePtr::from_usize(self.value.as_usize() + offset as usize))
    }

    pub fn jump_at_offset(&self, offset: isize) -> CodeLocationJump<PtrTag> {
        CodeLocationJump::new(CodePtr::from_usize(self.value.as_usize() + offset as usize))
    }
    
    pub fn call_at_offset(&self, offset: isize) -> CodeLocationCall<PtrTag> {
        CodeLocationCall::new(CodePtr::from_usize(self.value.as_usize() + offset as usize))
    }

    pub fn near_call_at_offset(&self, offset: isize) -> CodeLocationNearCall<PtrTag> {
        CodeLocationNearCall::new(CodePtr::from_usize(self.value.as_usize() + offset as usize))
    }

    pub fn data_label_ptr_at_offset(&self, offset: isize) -> CodeLocationDataLabelPtr<PtrTag> {
        CodeLocationDataLabelPtr::new(CodePtr::from_usize(self.value.as_usize() + offset as usize))
    }

    pub fn data_label32_at_offset(&self, offset: isize) -> CodeLocationDataLabel32<PtrTag> {
        CodeLocationDataLabel32::new(CodePtr::from_usize(self.value.as_usize() + offset as usize))
    }

    pub fn convertible_load_at_offset(&self, offset: isize) -> CodeLocationConvertibleLoad<PtrTag> {
        CodeLocationConvertibleLoad::new(CodePtr::from_usize(self.value.as_usize() + offset as usize))
    }
}

pub struct CodeLocationLabel<PtrTag> {
    pub common: CodeLocationCommon<PtrTag>,
}

impl<PtrTag> CodeLocationLabel<PtrTag> {
    pub fn new(value: CodePtr<PtrTag>) -> Self {
        CodeLocationLabel {
            common: CodeLocationCommon::new(value)
        }
    }
}

impl<PtrTag> Deref for CodeLocationLabel<PtrTag> {
    type Target = CodeLocationCommon<PtrTag>;

    fn deref(&self) -> &Self::Target {
        &self.common
    }
}

impl<PtrTag> DerefMut for CodeLocationLabel<PtrTag> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.common
    }
}

pub struct CodeLocationJump<PtrTag> {
    pub common: CodeLocationCommon<PtrTag>,
}

impl<PtrTag> CodeLocationJump<PtrTag> {
    pub fn new(value: CodePtr<PtrTag>) -> Self {
        CodeLocationJump {
            common: CodeLocationCommon::new(value)
        }
    }
}

impl<PtrTag> Deref for CodeLocationJump<PtrTag> {
    type Target = CodeLocationCommon<PtrTag>;

    fn deref(&self) -> &Self::Target {
        &self.common
    }
}

impl<PtrTag> DerefMut for CodeLocationJump<PtrTag> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.common
    }
}

pub struct CodeLocationInstruction<PtrTag> {
    pub common: CodeLocationCommon<PtrTag>,
}

impl<PtrTag> CodeLocationInstruction<PtrTag> {
    pub fn new(value: CodePtr<PtrTag>) -> Self {
        CodeLocationInstruction {
            common: CodeLocationCommon::new(value)
        }
    }
}

impl<PtrTag> Deref for CodeLocationInstruction<PtrTag> {
    type Target = CodeLocationCommon<PtrTag>;

    fn deref(&self) -> &Self::Target {
        &self.common
    }
}

impl<PtrTag> DerefMut for CodeLocationInstruction<PtrTag> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.common
    }
}

pub struct CodeLocationCall<PtrTag> {
    pub common: CodeLocationCommon<PtrTag>,
}

impl<PtrTag> CodeLocationCall<PtrTag> {
    pub fn new(value: CodePtr<PtrTag>) -> Self {
        CodeLocationCall {
            common: CodeLocationCommon::new(value)
        }
    }
}

impl<PtrTag> Deref for CodeLocationCall<PtrTag> {
    type Target = CodeLocationCommon<PtrTag>;

    fn deref(&self) -> &Self::Target {
        &self.common
    }
}

impl<PtrTag> DerefMut for CodeLocationCall<PtrTag> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.common
    }
}

pub struct CodeLocationNearCall<PtrTag> {
    pub common: CodeLocationCommon<PtrTag>,
}

impl<PtrTag> CodeLocationNearCall<PtrTag> {
    pub fn new(value: CodePtr<PtrTag>) -> Self {
        CodeLocationNearCall {
            common: CodeLocationCommon::new(value)
        }
    }
}

impl<PtrTag> Deref for CodeLocationNearCall<PtrTag> {
    type Target = CodeLocationCommon<PtrTag>;

    fn deref(&self) -> &Self::Target {
        &self.common
    }
}

impl<PtrTag> DerefMut for CodeLocationNearCall<PtrTag> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.common
    }
}

pub struct CodeLocationDataLabelPtr<PtrTag> {
    pub common: CodeLocationCommon<PtrTag>,
}

impl<PtrTag> CodeLocationDataLabelPtr<PtrTag> {
    pub fn new(value: CodePtr<PtrTag>) -> Self {
        CodeLocationDataLabelPtr {
            common: CodeLocationCommon::new(value)
        }
    }
}

impl<PtrTag> Deref for CodeLocationDataLabelPtr<PtrTag> {
    type Target = CodeLocationCommon<PtrTag>;

    fn deref(&self) -> &Self::Target {
        &self.common
    }
}

impl<PtrTag> DerefMut for CodeLocationDataLabelPtr<PtrTag> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.common
    }
}

pub struct CodeLocationDataLabel32<PtrTag> {
    pub common: CodeLocationCommon<PtrTag>,
}

impl<PtrTag> CodeLocationDataLabel32<PtrTag> {
    pub fn new(value: CodePtr<PtrTag>) -> Self {
        CodeLocationDataLabel32 {
            common: CodeLocationCommon::new(value)
        }
    }
}

impl<PtrTag> Deref for CodeLocationDataLabel32<PtrTag> {
    type Target = CodeLocationCommon<PtrTag>;

    fn deref(&self) -> &Self::Target {
        &self.common
    }
}

impl<PtrTag> DerefMut for CodeLocationDataLabel32<PtrTag> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.common
    }
}

pub struct CodeLocationConvertibleLoad<PtrTag> {
    pub common: CodeLocationCommon<PtrTag>,
}

impl<PtrTag> CodeLocationConvertibleLoad<PtrTag> {
    pub fn new(value: CodePtr<PtrTag>) -> Self {
        CodeLocationConvertibleLoad {
            common: CodeLocationCommon::new(value)
        }
    }
}

impl<PtrTag> Deref for CodeLocationConvertibleLoad<PtrTag> {
    type Target = CodeLocationCommon<PtrTag>;

    fn deref(&self) -> &Self::Target {
        &self.common
    }
}

impl<PtrTag> DerefMut for CodeLocationConvertibleLoad<PtrTag> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.common
    }
}