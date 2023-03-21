use std::{ops::{Deref, DerefMut}, borrow::Cow};

use tinyvec::TinyVec;

use super::{buffer::AssemblerLabel, *, link_buffer::LinkBuffer};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
#[repr(u8)]
pub enum Scale {
    TimesOne,
    TimesTwo,
    TimesFour,
    TimesEight,
}

#[allow(non_upper_case_globals)]
impl Scale {
    pub const Ptr: Self = if cfg!(target_pointer_width = "64") {
        Scale::TimesEight
    } else {
        Scale::TimesFour
    };

    pub const RegularWord: Self = if cfg!(target_pointer_width = "64") {
        Scale::TimesEight
    } else {
        Scale::TimesFour
    };
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
#[repr(u8)]
pub enum Extend {
    ZExt32,
    SExt32,
    None,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub struct Address {
    pub base: u8,
    pub offset: i32,
}

impl Address {
    pub const fn new(base: u8, offset: i32) -> Self {
        Self { base, offset }
    }

    pub const fn with_offset(self, offset: i32) -> Self {
        Self {
            offset: self.offset + offset,
            ..self
        }
    }

    pub const fn with_swapped_register(self, left: u8, right: u8) -> Self {
        Self {
            base: with_swapped_register(self.base, left, right),
            ..self
        }
    }

    pub const fn indexed_by(self, index: u8, scale: Scale) -> BaseIndex {
        BaseIndex::new(self.base, index, scale, self.offset, Extend::None)
    }
}

pub const fn with_swapped_register(original: u8, left: u8, right: u8) -> u8 {
    if original == left {
        return right;
    } else if original == right {
        return left;
    } else {
        return original;
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub struct ExtendedAddress {
    pub base: u8,
    pub offset: isize,
}

impl ExtendedAddress {
    pub const fn new(base: u8, offset: isize) -> Self {
        Self { base, offset }
    }

    pub const fn with_offset(self, offset: isize) -> Self {
        Self {
            offset: self.offset + offset,
            ..self
        }
    }

    pub const fn with_swapped_register(self, left: u8, right: u8) -> Self {
        Self {
            base: with_swapped_register(self.base, left, right),
            ..self
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub struct BaseIndex {
    pub base: u8,
    pub index: u8,
    pub scale: Scale,
    pub offset: i32,
    pub extend: Extend,
}

impl BaseIndex {
    pub const fn new(base: u8, index: u8, scale: Scale, offset: i32, extend: Extend) -> Self {
        Self {
            base,
            index,
            scale,
            offset,
            extend,
        }
    }

    pub const fn with_offset(self, offset: i32) -> Self {
        Self {
            offset: self.offset + offset,
            ..self
        }
    }

    pub const fn with_swapped_register(self, left: u8, right: u8) -> Self {
        Self {
            base: with_swapped_register(self.base, left, right),
            index: with_swapped_register(self.index, left, right),
            ..self
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub struct PreIndexAddress {
    pub base: u8,
    pub index: i32,
}

impl PreIndexAddress {
    pub const fn new(base: u8, index: i32) -> Self {
        Self { base, index }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub struct AbsoluteAddress {
    pub ptr: *const u8,
}

impl AbsoluteAddress {
    pub const fn new(ptr: *const u8) -> Self {
        Self { ptr }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub enum Operand {
    Address(Address),
    ExtendedAddress(ExtendedAddress),
    BaseIndex(BaseIndex),
    PreIndexAddress(PreIndexAddress),
    AbsoluteAddress(AbsoluteAddress),
    Register(u8),
    Imm32(i32),
    Imm64(i64),
    ImmPtr(usize),
}

impl From<i32> for Operand {
    fn from(imm: i32) -> Self {
        Self::Imm32(imm)
    }
}

impl From<i64> for Operand {
    fn from(imm: i64) -> Self {
        Self::Imm64(imm)
    }
}

impl From<usize> for Operand {
    fn from(imm: usize) -> Self {
        Self::ImmPtr(imm)
    }
}

impl From<Address> for Operand {
    fn from(address: Address) -> Self {
        Self::Address(address)
    }
}

impl From<ExtendedAddress> for Operand {
    fn from(address: ExtendedAddress) -> Self {
        Self::ExtendedAddress(address)
    }
}

impl From<BaseIndex> for Operand {
    fn from(base_index: BaseIndex) -> Self {
        Self::BaseIndex(base_index)
    }
}

impl From<PreIndexAddress> for Operand {
    fn from(pre_index_address: PreIndexAddress) -> Self {
        Self::PreIndexAddress(pre_index_address)
    }
}

impl From<AbsoluteAddress> for Operand {
    fn from(absolute_address: AbsoluteAddress) -> Self {
        Self::AbsoluteAddress(absolute_address)
    }
}

impl From<u8> for Operand {
    fn from(register: u8) -> Self {
        Self::Register(register)
    }
}

/// Label:
///
/// A Label records a point in the generated instruction stream, typically such that
/// it may be used as a destination for a jump.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub struct Label {
    pub label: AssemblerLabel,
}

impl Label {
    pub fn is_set(&self) -> bool {
        self.label.is_set()
    }

    pub fn new(masm: &mut AbstractMacroAssembler) -> Self {
        Self {
            label: masm.assembler.label(),
        }
    }

    pub const fn unset() -> Self {
        Self {
            label: AssemblerLabel::new(u32::MAX),
        }
    }
}

/// ConvertibleLoadLabel:
///
/// A ConvertibleLoadLabel records a loadPtr instruction that can be patched to an addPtr
/// so that:
///
/// loadPtr(Address(a, i), b)
///
/// becomes:
///
/// addPtr(TrustedImmPtr(i), a, b)
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub struct ConvertibleLoadLabel {
    pub label: AssemblerLabel,
}

impl ConvertibleLoadLabel {
    pub fn new(masm: &mut AbstractMacroAssembler) -> Self {
        Self {
            label: masm.assembler.label_ignoring_watchpoints(),
        }
    }

    pub fn is_set(&self) -> bool {
        self.label.is_set()
    }

    pub const fn unset() -> Self {
        Self {
            label: AssemblerLabel::new(u32::MAX),
        }
    }
}

/// DataLabelPtr:
///
/// A DataLabelPtr is used to refer to a location in the code containing a pointer to be
/// patched after the code has been generated.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub struct DataLabelPtr {
    pub label: AssemblerLabel,
}

impl DataLabelPtr {
    pub fn new(masm: &mut AbstractMacroAssembler) -> Self {
        Self {
            label: masm.assembler.label_ignoring_watchpoints(),
        }
    }

    pub fn is_set(&self) -> bool {
        self.label.is_set()
    }

    pub const fn unset() -> Self {
        Self {
            label: AssemblerLabel::new(u32::MAX),
        }
    }
}

/// DataLabel32:
///
/// A DataLabel32 is used to refer to a location in the code containing a 32-bit constant to be
/// patched after the code has been generated.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub struct DataLabel32 {
    pub label: AssemblerLabel,
}

impl DataLabel32 {
    pub fn new(masm: &mut AbstractMacroAssembler) -> Self {
        Self {
            label: masm.assembler.label_ignoring_watchpoints(),
        }
    }

    pub fn is_set(&self) -> bool {
        self.label.is_set()
    }

    pub const fn unset() -> Self {
        Self {
            label: AssemblerLabel::new(u32::MAX),
        }
    }
}

/// DataLabelCompact:
///
/// A DataLabelCompact is used to refer to a location in the code containing a
/// compact immediate to be patched after the code has been generated.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub struct DataLabelCompact {
    pub label: AssemblerLabel,
}

impl DataLabelCompact {
    pub fn new(masm: &mut AbstractMacroAssembler) -> Self {
        Self {
            label: masm.assembler.label_ignoring_watchpoints(),
        }
    }

    pub fn from_label(label: AssemblerLabel) -> Self {
        Self { label }
    }

    pub fn is_set(&self) -> bool {
        self.label.is_set()
    }

    pub const fn unset() -> Self {
        Self {
            label: AssemblerLabel::new(u32::MAX),
        }
    }
}

/// Call:
///
/// A Call object is a reference to a call instruction that has been planted
/// into the code buffer - it is typically used to link the call, setting the
/// relative offset such that when executed it will call to the desired
/// destination.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub struct Call {
    flags: u8,
    pub label: AssemblerLabel,
}

impl Call {
    pub const NONE: u8 = 0x0;
    pub const LINKABLE: u8 = 0x1;
    pub const NEAR: u8 = 0x2;
    pub const TAIL: u8 = 0x4;

    pub const LINKABLE_NEAR: u8 = Self::LINKABLE | Self::NEAR;
    pub const LINKABLE_NEAR_TAIL: u8 = Self::LINKABLE | Self::NEAR | Self::TAIL;

    pub const fn none() -> Self {
        Self {
            flags: Self::NONE,
            label: AssemblerLabel::new(u32::MAX),
        }
    }

    pub const fn new(jmp: AssemblerLabel, flags: u8) -> Self {
        Self { flags, label: jmp }
    }

    pub const fn is_flag_set(&self, flag: u8) -> bool {
        (self.flags & flag) != 0
    }

    pub const fn from_tail_jump(jmp: Jump) -> Self {
        Self {
            flags: Self::LINKABLE,
            label: jmp.label,
        }
    }
}

/// Jump:
///
/// A jump object is a reference to a jump instruction that has been planted
/// into the code buffer - it is typically used to link the jump, setting the
/// relative offset such that when executed it will jump to the desired
/// destination.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash, Default)]
pub struct Jump {
    pub label: AssemblerLabel,
}

impl Jump {
    pub const fn new(label: AssemblerLabel) -> Self {
        Self { label }
    }

    pub const fn label(&self) -> Label {
        Label { label: self.label }
    }

    pub fn link(&self, masm: &mut AbstractMacroAssembler) {
        let label = masm.assembler.label();
        masm.assembler.link_jump(self.label, label);
    }

    pub fn link_to(&self, masm: &mut AbstractMacroAssembler, target: Label) {
        masm.assembler.link_jump(self.label, target.label);
    }

    pub fn is_set(&self) -> bool {
        self.label.is_set()
    }
}

pub struct PatchableJump(pub Jump);

impl Deref for PatchableJump {
    type Target = Jump;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for PatchableJump {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

/// JumpList:
///
/// A JumpList is a set of Jump objects.
/// All jumps in the set will be linked to the same destination.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub struct JumpList {
    pub jumps: TinyVec<[Jump; 2]>,
}

impl JumpList {
    pub fn new() -> Self {
        Self {
            jumps: TinyVec::new(),
        }
    }

    pub fn from_jump(jump: Jump) -> Self {
        let mut jumps = TinyVec::new();
        if jump.is_set() {
            jumps.push(jump);
        }
        Self { jumps }
    }

    pub fn link(&self, masm: &mut AbstractMacroAssembler) {
        for jump in &self.jumps {
            jump.link(masm);
        }
    }

    pub fn link_to(&self, masm: &mut AbstractMacroAssembler, target: Label) {
        for jump in &self.jumps {
            jump.link_to(masm, target);
        }
    }

    pub fn push(&mut self, other: Jump) {
        if other.is_set() {
            self.jumps.push(other);
        }
    }

    pub fn append(&mut self, other: &JumpList) {
        self.jumps.extend(other.jumps.iter().copied());
    }

    pub fn is_empty(&self) -> bool {
        self.jumps.is_empty()
    }

    pub fn clear(&mut self) {
        self.jumps.clear();
    }

    pub fn jumps(&self) -> &[Jump] {
        &self.jumps
    }
}

pub struct AbstractMacroAssembler {
    pub assembler: TargetAssembler,
    pub link_tasks: Vec<Box<dyn FnOnce(&mut LinkBuffer)>>,
    pub late_link_tasks: Vec<Box<dyn FnOnce(&mut LinkBuffer)>>,
    pub comments: Vec<(Label, Cow<'static, str>)>,
}

impl AbstractMacroAssembler {
    pub fn code_size(&self) -> usize {
        self.assembler.code_size()
    }

    pub fn label_ignoring_watchpoints(&mut self) -> Label {
        Label {
            label: self.assembler.label_ignoring_watchpoints(),
        }
    }

    pub fn label(&mut self) -> Label {
        Label {
            label: self.assembler.label(),
        }
    }

    pub fn watchpoint_label(&mut self) -> Label {
        Label {
            label: self.assembler.label_for_watchpoint(),
        }
    }

    pub fn align(&mut self) -> Label {
        self.assembler.align(16);
        self.label()
    }

    pub fn debug_offset(&mut self) -> usize {
        self.assembler.debug_offset()
    }

    pub unsafe fn link_jump_(code: *mut u8, jump: Jump, target: *mut u8) {
        TargetAssembler::link_jump_(code, jump.label, target);
    }

    pub unsafe fn link_pointer(code: *mut u8, jump: Jump, target: *mut u8) {
        TargetAssembler::link_pointer(code, jump.label, target);
    }

    pub unsafe fn get_linker_address(code: *mut u8, label: AssemblerLabel) -> *mut u8 {
        TargetAssembler::get_relocate_address(code, label)
    }

    pub fn get_linker_call_return_offset(call: Call) -> usize {
        TargetAssembler::get_call_return_offset(call.label)
    }

    pub unsafe fn repatch_jump(jump: *mut u8, destination: *mut u8) {
        TargetAssembler::relink_jump(jump, destination);
    }

    pub unsafe fn repatch_near_call(call: *mut u8, tail: bool, destination: *mut u8) {
        if tail {
            TargetAssembler::relink_tail_call(call, destination)
        } else {
            TargetAssembler::relink_call(call, destination)
        }
    }

    pub unsafe fn repatch_int32(data_label: *mut u8, value: i32) {
        TargetAssembler::repatch_int32(data_label, value)
    }

    pub unsafe fn repatch_pointer(data_label: *mut u8, value: *mut u8) {
        TargetAssembler::repatch_pointer(data_label, value)
    }

    pub unsafe fn read_pointer(data_label: *mut u8) -> *mut u8 {
        TargetAssembler::read_pointer(data_label)
    }

    pub unsafe fn replace_with_load(label: *mut u8) {
        TargetAssembler::replace_with_load(label)
    }

    pub unsafe fn replace_with_address_computation(label: *mut u8) {
        TargetAssembler::replace_with_address_computation(label)
    }

    pub fn add_link_task(&mut self, f: Box<dyn FnOnce(&mut LinkBuffer)>) {
        self.link_tasks.push(f);
    }

    pub fn add_late_link_task(&mut self, f: Box<dyn FnOnce(&mut LinkBuffer)>) {
        self.late_link_tasks.push(f);
    }

    pub fn emit_nops(&mut self, memory_to_fill_with_nops_in_bytes: usize) {
        let buffer = self.assembler.buffer_mut();
        
        let start_code_size = buffer.code_size();
        let target_code_size = start_code_size + memory_to_fill_with_nops_in_bytes;

        buffer.ensure_space(target_code_size);

        unsafe {
            TargetAssembler::fill_nops(buffer.data_mut().as_mut_ptr().add(start_code_size), memory_to_fill_with_nops_in_bytes);
        }

        buffer.set_code_size(target_code_size);
    }

    pub fn comment(&mut self, comment: impl Into<Cow<'static, str>>) {
        self.comment_impl(comment.into());
    }

    fn comment_impl(&mut self, comment: Cow<'static, str>) {
        let lbl = self.label_ignoring_watchpoints();
        self.comments.push((lbl, comment));
    }

    pub fn new() -> Self {
        Self {
            assembler: TargetAssembler::new(),
            link_tasks: Vec::new(),
            late_link_tasks: Vec::new(),
            comments: Vec::new(),
        }
    }

    pub fn pad_before_patch(&mut self) {
        let _ = self.label();
    }
}

pub enum Location {
    Call(Call),
    NearCall(Call),
    PatchableJump(PatchableJump),
    Label(Label),
    DataLabelPtr(DataLabelPtr),
    DataLabel32(DataLabel32),
    DataLabelCompact(DataLabelCompact),
    ConvertibleLoadLabel(ConvertibleLoadLabel)
}

impl Into<Location> for Call {
    fn into(self) -> Location {
        Location::Call(self)
    }
}

impl Into<Location> for PatchableJump {
    fn into(self) -> Location {
        Location::PatchableJump(self)
    }
}

impl Into<Location> for Label {
    fn into(self) -> Location {
        Location::Label(self)
    }
}

impl Into<Location> for DataLabelPtr {
    fn into(self) -> Location {
        Location::DataLabelPtr(self)
    }
}

impl Into<Location> for DataLabel32 {
    fn into(self) -> Location {
        Location::DataLabel32(self)
    }
}

impl Into<Location> for DataLabelCompact {
    fn into(self) -> Location {
        Location::DataLabelCompact(self)
    }
}

impl Into<Location> for ConvertibleLoadLabel {
    fn into(self) -> Location {
        Location::ConvertibleLoadLabel(self)
    }
}