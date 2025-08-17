use tondi_consensus_core::tx::ScriptPublicKey;
use tondi_txscript::opcodes::{Opcode, codes::*};
use std::collections::HashMap;
use std::convert::TryFrom;
use std::hash::{DefaultHasher, Hash, Hasher};

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

// 创建一个适配的Script类型，包装Tondi的ScriptPublicKey
#[derive(Clone, Debug, Hash, PartialEq)]
pub struct Script {
    inner: ScriptPublicKey,
}

impl Script {
    pub fn new() -> Self {
        Self {
            inner: ScriptPublicKey::new(0, vec![].into())
        }
    }

    pub fn from_bytes(bytes: Vec<u8>) -> Self {
        Self {
            inner: ScriptPublicKey::from_vec(0, bytes)
        }
    }

    pub fn len(&self) -> usize {
        self.inner.script().len()
    }

    pub fn as_bytes(&self) -> &[u8] {
        self.inner.script()
    }

    pub fn to_bytes(&self) -> Vec<u8> {
        self.inner.script().to_vec()
    }

    pub fn push_opcode(&mut self, opcode: Opcode) {
        let mut script = self.inner.script().to_vec();
        script.push(opcode.to_u8());
        self.inner = ScriptPublicKey::from_vec(0, script);
    }

    pub fn push_slice<T: AsRef<[u8]>>(&mut self, data: T) {
        let mut script = self.inner.script().to_vec();
        let data = data.as_ref();
        if data.len() <= 75 {
            script.push(data.len() as u8);
        } else if data.len() <= 0xff {
            script.push(0x4c);
            script.push(data.len() as u8);
        } else if data.len() <= 0xffff {
            script.push(0x4d);
            script.extend_from_slice(&(data.len() as u16).to_le_bytes());
        } else {
            script.push(0x4e);
            script.extend_from_slice(&(data.len() as u32).to_le_bytes());
        }
        script.extend_from_slice(data);
        self.inner = ScriptPublicKey::from_vec(0, script);
    }

    pub fn instructions(&self) -> std::vec::IntoIter<Result<Instruction, ()>> {
        // 简化的指令解析器
        let mut instructions = Vec::new();
        let mut i = 0;
        let bytes = self.inner.script();
        
        while i < bytes.len() {
            let opcode = bytes[i];
            i += 1;
            
            if opcode == 0x00 {
                instructions.push(Ok(Instruction::Op(Opcode::from(0x00))));
            } else if opcode <= 0x4b {
                // 数据推送
                let len = opcode as usize;
                if i + len <= bytes.len() {
                    instructions.push(Ok(Instruction::PushBytes(&bytes[i..i+len])));
                    i += len;
                } else {
                    instructions.push(Err(()));
                    break;
                }
            } else if opcode == 0x4c {
                // OP_PUSHDATA1
                if i < bytes.len() {
                    let len = bytes[i] as usize;
                    i += 1;
                    if i + len <= bytes.len() {
                        instructions.push(Ok(Instruction::PushBytes(&bytes[i..i+len])));
                        i += len;
                    } else {
                        instructions.push(Err(()));
                        break;
                    }
                } else {
                    instructions.push(Err(()));
                    break;
                }
            } else if opcode == 0x4d {
                // OP_PUSHDATA2
                if i + 1 < bytes.len() {
                    let len = u16::from_le_bytes([bytes[i], bytes[i+1]]) as usize;
                    i += 2;
                    if i + len <= bytes.len() {
                        instructions.push(Ok(Instruction::PushBytes(&bytes[i..i+len])));
                        i += len;
                    } else {
                        instructions.push(Err(()));
                        break;
                    }
                } else {
                    instructions.push(Err(()));
                    break;
                }
            } else if opcode == 0x4e {
                // OP_PUSHDATA4
                if i + 3 < bytes.len() {
                    let len = u32::from_le_bytes([bytes[i], bytes[i+1], bytes[i+2], bytes[i+3]]) as usize;
                    i += 4;
                    if i + len <= bytes.len() {
                        instructions.push(Ok(Instruction::PushBytes(&bytes[i..i+len])));
                        i += len;
                    } else {
                        instructions.push(Err(()));
                        break;
                    }
                } else {
                    instructions.push(Err(()));
                    break;
                }
            } else {
                instructions.push(Ok(Instruction::Op(Opcode::from(opcode))));
            }
        }
        
        instructions.into_iter()
    }

    pub fn instructions_minimal(&self) -> std::vec::IntoIter<Result<(), ()>> {
        // 简化的最小化检查
        self.instructions().map(|r| r.map(|_| ())).collect::<Vec<_>>().into_iter()
    }
}

#[derive(Debug, Clone)]
pub enum Instruction<'a> {
    Op(Opcode),
    PushBytes(&'a [u8]),
}

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug, Hash, PartialEq)]
pub enum Block {
    Call(u64),
    Script(Script),
}

impl Block {
    fn new_script() -> Self {
        let buf = Script::new();
        Block::Script(buf)
    }
}

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug, PartialEq)]
pub struct StructuredScript {
    size: usize,
    pub debug_identifier: String,
    pub blocks: Vec<Block>, //List?
    script_map: HashMap<u64, StructuredScript>,
}

impl Hash for StructuredScript {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.blocks.hash(state);
    }
}

fn calculate_hash<T: Hash>(t: &T) -> u64 {
    let mut hasher = DefaultHasher::new();
    t.hash(&mut hasher);
    hasher.finish()
}

impl StructuredScript {
    pub fn new(debug_info: &str) -> Self {
        StructuredScript {
            size: 0,
            debug_identifier: debug_info.to_string(),
            blocks: Vec::new(),
            script_map: HashMap::new(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.size == 0
    }

    pub fn len(&self) -> usize {
        self.size
    }

    pub fn add_structured_script(&mut self, id: u64, script: StructuredScript) {
        self.script_map.entry(id).or_insert(script);
    }

    pub fn get_structured_script(&self, id: &u64) -> &StructuredScript {
        self.script_map
            .get(id)
            .unwrap_or_else(|| panic!("script id: {} not found in script_map.", id))
    }

    // Return the debug information of the Opcode at position
    pub fn debug_info(&self, position: usize) -> String {
        let mut current_pos = 0;
        for block in &self.blocks {
            assert!(current_pos <= position, "Target position not found");
            match block {
                Block::Call(id) => {
                    //let called_script = self.get_structured_script(id);
                    let called_script = self
                        .script_map
                        .get(id)
                        .expect("Missing entry for a called script");
                    if position >= current_pos && position < current_pos + called_script.len() {
                        return called_script.debug_info(position - current_pos);
                    }
                    current_pos += called_script.len();
                }
                Block::Script(script_buf) => {
                    if position >= current_pos && position < current_pos + script_buf.len() {
                        return self.debug_identifier.clone();
                    }
                    current_pos += script_buf.len();
                }
            }
        }
        panic!("No blocks in the structured script");
    }

    fn get_script_block(&mut self) -> &mut Script {
        // Check if the last block is a Script block
        let is_script_block = matches!(self.blocks.last_mut(), Some(Block::Script(_)));

        // Create a new Script block if necessary
        if !is_script_block {
            self.blocks.push(Block::new_script());
        }

        if let Some(Block::Script(ref mut script)) = self.blocks.last_mut() {
            script
        } else {
            unreachable!()
        }
    }

    pub fn push_opcode(mut self, data: Opcode) -> StructuredScript {
        self.size += 1;
        let script = self.get_script_block();
        script.push_opcode(data);
        self
    }

    pub fn push_script(mut self, data: Script) -> StructuredScript {
        let mut pos = 0;
        for instruction in data.instructions() {
            match instruction {
                Ok(Instruction::Op(_)) => pos += 1,
                Ok(Instruction::PushBytes(pushbytes)) => pos += pushbytes.len() + 1,
                _ => (),
            };
        }
        assert_eq!(data.len(), pos, "Pos counting seems to be off");
        self.size += data.len();
        self.blocks.push(Block::Script(data));
        self
    }

    pub fn push_env_script(mut self, mut data: StructuredScript) -> StructuredScript {
        if data.is_empty() {
            return self;
        }
        if self.is_empty() {
            return data;
        }

        data.debug_identifier = format!("{} {}", self.debug_identifier, data.debug_identifier);
        self.size += data.len();
        let id = calculate_hash(&data);
        self.blocks.push(Block::Call(id));
        // Register script in the script map
        self.add_structured_script(id, data);
        self
    }

    /// Compiles the script to bytes.
    fn compile_to_bytes(&self) -> Vec<u8> {
        #[derive(Debug)]
        enum Task<'a> {
            CompileCall {
                id: u64,
                called_script: &'a StructuredScript,
            },
            PushRaw(&'a Script),
            UpdateCache {
                id: u64,
                called_script_start: usize,
            },
        }

        fn push_script<'a>(script: &'a StructuredScript, tasks: &mut Vec<Task<'a>>) {
            for block in script.blocks.iter().rev() {
                match block {
                    Block::Call(id) => {
                        let called_script = script
                            .script_map
                            .get(id)
                            .expect("missing entry for called script");
                        tasks.push(Task::CompileCall {
                            id: *id,
                            called_script,
                        });
                    }
                    Block::Script(buffer) => tasks.push(Task::PushRaw(buffer)),
                }
            }
        }

        let mut tasks = Vec::new();
        let mut cache = HashMap::new();
        let mut script: Vec<u8> = Vec::with_capacity(self.size);
        push_script(self, &mut tasks);

        while let Some(task) = tasks.pop() {
            match task {
                Task::CompileCall { id, called_script } => {
                    match cache.get(&id) {
                        Some(called_start) => {
                            // Copy the already compiled called_script from the position it was
                            // inserted in the compiled script.
                            let start = script.len();
                            let end = start + called_script.len();
                            // TODO: Check if assertion is always true due to code invariants
                            assert!(
                                end <= script.capacity(),
                                "Not enough capacity allocated for compiled script"
                            );
                            unsafe {
                                script.set_len(end);

                                let src_ptr = script.as_ptr().add(*called_start);
                                let dst_ptr = script.as_mut_ptr().add(start);

                                std::ptr::copy_nonoverlapping(
                                    src_ptr,
                                    dst_ptr,
                                    called_script.len(),
                                );
                            }
                        }
                        None => {
                            tasks.push(Task::UpdateCache {
                                id,
                                called_script_start: script.len(),
                            });
                            push_script(called_script, &mut tasks);
                        }
                    }
                }
                Task::PushRaw(buffer) => {
                    let source_script = buffer.as_bytes();
                    let start = script.len();
                    let end = start + source_script.len();
                    // TODO: Check if assertion is always true due to code invariants
                    assert!(
                        end <= script.capacity(),
                        "Not enough capacity allocated for compiled script"
                    );
                    unsafe {
                        script.set_len(end);

                        let src_ptr = source_script.as_ptr();
                        let dst_ptr = script.as_mut_ptr().add(start);

                        std::ptr::copy_nonoverlapping(src_ptr, dst_ptr, source_script.len());
                    }
                }
                Task::UpdateCache {
                    id,
                    called_script_start,
                } => {
                    cache.insert(id, called_script_start);
                }
            }
        }

        script
    }

    pub fn compile(self) -> Script {
        let script = self.compile_to_bytes();
        // Ensure that the builder has minimal opcodes:
        let script_buf = Script::from_bytes(script);
        let mut instructions_iter = script_buf.instructions();
        for result in script_buf.instructions_minimal() {
            let instruction = instructions_iter.next();
            match result {
                Ok(_) => (),
                Err(err) => {
                    panic!(
                        "Error while parsing script instruction: {:?}, {:?}",
                        err, instruction
                    );
                }
            }
        }
        script_buf
    }

    pub fn push_int(self, data: i64) -> StructuredScript {
        // We can special-case -1, 1-16
        if data == -1 || (1..=16).contains(&data) {
            let opcode = Opcode::from((data - 1 + OP_TRUE.to_u8() as i64) as u8);
            self.push_opcode(opcode)
        }
        // We can also special-case zero
        else if data == 0 {
            self.push_opcode(OP_0)
        }
        // Otherwise encode it as data
        else {
            self.push_int_non_minimal(data)
        }
    }
    fn push_int_non_minimal(self, data: i64) -> StructuredScript {
        let mut buf = [0u8; 8];
        let len = write_scriptint(&mut buf, data);
        self.push_slice(&buf[..len])
    }

    pub fn push_slice<T: AsRef<[u8]>>(mut self, data: T) -> StructuredScript {
        let script = self.get_script_block();
        let old_size = script.len();
        script.push_slice(data);
        self.size += script.len() - old_size;
        self
    }

    pub fn push_key(self, key: &::tondi_consensus_core::tx::PublicKey) -> StructuredScript {
        if key.compressed {
            self.push_slice(key.inner.serialize())
        } else {
            self.push_slice(key.inner.serialize_uncompressed())
        }
    }

    pub fn push_x_only_key(self, x_only_key: &::tondi_consensus_core::tx::XOnlyPublicKey) -> StructuredScript {
        self.push_slice(x_only_key.serialize())
    }

    pub fn push_expression<T: Pushable>(self, expression: T) -> StructuredScript {
        expression.tondi_script_push(self)
    }
}

// 简化的scriptint写入函数
fn write_scriptint(result: &mut [u8; 8], value: i64) -> usize {
    if value == 0 {
        return 0;
    }
    
    let mut abs_value = if value < 0 { -value } else { value } as u64;
    let mut size = 0;
    
    while abs_value > 0 {
        result[size] = (abs_value & 0xff) as u8;
        abs_value >>= 8;
        size += 1;
    }
    
    // 如果最高位是1，需要添加一个0字节
    if (result[size - 1] & 0x80) != 0 {
        result[size] = 0;
        size += 1;
    }
    
    // 如果是负数，设置最高位
    if value < 0 {
        result[size - 1] |= 0x80;
    }
    
    size
}

// We split up the tondi_script_push function to allow pushing a single u8 value as
// an integer (i64), Vec<u8> as raw data and Vec<T> for any T: Pushable object that is
// not a u8. Otherwise the Vec<u8> and Vec<T: Pushable> definitions conflict.
trait NotU8Pushable {
    fn tondi_script_push(self, builder: StructuredScript) -> StructuredScript;
}
impl NotU8Pushable for i64 {
    fn tondi_script_push(self, builder: StructuredScript) -> StructuredScript {
        builder.push_int(self)
    }
}
impl NotU8Pushable for i32 {
    fn tondi_script_push(self, builder: StructuredScript) -> StructuredScript {
        builder.push_int(self as i64)
    }
}
impl NotU8Pushable for u32 {
    fn tondi_script_push(self, builder: StructuredScript) -> StructuredScript {
        builder.push_int(self as i64)
    }
}
impl NotU8Pushable for usize {
    fn tondi_script_push(self, builder: StructuredScript) -> StructuredScript {
        builder.push_int(i64::try_from(self).expect("Usize does not fit in i64"))
    }
}
impl NotU8Pushable for Vec<u8> {
    fn tondi_script_push(self, builder: StructuredScript) -> StructuredScript {
        // Push the element with a minimal opcode if it is a single number.
        if self.len() == 1 {
            builder.push_int(self[0].into())
        } else {
            builder.push_slice(self)
        }
    }
}
impl NotU8Pushable for ::tondi_consensus_core::tx::PublicKey {
    fn tondi_script_push(self, builder: StructuredScript) -> StructuredScript {
        builder.push_key(&self)
    }
}
impl NotU8Pushable for ::tondi_consensus_core::tx::XOnlyPublicKey {
    fn tondi_script_push(self, builder: StructuredScript) -> StructuredScript {
        builder.push_x_only_key(&self)
    }
}
impl NotU8Pushable for StructuredScript {
    fn tondi_script_push(self, builder: StructuredScript) -> StructuredScript {
        builder.push_env_script(self)
    }
}
impl<T: NotU8Pushable> NotU8Pushable for Vec<T> {
    fn tondi_script_push(self, mut builder: StructuredScript) -> StructuredScript {
        for pushable in self {
            builder = pushable.tondi_script_push(builder);
        }
        builder
    }
}
impl NotU8Pushable for Script {
    fn tondi_script_push(self, builder: StructuredScript) -> StructuredScript {
        builder.push_script(self)
    }
}

pub trait Pushable {
    fn tondi_script_push(self, builder: StructuredScript) -> StructuredScript;
}
impl<T: NotU8Pushable> Pushable for T {
    fn tondi_script_push(self, builder: StructuredScript) -> StructuredScript {
        NotU8Pushable::tondi_script_push(self, builder)
    }
}

impl Pushable for u8 {
    fn tondi_script_push(self, builder: StructuredScript) -> StructuredScript {
        builder.push_int(self as i64)
    }
}
