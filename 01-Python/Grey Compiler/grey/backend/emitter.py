"""
Bytecode Emitter for the Grey compiler.

Serializes a CompiledProgram into the Grey bytecode binary format (.greyc)
and can also emit human-readable disassembly.

Binary format:
  Header:
    magic:    4 bytes  "GREY"
    version:  2 bytes  (major, minor)
    flags:    2 bytes
  String table:
    count:    4 bytes
    entries:  [length: 4 bytes, data: N bytes]...
  Global names:
    count:    4 bytes
    entries:  [index into string table]...
  Chunks:
    count:    4 bytes
    For each chunk:
      name_index:    4 bytes
      param_count:   4 bytes
      local_count:   4 bytes
      const_count:   4 bytes
      constants:     [type: 1 byte, data: variable]...
      instr_count:   4 bytes
      instructions:  [encoded: 4 bytes]...
  Main chunk index: 4 bytes
"""

import struct
import json
from typing import BinaryIO
from .instruction import CompiledProgram, Chunk, VMInstruction, VMOpcode


GREY_MAGIC = b'GREY'
GREY_VERSION = (1, 0)


class BytecodeEmitter:
    """
    Emits compiled Grey programs in various formats.

    Usage:
        emitter = BytecodeEmitter(compiled_program)
        emitter.emit_binary("output.greyc")
        emitter.emit_disassembly("output.greyd")
    """

    def __init__(self, program: CompiledProgram):
        self.program = program

    # ══════════════════════════════════════════════════════════
    #  Binary Emission (.greyc)
    # ══════════════════════════════════════════════════════════

    def emit_binary(self, filename: str):
        """Emit the program as a binary bytecode file."""
        with open(filename, 'wb') as f:
            self._write_header(f)
            self._write_string_table(f)
            self._write_globals(f)
            self._write_chunks(f)
            self._write_u32(f, self.program.main_chunk_index)

    def _write_header(self, f: BinaryIO):
        f.write(GREY_MAGIC)
        f.write(struct.pack('BB', *GREY_VERSION))
        f.write(struct.pack('<H', 0))  # flags

    def _write_u32(self, f: BinaryIO, value: int):
        f.write(struct.pack('<I', value))

    def _write_string(self, f: BinaryIO, s: str):
        encoded = s.encode('utf-8')
        self._write_u32(f, len(encoded))
        f.write(encoded)

    def _write_string_table(self, f: BinaryIO):
        strings = self.program.string_table
        self._write_u32(f, len(strings))
        for s in strings:
            self._write_string(f, s)

    def _write_globals(self, f: BinaryIO):
        self._write_u32(f, len(self.program.global_names))
        for name in self.program.global_names:
            self._write_string(f, name)

    def _write_chunks(self, f: BinaryIO):
        self._write_u32(f, len(self.program.chunks))
        for chunk in self.program.chunks:
            self._write_chunk(f, chunk)

    def _write_chunk(self, f: BinaryIO, chunk: Chunk):
        self._write_string(f, chunk.name)
        self._write_u32(f, chunk.param_count)
        self._write_u32(f, chunk.local_count)

        # Constants
        self._write_u32(f, len(chunk.constants))
        for const in chunk.constants:
            self._write_constant(f, const)

        # Instructions
        self._write_u32(f, len(chunk.instructions))
        for instr in chunk.instructions:
            self._write_u32(f, instr.encode())

    def _write_constant(self, f: BinaryIO, value):
        """Write a constant value with type tag."""
        if value is None:
            f.write(b'\x00')  # nil
        elif isinstance(value, bool):
            f.write(b'\x01')  # bool
            f.write(b'\x01' if value else b'\x00')
        elif isinstance(value, int):
            f.write(b'\x02')  # int
            f.write(struct.pack('<q', value))  # 64-bit signed
        elif isinstance(value, float):
            f.write(b'\x03')  # float
            f.write(struct.pack('<d', value))  # 64-bit double
        elif isinstance(value, str):
            f.write(b'\x04')  # string
            self._write_string(f, value)
        else:
            # Fallback: serialize as string
            f.write(b'\x04')
            self._write_string(f, str(value))

    # ══════════════════════════════════════════════════════════
    #  Binary Loading
    # ══════════════════════════════════════════════════════════

    @staticmethod
    def load_binary(filename: str) -> CompiledProgram:
        """Load a compiled program from a binary file."""
        with open(filename, 'rb') as f:
            # Header
            magic = f.read(4)
            if magic != GREY_MAGIC:
                raise ValueError(f"Invalid Grey bytecode: bad magic {magic!r}")
            version = struct.unpack('BB', f.read(2))
            flags = struct.unpack('<H', f.read(2))[0]

            program = CompiledProgram(version=f"{version[0]}.{version[1]}")

            # String table
            str_count = struct.unpack('<I', f.read(4))[0]
            for _ in range(str_count):
                slen = struct.unpack('<I', f.read(4))[0]
                program.string_table.append(f.read(slen).decode('utf-8'))

            # Globals
            glob_count = struct.unpack('<I', f.read(4))[0]
            for _ in range(glob_count):
                slen = struct.unpack('<I', f.read(4))[0]
                program.global_names.append(f.read(slen).decode('utf-8'))

            # Chunks
            chunk_count = struct.unpack('<I', f.read(4))[0]
            for _ in range(chunk_count):
                chunk = BytecodeEmitter._read_chunk(f)
                program.add_chunk(chunk)

            # Main chunk index
            program.main_chunk_index = struct.unpack('<I', f.read(4))[0]

            return program

    @staticmethod
    def _read_chunk(f: BinaryIO) -> Chunk:
        # Name
        name_len = struct.unpack('<I', f.read(4))[0]
        name = f.read(name_len).decode('utf-8')

        param_count = struct.unpack('<I', f.read(4))[0]
        local_count = struct.unpack('<I', f.read(4))[0]

        # Constants
        const_count = struct.unpack('<I', f.read(4))[0]
        constants = []
        for _ in range(const_count):
            constants.append(BytecodeEmitter._read_constant(f))

        # Instructions
        instr_count = struct.unpack('<I', f.read(4))[0]
        instructions = []
        for _ in range(instr_count):
            word = struct.unpack('<I', f.read(4))[0]
            instructions.append(VMInstruction.decode(word))

        return Chunk(
            name=name,
            instructions=instructions,
            constants=constants,
            local_count=local_count,
            param_count=param_count,
        )

    @staticmethod
    def _read_constant(f: BinaryIO):
        type_tag = f.read(1)[0]
        if type_tag == 0x00:
            return None
        elif type_tag == 0x01:
            return bool(f.read(1)[0])
        elif type_tag == 0x02:
            return struct.unpack('<q', f.read(8))[0]
        elif type_tag == 0x03:
            return struct.unpack('<d', f.read(8))[0]
        elif type_tag == 0x04:
            slen = struct.unpack('<I', f.read(4))[0]
            return f.read(slen).decode('utf-8')
        return None

    # ══════════════════════════════════════════════════════════
    #  Disassembly (Human-readable)
    # ══════════════════════════════════════════════════════════

    def disassemble(self) -> str:
        """Return a human-readable disassembly of the program."""
        lines = [
            "+----------------------------------------------+",
            "|       Grey Compiler Disassembly Output       |",
            "+----------------------------------------------+",
            "",
            f"Version: {self.program.version}",
            f"Main chunk: {self.program.main_chunk_index}",
            f"Total chunks: {len(self.program.chunks)}",
            f"String table: {self.program.string_table}",
            "",
        ]

        for i, chunk in enumerate(self.program.chunks):
            marker = " [MAIN]" if i == self.program.main_chunk_index else ""
            lines.append(f"{'-' * 50}")
            lines.append(f"Chunk {i}: {chunk.name}{marker}")
            lines.append(f"  Params: {chunk.param_count}, Locals: {chunk.local_count}")
            lines.append(f"  Constants: {chunk.constants}")
            lines.append(f"  Instructions ({len(chunk.instructions)}):")

            for j, instr in enumerate(chunk.instructions):
                lines.append(f"    {j:04d}  {instr}")

            lines.append("")

        return "\n".join(lines)

    def emit_disassembly(self, filename: str):
        """Write disassembly to a file."""
        with open(filename, 'w', encoding='utf-8') as f:
            f.write(self.disassemble())

    # ══════════════════════════════════════════════════════════
    #  JSON Emission (for tooling)
    # ══════════════════════════════════════════════════════════

    def emit_json(self, filename: str):
        """Emit the program as JSON for debugging/tooling."""
        data = {
            "version": self.program.version,
            "main_chunk": self.program.main_chunk_index,
            "string_table": self.program.string_table,
            "global_names": self.program.global_names,
            "chunks": []
        }

        for chunk in self.program.chunks:
            chunk_data = {
                "name": chunk.name,
                "param_count": chunk.param_count,
                "local_count": chunk.local_count,
                "constants": [self._serialize_const(c) for c in chunk.constants],
                "instructions": [
                    {
                        "opcode": instr.opcode.name,
                        "a": instr.a,
                        "b": instr.b,
                        "c": instr.c,
                        "sbx": instr.sbx,
                        "comment": instr.comment,
                    }
                    for instr in chunk.instructions
                ]
            }
            data["chunks"].append(chunk_data)

        with open(filename, 'w', encoding='utf-8') as f:
            json.dump(data, f, indent=2)

    def _serialize_const(self, value) -> dict:
        if value is None:
            return {"type": "nil", "value": None}
        elif isinstance(value, bool):
            return {"type": "bool", "value": value}
        elif isinstance(value, int):
            return {"type": "int", "value": value}
        elif isinstance(value, float):
            return {"type": "float", "value": value}
        elif isinstance(value, str):
            return {"type": "string", "value": value}
        return {"type": "unknown", "value": str(value)}
