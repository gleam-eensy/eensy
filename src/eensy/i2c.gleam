import gleam/dynamic

pub type OpenParameters {
  Scl(Int)
  Sda(Int)
  ClockSpeedHz(Int)
}

pub type CloseError =
  dynamic.Dynamic

pub type BeginTransmissionError =
  dynamic.Dynamic

pub type EndTransmissionError =
  dynamic.Dynamic

pub type WriteBytesError =
  dynamic.Dynamic

pub type ReadBytesError =
  dynamic.Dynamic

@external(erlang, "i2c", "open")
pub fn open(parameters: List(OpenParameters)) -> Int

@external(erlang, "eensy_ffi", "i2c_open_with_result")
pub fn open_with_result(parameters: List(OpenParameters)) -> Int

@external(erlang, "eensy_ffi", "i2c_close_with_result")
pub fn close(i2c: Int) -> Result(Nil, CloseError)

@external(erlang, "eensy_ffi", "i2c_begin_transmission_with_result")
pub fn begin_transmission(
  i2c: Int,
  address: Int,
) -> Result(Nil, BeginTransmissionError)

@external(erlang, "eensy_ffi", "i2c_end_transmission_with_result")
pub fn end_transmission(i2c: Int) -> Result(Nil, EndTransmissionError)

@external(erlang, "eensy_ffi", "i2c_write_byte_with_result")
pub fn write_byte(i2c: Int, byte: BitArray) -> Result(Nil, WriteBytesError)

@external(erlang, "eensy_ffi", "i2c_write_bytes_with_result")
pub fn write_bytes(i2c: Int, bytes: BitArray) -> Result(Nil, WriteBytesError)

@external(erlang, "eensy_ffi", "i2c_read_bytes_with_result")
pub fn read_bytes(
  i2c: Int,
  address: Int,
  count: Int,
) -> Result(BitArray, ReadBytesError)
