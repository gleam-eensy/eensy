pub type PlatformName {
  GenericUnix
  Emscripten
  Esp32
  Pico
  Stm32
}

pub type Pull {
  Up
  Down
}

/// System processess
@external(erlang, "eensy_ffi", "processes_info")
pub fn processes_info() -> Nil

/// get the running platform name
@external(erlang, "atomvm", "platform")
pub fn get_system_platform() -> PlatformName

/// using erlang:display directly from gleam for debug purposes
@external(erlang, "erlang", "display")
pub fn erlang_display(any: dynamic) -> Nil

/// using erlang:garbage_collect directly from gleam for forcing vm behaviour
@external(erlang, "erlang", "garbage_collect")
pub fn erlang_garbage_collect() -> Nil

pub fn pin(name name: String, num num: Int, pull pull: Pull) {
  Pin(name, num, pull)
}

pub fn i2c(
  name name: String,
  sda sda: Int,
  scl scl: Int,
  clock_speed clock_speed: Int,
  pull pull: Pull,
) {
  I2c(name:, sda:, scl:, clock_speed:, pull:)
}

pub fn board(
  inputs inputs: List(BoardPeripheral),
  outputs outputs: List(BoardPeripheral),
) {
  BoardConfig(inputs, outputs)
}

pub opaque type BoardPeripheral {
  I2c(name: String, sda: Int, scl: Int, clock_speed: Int, pull: Pull)
  Pin(name: String, num: Int, pull: Pull)
}

pub opaque type BoardConfig {
  BoardConfig(inputs: List(BoardPeripheral), outputs: List(BoardPeripheral))
}
