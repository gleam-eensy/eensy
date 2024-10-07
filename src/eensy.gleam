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
