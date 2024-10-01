pub type PlatformName {
  GenericUnix
  Emscripten
  Esp32
  Pico
  Stm32
}

/// get the running platform name
@external(erlang, "atomvm", "platform")
pub fn get_system_platform() -> PlatformName

/// using a direct erlang:display directly from gleam for debug purposes
@external(erlang, "erlang", "display")
pub fn erlang_display(any: dynamic) -> Nil
