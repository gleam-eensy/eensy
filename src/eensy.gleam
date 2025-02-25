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

// 

// Create a Builder opaque type usage pattern
// Add plugins (gpio input, gpio output, tiles system, raw ssd1306 )
// update(tile, tile, tile)

// pub opaque type Handler(user_message, data) {
//   Handler(
//     interface: options.Interface,
//     on_init: fn(Connection(user_message)) ->
//       #(data, Option(Selector(user_message))),
//     loop: Loop(user_message, data),
//     on_close: Option(fn(data) -> Nil),
//     pool_size: Int,
//     http2_support: Bool,
//     ipv6_support: Bool,
//   )
// }

// pub opaque type App(flags, model, msg) {
//   App(
//     init: fn(flags) -> #(model, Effect(msg)),
//     update: fn(model, msg) -> #(model, Effect(msg)),
//     view: fn(model) -> Element(msg),
//     // The `dict.mjs` module in the standard library is huge (20+kb!). For folks
//     // that don't ever build components and don't use a dictionary in any of their
//     // code we'd rather not thrust that increase in bundle size on them just to
//     // call `dict.new()`.
//     //
//     // Using `Option` here at least lets us say `None` for the empty case in the
//     // `application` constructor.
//     //
//     on_attribute_change: Option(Dict(String, Decoder(msg))),
//   )
// }

// pub fn application(
//   init: fn(flags) -> #(model, Effect(msg)),
//   update: fn(model, msg) -> #(model, Effect(msg)),
//   view: fn(model) -> Element(msg),
// ) -> App(flags, model, msg) {
//   App(init, update, view, None)
// }

// BoardConfig should handle the gpio definition of a board for handling opaque type validations 
// like trying to access a pin that does not belong to the config definition. 
// Maybe aliasing the pin number using a fn name? We want to track the count of looping with the
// same pin state
// eensy.application(
// board, handlers, loop
// )

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
