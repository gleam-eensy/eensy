pub type Direction {
  Input
  Output
  OutputOd
}

pub type Level {
  Low
  High
}

/// Start gpio
@external(erlang, "eensy_ffi", "start_with_result")
pub fn start() -> Result(Int, Nil)

/// Set gpio pin direction
@external(erlang, "eensy_ffi", "set_pin_mode_with_result")
pub fn set_pin_mode(pin: Int, direction: Direction) -> Result(Int, Nil)

/// Write to gpio pin
@external(erlang, "eensy_ffi", "digital_write_with_result")
pub fn digital_write(pin: Int, level: Level) -> Result(Int, Nil)
