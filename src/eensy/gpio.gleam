pub type Direction {
  Input
  Output
  OutputOd
}

pub type Level {
  Low
  High
}


pub type Pull {
  Up
  Down
}

/// Start gpio
@external(erlang, "eensy_ffi", "start_with_result")
pub fn start() -> Result(Int, Nil)

/// Init pin
@external(erlang, "gpio", "init")
pub fn init(pin: Int) -> Nil

/// Set pin pull
@external(erlang, "gpio", "set_pin_pull")
pub fn set_pin_pull(pin: Int, pull: Pull) -> Result(Nil, Nil)

/// Set gpio pin direction
@external(erlang, "eensy_ffi", "set_pin_mode_with_result")
pub fn set_pin_mode(pin: Int, direction: Direction) -> Result(Int, Nil)

/// Write to gpio pin
@external(erlang, "eensy_ffi", "digital_write_with_result")
pub fn digital_write(pin: Int, level: Level) -> Result(Int, Nil)

/// Read from gpio pin
@external(erlang, "eensy_ffi", "digital_read_with_result")
pub fn digital_read(pin: Int) -> Result(Level, Nil)
