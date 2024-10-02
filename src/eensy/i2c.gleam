pub type OpenParameters {
  Scl(Int)
  Sda(Int)
  ClockSpeedHz(Int)
}

/// Start i2c
// pub fn open(parameters: List(OpenParameters)) -> Result(Int, Nil)

@external(erlang, "eensy_ffi", "i2c_open_with_result")
pub fn open(parameters: List(OpenParameters)) -> Int
