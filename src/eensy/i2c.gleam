pub type OpenParameters {
  Scl(Int)
  Sda(Int)
  ClockSpeedHz(Int)
}

/// Start i2c
// pub fn open(parameters: List(OpenParameters)) -> Result(Int, Nil)

@external(erlang, "i2c", "open")
pub fn open(parameters: List(OpenParameters)) -> Int
