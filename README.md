# eensy

Very early alpha stage. Please just use it for hobby projects. Open to contributions.

[![Package Version](https://img.shields.io/hexpm/v/eensy)](https://hex.pm/packages/eensy)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/eensy/)

```sh
gleam add eensy
```
```gleam
import eensy.{get_system_platform}
import eensy/gpio
import eensy/otp/task
import gleam/erlang/process
import gleam/result.{try}

pub fn start() -> Nil {
  case eensy.get_system_platform() {
    eensy.Esp32 -> {
      // Set pin mode to output
      let _ = gpio.set_pin_mode(pin(), gpio.Output)

      // Start blinking
      let _blinky = process.start(fn() { loop(pin(), gpio.High) }, False)

      Nil
    }
    _ -> Nil
  }

  process.sleep_forever()
}

fn pin() {
  case get_system_platform() {
    eensy.Esp32 | eensy.Pico -> 2
    _ -> 0
  }
}

fn loop(pin, level) {
  use _ <- try(gpio.digital_write(pin, toggle(level)))
  process.sleep(case level {
    gpio.High -> 150
    gpio.Low -> 300
  })
  loop(pin, toggle(level))
}

fn toggle(level) {
  case level {
    gpio.High -> gpio.Low
    _ -> gpio.High
  }
}

```

Further documentation can be found at <https://hexdocs.pm/eensy>.

## Development

```sh
gleam run -m eensy_dev_tools pack # Run devtools
atomvm ./build/.atomvm/<project_name>.release.avm # Run the avm file in a unix atomvm instance
gleam test  # Run the tests
```

