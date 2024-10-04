# 02 The GPIO Guide

The idea of Eensy gpio module is to provide type-safe functions for properly handle input/output from the microcontroller dev kit hardware.

## A quick example

In the next initial example related the gpio module. We are assuming a setup where we have 7 
input buttons distributed in the pins `[12, 14, 27, 26, 25, 32, 33]` and we're going to flash the main LED PIN when any of them are pressed. 

```gleam
import eensy.{get_system_platform}
import eensy/gpio
import gleam/erlang/process
import gleam/io
import gleam/list

pub fn start() -> Nil {
  case eensy.get_system_platform() {
    eensy.Esp32 -> {
      let _ = gpio.set_pin_mode(internal_blink_pin(), gpio.Output)

      let _ =
        process.start(
          fn() {
            [12, 14, 27, 26, 25, 32, 33]
            |> init_inputs
            |> check_inputs
          },
          False,
        )

      Nil
    }
    _ -> Nil
  }

  process.sleep_forever()
}

fn init_inputs(pins) {
  list.map(pins, fn(pin) {
    process.sleep(10)
    case gpio.set_pin_pull(pin, gpio.Up) {
      Error(_) -> io.println("set_pin_pull error")
      _ -> Nil
    }
    process.sleep(10)
    case gpio.set_pin_mode(pin, gpio.Input) {
      Error(_) -> io.println("init_inputs error")
      _ -> Nil
    }
  })
  pins
}

fn check_inputs(pins) {
  list.map(pins, fn(pin) {
    process.sleep(10)
    case gpio.digital_read(pin) {
      Ok(gpio.Low) -> {
        let _ = gpio.digital_write(internal_blink_pin(), gpio.High)
        Nil
      }
      Ok(gpio.High) -> {
        let _ = gpio.digital_write(internal_blink_pin(), gpio.Low)
        Nil
      }
      _ -> io.println("error")
    }
  })
  process.sleep(20)
  check_inputs(pins)
  pins
}

fn internal_blink_pin() {
  case get_system_platform() {
    eensy.Esp32 | eensy.Pico -> 2
    _ -> 0
  }
}
```

