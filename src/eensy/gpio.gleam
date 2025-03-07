import eensy.{type Pull}
import eensy/otp/actor
import gleam/erlang/process.{type Subject}
import gleam/option.{type Option, None, Some}
import gleam/result

// ============================================================
// Types
// ============================================================

pub type Direction {
  Input
  Output
  OutputOd
}

pub type Level {
  Low
  High
}

pub opaque type Pin {
  Pin(
    level: Level,
    pull: Pull,
    port: Int,
    direction: Direction,
    update: Option(fn(Level) -> Nil),
  )
}

pub opaque type PinActor(model, msg) {
  PinActor(subject: Subject(Msg))
}

type Msg {
  Write(value: Level)
  Read(reply_with: Subject(Result(Pin, Nil)))
  Sync
}

// ============================================================
// Functions
// ============================================================

pub fn start(pin: Pin) -> Result(PinActor(model, msg), actor.StartError) {
  let _ = set_pin_mode(pin.port, pin.direction)
  let _ = set_pin_pull(pin.port, pin.pull)

  actor.start(pin, handle_message)
  |> result.map(PinActor)
}

pub fn pin(
  level level: Level,
  pull pull: Pull,
  port port: Int,
  direction direction: Direction,
  update update: Option(fn(Level) -> Nil),
) -> Pin {
  Pin(level, pull, port, direction, update)
}

pub fn write(actor: PinActor(model, msg), value: Level) {
  process.send(actor.subject, Write(value))
}

pub fn read(actor: PinActor(model, msg)) -> Result(Pin, Nil) {
  process.try_call(actor.subject, Read(_), 100)
  |> result.map_error(fn(_) { Nil })
  |> result.flatten
}

pub fn sync(actor: PinActor(model, msg)) {
  process.send(actor.subject, Sync)
}

fn handle_message(message: Msg, state: Pin) -> actor.Next(Msg, Pin) {
  case message {
    Write(value) -> {
      let _level = digital_write(state.port, value)
      case state.update {
        Some(update) -> update(value)
        None -> Nil
      }

      let state = Pin(..state, level: value)
      actor.continue(state)
    }
    Read(client) -> {
      let level =
        digital_read(state.port)
        |> result.unwrap(Low)

      case state.update {
        Some(update) -> update(level)
        None -> Nil
      }
      let state = Pin(..state, level: level)
      process.send(client, Ok(state))
      actor.continue(state)
    }
    Sync -> {
      let level =
        digital_read(state.port)
        |> result.unwrap(Low)

      let state = Pin(..state, level: level)

      case state.update {
        Some(update) -> update(level)
        None -> Nil
      }

      actor.continue(state)
    }
  }
}

/// Start gpio
@external(erlang, "eensy_ffi", "start_with_result")
pub fn do_start() -> Result(Int, Nil)

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
