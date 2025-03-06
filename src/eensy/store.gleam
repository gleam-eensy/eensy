import eensy/otp/actor
import gleam/erlang/process.{type Subject}
import gleam/result

pub fn start(state: model) -> Result(StoreActor(model), actor.StartError) {
  actor.start(state, handle_message)
  |> result.map(StoreActor)
}

pub fn set(actor: StoreActor(model), value: model) {
  process.send(actor.subject, Set(value))
}

pub fn get(actor: StoreActor(a)) -> Result(a, Nil) {
  process.try_call(actor.subject, Get(_), 100)
  |> result.map_error(fn(_) { Nil })
  |> result.flatten
}

pub opaque type StoreActor(model) {
  StoreActor(subject: Subject(Msg(model)))
}

type Msg(model) {
  Set(value: model)
  Get(reply_with: Subject(Result(model, Nil)))
}

fn handle_message(
  message: Msg(model),
  state: model,
) -> actor.Next(Msg(model), model) {
  case message {
    Set(value) -> {
      actor.continue(value)
    }
    Get(client) -> {
      process.send(client, Ok(state))
      actor.continue(state)
    }
  }
}
