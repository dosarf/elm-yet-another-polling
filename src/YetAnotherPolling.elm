module YetAnotherPolling exposing (
    Configuration, defaultConfiguration
  , State (..), Model, stateOf, init
  , start, stop, update
  , PollingDecision (..)
  )

{-| A package for polling made easier.

You need to initialize it's model with
* basic retry-after and backing-off configuration
  ([`Configuration`](#Configuration)),
* a function that yields a task (this would be your HTTP task polling
  something, but it can actually be any other task)
* another function that defines, based on the result of a task, how to
  continue with polling,
* a constructor of a message of yours,

and out come messages bearing results of those tasks attempted.

You have to call [`update`](#update) the polling model ([`Model`](#Model))
with the messages you recieve to keep it going.

## Configuration

@docs Configuration, defaultConfiguration

## Model

The model for tracking polling state as well issuing request tasks.

@docs State, Model, stateOf, init

## Operations

@docs start, stop, update

@docs PollingDecision
-}

import Process
import Task

{-| How polling should behave in case of _recoverable_ failure
(see [`PollOne`](#PollingDecision)):
* `minRetryAfterMillis`: the minimum time interval to wait before retrying,
* `maxRetryAfterMillis`: the maximum time interval to wait before retrying,
* `backOffRate`: the ratio of the time interval to wait before retrying
and the time interval waited on the last try.
-}
type alias Configuration =
    { minRetryAfterMillis : Float
    , maxRetryAfterMillis : Float
    , backOffRate : Float
    }

millisPerSecond : Float
millisPerSecond = 1000

{-| Default [`Configuration`](#Configuration)
* `minRetryAfterMillis`: 10 seconds,
* `maxRetryAfterMillis`: 160 seconds,
* `backOffRate`: 2, i.e. twice the time interval will be waited before
subsequent retry attempts.
-}
defaultConfiguration : Configuration
defaultConfiguration =
    { minRetryAfterMillis = 10 * millisPerSecond
    , maxRetryAfterMillis = 160 * millisPerSecond + 1
    , backOffRate = 2
    }

{-| Current state of polling.
Whenever entering or re-entering the state `Polling`, a request task will
be executed.

`QuittingPolling` is that state between a call to
[`stop`](#stop) and the next result that comes back from a
currently outstanding retry, only after which the state will be finally
`NotPolling`. During this `QuittingPolling` state, any new calls to
`start` will be ignored.
-}
type State =
    Polling
    | NotPolling
    | RetryAfterMillis Float
    | QuittingPolling

{-| Model to track polling state, [`init`](#init)ialized with
[`Configuration`](#Configuration) and some necessary
functions.

Use [`stateOf`](#stateOf) to figure out the current [`State`](#State).
-}
type alias Model error value msg =
    { configuration : Configuration
    , requestTask : () -> Task.Task error value
    , decidePolling : Result error value -> PollingDecision
    , createMessage : Result error value -> msg
    , state : State
    }

{-| The current state of polling.
-}
stateOf : Model error value msg -> State
stateOf model =
    model.state

currentRetryAfterMillis : Model error value msg -> Float
currentRetryAfterMillis model =
    case model.state of
        RetryAfterMillis millis ->
            millis
        _ ->
            model.configuration.minRetryAfterMillis

{-| Decision to bring the model from previous polling state to the next one.
-}
type PollingDecision =
      PollOne
    | QuitPolling
    | RetryAfter


{-| Constructs a model to execute and track polling.

Arguments:
* `configuration`: min/max time intervals before retries as well as
  back-off rate, see [`Configuration`](#Configuration).
* `requestTask`: Provides the task to execute, for each try. Normally an
  `Http.task`, but could be anything, really.
* `decidePolling`: Based on the result of a try, decides the fate of the polling
  loop. Should a `HTTP 204 No Content` be interpreted as re-try immediately,
  or back-off, or quit the loop entirely? How about `HTTP 429 Too Many Requests`?
  In some cases, even a successfully obtained response (`HTTP 200 Ok`) could mean,
  based on the received contents, to quit the polling loop. Therefore such decision
  is application specific.
* `createMessage`: Turns the result of a try to a message of yours.

-}
init : Configuration -> (() -> Task.Task error value) -> (Result error value -> PollingDecision) -> (Result error value -> msg) -> Model error value msg
init configuration requestTask decidePolling createMessage =
    { configuration = configuration
    , requestTask = requestTask
    , decidePolling = decidePolling
    , createMessage = createMessage
    , state = NotPolling
    }

backOff : Model error value msg -> Model error value msg
backOff model =
    let
        millisCandidate = model.configuration.backOffRate * (currentRetryAfterMillis model)
        millis = min model.configuration.maxRetryAfterMillis millisCandidate
    in
        { model
        | state = RetryAfterMillis millis
        }

resetPolling : Model error value msg -> Model error value msg
resetPolling model =
    { model
    | state = Polling
    }


updateState : PollingDecision -> Model error value msg -> Model error value msg
updateState pollingDecision model =
    case (pollingDecision, model.state) of
        (PollOne, QuittingPolling) ->
            { model | state = NotPolling }

        (PollOne, _) ->
            resetPolling model

        (QuitPolling, NotPolling) ->
            model

        (QuitPolling, _) ->
            { model | state = QuittingPolling }

        (RetryAfter, QuittingPolling) ->
            { model | state = NotPolling }

        (RetryAfter, _) ->
            backOff model


cmdFor : Model error value msg -> Cmd msg
cmdFor model =
    case model.state of
        Polling ->
            Process.sleep 0
                |> Task.andThen model.requestTask
                |> Task.attempt model.createMessage

        NotPolling ->
            Cmd.none

        RetryAfterMillis millis ->
            Process.sleep millis
                |> Task.andThen model.requestTask
                |> Task.attempt model.createMessage

        QuittingPolling ->
            Cmd.none


{-| Puts the model into polling state and produces a command with an HTTP task.
-}
start : Model error value msg -> (Model error value msg, Cmd msg)
start model =
    let
        newModel = updateState PollOne model
    in
        ( newModel
        , cmdFor newModel
        )

{-| If currently in any of the states: `Polling`, `RetryAfterMillis` or
`QuittingPolling`, it will put to the state `QuittingPolling` (so that the
next result will put to final `NotPolling`).

If already in state `NotPolling`, remains in `NotPolling`.
-}
stop : Model error value msg -> (Model error value msg, Cmd msg)
stop model =
    let
        newModel = updateState QuitPolling model
    in
        ( newModel
        , cmdFor newModel
        )

{-| Updates the model, based on the results of the previous polling that you feed back.

Invoke this function on the current polling model whenever you get your message
carrying a `Result error value`, to keep things rolling.
-}
update : (Result error value) -> Model error value msg -> (Model error value msg, Cmd msg)
update result model =
    let
        pollingDecision = model.decidePolling result
        newModel = updateState pollingDecision model
    in
        ( newModel
        , cmdFor newModel
        )
