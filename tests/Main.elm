-- 'port' module req'd for node runner?


module Main exposing (..)

import Tests


-- import Test.Runner.Node exposing (TestProgram, run)

import Test.Runner.Html exposing (TestProgram, run)
import Json.Encode exposing (Value)


main : TestProgram
main =
    run Tests.all



-- port emit : ( String, Value ) -> Cmd msg
