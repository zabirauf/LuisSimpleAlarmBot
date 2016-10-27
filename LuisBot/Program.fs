open Suave
open Suave.Successful
open Suave.Web
open Suave.Operators
open Suave.Filters

[<EntryPoint>]
let main argv =
    startWebServer LuisBot.LuisBot.config LuisBot.LuisBot.app
    0 // return an integer exit cod