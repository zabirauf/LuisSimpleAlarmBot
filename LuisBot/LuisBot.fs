namespace LuisBot
open Suave
open Suave.Successful
open Suave.Web
open Suave.Operators
open Suave.Filters
open Newtonsoft.Json
open Newtonsoft.Json.Serialization

open Microsoft.Bot.Connector
open Microsoft.Bot.Builder.Dialogs
open Microsoft.Bot.Builder.Luis

open System
open System.Net
open System.Threading.Tasks

module LuisBot =

    [<AutoOpen>]
    module Helpers = 
        let toJson v =
            let jsonSerializerSettings = new JsonSerializerSettings()
            jsonSerializerSettings.ContractResolver <- new CamelCasePropertyNamesContractResolver()

            JsonConvert.SerializeObject(v, jsonSerializerSettings) |> OK
            >=> Writers.setMimeType "application/json; charset=utf-8"

        let fromJson<'a> json =
            JsonConvert.DeserializeObject(json, typeof<'a>) :?> 'a

        let getResourceFromReq<'a> (req : HttpRequest) =
            let getString rawForm =
                System.Text.Encoding.UTF8.GetString(rawForm)
            req.rawForm |> getString |> fromJson<'a>
            
        let awaitTask = Async.AwaitIAsyncResult >> Async.Ignore
      
    let botHandler (msg : Activity) =
        printfn "Handling the message from the bot with text -> \"%s\"" msg.Text
        async {
            let! m = Conversation.SendAsync(msg, (fun _ -> LuisBot.SimpleAlarm.SimpleAlarmDialog() :> IDialog<Object>), Threading.CancellationToken()) |> awaitTask
            m
        } |> Async.RunSynchronously 
        
    let hearbeet _ = 
        printfn "Saying hello world from F# and flynn %O" DateTime.UtcNow
        OK (sprintf "<html><body><h1>Hello World from %O</h1></body></html>" DateTime.UtcNow)

    let app = 
        choose [
            POST >=> path "/api/messages" >=> request (getResourceFromReq >> botHandler >> toJson )
            GET >=> path "/heartbeet" >=> request hearbeet]

    let config = 
        let port = System.Environment.GetEnvironmentVariable("PORT")
        let ip127  = IPAddress.Parse("127.0.0.1")
        let ipZero = IPAddress.Parse("0.0.0.0")

        { defaultConfig with 
            logger = Logging.Loggers.saneDefaultsFor Logging.LogLevel.Verbose
            bindings=[ (if port = null then HttpBinding.mk HTTP ip127 (uint16 8080)
                        else HttpBinding.mk HTTP ipZero (uint16 port)) ] }
                    