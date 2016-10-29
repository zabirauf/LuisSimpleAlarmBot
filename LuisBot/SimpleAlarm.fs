namespace LuisBot

open Microsoft.Bot.Connector
open Microsoft.Bot.Builder.Dialogs
open Microsoft.Bot.Builder.Luis
open Microsoft.Bot.Builder.Luis.Models

open Microsoft.FSharp.Linq.NullableOperators

open System
open System.Collections.Generic
open System.Net
open System.Threading.Tasks

[<AutoOpen>]
module Async =
    let inline startAsPlainTask (work : Async<unit>) = Task.Factory.StartNew(fun () -> work |> Async.RunSynchronously)

module SimpleAlarm = 
    type Alarm = {
        When : DateTime;
        What : string;
    }

    type AlarmMap = Map<string, Alarm>

    type TimeRemainingInAlarm =
        | AlarmNotFound
        | AlarmExpired of Alarm
        | AlarmFound of Alarm * TimeSpan
        
    let defaultAlarm = {
        When = DateTime.UtcNow;
        What = "default"
    }

    module EntityName =
        let alarmTitle = "builtin.alarm.title"
        let alarmStartTime = "builtin.alarm.start_time"
        let alarmStartDate = "builtin.alarm.start_date"

        let defaultTitleEntity() = EntityRecommendation(``type`` = alarmTitle, entity = defaultAlarm.What)
        let defaultDateEntity() = EntityRecommendation(``type`` = alarmStartDate, entity = String.Empty)
        let defaultTimeEntity() = EntityRecommendation(``type`` = alarmStartTime, entity = String.Empty)
    
    module AlarmHelpers =
        let tryFindAlarm (result : LuisResult) (alarmMap : AlarmMap) =

            let title = match result.TryFindEntity(EntityName.alarmTitle) with
                | (true, title) -> title.Entity
                | (false, _) -> defaultAlarm.What

            alarmMap.TryFind(title)

        let deleteAlarm (result : LuisResult) (alarmMap : AlarmMap) =

            match tryFindAlarm result alarmMap with
            | Some alarm -> Some (alarm, alarmMap.Remove alarm.What)
            | None -> None

        let setAlarm (result : LuisResult) (alarmMap : AlarmMap) =

            let getEntityOrDefault entityName defaultEntity =
                match result.TryFindEntity(entityName) with
                | (true, entity) -> entity
                | (false, _) -> defaultEntity()

            let title = getEntityOrDefault EntityName.alarmTitle EntityName.defaultTitleEntity
            let date = getEntityOrDefault EntityName.alarmStartDate EntityName.defaultDateEntity
            let time = getEntityOrDefault EntityName.alarmStartTime EntityName.defaultTimeEntity

            let parser = Chronic.Parser()
            let span = parser.Parse(date.Entity + "" + time.Entity)

            match span with
            | null -> None
            | _ ->
                let ``when`` = match span.Start.HasValue with
                    | true -> span.Start.Value
                    | false -> span.End.Value

                let alarm = {defaultAlarm with What = title.Entity; When = ``when``}
                Some (alarmMap.Add(alarm.What, alarm), alarm)

        let snoozeAlarm (result : LuisResult) (alarmMap : AlarmMap) =

                match tryFindAlarm result alarmMap with
                | Some alarm -> 
                    let snoozedTime = 7.0 |> TimeSpan.FromMinutes |> alarm.When.Add
                    let snoozedAlarm = { alarm with When = snoozedTime } 
                    Some (alarmMap.Add(snoozedAlarm.What, snoozedAlarm), alarm)
                | None -> None

        let timeRemainingForAlarm (result : LuisResult) (alarmMap : AlarmMap) =
            match tryFindAlarm result alarmMap with
            | Some alarm when alarm.When > DateTime.UtcNow -> AlarmFound (alarm, (alarm.When.Subtract DateTime.UtcNow))
            | Some alarm -> AlarmExpired alarm
            | _ -> AlarmNotFound

        let turnOffAlarm (alarm : Alarm) (alarmMap : AlarmMap) = 
            match alarmMap.TryFind(alarm.What) with
            | Some alarm -> 
                Some (alarmMap.Remove(alarm.What), alarm)
            | None -> None

    [<LuisModel("c413b2ef-382c-45bd-8ff0-f76d60e2a821", "6d0966209c6e4f6b835ce34492f3e6d9")>]
    [<Serializable>]
    type SimpleAlarmDialog() = 
        inherit LuisDialog<Object>()

        let awaitTask = Async.AwaitIAsyncResult >> Async.Ignore >> ignore

        let sendMessage (context : IDialogContext) (message : string) = context.PostAsync(message) |> awaitTask

        let toMap dictionary = 
            (dictionary :> seq<_>)
            |> Seq.map (|KeyValue|)
            |> Map.ofSeq

        member val alarmDict = Dictionary<string, Alarm>() with get, set

        member this.getAlarmMap() = toMap this.alarmDict

        member this.setAlarmMap(updatedMap : AlarmMap) = 
            let dictin = Dictionary<string, Alarm>()
            updatedMap |> Map.iter (fun key value -> dictin.Add(key, value))
            this.alarmDict <- dictin
            printfn "Updated dict to %A" this.alarmDict

        member this.MessageReceived context messageActivity = base.MessageReceived(context, messageActivity)
            

        [<LuisIntent("")>]
        member this.None (context : IDialogContext) (result : LuisResult) = Async.startAsPlainTask (async {
            let intents = result.Intents |> Seq.map(fun i -> i.Intent)
            let intentString = String.Join(",", intents)
            let message = sprintf "Sorry I did not understand: %s" intentString

            message
            |> sendMessage(context)

            ResumeAfter(this.MessageReceived)
            |> context.Wait
        })

        [<LuisIntent("builtin.intent.alarm.delete_alarm")>]
        member this.DeleteAlarm (context : IDialogContext) (result : LuisResult) = Async.startAsPlainTask (async {
            let message = match AlarmHelpers.deleteAlarm result (this.getAlarmMap()) with
                | Some (alarm, updatedMap) -> 
                    updatedMap |> this.setAlarmMap
                    sprintf "Found alarm %s" alarm.What
                | None -> sprintf "Did not find alarm"

            message
            |> sendMessage(context)

            ResumeAfter(this.MessageReceived)
            |> context.Wait
        })

        [<LuisIntent("builtin.intent.alarm.find_alarm")>]
        member this.FindAlarm (context : IDialogContext) (result : LuisResult) = Async.startAsPlainTask (async {
            let message = match AlarmHelpers.tryFindAlarm result (this.getAlarmMap()) with
                | Some alarm -> sprintf "Found alarm %A" alarm 
                | None -> sprintf "Did not find alarm"

            message
            |> sendMessage(context)

            ResumeAfter(this.MessageReceived)
            |> context.Wait
        })

        [<LuisIntent("builtin.intent.alarm.set_alarm")>]
        member this.SetAlarm (context : IDialogContext) (result : LuisResult) = Async.startAsPlainTask (async {
            let message = match AlarmHelpers.setAlarm result (this.getAlarmMap()) with
                | Some (updatedMap, alarm) -> 
                    updatedMap |> this.setAlarmMap
                    sprintf "Alarm %A created" alarm
                | None -> "Count not find time in alarm"

            message
            |> sendMessage(context)

            ResumeAfter(this.MessageReceived)
            |> context.Wait
        })

        [<LuisIntent("builtin.intent.alarm.snooze")>]
        member this.SnoozeAlarm (context : IDialogContext) (result : LuisResult) = Async.startAsPlainTask (async {
            let message = match AlarmHelpers.snoozeAlarm result (this.getAlarmMap()) with
                | Some (updatedMap, alarm) ->
                    updatedMap |> this.setAlarmMap
                    sprintf "Alarm %A snoozed" alarm
                | None -> "Did not find alarm"
            
            message
            |> sendMessage(context)

            ResumeAfter(this.MessageReceived)
            |> context.Wait
        })

        [<LuisIntent("builtin.intent.alarm.time_remaining")>]
        member this.TimeRemainingInAlarm (context : IDialogContext) (result : LuisResult) = Async.startAsPlainTask (async {
            let message = match AlarmHelpers.timeRemainingForAlarm result (this.getAlarmMap()) with
                | AlarmFound (alarm, timespan) -> sprintf "There is %A remaining for alarm %A" timespan alarm
                | AlarmExpired alarm -> sprintf "The alarm %A expired already" alarm
                | AlarmNotFound -> "Did not find alarm"

            message
            |> sendMessage(context)

            ResumeAfter(this.MessageReceived)
            |> context.Wait
        })

        [<LuisIntent("builtin.intent.alarm.turn_off_alarm")>]
        member this.TurnOffAlarm (context : IDialogContext) (result : LuisResult) = Async.startAsPlainTask (async {
            match AlarmHelpers.tryFindAlarm result (this.getAlarmMap()) with
                | Some alarm ->
                    PromptDialog.Confirm(context, ResumeAfter(this.AfterConfirmingTurnOffAlarm alarm), "Are you sure?", promptStyle = PromptStyle.None)
                | None -> 
                    sprintf "Did not find alarm"
                    |> sendMessage(context)

                    ResumeAfter(this.MessageReceived)
                    |> context.Wait
        })

        member this.AfterConfirmingTurnOffAlarm (alarm : Alarm) (context : IDialogContext) (argument : IAwaitable<bool>) = Async.startAsPlainTask (async {
            let confirmation = argument.GetAwaiter().GetResult()
            let message = match confirmation with
            | true ->
                match AlarmHelpers.turnOffAlarm alarm (this.getAlarmMap()) with
                | Some (updatedAlarmMap, _) -> updatedAlarmMap |> this.setAlarmMap

                sprintf "Ok, alarm %A was disabled" alarm
            | false -> "Ok! We haven't modified your alarms!"

            message
            |> sendMessage(context)

            ResumeAfter(this.MessageReceived)
            |> context.Wait
        })

        
        [<LuisIntent("builtin.intent.alarm.alarm_other")>]
        member this.AlarmOther(context : IDialogContext) (result : LuisResult) = Async.startAsPlainTask (async {
            printfn "The map of alarms is %A" (this.getAlarmMap())
            "What ?"
            |> sendMessage(context)

            ResumeAfter(this.MessageReceived)
            |> context.Wait
        })