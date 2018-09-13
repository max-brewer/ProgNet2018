module App.View


open Elmish
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fulma
open Fulma.FontAwesome
open Fable.PowerPack
open Fable.PowerPack.Fetch


open Elmish.React
open Elmish.Debug
open Elmish.HMR

type Model =
    { PokemonDisplayModel1 : Pokemon.Model
      PokemonDisplayModel2 : Pokemon.Model }

type Msg =
    | UpdatePokemonDisplay1 of Pokemon.Msg
    | UpdatePokemonDisplay2 of Pokemon.Msg

let init _ =
    let pokemonModel1, pokemonCmds1 =
        Pokemon.init ()
    let pokemonModel2, pokemonCmds2 =
        Pokemon.init ()

    let mapped1 = Cmd.map UpdatePokemonDisplay1 pokemonCmds1
    let mapped2 = Cmd.map UpdatePokemonDisplay2 pokemonCmds2

    { PokemonDisplayModel1 = pokemonModel1
      PokemonDisplayModel2 = pokemonModel2 },

    Cmd.batch [mapped1; mapped2]

let update msg model =
    match msg with
    | UpdatePokemonDisplay1 pmsg ->
        let pokemonModel, pokemonCmds =
            Pokemon.update pmsg model.PokemonDisplayModel1
        { model with PokemonDisplayModel1 = pokemonModel },
        Cmd.map UpdatePokemonDisplay1 pokemonCmds
    | UpdatePokemonDisplay2 pmsg ->
        let pokemonModel, pokemonCmds =
            Pokemon.update pmsg model.PokemonDisplayModel2
        { model with PokemonDisplayModel2 = pokemonModel },
        Cmd.map UpdatePokemonDisplay2 pokemonCmds


let view model dispatch =
    let pokemonView1 =
        Pokemon.view model.PokemonDisplayModel1
            (fun m -> dispatch (UpdatePokemonDisplay1 m))

    let pokemonView2 =
        Pokemon.view model.PokemonDisplayModel2
            (fun m -> dispatch (UpdatePokemonDisplay2 m))

    Container.container [] [
        pokemonView1
        pokemonView2 ]


Program.mkProgram init update view
#if DEBUG
|> Program.withHMR
#endif
|> Program.withReactUnoptimized "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
