module othello.Client.Main

open System
open Elmish
open Bolero
open Bolero.Html
open Bolero.Remoting
open Bolero.Remoting.Client
open Bolero.Templating.Client


/// Routing endpoints definition.
type Page =
    | [<EndPoint "/">] Home
    | [<EndPoint "/counter">] Counter
    | [<EndPoint "/data">] Data
    | [<EndPoint "/othello">] Othello

/// The Elmish application's model.
type Model =
    {
        page: Page
        counter: int
        booksModel : Books.Model
        error: string option
        username: string
        password: string
        signedInAs: option<string>
        signInFailed: bool

        board: Othello.Board.State
        player : Othello.Board.Color
    }

let initModel =
    {
        page = Home
        counter = 0
        booksModel = Books.initModel
        error = None
        username = ""
        password = ""
        signedInAs = None
        signInFailed = false
        board = Othello.Board.create()
        player = Othello.Board.Color.White
    }

/// The Elmish application's update messages.
type Message =
    | SetPage of Page
    | BooksMessage of Books.Message
    | Increment
    | Decrement
    | SetCounter of int
    | Error of exn
    | ClearError
    | MakeMove of int*int

let update remote message model =
    match message with
    | SetPage page ->
        { model with page = page }, Cmd.none
    | BooksMessage msg ->
        let (a,b) = Books.update remote msg model.booksModel
        { model with booksModel = a}, Cmd.map BooksMessage b
    | Increment ->
        { model with counter = model.counter + 1 }, Cmd.none
    | Decrement ->
        { model with counter = model.counter - 1 }, Cmd.none
    | SetCounter value ->
        { model with counter = value }, Cmd.none
    | MakeMove (x,y) as pos ->
        {model with board = (Othello.Board.move (x,y) model.player model.board); player = Othello.Board.flipColor model.player}, Cmd.none
    | Error RemoteUnauthorizedException ->
        { model with error = Some "You have been logged out."; signedInAs = None }, Cmd.none
    | Error exn ->
        { model with error = Some exn.Message }, Cmd.none
    | ClearError ->
        { model with error = None }, Cmd.none

/// Connects the routing system to the Elmish application.
let router = Router.infer SetPage (fun model -> model.page)

type Main = Template<"wwwroot/main.html">

let homePage model dispatch =
    Main.Home().Elt()

let counterPage model dispatch =
    Main.Counter()
        .Decrement(fun _ -> dispatch Decrement)
        .Increment(fun _ -> dispatch Increment)
        .Value(model.counter, SetCounter >> dispatch)
        .Elt()

open Othello.Board

let whiteCell() = Main.White().Elt()
let blackCell() = Main.Black().Elt()
let emptyCell() = Main.EmptyCell().Elt()

let cell (x,y) model dispatch =
    match get (x,y) model.board with
    | Some White -> whiteCell()
    | Some Black -> blackCell()
    | None when Othello.Board.isMoveValid (x,y) model.player model.board ->  
                Main.ValidMove()
                    .OnClick(fun args -> dispatch (MakeMove (x,y)))
                    .Elt()
    | _ -> emptyCell()

let othelloPage model dispatch = 
    Main.Othello()
        .Player(model.player |> string)
        .Table(concat 
            [for y in rows do
                yield tr [] [for x in cols do yield cell (x,y) model dispatch]
                ])
        .Elt()



let menuItem (model: Model) (page: Page) (text: string) =
    Main.MenuItem()
        .Active(if model.page = page then "is-active" else "")
        .Url(router.Link page)
        .Text(text)
        .Elt()

let view model dispatch =
    Main()
        .Menu(concat [
            menuItem model Home "Home"
            menuItem model Counter "Counter"
            menuItem model Data "Books service"
            menuItem model Othello "Othello"
        ])
        .Body(
            cond model.page <| function
            | Home -> homePage model dispatch
            | Counter -> counterPage model dispatch
            | Data -> Books.view model.booksModel (BooksMessage >> dispatch)
            | Othello -> othelloPage model dispatch
        )
        .Error(
            cond model.error <| function
            | None -> empty
            | Some err ->
                Main.ErrorNotification()
                    .Text(err)
                    .Hide(fun _ -> dispatch ClearError)
                    .Elt()
        )
        .Elt()

type MyApp() =
    inherit ProgramComponent<Model, Message>()

    override this.Program =
        let bookService = this.Remote<Books.BookService>()
        let update = update bookService
        Program.mkProgram (fun _ -> initModel, Cmd.ofMsg (BooksMessage Books.GetSignedInAs)) update view
        |> Program.withRouter router
#if DEBUG
        |> Program.withHotReload
#endif
