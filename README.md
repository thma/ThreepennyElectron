# Writing Haskell native GUI Applications with Threepenny GUI and Electron.

## tl;dr

Threepenny is an awesome Haskell library for creating browser based application running on localhost.

By combining it with the Electron.js framework you have a great toolset for writing cross-platform standalone GUI applications.

See it in action:

```bash
git clone https://github.com/thma/ThreepennyElectron.git
cd ThreepennyElectron
stack init
npm install
npm start
```

## Immature support for writing Desktop Applications in Haskell ?

Since reading The GUI chapter in [Real World Haskell](http://book.realworldhaskell.org/read/gui-programming-with-gtk-hs.html) 
I had the impression that Haskell does not excel in GUI programming.
The GUI examples in Real World Haskell are based on  [gtk2hs](https://github.com/gtk2hs/gtk2hs). Gtk2hs is a Haskell library that allows to write 
platform-independent GUI applications based on the GTK library.
There are some large applications based on gtk2hs or its successor [gi-gtk-hs](https://github.com/haskell-gi/gi-gtk-hs) like the 
Haskell IDE [Leksah](http://leksah.org/). It's rock solid technology. But it's also quite dated and the imperative programming model 
is not an ideal fit for a purely functional language like Haskell.

So even though I'm a Haskell enthusiast I tended to agree with [Gabriel Gonzales "Immature" rating](https://github.com/Gabriel439/post-rfc/blob/master/sotu.md#standalone-gui-applications) 
of Haskell's support for standalone GUI Applications.  

## Threepenny to the rescue
A few weeks back I was asked to write a standalone GUI version of an existing Haskell commandline app. So I had to make up
my mind about choosing a GUI library with an optimal fit to my needs:

- provide a multi-platform (Windows, MacOS, Linux) standalone GUI application.
- use functional reactive programming instead of event handler callbacks
- provide a modern look and feel e.g. material design

I never was satisfied with the look and feel of GTK based applications.
And I also wasn't keen on going back to callback based UI programming.
So I had a look at Gabriel Gonzalez great resource 
[State of the Haskell ecosystem](https://github.com/Gabriel439/post-rfc/blob/master/sotu.md).

In the [section on Standalone GUI applications](https://github.com/Gabriel439/post-rfc/blob/master/sotu.md#standalone-gui-applications) 
he mainly mentions GTK and Qt bindings but also some other libraries.
One of them is [Threepenny GUI](http://wiki.haskell.org/Threepenny-gui) which caught my attention because it uses 
the web browser as a display. And it comes with an (optional) functional reactive programming model!

> A program written with Threepenny is essentially a small web server that displays the user interface as a web 
> page to any browser that connects to it. You can freely manipulate the HTML DOM and handle JavaScript events 
> from your Haskell code.
>
> (Quoted from the  [hackage documentation](https://hackage.haskell.org/package/threepenny-gui))

My next thought was: It would be cool to use [Electron](https://electronjs.org/) to host the Threepenny HTML/Javascript 
frontend against the Threepenny Haskell backend. By making use of the [electron packager](https://www.npmjs.com/package/electron-packager) 
this would allow to package platform specific standalone GUI application for Windows, MacOS and Linux. 

I really got excited when I found out that Heinrich Apfelmus (the autor of Threppenny GUI) already had already  written a short 
[HOWTO document](https://github.com/HeinrichApfelmus/threepenny-gui/blob/master/doc/electron.md) that 
explains the required steps to glue an ELectron frontend to a Threepenny backend.

Based on this Howto I was able to deliver a native windows GUI Application with an embed Haskell backend within just a few days.
I received very good feedback from the endusers and my impression was:

>"Thanks to Threepenny GUI support in Haskell has just become a bit more mature!"

As I did not find much coverage of this specific Electron / Threepenny GUI combination in blogs or tutorials I thought it 
would be a good idea to spread the word by writing a short tutorial featuring the basic building blocks of this approach.

So without further ado let's get started:

## Writing a platform independent standalone calculator app

In order to provide a bit more than just a hello world example I'm showcasing a simple pocket calculator app. This allows to demonstrate basic features of writing real world UI applications. The calculator is based on an earlier [Threepenny GUI demo by Aleksey Pirogov](https://bitbucket.org/astynax/threep/src/default/).

The UI of the calculator is shown in the screenshot below. It features a display, a numeric block for entering digits and a decimal point, buttons for the four basic arithmetical operations, a **clear** button and a **clear error** button:

![screenshot of the calculator](screenshot.png)


### The calculator

At the heart of an application sits the model. In this case the [calculator](src/Calc.hs). It is implemented as a simple state machine. The state machine knows five different states:

1. Entering a number into the A register
2. Finishing the entry of the first number by entering an Operation (+, -, *, /)
3. Entering a number into the second register
4. Finishing the Operation of the second number by entering **=** or another arithmetic operation
5. an Error state in case of divison by zero or by entering a wrong sequence of buttons 

This is reflected in the following data type declaration:

```haskell
data State = EnteringA     Entering                    -- entering A
           | EnteredAandOp Double  Operation           -- A, Op
           | EnteringB     Double  Operation Entering  -- A, Op, entering B
           | Calculated    Double  Operation Double    -- A, Op, B
           | Error         Double  String              -- A, Message
           deriving (Show, Eq)

type Entering = (String, Bool) -- A tuple of the String representation of the entered digits and a flag signalling that **.** has already been pressed.
```

Starting with an initial state 

```haskell
initialState :: State
initialState = EnteringA ("0", False)
```

we can use the calculator by populating it with Button events:

```haskell
-- in GHCi:
> populate "9" initialState
EnteringA ("09",False)
> populate "9" it
EnteringA ("099",False)
> populate "/" it
EnteredAandOp 99.0 Div
> populate "7" it
EnteringB 99.0 Div ("7",False)
> populate "=" it
Calculated 14.142857142857142 Div 7.0
```

The `populate` function is defined as :

```haskell
populate :: String -> State -> State
populate i =
  case parseInput i of
    Digit x      -> addDigit x
    Dot          -> addDot
    Operation op -> applyOp  op
    cmd          -> applyCmd cmd
```

First the input is parsed to a Command. Based on the parsed Command 
(either a digit, a dot, an arithmetic operation or `=`, `C` or `CE`) the current state is modified
by one of the functions `addDigit`, `addDot`, `applyOp` or `applyCmd`.

I won't dive deeper into those functions, as you will easily grasp the mechanism by studying the [source code](src/Calc.hs).

## The Threepenny GUI

I will not give an introduction to the Threepenny GUI programming model here as 
Threepenny already ships with [plenty of samples](https://github.com/HeinrichApfelmus/threepenny-gui/tree/master/samples) and a good [getting started tutorial](https://github.com/HeinrichApfelmus/threepenny-gui/tree/master/doc/hal-2017). 
Instead I will focus on presenting only those parts that are necessary to understand the calculator GUI.   

The application Main module consists of a single function `main`. It reads a port number from the commandline an then call `Ui.start` to launch a WebServer hosting the Ui application on that port:

```haskell
main :: IO ()
main = do
  [port] <- getArgs
  Ui.start (read port)
```

This function will either be called when starting the application with `stack exec ThreepennyElectron 8080` or by the electron launch script main.js (which we will discuss later).

The `Ui` module contains all code for rendering the HTML dom, setting up the event binding to GUI widgets and the respective interaction with application backend.

Let's start with the main entry point `Ui.start` which is called on application launch:

```haskell
start :: Int -> IO ()
start port = startGUI defaultConfig
    { jsPort   = Just port
    , jsStatic = Just "static"
    } setup
```

It takes the port number as parameter and starts up a web server with the Threepenny `startGUI` function.
`startGUI` has the folloing type signature: 

```haskell
-- | Start server for GUI sessions.
startGUI
    :: Config               -- ^ Server configuration.
    -> (Window -> UI ())    -- ^ Action to run whenever a client browser connects.
    -> IO ()
```

We build our server configuration by starting with the default configuration `defaultConfig` and 
then modifying two properties:

1. setting the port number to the one read from the command line. 
2. declaring the static content (i.e any html, JavaScript and CSS content) to reside in the directory `./static`.

The `(Window -> UI ())` action parameter is filled with the function `setup`.

Obviously this function must have the following signature:

```haskell
-- | setup window layout and event handling
setup :: Window -> UI ()
```

As this function defines the whole layout and user interaction we will inspect it step by step.
The first step is to define the UI elements the overall window layout:

```haskell
setup win = void $ do
  -- define page + stylesheet
  return win # set title "3PennyCalc"
  UI.addStyleSheet win "semantic.css"
```

We start by assigning a title to the window `win` and adding a stylesheet. In our example we are using the [Semantic UI](https://semantic-ui.com/) stylesheet. (You could of course use any other css framework or roll your own.)

Next we define the calculator display element `outputBox` as a `UI.input` element. These elements willbe rendered as HTML DOM elements in the browser. Threepenny provides combinators to define css classes and other html attributes. In this case we set the input field to readonly, make the text align to the right and set its width:

```haskell
  -- define UI controls
  outputBox <- UI.input
                # set (attr "readonly") "true"
                # set (attr "style") "text-align: right; min-width: 324px"
```

This is resulting HTML DOM element:

```html
<input readonly="readonly" style="text-align: right; min-width: 324px">
```

In the next step we define the calculator buttons for digits, operations and commands:

```haskell
  -- define the button grid
  buttons   <- mapM (mapM mkButton) buttonLabels

  where
    mkButton :: (String, Color) -> UI Element
    mkButton (s, c) =
      UI.button #. ("ui " ++ color c ++ " button") 
                # set text s # set value s 
                # set (attr "type") "button" 
                # set (attr "style") "min-width: 60px"

    color :: Color -> String
    color = map toLower . show
                  
    buttonLabels :: [[(String, Color)]]
    buttonLabels =
      [ [(lbl $ Digit Seven, Grey), (lbl $ Digit Eight, Grey), (lbl $ Digit Nine, Grey),  (lbl   ClearError, Orange),   (lbl   Clear, Orange)]
      , [(lbl $ Digit Four, Grey),  (lbl $ Digit Five, Grey),  (lbl $ Digit Six, Grey),   (lbl $ Operation Add, Brown), (lbl $ Operation Sub, Brown)]
      , [(lbl $ Digit One, Grey),   (lbl $ Digit Two, Grey),   (lbl $ Digit Three, Grey), (lbl $ Operation Mul, Brown), (lbl $ Operation Div, Brown)]
      , [(lbl   Dot, Grey),         (lbl $ Digit Zero, Grey),  (lbl   Flush, Black)] ]

-- | Button colors
data Color = Grey | Orange | Brown | Black deriving (Show)
```

We start with a list of lists of `(String, Color)` tuples `buttonLabels :: [[(String, Color)]]`. The outer list represents the rows, the inner list the columns in each row. The tuples represent the labels and colors we want to see on the calculator buttons.

Mapping the function `mkButton` over the `buttonLabels` is then used to create the `buttons :: [[UI Element]]`. Where `mkButton` defines each button as a `UI.button`, assigns a css class `("ui " ++ color c ++ " button")` to it (using the `#.` combinator) and sets text and other attributes by using the `# set` combinator. 

To give an example the first element from `buttonLabels`: `(lbl $ Digit Seven, Grey)` will be rendered in the HTML DOM as:

```html
<button class="ui grey button" value="7" type="button" style="min-width: 60px">7</button>
```

As the last step of the layouting stage we glue everything together to a nice grid and place it as the HTML body into the DOM tree:

```haskell
  UI.getBody win # set (attr "style") "overflow: hidden" #+
    [ UI.div #. "ui raised very padded text container segment" #+
      [UI.table #+ [UI.row [UI.div #. "ui input focus" #+ [element outputBox]]] #+ 
                    map (UI.row . map element) buttons]
    ]
```

```html
<body style="overflow: hidden">
<noscript>Please enable JavaScript.</noscript>


<div class="ui raised very padded text container segment">
    <table>
        <div class="table">
            <div class="table-row">
                <div class="table-cell">
                    <div class="ui input focus"><input readonly="readonly" style="text-align: right; min-width: 324px">
                    </div>
                </div>
            </div>
        </div>
        <div class="table">
            <div class="table-row">
                <div class="table-cell">
                    <button class="ui grey button" value="7" type="button" style="min-width: 60px">7</button>
                </div>
                <div class="table-cell">
                    <button class="ui grey button" value="8" type="button" style="min-width: 60px">8</button>
                </div>
                <div class="table-cell">
                    <button class="ui grey button" value="9" type="button" style="min-width: 60px">9</button>
                </div>
                <div class="table-cell">
                    <button class="ui orange button" value="CE" type="button" style="min-width: 60px">CE</button>
                </div>
                <div class="table-cell">
                    <button class="ui orange button" value="C" type="button" style="min-width: 60px">C</button>
                </div>
            </div>
        </div>

        ...

    </table>
</div>
</body>
```

## WIP
----



electron integration:
https://github.com/HeinrichApfelmus/threepenny-gui/blob/master/doc/electron.md


howto:

- npm install
- npm start   // this will also do a 'stack install --local-bin-path build' to build the haskell app

npm install


npm install electron-packager

./node_modules/.bin/electron-packager .

./node_modules/.bin/electron-packager . --ignore=app --ignore=src
