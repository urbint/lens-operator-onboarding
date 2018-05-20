Lens Operator Onboarding


The Why
-------

Lenses are immensely useful to the Haskell programmer, but suffer from a discovery problem - without enough exposure or experience, it's hard to know which operator to use in a given situation. This post provides explanations for a handful of operators as well as an example-driven reference for developers just getting started with lenses.

There is plenty of prior art for lens-related blog posts - we've pulled together a short list at the bottom of this post (including [one from another Urbinite](lookatmeimalinktoanarticle!)). The [Lens Library]() also provides nice documentation and some usage examples, once you know what you're looking for.

The motivation I have for writing this post is that I feel that I could have learned how to use Lens faster. There are useful mnemonics associated with its operators that I learned primarily by word of mouth. I spent too long staring blankly at Lens's `Ix/At` and `Folded` operators, frustrated that all I wanted was to list the values in my HashMap. Lens makes this very easy, but only if you've done it before.


Search
------

First things first - you need instant operator documentation lookup. If you're on a Mac, this can be done via [Alfred]() and [Dash](). Setup is on your own, but to motivate you, searching for a lens operator like `^.` can be done via `[cmd]+[space] hs ^.`.

[Screenshots]

This workflow feature is crucial for quickly looking up operators you don't know, which should help you feel like `!?!?!?`, and more like `^?!`.

Tooling like this is critical for searching for obscure operators and super generic function names. "get" and "set" are not quite specific enough for today's google.

If an OS built-in tool is not available, the "Index" pages on hackage can also make quick work of finding an operator exposed by a library. Here's the [index page for the lens library](linkmemofo).


Literate Haskell
----------------

This was originally written as a Literate Haskell file (.lhs), which can be compiled and run like any other Haskell file. The source document can be found [here](linksandshit). Please don't mind the setup here: it'll make it easier to hop straight into live examples in context.

Create a module and require a few packages, namely 'Control.Lens':

\begin{code}
{-# LANGUAGE OverloadedStrings #-}

module LensCheatSheet where

import           Control.Lens

import           Data.Text
import qualified Data.Text as T
import qualified Data.HashMap.Lazy as HM
\end{code}

Provide a few data types to work with.

\begin{code}
-- | A newtype wrapper to force UserNames to be labeled, and prevent us from
-- passing the wrong type of name around.
newtype UserName = UserName Text deriving (Show, Eq)
newtype PetName = PetName Text deriving (Show, Eq)

-- | A type alias to improve the readability of our types.
-- An inventory is a HashMap with a Text key and Item value.
type Inventory = HM.HashMap Text Item

-- | A User record.
data User
  = User
  { _userName :: UserName
  , _userScore :: Int
  , _userPet :: Maybe Pet
  , _userInventory :: Inventory
  } deriving (Show, Eq)

data Pet
  = Pet
  { _petName :: PetName } deriving (Show, Eq)

-- | An Item record.
data Item
  = Item
  { _itemValue :: Int
  , _itemWeight :: Int
  } deriving (Show, Eq)
\end{code}

Now let's write some lenses for our data types.

Note that it is possible to generate these with Template Haskell!

\begin{code}
-- | A lens from a User to Text.
--
-- Written quite explicitly with getter and setter helper functions to expose
-- Lens's nature.
userName :: Lens' User UserName
userName = lens getter setter
  where
    getter user = _userName user
    setter user newName = user { _userName = newName }

score :: Lens' User Int
score = lens _userScore (\user newScore -> user { _userScore = newScore })

-- | Note that this lens targets a 'Maybe Pet'.
pet :: Lens' User (Maybe Pet)
pet = lens _userPet (\user maybePet -> user {_userPet = maybePet})

-- | Single letter vars, seriously? Yep.
inventory :: Lens' User Inventory
inventory = lens _userInventory (\u i -> u { _userInventory = i })

------------------------------------------------------------------------------

petName :: Lens' Pet PetName
petName = lens _petName (\p n -> p { _petName = n })

------------------------------------------------------------------------------

value :: Lens' Item Int
value = lens _itemValue (\i v -> i { _itemValue = v })

weight :: Lens' Item Int
weight = lens _itemWeight (\i w -> i { _itemWeight = w })
\end{code}

Great. Let's get into how to use these bad boys.

Operators and Mnemonics

  > Note that there are fancier versions of most of these for dealing with StateT and ReaderT MonadTransformers. We're not going to touch those at all here.

`view (^.)` and `preview (^?)`

View is used for applying the 'getter' in your lenses to the 'a' of your choice.

The use of the carrot `^` is quite literal - it's an upside down `v`, as in `view`. If that's not mnemoic enough, wait until we get to `over` as `%` as `mod` as `modify`. You'll be all, "smh Ed Kemmet, you are one cheeky operator."

\begin{code}
viewExamples :: IO ()
viewExamples = do
  let bob = User (UserName "Bob") 42 Nothing HM.empty

  print "Bob's name is: "
  print $ view userName bob
  -- UserName "bob"
  print $ bob ^. userName
  -- UserName "bob"

  print "Bob's score is: "
  print $ view score bob
  -- 42
  print $ bob ^. score
  -- 42
\end{code}

The best part about Lenses, of course, is that they compose!

\begin{code}
composedViewExamples :: IO ()
composedViewExamples = do
  let
    bob = User (UserName "bob") 42 Nothing HM.empty
    fitzgerald = Pet (PetName "Fitzgerald")
    jeff = User (UserName "jeff") 42 (Just fitzgerald) HM.empty

  print "Bob's pet's name is: "
  print $ preview (pet . _Just . petName) bob
  -- Nothing
  print $ bob ^? pet . _Just . petName
  -- Nothing

  print "Jeff's pet's name is: "
  print $ preview (pet . _Just . petName) jeff
  -- Just (PetName "Fitzgerald")
  print $ jeff ^? pet . _Just . petName
  -- Just (PetName "Fitzgerald")
\end{code}

But what's this? `_Just`? `preview` and `^?`!? Did you just drop a new operator out of nowhere? Welcome to Haskell, where the operators are fun toy things just anyone can drop in anywhere! Whenever you see a new operator, I encourage you to laugh maniacally. It might help.

`preview` (and its infix version (`^?`)) are similar to `view` and (`^.`). The difference is that `preview` lets us walk over 'Fold's or 'Traversal's, in this case, the 'Maybe Pet'. `_Just` is a 'Prism' providing a 'Traversal' for targeting the `Just a` of a `Maybe a`. I'm sure you caught all of that. More examples please!

\begin{code}
previewExamples :: IO ()
previewExamples = do
  let maybeIntA = Just 1
  -- Have to tell the compiler this was a 'Maybe Int' for it to be printable
      maybeIntB = Nothing :: Maybe Int

  print "maybeIntA"
  print $ maybeIntA ^? _Just
  -- Just 1
  print "maybeIntB"
  print $ maybeIntB ^? _Just
  -- Nothing

  let justiceCity = Just 1
      crashCity = Nothing :: Maybe Int

  print "Unwrap this Maybe Int or die trying!"
  print $ justiceCity ^?! _Just
  -- 1
  print "Crash city!"
  -- print $ crashCity ^?! _Just
  -- This will throw an 'empty fold' exception. `^?!` can be useful for
  -- forcing Maybes to unwrap when writing tests.
\end{code}

I don't know if there's a named version for `^?!`. Maybe it's 'unsafePreview'? I'm sure there are plenty of snarky 'view/preview' related extended metaphors to explore. 'bangview'? Yikes.

`set (.~)`

Set lets us update values without lenses.

The `~` tilda should be read close to the `=` operator in an imperative - it sets the value on the left to be the target of the lens, and returns the updated object. For the interested, `=` is used for similar actions when working in a StateT context.

\begin{code}
setExamples :: IO ()
setExamples = do
  let bob = User (UserName "bob") 0 Nothing HM.empty

  print "Bob, with an updated score"
  print $ set score 42 bob
  -- User {_userName = UserName "bob", _userScore = 42, _userPet = Nothing, _userInventory = fromList []}
  print $ (score .~ 42) bob
  -- User {_userName = UserName "bob", _userScore = 42, _userPet = Nothing, _userInventory = fromList []}

  -- print bob with score set to 42
  print $ bob & score .~ 42
  -- User {_userName = UserName "bob", _userScore = 42, _userPet = Nothing, _userInventory = fromList []}
\end{code}

But what's this? An ampersand? `&`?!? Did you laugh like a super villain? I hope you did.

If you ask a seasoned Haskell Lenser what this `&` is all about, they'll say, oh, it's just inverse `$`. You know, a reverse application operator. Duh.

The `&` makes setting easier. The advice I got was to read it as "with". That last line should read: "print bob with score set to 42".

You know what else is cool? Using it to set multiple targets on the same object.

\begin{code}
fancySetExamples :: IO ()
fancySetExamples = do
  let bob = User (UserName "bob") 0 Nothing HM.empty

  -- check out this multi-line string, why don't ya?
  print "Bob changes his name to 'Bill'\
        \, updates his score, and now owns Jeff's pet fish,\
        \who is named Fitzgerald."
  print $
    bob
    & userName .~ (UserName "Bill")
    & score .~ 50
    & pet ?~ (Pet (PetName "Fitzgerald"))
  -- User {_userName = UserName "Bill", _userScore = 50,
  --       _userPet = Just (Pet {_petName = PetName "Fitzgerald"}),
  --       _userInventory = fromList []}

  -- note the `?~` - any guesses what that's doing?
  -- These two are equivalent:
  print $ bob & pet .~ Just (Pet (PetName "Fitzgerald"))
  print $ bob & pet ?~ (Pet (PetName "Fitzgerald"))
\end{code}

So now you can get (view) and set with lenses, and compose things arbitrarily. Remember that setting is not just an update, but at times a delete, depending on the lens - we'll see an example of that when we get to `at`.

But first! You probably want something a little more flexible than set - say you wanted to increment Bob's score, but don't want to go fetch it, and also don't know that Ed Kemmet already wrote `+~` for this exact use-case.

`over (%~)`

Over (%~) is like `set (.~)`, but takes a function from `a -> b` rather than just a `b`. I alluded to the theory behind the `%` earlier - this is your mod operator, used as a kind of nerdy pun on 'modulo'/'modify'. Seriously.

  > If you're curious about the StateT related stuff, note that this same pun-cutuation applies to a similar operator: `(%=)`.

\begin{code}
overExamples :: IO ()
overExamples = do
  let fitz = Pet (PetName "Fitz")
  let bob = User (UserName "bob") 0 (Just fitz) HM.empty

  print "Bob scores a point. Way to go, Bob."
  -- These all print bob with a score of 1.
  print $ bob & score %~ (\sc -> sc + 1)
  print $ bob & score %~ (+1)
  print $ over score (+1) bob
  print $ bob & score +~ 1

  -- Walk to Bob's fish, update it's name
  let bobWithFitzy = bob & pet . _Just . petName %~
        (\(PetName n) -> PetName (T.concat [n, "y"]))
  print $ bobWithFitzy ^? pet . _Just . petName
  -- Just (UserName "Fitzy")
\end{code}

Over is pretty handy! I hit a use-case for wrapping errors the other day that I rather liked:

\begin{code}
-- StorageError in a module somewhere
newtype StorageError = StorageError Text deriving (Eq, Show)

-- WebError wraps storage Error
data WebError
  = WebTextError Text
  | WebStorageError StorageError
  deriving (Eq, Show)

-- Let's convert an 'Either StorageError a' to an 'Either WebError a'
moreOverExamples :: IO ()
moreOverExamples = do
  let badStorageResponse =
        Left (StorageError "fail!") :: Either StorageError Text
      goodStorageResponse =
        Right "datadata" :: Either StorageError Text

  -- Wrap the error in WebStorageError if it's a `Left StorageError`
  print $ over _Left (\stErr -> WebStorageError stErr) badStorageResponse
  print $ over _Left WebStorageError badStorageResponse
  print $ badStorageResponse & _Left %~ (\stErr -> WebStorageError stErr)
  print $ badStorageResponse & _Left %~ WebStorageError
  -- Left (WebStorageError (StorageError "fail!"))

  -- This passes the 'Right Text' through just fine.
  print $ over _Left WebStorageError goodStorageResponse
  -- Right "datadata"
\end{code}

The same use of `over` above uses Lens's `_Left` 'Prism' to target the Either's 'Left a' cases, and apply the function to that target if the 'Traversal' finds a target. If it's a 'Right b', no target is found, and the object is returned unmodified.

`at` and `ix`

At and Ix are for things that are indexed. Maps, HashMaps, Lists - collections with keys or indexes. At and Ix are some of my favorites - once their in your repertoire, you'll be hella annoyed working with indexed structures in other languages.

At and Ix are roughly the same, with a key difference - Ix is a 'Traversal', while At is a 'Lens'. You don't know what a 'Traversal' is? Geez, have your Monads even dropped? You probably didn't even know Redux is just a big Monadic Klieisli composition (>=>). Even Fitzgerald knows that, and he's just a goldfish. Anyway.

Traversals are different from Lenses (and Folds) in this context because a Traversal cannot change the structure of the thing it is traversing - it can adjust the values in-place, but it cannot add or remove elements. Thus, Ix makes for more useful in-place adjustments, while At is useful for adding and removing elements.

You may be wondering why we don't just us At for everything - indeed, you can if you'd like. One aesthetic reason why not is that At is the `?~/?^` to your nice, Maybe-less `.~/.^` Ix-y goodness. That is to say, At requires you to use a _Just or a `preview`/`maybe-set`, rather than simply setting a new value.

A more practical reason is that, at times, At cannot be legally implemented. You may write a custom data structure for which the lens laws are not satisfied. Because Ix and Traversals cannot modify the structure itself, there are fewer requirements.

Last thing before diving into an example: `At` is named as such for referring to an element "at" a key, while `Ix` is said to represent the "i-th" element in a structure. Both, however, can take keys of any type, as long as you implement the required type classes for that key.

Enough jabbering. Let's see what `At` and `Ix` can do!

\begin{code}
atIxExamples :: IO ()
atIxExamples = do
  -- Yep, you can use apostrophes in var names. Not that you should...
  let bob'sInventory = HM.fromList [ ("gold", Item 99 10)
                                    , ("silver", Item 10 9)
                                    ]
      bob = User (UserName "bob") 42 Nothing bob'sInventory

  print "Printing Bob's gold value"
  print $ bob ^? inventory . at "gold" . _Just . value
  -- Just 99
  print $ bob ^? inventory . ix "gold" . value
  -- Just 99
  print $ bob ^? inventory . at "doesnotexist" . _Just . value
  -- Nothing
  print $ bob ^? inventory . ix "doesnotexist" . value
  -- Nothing


  print "Bob finds a diamond"
  let bobFindsDiamond  = bob & inventory . at "diamond" ?~ (Item 1000 1)
      bobFindsDiamond' = bob & inventory . at "diamond" .~ Just (Item 1000 1)
  print $ bobFindsDiamond ^? inventory . ix "diamond"
  -- Just (Item 1000 1)
  print $ bobFindsDiamond' ^? inventory . ix "diamond"
  -- Just (Item 1000 1)

  print "Bob loses his gold, some points, and is sad"
  let bobLosesGold = bob
        & inventory . at "gold" .~ Nothing
        & score %~ (\sc -> sc - 41)
        & userName .~ UserName "Sad Bob"

  -- Note the differences in `^./^?/at/ix` usage
  print $ bobLosesGold ^? inventory . at "gold"
  -- Just Nothing
  print $ bobLosesGold ^. inventory . at "gold"
  -- Nothing
  print $ bobLosesGold ^? inventory . ix "gold"
  -- Nothing

  -- This won't compile without an instance of Monoid for "Item".
  -- If you implement that instance, and run this, it will assume you
  -- wanted to use that Monoid instance, and return it's mempty for you.
  -- print $ bobLosesGold ^. inventory . ix "gold"

  print $ bobLosesGold ^. score
  -- 1
  print $ bobLosesGold ^. userName
  -- UserName "Sad Bob"
\end{code}

Per the monoid instance listed above - it is worth mentioning and showing a case for yet another lens, called `non`.

\begin{code}
atIxNonExamples :: IO ()
atIxNonExamples = do
  let bob = User (UserName "bob") 42 Nothing HM.empty
      -- if you were doing this for-real, you would impl and use Data.Default
      defaultGoldItem = Item 0 0

  print "Return the value of Bob's gold, whether he has it or not."
  print $ bob ^. inventory . at "gold" . non defaultGoldItem . value
  -- 0
  print $ bob ^? inventory . at "gold" . _Just . value
  -- Nothing
\end{code}

`non` is useful when paired with Monoid's `mempty` or Default's `def`.

`toListOf (^..)` and `folded`

Just a few more operators and we'll let Bob go.

Structures like HashMaps are useful for looking things up by key - but how do you get a list of values? I was hung up on this for a while, even after finding Control.Lens.Fold. The answer: `toListOf` (aka `(^..)`) combined with `folded`.

\begin{code}
toListOfExamples :: IO ()
toListOfExamples = do
  let tory = HM.fromList [ ("gold", Item 99 10)
                          , ("silver", Item 10 9)
                          ]
      bob = User (UserName "bob") 42 Nothing tory

  print "A list of Bob's items"
  print $ bob ^.. inventory . folded
  -- [Item {_itemValue = 10, _itemWeight = 9},Item {_itemValue = 99, _itemWeight = 10}]
  print $ toListOf (inventory . folded) bob
  -- [Item {_itemValue = 10, _itemWeight = 9},Item {_itemValue = 99, _itemWeight = 10}]

  print "Bob uses ifolded . asIndex to list itemNames."
  print $ bob ^.. inventory . ifolded . asIndex
  -- ["silver","gold"]

  print "Bob's filtering to only his valuable items."
  print $
    bob ^.. inventory . folded . filtered (\item -> (item ^. value) > 50)
  -- [Item {_itemValue = 99, _itemWeight = 10}]
\end{code}

There's more to `folded` and working with `toListOf`, but this is enough to get you started.

`has` and `hasn't`

`has` is a useful operator with a big gotcha you're sure to see once. From the docs, `has` checks to see if a passed 'Fold' or 'Traversal' matches one or more entries. The thing to note is that it will ALWAYS return true for a Lens.

\begin{code}
hasGotcha :: IO ()
hasGotcha = do
  let bob = User (UserName "bob") 42 Nothing HM.empty

  print "Has bob gold in his inventory?"
  print $ has (inventory . at "gold") bob
  -- True
\end{code}

What? True? Remember that `At` is a Lens. If you want this to be useful, you'll have to use 'Ix', our resident index Traversal.

\begin{code}
hasGotchaIx :: IO ()
hasGotchaIx = do
  let bob = User (UserName "bob") 42 Nothing HM.empty
  print "Has bob gold in his inventory?"
  print $ has (inventory . ix "gold") bob
  -- False

  let richBob = User (UserName "bob") 42 Nothing
                  $ HM.fromList [("gold", Item 10 10)]
  print "Has bob gold in his inventory?"
  print $ has (inventory . ix "gold") richBob
  -- True
\end{code}

And `hasn't` of course does what you'd expect.

\begin{code}
hasn'tExample :: IO ()
hasn'tExample = do
  let bob = User (UserName "bob") 42 Nothing HM.empty
  print "Hasn't bob gold in his inventory?"
  print $ hasn't (inventory . ix "gold") bob
  -- True
  -- As in, "Yes, he doesn't."
\end{code}

Ed Kemmet is one cheeky operator.

Some things not covered:

  - `review/(#)` and somewhat related, `_Wrapped` and newtypes

\begin{code}
main :: IO ()
main = do
  print "Running 'viewExamples'"
  viewExamples

  print "Running 'composedViewExamples'"
  composedViewExamples

  print "Running 'previewExamples'"
  previewExamples

  print "Running 'setExamples'"
  setExamples

  print "Running 'fancySetExamples'"
  fancySetExamples

  print "Running 'overExamples'"
  overExamples

  print "Running 'moreOverExamples'"
  moreOverExamples

  print "Running 'atIxExamples'"
  atIxExamples

  print "Running 'atIxNonExamples'"
  atIxNonExamples

  print "Running 'toListOfExamples'"
  toListOfExamples

  print "Running 'hasGotcha'"
  hasGotcha

  print "Running 'hasGotcha'"
  hasGotchaIx

  print "Running 'hasn'tExample'"
  hasn'tExample

  print "Finished."
\end{code}


TODO prior art/other sources list

TODO write conclusion
