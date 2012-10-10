Hector is a Literate Haskell implementation of Weizenbaum's ELIZA program.

ELIZA is more or less the original "chatterbot". Implemented by Weizenbaum
in the mid-1960s to demonstrate how seemingly "intelligent" behaviour could
be mimicked by very simple pattern-matching, it shocked even him with
how much ordinary people were taken in by the illusion. It is probably best
known in conjunction with its DOCTOR script, which allows it to act like a
Rogerian psychotherapist (a version of this can be invoked in the Emacs text
editor with "M-x doctor"). One user was reportedly so affected by the program
that he requested that Weizenbaum give him some privacy to discuss his issues
with the doctor!

The program works by simple pattern matching. The user's input is checked
against a list of pre-defined patterns, and if there's a match, a corresponding
output is returned. Eliza also has some facilities for extracting content from
the input and using it in the reply (e.g. "I need a vacation" might result in
"How would you feel if you got a vacation?"). The final bit of finesse is
a rudimentary memory, so that it can bring up past topics again if it doesn't
know what to say (Hector does not do this).

Hector is a Haskell reimplementation of a very similar system. It is written
in a "literate" style, and should be simple to follow along with. Hector is
not a doctor like Eliza -- instead, he's more of a grizzled old programming
veteran, having got his start feeding punchcards into a mainframe in his
undergraduate years. He can't help you with relationships or inferiority
complexes, but he'll be happy to discuss your programming problems with you.


Onward, to the code!

We're going to use regular expressions for our pattern matching, and select
responses randomly from the set of available responses to that input, so we'll
need some imports.

> import Data.List
> import System.Random
> import Text.Regex


The main loop of the program is quite simple: we just keep prompting the user
for input and outputting a response until we get an EOF. Before we
enter this loop, we'll just print a short greeting. Each iteration of the loop
we'll take advantage of the system random number generator to create a new
generator for use in picking our output.

> main :: IO ()
> main = do
>   putStrLn "Hello there. What's the matter?"
>   mainloop
>
> mainloop :: IO ()
> mainloop = do
>   input <- getLine
>   seed <- randomIO
>   putStrLn $ respond input (mkStdGen seed)
>   mainloop


The format we'll use for our canned input/response pairs is simple. Each
possible input is a regular expression, originally entered as a string. Before
use, we'll do some gentle massaging to get it into the Regex type our
libraries want. Every input/response pair actually consists of many different
inputs and responses: any input in the pair can cause any of the responses to
be output. In other words, if any of the inputs in a pair match, one of the
outputs in the pair will be returned.

> type RawIRPair = ([String], [String])
> type IRPair = ([Regex], [String])

Our actual input/response pairs are defined at the end of the program, since
they're very bulky and largely uninteresting. They're grouped into categories,
so we'll need to put all of them together into one big list.

> combinedResponses :: [RawIRPair]
> combinedResponses = foldl' (++) [] [general, tech, nonSequitur]

While we're at it, let's convert our input strings into Regexes -- this is the
final form that we'll actually use. Note that we're making the regular
expressions case-insensitive.

> responses :: [IRPair]
> responses = regexify combinedResponses
>   where regexify :: [RawIRPair] -> [IRPair]
>         regexify []     = []
>         regexify (x:xs) =
>           [(map (\i -> mkRegexWithOpts i True False) (fst x), snd x)] ++
>           regexify xs


Here's where the meat of the program happens: the process of selecting a
response. Essentially, we just find the list of responses that match our input,
and pick one at random.

> respond :: RandomGen g => String -> g -> String
> respond input randGen = pickResponse (findMatchingResponses input) randGen

The process of picking the response at random is also straightforward. Once 
it's chosen at random, we run it through a function that substitutes any 
captured subgroups in place of patterns of the form !0, !1, etc.

> pickResponse :: RandomGen g => ([String], [String]) -> g -> String
> pickResponse (responses, substrings) randGen =
>   fillIn (responses !! chosen) substrings
>   where chosen = (fst $ randomR (0, length responses - 1) randGen :: Int)

> fillIn :: String -> [String] -> String
> fillIn response substrings = fillInCount response substrings 0
>
> fillInCount :: String -> [String] -> Int -> String
> fillInCount response (x:xs) n =
>   fillInCount (subRegex (mkRegex ("!" ++ show n)) response x) xs (n+1)
> fillInCount response [] _ = response

Finding the list of matching responses, however, is a little trickier, largely
due to the deep nesting that we need to flatten out before we get a chance to
actually try to match our regular expressions. Along with the list of matching
potential responses, we return the list of matching substrings (if any).

> findMatchingResponses :: String -> ([String], [String])
> findMatchingResponses input = checkListOfTuples input responses
>   where checkListOfTuples input (x:xs) =
>            case (checkTuple input x) of
>              ([], []) -> checkListOfTuples input xs
>              (responses, substrings) -> (responses, substrings)
>
> checkTuple :: String -> IRPair -> ([String], [String])
> checkTuple input (i:is, r) =
>   case (matchRegex i input) of
>     Just a -> (r, a)
>     Nothing -> checkTuple input (is, r)
> checkTuple _ _ = ([], [])


Finally, we need to give Hector something to say. Each of the following lists
represents a category of input/response tuples. They are simple regular
expressions, with the substring matches from the input accessible via !n
patterns in the output, where n is the desired capture group.

The illusion of conversation relies on the user doing most of the intellectual
gruntwork. It is best for Hector to just try to extract information, and let
the user to the talking. A lot of these are borrowed straight from Eliza. The 
ones included here are in no way sufficient for having a reasonable 
conversation. You need way more open-ended ones that let the user talk more.

> general :: [RawIRPair]
> general = [
>   (["hello"], ["Hi. What's going on?", "Yes, hi. Tell me your problem."]),
>   (["delayed", "behind schedule"],
>    ["Can you get more time?", "What happens if you don't make it?"]),
>   (["yes[ .,!]*$", "yeah"],
>    ["At least we agree on something!", "Good.", "I'm glad, then."]),
>   (["no[ .,!]*$", "nope"],
>    ["Try not to be so negative.", "Well, I don't know what to say, then."]),
>   (["sorry"], ["No need to apologize!"]),
>   (["I don't know how ([a-zA-Z ]*)"],
>    ["Does anyone have a reference on how !0?",
>     "Is it essential that you learn !0?"]),
>   (["I don't know"], ["So can you try to find out?"]),
>   (["I have to"], ["What, right now?"]),
>   (["I have ([a-zA-Z ]*)"], ["How long have you had !0?"]),
>   (["I need ([a-zA-Z ]*)", "I want ([a-zA-Z ]*)"],
>    ["Well, do you think it's at all possible to get !0?",
>     "What would change if you got !0?"])]

Hector needs to have some snarky, dismissive opinions on major programming
technologies, so let's create another set of input/response pairs.

> tech :: [RawIRPair]
> tech = [
>  (["python"], ["It's like executable pseudocode!"]),
>  (["perl"], ["It's like executable line noise."]),
>  (["c\\+\\+"], ["C++ is an octopus made by nailing legs onto a dog!"]),
>  (["haskell"], ["Oh Haskell; someday it'll be useful. Don't you agree?"]),
>  (["internet"],
>   ["Oh Internet... a thousand monkeys and associated typewriters, eh?"]),
>  (["regular expression", "regex"],
>   ["Regular Expressions... Now you have two problems.",
>    "Do you prefer POSIX or Perl regexes?"]),
>  (["xml"],
>   ["XML is just s-expressions poorly reinvented. Don't you agree?",
>    "Have you considered JSON instead of XML?",
>    "XML? Ah, right, ten times as much markup as data is always fun."]),
>  (["java"], ["Bah! Java? Lacks downward funargs. Do you know Lisp?"])]

If nothing matches, we can still reply with a chatty output unrelated to what
the user just said.

> nonSequitur :: [RawIRPair]
> nonSequitur = [([".*"], [
>  "So what UNIX do you use?",
>  "Where did you learn to program?",
>  "Tell me, what Open Source projects are you involved with?",
>  "So, have you read Knuth's books?",
>  "I'm pretty sure Dijkstra has a paper that would help you.",
>  "I was wondering, do you know any good interview questions?",
>  "Let me tell you a story. A novice was trying to fix a broken Lisp \
>   \machine by turning the power off and on. Knight, seeing what the \
>   \student was doing, spoke sternly: 'You cannot fix a machine by just \
>   \power-cycling it with no understanding of what is going wrong.' Knight \
>   \turned the machine off and on. The machine worked. Are you enlightened \
>   \yet?",
>  "Let me tell you a story. In the days when Sussman was a novice, Minsky \
>   \once came to him as he sat hacking at the PDP-6. 'What are you doing?', \
>   \asked Minsky. 'I am training a randomly wired neural net to play \
>   \Tic-tac-toe', Sussman replied. 'Why is the net wired randomly?', asked \
>   \Minsky. 'I do not want it to have any preconceptions of how to play', \
>   \Sussman said. Minsky then shut his eyes. 'Why do you close your eyes?' \
>   \Sussman asked his teacher. 'So that the room will be empty.' At that \
>   \moment, Sussman was enlightened. Are you enlightened now?"])]

