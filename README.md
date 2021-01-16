# Advent of Code 2020

## Overview

This repo contains solutions to all days of the [Advent of Code 2020](https://adventofcode.com/2020) done in Common Lisp. Each day is in its own file and defined in its own package. Doing it this way makes every day self contained and easy to work on. Load the system using quicklisp. You will also need to tell lisp where to find the inputs for each day. They are in the inputs directory. For example:

```Common Lisp
(ql:quickload "advent-of-code-2020")
(setf utils:*input-directory* "/my/development/directory/advent-of-code-2020/inputs/")
```

Change the path to match what's on your machine. To load and run the tests (only for day 23):

```Common Lisp
(ql:quickload "advent-of-code-2020-tests")
(5am:run! 'day-23-test::day-23-tests)
```

Each day is in its own package called `day-n`, replace n with the day number. Each day also has two functions which show the solutions for that day named `part-1` and `part-2`. So if you want to see the solutions for day 1:

```Common Lisp
(in-package :day-1)
(part-1)
(part-2)
```

Run all of the above commands inside your common lisp environment. And your common lisp environemnt SHOULD be a properly set up to use SLIME. The easiest way to do that is to load [quicklisp](https://www.quicklisp.org/beta/) and then let quicklisp set everything up for you.

**Addendum Jan 9, 2021:** I saw [Peter Norvig's solutions](https://github.com/norvig/pytudes/blob/master/ipynb/Advent-2020.ipynb) to Advent of Code 2020. In terms of elegance and compactness, my code compares unfavorably to them. I think this is mainly due to my unfamiliarity with both Common Lisp and the Advent of Code format. However, I'm taking this as a chance to re-work some of the days to continue learning Common Lisp and improving my skills. I also appreciate the irony of improving my usage of Common Lisp by looking at a Common Lisp programmer's python code.

I'm keeping my original code in place for comparison. All re-worked code will be in files and packages called "day-n-v2", where n is the day.

## [Day 1](src/day-1.lisp) Loops and Getting my Feet Wet

Not much interesting on day one, just a lot of loops. You can solve part-1 with a O(n) complexity by using hash maps or sets to pre-load all of the numbers to search, thus avoiding the second inner loop (the find function). I initially did it that way, but then replaced it with the simpler O(n^2) solution since I was already having to execute part-2 with complexity O(n^3).

**v2 Addendum:** The main insight to improving this code is realizing it's just enumerating combinations. After that, it's just a matter of finding the Common Lisp version of python's itertools. Alexandria provides a good `map-combinations` function that does just that. Now the code *is* purely functional.

I also spent some time trying to replicate the functionality exactly by using the [Snakes library](https://github.com/BnMcGn/snakes). However, the combinations function is just broken. In the end I was pretty happy with the more "lispy" solution so this didn't end up being a problem.

## [Day 2](src/day-2.lisp) Regular Expressions (CL-PPCRE) Make Their First Appearance

First appearance of [CL-PPCRE](https://edicl.github.io/cl-ppcre/), it will become a regular fixture in almost every day. The `do-register-groups` macro highlights the power of lisp in three ways. First, CL-PPCRE auto-compiles the regex when the form itself is compiled. This results in wicked fast code execution. Second, the contained form is only executed if the regex groups all match, no need for any `if` statements. Third, because it is generating code, you can give it functions to run on the values. The functions will be run _before_ the values are bound. In other words, in `parse-spec`, both `s-min` and `s-max` will be bound as integers before the inner form is evaluated.

**v2 Addendum** This is the first day where I think my original solution compared pretty favorably to Norvig's solution. The main changes are using the `<=` function correctly and pulling in an `xor` function. Not a huge improvement, but it did allow for reduction in visual clutter.

## [Day 3](src/day-3.lisp) Cheating Sometimes Pays Off

First appearance of [Alexandria](https://common-lisp.net/project/alexandria/), this library will also become a regular fixture on most days, mainly using the curry/rcurry functions.

The main insight of today was to just cheat: don't bother to calculate wrapping, just extend all of the grids in memory so that the logic is simple. This only works because you don't have to extend them too far. It always pays to know when you can cheat.

Also, this is the first day to see more advanced accumulation features in the loop macro.

**v2 Addendum** It turns out that you can do *even better* when you don't cheat. By properly doing a little modular arithmetic you can eliminate all of the string extension code. My `trees` function is longer than Norvig's equivalent code because python's `enumerate` function provides a lot of functionality for free.

I did spend some time playing [CL-ENUMERATIONS](https://common-lisp.net/project/cl-enumeration/) to see if I could get equivalent functionality, but it lacks the ability to skip rows during the enumeration. Without that, using enumerations would end up making the code even longer and uglier.

**Philosophical Note** To allow for the simplest implementation of enumerations in a language your really need a `yield` keyword or something that provides equivalent functionality. CL-ENUMERATIONS shows what happens when you don't have `yield`: your implementation is very baroque and is missing features. What else does a `yield` keyword give you? The ability to make generators easily. This is probably why Snakes was broken on day 1. When you look at the code, there is a lot of machinery to try and fake having a `yield` keyword. Even if I'm wrong about it being broken, the amount of code used to fake yielding is pretty high.

This is where the philosophy comes into play. There are a bewlidering array of means to iterate collections, both finite and infinite ones. For example, the [Common Lisp Iterate library](https://common-lisp.net/project/iterate/) documentation has a [great discussion of many of them](https://common-lisp.net/project/iterate/doc/Don_0027t-Loop-Iterate.html#Don_0027t-Loop-Iterate). Implementing them all for a single language would be nightmare and, to be honest, probably doesn't even make sense. For example, most lisps provide recursion with tail call optimization. Python doesn't and apparently never will. Common Lisp can fake generators, but in my limited experience doesn't do a good job of it, while Python looks to do them quite well. Every language decision involves tradeoffs because language features interact: a single feature can automatically make other features hard or impossible.

The end result is that Day 3 seems to be pretty well optimized for enumerators. It allows Norvig's `quantify` method to operate very generically on an arbitrary amount of data. You could make something similar for Lisp, but the natural implementation would not be lazy. Since AOC datasets are small, it's not a big deal. However, implementing that for the general case will still not be very natural, again because there's not a general `yield` mechanism (see CL-ENUMERATIONS). So, you are stuck dealing with indices and looping. At least the `loop` macro makes it easier than it would be otherwise. 

## [Day 4](src/day-4.lisp) Both Higher Order Functions and Mutability

I was getting more comfortable using higher order functions on this day, hence the use of nested `count-if` function calls.

The only other thing of note on this day is constructing the `*call-table*` a-list to associate keys with functions that will validate the values. Later you will see a simpler way to get lisp to automagically do the association for you. However, using a-lists as a quick and dirty lookup table is pretty standard lisp stuff.

This is also the first day that is not functional/immutable. Technically days that involve `loop` would also not be functional, but because _I'm_ not doing the mutating, it's good enough for me. Any lisp code that uses setf/setq is non-immutable, and I use setf in `load-passports`.

**v2 Addendum** Cleaning up the initial parse simplified a lot of the rest of the code. Using `labels` also made things more readable and simpler, which I found surprising. I eliminated the named functions and put lambdas directly into the call table. Again, using `<=` correctly saved more visual clutter than I expected. Finally, one thing that bothered me about all of my initial implementations was an over reliance on `loop`, which v2 code is addressing.

## [Day 5](src/day-5.lisp) Recursion, It's Kind of What Lisp is Made For

Lisp is a natural fit for recursive solutions. Day 5 was basically a binary search, which ends up being easy to code recursively. Also nice is the fact that will propery parameters you don't need to have two separate functions for handling F/B and L/R codes. `part-2` was just a matter of ordering the seats by seat-id and then figuring out where successive seats differed by 2.

While the call stack will never go beyond 8, there are later days that will go much higher. However, since [SBCL](https://0branch.com/notes/tco-cl.html#sec-2-2) is properly tail recursive, the stack won't grow as long as the recursive function is written properly.

Finally, of note, is that after day 4, the solution is immutable/functional once again.

**v2 Addendum** I should have realized that FBRL was encoding a binary number. My initial `partition->number` was basically a recursive way of finding that binary number. Shortening the code to find the seat id is a big part of the savings. However, I again want to lodge a complaint about the part 1/part 2 format. Another part of the savings was just eliminating code to compute rows/columns. I kept those around because by this time I was trying to anticipate what I could reuse from part 1 for part 2. This was one of the times I guessed wrong. While I still might end up with code that is not needed, being able to see the whole problem from the start would help eliminate that.

## [Day 6](src/day-6.lisp) More Advanced Looping

More aggregating and grouping, which is most easily done with `loop`.

With Advent of Code 2020 (and maybe in years past too), there was a lot of commonality with parsing inputs. There are three functions in the [utils package](src/utils.lisp) used for these common features. `read-day-file` returns the contents of a file as a list of strings. `load-numbers` takes a list of strings, assumes each line is a single integer, parses thoses strings as integers, and returns the numbers as a vector. Finally, `split-blank-lines` handles the case where the inputs are parsed as groups, with groups being separated by blank lines. It returns a list of list of strings.

**v2 Addendum** Now with no looping! Makes the title ironic, but I'll leave it as is. I always forget that Common Lisp has set functions, hence the original verbose code.

## [Day 7](src/day-7.lisp) Inner Functions (labels)

Only two small things of note today. `parse-bag-contents` shows the idiomatic way in lisp to accumulate values in a list, using push/nreverse. `can-contain` shows how to have a nested function (`analyze`) that can recursively call itself.

**v2 Addendum** The original code is too write-only, the newer code is much simpler and much less verbose. If you ignore the package imports, the lengths are comparable (45 Norvig vs 48 mine). It's justified to do that because it would be easy enough to put all code in the same package and have a single set of imports (which is similar to what is done in the Python notebook).

I did have some fun figuring out how to [memoize functions in Common Lisp](https://github.com/fare/fare-memoization). I'm guessing that's the point of the @lru_cache annotation. Turns out the code is still super fast without memoization. I left it in place to keep line counts comparable and to have an example of memoization. Most of the cleanup and code savings comes from moving bags to hash tables and getting rid of loops.

## [Day 8](src/day-8.lisp) Functions Are Just Symbols

The instructions are converted to symbols and interned in the day-8 package inside the `load-instructions` functions. This is so that I could use a `case` expression inside the `machine` function instead of a `cond` expression.

**v2 Addendum** My original was comparable to Norvig's in terms of both length and readability. All of the improvements for v2 are in making the code more clearer and idiomatic: using `multiple-value-bind` and `destructuring-bind` to simplify variable management, returning `values` instead of lists, and making the parser code's regex more explicit.

## [Day 9](src/day-9.lisp) Nothing But Loops

In the early days of Advent of Code 2020, many days are loop heavy. But Day 9 is nothing but loops. I don't know if this shows the limitation of the programming puzzle format or the power of the lisp loop macro. Or maybe both.

**v2 Addendum** There are still two loops, but it's no longer nothing but loops. I'll keep the original title in place for history. Part 1 was radically simplified by using `map-combinations` from Alexandria and displaced arrays to avoid copying and hash table creation. Like Norvig notes, this is the brute force approach. In my original it looks like I started going for the O(n) approach, using hash tables, but ended up with the worst of both worlds: long code to *almost* implement an O(n) approach with the runtime complexity of the brute force approach.

I mostly eliminated custom code in favor of Common Lisp's native features. `find-sum-range` stayed the same, I didn't feel like re-writing it to use a deque. I got rid of the `slice` method and replaced it with Common Lisp's `subseq` function. Also, no point in sorting the subsequence, just use `min` and `max`.

The code length is now competitive with Norvig's apporach (33 LOC for me, 30 LOC for his).

## [Day 10](src/day-10.lisp) Common Lisp Isn't Judgmental

This day shows one of the great things about Common Lisp: it lets you use whatever paradigm makes sense for any given task. `part-1` takes a very imperative/mutation heavy approach. However, `part-2` combines both and uses whatever makes sense for a given task.

**v2 Addendum** I didn't use Norvig's code for inspiration here. For part 1 I improved it by not using a hash table. Instead I used an array and it made the code much shorter and easier to read. I'm leaving my part 2 as is. Norvig's approach is the brute force approach. Mine is more complicated, but only computes combinations for runs that can possibly form new combinations. That's what I called a "dense run" in the code. Once you compute the combinations for the dense runs, you just multiply the combination counts together and that's your answer. To be mathematically correct, you also have to multiply by 1 for the original arrangment, but the identity property of multiplication being what it is, why bother. Norvig's code is more elegant, while mine is more computationally efficient.

## [Day 11](src/day-11.lisp) Macros!!!

I thought I would use macros more during Advent of Code. However, I only used them on two days, and this is one of them. I got sick of typing the same loops across the rows and columns of the grid, so I made the `loop-grid` and `loop-rows` macros. In other languages, functions are one of the few abstractions to reduce repetition. However, sometimes, they don't make sense. In Common Lisp you are in luck, that's when macros really come in handy.

**v2 Addendum** This was a major overhaul. Main highlight is that it went from 171 LOC -> 63 LOC (leaving out imports as I don't count that for Norvig either), which is competitive with Norvig's code. How?

* I deleted code that wasn't being used. Since I'm learning/relearning Common Lisp I was really wandering around on this one. As soon as I got the correct answers, I moved on. Lots of dead and unnecessary code was left in place.
* I switched to not mutating the grid in place. This saved lots of code as I no longer had to accumlate/track changes. Knowing if the grid had stabilized is now just a call to `equalp`.
* Got rid of my custom macros :(. This is becoming a major theme, my original title is now a lie. As always, I'll leave it in place for historical honesty.
* However, I'm using even more macros. I am now using the excellent [array-operations library](https://github.com/bendudson/array-operations). This by far made the biggest difference. Previously I way over abstracted access to `*grid*` in a futile attempt to simplify index management and eliminate ugly calls (`(aref *grid* row col)`). Array-operations simplifies all of this and made eliminating in place updates of the grid super easy. Even better, you can use higher order functions, like reduce, on multidimensional arrays easily. That simplified counting occupied cells. It also simplified and streamlined the `make-grid` code.
* Norivg gave the hint to parametrize the `crowded` variable which allow me to save lots of code.
* Got rid of all of the little accessor functions. They were an attempt to abstract access to `*grid*`, but with array-operations it's so much easier to just go directly to the grid.


## [Day 12](src/day-12.lisp) Object Orientation (CLOS)!!!

I also thought that I would use more Object Oriented programming during Advent of Code. However, in most cases, just using lists, vectors, maps, and cons cells ended up being good enough for most days.

I ended up using OO because this is a classic case of having the same interface, some shared implementation, and non-shared implementation. `pointer-game` and `waypoint-game` share decoding, execution, and manhattan distance functionality. The instructions (NSEWRLF) stay the same, though the interpretations change.

This also does show one of the advantages of CLOS over standard OO systems. All dispatch is dynamically determined at runtime. One line of code dispatches all instructions:

```Common Lisp
(funcall (car cell) the-game (cdr cell))
```

Here `cell` (a cons cell) contains the symbol representing the function and the delta. So the call to the correct method is fully dynamic because neither the target object nor the method are known until the actual `funcall` call is made. This is possible in other langauges, but it's not as natural or easy. Saving the function symbol in the cons cell uses the same trick as on day 8.

**v2 Addendum**

Once again, the original title is a lie, but is left for historical honesty. This one was a challenge to streamline, but I did learn a lot about making tighter lisp code and I also learned a lot about clever data representation from Norvig's code. The challenge was large because my initial code weighed in at 130 LOC. Norvig's is 37 LOC. My final code is 41 LOC. So how did this happen?

* The biggest win, by *far* was the elimination of object orientation, which means no more CLOS. I have additional thoughts I'll save for the philosophical notes section. All decisions are now in `ecase` statements which are much more compact and can easily share implementation details.
* The next biggest win was eliminating most of the mutation of the waypoints, positions, and rotation states. This shortened a lot of code and eliminated all of the dynamic variables/constants.
* Immutability enabled the next big win: abstracting out the play-game from the movement/rotation decisions. Now the decision functions can just work on state passed in, they are pure functions. Immutability enabled this because now the decision functions no longer have to update specific variables, they can just be passed generic position and rotation pairs and operate on them without knowing where they will be stored.
* The move and rotation code is now 100% shared. I saw this in Norvig's notes, but wasn't planning on even trying to do this initially. However, once all of the cruft was removed, it became obvious that the two games shared more than I initially thought they did.
* The rotate code is now a single line and was cribbed directly from Norvig's code.
* Some small savings in realizing that `(setf (values var1 var2...) (function-call...)` is a thing. I could have had the decision functions pass lists back, but using `values` is much more idiomatic.

**Philosophical Note**

My long and meandering initial implementation is really in the [spirit of Pascal](https://www.famousquotes.com/quote/blaise-pascal-quotes-1045873) who said "I didn't have time to write a short letter, so I wrote a long one instead." I wrote the thing on autopilot, trusting that the CLOS engine would wire everything together for me and allow me to solve the problem without understanding it all that well. And, it worked, I got the right answer. In fact, I would even argue there's a certain charm and aesthetic pleasantness to the original code because of its repeating patterns that lead to comfortable regularity. But then you have to ask, it that worth it for 3x the code?

On one hand, yes it is. Like I said, it was easier (for me) to write because it allowed me to write code without ever understanding the game as a whole. And when you are writing code in a business environment, being able to produce code with an incomplete understanding is not ideal, but it sure can save your job. Plus, anything that lowers the bar so that mere mortals can get the code working is a huge win.

But even with all that, I'm pretty sure it would be better if I were to maintain the second version. Because it's tight and compact, I'm forced to understand it before I touch it. There's no false sense of security hidden behind verbosity and regularity. Plus "regularity" is probably just a nice way of saying "duplication" which everyone agrees is bad in software. Of course, I might still prefer to maintain the original, it turns out that OO is a hell of a drug.

## [Day 13](src/day-13.lisp) You Really Need Some Math Theory For This One

This day I thought was one of the weakest. It depends on knowing the Chinese Remainder Theorem and knowing that it applies in this case. See the extensive comments in the code for an explanation of what is going on.

I thought it was weak because it is plausible that someone would come up with the algorithm to find the final result for part-2. However, knowing that it is correct for the given numbers is not straighforward. The algorithm will not give the correct result for an arbitrary set of numbers.

Lisp note: this is the first day I used Lisp structures (`defstruct`). They are useful when you need more data organization than just using lists, vectors, maps, and cons cells, but don't need a full blown OO system.

**v2 Addendum**

I didn't really look at what Norvig did. I mainly did cleanup based on notes I kept when I did the project initially. I also cleaned up the code to make it more idiomatic lisp and to add some native common lisp features that make things less verbose. I cleaned up the parsing code, I forgot I ended up doing it twice. The destructuring really made a lot of the code easier to understand, there are now many fewer calls to car/cdr even though the code now makes even more use of cons cells.

I only looked at Norvig's code at the end to get a general idea of what he did. The number of LOC seems comparable. Finally, I no longer use structures in the code.

## [Day 14](src/day-14.lisp) Lisp Goes Low

Lisp has surprisingly good support for low level bit-vector and bitmask operations. This day makes heavy use of bit-vectors, which is a natural fit since the bit size of the inputs is not an integer multiple of 32 (in that case fixnum/integers would make sense).

Another nice thing about Lisp is that you never have to worry about integer overflow. While I don't think any of the numbers used in computation here would have overflowed, I never had to work about it.

**v2 Addendum**

I again mostly used Norvig's solution as a target for LOC. My original solution was 115 LOC, the new solution is 52 LOC. Quite a bit less, but Norvig's is in the low/mid 40's. And, I had to cheat a litle in that I moved one of the functions to utils. There is some justification for this, the `bit-vector->integer` function was already there, so it made sense to the reverse operation, `integer->bit-vector` there.

In any case, python really shines with this type of stuff. The ability to chain lots of functions on the same line is definitely a python strength. This is where python as "unix shell replacement" really shows, it's compact in the same way shell scripts are.

**Philosophical Note**

Once again CLOS went away and there is no OO in this code anymore. The amount of ceremony that CLOS added was just too much when you are trying to boil down code to as minimal as possible. I also have to admit that in the original version the OO code never seemed to pay off. Sure, I was able to treat instructions as generic, but the machine itself was so simple that it didn't really pay off in savings. Perhaps you just need a larger codebase for the OO ceremony to be worth the trouble.

## [Day 15](src/day-15.lisp) Algorithmic Cleverness Beats Optimization Cleverness

This is the first day where you need some algorithmic cleverness to keep track of everything that's happening. The naive approach of saving everything in a list or array works for part-1, but then fails miserably for part-2. The trick is realizing that you only have to remember the past two times you see a number. After that, it's irrelevant, and you can get rid of numbers seen more than 2 turns ago.

I made a note to attempt optimizations later. However, the standard optimizations that I did apply didn't help much. Without types and inlining `part-2` runs in 5.5 seconds. After types and inlining, that drops to 4.3 seconds. That's about a 20% reduction, which in my opinion isn't really worth it. Profiling indicates that the problems are in the hash table operations. This indicates that the only way to really speed this up is to either 1) use a better hash table or 2) switch to array storage.

**v2 Addendum**

It was dumb of me to think I needed to store 2 turns of information. This is the case of my getting fixated on a particular representation early on and then coding functions that locked that in place. I only realized this when trying to find the array Norvig was using in his code (there isn't one). I confess I have a hard time thinking though how these games are played/represented.

Now that arrays are gone, along with `aref` calls, the code is substantially faster, shorter, and easier to read. In fact, it consistently takes 3.3 secs on my laptop. That's better than the fully optimized version in v1. It's also faster than Norvig's version. At 25 LOC it's more verbose than Norvig's version too.

## [Day 16](src/day-16.lisp) Lisp Makes Tedious Things Possible

One of the more tedious days. The parsing was pretty complex and detail oriented. Lots of small functions to try and make the code understandtable, but I was not as successful as I would have liked. 

However, this day stil shows a lot of the power of lisp. First, working in the REPL means that it was very easy to incrementally write and test all of those small functions. Second, working in a dynamic language means that it's easy to evolve and change those functions on the fly and not deal with lots of types. Third, this allows for a bottom up design, you can build up the code in small pieces. Finally, because lisp is not opinioned on which paradigm or style you use, it's easy to mix and match styles. For example, the use of lots of higher order functions mixed in with imperative loop code. While other languages have all of these features, having this all at the same time makes lisp greater than the sum of its parts.

## [Day 17](src/day-17.lisp) Space, the Final Frontier...

This was the day I wished Advent of Code showed both parts at once. Part 2 always _can_ reuse part 1, provided you guess correctly what will be needed for the second part. On this day I guessed very badly and ended up writing the whole the over again; the original part 1 was very different.

I guessed that part 2 would need a very efficient part 1. However, part 2 needed a very general approach, which made my original part 1 approach useless. Since dimensions was now a parameter in part 2, both `visit-all-neighbors` and `visit-space` went from being loop based to recursive.

## [Day 18](src/day-18.lisp) Where David Learns The Shunting Yard Algorithm

This day required the most research for me. I had never made a parser for infix mathematical expressions (there's _one_ reason lisp uses prefix expressions, then the parser is much simpler). After a bit of research, I settled on using the shunting yard algorithm. With shunting yard you can convert the infix expressions to either postfix or prefix notation.

Choosing prefix notation was the obvious solution for lisp as the executor can then just use regular lisp `funcall` or `apply` to call the correct math functions (see `execute prefix`). The only thing needed for part 2 was to add a configuration for operator precedences and then use dynamic variables to make the functions behave properly.

**v2 Addendum**

I still had fun doing the first day and I'm glad I learned the basics of the shunting yard algorithm and postfix/prefix math evaluation. However, I was excited to learn about more lisp libraries. I discovered [infix-math](https://github.com/ruricolist/infix-math) which is a series of macros that transform infix math to prefix math (which lisp will then evaluate). That's 99% of the problem. The only remaining part is to play around with precedences, which infix-math makes easy to do.

Total LOC savings is pretty dramatic. Version 1 is 93 LOC, while Version 2 is 25 LOC. The biggest part of that isn't even the evaluation code, it's the code to save and restore precedences. For comparison Norvig's code is 30 LOC.

## [Day 19](src/day-19.lisp) Lisp Can Make Hacks Too

Part 1 turned out to be very straightforward. The input was equivalent to a regex engine with simple rules. So, all that was needed was to convert the spec to some very complex regular expressions and feed those into cl-ppcre.

However, part 2 was much trickier. One of the changed just meant that the regular expressions how had to account for a "one or more"/+ operator. However, the other could only really be solved with a recursive regex engine or a custom parser. As I had already started Advent of Code late and had never caught up, I ended up importing a regex engine that could handle recursive regexes, perl. In the end, it was a nice hack because `uiop` made calling another process and processing its output _very_ easy. So, making compromised hacks is also another strength of common lisp.

## [Day 20](src/day-20.lisp) My First Backtracking Algorithm

This may have been the most frustrating day for me. I had never written a backtracking algorithm before. I spent hours trying to figure out why my code was so slow. Turns out I had introduced an infinite loop into my code that only showed up when working on large grids.

There was however an optimization that did make a large difference in execution time. When profiling, the `rotate` function was consistently the time sink. The solution was to pre-compute the rotations for each tile. This also forced me to figure out how many different ways there are to orient a tile in space whose edges have ordered numbers on them. Turns out the answer is 8. This also simplified the backtracking algorithm considerably which is probably why I was able to code it correctly.

In doing part 2 I made heavy use of the REPL as it involved lots of exploration and experimentation to arrive at the correct answer. While part 1 had to be correct theoretically, part 2 mostly involved just putting pieces together in the correct way; it was straighforward but tedious.

## [Day 21](src/day-21.lisp) My Second Backtracking Algorithm

It took me a while to figure out that day 21 was yet another backtracking problem. Part of the problem was just understanding what, exactly, was being asked for. The other challenge was that unlike day 20, the inputs here did require significant amounts of backtracking to solve the problem. However, this led to another problem. The slow execution and the large amounts of backtracking had me convinced that I had another infinite loop somewhere.

However, after randomizing the input order, the algorithm would sometimes arrive at a solution very quickly. I finally just let it run with the inputs in the order they are in the file and the algorithm did eventually find a solution. 

## [Day 22](src/day-22.lisp) No Unit Tests, Just a REPL

This day was really just a matter of reading and understanding the detailed instructions, then coding it as clearly as possible. By clearly I mean make the code as much like the instructions as possible. This meant making larger functions, resisting the urge to break down the instructions into more "understandable" parts. It also meant not sharing code between parts, this was the one day where almost nothing is shared. I'm sure an enterprise architect would tell me that I have too much duplicated code and that my code isn't testable.

However, I'm guessing that if you try and make the code testable and break it down into smaller pieces that it would quickly become incomprehensible. Sometimes you just have to understand the whole, breaking it down into parts eliminates the possibility of understanding and solving a problem. I'm reminded of the episode of Ron Jeffries trying to write a Sudoku solver using TDD; he naturally broke the problem down into testable pieces. He never solved the problem. Peter Norvig understood the problem holistically and wrote a complete solver. I would say precisely because he did not attempt to break the problem into pieces, but understood it as a whole. For a recounting of the whole episode (from a lisper!) [check out Peter Seibel's old blog](https://gigamonkeys.wordpress.com/2009/10/05/coders-unit-testing/).

In any case, that was my goal, understand the problem and stay close to it. Because of zero unit testing, it really made the REPL invaluable for doing exploration and using the code as I made it.

## [Day 23](src/day-23.lisp) OK, Now I Need Some Unit Tests

I'm not sure which day was the hardest for me in Advent of Code 2020. However, I know which were the two hardest for me: day 20 and day 23. Again, I wish they would give both parts at once. My solution to part 1 was (I think) a fairly elegant use of cons, car, and cdr to manipulate circular linked lists. I also was able to see how the lisp printer handles circular references and what you have to do so that lisp doesn't go crazy trying to print circular lists. That's the point of:

```Common Lisp
(setf *print-circle* t)
```

However, part 2 made that solution completely useless. It turns out the trick you need is to represent a linked list with an array. With this representation a node is an index while the node it points to (the next right node) is contained in the contents at that index. This only works if nodes are a dense set of numbers. In this case, since they are always consecutive, that requirement is met.

It's really easy to get confused when dealing with indexes and their contents. It took me a while to design the splice function on paper with diagrams before I finally stopped confusing myself and got it right.

Because this was a lot of low level programming, and because I guessed (correctly) that this would be a good program to try out optimization tricks on, I went ahead and did unit testing. I think it helped to keep code from breaking and it really helped with ensuring that optimization hints I added did not break code.

Because this is all integers and array access, optimizing was straighforward. Put the right optimizer hints and the right type declarations in place and lisp will give you pretty fast code. It's nice to know that if you are forced to write C-like code in lisp that lisp can give you C-like performance as well. Optimization was also straighforward because I was pretty sure that representing a linked list as an array is about as optimal as you can get from an algorithmic perspective. All of those O(n) operations on node based linked lists all become O(1) when it is array based.

**v2 Addendum**

I won't be doing further work on this day. Norvig's solution was more verbose than I expected. Mine is even more verbose, but not ridiculously so. Plus, he did not solve part 2, which was the more interesting problem.

## [Day 24](src/day-24.lisp) Representation is Key

The key to this puzzle is getting the representation correct. Square grids are straighforward to represent, hexagonal grids are not. So, I spent time [researching how best to do that](https://www.redblobgames.com/grids/hexagons/). Even then, I miscoded the direction functions initially, _even though I was copying them straight from the web page!_.

However, once I got that correct, the rest of the problem was straightforward. It was very similar to day 17, though the neighbor rules were different.

**v2 Addendum**

It looks like Norvig was able to reuse code from previous years or days for solving Life game puzzles. This makes LOC comparisons more difficult. But, I'm fairly certain that Norvig's code is more concise than any of my versions. My third version is quite speedy and is the smallest, so I'll take that as consolation.

The v2 version has four of major changes which makes the code smaller and one change that blows it back up again. By far the biggest change was changing `day-flips` to only investigate tiles it needs to. Norvig's code was much simpler, but since I'm not a python expert (and it's pretty dense python), I studied the problem again. Once again *understanding the actual problem* allowed me to simplify all of the code in `day-flips` and all of the code it calls. In particular, I was able to eliminate all of the min/max computation. The second major change was to store plain symbols for directions instead of function symbols. Third major change was to reduce consing by making walk mutable. It didn't complicate the code by much, but it sure reduced consing. Finally, I was able to eliminate most of the parsing code, this I did get from reading Norvig's code.

The one change that increased the code size was to add optmiziation hints and write custom hash and equal functions because profiling showed that `equalp` was by far the biggest time sink. The first four changes dropped the runtime from 50 sec -> 1 sec. The optimizations pared that down to 0.52 - 0.55 seconds.

I was going to stop there but Norvig's code said he used the axial coordinate system for hexagonal grids. He even provided the same url I did in my code comments. So I went back there to read up. It turns out if I had read just a couple of paragraphs more I would have seen the axial coordinate documentation. Even better, the axial system is virtually identical to the cube coordinates, you just drop the y axis. This allowed me to convert tiles from 3-d arrays to cons cells and reduce the number of operations in `walk!`. I had to stop using `copy-array` and make a custom `copy-tile` function. Finally, I was able to use the more efficient `equal` function in the hash table. The result is that the run time is now 0.46 secs with 2/3 less consing. As always really understand the problem *because good algorithms, especially with fewer computations and less storage, are almost always faster.* Not to mention **simpler**. 

## [Day 25](src/day-25.lisp) The Finish Line

Once again I was glad to have a REPL. I was having a hard time understanding the puzzle so being able to test and try things out to make sure I was understanding the puzzle was half the battle. Because the REPL made that experimentation easy, it made the day a lot easier. In the end, it was an easier day, which was great. I'm definitely a fan of ending Advent of Code on a easy note. If it had ended with the hardest puzzle, I would probably have finished less likely to try Advent of Code again. 

**v2 Addendum**

Once again, python really lets you express things compactly with generators and enumerators. Since Common Lisp doesn't have those natively, I went with the more lispy solution: recursion. As far as I know, all lisps do tail call elimination. If your's doesn't, the v2 solution will kill your lisp. There is [one more generators](https://github.com/cbeo/gtwiwtg) library for Common Lisp that looks interesting and I may check out.
