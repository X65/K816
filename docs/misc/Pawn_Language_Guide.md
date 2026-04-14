# Pawn

## embedded scripting language

# The Language

February 2024 CompuPhase

ii

“CompuPhase” and “Pawn” are trademarks of CompuPhase.
“Java” is a trademark of Sun Microsystems, Inc.
“Microsoft” and “Microsoft Windows” are registered trademarks of Microsoft Corporation.
“Linux” is a registered trademark of Linus Torvalds.
“Unicode” is a registered trademark of Unicode, Inc.

Copyright⃝c1997–2023, CompuPhase
Eerste Industriestraat 19–21, 1401VL Bussum The Netherlands
telephone: (+31)-(0)35 6939 261
e-mail: <info@compuphase.com>
WWW: [http://www.compuphase.com](http://www.compuphase.com)

This manual and the associated software are made available under the conditions
listed inappendix Dof this manual.

Typeset with TEX in the “Adobe Source” typeface family.

## iii

- Foreword::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: Contents
- A tutorial introduction:::::::::::::::::::::::::::::::::::::::::::::::::::::::
  - Arithmetic and expressions::::::::::::::::::::::::::::::::::::::::::::::::
  - Arrays and constants:::::::::::::::::::::::::::::::::::::::::::::::::::::::
  - Using functions::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  - Rational numbers:::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  - Strings::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  - Symbolic subscripts (structured data):::::::::::::::::::::::::::::::::::::
  - Bit operations to manipulate “sets”::::::::::::::::::::::::::::::::::::::::
  - A simple RPN calculator::::::::::::::::::::::::::::::::::::::::::::::::::
  - Event-driven programming:::::::::::::::::::::::::::::::::::::::::::::::
  - State programming:::::::::::::::::::::::::::::::::::::::::::::::::::::::
  - Program verification::::::::::::::::::::::::::::::::::::::::::::::::::::::
  - Documentation comments::::::::::::::::::::::::::::::::::::::::::::::::
  - Warnings and errors::::::::::::::::::::::::::::::::::::::::::::::::::::::
  - In closing:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
- Data and declarations::::::::::::::::::::::::::::::::::::::::::::::::::::::
  - State variable declarations::::::::::::::::::::::::::::::::::::::::::::::::
  - Static local declarations:::::::::::::::::::::::::::::::::::::::::::::::::::
  - Static global declarations::::::::::::::::::::::::::::::::::::::::::::::::::
  - Stock declarations::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  - Public declarations:::::::::::::::::::::::::::::::::::::::::::::::::::::::
  - Constant variables::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  - Arrays (single dimension):::::::::::::::::::::::::::::::::::::::::::::::::
  - Initialization::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  - Progressive initiallers for arrays:::::::::::::::::::::::::::::::::::::::::::
  - Symbolic subscripts for arrays::::::::::::::::::::::::::::::::::::::::::::
  - Multi-dimensional arrays:::::::::::::::::::::::::::::::::::::::::::::::::
  - Arrays and the sizeof operator::::::::::::::::::::::::::::::::::::::::::::
  - Tag names::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
- Functions::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  - Function arguments::::::::::::::::::::::::::::::::::::::::::::::::::::::
  - Coercion rules::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  - Calling functions::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  - Recursion:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  - Forward declarations:::::::::::::::::::::::::::::::::::::::::::::::::::::
  - State classifiers:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  - Public functions, function main:::::::::::::::::::::::::::::::::::::::::::
  - Static functions:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  - Stock functions:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  - Native functions:::::::::::::::::::::::::::::::::::::::::::::::::::::::::: iv — Table of contents
  - User-defined operators::::::::::::::::::::::::::::::::::::::::::::::::::::
- The preprocessor:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
- General syntax::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
- Operators and expressions:::::::::::::::::::::::::::::::::::::::::::::::::
  - Notational conventions:::::::::::::::::::::::::::::::::::::::::::::::::::
  - Arithmetic::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  - Bit manipulation::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  - Assignment:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  - Relational:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  - Boolean:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  - Miscellaneous::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  - Operator precedence:::::::::::::::::::::::::::::::::::::::::::::::::::::
- Statements::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
- Directives::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
- Proposed function library:::::::::::::::::::::::::::::::::::::::::::::::::
  - Core functions:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  - Console functions:::::::::::::::::::::::::::::::::::::::::::::::::::::::
  - Date/time functions::::::::::::::::::::::::::::::::::::::::::::::::::::::
  - File input/output:::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  - Fixed point arithmetic:::::::::::::::::::::::::::::::::::::::::::::::::::
  - Floating point arithmetic::::::::::::::::::::::::::::::::::::::::::::::::
  - Process and library call interface:::::::::::::::::::::::::::::::::::::::::
  - String manipulation:::::::::::::::::::::::::::::::::::::::::::::::::::::
- Pitfalls: differences from C::::::::::::::::::::::::::::::::::::::::::::::::
- Assorted tips:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  - Working with characters and strings:::::::::::::::::::::::::::::::::::::
  - Internationalization:::::::::::::::::::::::::::::::::::::::::::::::::::::
  - Working with tags:::::::::::::::::::::::::::::::::::::::::::::::::::::::
  - Concatenating lines::::::::::::::::::::::::::::::::::::::::::::::::::::::
  - A program that generates its own source code::::::::::::::::::::::::::::
- Appendices::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  - A: Error and warning messages::::::::::::::::::::::::::::::::::::::::::
  - B: The compiler:::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  - C: Rationale:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  - D: License:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
- Index:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

### 1

Foreword

PAWNis a simple, typeless, 32-bit “scripting” language with a C-like syntax. Execu-
tion speed, stability, simplicity and a small footprint were essential design criteria
for both the language and the interpreter/abstract machine that a PAWNprogram
runs on.

An application or tool cannot do or beeverythingforallusers. This not only justifies
the diversity of editors, compilers, operating systems and many other software sys-
tems, it also explains the presence of extensive configuration options and macro
or scripting languages in applications. My own applications have contained a va-
riety of little languages; most were very simple, some were extensive:::and most
needs could have been solved by a general purpose language with a special purpose
library. Hence, PAWN.

The PAWNlanguage was designed as a flexible language for manipulating objects in
a host application. The tool set (compiler, abstract machine) were written so that
they were easily extensible and would run on different software/hardware archi-
tectures.

PAWNis a descendent of the original Small C by Ron Cain and James Hendrix, which
at its turn was a subset of C. Some of the modifications that I did to Small C, e.g. the
removal of the type system and the substitution of pointers by references, were so
fundamental that I could hardly call my language a “subset of C” or a “C dialect” any
more. Therefore, I stripped off the “C” from the title and used the name “SMALL”
for the name of the language in my publication in Dr. Dobbʼs Journal and the years
since. During development and maintenance of the product, I received many re-
quests for changes. One of the frequently requested changes was to use a different
name for the language —searching for information on the SMALLscripting language
on the Internet was hindered by “small” being such a common word. The name
change occurred together with a significant change in the language: the support of
“states” (and state machines).

I am indebted to Ron Cain and James Hendrix (and more recently, Andy Yuen),
for their work on Small C and to Dr. Dobbʼs Journal for publishing it. Although I
must have touched nearly every line of the original code multiple times, the Small
C origins are still clearly visible.

A detailed treatise of the design goals and compromises is inappendix C; here I
would like to summarize a few key points. As written in the previous paragraphs,
PAWNis for customizing applications (by writing scripts), not for writing applica-
tions. PAWNis weak on data structuring because PAWNprograms are intended to
manipulate objects (text, sprites, streams, queries,:::) in the host application, but

2 — Foreword

the PAWNprogram is,by intent, denied direct access to any data outside its abstract
machine. The only means that a PAWNprogram has, to manipulate objects in the
host application is by calling routines, so called “native functions”, that the host
application provides.

PAWNis flexible in that key area:calling functions. PAWNsupports default values
foranyof the arguments of a function; it supports call-by-reference as well as call-
by-value, and “named” as well as “positional” function arguments. PAWNdoes not
have a “type checking” mechanism, by virtue of being a typeless language, but it
doesoffer in replacement a “classification checking” mechanism, called “tags”. The
tag system is especially convenient for function arguments because each argument
may specify multiple acceptable tags.

For any language, the power (or weakness) lies not in the individual features, but
in their combination. For PAWN, I feel that the combination of named arguments
—which lets you specify function arguments in any order, and default values —
which allows you to skip specifying arguments that you are not interested in, blend
together to a convenient and “descriptive” way to call (native) functions to manip-
ulate objects in the host application.

### 3

A tutorial introduction

PAWNis a simple programming language with a syntax reminiscent to the “C” pro-
gramming language. A PAWNprogram consists of a set of functions and a set of
variables. The variables are data objects and the functions contain instructions
(called “statements”) that operate on the data objects or that perform tasks.

The first program in almost any computer language is one that prints a simple
Compiling and run-
ning scripts:page
145

string; printing “Hello world” is a classic example. In PAWN, the program would
look like:

LISTING: hello.p
@start()
printf "Hello world\n"

This manual assumes that you know how to run a PAWNprogram; if not, please
consult the application manual orappendix B).

A PAWNprogram starts execution in an “entry” function—in nearly all examples
of this manual, this entry function is called “@start”. Here, the function@start
contains only a single instruction, which is at the line below the function head itself.
Line breaks and indenting are insignificant; the invocation of the functionprint
could equally well be on the same line as the head of function@start.

Thedefinitionof a function requires that a pair of parentheses follow the function
name. If a function takes parameters, their declarations appear between the paren-
theses. The function@startdoes not take any paremeters. The rules are different
for a function invocation (or a functioncall); parentheses are optional in the call to
theprintfunction.

The single argument of theprintfunction is a string, which must be enclosed in String literals: 85
quotes. The characters\nnear the end of the string form anescape sequence, in this
Escape sequence:
case they indicate a “newline” symbol. Whenprintencounters the newline escape 85
sequence, it advances the cursor to the first column of the next line. One has to use
the\nescape sequence to insert a “newline” into the string, because a string may
not wrap over multiple lines.

PAWNis a “case sensitive” language: upper and lower case letters are considered to
be different letters. It would be an error to spell the functionprintfin the above
example as “PrintF”. Keywords and predefined symbols, like the name of function
“@start”, must be typed in lower case.

If you know the C language, you may feel that the above example does not look
much like the equivalent “Hello world” program in C/C++. PAWNcan also look very

```
 This should not be confused with the “state” entry functions, which are calledentry, but serve a
different purpose —seepage 35.
```

```
4 — A tutorial introduction
```

```
similar to C, though. The next example program is also valid PAWNsyntax (and it
has the same semantics as the earlier example):
```

```
LISTING: hello.p — C style
#include <console>
main()
{
printf("Hello world\n");
}
```

```
These first examples also reveal a few differences between PAWNand the C lan-
guage:
⋄there is usually no need to include any system-defined “header file”;
⋄you may use “main” in PAWNas an alternative to@start;
⋄semicolons are optional (except when writing multiple statements on one line);
⋄when the body of a function is a single instruction, the braces (for a compound
instruction) are optional;
⋄when you do not use the result of a function in an expression or assignment,
parentheses around the function argument are optional.
```

```
As an aside, the few preceding points refer tooptionalsyntaxes. It is your choice
what syntax you wish to use: neither style is “deprecated” or “preferred”. The ex-
amples in this manual position the braces and use an indentation format that is
known as the “Whitesmithʼs style”, but PAWNis a free format language and other
indenting styles are just as fine.
```

Because PAWNis designed to be anextension languagefor applications, the function
set/library that a PAWNprogram has at its disposal depends on the host applica-
tion. As a result, the PAWNlanguagehas no intrinsic knowledge ofanyfunction.
Theprintfunction, used in this first example, must be made available by the host
More function de-
scriptions atpage
106

```
application and be “declared” to the PAWNparser.y It is assumed, however, that
all host applications provide a minimal set of common functions, likeprintand
printf.
```

```
In some environments, the display or terminal must be enabled before any text can
be output onto it. If this is the case, you must add a call to the function “console”
before the first call to functionprintorprintf. Theconsolefunction also allows
you to specify device characteristics, such as the number of lines and columns of
the display. The example programs in this manual do not use theconsolefunc-
tions, because many platforms do not require or provide it.
```

```
yIn the language specification, the term “parser” refers to any implementation that processes and
runs on conforming Pawn programs —either interpreters or compilers.
```

```
Arithmetic and expressions — 5
```

Arithmetic and expressions

Fundamental elements of most programs are calculations, decisions (conditional
execution), iterations (loops) and variables to store input data, output data and in-
termediate results. The next program example illustrates many of these concepts.
The program calculates the greatest common divisor of two values using an algo-
rithm invented by Euclides.

LISTING: gcd.p
/*
The greatest common divisor of two values,
using Euclides' algorithm.
*/

@start()
{
print "Input two values\n"
var a = getvalue()
var b = getvalue()
while (a != b)
if (a > b)
a = a - b
else
b = b - a
printf "The greatest common divisor is %d\n", a
}

The@startfunction now contains more than just a single “print” statement. When
the body of a function contains more than one statement, these statements must
be embodied in braces —the “{” and “}” characters.Braces group the instructions
Compound state-
to a singlecompound statement. The notion of grouping statements in a compound ment: 96
statement applies as well to the bodies ofif–elseand loop instructions.

Thevarkeyword creates a variable. The name of the variable followsvar. It is com- For backward com-
patibility, “new”
may be used as
well as “var”.

mon, but not imperative, to assign a value to the variable already at the moment of
its creation. Variables must be declared before they are used in an expression. The
getvaluefunction (also common predefined function) reads in a value from the
keyboard and returns the result. Note that PAWNis atypelesslanguage, all variables
are numeric cells that can hold a signed integral value.

Thegetvaluefunction name is followed by a pair of parentheses. These arere-

```
Data declarations
are described in
detail starting at
page 52
```

quiredbecause the value thatgetvaluereturns is stored in a variable. Normally,
the functionʼs arguments (or parameters) would appear between the parentheses,
butgetvalue(as used in this program) does not take any explicit arguments. If you
do not assign the result of a function to a variable or use it in a expression in another
way, the parentheses are optional. For example, the result of theprintandprintf
statements are not used. You may still use parentheses around the arguments, but
it is not required.

```
6 — Arrays and constants
```

Loop instructions, like “while”, repeat a single statement as long as the loop con-
“while” loop: 99
“if–else”: 97 dition (the expression that follows thewhilekeyword) is “true”. One can execute
multiple statements in a loop by grouping them in a compound statement. The
if–elseinstruction has one statement for the “true” clause and one for the “false”.
Observe that some statements, likewhileandif–else, contain (or “fold around”)
another instruction —in the case ofif–elseeventwoother instructions. The com-
plete bundle is, again, a single instruction. That is:
⋄the assignment statements “a = a - b” below theifand “b = b - a” below the
elseare statements;
⋄theif–elsestatement folds around these two assignments and forms a single
statement of itself;
⋄thewhilestatement folds around theif–elsestatement and thus forms, again,
a single statement.
It is common to make the nesting of the statements explicit by indenting any sub-
statements below a statement in the source text. In the “Greatest Common Divisor”
example, the left margin indent increases by four space characters after thewhile
statement, and again after theifandelsekeywords. Statements that belong to
the same level, such as bothprintfinvocations and thewhileloop, have the same
indentation.
The loop condition for thewhileloop is “(a != b)”; the symbol!=is the “not
Relational opera-
tors: 92 equal to” operator. That is, theif–elseinstruction is repeated until “a” equals
“b”. It is good practice to indent the instructions that run under control of another
statement, as is done in the preceding example.
The call toprintf, near the bottom of the example, differs from theprintcall right
below the opening brace (“{”). The “f” inprintfstands for “formatted”, which
means that the function can format and print numeric values and other data (in
a user-specified format), as well as literal text. The%dsymbol in the string is a
token that indicates the position and the format that the subsequent argument to
functionprintfshould be printed. At run time, the token%dis replaced by the
value of variable “a” (the second argument ofprintf).
Functionprintcan only print text; it is quicker thanprintf. If you want to print
a literal “%” at the display, you have to useprint, or you have to double it in the
string that you give toprintf. That is:
print "20% of the personnel accounts for 80% of the costs\n"
and
printf "20%% of the personnel accounts for 80%% of the costs\n"
print the same string.

```
Arrays and constants
Next tosimplevariables with a size of a single cell, PAWNsupports “array variables”
that hold many cells/values. The following example program displays a series of
```

```
Arrays and constants — 7
```

prime numbers using the well known “sieve of Eratosthenes”. The program also
introduces another new concept: symbolic constants. Symbolic constants look like
variables, but they cannot be changed.

LISTING: sieve.p
/*Print all primes below 100, using the "Sieve of Eratosthenes"*/

@start()
{
const max_primes = 100
var series[max_primes] = [ true, ... ]
for (var i = 2; i < max_primes; ++i)
if (series[i])
{
printf "%d ", i
/*filter all multiples of this "prime" from the list*/
for (var j = 2 * i; j < max_primes; j += i)
series[j] = false
}
}

When a program or sub-program has some fixed limit built-in, it is good practice
Constant declara-
create a symbolic constant for it. In the example above, the symbolmax_primes tion: 87
is a constant with the value 100. The program uses the symbolmax_primesthree
times after its definition: in the declaration of the variableseriesand in bothfor
loops. If we were to adapt the program to print all primes below 500, there is now
only a single line to change.

Like simple variables, arrays may be initialized upon creation. PAWNoffers a con-
Progressive ini-
venient shorthand to initialize all elements to a fixed value: all hundred elements of tiallers: 55
the “series” array are set totrue—without requiring that the programmer types
in the word “true” a hundred times. The symbolstrueandfalseare predefined
constants.

When a simple variable is declared in the first expression of aforloop (like the
variablesiandjin the primes sieve example), the variable is valid only inside the
loop. Variable declaration has its own rules; it is not a statement —although it looks
like one.One of the special rules for variable declaration is that the first expression
of aforloop may contain a variable declaration. “for” loop:^97

Bothforloops also introduce new operators in their third expression. The++op-
An overview of all
erator increments its operand by one; meaning that,++iis equal toi = i + 1. operators: 89
The+=operator adds the expression on its right to the variable on its left; that is,
j += iis equal toj = j + i.

There is an “off-by-one” issue that you need to be aware if when working with arrays.
The first element in theseriesarray isseries[0]. Therefore, if the array holds
max_primeselements, the last element in the array isseries[max_primes-1]. If
max_primesis 100, the last element, then, isseries[99]. Accessingseries[100]
is invalid.

```
8 — Using functions
```

Using functions

```
Larger programs separate tasks and operations into functions. Using functions
increases the modularity of programs. And, when well written, functions are re-
suable in other scripts. The following example implements a function to calculate
numbers from the Fibonacci series.
The Fibonacci sequence was coined by Leonardo “Fibonacci” of Pisa, an Italian
mathematician of the 13 thcentury —whose greatest achievement was popularizing
the Hindu-Arabic numerals in the Western world. The goal of the sequence was to
describe the growth of a population of (idealized) rabbits; and the sequence is 1, 1,
2, 3, 5, 8, 13, 21,:::(every next value is the sum of its two predecessors).
LISTING: fib.p
/* Calculation of Fibonacci numbers by iteration */
@start()
{
print "Enter a value: "
var v = getvalue()
if (v > 0)
printf "The value of Fibonacci number %d is %d\n",
v, fibonacci(v)
else
printf "The Fibonacci number %d does not exist\n", v
}
fibonacci(n)
{
assert n > 0
var a = 0, b = 1
for (var i = 2; i < n; i++)
{
var c = a + b
a = b
b = c
}
return a + b
}
```

“assert” statement: Theassertinstruction at the top of thefibonaccifunction deserves explicit men-
96 tion; it guards against “impossible” or invalid conditions. A negative Fibonacci
number isinvalid, and theassertstatement flags it as a programmerʼs error if this
case ever occurs. Assertions should only flag programmerʼs errors, never user in-
put errors.

The implementation of a user-defined function is not much different than that of
Functions: proper-
ties & features: 60 function@start. Functionfibonaccishows two new concepts, though: it receives
an input value through a parameter and it returns a value (it has a “result”).
Function parameters are declared in the function header; the single parameter in
this example is “n”. Inside the function, a parameter behaves as a local variable,
but one whose value is passed from the outside at thecallto the function.

```
Using functions — 9
```

Thereturnstatement ends a function and sets the result of the function. It need
not appear at the very end of the function; early exits are permitted.

The@startfunction of the Fibonacci example calls predefined “native” functions,
Native function in-
likegetvalueandprintf, as well as the user-defined functionfibonacci. From terface: 72
the perspective ofcallinga function (as in function@start), there is no difference
between user-defined and native functions.

The Fibonacci numbers sequence describes a surprising variety of natural phe-
nomena. For example, the two or three sets of spirals in pineapples, pine cones
and sunflowers usually have consecutive Fibonacci numbers between 5 and 89 as
their number of spirals. The numbers that occur naturally in branching patterns
(e.g. that of plants) are indeed Fibonacci numbers. Finally, although the Fibonacci
sequence isnota geometric sequence, the further the sequence is extended, the
more closely the ratio between successive terms approaches theGolden Ratio, of
1.618 that appears so often in art and architecture.

Call-by-reference and call-by-value

Dates are a particularly rich source of algorithms and conversion routines, because
the calenders that a date refers to have known such a diversity, through time and
around the world.

The “Julian Day Number” is attributed to Josephus Scaligeryand it counts the num-
ber of days since November 24, 4714 BC (proleptic Gregorian calendarz). Scaliger
chose that date because it marked the coincidence of three well-established cycles:
the 28-year Solar Cycle (of the old Julian calendar), the 19-year Metonic Cycle and
the 15-year Indiction Cycle (periodic taxes or governmental requisitions in ancient
Rome), and because no literature or recorded history was known to pre-date that
particular date in the remote past. Scaliger used this concept to reconcile dates in
historic documents, later astronomers embraced it to calculate intervals between
two events more easily.

Julian Day numbers (sometimes denoted with unit “JD”) should not be confused
with Julian Dates (the number of days since the start of thesameyear), or with the
Julian calendar that was introduced by Julius Caesar.

 The exact value for the Golden Ratio is (^1) = 2 (p5 + 1). The relation between Fibonacci numbers
and the Golden Ratio also allows for a “direct” calculation of any sequence number, instead of the
iterative method described here.
yThere is some debate on exactlywhatJosephus Scaliger invented andwhoorwhathe called it
after.
zThe Gregorian calendar was decreed to start on 15 October 1582 by pope Gregory XIII, which
means that earlier dates do not really exist in the Gregorian calendar. When extending the Gre-
gorian calendar to days before 15 October 1582, we refer to it as theprolepticGregorian calendar.

10 — Using functions

Below is a program that calculates the Julian Day number from a date in the (pro-
leptic) Gregorian calendar, and vice versa. Note that in the proleptic Gregorian
calendar, the first year is 1 AD (Anno Domini) and the year before that is 1 BC (Be-
fore Christ): year zero does not exist! The program uses negative year values for BC
years and positive (non-zero) values for AD years.

LISTING: julian.p
/*calculate Julian Day number from a date, and vice versa*/

@start()
{
var d, m, y, jdn
print "Give a date (dd-mm-yyyy): "
d = getvalue(_, '-', '/')
m = getvalue(_, '-', '/')
y = getvalue()
jdn = DateToJulian(d, m, y)
printf "Date %d/%d/%d = %d JD\n", d, m, y, jdn
print "Give a Julian Day Number: "
jdn = getvalue()
JulianToDate jdn, d, m, y
printf "%d JD = %d/%d/%d\n", jdn, d, m, y
}

DateToJulian(day, month, year)
{
/* The first year is 1. Year 0 does not exist: it is 1 BC (or -1) _/
assert year != 0
if (year < 0)
year++
/_ move January and February to the end of the previous year */
if (month <= 2)
year--, month += 12
var jdn = 365*year + year/4 - year/100 + year/
- (153*month - 457) / 5
- day + 1721119
return jdn
}

JulianToDate(jdn, &day, &month, &year)
{
jdn -= 1721119
/*approximate year, then adjust in a loop */
year = (400 * jdn) / 146097
while (365*year + year/4 - year/100 + year/400 < jdn)
year++
year--
/* determine month */
jdn -= 365*year + year/4 - year/100 + year/
month = (5*jdn + 457) / 153
/* determine day */
day = jdn - (153*month - 457) / 5
/*move January and February to start of the year*/
if (month > 12)

```
Using functions — 11
```

```
month -= 12, year++
/* adjust negative years (year 0 must become 1 BC, or -1) */
if (year <= 0)
year--
}
```

Function@startstarts with creating the variables to hold the day, month and year,
and the calculated Julian Day number. Then it reads in a date —three calls toget-
value— and calls functionDateToJulianto calculate the day number. After calcu-
lating the result,@startprints the date that you entered and the Julian Day number
for that date.

Near the top of functionDateToJulian, it increments theyearvalue if it is nega-
tive; it does this to cope with the absence of a “zero” year in the proleptic Grego-
rian calendar. In other words, functionDateToJulianmodifies its function argu-
ments (later, it also modifiesmonth). Inside a function, an argument behaves like
a local variable: you may modify it. These modifications remain local to the func-
tionDateToJulian, however. Function@startpasses the values ofd,mandyinto
DateToJulian, who maps them to its function argumentsday,monthandyearre-
spectively.AlthoughDateToJulianmodifiesyearandmonth, it does not changey
“Call by value” ver-
sus “call by refer-
ence”: 61

andmin function@start; it only changes local copies ofyandm. This concept is
called “call by value”.

The example intentionally uses different names for the local variables in the func-
tions@startandDateToJulian, for the purpose of making the above explanation
easier. Renaming@startʼs variablesd,mandytoday,monthandyearrespectively,
does not change the matter: then you just happen to have two local variables called
day, two calledmonthand two calledyear, which is perfectly valid in PAWN.

The remainder of functionDateToJulianis, regarding the PAWNlanguage, unin-
teresting arithmetic.

Returning to the second part of the function@startwe see that it now asks for a day
number and calls another function,JulianToDate, to find the date that matches
the day number. FunctionJulianToDateis interesting because it takes one input
argument (the Julian Day number) and needs to calculate three output values, the
day, month and year. Alas, a function can only have a single return value —that is,
areturnstatement in a function may only containoneexpression. To solve this,
JulianToDatespecifically requests that changes that it makes to some of its func-
tion arguments are copied back to the variables of the caller of the function. Then,
in@start, the variables that must hold the result ofJulianToDateare passed as
arguments toJulianToDate.

FunctionJulianToDatemarks the appropriate arguments for being “copied back
to caller” by prefixing them with an&symbol. Arguments with an&are copied back,
arguments without is are not. “Copying back” is actually not the correct term. An
argument tagged with an&is passed to the function in a special way that allows the

12 — Rational numbers

function to directly modify the original variable. This is called “call by reference”
and an argument that uses it is a “reference argument”.

In other words, if@startpassesytoJulianToDate—who maps it to its func-
tion argumentyear— andJulianToDatechangesyear, thenJulianToDatereally
changesy. Only through reference arguments can a function directly modify a
variable that is declared in a different function.

To summarize the use of call-by-value versus call-by-reference: if a function has
one output value, you typically use areturnstatement; if a function has more out-
put values, you use reference arguments. You may combine the two inside a single
function, for example in a function that returns its “normal” output via a reference
argument and an error code in its return value.

As an aside, many desktop application use conversions to and from Julian Day num-
bers (or varieties of it) to conveniently calculate the number of days between to
dates or to calculate the date that is 90 days from now —for example.

Rational numbers

All calculations done up to this point involved only whole numbers —integer values.
PAWNalso has support for numbers that can hold fractional values: these are called
“rational numbers”. However, whether this support is enabled depends on the host
application.

Rational numbers can be implemented as either floating-point or fixed-point num-
bers. Floating-point arithmetic is commonly used for general-purpose and scien-
tific calculations, while fixed-point arithmetic is more suitable for financial pro-
cessing and applications where rounding errors should not come into play (or at
least, they should be predictable). The PAWNtoolkit has both a floating-point and a
fixed-point module, and the details (and trade-offs) for these modules in their re-
spective documentation. The issue is, however, that a host application may imple-
menteitherfloating-pointorfixed-point,orbothorneither. The program below
requires that at least either kind of rational number support is available; it will fail
to run if the host application does not support rational numbers at all.

LISTING: c2f.p
# include <rational>
@start()
{
var Rational: Celsius
var Rational: Fahrenheit
print "Celsius\t Fahrenheit\n"

```
 Actually, this is already true ofallnative functions, including all native functions that the examples
in this manual use.
```

```
Strings — 13
```

```
for (Celsius = 5; Celsius <= 25; Celsius++)
{
Fahrenheit = (Celsius * 1.8) + 32
printf "%r \t %r\n", Celsius, Fahrenheit
}
}
```

The example program converts a table of degrees Celsius to degrees Fahrenheit.
The first directive of this program is to import definitions for rational number sup-
port from an include file. The file “rational” includes either support for floating-
point numbers or for fixed-point numbers, depending on what is available.

The variablesCelsiusandFahrenheitare declared with a tag “Rational:” be-
tween the keywordvarand the variable name. A tag name denotes the purpose of Tag names:^58
the variable, its permitted use and, as a special case for rational numbers, its mem-
ory lay-out. TheRational:tag tells the PAWNparser that the variablesCelsius
andFahrenheitcontain fractional values, rather than whole numbers.

The equation for obtaining degrees Fahrenheit from degrees Celsius is

### ◦F=^9

### 5

### + 32◦C

The program uses the value 1.8 for the quotient^9 / 5. When rational number support
is enabled, PAWNsupports values with a fractional part behind the decimal point.

The only other non-trivial change from earlier programs is that the format string
for theprintffunction now has variable placeholders denoted with “%r” instead
of “%d”. The placeholder%rprints a rational number at the position;%dis only for
integers (“whole numbers”).

I used the include file “rational” rather than “float” or “fixed” in an attempt to
make the example program portable. If you know that the host application supports
floating point arithmetic, it may be more convenient to “#include” the definitions
from the filefloatand use the tagFloat:instead ofRational—when doing so,
you should also replace%rby%fin the call toprintf. For details on fixed point
and floating point support, please see the application notes “Fixed Point Support
Library” and “Floating Point Support Library” that are available separately.

Strings

PAWNhas no intrinsic “string” type; character strings are stored in arrays, with the
convention that the array element behind the last valid character is zero. Working
with strings is therefore equivalent with working with arrays.

Among the simplest of encryption schemes is one called “ROT13” —actually the
algorithm is quite “weak” from a cryptographical point of view. It is most widely

```
14 — Strings
```

```
used in public electronic forums (BBSes, Usenet) to hide texts from casual reading,
such as the solution to puzzles or riddles. ROT13 simply “rotates” the alphabet by
half its length, i.e. 13 characters. It is a symmetric operation: applying it twice on
the same text reveals the original.
LISTING: rot13.p
/* Simple encryption, using ROT13 */
@start()
{
printf "Please type the string to mangle: "
var str[100]
getstring str, sizeof str, .pack = false
rot13 str
printf "After mangling, the string is: \"%s\"\n", str
}
rot13(string[])
{
for (var index = 0; string[index]; index++)
if ('a' <= string[index] <= 'z')
string[index] = (string[index] - 'a' + 13) % 26 + 'a'
else if ('A' <= string[index] <= 'Z')
string[index] = (string[index] - 'A' + 13) % 26 + 'A'
}
```

```
In the function header ofrot13, the parameter “string” is declared as an array,
but without specifying the size of the array —there is no value between the square
brackets. When you specify a size for an array in a function header, it must match
the size of theactualparameter in the function call. Omitting the array size speci-
fication in the function header removes this restriction and allows the function to
be called with arrays of any size. You must then have some other means of deter-
mining the (maximum) size of the array. In the case of a string parameter, one can
simply search for the zero terminator.
```

```
Theforloop that walks over the string is typical for string processing functions.
The loop condition is “string[index]”, the rule for true/false conditions in PAWN
is that any value is “true”, except zero. That is, when the array cell atstring[index]
is zero, it is “false” and the loop aborts.
```

```
The ROT13 algorithm rotates only letters; digits, punctuation and special charac-
ters are left unaltered. Additionally, upper and lower case letters must be handled
separately. Inside theforloop, twoifstatements filter out the characters of in-
terest. The way that the secondifis chained to the “else” clause of the firstifis
noteworthy, as it is a typical method of testing for multiple non-overlapping condi-
tions.
```

```
Earlier in this chapter, the concept of “call by value” versus “call by reference” was
```

A function that
takes an array as an
argument and that
does not change it,
may mark the ar-
gument as “const”;
seepage 61

```
discussed. When you are working with strings, or arrays in general, note that PAWN
alwayspasses arraysby reference. It does this to conserve memory and to increase
```

```
Strings — 15
```

performance —arrays can be large data structures and passing them by value re-
quires a copy of this data structure to be made, taking both memory and time. Due
to this rule, functionrot13can modify its function parameter (called “string” in
the example) without needing to declare as a reference argument.

Another point of interest are the conditions in the twoifstatements. The first
Relational opera-
if, for example, holds the condition “'a' <= string[index] <= 'z'”, which tors: 92
means that the expression is true if (and only if) both'a' <= string[index]and
string[index] <= 'z'are true. In the combined expression, the relational opera-
tors are said to be “chained”, as they chain multiple comparisons in one condition.

Finally, note how the lastprintfin function@startuses the escape sequence\"
Escape sequence:
to print a double quote. Normally a double quote ends the literal string; the escape 85
sequence “\"” inserts a double quote into the string.

Staying on the subject of strings and arrays, below is a program that separates a
string of text into individual words and counts them. It is a simple program that
shows a few new features of the PAWNlanguage.

LISTING: wcount.p
/*word count: count words on a string that the user types*/
# include <string>

@start()
{
print "Please type a string: "
var string[100]
getstring string, sizeof string, false
var count = 0
var word[20]
var index
for ( ;; )
{
word = strtok(string, index)
if (strlen(word) == 0)
break
count++
printf "Word %d: '%s'\n", count, word
}
printf "\nNumber of words: %d\n", count
}

strtok(const string[], &index)
{
var length = strlen(string)
/*skip leading white space _/
while (index < length && string[index] <= ' ')
index++
/_ store the word letter for letter _/
var offset = index /_ save start position of token _/
var result[20] /_ string to store the word in*/

```
16 — Strings
```

```
while (index < length
&& string[index] > ' '
&& index - offset < sizeof result - 1)
{
result[index - offset] = string[index]
index++
}
result[index - offset] = EOS /* zero-terminate the string */
return result
}
```

```
Function@startfirst displays a message and retrieves a string that the user must
```

“for” loop: (^97) type. Then it enters a loop: writing “for (;;)” creates a loop without initiali-
sation, without increment and without test —it is an infinite loop, equivalent to
“while (true)”. However, where the PAWNparser will give you a warning if you
type “while (true)” (something along the line “redundant test expression; always
true”), “for (;;)” passes the parser without warning.
A typical use for an infinite loop is a case where you need a loop with the test in
the middle —a hybrid between awhileand ado:::whileloop, so to speak. PAWN
does not support loops-with-a-test-in-the middle directly, but you can imitate one
by coding an infinite loop with a conditionalbreak. In this example program, the
loop:
⋄gets a word from the string —code before the test;
⋄tests whether a new word is available, and breaks out of the loop if not —the test
in the middle;
⋄prints the word and its sequence number —code after the test.
As is apparent from the line “word = strtok(string, index)” (and the declaration
of variableword), PAWNsupports array assignment and functions returning arrays.
The PAWNparser verifies that the array thatstrtokreturns has the same size and
dimensions as the variable that it is assigned into.
Functionstrlenis a native function (predefined), butstrtokis not: it must be
implemented by ourselves. The functionstrtokwas inspired by the function of
the same name from C/C++, but it does not modify the source string. Instead it
copies characters from the source string, word for word, into a local array, which
it then returns.
A common operation is to clear a string. There are various ways to do so. The
recommended way to clear a string is to assign a zero-length literal string to the
variable.
LISTING: clearing a string
my_string = "" // assuming my_string is declared as packed array

```
Symbolic subscripts (structured data) — 17
```

Symbolic subscripts (structured data)

In a typeless language, we might assign a different purpose to some array elements
than to other elements in the same array. PAWNsupportssymbolic substriptsthat
allow to assign specific tag names or ranges to individual array elements.

The example to illustrate symbolic subscripts is longer than previous PAWNpro-
grams, and it also displays a few other features, such as global variables and named
parameters.

LISTING: queue.p
/*Priority queue (for simple text strings) _/
# include <string>
@start()
{
var msg[.text{40}, .priority]
/_ insert a few items (read from console input) _/
printf "Please insert a few messages and their priorities; " ...
"end with an empty string\n"
for ( ;; )
{
printf "Message: "
getstring msg.text, .pack = true
if (strlen(msg.text) == 0)
break
printf "Priority: "
msg.priority = getvalue()
if (!insert(msg))
{
printf "Queue is full, cannot insert more items\n"
break
}
}
/_ now print the messages extracted from the queue*/
printf "\nContents of the queue:\n"
while (extract(msg))
printf "[%d] %s\n", msg.priority, msg.text
}
const queuesize = 10
var queue[queuesize][.text{40}, .priority]
var queueitems = 0

insert(const item[.text{40}, .priority])
{
/*check if the queue can hold one more message _/
if (queueitems == queuesize)
return false /_ queue is full _/
/_ find the position to insert it to _/
var pos = queueitems /_ start at the bottom _/
while (pos > 0 && item.priority > queue[pos-1].priority)
--pos /_ higher priority: move up a slot _/
/_ make place for the item at the insertion spot*/
for (var i = queueitems; i > pos; --i)
queue[i] = queue[i-1]

18 — Symbolic subscripts (structured data)

```
/* add the message to the correct slot */
queue[pos] = item
queueitems++
return true
}
```

extract(item[.text{40}, .priority])
{
/*check whether the queue has one more message _/
if (queueitems == 0)
return false /_ queue is empty _/
/_ copy the topmost item _/
item = queue[0]
--queueitems
/_ move the queue one position up*/
for (var i = 0; i < queueitems; ++i)
queue[i] = queue[i+1]
return true
}

Function@startstarts with a declaration of array variablemsg. The array has two
fields, “.text” and “.priority”; the “.text” field is declared as a sub-array hold-
ing 40 characters. The period is required for symbolic subscripts and there may be
no space between the period and the name.

When an array is declared with symbolic subscripts, it mayonlybe indexed with
these subscripts. It would be an error to say, for example, “msg[0]”. On the other
hand, since there can only be a single symbolic subscript between the brackets, the
brackets become optional. That is, you can write “msg.priority” as a shorthand
for “msg.[priority]”.

Further in@startare two loops. Theforloop reads strings and priority values
from the console and inserts them in a queue. Thewhileloop below that extracts
element by element from the queue and prints the information on the screen. The
point to note, is that theforloop stores both the string and the priority number (an
integer) in the same variablemsg; indeed, function@startdeclares only a single
variable. Functiongetstringstores the message text that you type starting at array
msg.textwhile the priority value is stored (by an assignment a few lines lower) in
msg.priority. Theprintffunction in thewhileloop reads the string and the
value from those positions as well.

At the same time, themsgarray is an entity on itself: it is passed in its entirety to
functioninsert. That function, in turn, says near the end “queue[queueitems]
= item”, whereitemis an array with the same declaration as themsgvariable in
@start, andqueueis a two-dimensional array that holdsqueuesizeelements, with
the minor dimension having symbolic subscripts. The declaration ofqueueand
queuesizeare just above functioninsert.

At several spots in the example program, the same symbolic subscripts are re-
peated. In practice, a program would declare the list of symbolic constants in a

```
Bit operations to manipulate “sets” — 19
```

# definedirective and declare the arrays using this text-substition macro. This
saves typing and makes modifications of the declaration easier to maintain. Con-
cretely, when adding near the top of the program the following line:

# define MESSAGE[.text{40}, .priority]

you can declare an array as “msg[MESSAGE]” and subsequently access the symbolic
subscripts.

The example implements a “priority queue”. You can insert a number of messages
into the queue and when these messages all have the same priority, they are ex-
tracted from the queue in the same order. However, when the messages have
different priorities, the one with the highest priority comes out first. The “intel-
ligence” for this operation is inside functioninsert: it first determines the posi-
tion of the new message to add, then moves a few messages one position upward
to make space for the new message. Functionextractsimply always retrieves the
first element of the queue and shifts all remaining elements down by one position.

Note that both functionsinsertandextractwork on two shared variables,queue
andqueueitems. A variable that is declared inside a function, like variablemsgin
function@startcan only be accessed from within that function. A “global variable”
is accessible byallfunctions, and that variable is declared outside the scope of any
function. Variables must still be declared before they are used, so@startcannot
access variablesqueueandqueueitems, but bothinsertandextractcan.

Functionextractreturns the messages with the highest priority via its function
argumentitem. That is, it changes its function argument by copying the first ele-
ment of thequeuearray intoitem. Functioninsertcopies in the other direction
and it does not change its function argumentitem. In such a case, it is advised to
mark the function argument as “const”. This helps the PAWNparser to both check
for errors and to generate better (more compact, quicker) code.

A final remark on this latest sample is the call togetstringin function@start:
Named parameters:
62
getstring: 110

if you look up the function declaration, you will see that it takes three parameters,
two of which are optional. In this example, only the first and the last parameters are
passed in. Note how the example avoids ambiguity about which parameter follows
the first, by putting the argument name in front of the value. By using “named
parameters” rather than positional parameters, the order in which the parameters
are listed is not important. Named parameters are convenient in specifying —and
deciphering— long parameter lists.

Bit operations to manipulate “sets”

A few algorithms are most easily solved with “set operations”, like intersection,
union and inversion. In the figure below, for example, we want to design an al-
gorithm that returns us the points that can be reached from some other point in a

20 — Bit operations to manipulate “sets”

specified maximum number of steps. For example, if we ask it to return the points
that can be reached in two steps starting from **B** , the algorithm has to return **C** , **D** ,
**E** and **F** , but not **G** because **G** takes three steps from **B**.

Our approach is to keep, for each point in the graph, the set of other points that it
can reach inonestep —this is the “next_step” set. We also have a “result” set that
keeps all points that we have found so far. We start by setting theresultset equal
to thenext_stepset for the departure point. Now we have in theresultset all
points that one can reach in one step. Then, for every point in ourresultset, we
create a union of theresultset and thenext_stepset for that point. This process
is iterated for a specified number of loops.

An example may clarify the procedure outlined above. When the departure point
is **B** , we start by setting theresultset to **D** and **E** —these are the points that one can
reach from **B** in one step. Then, we walk through theresultset. The first point
that we encounter in the set is **D** , and we check what points can be reached from **D**
in one step: these are **C** and **F**. So we add **C** and **F** to theresultset. We knew that
the points that can be reached from **D** in one step are **C** and **F** , because **C** and **F** are
in thenext_stepset for **D**. So what we do is to merge thenext_stepset for point
**D** into theresultset. The merge is called a “union” in set theory. That handles
**D**. The originalresultset also contained point **E** , but thenext_stepset for **E** is
empty, so no more point is added. The newresultset therefore now contains **C** ,
**D** , **E** and **F**.

A set is a general purpose container for elements. The only information that a set
holds of an element is whether it is present in the set or not. The order of elements
in a set is insignificant and a set cannot contain the same element multiple times.
The PAWNlanguage does not provide a “set” data type or operators that work on
sets. However, sets with up to 32 elements can be simulated by bit operations. It
takes just one bit to store a “present/absent” status and a 32-bit cell can therefore
maintain the status for 32 set elements —provided that each element is assigned a
unique bit position.

The relation between set operations and bitwise operations is summarized in the
following table. In the table, an upper case letter stands for a set and a lower case

```
Bit operations to manipulate “sets” — 21
```

letter for an element from that set.

```
concept mathematical notation PAWNexpression
intersection A\B A & B
union A[B A | B
complement A ~A
empty set " 0
membership x 2 A (1 << x) & A
```

To test for membership —that is, to query whether a set holds a particular element,
create a set with just one element and take the intersection. If the result is 0 (the
empty set) the element is not in the set. Bit numbering starts typically at zero; the
lowest bit is bit 0 and the highest bit in a 32-bit cell is bit 31. To make a cell with
only bit 7 set, shift the value 1 left by seven —or in a PAWNexpression: “1 << 7”.

Below is the program that implements the algorithm described earlier to find all
points that can be reached from a specific departure in a given number of steps.
The algorithm is completely in thefindtargetsfunction.

LISTING: set.p
/*Set operations, using bit arithmetic*/

const
{ A = 0b0000001,
B = 0b0000010,
C = 0b0000100,
D = 0b0001000,
E = 0b0010000,
F = 0b0100000,
G = 0b1000000
}

@start()
{
var nextstep[] =
[ C | E, /*A can reach C and E _/
D | E, /_ B " " D and E _/
G, /_ C " " G _/
C | F, /_ D " " C and F _/
0, /_ E " " none _/
0, /_ F " " none _/
E | F, /_ G " " E and F _/
]
print "The departure point: "
var start = clamp( .value = toupper(getchar()) - 'A',
.min = 0,
.max = sizeof nextstep - 1
)
print "\nThe number of steps: "
var steps = getvalue()
/_ make the set*/
var result = findtargets(start, steps, nextstep)
printf "The points in range of %c in %d steps: ", start + 'A', steps

```
22 — Bit operations to manipulate “sets”
```

```
for (var i = 0; i < sizeof nextstep; i++)
if (result & 1 << i)
printf "%c ", i + 'A'
}
findtargets(start, steps, nextstep[], numpoints = sizeof nextstep)
{
var result = 0
var addedpoints = nextstep[start]
while (steps-- > 0 && result != addedpoints)
{
result = addedpoints
for (var i = 0; i < numpoints; i++)
if (result & 1 << i)
addedpoints |= nextstep[i]
}
return result
}
```

Theconststatement just below the header of the@startfunction declares the
“const” statement:
87 constants for the nodesAtoG, usingbinary radixso that that only a single bit is
set in each value.

```
When working with sets, a typical task that pops up is to determine the number of
```

cellbits: (^87) elements in the set. A straightforward function that does this is below:
LISTING: simple bitcount function
bitcount(set)
{
var count = 0
for (var i = 0; i < cellbits; i++)
if (set & (1 << i))
count++
return count
}
With a cell size of 32 bits, this functionʼs loop iterates 32 times to check for a single
bit at each iteration. With a bit of binary arithmetic magic, we can reduce it to loop
only for the number of bits that are “set”. That is, the following function iterates
only once if the input value has only one bit set:
LISTING: improved bitcount function
bitcount(set)
{
var count = 0
if (set)
do
count++
while ((set = set & (set - 1)))
return count
}

```
A simple RPN calculator — 23
```

A simple RPN calculator

The common mathematical notation, with arithmetic expressions like “ 26  3 
Algebraic notation
is also called “infix”
notation

(5 + 2)”, is known as thealgebraicnotation. It is a compact notation and we have
grown accustomed to it. PAWNand by far most other programming languages use
the algebraic notation for their programming expressions. The algebraic notation
does have a few disadvantages, though. For instance, it occasionallyrequiresthat
the order of operations is made explicit by folding a part of the expression in paren-
theses. The expression at the top of this paragraphcanbe rewritten to eliminate the
parentheses, but at the cost of nearly doubling its length. In practice, the algebraic
notation is augmented with precedence level rules that say, for example, that mul-
tiplication goes before addition and subtraction.Precedence levels greatly reduce
the need for parentheses, but it does not fully avoid them. Worse is that when the
number of operators grows large, the hierarchy of precedence levels and the partic-
ular precedence level for each operator becomes hard to memorize —which is why
an operator-rich language as APL does away with precedence levels altogether.

Around 1920, the Polish mathematician Jan Łukasiewicz demonstrated that, when
putting the operators in front of their operands, instead of between them, prece-
dence levels became redundant and parentheses were never necessary. This no-
tation became known as the “Polish Notation”.yLater, Charles Hamblin proposed
Reverse Polish No-
tation is also called
“postfix” notation

to put operatorsbehindthe operands, calling it the “Reverse Polish Notation”. The
advantage ofreversingthe order is that the operators are listed in the same order as
they must be executed: when reading the operators from the left to the right, you
also have the operations to perform in that order. The algebraic expression from
the beginning of this section would read inRPNas:

```
26 3 5 2+
```

When looking at the operators only, we have: first an addition, then a multiplication
and finally a subtraction. The operands of each operator are read from right to left:
the operands for the+operator are the values 5 and 2, those for theoperator are
the result of the previous addition and the value 3, and so on.

It is helpful to imagine the values to be stacked on a pile, where the operators take
one or more operands from the top of the pile and put a result back on top of
the pile. When reading through theRPNexpression, the values 26, 3, 5 and 2 are
“stacked” in that order. The operator+removes the top two elements from the
stack (5 and 2) and pushes the sum of these values back —the stack now reads “26
3 7”. Then, theoperator removes 3 and 7 and pushes the product of the values

```
 These rules are often summarized in a mnemonic like “Please Excuse My Dear Aunt Sally” (Paren-
theses, Exponentiation, Multiplication, Division, Addition, Subtraction).
```

```
yPolish Notation is completely unrelated to “Hungarian Notation” —which is just the habit of adding
“type” or “purpose” identification warts to names of variables or functions.
```

24 — A simple RPN calculator

onto the stack —the stack is “26 21”. Finally, theoperator subtracts 21 from 26
and stores the single value 5, the end result of the expression, back onto the stack.

Reverse Polish Notation became popular because it was easy to understand and
easy to implement in (early) calculators. It also opens the way to operators with
more than two operands (e.g. integration) or operators with more than one result
(e.g. conversion between polar and Cartesian coordinates).

The main program for a Reverse Polish Notation calculator is below:

LISTING: rpn.p
/*a simple RPN calculator*/
# include strtok
# include stack
# include rpnparse

main()
{
print "Type expressions in Reverse Polish Notation " ...
"(or an empty line to quit)\n"
new string{100}
while (getstring(string, .pack = true))
rpncalc string
}

The main program contains very little code itself; instead itincludesthe required
code from three other files, each of which implements a few functions that, to-
gether, build theRPNcalculator. When programs or scripts get larger, it is usually
advised to spread the implementation over several files, in order to make mainte-
nance easier.

Function@startfirst puts up a prompt and calls the native functiongetstringto
read an expression that the user types. Then it calls the custom functionrpncalc
to do the real work. Functionrpncalcis implemented in the filerpnparse.inc,
reproduced below:

LISTING: rpnparse.i
/*main rpn parser and lexical analysis, part of the RPN calculator*/
# include <rational>
# include <string>

# define Token [
.type, /*operator or token type _/
Rational: .value, /_ value, if t_type is "Number" _/
.word{20}, /_ raw string*/
]
const Number = '0'
const EndOfExpr = '#'
rpncalc(const string{})
{
new index
new field[Token]
for ( ;; )
{
field = gettoken(string, index)

```
A simple RPN calculator — 25
```

```
switch (field.type)
{
case Number:
push field.value
case '+':
push pop() + pop()
case '-':
push - pop() + pop()
case '*':
push pop() * pop()
case '/', ':':
push 1.0 / pop() * pop()
case EndOfExpr:
break /* exit "for" loop */
default:
printf "Unknown operator '%s'\n", field.word
}
}
printf "Result = %r\n", pop()
if (clearstack())
print "Stack not empty\n", red
}
```

gettoken(const string{}, &index)
{
/*first get the next "word" from the string _/
new word{20}
word = strtok(string, index)
/_ then parse it _/
new field[Token]
field.word = word
if (strlen(word) == 0)
{
field.type = EndOfExpr /_ special "stop" symbol*/
field.value = 0
}
else if ('0' <= word{0} <= '9')
{
field.type = Number
field.value = rval(word)
}
else
{
field.type = word{0}
field.value = 0
}
return field
}

TheRPNcalculator uses rational numbers andrpnparse.incincludes the “ratio-
Rational numbers,
see also the “Cel-
sius to Fahrenheit”
example onpage 12

nal” file for that purpose. Almost all of the operations on rational numbers is hid-
den in the arithmetic. The only direct references to rational numbers are the “%r”
format code in theprintfstatement near the bottom of functionrpncalcand the
call torationalstrhalfway functiongettoken.

Near the top in the filerpnparse.incis a preprocessor macro that declares the Preprocessor: 79

```
26 — A simple RPN calculator
```

symbolic subscripts for an array. The macro name, “Token” will be used throughout
the program to declare arrays with those fields. For example, functionrpncalc
declares variablefieldas an array using the macro to declare the field names.
Arrays with symbolic subscripts were already introduced in the sectionArrays and
symbolic subscriptsonpage 17; this script shows another feature of symbolic sub-
scripts: individual substripts may have a tag name of their own. In this example,
.typeis a simple cell,.valueis a rational value (with a fractional part) that is
tagged as such, and.wordcan hold a string of 20 characters (includding the ter-
minating zero byte). See, for example, the line:
printf "Unknown operator '%s'\n", field.word
how the.wordsubscript of thefieldvariable is used as a string.
If you know C/C++or Java, you may want to look at theswitchstatement. The
“switch” statement:
99 switchstatement differs in a number of ways from the other languages that provide
it. The cases are not fall-through, for example, which in turn means that thebreak
statement for thecase EndOfExprbreaks out of the enclosing loop, instead of out
of theswitch.
On the top of theforloop in functionrpncalc, you will find the instruction “field
= gettoken(string, index)”. As already exemplified in thewcount.p(“word
count”) program onpage 15, functions may return arrays. It gets more interesting
for a similar line in functiongettoken:
field.word = word
wherewordis an array for 20 characters andfieldis an array with 3 (symbolic)
subscripts. However, as the.wordsubscript is declared as having a size of 20 char-
acters, the expression “field.word” is considered a sub-array of 20 characters, pre-
cisely matching the array size ofword.
LISTING: strtok.i
/*extract words from a string (words are separated by white space) _/
# include <string>
strtok(const string{}, &index)
{
new length = strlen(string)
/_ skip leading white space _/
while (index < length && string{index} <= ' ')
index++
/_ store the word letter for letter _/
new offset = index /_ save start position of token _/
const wordlength = 20 /_ maximum word length _/
new result{wordlength} /_ string to store the word in _/
while (index < length
&& string{index} > ' '
&& index - offset < wordlength)
{
result{index - offset} = string{index}
index++
}
result{index - offset} = EOS /_ zero-terminate the string*/

```
A simple RPN calculator — 27
```

```
return result
}
```

Functionstrtokis the same as the one used in thewcount.pexample. It is im-
plemented in a separate file for theRPNcalculator program. Note that thestrtok wcount.p:^15
function as it is implemented here can only handle words with up to 19 characters
—the 20 thcharacter is the zero terminator. A truly general purpose re-usable im-
plementation of anstrtokfunction would pass the destination array as a parame-
ter, so that it could handle words of any size. Supporting both packed and unpack
strings would also be a useful feature of a general purpose function.

When discussing the merits of Reverse Polish Notation, I mentioned that a stack is
both an aid in “visualizing” the algorithm as well as a convenient method to imple-
ment anRPNparser. This exampleRPNcalculator, uses a stack with the ubiquitous
functionspushandpop. For error checking and resetting the stack, there is a third
function that clears the stack.

LISTING: stack.i
/*stack functions, part of the RPN calculator*/
# include <rational>

static Rational: stack[50]
static stackidx = 0

push(Rational: value)
{
assert stackidx < sizeof stack
stack[stackidx++] = value
}

Rational: pop()
{
assert stackidx > 0
return stack[--stackidx]
}

clearstack()
{
assert stackidx >= 0
if (stackidx == 0)
return false
stackidx = 0
return true
}

The filestack.incincludes the filerationalagain. This is technically not neces-
sary (rpnparse.incalready included the definitions for rational number support),
but it does not do any harm either and, for the sake of code re-use, it is better to
make any file include the definitions of the libraries that it depends on.

Notice how the two global variablesstackandstackidxare declared as “static”
variables; using the keywordstaticinstead ofvar. Doing this makes the global
variables “visible” in that file only. For all other files in a larger project, the sym-
bolsstackandstackidxare invisible and they cannot (accidentally) modify the

28 — Event-driven programming

variables. It also allows the other modules to declare their ownprivatevariables
with these names, so it avoids name clashing.

TheRPNcalculator is actually still a fairly small program, but it has been set up as if
it were a larger program. It was also designed to demonstrate a set of elements of
the PAWNlanguage and the example program could have been implemented more
compactly.

Event-driven programming

All of the example programs that were developed in this chapter so far, have used
a “flow-driven” programming model: they start with@startand the code deter-
mines what to do and when to request input. This programming model is easy to
understand and it nicely fits most programming languages, but it is also a model
does not fit many “real life” situations. Quite often, a program cannot simply pro-
cess data and suggest that the user provides input only when it is ready for him/
her. Instead, it is the user who decides when to provide input, and the program or
script should be prepared to process it in an acceptable time, regardless of what it
was doing at the moment.

The above description suggests that a program should therefore be able to interrupt
its work and do other things before picking up the original task. In early implemen-
tations, this was indeed how such functionality was implemented: a multi-tasking
system where one task (or thread) managed the background tasks and a second
task/thread that sits in a loop continuously requesting user input. This is a heavy-
weight solution, however. A more light-weight implementation of a responsive sys-
tem is what is called the “event-driven” programming model.

In the event-driven programming model, a program or script splits any lengthy
(background) task into short manageable blocks and in between, it is available for
input. Instead of having the program poll for input, however, the host application
(or some other sub-system) calls a function that is attached to the event —but only
if the event occurs.

A typical event is “input”. Observe that input does not only come from human op-
erators. Input packets can arrive over serial cables, network stacks, internal sub-
systems such as timers and clocks, and all kinds of other equipment that you may
have attached to your system. Many of the apparatus that produce input, just send
it. The arrival of such input is an event, just like a key press. If you do not catch
the event, a few of them may be stored in an internal system queue, but once the
queue is saturated the events are simply dropped.

PAWNdirectly supports the event-driven model, because it supports multiple entry
points. The sole entry point of a flow-driven program is@start; an event-driven
program has an entry point for every event that it captures. When compared to
the flow-driven model, event-driven programs often appear “bottom-up”: instead

```
Event-driven programming — 29
```

of your program calling into the host application and deciding what to do next, your
program isbeing calledfrom the outside and it is required to respond appropriately
and promptly.

PAWNdoes not specify a standard library, and so there is no guarantee that a par-
ticular implementation provides functions likeprintfandgetvalue. Although
it is suggested that every implementation implements some (minimal) console/
terminal interface with a these functions, their availability is ultimately implemen-
tation dependent.The same holds for the public functions —the entry points for a
script. It is implementation-dependent which public functions a host application Public functions:^71
supports. The script in this section may therefore not run on your platform (even
if all previous scripts ran fine). The tools in the standard distribution of the PAWN
system support all scripts developed in this manual, provided that your operating
system or environment supports standard terminal functions such as setting the
cursor position.

An early programming language that was developed solely for teaching the con-
cepts of programming to children was “Logo”. This dialect of LISP made program-
ming visual by having a small robot, the “turtle”, drive over the floor under control
of a simple program. This concept was then copied to moving a (usually triangular)
cursor of the computer display, again under control of a program. A novelty was
that the turtle now left a trail behind it, allowing you to create drawings by prop-
erly programming the turtle —it became known asturtle graphics. The term “turtle
graphics” was also used for drawing interactively with the arrow keys on the key-
board and a “turtle” for the current position. This method of drawing pictures on
the computer was briefly popular before the advent of the mouse.

LISTING: turtle.p
@keypressed(key)
{
/* get current position _/
var x, y
wherexy x, y
/_ determine how the update the current position _/
switch (key)
{
case 'u': y-- /_ up _/
case 'd': y++ /_ down _/
case 'l': x-- /_ left _/
case 'r': x++ /_ right _/
case '\e': exit /_ Escape = exit _/
}
/_ adjust the cursor position and draw something _/
moveturtle x, y
}
moveturtle(x, y)
{
gotoxy x, y
print "_"
gotoxy x, y

30 — Event-driven programming

```
}
```

The entry point of the above program is@keypressed—it is called on a key press.
If you run the program and do not type any key, the function@keypressednever
runs; if you type ten keys,@keypressedruns ten times. Contrast this behaviour
with@start: function@startruns immediately after you start the script and it
runs only once.

It is still allowed to add a@startfunction to an event-driven program: the@start
function will then serve for one-time initialization. A simple addition to this ex-
ample program is to add a@startfunction, in order to clear the console/terminal
window on entry and perhaps set the initial position of the “turtle” to the centre.

Support for function keys and other special keys (e.g. the arrow keys) is highly
system-dependent. On ANSI terminals, these keys produce different codes than
in a Windows “DOS box”. In the spirit of keeping the example program portable, I
have used common letters (“u” forup, “l” forleft, etc.). This does not mean, how-
ever, that special keys are beyond PAWNʼs capabilities.

In the “turtle” script, the “Escape” key terminates the host application through the
instructionexit. For a simple PAWNrun-time host, this will indeed work. With host
applications where the script is an add-on, or host-applications that are embedded
in a device, the script usually cannot terminate the host application.

Multiple events

The advantages of the event-driven programming model, for creating reactive pro-
grams, become apparent in the presence ofmultipleevents. In fact, the event-
driven model is only useful if you have more that one entry point; if your script just
handles a single event, it might as well enter a polling loop for that single event. The
more events need to be handled, the harder the flow-driven programming model
becomes. The script below implements a bare-bones “chat” program, using only
two events: one for sending and one for receiving. The script allows users on a
network (or perhaps over another connection) to exchange single-line messages.

The script depends on the host application to provide the native and public func-
tions for sending and receiving “datagrams” and for responding to keys that are
typed in.Howthe host application sends its messages, over a serial line or using
TCP/IP, the host application may decide itself. The tools in the standard PAWNdis-
tribution push the messages over the TCP/IP network, and allow for a “broadcast”
mode so that more than two people can chat with each other.

LISTING: chat.p
# include <datagram>
const cellchars = cellbits / charbits

@receivestring(const message[], const source[])
printf "[%s] says: %s\n", source, message

```
State programming — 31
```

@keypressed(key)
{
static string{100}
static index
if (key == '\e')
exit /*quit on 'Esc' key _/
echo key
if (key == '\r' || key == '\n' || index == sizeof string * cellchars)
{
string{index} = '\0' /_ terminate string*/
sendstring string
index = 0
string{index} = '\0'
}
else
string{index++} = key
}

echo(key)
{
var string{2} = { 0 }
string{0} = key == '\r'? '\n' : key
printf string
}

The bulk of the above script handles gathering received key-presses into a string
and sending that string after seeing theENTERkey. The “Escape” key ends the pro-
gram. The functionechoserves to give visual feedback of what the user types: it
builds a zero-terminated string from the key and prints it.

Despite its simplicity, this script has the interesting property that there is no fixed
or prescribed order in which the messages are to be sent or received —there is no
query–reply scheme where each host takes its turn in talking & listening. A new
message may even be received while the user is typing its own message.

State programming

In a program following the event-driven model, events arrive individually, and they
are also responded to individually. On occasion, though, an event is part of a se-
quential flow, that must be handled in order. Examples are data transfer protocols
over, for example, a serial line. Each event may carry a command, a snippet of data
that is part of a larger file, an acknowledgement, or other signals that take part in
the protocol. For the stream of events (and the data packets that they carry) to make
sense, the event-driven program must follow a precise hand-shaking protocol.

```
 As this script makes no attempt to separate received messages from typed messages (for example,
in two different scrollable regions), the terminal/console will look confusing when this happens.
With an improved user-interface, this simple script could indeed be a nice message-base chat
program.
```

32 — State programming

To adhere to a protocol, an event-driven program must respond to each event in
compliance with the (recent) history of events received earlier and the responses
to those events. In other words, the handling of one event may set up a “condition”
or “environment” for the handling any one or more subsequent events.

A simple, but quite effective, abstraction for constructing reactive systems that
need to follow (partially) sequential protocols, is that of the “automaton” or state
machine. As the number of states are usually finite, the theory often refers to such
automatons asFinite State AutomatonsorFinite State Machines. In an automaton, the
context (or condition) of an event is itsstate. An event that arrives may be handled
differently depending on the state of the automaton, and in response to an event,
the automaton may switch to another state —this is called atransition. A transition,
in other words, as a response of the automaton to an event in the context of its state.

Automatons are very common in software as well as in mechanical devices (you
may see the Jacquard Loom as an early state machine). Automatons, with a finite
number of states, are deterministic (i.e. predictable in behaviour) and their rel-
atively simple design allows a straightforward implementation from a “state dia-
gram”.

In a state diagram, the states are usually represented as circles or rounded rectan-
gles and the arrows represent the transitions. As transitions are the response of the
automaton to events, an arrow may also be seen as an event “that does something”.
An event/transition that is not defined in a particular state is assumed to have no
effect —it is silently ignored. A filled dot represents the entry state, which your pro-
gram (or the host application) must set in start-up. It is common to omit in a state
diagram all event arrows that drop back into the same state, but for the preceding
figure I have chosen to make the response to all events explicit.

The above state diagram is for “parsing” comments that start with “/_” and end
with “_/”. There are states for plain text and for text inside a comment, plus two
states fortentativeentry into or exit from a comment. The automaton is intended
to parse the commentsinteractively, from characters that the user types on the key-
board. Therefore, the only events that the automaton reacts on are key presses. Ac-

```
State programming — 33
```

tually, there is onlyoneevent (“key-press”) and the state switches are determined
by eventʼs parameter: thekey.

PAWNsupports automatons and states directly in the language. Every function
may optionally have one or more states assigned to it. PAWNalso supports multiple
automatons, and each state is part of a particular automaton. The following script
implements the preceding state diagram (in a single, anonymous, automaton). To
differentiate plain text from comments, both are output in a different colour.

LISTING: comment.p
/*parse C comments interactively, using events and a state machine*/

@start()
state plain

@keypressed(key) <plain>
{
state (key == '/') slash
if (key != '/')
echo key
}

@keypressed(key) <slash>
{
state (key != '/') plain
state (key == '_') comment
echo '/' /_ print '/' held back from previous state */
if (key != '/')
echo key
}

@keypressed(key) <comment>
{
echo key
state (key == '*') star
}

@keypressed(key) <star>
{
echo key
state (key != '*') comment
state (key == '/') plain
}

echo(key) <plain, slash>
printchar key, yellow

echo(key) <comment, star>
printchar key, green

printchar(ch, colour)
{
setattr .foreground = colour
printf "%c", ch
}

```
 With the exception of “native functions” and user-defined operators.
```

34 — State programming

Function@startsets the starting state to “plain” and exits; all logic is event-driven.
When a key arrives in stateplain, the program checks for a slash and condition-
ally prints the received key. The interaction between the statesplainandslash
demonstrates a complexity that is typical for automatons: you must decide how to
respond to an event when it arrives, without being able to “peek ahead” or undo
responses to earlier events. This is usually the case for event-driven systems —you
neither knowwhatevent you will receive next, norwhenyou will receive it, and
whatever your response to the current event, there is a good chance that you can-
not erase it on a future event and pretend that it never happened.

In our particular case, when a slash arrives, thismight bethe start of a comment
sequence (“/*”), but it is not necessarily so. By inference, we cannot decide on
reception of the slash character what colour to print it in. Hence, we hold it back.
However, there is no global variable in the script that says that a character is held
back —in fact, apart from function parameters, no variable is declared at all in this
script. The information about a character being held back is “hidden” in the state
of the automaton.

As is apparent in the script, state changes may be conditional. The condition is
optional, and you can also use the commonif–elseconstruct to change states.

Being state-dependent is not reserved for the event functions. Other functions may
have state declarations as well, as theechofunction demonstrates. When a func-
tion would have the same implementation for several states, you just need to write
a single implementation and mention all applicable states. For functionechothere
are two implementations to handle the four states.y

That said, an automaton must be prepared to handleallevents inanystate. Typ-
ically, the automaton has neither control overwhichevents arrive nor overwhen
they arrive, so not handling an event in some state could lead to wrong decisions. It
frequently happens, then, that a some events are meaningful only in a few specific
states and that they should trigger an error or “reset” procedure in all other cases.
The function for handling the event in such “error” condition might then hold a
lot of state names, if you were to mention them explicitly. There is a shorter way:
by not mentioninganyname between the angle brackets, the function matchesall
states that have not explicit implementation elsewhere. So, for example, you could
use the signature “echo(key) <>” for either of the two implementations (but not
for both).

A single anonymous automaton is pre-defined. If a program contains more than
one automaton, the others must be explicitly mentioned, both in the state classi-
fier of the function and in thestateinstruction. To do so, add the name of the
automaton in front of the state name and separate the names of the automaton and

```
yA function that has the same implementation forallstates, does not need a state classifier at all
—seeprintchar.
```

```
State programming — 35
```

the state with a colon. That is, “parser:slash” stands for the stateslashof the au-
tomatonparser. A function can only be part of a single automaton; you can share
one implementation of a function for several states ofthe sameautomaton, but you
cannot share that function for states ofdifferentautomatons.

Entry functions and automata theory

State machines, and the foundation of “automata theory”, originate from mechani-
cal design and pneumatic/electric switching circuits (using relays rather than tran-
sistors). Typical examples are coin acceptors, traffic light control and communica-
tion lines switching circuits. In these applications, robustness and predictability
are paramount, and it was found that in this context it was best to link actions (out-
put) to thestatesrather than to theevents(input). In this design, entering a state
causes activity —events cause state changes, but do notdirectlycarry out opera-
tions.

In a pedestrian crossing lights system, the lights for the vehicles and the pedestri-
ans must be synchronized. Technically, there are six possible combinations, but
obviously the combination of a green light for the traffic and a “walk” sign for the
pedestrians is recipe for disaster. We can also immediately dismiss the combina-
tion ofyellow/walkas too dangerous. Thus, four combinations remain to be han-

36 — State programming

dled. The figure above is a state diagram for the pedestrian crossing lights. The
entire process is activated with a button, and operates on a timer.

When the statered/walktimes out, it cannot immediately go back togreen/wait, be-
cause the pedestrians that are busy crossing the road at that moment need some
time to clear the road —the statered/waitallows for this. For purpose of demon-
stration, this pedestrian crossing has the added functionality that when a pedes-
trian pushes the button while the light for the traffic is already red, the time that the
pedestrian has for crossing is lengthened. If the state isred/waitand the button is
pressed, it switches back tored/walk. The enfolding box around the statesred/walk
andred/waitfor handling the button event is just a notational convenience: I could
also have drawn two arrows from either state back tored/walk. The script source
code (which follows below) reflects this same notational convenience, though.

In the implementation in PAWN, all event functions now have only a single state-
ment, which is either a state change or an empty statement. Events that do not
cause a state change are absent in the diagram, but theymustbe handled in the
script; hence, the “fall-back” event functions that do nothing. The output is all done
in the special functionsentry—in this example program, the output consists only
of messages printed on the console. The functionentrymay be seen as a@start
for a state: it is implicitly called when the state that it is attached to is entered.
Note that theentryfunction is also called when “switching” to the state that the
automaton is already in: when the state isred_walkan invocation of the@key-
pressedsets the state tored_walk(which it is already in) and causes theentry
function ofred_walkto run —this is a re-entry of the state.

LISTING: traffic.p
/*traffic light synchronizer, using states in an event-driven model*/
# include <time>
@start() state green_wait

@keypressed(key) <green_wait> state yellow_wait
@keypressed(key) <red_walk, red_wait> state red_walk
@keypressed(key) <> {} /*fallback*/

@timer() <yellow_wait> state red_walk
@timer() <red_walk> state red_wait
@timer() <red_wait> state green_wait
@timer() <> {} /*fallback*/

entry() <green_wait>
print "Green / Don't walk\n"

entry() <yellow_wait>
{
print "Yellow / Don't walk\n"
settimer 2000
}

entry() <red_walk>
{
print "Red / Walk\n"
settimer 5000

```
State programming — 37
```

```
}
```

entry() <red_wait>
{
print "Red / Don't walk\n"
settimer 2000
}

This example program has an additional dependency on the host application: in ad-
dition to the “@keypressed” event function, the host must also provide a “@timer”
event with an adjustable delay. Because of the timing functions, the script includes
the system filetime.incnear the top of the script.

The event functions with the state changes are all on the top part of the script. The
functions are laid out to take a single line each, to suggest a table-like structure. All
state changes are unconditional in this example, but conditional state changes may
be used withentryfunctions too.

Two transitions to the statered_walkexist (on a@timerevent inyellow_waitstate,
and on a@keypressedevent in eitherred_walkorred_waitstates). These transi-
tions all pass through the sameentryfunction. Thus, by putting all actions in the
entryfunctions and only switching states in the event functions, it reduces and
simplifies the code.

In automata theory, an automaton that associates activity with state entries, such as
this pedestrian traffic lights example, is a “Moore automaton”; an automaton that
associates activity with (state-dependent) events or transitions is a “Mealy automa-
ton”. The interactive comment parser onpage 33is a typical Mealy automaton. The
two kinds are equivalent: a Mealy automaton can be converted to a Moore automa-
ton and vice versa, although a Moore automaton may need more states to imple-
ment the same behaviour. In practice, the models are oftenmixed, with an overall
“Moore automaton” design, and a few “Mealy states” where that saves a state.

State variables

The model of a pedestrian crossing light in the previous example is not very realistic
(its only goal is to demonstrate a few properties of state programming with PAWN).
The first thing that is lacking is a degree offairness: pedestrians should not be able
to block car traffic indefinitely. The car traffic should see a green light for a period
of some minimum duration after pedestrians have had their time slot for crossing
the road. Secondly, many traffic lights have a kind of remote control ability, so that
emergency traffic (ambulance, fire truck,:::) can force green lights on their path.
A well-known example of such remote control is theMIRTsystem (Mobile Infra-Red
Transmitter) but other systems exist —the Netherlands use a radiographic system
calledVETAGfor instance.

The new state diagram for the pedestrian crossing light has two more states, but
more importantly: it needs to save data across events and share it between states.

38 — State programming

When the pedestrian presses the button while the state isred_wait, we neither
want to react on the button immediately (this was our “fairness rule”), nor the but-
ton to be ignored or “forgotten”. Istead, we move to the stategreen_wait_interim
regardless of the button press, but memorize the press for a decision made at the
point of leaving stategreen_wait_interim.

Automatons excel in modelling control flow in reactive and interactive systems, but
data flow has traditionally been a weak point. To see why, consider that each event
is handled individually by a function and that the local variables in that function
disappear when the function returns. Local variables can, hence, not be used to
pass data from one event to the next. Global variables, while providing a work-
around, have drawbacks: global scope and an “eternal” lifespan. If a variable is
used only in the event handlers of a single state, it is desirable to hide it from the
other states, in order to protect it from accidental modification. Likewise, short-
ening the lifespan to the state(s) that the variable is active in, reduces the memory
footprint. “State variables” provide this mix of variable scope and variable lifespan
that are tied to a series of states, rather than to functions or modules.

PAWNenriches the standardfinite state machine(or automaton) with variables that
are declared with a state classifier. These variables are only accessible from the
listed states and the memory these variable hold may be reused by other purposes
while the automaton is in a different state (different than the ones listed). Apart
from the state classifier, the declaration of a state variable is similar to that of a
global variable. The declaration of the variablebutton_memoin the next listing
illustrates the concept.

To reset the memorized button press, the script uses an “exit” function. Just like
anentryfunction is called when entering a state, theexitfunction is called when
leaving a state.

LISTING: traffic2.p
/* a more realistic traffic light synchronizer, including an
- "override" for emergency vehicles
*/
# include <time>

```
State programming — 39
```

@start()
state green_wait_interim
var bool: button_memo <red_wait, green_wait_interim>

@keypressed(key)
{
switch (key)
{
case ' ': button_press
case '*': mirt_detect
}
}
button_press() <green_wait>
state yellow_wait

button_press() <red_wait, green_wait_interim>
button_memo = true
button_press() <> /*fallback*/
{}

mirt_detect()
state mirt_override
@timer() <yellow_wait>
state red_walk

@timer() <red_walk>
state red_wait
@timer() <red_wait>
state green_wait_interim

@timer() <green_wait_interim>
{
state (!button_memo) green_wait
state (button_memo) yellow_wait
}
@timer() <mirt_override>
state green_wait

@timer() <> /*fallback*/
{}

entry() <green_wait_interim>
{
print "Green / Don't walk\n"
settimer 5000
}
exit() <green_wait_interim>
button_memo = false

entry() <yellow_wait>
{
print "Yellow / Don't walk\n"
settimer 2000
}
entry() <red_walk>
{
print "Red / Walk\n"
settimer 5000

40 — State programming

```
}
```

entry() <red_wait>
{
print "Red / Don't walk\n"
settimer 2000
}

entry() <mirt_override>
{
print "Green / Don't walk\n"
settimer 5000
}

State programming wrap-up

The common notation used in state diagrams is to indicate transitions with arrows
and states with circles or rounded rectangles. The circle/rounded rectangle op-
tionally also mentions the actions of anentryorexitfunction and of events that
are handled internally —without causing a transition. The arrow for a transition
contains the name of the event (or pseudo-event), an optional condition between
square brackets and an optional action behind a slash (“/”).

States are ubiquitous, even if we do not always recognize them as such. The concept
of finite state machines has traditionally been applied mostly to programs mim-
icking mechanical apparatus and software that implements communication pro-
tocols. With the appearance of event-driven windowing systems, state machines
now also appear in the GUI design of desktop programs. States abound in web pro-
grams, because the browser and the web-site scripting host have only a weak link.
That said, the state machine in web applications is typically implemented in an
ad-hoc manner.

States can also be recognized in common problems and riddles. In the well known
riddle of the man that must move a cabbage, a sheep and a wolf across a river,the
states are obvious —the trick of the riddle is to avoid theforbiddenstates.

But now that we are discovering “states” everywhere, we must be careful not to
overdo it. For example, in the second implementation of a pedestrian crossing
light, seepage 38, I used a variable (button_memo) to hold a criterion for a decision
made at a later time. An alternative implementation would be to throw in a couple
of more states to hold the situations “red-wait-&-button-pressed” and “green-wait-
interim-&-button-pressed”. No more variable would then be needed, but at the cost
of a more complex state diagram and implementation. In general, the number of
states should be kept small.

```
 A man has to ferry a wolf, a sheep and a cabbage across a river in a boat, but the boat can only
carry the man and a single additional item. If left unguarded, the wolf will eat the sheep and the
sheep will eat the cabbage. How can the man ferry them across the river?
```

```
Program verification — 41
```

Although automata provide a good abstraction to model reactive and interactive
systems, coming to a correct diagram is not straightforward —and sometimes just
outright hard. Too often, the “sunny day scenario” of states and events is plotted
out first, and everything straying from this path is then added on an impromptu
basis. This approach carries the risk that some combinations of events & states are
forgotten, and indeed I have encountered two comment parser diagrams (like the
one atpage 33) by different book/magazine authors that were flawed in such way.
Instead, I advise to focus on the events and on the responses for individual events.
For every state, every event should be considered; do not route events through a
general purpose fall-back too eagerly.

It has become common practice, unfortunately, to introduce automata theory with
applications for which better solutions exist. One, oft repeated, example is that of
an automaton that accumulates the value of a series of coins, or that “calculates”
the remainder after division by 3 of a binary number. These applications may have
made sense in mechanical/pneumatic design where “the state” is the only memory
that the automaton has, but in software, using variables and arithmetic operations
isthe better choice. Another typical example is that of matching words or patterns
using a state machine: every next letter that is input switches to a new state. Lexical
scanners, such as the ones that compilers and interpreters use to interpret source
code, might use such state machines to filter out “reserved words”. However, for
any practical set of reserved words, such automatons become unwieldy, and no
one will design them by hand. In addition, there is no reason why a lexical scanner
cannot peek ahead in the text or jump back to a mark that it set earlier —which is
one of the criteria for choosing a state implementation in the first place, and finally,
solutions like “trie lookups” are likely simpler to design and implement while being
at least as quick.

Program verification

Should the compiler/interpreter not catch all bugs? This rhetorical question has
both technical and philosophical sides. I will forego all non-technical aspects and
only mention that, in practice, there is a trade-off between the “expressiveness”
of a computer language and the “enforced correctness” (or “provable correctnessʼ)
of programs in that language. Making a language very “strict” is not a solution if
work needs to be done that exceeds the size of a toy program. A too strict lan-
guage leaves the programmer struggling with the language, whereas the “problem
to solve” should be therealstruggle and the language should be a simple means to
express the solution in.

The goal of the PAWNlanguage is to provide the developer with an informal, and
convenient to use mechanism to test whether the program behaves how it should.
This mechanism is called “assertions” and, although the concept of assertions pre-

```
42 — Program verification
```

```
dates the idea of “design by contract”, it is most easily explained through the design-
by-contract concept.
```

```
The “design by contract” paradigm provides an alternative approach for dealing
with erroneous conditions. The premise is that the programmer knows the task at
hand, the conditions under which the software must operate and the environment.
In such an environment, each function specifies the specific conditions, in the form
ofassertions, that must hold true before a client may execute the function. In addi-
tion, the function may also specify any conditions that hold true after it completes
its operation. This is the “contract” of the function.
The name “design by contract” was coined by Bertrand Meyer and its principles
trace back to predicate logic and algorithmic analysis.
```

```
⋄Preconditions specify the valid values of the input parameters and environmen-
tal attributes;
```

```
⋄Postconditions specify the output and the (possibly modified) environment;
```

```
⋄Invariants indicate the conditions that must hold true at key points in a function,
regardless of the path taken through the function.
```

```
For example, a function that computes a square root of a number may specify that
its input parameter be non-negative. This is a precondition. It may also specify
that its output, when squared, is the input value 0 :01%. This is a postcondition; it
verifies that the routine operated correctly, within the required precision.A con-
```

Example square
root function (using
bisection): 66

```
venient way to calculate a square root is via “bisection”. At each iteration, this al-
gorithm gives at least one extrabit(binary digit) of accuracy. This is an invariant:
at each iteration, the error of the intermediate result decreases.
```

```
Preconditions, postconditions and invariants are similar in the sense that they all
consist of a test and that a failed test indicates an error in the implementation. As
a result, you can implement preconditions, postconditions and invariants with a
single construct: the “assertion”. For preconditions, write assertions at the very
start of the routine; for invariants, write an assertion where the invariant should
hold; for post conditions, write an assertion before each “return” statement or at
the end of the function.
```

```
In PAWN, the instruction is calledassert; it is a simple statement that contains a
test. If the test outcome is “true”, nothing happens. If the outcome is “false”, the
assert instruction terminates the program with a message containing the details of
the assertion that failed.
```

```
Assertions are checks that should never fail. Genuine errors, such as user input er-
rors, should be handled with explicit tests in the program, andnotwith assertions.
As a rule, the expressions contained in assertions should be free of side effects:
an assertion should never contain code that your application requires for correct
operation.
```

```
Documentation comments — 43
```

This does have the effect, however, that assertions never fire in a bug-free program:
they just make the codefatterandslower, without any user-visible benefit. It is not
this bad, though. An additional feature of assertions is that you can build the source
codewithout assertionssimply using a flag or option to the PAWNparser. The idea is
that you enable assertions during development and build the “retail version” of the
code without assertions. This is a better approach than removing the assertions,
because all assertions are automatically “back” when recompiling the program —
e.g. for maintenance.

During maintenance, or even during the initial development, if you catch a bug that
was not trapped by an assertion, before fixing the bug, you should think of how an
assertion could have trapped this error. Then, add this assertion and test whether
it indeed catches the bugbeforefixing the bug. By doing this, the code will gradually
become sturdier and more reliable.

Documentation comments

When programs become larger, documenting the program and the functions be-
comes vital for its maintenance, especially when working in a team. The PAWN
language tools have some features to assist you in documenting the code in com-
ments. Documenting a program or library in its comments has a few advantages
—for example: documentation is more easily kept up to date with the program, it is
efficient in the sense that programming comments now double as documentation,
and the parser helps your documentation efforts in generating syntax descriptions
and cross references.

Every comment that starts with three slashes (“///”) followed by white-space, or
Comment syntax:
that starts with a slash and two stars (“/** ”) followed by white-space is a special 83
documentationcomment. The PAWNcompiler extracts documentation comments
and optionally writes these to a “report” file. See the application documentation,
orappendix B, how to enable the report generation.

As an aside, comments that start with “/**” must still be closed with “*/”. Single
line documentation comments (“///”) close at the end of the line.

The report file is an XML file that can subsequently be transformed to HTML doc-
umentation via an XSL/XSLT stylesheet,or be run through other tools to create
printed documentation. The syntax of the report file is compatible with that of the
“.Net” developer products —except that the PAWNcompiler stores more information
in the report than just the extracted documentation strings.

The example below illustrates documentation comments in a simple script that has
a few functions. You may write documentation comments for a function above its

```
 The report file contains a reference to the “PAWNDOC.XSL” stylesheet.
```

44 — Documentation comments

declaration or in its body. All documentation comments that appear before the
end of the function are attributed to the function. You can also add documentation
comments to global variables and global constants —these comments must appear
above the declaration of the variable or constant. Thefigure 1shows part of the
output for this (rather long) example. The style of the output is adjustable in the
cascading style sheet (CSS-file) associated with the XSLT transformation file.

LISTING: weekday.p
/**
- This program illustrates Zeller's congruence algorithm to calculate
- the day of the week given a date.
*/

/**
- <summary>
- The main program: asks the user to input a date and prints on
- what day of the week that date falls.
- </summary>

*/
@start()
{
var day, month, year
if (readdate(day, month, year))
{
var wkday = weekday(day, month, year)
printf "The date %d-%d-%d falls on a ", day, month, year
switch (wkday)
{
case 0:
print "Saturday"
case 1:
print "Sunday"
case 2:
print "Monday"
case 3:
print "Tuesday"
case 4:
print "Wednesday"
case 5:
print "Thursday"
case 6:
print "Friday"
}
}
else
print "Invalid date"
print "\n"
}

/**
- <summary>
- The core function of Zeller's congruence algorithm. The function
- works for the Gregorian calender.
- </summary>
-
- <param name="day">
- The day in the month, a value between 1 and 31.

```
Documentation comments — 45
```

* </param>
- <param name="month">
- The month: a value between 1 and 12.
- </param>
- <param name="year">
- The year in four digits.
- </param>
-
- <returns>
- The day of the week, where 0 is Saturday and 6 is Friday.
- </returns>
-
- <remarks>
- This function does not check the validity of the date; when the
- date in the parameters is invalid, the returned "day of the week"
- will hold an incorrect value.
- <p/>
- This equation fails in many programming languages, notably most
- implementations of C, C++ and Pascal, because these languages have
- a loosely defined "remainder" operator. Pawn, on the other hand,
- provides the true modulus operator, as defined in mathematical
- theory and as was intended by Zeller.
- </remarks>

*/
weekday(day, month, year)
{
/_*
- <remarks>
- For Zeller's congruence algorithm, the months January and
- February are the 13th and 14th month of the <em>preceding</em>
- year. The idea is that the "difficult month" February (which
- has either 28 or 29 days) is moved to the end of the year.
- </remarks>

*/
if (month <= 2)
month += 12, --year
var j = year % 100
var e = year / 100
return (day + (month+1)*26/10 + j + j/4 + e/4 - 2*e) % 7
}

/**
- <summary>
- Reads a date and stores it in three separate fields.
- </summary>
-
- <param name="day">
- Will hold the day number upon return.
- </param>
- <param name="month">
- Will hold the month number upon return.
- </param>
- <param name="year">
- Will hold the year number upon return.
- </param>
-
- <returns>
- <em>true</em> if the date is valid, <em>false</em> otherwise;

46 — Documentation comments

- if the function returns <em>false</em>, the values of
- <paramref name="day"/>, <paramref name="month"/> and
- <paramref name="year"/> cannot be relied upon.
- </returns>

*/
bool: readdate(&day, &month, &year)
{
print "Give a date (dd-mm-yyyy): "
day = getvalue(_,'-','/')
month = getvalue(_,'-','/')
year = getvalue()
return 1 <= month <= 12 && 1 <= day <= daysinmonth(month,year)
}

/**
- <summary>
- Returns whether a year is a leap year.
- </summary>
-
- <param name="year">
- The year in 4 digits.
- </param>
-
- <remarks>
- A year is a leap year:
- <ul>
- <li> if it is divisable by 4, </li>
- <li> but <strong>not</strong> if it is divisable by 100, </li>
- <li> but it <strong>is</strong> it is divisable by 400. </li>
- </ul>
- </remarks>

*/
bool: isleapyear(year)
return year % 400 == 0 || year % 100 != 0 && year % 4 == 0

/**
- <summary>
- Returns the number of days in a month (the month is an integer
- in the range 1 .. 12). One needs to pass in the year as well,
- because the function takes leap years into account.
- </summary>
-
- <param name="month">
- The month number, a value between 1 and 12.
- </param>
- <param name="year">
- The year in 4 digits.
- </param>

*/
daysinmonth(month, year)
{
static daylist[] = [ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 ]
assert 1 <= month <= 12
return daylist[month-1] + _:(month == 2 && isleapyear(year))
}

The format of the XML file created by “.Net” developer products is documented in
the Microsoft documentation. The PAWNparser creates a minimal description of

```
Documentation comments — 47
```

FIGURE1:Documentation generated from the source code

each function or global variable or constant that isusedin a project, regardless of
whether you used documentation comments on that function, variable or constant.
The parser also generates few tags of its own:

attribute Attributes for a function, such as “native” or “stock”.

automaton The automaton that the function belongs to (if any).

dependency The names of the symbols (functions, global variables and/global con-
stants) that the function depends on (meaning that it uses or requires
them). If desired, a call tree can be constructed from the dependen-
cies.

param Function parameters. When you add a parameter description in a
documentation comment, this description is combined with the auto-
generated content for the parameter.

paraminfo Tags and array or reference information on a parameter.

referrer All functions that refer to this symbol; i.e., all functions that use or
call this variable/function. This information is sufficient to serve as

48 — Documentation comments

```
a “cross-reference” —the “referrer” tree is the inverse of the “depen-
dency” tree.
```

stacksize The estimated number of cells that the function will allocate on the
stack and heap. This stack usage estimateexcludesthe stack require-
ments of any functions that are “called” from the function to which
the documentation applies. For example, functionreaddateis doc-
umented as taking 6 cells on the stack, but it also callsdaysinmonth
which takes 4 additional cells —and in turn callsisleapyear. To cal-
culate the total stack requirements for functionreaddate, the call
tree should be considered.
In addition to the local variables and function parameters, the com-
piler also uses the stack for storing intermediate results in complex
expressions. The stack space needed for these intermediate results
are alsoexcludedfrom this report. In general, the overhead for the
intermediate results is not cumulative (over all functions), which is
why it would be inaccurate to add a “safety margin” to every func-
tion. For the program as a whole, a safety margin would be highly
advised. Seeappendix B(page 145) for the-voption which can tell
you the maximum estimate stack usage, based on the call tree.

tagname The tag of the constant, variable, function result or function parame-
ter(s).

transition The transitions that the function provokes and their conditions —see
the sectionState programmingonpage 31.

All text in the documentation comment(s) is also copied to each function, variable
or constant to which it is attached. The text in the documentation comment is
copied without further processing —with one exception, see below. As the rest
of the report file is in XML format, and the most suitable way to process XML to
on-line documentation is through an XSLT processor (such as a modern browser),
you may choose to do any formatting in the documentation comments using HTML
tags. Note that you will often need to explicitly close any HTML tags; the HTML
standard does not require this, but XML/XSLT processors usually do. The PAWN
toolkit comes with an example XSLT file (with a matching style sheet) which sup-
ports the following XML/HTML tags:
<code> </code>
Formatted source code in a monospaced font; although the “&”, “<” and
“>” must be typed as “&amp;”, “&lt;” and “&rt;” respectively.
<example> </example>
Text set under the topic “Example”.
<param name="..."> </param>
A parameter description, with the parameter name appearing inside the
opening tag (the “name=” option) and the parameter description following
it.

```
Documentation comments — 49
```

<paramref name="..." />
A reference to a parameter, with the parameter name appearing inside the
opening tag (the “name=” option).

<remarks> </remarks>
Text set under the topic “Remarks”.

<returns> </returns>
Text set under the topic “Returns”.

<seealso> </seealso>
Text set under the topic “See also”.

<summary> </summary>
Text set immediately below the header of the symbol.

<section> </section>
Sets the text in a header. This should only be used in documentation that
is not attached to a function or a variable.

<subsection> </subsection>
Sets the text in a sub-header. This should only be used in documentation
that is not attached to a function or a variable.

The following additional HTML tags are supported for general purpose formatting
text inside any of the above sections:

<c> </c>
Text set in a monospaced font.

<em> </em>
Text set emphasized, usually in italics.

<p> </p>
Text set in a new paragraph. Instead of wrapping<p>and</p>around
every paragraph, inserting<p/>as a separator between two paragraphs
produces the same effect.

<para> </para>
An alternative for<p> </p>

<ul> </ul>
An unordered (bulleted) list.

<ol> </ol>
An ordered (numbered) list.

<li> </li>
An item in an ordered or unordered list.

As stated, there is one exception in the processing of documentation comments: if
the documentation comment contains a<param ...>tag, the PAWNparser looks up
the parameter and combines your description of the parameter with the contents
that it has automatically generated.

50 — Warnings and errors

Warnings and errors

The big hurdle that I have stepped over is how to actually compile the code snip-
pets presented in this chapter. The reason is that the procedure depends on the
system that you are using: in some applications there is a “Make” or “Compile script”
command button or menu option, while in other environments you have to type a
command like “pawncc myscript” on a command prompt. If you are using the
standard PAWNtoolset, you will find instructions of how to use the compiler and
run-time in the companion booklet “The PAWNbooklet — Implementerʼs Guide”, as
well as inappendix Bof this book. If you are using Microsoft Windows, it may be
convenient to use the Quincy IDE that comes with PAWNfor writing, running and
debugging scripts.

Regardless of the differences in launching the compile, the phenomenon that re-
sults from launching the compile are likely to be very similar between all systems:

⋄either the compile succeeds and produces an executable program —that may or
may not run automatically after the compile;

⋄or the compile gives a list of warning and error messages.

Mistakes happen and the PAWNparser tries to catch as many of them as it can. When
you inspect the code that the PAWNparser complains about, it may on occasion
be rather difficult for you to see why the code is erroneous (or suspicious). The
following hints may help:

⋄Each error or warning number is numbered. You can look up the error message
with this number inappendix A, along with a brief description on what the mes-
sage really means.

⋄If the PAWNparser produces a list of errors, thefirsterror in this list is a true error,
but the diagnostic messages below it may not be errors at all.

```
After the PAWNparser sees an error, it tries to step over it and complete the compi-
lation. However, the stumbling on the error may have confused the PAWNparser
so that subsequent legitimate statements are misinterpreted and reported as er-
rors too.
```

```
When in doubt, fix the first error and recompile.
```

⋄The PAWNparser checks only the syntax (spelling/grammar), not the semantics
(i.e. the “meaning”) of the code. When it detects code that does not comply to the
syntactical rules, there may actually exist different ways in which the code can be
changed to be “correct”, in the syntactical sense of the word —even though many
of these “corrections” would lead to nonsensical code. The result is, though, that
the PAWNparser may have difficulty to precisely locate the error: it does not know
what you meant to write. Hence, the parser often outputs two line numbers and
the error is somewhere in the range (between the line numbers).

```
In closing — 51
```

⋄Remember that a program that has no syntactical errors (the PAWNparser accepts
it without error & warning messages) may still have semantical and logical errors
which the PAWNparser cannot catch. Theassertinstruction (page 96) is meant
to help you catch these “run-time” errors.

In closing

If you know the C programming language, you will have seen many concepts that
you are familiar with, and a few new ones. If you donʼt know C, the pace of this
introduction has probably been quite high. Whether you are new to C or experi-
enced in C, I encourage you to read the following pages carefully. If you know C
or a C-like language, by the way, you may want to consult the chapterPitfalls(page
115 ) first.

This booklet attempts to be both an informal introduction and a (more formal) lan-
guage specification at the same time, perhaps succeeding at neither. Since it is
also thestandardbook on PAWN,the focus of this booklet is on being accurate and
complete, rather than being easy to grasp.

The double nature of this booklet shows through in the order in which it presents
the subjects. The larger conceptual parts of the language, variables and functions,
are covered first. The operators, the statements and general syntax rules follow
later —not that they are less important, but they are easier to learn, to look up, or
to take for granted.

```
 It is no longer theonlybook on Pawn.
```

### 52

Data and declarations

```
PAWNis a typeless language. All data elements are of type “cell”, and a cell can hold
an integral number. The size of a cell (in bytes) is system dependent —usually, a
cell is 32-bits.
```

For backward com- The keywordvardeclares a new variable. For special declarations, the keywordvar
patibility, “new”
may be used as
well as “var”.

```
is replaced bystatic,publicorstock(see below). A simple variable declaration
creates a variable that occupies one “cell” of data memory. Unless it is explicitly
initialized, the value of the new variable is zero.
A variable declaration may occur:
⋄at any position where a statement would be valid —local variables;
⋄at any position where a function declaration (native function declarations) or a
function implementation would be valid —global variables;
```

“for” loop: 97 ⋄in the first expression of aforloop instruction —also local variables.

**Local declarations**
A local declaration appears inside a compound statement. A local vari-
Compound state-
ment: 96 able can only be accessed from within the compound statement, and from
nested compound statements. A declaration in the first expression of afor
loop instruction is also a local declaration.

```
Global declarations
A global declaration appears outside a function. A global variable is ac-
cessible to any function. Global data objects can only be initialized with
constant expressions.
```

```
State variable declarations
A state variable is a global variable with a state classifier appended at the end. The
scope and the lifespan of the variable are restricted to the states that are listed in
the classifier. Fall-back state specifiers are not permitted for state variables.
State variables may not be initialized. In contrast to normal variables (which are
zero after declaration —unless explicitly initialized), state variables hold an indeter-
minate value after declaration and after first entering a state in its classifier. Typ-
ically, one uses the stateentryfunction(s) to properly initialize the state variable,
and theexitfunction(s) to reset these variables.
```

```
Static local declarations
A local variable is destroyed when the execution leaves the compound block in
which the variable was created. Local variables in a function only exist during the
```

```
Public declarations — 53
```

run time of that function. Each new run of the function creates and initializes new
local variables. When a local variable is declared with the keywordstaticrather
thanvar, the variable remains in existence after the end of a function. This means
that static local variables provide private, permanent storage that is accessible only
from a single function (or compound block). Like global variables, static local vari-
ables can only be initialized with constant expressions.

Static global declarations

A static global variable behaves the same as a normal global variable, except that
its scope is restricted to the file that the declaration resides in. To declare a global
variable as static, replace the keywordvarbystatic.

Stock declarations

A global variable may be declared as “stock”. A stock declaration is one that the
parser may remove or ignore if the variable turns out not to be used in the program. Stock functions:^72

Stock variables are useful in combination with stock functions. A public variable
may be declared as “stock” as well —declaring public variables as “public stock”
enables you to declare al public variables that a host application provides in an
include file, with only those variables that the script actually uses winding up in
the P-code file.

Public declarations

Global “simple” variables (no arrays) may be declared “public” in two ways:

⋄declare the variable using the keywordpublicinstead ofvar;

⋄start the variable name with the “@” symbol.

Public variables behave like global variables, with the addition that the host pro-
gram can also read and write public variables. A (normal) global variable can only
be accessed by the functions in your script —the host program is unaware of them.
As such, a host program may require that you declare a variable with a specific
name as “public” for special purposes —such as the most recent error number, or
the general program state.

```
54 — Constant variables
```

Constant variables

Symbolic constants: It is sometimes convenient to be able to create a variable that is initialized once and
87 that may not be modified. Such a variable behaves much like a symbolic constant,
but it still is a variable.

```
To declare a constant variable, insert the keywordconstbetween the keyword that
starts the variable declaration —var,static,publicorstock— and the variable
name.
Examples:
var const address[4] = { 192, 0, 168, 66 }
public const status /* initialized to zero */
```

```
Three typical situations where one may use a constant variable are:
⋄To create an “array” constant; symbolic constants cannot be indexed.
⋄For a public variable that should be set by the host application, andonlyby the
host application. See the preceding section for public variables.
⋄A special case is to mark array arguments to functions asconst. Array argu-
ments are always passed by reference, declaring them asconstguards against
unintentional modification. Refer topage 61for an example ofconstfunction
arguments.
```

Arrays (single dimension)
The syntaxname[constant]declaresnameto be an array of “constant” elements,
See also “multi-
dimensional arrays”,
page 56 , and “sym-
bolic subscripts”,
page 55

```
where each element is a single cell. Thenameis a placeholder of an identifier name
of your choosing andconstantis a positive non-zero value;constantmay be ab-
sent. If there is no value between the brackets, the number of elements is set equal
to the number of initiallers —see the example below.
The array index range is “zero based” which means that the first element isname[0]
and the last element isname[constant-1].
The syntaxname{constant}also declaresnameas an array ofconstantelements,
but now the elements arecharactersrather than cells. The number of characters
that fit in a cell depends on the configuration of the PAWNparser.
```

```
Initialization
Data objects can be initialized at their declaration.The initialler of a global data
```

Constants: (^84) object must be a constant. Arrays, global or local, must also be initialized with
constants.
Uninitialized data defaults to zero.

```
Symbolic subscripts for arrays — 55
```

Examples:

LISTING: good declaration
var i = 1
var j /*j is zero _/
var k = 'a' /_ k has character code for letter 'a'*/

var a[] = [1,4,9,16,25] /*a has 5 elements _/
var s1[20] = ['a','b'] /_ the other 18 elements are 0*/

var s2[] = ''Hello world...'' /*an unpacked string*/

Examples of **invalid** declarations:

LISTING: bad declarations
var c[3] = 4 /*an array cannot be set to a value _/
var i = "Good-bye" /_ only an array can hold a string _/
var q[] /_ unknown size of array _/
var p[2] = { i + j, k - 3 } /_ array initiallers must be constants*/

Progressive initiallers for arrays

The ellipsis operator continues the progression of the initialisation constants for an
array, based on the last two initialized elements. The ellipsis operator (three dots,
or “...”) initializes the array up to its declared size.

Examples:

LISTING: array initializers
var a[10] = { 1, ... } // sets all ten elements to 1
var b[10] = { 1, 2, ... } // b = 1, 2, 3, 4, 5, 6, 7, 8, 9, 10
var c[8] = { 1, 2, 40, 50, ... } // c = 1, 2, 40, 50, 60, 70, 80, 90
var d[10] = { 10, 9, ... } // d = 10, 9, 8, 7, 6, 5, 4, 3, 2, 1

Symbolic subscripts for arrays

An array may be declared with a list of symbols instead of a value for its size: an
example of this is the “priority queue” sample program onpage 17. An individual
subscript may also be interpreted as asub-arrays, for example, see theRPNcalcula-
tor program atpage 24.

The sub-array syntax applies as well to the initialization of an array with symbolic
Use a #define for
convenient declara-
tion: 17

subscripts. Referring again to the “priority queue” sample program, to initialize a
“message” array with fixed values, the syntax is:

LISTING: array initializers
var msg[.text{40}, .priority] = { "new message", 1 }

```
56 — Multi-dimensional arrays
```

```
The initialler consists of a string (a literal array) and a value; these go into the fields
“.text” and “.priority” respectively.
An array dimension that is declared as a list of symbolic subscripts, may only be
indexed with these subscripts. From the above declaration of variable “msg”, we
may use:
LISTING: array initializers
msg[.text] = "another message"
msg[.priority] = 10 - msg[.priority]
```

```
It is an error, however, to use a (numeric) expression to index “msg”. For example,
“msg[1]” is an invalid expression.
Since an array with symbolic subscripts may not be indexed with an expression,
the square brackets that enclose the expression become optional. These brackets
may be omitted. The snippet below is equivalent to the previous snippet.
LISTING: array initializers
msg.text = "another message"
msg.priority = 10 - msg.priority
```

```
A subscript may have an explicit tag name as well. This tag will then override the
```

Tag names: (^58) default tag for array elements. TheRPNcalculator program makes use of this fea-
ture to mark one of the subscripts as a rational value. In the declaration in the
snippet below, the expression “field.type” is a plain integer (without tag), but
the expression “field.value” has tagRational:.
LISTING: array initializers
var field[ .type, /*operator or token type _/
Rational: .value, /_ value, if t_type is "Number" _/
.word{20} /_ raw string*/
]
Multi-dimensional arrays
Multi-dimensional arrays are arrays that contain references to the sub-arrays. That
is, a two-dimensional array is an “array of single-dimensional arrays”.Below are
a few examples of declarations of two-dimensional arrays.
LISTING: two-dimensional arrays
var a[4][3]
var b[3][2] = [ [ 1, 2 ], [ 3, 4 ], [ 5, 6 ] ]
var c[3][3] = [ [ 1 ], [ 2, ...], [ 3, 4, ... ] ]
var d[2]{10} = [ "agreement", "dispute" ]
var e[2][] = [ ''OK'', ''Cancel'' ]
var f[][] = [ ''OK'', ''Cancel'' ]
 The current implementation of the Pawn compiler supports only arrays with up to three dimen-
sions.

```
Arrays and the sizeof operator — 57
```

As the last two declarations (variable “e” en “f”) show, the final dimension of an
array may have an unspecified length, in which case the length of each sub-array is
determined from the related initializer. Every sub-array may have a different size;
in this particular example, “e[1][5]” contains the letter “l” from the word “Cancel”,
but “e[0][5]” isinvalidbecause the length of the sub-array “e[0]” is only three
cells (containing the letters “O”, “K” and a zero terminator).

The difference between the declarations for arrays “e” and “f” is that we let the
compiler count the number of initializers for the major dimension —“sizeof f” is
2, like “sizeof e” (see the next section on thesizeofoperator).

Arrays and the sizeof operator

Thesizeofoperator returns the size of a variable in “elements”. For a simple (non-
compound) variable, the result of sizeof is always 1, because an element is a cell for
a simple variable.

An array with one dimension holds a number of cells and thesizeofoperator re-
turns that number. The snippet below would therefore print “5” at the display,
because the array “msg” holds four characters (each character in a separate cell),
plus a zero-terminator:

LISTING: sizeof operator
var msg[] = ''Help''
printf(''%d'', sizeof msg);

Thesizeofoperator always returns the number of cells, even for a packed array.
That is, in the next snippet, the value printed would be less than “5” —although
there are five characters in the array, those are packed in fewer cells.

LISTING: sizeof operator
var msg{} = "Help"
printf(''%d'', sizeof msg);

With multi-dimensional arrays, thesizeofoperator can return the number of ele-
ments in each dimension. For the last (minor) dimension, an element will again be
a cell, but for the major dimension(s), an element is a sub-array. In the following
code snippet, observe that the syntaxsizeof matrixrefers to the major dimen-
sion of the two-dimensional array and the syntaxsizeof matrix[]refers to the
minor dimension of the array. The values that this snippet prints are 3 and 2 (for
the major and minor dimensions respectively):

LISTING: sizeof operator and multidimensional arrays
var matrix[3][2] = { { 1, 2 }, { 3, 4 }, { 5, 6 } }
printf(''%d %d'', sizeof matrix, sizeof matrix[]);

The application of thesizeofoperator on multi-dimensional arrays is especially

Default function ar-
guments and sizeof:
convenient when used as a default value for function arguments.^64

```
58 — Tag names
```

Tag names

```
A tag is a label that denotes the objective of —or the meaning of— a variable, a con-
stant or a function result. Tags are optional, their only purpose is to allow a stronger
compile-time error checking of operands in expressions, of function arguments
and of array indices.
```

```
A tag consists of a symbol name followed by a colon; it has the same syntax as a
```

Label syntax: (^96) label. A tag precedes the symbol name of a variable, constant or function. In an
assignment, only the right hand of the “=” sign may be tagged.
Examples of valid tagged variable and constant definitions are:
LISTING: tag names
var bool:flag = true /*"flag" can only hold "true" or "false"*/
const error:success = 0
const error:fatal= 1
const error:nonfatal = 2
error:errno = fatal
The sequence of the constantssuccess,fatalandnonfatalcould more conve-
“const” statement:
87 niently be declared by grouping the constants in a compount block, as illustrated
below. The declaration below creates the same three constants, all with the tag
error:. It is required to specify a value for the first constant of the list, the subse-
quent constants are automatically assigned a value that is the value of the previous
constant+1—unless an explicit value is present.
LISTING: enumerated constants
const error: {
notice = 0,
warning,
nonfatal,
fatal,
}
var error: code
After declaring variable “code” with tag name “error:”, you can assign any of the
constants with that same tag name to it; however, writing “code = 2” will give a
parser diagnostic (a warning or error message). A tag override (or a tagcast) adjusts
an expression to the desired tag name. As a somewhat contrived example, the next
snippet elevates “code” to a higher level (a “more serious error”) —note how the
literal value 1 is forced to the tag name “error:”
LISTING: tag override
if (code < fatal)
code = code + error:1

```
Tag names — 59
```

Tag names introduced so far started with a lower case letter; these are “weak” tags.
Tag names that start with an upper case letter are “strong” tags. The difference
between weak and strong tags is that weak tags may, in a few circumstances, be
dropped implicitly by the PAWNparser —so that a weakly tagged expression be-
comes an untagged expression. The tag checking mechanism verifies the following
situations:
⋄When the expressions on both sides of a binary operator have a different tag, or
when one of the expressions is tagged and the other is not, the compiler issues a
“tag mismatch” diagnostic. There is no difference between weak and strong tags
in this situation.
⋄There is a special case for the assignment operator: the compiler issues a diag-
“lvalue”: the vari-
able on the left side
in an assignment,
seepage 89

nostic if the variable on the left side of an assignment operator has a tag, and the
expression on the right side either has a different tag or is untagged. However, if
the variable on the left of the assignment operator is untagged, it accepts an ex-
pression (on the right side) with aweaktag. In other words, a weak tag is dropped
in an assignment when thelvalueis untagged.
⋄Passing arguments to a function follows the rule for assignments. The compiler
issues a diagnostic when theformalparameter (in a function definition) has a tag
and theactualparameter (in the function call) either is untagged or has a different
tag. However, if the formal parameter is untagged, it also accepts a parameter
with anyweaktag.

### 60

Functions

```
A function declaration specifies the name of the function and, between parenthe-
ses, its formal parameters. A function may also return a value. A function declara-
tion must appear on a global level (i.e. outside any other functions) and is globally
accessible.
```

The preferred way If a semicolon follows the function declaration (rather than a statement), the dec-
to declare forward
functions is atpage
70

```
laration denotes a forward declaration of the function.
Thereturnstatement sets the function result. For example, functionsum(see be-
low) has as its result the value of both its arguments added together. Thereturn
expression is optional for a function, but one cannot use the value of a function that
does not return a value.
LISTING: sum function
sum(a, b)
return a + b
```

```
Arguments of a function are (implicitly declared) local variables for that function.
Thefunction calldetermines the values of the arguments.
Another example of a complete definition of a function is below: functionleapyear
returnstruefor a leap year andfalsefor a non-leap year.
LISTING: leapyear function
leapyear(y)
return y % 4 == 0 && y % 100 != 0 || y % 400 == 0
```

The logical and arithmetic operators used in theleapyearexample are covered on
pages 92 and 89 respectively.
Usually a function contains local variable declarations and consists of a compound
“assert” statement:
96 statement. In the following example, note theassertstatement to guard against
negative values for the exponent.
LISTING: power function (raise to a power)
power(x, y)
{
/*returns x raised to the power of y*/
assert y >= 0
var r = 1
for (var i = 0; i < y; i++)
r *= x
return r
}

```
A function may contain multiplereturnstatements —when areturnappears in
the middle of a function, the rest of the function is skipped. If a function returns
an array, allreturnstatements must specify an array with the same size and the
same dimensions.
```

```
Function arguments — 61
```

Function arguments

The “faculty” function in the next program has one parameter which it uses in
Another example is
function JulianTo-
Date atpage 10

a loop to calculate the faculty of that number. What deserves attention is that the
function modifies its argument.

LISTING: faculty.p
/*Calculation of the faculty of a value*/
@start()
{
print "Enter a value: "
var v = getvalue()
var f = faculty(v)
printf "The faculty of %d is %d\n", v, f
}

faculty(n)
{
assert n >= 0
var result = 1
while (n > 0)
result *= n--
return result
}

Whatever (positive) value that “n” had at the entry of thewhileloop in function
faculty, “n” will be zero at the end of the loop. In the case of thefacultyfunction,
the parameter is passed “by value”, so the change of “n” is local to thefaculty
function. In other words, function@startpasses “v” as input to functionfaculty,
but upon return offaculty, “v” still has the same value as before the function call.

call-by-value versus call-by-reference

Arguments that occupy a single cell can be passed by value or by reference. The
default is “pass by value”. To create a function argument that is passed by reference,
prefix the argument name with the character&.

Example:

LISTING: swap function
swap(&a, &b)
{
var temp = b
b = a
a = temp
}

To pass an array to a function, append a pair of brackets to the argument name.
You may optionally indicate the size of the array; doing so improves error checking
of the parser.

Example:

```
62 — Function arguments
```

```
LISTING: addvector function
addvector(a[], const b[], size)
{
for (var i = 0; i < size; i++)
a[i] += b[i]
}
```

Arrays are always passed by reference. As a side note, arraybin the above exam-
Constant variables:
54 ple does not change in the body of the function. The function argument has been
declared asconstto make this explicit. In addition to improving error checking, it
also allows the PAWNparser to generate more efficient code.

```
To pass an array of literals to a function, use the same syntax as for array initiallers:
a literal string or the series of array indices enclosed in braces (seepage 85; the
ellipsis for progressive initiallers cannot be used). Literal arrays can only have a
single dimension.
```

```
The following snippet callsaddvectorto add five to every element of the array
“vect”:
```

```
LISTING: addvector usage
var vect[3] = [ 1, 2, 3 ]
addvector(vect, [5, 5, 5], 3)
/* vect[] now holds the values 6, 7 and 8 */
```

The call to functionprintfwith the string"Hello world\n"in the first ubiquitous
“Hello world” pro-
gram: 3 program is another example of passing a literal array to a function.

Named parameters versus positional parameters

```
In the previous examples, the order of parameters of a function call was impor-
tant, because each parameter is copied to the function argument with the same
sequential position. For example, with the functionweekday(which uses Zellerʼs
congruence algorithm) defined as below, you would callweekday(12,31,1999)to
get the week day of the last day of the preceding century.
```

```
LISTING: weekday function
weekday(month, day, year)
{
/* returns the day of the week: 0=Saturday, 1=Sunday, etc. */
if (month <= 2)
month += 12, --year
var j = year % 100
var e = year / 100
return (day + (month+1)*26/10 + j + j/4 + e/4 - 2*e) % 7
}
```

```
Function arguments — 63
```

Date formats vary according to culture and nation. While the formatmonth/day/
yearis common in the United States of America, European countries often use the
day/month/yearformat, and technical publications sometimes standardize on the
year/month/dayformat (ISO/IEC 8824). In other words, no order of arguments in the
weekdayfunction is “logical” or “conventional”. That being the case, the alternative
way to pass parameters to a function is to use “named parameters”, as in the next
examples (the three function calls are equivalent):

LISTING: weekday usage —positional parameters
var wkday1 = weekday( .month = 12, .day = 31, .year = 1999)
var wkday2 = weekday( .day = 31, .month = 12, .year = 1999)
var wkday3 = weekday( .year = 1999, .month = 12, .day = 31)

With named parameters, a period (“.”) precedes the name of the function argu-
ment. The function argument can be set to any expression that is valid for the ar-
gument. The equal sign (“=”) does in the case of a named parameter not indicate
an assignment; rather it links the expression that follows the equal sign to one of
the function arguments.

One may mix positional parameters and named parameters in a function call with
the restriction that all positional parameters must precede any named parameters.

Default values of function arguments

A function argument may have a default value. The default value for a function
Public functions do
not support default
argument values;
seepage 71

argument must be a constant. To specify a default value, append the equal sign
(“=”) and the value to the argument name.

When the function call specifies an argument placeholder instead of a valid argu-
ment, the default value applies. The argument placeholder is the underscore char-
acter (“_”). The argument placeholder is only valid for function arguments that
have a default value.

The rightmost argument placeholders may simply be stripped from the function
argument list. For example, if functionincrementis defined as:

LISTING: increment function —default values
increment(&value, incr=1) value += incr

the following function calls are all equivalent:

LISTING: increment usage
increment(a)
increment(a, _)
increment(a, 1)

Default argument values for passed-by-reference arguments are useful to make the
input argument optional. For example, if the functiondivmodis designed to return
both the quotient and the remainder of a division operation through its arguments,
default values make these arguments optional:

```
64 — Function arguments
```

```
LISTING: divmod function —default values for reference parameters
divmod(a, b, &quotient=0, &remainder=0)
{
quotient = a / b
remainder = a % b
}
```

```
With the preceding definition of functiondivmod, the following function calls are
now all valid:
LISTING: divmod usage
var p, q
divmod(10, 3, p, q)
divmod(10, 3, p, _)
divmod(10, 3, _, q)
divmod(10, 3, p)
divmod 10, 3, p, q
```

```
Default arguments for array arguments are often convenient to set a default string
or prompt to a function that receives a string argument. For example:
LISTING: printerror function
print_error(const message[], const title[] = "Error: ")
{
print title
print message
print "\n"
}
```

```
The next example adds the fields of one array to another array, and by default in-
crements the first three elements of the destination array by one:
LISTING: addvector function, revised
addvector(a[], const b[] = {1, 1, 1}, size = 3)
{
for (var i = 0; i < size; i++)
a[i] += b[i]
}
```

```
sizeof operator and default function arguments
A default value of a function argument must be a constant, and its value is deter-
```

“sizeof” operator (^94) mined at the point of the functionʼsdeclaration. Using the “sizeof” operator to
set the default value of a function argument is a special case: the calculation of the
value of thesizeofexpression is delayed to the point of the functioncalland it takes
the size of theactualargument rather than that of theformalargument. When the
function is used several times in a program, with different arguments, the outcome
of the “sizeof” expression is potentially different at every call —which means that
the “default value” of the function argument may change.
Below is an example program that draws ten random numbers in the range of 0–51
without duplicates. An example for an application for drawing random numbers

```
Function arguments — 65
```

without duplicates is in card games —those ten numbers could represent the cards
for two “hands” in a poker game. The virtues of the algorithm used in this program,
invented by Robert W. Floyd, are that it is efficient and unbiased —provided that the
pseudo-random number generator is unbiased as well.

```
“random” is a pro-
posed core func-
tion, see page 106
```

LISTING: randlist.p
@start()
{
var HandOfCards[10]
FillRandom(HandOfCards, 52)
print "A draw of 10 numbers from a range of 0 to 51 " ...
"(inclusive) without duplicates:\n"
for (var i = 0; i < sizeof HandOfCards; i++)
printf "%d ", HandOfCards[i]
}

FillRandom(Series[], Range, Number = sizeof Series)
{
assert Range >= Number /* cannot select 50 values
- without duplicates in the
- range 0..40, for example */
var Index = 0
for (var Seq = Range - Number; Seq < Range; Seq++)
{
var Val = random(Seq + 1)
var Pos = InSeries(Series, Val, Index)
if (Pos >= 0)
{
Series[Index] = Series[Pos]
Series[Pos] = Seq
}
else
Series[Index] = Val
Index++
}
}

InSeries(Series[], Value, Top = sizeof Series)
{
for (var i = 0; i < Top; i++)
if (Series[i] == Value)
return i
return -1
}

Function@startdeclares the arrayHandOfCardswith a size of ten cells and then
Array declarations:
calls functionFillRandomwith the purpose that it draws ten positive random num- 54
bers below 52. Observe, however, that the only two parameters that@startpasses
into the call toFillRandomare the arrayHandOfCards, where the random numbers
should be stored, and the upper bound “52”. The number of random numbers to
draw (“10”) is passedimplicitlytoFillRandom.

The definition of functionFillRandombelow@startspecifies for its third param-
eter “Number = sizeof Series”, where “Series” refers to the first parameter of
the function. Due to the special case of a “sizeofdefault value”, the default value

```
66 — Function arguments
```

```
of theNumberargument is not the size of the formal argumentSeries, but that of
the actual argument at the point of the function call:HandOfCards.
Note that inside functionFillRandom, asking the “sizeof” the function argument
Serieswould (still) evaluate in zero, because theSeriesarray is declared with
unspecified length (seepage 94for the behaviour ofsizeof). Usingsizeofas a
default value for a function argument is a specific case. If the formal parameter
Serieswere declared with an explicit size, as inSeries[10], it would be redundant
to add aNumberargument with the array size of the actual argument, because the
parser would then enforce that both formal and actual arguments have the size and
dimensions.
```

```
Arguments with tag names
A tag optionally precedes a function argument. Using tags improves the compile-
```

Tag names: (^58) time error checking of the script and it serves as “implicit documentation” of the
function. For example, a function that computes the square root of an input value
in fixed point precision may require that the input parameter is a fixed point value
and that the result is fixed point as well. The function below uses the fixed point ex-
tension module,and an approximation algorithm known as “bisection” to calculate
Fixed point arith-
metic: 77 ;
see also the appli-
cation note “Fixed
Point Support Li-
brary”
the square root. Note the use of tag overrides on numeric literals and expression
results.
LISTING: sqroot function —strong tags
Fixed: sqroot(Fixed: value)
{
var Fixed: low = 0.0
var Fixed: high = value
while (high - low > Fixed: 1)
{
var Fixed: mid = (low + high) >> 1
if (fmul(mid, mid) < value)
low = mid
else
high = mid
}
return low
}
With the above definition, the PAWNparser issues a diagnostic if one calls thesq-
rootfunction with a parameter with a tag different from “Fixed:”, or when it tries
to store the function result in a variable with a “non-Fixed:” tag.
The bisection algorithm is related to binary search, in the sense that it continu-
ously halves the interval in which the result must lie. A “successive substitution”
algorithm like Newton-Raphson, that takes the slope of the functionʼs curve into
account, achieves precise results more quickly, but at the cost that a stopping cri-
terion is more difficult to state. State of the art algorithms for computing square
roots combine bisection and Newton-Raphson algorithms.

```
Coercion rules — 67
```

Variable arguments

A function that takes a variable number of arguments, uses the “ellipsis” opera-
tor (“...”) in the function header to denote the position of the first variable argu-
ment. The function can access the arguments with the predefined functionsnu-
margs,getargandsetarg(seepage 106).

Functionsumreturns the summation of all of its parameters. It uses a variable
length parameter list.

LISTING: sum function, revised
sum(...)
{
var result = 0
for (var i = 0; i < numargs(); ++i)
result += getarg(i)
return result
}

This function could be used in:

LISTING: sum function usage
var v = sum(1, 2, 3, 4, 5)

A tag may precede the ellipsis to enforce that all subsequent parameters have the
same tag, but otherwise there is no error checking with a variable argument list Tag names:^58
and this feature should therefore be used with caution.

The functionsgetargandsetargassume that the argument is passed “by refer-
ence”. When usinggetargon normal function parameters (instead of variable ar-
guments) one should be cautious of this, as neither the compiler nor the abstract
machine can check this. Actual parameters that are passed as part of a “variable
argument list” are always passed by reference.

Coercion rules

If the function argument, as per the function definition (or its declaration), is a
“value parameter”, the caller can pass as a parameter to the function:
⋄a value, which is passed by value;
⋄a reference, whose dereferenced value is passed;
⋄an (indexed) array element, which is a value.

If the function argument is a reference, the caller can pass to the function:
⋄a value, whose address is passed;
⋄a reference, which is passed by value because it has the type that the function
expects;
⋄an (indexed) array element, which is a value.

If the function argument is an array, the caller can pass to the function:

```
68 — Calling functions
```

```
⋄an array with the same dimensions, whose starting address is passed;
⋄an (indexed) array element, in which case the address of the element is passed.
```

```
Calling functions
When inserting a function name with its parameters in a statement or expression,
the function will get executed in that statement/expression. The statement that
refers to the function is the “caller” and the function itself, at that point, is the
“callee”: the one being called.
The standard syntax for calling a function is to write the functionʼs name, followed
by a list with all explicitly passed parameters between parentheses. If no parame-
ters are passed, or if the function does not have any, the pair of parentheses behind
the function name are still present.For example, to try out thepowerfunction, the
```

Function power: (^60) following program calls it thus:
LISTING: example program for the power function
@start()
{
print "Please give the base value and the power to raise it to:"
var base = getvalue()
var power = getvalue()
var result = power(base, power)
printf "%d raised to the power %d is %d", base, power, result
}
A function may optionally return a value. Thesum,leapyearandpowerfunctions
Functions sum &
leapyear: 60
Function swap: 61
all return a value, but theswapfunction does not. Even if a function returns a value,
the caller may ignore it.
For the situation that the caller ignores the functionʼs return value, there is an alter-
native syntax to call the function, which is also illustrated by the preceding example
program calls thepowerfunction. The parentheses around all function arguments
are optional if the caller does not use the return value. In the last statement, the
example program reads
printf "%d raised to the power %d is %d", base, power, result
rather than
printf("%d raised to the power %d is %d", base, power, result)
which does the same thing.
The syntax without parentheses around the parameter list is the “procedure call”
syntax. You can use it only if:
⋄the caller does not assign the functionʼs result to a variable and does not use it in
an expression, or as the “test expression” of anifstatement for example;
⋄the first parameter does not start with an opening parenthesis;
⋄the first parameter is on the same line as the function name, unless you use
named parameters (see the next section).

```
Recursion — 69
```

As you may observe, the procedure call syntax applies to cases where a function call
behaves rather as a statement, like in the calls toprintandprintfin the preceding
example. The syntax is aimed at making such statements appear less cryptic and
friendlier to read, but not that the use of the syntax is optional.

As a side note, all parentheses in the example program presented in this section are
required: the return values of the calls togetvalueare stored in two variables, and
therefore an empty pair of parentheses must follow the function name. Function
getvaluehas optional parameters, but none are passed in this example program.

Recursion

Afacultyexample function earlier in this chapter used a simple loop. An example
“faculty”: 61
function that calculated a number from the Fibonacci series also used a loop and “fibonacci”: 8
an extra variable to do the trick. These two functions are the most popular routines
to illustrate recursive functions, so by implementing these as iterative procedures,
you might be inclined to think that PAWNdoes not support recursion.

Well, PAWNdoessupport recursion, but the calculation of faculties and of Fibonacci
numbers happen to be good examples of whennotto use recursion. Faculty is eas-
ier to understand with a loop than it is with recursion. Solving Fibonacci numbers
by recursion indeed simplifies the problem, but at the cost of being extremely in-
efficient: the recursive Fibonacci calculates the same values over and over again.

The program below is an implementation of the famous “Towers of Hanoi” game
There exists an in-
triguing iterative
solution to the Tow-
ers of Hanoi.

in a recursive function:

LISTING: hanoi.p
/*The Towers of Hanoi, a game solved through recursion*/

@start()
{
print "How many disks: "
var disks = getvalue()
move 1, 3, 2, disks
}

move(from, to, spare, numdisks)
{
if (numdisks > 1)
move from, spare, to, numdisks-1
printf "Move disk from pillar %d to pillar %d\n", from, to
if (numdisks > 1)
move spare, to, from, numdisks-1
}

```
70 — Forward declarations
```

Forward declarations

For standard functions, the current “reference implementation” of the PAWNcom-
piler does not require functions to be declared before their first use.User-defined
operators are special functions, and unlike standard functions theymustbe de-
clared before use. In many cases it is convenient to put the implementation of a
user-defined operator in an include file, to make sure that its declaration precedes
its first use.Sometimes, it may however be required (or convenient) to declare a
Forbidden user-
defined operators:
78

```
user- defined operator first and implement it elsewhere. A particular use of this
technique is to implement “forbidden” user-defined operators.
```

```
To create a forward declaration, precede the function name and its parameter list
with the keywordforward. For compatibility with early versions of PAWN, and for
similarity with C/C++, an alternative way to forwardly declare a function is by typ-
ing the function header and terminating it with a semicolon (which follows the clos-
ing parenthesis of the parameter list).
```

```
The full definition of the function, with a non-empty body, is implemented else-
where in the source file (except for forbidden user-defined operators).
```

```
State classifiers are ignored on forward declarations.
```

State classifiers

```
All functions except native functions may optionally have a state attribute. This
```

Example: (^33) consists of a list of state (and automata) names between angle brackets behind the
function header. The names are separated by commas. When the state is part of
a non-default automaton, the name of the automaton and a colon separator must
precede the state; for example, “parser:slash” stands for the stateslashof the
automatonparser.
If a function has states, there must be several “implementations” of the function in
the source code. All functions must have the same function header (excluding the
state classifier list).
As a special syntax, when there arenonames between the angle brackets, the func-
tion is linked to all states that are not attributed to other implementations of the
function. The function that handles “all states not handled elsewhere” is the so-
calledfall-backfunction.
 Other implementations of the Pawn language (if they exist) may use “single pass” parsers, requir-
ing functions to be defined before use.

```
Public functions, function main — 71
```

Public functions, function main

A stand-alone program must have the function@start. This function is the starting
point of the program. The function@startmay not have arguments. For backward
compatibility,mainis an equivalent name for@start.

A function library need not to have a@startfunction, but it must have at least
one public function. Function@startis the primary entry point into the compiled
program; the public functions are alternative entry points to the program. The
virtual machine can start execution with one of the public functions. A function
library may have a@startfunction to perform one-time initialization at start-up.

To make a function public, prefix the function name with the keywordpublic. For
example, a text editor may call the public function “onkey” for every key that the
user typed in, so that the user can change (or reject) keystrokes. Theonkeyfunction
below would replace every “~” character (code 126 in the ISO Latin-1 character set)
by the “hard space” code in the ANSI character table:

LISTING: onkey function

public onkey(keycode)
{
if (key == '~')
return 160 // replace ~ by hard space (code 160 in Latin-1)
else
return key // leave other keys unaltered
}

Functions whose name starts with the “@” symbol are also public. So an alternative
way to write the public functiononkeyfunction is:

LISTING: @onkey function

@onkey(keycode)
return key=='~'? 160 : key

The “@” character, when used, becomes part of the function name; that is, in the
last example, the function is called “@onkey”. The host application decides on the
names of the public functions that a script may implement.

Arguments of a public function may not have default values. A public function
Default values of
function arguments:
63

interfaces the host application to the PAWNscript. Hence, the arguments passed
to the public function originate from the host application, and the host application
cannot know what “default values” the script writer plugged for function arguments
—which is why the PAWNparser flags the use of default values for arguments of pub-
lic functions as an error. The issue of default values in public function arguments
only pops up in the case that you wish to call public functions from the script itself.

```
72 — Static functions
```

Static functions

```
When the function name is prefixed with the keywordstatic, the scope of the
function is restricted to the file that the function resides in.
```

```
Thestaticattribute can be combined with the “stock” attribute.
```

Stock functions

```
A “stock” function is a function that the PAWNparser must “plug into” the program
when it is used, and that it may simply “remove” from the program (without warn-
ing) when it is not used. Stock functions allow a compiler or interpreter to optimize
the memory footprint and the file size of a (compiled) PAWNprogram: any stock
function that is not referred to, is completely skipped —as if it were lacking from
the source file.
```

```
A typical use of stock functions, hence, is in the creation of a set of “library” func-
tions. A collection of general purpose functions, all marked as “stock” may be put
in a separate include file, which is then included in any PAWNscript. Only the li-
brary functions that are actually used get “linked” in.
```

To declare a stock function, prefix the function name with the keywordstock. Pub-
Public variables can
be declared “stock” lic functions and native functions cannot be declared “stock”.

```
When a stock function calls other functions, it is usually a good practice to declare
those other functions as “stock” too —with the exception of native functions.Simi-
```

Stock variables: (^53) larly, any global variables that are used by a stock function should in most cases also
be defined “stock”. The removal of unused (stock) functions can cause a chain re-
action in which other functions and global variables are not longer accessed either.
Those functions are then removed as well, thereby continuing the chain reaction
until only the functions that are used, directly or indirectly, remain.
Native functions
A PAWNprogram can call application-specific functions through a “native function”.
The native function must be declared in the PAWNprogram by means of a function
prototype. The function name must be preceded by the keywordnative.
Examples:
native getparam(a[], b[], size)
native multiply_matrix(a[], b[], size)
native openfile(const name[])

```
User-defined operators — 73
```

The names “getparam”, “multiply_matrix” and “openfile” are theinternalnames
of the native functions; these are the names by which the functions are known in
the PAWNprogram. Optionally, you may also set anexternalname for the native
function, which is the name of the function as the “host application” knows it. To
do so, affix an equal sign to the function prototype followed by the external name.
For example:
native getparam(a[], b[], size) = host_getparam

native multiply_matrix(a[], b[], size) = mtx_mul

When a native function returns an array, the dimensions and size of the array must
be explicitly declared. The array specification occurs between the function name
and the parameter list. For example:

# define Rect [ .left, .top, .right, .bottom ]
native intersect[Rect](src1[Rect], src2[Rect])

Unless specified explicitly, the external name is equal to the internal name of a
An example of a
native user-defined
operator is onpage
76

native function. One typical use for explicit external names is to set a symbolic
name for a user-defined operator that is implemented as a native function.

See the “Implementerʼs Guide” for implementing native functions in C/C++(on the
“host application” side).

Native functions may not have state specifiers.

User-defined operators

The only data type of PAWNis a “cell”, typically a 32-bit number or bit pattern. The
meaning of a value in a cell depends on the particular application —it need not Tags:^58
always be a signed integer value. PAWNallows to attach a “meaning” to a cell with
its “tag” mechanism.

Based on tags, PAWNalso allows you to redefine operators for cells with a specific
purpose. The example below defines a tag “ones” and an operator to add two “ones”
values together (the example also implements operators for subtraction and nega-
tion). The example was inspired by the checksum algorithm of several protocols in
the TCP/IP protocol suite: it simulates oneʼs complement arithmetic by adding the
carry bit of an arithmetic overflow back to the least significant bit of the value.

LISTING: ones.p
forward ones: operator+(ones: a, ones: b)
forward ones: operator-(ones: a, ones: b)
forward ones: operator-(ones: a)

@start()
{
var ones: chksum = ones: 0xffffffff
print "Input values in hexadecimal, zero to exit\n"
var ones: value

74 — User-defined operators

do
{
print ">> "
value = ones: getvalue(.base=16)
chksum = chksum + value
printf "Checksum = %x\n", chksum
}
while (value)
}
stock ones: operator+(ones: a, ones: b)
{
const ones: mask = ones: 0xffff /* word mask _/
const ones: shift = ones: 16 /_ word shift _/
/_ add low words and high words separately _/
var ones: r1 = (a & mask) + (b & mask)
var ones: r2 = (a >>> shift) + (b >>> shift)
var ones: carry
restart: /_ code label (goto target) _/
/_ add carry of the new low word to the high word, then
- strip it from the low word
_/
carry = (r1 >>> shift)
r2 += carry
r1 &= mask
/_ add the carry from the new high word back to the low
- word, then strip it from the high word
_/
carry = (r2 >>> shift)
r1 += carry
r2 &= mask
/_ a carry from the high word injected back into the low
- word may cause the new low to overflow, so restart in
- that case
*/
if (carry)
goto restart
return (r2 << shift) | r1
}

stock ones: operator-(ones: a)
return (a == ones: 0xffffffff)? a : ~a
stock ones: operator-(ones: a, ones: b)
return a + -b

The notable line in the example is “chksum = chksum + value” in the loop in
function@start. Since both the variableschksumandvaluehave the tagones, the
“+” operator refers to the user-defined operator (instead of the default “+” opera-
tor). User-defined operators are merely a notational convenience; the same effect
is achieved by calling functions explicitly.

The definition of an operator is similar to the definition of a function, with the dif-
ference that the name of the operator is composed by the keyword “operator” and
the character of the operator itself. In the above example, both the unary “-” and

```
User-defined operators — 75
```

the binary “-” operators are redefined. An operator function for a binary opera-
tor must have two arguments, one for an unary operator must have one argument.
Note that the binary “-” operator adds the two values together after inverting the
sign of the second operand. The subtraction operator thereby refers to both the
user-defined “negation” (unary “-”) and addition operators.

A redefined operator must adhere to the following restrictions:

⋄A user-defined operator must be declared before use (this is in contrast to “nor- Forward declara-
mal” functions); so either put the implementation of the user-defined operator tion: 70
above the functions that use it, or add a forward declaration near the top of the
file.

⋄Only the following operators may be redefined:+,-,*,/,%,++,--,==,!=,<,
>,<=,>=,!and=. That is, the sets of arithmetic and relational operators can
be overloaded, but the bitwise operators and the logical operators cannot. The=
and!operators are a special case.

⋄You cannot invent new operators; you cannot define operator “#” for example.

⋄The precedence level and associativity of the operators, as well as their “arity”
remain as defined. You cannot make an unary “+” operator, for example.

⋄The return tag of the relational operators and of the “!” operator must be “bool:”.

⋄The return tag of the arithmetic operators is at your choosing, but you cannot
redefine an operator that is identical to another operator except for its return
tag. For example, you cannot make both
alpha: operator+(alpha: a, alpha: b)
and
beta: operator+(alpha: a, alpha: b)
(The assignment operator is an exception to this rule.)

⋄PAWNalready defines operators to work on untagged cells, you cannot redefine
the operators where all arguments are without a tag.

⋄The arguments of the operator function must be non-arrays passed by value. You
cannot make an operator work on arrays.

In the example given above, both arguments of the binary operators have the same
tag. This is not required; you may, for example, define a binary “+” operator that
adds an integer value to a “ones:” number.

Essentially, the operation of the PAWNparser is to look up the tag(s) of the operand(s)
that the operator works on and to look up whether a user-defined operator exists
for the combination of the operator and the tag(s). However, the parser recognizes
special situations and provides the following features:

⋄The parser recognizes operators like “+=” as a sequence of “+” and “=” and it will
call a user-defined operator “+” if available and/or a user-defined operator “=”.
In the example program, the line “chksum = chksum + value” might have been
abbreviated to “chksum += value”.

```
76 — User-defined operators
```

⋄The parser recognizes commutative operators (“+”, “*”, “==”, and “!=”) and it will
swap the operands of a commutative operator if that produces a fit with a user-
defined operator. For example, there is usually no need to implement both
ones:operator+(ones:a, b)
and
ones:operator+(a, ones:b)
(implementing both functions is valid, and it is useful in case the user-defined
operator should not be commutative).
⋄Prefix and postfix operators are handled automatically. You only need to define
one user operator for the “++” and “--” operators for a tag.
⋄The parser calls the “!” operator implicitly in case of a test without explicit com-
parison. For example, in the statement “if (var) ...” when “var” has tag
“ones:”, the user-defined operator “!” will be called forvar. The “!” operator
thus doubles as a “test for zero” operator. (In oneʼs complement arithmetic, both
the “all-ones” and the “all-zeros” bit patterns represent zero.)
⋄The user-defined assignment operator is implicitly called for a function argu-
“Call by value” ver-
sus “call by refer-
ence”: 61

```
ment that is passed “by value” when the tag names of theformaland theactual
arguments match the tag names of the left and right hand sides of the operator.
In other words, the PAWNparser simulates that “pass by value” happens through
assignment. The user-defined operator is not called for function arguments that
are passed “by reference”.
⋄If you wish to forbid an operation, you can “forward declare” the operator with-
out ever defining it (seepage 70). This will flag an error when the user-defined
operator is invoked. For example, to forbid the “%” operator (remainder after di-
vision) on floating point values, you can add the line:
forward Float: operator%(Float: a, Float: b)
```

```
User-defined operators can be declared “stock” or “native”. In the case of a native
```

Native functions: (^72) operator function, the definition should include an external name. For example
(when, on the hostʼs side, the native function is calledfloat_add):
LISTING: native operator+ function
native Float: operator+(Float: val, Float: val) = float_add
The user-defined assignment operator is a special case, because it is an operator
that has a side effect. Although the operator has the appearance of a binary opera-
tor, its “expression result” is the value at the right hand —the assignment operator
would be a “null”-operator if it werenʼt for its side-effect. In PAWNa user-defined
assignment operator is declared as:
LISTING: operator= function
ones: operator=(a)
return ones: ( (a >= 0)? a : ~(-a) )

```
User-defined operators — 77
```

The user-defined “=” operator looks like a unary operator in this definition, but it is
a special case nevertheless. In contrast to the other operators, the tag of the return
value for the user-defined operator is important: the PAWNparser uses the tags of
the argument and the return value to find a matching user-defined operator.

The example function above is a typical application for a user-defined assignment
operator: to automatically coerce/convert an untagged value to a tagged value, and
to optionally change the memory representation of the value in the process. Specif-
ically, the statement “var ones:A = -5” causes the user-defined operator to run,
and for the constant-5the operator will return “~(- -5)”, or~5, or 6 .

Floating point and fixed point arithmetic

PAWNonly has intrinsic support for integer arithmetic (theZ-domain, or “whole
numbers”, both positive and negative). Support for floating point arithmetic or
fixed point arithmetic must be implemented through functions or user operators.
User operators allow a more natural notation of expressions with fixed or floating
point numbers.

The PAWNparser has support for literal values with a fractional part, which it calls
Rational literals: 84
# pragma rational:
104

“rational numbers”. Support for rational literals must be enabled explicitly with
a#pragma. The#pragmaindicates how the rational numbers must be stored —
floating point or fixed point. For fixed point rational values, the#pragmaalso spec-
ifies the precision in decimals. Two examples for the#pragmaare:

# pragma rational Float /*floating point format _/
# pragma rational Fixed(3) /_ fixed point, with 3 decimals*/

Since a fixed point value must still fit in a cell, the number of decimals and the range
of a fixed point value are related. For a fixed point value with 3 decimals, the range
would be 2 ; 147 ; 482 :::+ 2; 147 ; 482.

The format for a rational number may only be specified once for the entire PAWN
program. In an implementation one typically chooses either floating point sup-
port or fixed point support. As stated above, for the actual implementation of the
floating point or fixed point arithmetic, PAWNrequires the help of (native) functions
and user-defined operators. A good place to put the#pragmafor rational number
support would be in the include file that also defines the functions and operators.

The include fileyfor fixed point arithmetic contains definitions like:

```
 Modern CPUs use twoʼs complement integer arithmetic. For positive values, the bitwise represen-
tation of a value is the same in oneʼs complement and twoʼs complement, but the representations
differ for negative values. For instance, the same bit pattern that means -5 in oneʼs complement
stands for -6 in twoʼs complement.
```

```
ySee the application note “Fixed Point Support Library” for where to obtain the include file.
```

```
78 — User-defined operators
```

```
native Fixed: operator*(Fixed: val1, Fixed: val2) = fmul
native Fixed: operator/(Fixed: val1, Fixed: val2) = fdiv
```

```
The user-defined operators for multiplication and division of two fixed point num-
bers are aliased directly to the native functionsfmulandfdiv. The host application
must, then, provide these native functions.
Another native user-defined operator is convenient to transform an integer to fixed
point automatically, if it is assigned to a variable tagged as “Fixed:”:
native Fixed: operator=(oper) = fixed
```

With this definition, you can say “var Fixed: fract = 3” and the value will
User-defined opera-
tors: 73 be transformed to3.000when it is stored in variablefract. As explained in the
section on user-defined operators, the assignment operator also runs for function
arguments that are passed by value. In the expression “var Fixed: root =
sqroot(16)” (see the implementation of functionsqrootonpage 66), the user-
defined assignment operator is called on the argument 16.
For adding two fixed point values together, the default “+” operator is sufficient, and
the same goes for subtraction. Adding a normal (integer) number to a fixed point
number is different: the normal value must be scaled before adding it. Hence, the
include file implements operators for that purpose too:
LISTING: additive operators, commutative and non-commutative
stock Fixed: operator+(Fixed: val1, val2)
return val1 + fixed(val2)
stock Fixed: operator-(Fixed: val1, val2)
return val1 - fixed(val2)
stock Fixed: operator-(val1, Fixed: val2)
return fixed(val1) - val2

```
The “+” operator is commutative, so one implementation handles both cases. For
the “-” operator, both cases must be implemented separately.
Finally, the include file forbids the use of the remainder operator (“%”) on fixed
point values: the remainder is only applicable to integer divisions:
LISTING: forbidden operators on fixed point values
forward Fixed: operator%(Fixed: val1, Fixed: val2)
forward Fixed: operator%(Fixed: val1, val2)
forward Fixed: operator%(val1, Fixed: val2)
```

```
Because of the presence of the (forward) declaration of the operator, the PAWN
parser will attempt to use the user-defined operator rather than the default “%” op-
erator. By not implementing the operator, the parser will subsequently issue an
error message.
```

### 79

The preprocessor

The first phase of compiling a PAWNsource file to the executable P-code is “pre-
processing”: a general purpose text filter that modifies/cleans up the source text
before it is fed into the parser. The preprocessing phase removes comments and
“conditionally compiled” blocks, processes the compiler directives and performs
find-&-replace operations on the text of the source file. The compiler directives are
summarized onpage 100and the text substitution (“find-&-replace”) is the topic of
this chapter.

The preprocessor is a process that is invoked on all source lines immediately after
they are read. No syntax checking is performed during the text substitutions. While
the preprocessor allows powerful tricks in the PAWNlanguage, it is also easy to shoot
yourself in the foot with it.

In this chapter, I will refer to the C/C++language on several occasions because
PAWNʼs preprocessor is similar to the one in C/C++. That said, the PAWNpreproces-
sor is incompatible with the C/C++preprocessor.

The#definedirective defines the preprocessor macros. Simple macros are:

# define maxsprites 25
# define CopyRightString "(c) Copyright 2004 by me"

In the PAWNscript, you can then use them as you would use constants. For example:

# define maxsprites 25
# define CopyRightString "(c) Copyright 2004 by me"
@start()
{
print( Copyright )
var sprites[maxsprites]
}

By the way, for these simple macros there are equivalent PAWNconstructs:

const maxsprites = 25
stock const CopyRightString[] = "(c) Copyright 2004 by me"

These constant declarations have the advantage of better error checking and the
ability to create tagged constants. The syntax for a string constant is an array vari-
able that is declared both “const” and “stock”. Theconstattribute prohibits any
change to the string and thestockattribute makes the declaration “disappear” if it
is never referred to.

Substitution macros can take up to 10 parameters. A typical use for macros with
parameters is to simulate tiny functions:

LISTING: the “min” macro
# define min(%1,%2) ((%1) < (%2)? (%1) : (%2))

```
80 — The preprocessor
```

```
If you know C/C++, you will recognize the habit of enclosing each argument and
the whole substitution expression in parentheses.
If you use the above macro in a script in the following way:
LISTING: bad usage of the “min” macro
var a = 1, b = 4
var min = min(++a,b)
```

```
the preprocessor translates it to:
var a = 1, b = 4
var min = ((++a) < (b)? (++a) : (b))
```

```
which causes “a” to possibly be incremented twice. This is one of the traps that
you can trip into when using substitution macros (this particular problem is well
known to C/C++programmers). Therefore, it may be a good idea to use a naming
convention to distinguish macros from functions. In C/C++it is common practice
to write preprocessor macros in all upper case.
To show why enclosing macro arguments in parentheses is a good idea, consider
the macro:
#define ceil_div(%1,%2) (%1 + %2 - 1) / %2
```

This macro divides the first argument by the second argument, but roundingup-
wardsto the nearest integer (the divide operator, “/”, rounds downwards). If you
use it as follows:
var a = 5
var b = ceil_div(8, a - 2)
the second line expands to “var b = (8 + a - 2 - 1) / a - 2”, which, considering
Operator prece-
dence: 94 the precedence levels of the PAWNoperators, leads to “b” being set to zero (if “a”
is 5). What you would have expected from looking at the macro invocation is eight
divided by three (“a - 2”), rounded upwards —hence, that “b” would be set to the
value 3. Changing the macro to enclose each parameter in parentheses solves the
problem. For similar reasons, it is also advised to enclose the complete replace-
ment text in parentheses. Below is theceil_divmacro modified accordingly:
# define ceil_div(%1,%2) ( ((%1) + (%2) - 1) / (%2) )

```
The pattern matching is subtler than matching strings that look like function calls.
The pattern matches text literally, but accepts arbitrary text where the pattern spec-
ifies one or more parameter(s). You can create patterns like:
LISTING: macro that translates a syntax for variable assignment to a function call
#define Field.%1=%2; SetField(%1,%2)
When the expansion of a macro contains text that matches other macros, the ex-
pansion is performed at invocation time, not at definition time. Thus the code:
#define a(%1) (1+b(%1))
#define b(%1) (2*(%1))
var c = a(8)
```

```
The preprocessor — 81
```

will evaluate to “var c = (1+(2*(8)))”, even though the macro “b” was not defined
at the time of the definition of “a”.

If an argument in the replacement text has a “#” immediately in front of it, the
argument is converted to a packed string constant —meaning that double quotes
are tagged at the beginning and the end. For example, if you have the definition:

# define log(%1) "ERR: " ... #%1 ... "\n"

then the expressionlog(test)will result in"ERR: "::: "test"::: "nn". The “#” See page^86 for
concatenating lit-
eral strings with the
“...” operator

operator is also called the “stringize” operator, as it converts arguments to (packed)
strings.

In the preceding examples, the pattern and the substitution text fit on a single line,
as is the case for all directives. For macros where this is inconvenient, an alternative Directives:^100
syntax is to enclose the substitution text between square brackets. The substitution
text must stillstartat the same line as the pattern, but it may be split over multiple
lines.

The pattern matching is constrained to the following rules:
⋄There may beno square bracketsandno space charactersin the pattern. If you must
match a space, you need to use the “\32;” escape sequence. The substitution
text, on the other hand,maycontain space characters and brackets. Due to the
matching rules of the macro pattern (explained below), matching a space char-
acter is rarely needed.
⋄Escape sequences may appear in the pattern, and can be used, for example, to
match a literal “%” character.
⋄The pattern must not end with a parameter; a pattern like “set:%1=%2” is illegal.
If you wish to match with the end of a statement, you can add a semicolon at the
end of the pattern. If semicolons are optional at the end of each statement, the
semicolon will also match a newline in the source.
⋄The pattern must start with a letter, an underscore, or an “@” character The first
part of the pattern that consists of alphanumeric characters (plus the “_” and/
“@”) is the “name” or the “prefix” of the macro. On thedefinedoperator and the
# undefdirective, you specify the macro prefix.
⋄In matching a pattern, the preprocessor ignores white space between non-alpha-
numeric symbols and white space between an alphanumeric symbol and a non-
alphanumeric one, with one exception: between two identical symbols, white
space is not ignored. Therefore:
the patternabc(+-)matches “abc ( + - )”
the patternabc(--)matches “abc ( -- )” but does not match
“abc(- -)”
⋄There are up to 10 parameters, denoted with a “%” and a single digit (1 to 9 and
0). The order of the parameters in a pattern is not important.
⋄The#definesymbol is a parserdirective. As with all parser directives, the pattern
definition must fit on a single line. You can circumvent this with a “\” on the end Directives:^100
of the line. The text to match must also fit on a single line.

82 — The preprocessor

Note that in the presence of macros, lines of source code may not be what they
appear: what looks like an array access may be “preprocessed” to a function call,
and vice versa.

A host application that embeds the PAWNparser may provide an option to let you
check the result of text substitution through macros. If you are using the standard
PAWNtoolset, you will find instructions of how to use the compiler and run-time in
the companion booklet “The PAWNbooklet — Implementerʼs Guide”.

### 83

General syntax

**Format**
Identifiers, numbers and tokens are separated by spaces, tabs, carriage re-
turns and “form feeds”. Series of one or more of these separators are called
white space.

**Optional semicolons**
Semicolons (to end a statement) are optional if they occur at the end of a
Optional semi-
line. Semicolons are required to separate multiple statements on a single colons: 104
line. An expression may still wrap over multiple lines, but postfix operators
(++and--)mustappear on the same line as their operand.

**Comments**
Text between the tokens/_and_/(both tokens may be at the same line or
at different lines) and text behind//(up to the end of the line) is a program-
ming comment. The parser treats a comment as white space. Comments
may not be nested.

```
A comment that starts with “/** ” (two stars and white-space behind the
second star) and ends with “*/” is a documentation comment. A comment
that starts with “///” (three slashes and white-space behind the third slash)
is also a documentation comment. The parser may treat documentation
comments in a special way; for example, it may construct on-line help from
it.
```

**Identifiers**
Names of variables, functions and constants. Identifiers consist of the char-
actersa:::z,A:::Z, 0 ::: 9 ,_or@; the first character may not be a digit. The
characters@and_by themselves are not valid identifiers, i.e. “_Up” is a
valid identifier, but “_” is not.
PAWNis case sensitive.
A parser may truncate an identifier at a maximum length. The number of
significant characters is implementation defined, but it is at least 16 char-
acters.

**Reserved words (keywords)**
Statements Operators Directives Other
assert defined #assert const
break sizeof #define forward
case state #else native
continue tagof #elseif new
default #endif operator
do #endinput public

```
84 — General syntax
```

else #error static
exit #file stock
for #if var
goto #include
if #line
return #pragma
sleep #section
state #tryinclude
switch #undef
while #warning
Next to reserved words, PAWNalso has several predefined constants, you
Predefined con-
stants: 87 cannot use the symbol names of the predefined constants for variable or
function names.
**Constants (literals)
Integer numeric constants
binary**
0bfollowed by a series of the digits 0 and 1
**decimal**
a series of digits between 0 and 9
**hexadecimal**
0xfollowed by a series of digits between 0 and 9 and the
lettersatof
In all number radices, the single quote may be used as a “digit group
separator”. For decimal numbers, the quote separates 3 digit se-
quencess (thousands separator); for hexadecimal numbers, a quote
separates 4 (hexadecimal) digits; for binary numbers, the quote
separates 8 digits. Using the single quote as a digit group separator
is optional, but if used, the number of digits after the quote must
conform to the above specification.
**Rational number constants**
A rational number is a number with a fractional part. A rational

Rational numbers
are also called “real
numbers” or “float-
ing point numbers”

number starts with one or more digits, contains a decimal point
and has at least one digit following the decimal point. For exam-
ple, “12.0” and “0.75” are valid rational numbers. Optionally, an
exponent may be appended to the rational number; the exponent
notation is the letter “e” (lower case) followed by a signed integer
numeric constant. For example, “3.12e4” is a valid rational number
with an exponent.
Support for rational numbers must be enabled with#pragma ra-
# pragma rational:
104 tionaldirective. Depending on the options set with this direc-
tive, the rational number represents a floating point or a fixed point
number.

```
General syntax — 85
```

```
The single quote may be used as a “thousands separator” (separat-
ing 3 digits) in the “whole part” of the number.
```

**Character constants**
A single ASCII or Unicode character surrounded by single quotes is
a character constant (for example:'a','7','$'). The single quote
that starts a character constant may either be a normal (forward)
single quote or a reverse single quote. The terminating quote must
always be a forward single quote.

```
Character constants are assumed to be numeric constants.
```

```
Escape sequences
'na' Audible alarm (beep)
'nb' Backspace
'ne' Escape
'nf' Form feed
'nn' New-line
'nr' Carriage Return
'nt' Horizontal tab
'nv' Vertical tab
'nn' n the escape character
'n'' ' single quote
'n"' " double quote
'n% % percent sign
'nddd;' character code withdecimalcode “ddd”
'nxhhh;' character code withhexadecimalcode “hhh”
```

```
The semicolon after thenddd;andnxhhh;codes is optional. Its
purpose is to give the escape sequence sequence an explicit termi-
nation symbol when it is used in a string constant.
```

```
The backslash (“\”) is the default “escape” character. If desired, you
can set a different escape character with the#pragma ctrlchar
directive (page 102).
```

**String constants**
String constants are assumed to be arrays with a size that is suffi-
cient to hold all characters plus a terminating'\0'. Each string is
stored at a unique position in memory; there is no elimination of
duplicate strings.

```
Anunpackedstring is a sequence of zero or more ASCII or Unicode
characters surrounded bydoubled single quotes. Each array element
contains a single character. Both the forward and the reversed sin-
gle quotes can start a string.
```

86 — General syntax

```
unpacked string constant:
``the quick brown fox...''
Apackedstring literal sequence of zero or more ASCII characters
surrounded by double quotes.
packed string constant:
"...packed the lazy dog in a bag"
In the case of a packed string, the parser packs as many characters
in a cell as will fit. A character is not addressable as a single unit, in-
stead each element of the array contains multiple characters. The
first character in a “pack” occupies the highest bits of the array ele-
ment. In environments that store memory words with the high byte
at the lower address (Big Endian, or Motorola format), the individ-
ual characters are stored in the memory cells in the same order as
they are in the string. A packed string ends with a zero character
and the string is padded (with zero bytes) to a multiple of cells.
A packed string can only hold characters from asingle-bytecharac-
ter set, such asASCIIor one of the extendedASCIIsets from the ISO
8859 norm.
Escape sequences may be used within strings. See the section on
character constants (the previous page) for a list of the escape se-
quences.
There is an alternative syntax for “plain strings”. In a plain string,
every character is taken as-is and escape sequences are not recog-
nized. Plain strings are convenient to store file/resource names,
especially in the case where the escape character is also used as a
special character by the operating system or host application.
The syntax for a plain string is the escape character followed by
the string in double quotes. The backslash (“\”) is the default “es-
cape” character. In a plain string all characters are taken literally,
including escape sequences.
plain (unpacked) string constant:
\''C:\all my work\novel.rtf''
In the above example, the occurrences of “\a” and “\n” donotin-
dicate escape sequences, but rather the literal character pairs “\”
and “a”, and “\” and “n”.
Plainpackedstrings exist as well:
\"C:\all my work\novel.rtf"
Two string literals may be concatenated by inserting with an ellip-
sis operator (three dots, or “...”) between the strings. For exam-
```

```
General syntax — 87
```

```
ple:
"The quick" ... "brown fox"
This syntax works with packed, unpacked and raw strings. Dif-
ferent kinds of string literals should not be concatenated, though.
String concatenation is valid for stringliteralsonly —string vari-
ables must be concatenated with a library function or with run-
time code.
```

```
Array constants
A series of numeric constants between braces is an array constant.
Array constants can be used to initialize array variables with (see
page 54) and they can be passed as function arguments (seepage
61 ).
```

**Symbolic constants**
A source file declares symbolic constants with theconstinstruction. A sin-
gleconstkeyword may declare a list of constants with sequentially incre-
mented values and sharing the same tag name.

```
const identifier = constant expression
Creates a single symbolic constantwith the value of the constant
expression on the right hand of the assignment operator. The con- Examples:^7 ,^17
stant can be used at any place where a literal number is valid (for ex-
ample: in expressions, in array declarations and in directives like
“#if” and “#assert”).
```

```
const tagnamefconstant listg
A list of symbolic names, grouped by braces, may follow theconst
keyword. Theconstant listis a series of identifiers separated by
commas. The first indentifier must be explicitly assigned a (nu- Identifiers:^83
meric) value. Unless overruled, every subsequent constant has the
value of its predecessor plus 1.
Example: 21
The optionaltagnametoken that follows theconstkeyword is used
as the default tag name for every symbol in the constant list.The
See page 58 for ex-
amples of enumer-
ated “const” decla-
rations
```

```
symbols in the constant list may have an explicit tag, which over-
rules the default tag name.
```

```
A symbolic constant that is defined locally, is valid up to the end of the en-
closing block. A local symbolic constant may not have the same name as a
variable (local or global), a function, or another constant (local or global).
```

**Predefined constants**

```
cellbits The size of a cell in bits.
cellmax The largest valid positive value that a cell can hold (2147483647
for a 32-bit cell).
```

```
88 — General syntax
```

```
cellmin The largest valid negative value that a cell can hold (-2147483648
for a 32-bit cell).
charbits The size of apackedcharacter in bits; usually 8.
charmax The largest validpackedcharacter value; usually 255.
charmin The smallest valid character value, zero for both packed and un-
packed characters.
debug The debug level: 2 if the parser creates full symbolic informa-
tion plus run-time bounds checking, 1 if the parser generates
run-time checking only (assertions and array bounds checks),
and 0 (zero) if all debug support and run-time checking was
turned off.
false 0 (this constant is tagged asbool:).
line The current line number in the source file.
Pawn The version number of the PAWNcompiler in Binary Coded Dec-
imals (BCD) —that is, for version 2.8.1 the constant is “0x281”.
true 1 (this constant is tagged asbool:).
ucharmax The largestunpackedcharacter value, its value depends on the
size of a cell. one can use this constant to check whether a string
is packed or unpacked, seepage 117.
Tag names
A tag consists of an identifier followed by a colon. There may be no white
```

Identifiers: (^83) space between the identifier and the colon.
**Predefined tag names**
bool: For “true/false” flags. The predefined constantstrueandfalse
have this tag.
Fixed: Rational numbers have this tag when fixed point support is en-
abled (page 104).
Float: Rational numbers have this tag when floating point support is
enabled (page 104).

### 89

Operators and expressions

An expression consists of one or more operands with an operator. The operand
can be a variable, a constant or another expression. An expression followed by a
semicolon is a statement.

LISTING: examples of expressions
v++
f(a1, a2)
v = (ia1 * ia2) / ia3

Notational conventions

The operation of some operators depends on the specific kinds of operands. In the
following sections in this chapter, operands are notated thus:
**e** any expression;
**v** any expression to which a value can be assigned (“lvalue” expressions);
**a** an array;
**f** a function;
**s** a symbol —which is a variable, a constant or a function.

Arithmetic
- e1 + e2
Results in the addition ofe1ande2.

- e1 - e2
    Results in the subtraction ofe1ande2.

```
-e
Results in the arithmetic negation of a (twoʼs complement).
```

```
* e1 * e2
Results in the multiplication ofe1ande2.
```

```
/ e1 / e2
Results in the division ofe1bye2. The result is truncated to
the nearest integral value that is less than or equal to the quo-
tient. Both negative and positive values are rounded down,
i.e. towards1.
```

90 — Bit manipulation

```
% e1 % e2
Results in the remainder of the division ofe1bye2. The sign
of the remainder follows the sign ofe2. Integer division and
remainder have the Euclidean property:D = q*d + r, where
q = D/dandr = D%d.
```

```
++ v++
incrementsvby 1; the result if the expression is the value of
vbeforeit is incremented.
++v
incrementsvby 1; the result if the expression is the value of
vafterit is incremented.
```

```
-- v--
decrementsvby 1; the result if the expression is the value of
vbeforeit is decremented.
--v
decrementsvby 1; the result if the expression is the value of
vafterit is decremented.
```

```
Notes: The unary + is not defined in PAWN.
The operators++and--modify the operand. The operand
must be anlvalue.
```

Bit manipulation

```
~ ~e
results in the oneʼs complement ofe.
```

```
>> e1 >> e2
results in thearithmeticshift to the right ofe1bye2bits. The
shift operation is signed: the leftmost bit ofe1is copied to
vacant bits in the result.
>>> e1 >>> e2
results in thelogicalshift to the right ofe1bye2bits. The
shift operation is unsigned: the vacant bits of the result are
filled with zeros.
<< e1 << e2
results in the value ofe1shifted to the left bye2bits; the right-
most bits are set to zero. There is no distinction between an
arithmetic and a logical left shift
```

```
& e1 & e2
results in the bitwise logical “and” ofe1ande2.
```

```
Assignment — 91
```

```
| e1 | e2
results in the bitwise logical “or” ofe1ande2.
```

```
^ e1^ e2
results in the bitwise “exclusive or” ofe1ande2.
```

Assignment

The result of an assignment expression is the value of the left operand after the
assignment.The left operand may not have a tag override. Tag names:^58

```
= v = e
assigns the value ofeto variablev.
= v = a
assigns arrayato variablev;vmust be an array with the same
size and dimensions asa;amay be a string or a literal array.
```

```
Note: the following operators combine an assignment with an op-
eration (arithmetic or bitwise); the result of the expression
is the value of the left operand after the arithmetic or bitwise
operation.
+= v += e
incrementsvwithe.
-= v -= e
decrementsvwithe
*= v *= e
multipliesvwithe
/= v /= e
dividesvbye.
%= v %= e
assigns the remainder of the division ofvbyetov.
>>= v >>= e
shiftsvarithmetically to the right byebits.
>>>= v >>>= e
shiftsvlogically to the right byebits.
<<= v <<= e
shiftsvto the left byebits.
&= v &= e
applies a bitwise “and” tovandeand assigns the result tov.
|= v |= e
applies a bitwise “or” tovandeand assigns the result tov.
^= v ^= e
applies a bitwise “exclusive or” tovandeand assigns the re-
sult tov.
```

92 — Relational

Relational

A logical “false” is represented by an integer value of 0; a logical “true” is repre-
sented by any value other than 0. Value results of relational expressions are either
0 or 1, and their tag is set to “bool:”.

```
== e1 == e2
results in a logical “true” ife1is equal toe2.
```

```
!= e1 != e2
results in a logical “true” ife1differs frome2.
```

```
Note: the following operators may be “chained”, as in the expres-
sion “e1 <= e2 <= e3”, with the semantics that the result is
“1” ifallindividual comparisons hold and “0” otherwise.
```

```
< e1 < e2
results in a logical “true” ife1is smaller thane2.
```

```
<= e1 <= e2
results in a logical “true” ife1is smaller than or equal toe2.
```

```
> e1 > e2
results in a logical “true” ife1is greater thane2.
```

```
>= e1 >= e2
results in a logical “true” ife1is greater than or equal toe2.
```

Boolean

A logical “false” is represented by an integer value of 0; a logical “true” is repre-
sented by any value other than 0. Value results of Boolean expressions are either 0
or 1, and their tag is set to “bool”.

```
! !e
results to a logical “true” ifewas logically “false”.
```

```
|| e1 || e2
results to a logical “true” if eithere1ore2(or both) are logi-
cally “true”. The expressione2is only evaluated ife1is logi-
cally “false”.
```

```
Miscellaneous — 93
```

```
&& e1 && e2
results to a logical “true” if bothe1ande2are logically “true”.
The expressione2is only evaluated ife1is logically “true”.
```

Miscellaneous
[ ] a[e]
array index: results tocellefrom array a.

```
{ } a{e}
array index: results tocharacterefrom “packed” array a.
```

```
( ) f(e1,e2,...eN)
results to the value returned by the functionf. The function
is called with the argumentse1,e2,:::eN. The order of evalu-
ation of the arguments is undefined (an implementation may
choose to evaluate function arguments in reversed order).
```

```
? : e1? e2 : e3
results in eithere2ore3, depending on the value ofe1. The
conditional expression is a compound expression with a two
part operator, “?” and “:”. Expressione2is evaluated ife1is
logically “true”,e3is evaluated ife1is logically “false”.
```

```
: tagname: e
tag override; the value of the expressionedoes not change,
but its tag changes. Seepage 58for more information.
```

```
, e1, e2
results ine2,e1is evaluated beforee2. If used in function
argument lists or a conditional expression, the comma ex-
pression must be surrounded by parentheses.
```

```
defined defined s
results in the value 1 if the symbol is defined. The symbol
may be a constant (page 84), or a global or local variable.
The tag of this expression isbool:.
```

```
94 — Operator precedence
```

Example: 65 sizeof sizeof s
results in the size in “elements” of the specified variable. For
simple variables and for arrays with a single dimension, an
element is a cell. For multi-dimensional arrays, the result is
the number of array elements in that dimension —append[]
to the array name to indicate a lower/more minor dimension.
If the size of a variable is unknown, the result is zero.
When used in a default value for a function argument, the
expression is evaluation at the point of the function call, in-
stead of in the function definition.

See alsopage 98
for state specifiers

```
state state s
wheresis the name of a state that is optionally prefixed with
the automaton name, this operator results in the value 1 if
the automatons is in the indicated state and in 0 otherwise.
The tag of this expression isbool:.
```

```
tagof tagof s
results in the a unique number that represents the tag of the
variable, the constant, the function result or the tag label.
When used in a default value for a function argument, the
expression is evaluation at the point of the function call, in-
stead of in the function definition.
```

```
Operator precedence
The table groups operators with equal precedence, starting with the operator group
with the highest precedence at the top of the table.
If, within a group, the evaluation order is not explicitly established by parenthe-
ses, it is determined by the association rules. For example:a*b/cis equivalent
with(a*b)/cbecause of the left-to-right association, anda=b=cis equivalent with
a=(b=c).
```

```
Operator precedence — 95
```

() function call left-to-right
[] array index (cell)
{} array index (character)
! logical not right-to-left

~ oneʼs complement

- twoʼs complement (unary minus)
++ increment
-- decrement
: tag override
defined symbol definition status
sizeof symbol size in “elements”
state automaton/state condition
tagof unique number for the tag

* multiplication left-to-right
/ division
% remainder

+ addition left-to-right

- subtraction

>> arithmetic shift right left-to-right
>>> logical shift right
<< shift left
& bitwise and left-to-right
^ bitwise exclusive or left-to-right
| bitwise or left-to-right
< smaller than left-to-right
<= smaller than or equal to
> greater than
>= greater than or equal to
== equality left-to-right
!= inequality
&& logical and left-to-right
|| logical or left-to-right
? : conditional right-to-left
= assignment right-to-left
*= /= %= += -= >>= >>>= <<= &= ^= |=
, comma left-to-right

### 96

Statements

```
A statement may take one or more lines, whereas one line may contain two or more
statements.
```

```
Control flow statements (if,if–else,for,while,do–whileandswitch) may be
nested.
```

```
Statement label
A label consists of an identifier followed by a colon (“:”). A label is a “jump
```

Identifiers: (^83) target” of thegotostatement.
Each statement may be preceded by a label. There must be a statement
after the label; an empty statement is allowed.
The scope of a label is the function in which it is declared (agotostatement
cannot therefore jump out off the current function to another function).
**Compound statement**
A compound statement is a series of zero or more statements surrounded
by braces ({and}). The final brace (}) should not be followed by a semi-
colon. Any statement may be replaced by a compound statement. A com-
pound statement is also called a block. A compound statement with zero
statements is a special case, and it is called an “empty statement”.
**Expression statement**
Any expression becomes a statement when a semicolon (“;”) is appended to
it. An expression also becomes a statement when only white space follows
it on the line and the expression cannot be extended over the next line.
**Empty statement**
An empty statement performs no operation and consists of a compound
block with zero statements; that is, it consists of the tokens “{ }”. Empty
statements are used in control flow statements if there is no action (e.g.
while (!iskey()) {}) or when defining a label just before the closing
brace of a compound statement. An empty statement does not end with a
semicolon.
**assert** expression
Aborts the program with a run-time error if the expression evaluates to log-
Example: (^8) ically “false”.
**break**
Terminates and exits the smallest enclosingdo,fororwhilestatement
Example: (^17) from any point within the loop other than the logical end. Thebreakstate-
ment moves program control to the next statement outside the loop.

```
Statements — 97
```

**continue**
Terminates the current iteration of the smallest enclosingdo,fororwhile
statement and moves program control to the condition part of the loop. If
the looping statement is aforstatement, control moves to the third expres-
sion in theforstatement (and thereafter to the second expression).

**do** statement **while (** expression **)**
Executes a statement before the condition part (thewhileclause) is evalu-
ated. The statement is repeated while the condition is logically “true”. The Example:^22
statement is at least executed once.

**exit** expression
Abort the program. The expression is optional, but it must start on the
same line as theexitstatement if it is present. Theexitinstruction re-
turns the expression value (plus the expression tag) to the host application,
or zero if no exit expression is present. The significance and purpose of
exit codes is implementation defined.

**for (** expression 1 **;** expression 2 **;** expression 3 **)** statement
All three expressions are optional. Examples: 7 , 8 , 17

```
Variable declara-
tions: 52
```

```
expression 1 Evaluated only once, and before entering the loop. This ex-
pression may be used to initialize a variable.
This expression may also hold a variable declaration, using
thevarsyntax. A variable declared in this expression exists
only in theforloop.
You cannot combine an expression (using existing variables)
and a declaration of new variables in this field —either all vari-
ables in this field must already exist, or they must all be cre-
ated in this field.
expression 2 Evaluated before each iteration of the loop; it ends the loop if
the expression results to logically “false”. If omitted, the re-
sult of expression 2 is assumed to be logically “true” (creating
an endless loop).
expression 3 Evaluated after each execution of the statement. Program
control moves from expression 3 to expression 2 for the next
iteration of the loop (unless expression 2 evaluates tofalse).
```

**goto** label
Moves program control (unconditionally) to the statement that follows the
specified label. The label must be within the same function as the goto
statement (a goto statement cannot jump out of a function).

**if (** expression **)** statement 1 **else** statement 2
Executes statement 1 if the expression results to logically “true”. Theelse
Example: 5

```
98 — Statements
```

```
clause of theifstatement is optional. If the expression results to logically
“false” and anelseclause exists, the statement 2 executes.
Whenifstatements are nested, anelseis associated with the closest pre-
cedingifstatement in the same block that does not yet have anelse.
```

```
return expression
Terminates the current function and moves program control to the state-
```

Examples: 8 , (^17) ment following the calling statement. The value of the expression is re-
turned as the function result. The expression may be an array variable or
a literal array.
The expression is optional, but it must start on the same line as thereturn
statement if it is present. If absent, the value of the function is zero.
**sleep** expression
Abort the program, but leave it in a re-startable state. The expression is
optional. If included, thesleepinstruction returns the expression value
(plus the expression tag) to the host application. The significance and pur-
pose of exit codes/tags is implementation defined; typically, an application
uses thesleepinstruction to allow for light-weight multi-tasking of several
concurrent PAWNprograms, or to implement “latent” functions.
**state (** expression **) automaton :** name
Changes the current state in the specified automaton. The expression be-
tween parentheses is optional; if it is absent, the parentheses must be omit-
ted as well. The name of the automaton is optional as well, when changing
the state of the default, anonymous, automaton; if the automaton name is
absent, the colon (“:”) must be omitted as well.
Below are two examples ofunconditionalstate changes. The first is for the
default automaton:
state handshake
and the second for a specific automaton:
state gps:handshake
Often, whether or not a state changes, depends on parameters of the event
Examples of state
changes: 33 or on the condition of the automaton as a whole. Since conditional state
changes are so frequent, the condition may be in thestateinstruction it-
self.The condition follows the keywordstate, between parentheses. The
state willonlychange if the condition is logically “true”.
Thestateinstruction causes an implied call to theexitfunction of the
“entry” functions:
36 , “exit” functions:
38
current state and to theentryfunction for the new state —if suchexitand
entryfunctions exist.
 The alternative is to foldunconditional state changes in the common if–else construct.

```
Statements — 99
```

**switch (** expression **)** fcase listg
Transfers control to one of several statements within the switch body, de-
pending on the value of the switch expression. The body of theswitch
statement is a compound statement that contains a series of “case clauses”.
Each “case clause” starts with the keywordcasefollowed by a constant list
andonestatement. The constant list is a series of expressions, separated
by commaʼs, that each evaluates to a constant value. The constant list ends
with a colon. To specify a “range” in the constant list, separate the lower
and upper bounds of the range with a double period (“..”). An example of
a range is: “case 1..9:”.
Theswitchstatement moves control to a “case clause” if the value of one of
the expressions in the constant list is equal to theswitchexpression result.
The “default clause” consists of the keyworddefaultand a colon. The de-
fault clause is optional, but if it is included, it must be the last clause in the
switch body. Theswitchstatement moves control to the “default clause”
if none of the case clauses match the expression result.
**Example** :
switch (weekday(12,31,1999))
{
case 0, 1: /*0 == Saturday, 1 == Sunday*/
print("weekend")
case 2:
print("Monday")
case 3:
print("Tuesday")
case 4:
print("Wednesday")
case 5:
print("Thursday")
case 6:
print("Friday")
default:
print("invalid week day")
}

**while (** expression **)** statement
Evaluates the expression and executes the statement if the expression eval-
uates to logically “true”. After executing the statement, program control Examples:^5 ,^17 ,^21
returns to the expression again. The statement is thus executed while the
expression is true.

### 100

Directives

All directives must appear first on a line (they may be preceded by white space, but
Line continuation:
125 not by any other characters). All directives start with the character#and the com-
plete instructionmay notspan more than one line —with an exception for#define.
**#assert** constant expression
Issues a compile time error if the supplied constant expression evaluates
to zero.The#assertdirective is useful to guard against implementation
See also “Prede-
fined constants” on
page 87

```
defined constructs on which a program may depend, such as the cell size
in bits, or the number of packed characters per cell.
#define pattern replacement
Defines a text substitution macro. The pattern is matched to all lines read
from the source files; the sections that match are replaced by the replace-
ment texts. The pattern and the replacement texts may contain parame-
ters, denoted by “%0” to “%9”. Seepage 79for details and examples on text
substitution.
#endinput
Closes the current file and thereby ignores all the text below the#endinput
directive.
#error message
Signals a “user error” with the specified message. User errors are fatal er-
rors and they serve a similar purpose as the#assertdirective. See#warn-
ingfor a non-fatal user error.
#file name
Adjusts the name for the current file. This directive is used implicitly by the
text preprocessor; there is usually no need to set a filename explicitly.
#if constant expression , #elseif, #else, #endif
Portions of a program may be parsed or be ignored depending on certain
conditions. The PAWNparser (compiler or interpreter) generates code only
for those portions for which the condition is true.
The directive#ifmust be followed by a constant expression. To check
whether a variable or constant is defined, use thedefinedoperator.
Zero or more#elseifdirectives may follow the initial#ifdirective. These
blocks are skipped if any of the preceding#ifor#elseifblocks was taken
(i.e. not skipped). As with the#ifdirective, a constant expression must
follow the#elseifexpression.
The#elsecauses the parser to skip all lines up to#endifif the preceding
#ifor any of the preceding#elseifdirectives were “true”, and the parses
these lines if all preceding blocks were skipped. The#elsedirective may
```

```
Directives — 101
```

```
be omitted; if present, there may be only be one#elseassociated with each
#if.
The#endifdirective terminates a program portion that is parsed condi-
tionally. Conditional directives can be nested and each#ifdirective must
be ended by an#endifdirective.
```

**#include** filename **or** <filename>
Inserts the contents of the specified file at the current position within the
current file. A filename between angle brackets (“<” and “>”) refers to a
system file; the PAWNparser (compiler or interpreter) will search for such
files only in a pre-set list of directories and not in the “current” directory.
Filenames that are unquoted or that appear in double quotes are normal
include files, for which a PAWNparser will look in the current directory first.
The PAWNparser first attempts to open the file with the specified name. If
that fails, it tries appending the extensions “.inc”, “.p” and “.pawn” to the
filename (in that order). The proposed default extension of include files is
“.inc”.

```
When the file can be opened successfully, the#includedirective defines
a constant with the name “_inc_” plus the base name of the file (the file-
name without path and extension) and the value 1. If the constant already
exists, the#includedirective skips opening and including the file, thus
preventing a double inclusion. To force a double include, remove the con-
stant definition with the#undefdirective before the second inclusion of
the file.
```

**#line** number
The current line number (in the current file). This directive is used implic-
itly by the text preprocessor; there is usually no need to set the line number
explicitly.

**#pragma** extra information
A “pragma” is a hook for a parser to specify additional settings, such as
warning levels or extra capabilities. Common#pragmas are:
# pragma align
Aligns the next declaration to the offset set with the alignment com-
piler option. Some (native) functions may perform better with pa-
rameters that are passed by reference when these are on bound-
aries of 8, 16, or even 32 bytes. Alignment requirements are de-
pendent of the host applications.
Putting the#pragma alignline in front of a declaration of a global
or a static variable aligns this variable to the boundary set with the
compiler option. This#pragmaaligns only the variable that imme-
diately follows the#pragma. The alignment of subsequent variables

```
102 — Directives
```

```
depends on the size and alignment of the variables that precede
it. For example, if a global array variable of 2 cells is aligned on a
16-byte boundary and a cell is 4 bytes, the next global variable is
located 8 bytes further.
Putting the#pragma alignline in front of a declaration of a func-
tion will align the stack frame of that function to the boundary spec-
ified earlier, with the result that the first local, non-“static”, variable
is aligned to that boundary. The alignment of subsequent variables
depends on the size and alignment of the variables that precede it.
In practice, to align a local non-static variable, you must align the
functionʼs stack frame and declare that variable before any other
variables.
#pragma amxlimitvalue
Sets themaximumsize, in bytes, that the compiled script may grow
to. This pragma is useful for (embedded) environments where the
maximum size of a script is bound to a hard upper limit.
If there is no setting for the amount of RAM for the data and stack
(see the pragmaamxram), this refers to the total memory require-
ments; if the amount of RAM is explicitly set, this value only gives
the amount of memory needed for the code and the static data.
```

```
#pragma amxram value
Sets themaximummemory requirements, in bytes, for data and
stack that a compiled script may have. This value is useful for (em-
bedded) environments where the maximum data size of a script is
bound to a hard upper limit. Especially in the case where the PAWN
script runs from ROM, the sizes for the code and data sections need
both to be set.
```

```
#pragma codepagename/value
The PAWNparser can translate characters in character constants
and inunpackedstrings to Unicode/UCS-4 “wide” characters. This
#pragmaindicates the codepage that must be used for the transla-
tion. See the sectionInternationalizationonpage 118for details and
required extra resources for the codepage translation.
```

# pragma ctrlcharcharacter
Defines the character to use to indicate the start of a “escape se-
Escape character:
85 quence”. By default, the control character is “\”.

```
For example
#pragma ctrlchar '$'
You may give the new value for the control character as a character
constant (between single quotes) or as a decimal or hexadecimal
```

```
Directives — 103
```

```
value. When you omit the value of the new control character, the
parser reverts to the default control character.
```

# pragma deprecatedvalue
The subsequent symbol is flagged as “deprecated”. If a script uses
it, the parser issues a warning.

# pragma dynamicvalue
Sets the size, in cells, of the memory block for dynamic data (the
stack and the heap) to the value specified by the expression. The
default size of the dynamic data block is implementation defined.
An implementation may also choose to grow the block on an as-
needed basis (see the host programʼs documentation, or the “Im-
plementerʼs Guide” for details).

# pragma libraryname
Sets the name of the (dynamically linked) extension module, in
which the native functions are implemented. This#pragmashould
appear above native function declarations that are part of the ex-
tension module.

```
The scope of this#pragmais from the line at which it appears until
the end of the file in which it appears. In typical usage, a#pragma
libraryline will appear at the top of an include file that declares
native functions for an extension module, and the scope of the li-
brary “link” ends at the end of that include file.
```

```
Thenameparameter may be absent, in which case the subsequent
declarations of native function are not associated withanyexten-
sion module.
```

```
A special syntax is “#pragma library -”, which instructs the PAWN
compiler to not generate a “library table” in the P-code file. This
makes the generated file smaller, at the cost of disabling dynami-
cally loaded extension modules.
```

# pragma opcodeset value
Instructs the PAWNcompiler to only use instructions in the indi-
cated set and below. The value can be 1, 2 or 3.

```
Optimization levels are directly related to the instruction set that
the compiler uses. Optimization levels 0 and 1 only use instruc-
tions from set 1 (core set); whereas optimization levels 2 and 3 in-
clude instructions from sets 2 (macro instructions) and 3 (packed
opcodes) respectively. Therefore, this pragma directive limits the
optimization level.
```

```
104 — Directives
```

# pragma overlayvalue
The PAWNcompiler can generate P-code such that it runs as dy-
namically loaded overlays. Currently, an overlay is the size of a
function. Whether the parser generates P-code for overlaid execu-
tion or for standard execution depends on the parser configuration
(and, perhaps, user settings). This#pragmaallows the script writer
to override the default. The parameter of this#pragmais the size
of the overlay pool in bytes, or zero to turn overlay support off.
# pragma rationaltagname(value)
Enables support for rational numbers. Thetagnameis the name of
Rational number
support: 84 the tag that rational numbers will have; typically one chooses the
names “Float:” or “Fixed:”. The presence ofvaluein parentheses
behindtagnameis optional: if it is omitted, a rational number is
stored as a “floating point” value according to the IEEE 754 norm; if
it is present, a rational number is a fixed precision number (“scaled
integer”) with the specified number of decimals.
# pragma semicolon value
Ifvalueis zero, no semicolon is required to end a statement if that
statement is last on a line. Semicolons are still needed to separate
multiple statements on the same line.

```
When semicolons are optional (the default), a postfix operator (one
of “++” and “--”) may not be the first token on a line, as they will be
interpreted as prefix operators.
```

# pragma tabsizevalue
The number of characters between two consecutiveTABpositions.
The default value is 8. You may need to set theTABsize to avoid
warning 217 (loose indentation) if the source code is indented al-
ternately with spaces and withTABcharacters. Alternatively, by set-
ting the “tabsize”#pragmato zero, the parser will no longer issue
warning 217.
# pragma unused symbol,:::
Marks the named symbol as “used”. Normally, the PAWNparser
Warning messages:
139 warns about unused variables and unused local constants. In most
situations, these variables and constants are redundant, and it is
better to remove them for the sake of code clarity. Especially in the
case of local constants, it may, however, be better (or required) to
keep the constant definitions. This#pragmathen permits to mark
the symbol (a variable or a constant) as “used”, and avoid a parser
warning.
The#pragmamust appearafterthe symbol declaration —but it need
not appear immediately after the declaration.

```
Directives — 105
```

```
There may appear multiple symbol names in a single#pragma, sep-
arated by commas.
#pragma warning disablevalue,:::
Disables (hides) the warnings that are specified on the line (by their
warning numbers). See appendix A for the list of warnings.
#pragma warning enable value,:::
Enables the warnings with the numeric identifiers.
#pragma warning pop
Restores the enabled/disabled status of all warnings, which must
have been “pushed” first.
#pragma warning push
Stores the enabled/disabled status of all warnings on an internal
stack, so that these can be restored by a later “pop”.
```

**#section** name
Starts a new section for the generated code. Any variables and functions
that are declared “static” are only visible to the section to which they be-
long. By default, each source file is a separate section and there is only one
section per file.
With the#sectiondirective, you can create multiple sections in a source
file. The name of a section is optional, if it is not set, a unique identifier for
the source file is used for the name of the section.
Any declared section ends automatically at the end of the file.

**#tryinclude** filename **or** <filename>
This directive behaves similarly as the#includedirective, but it does not
give an error when the file to include does not exist —i.e., try to include but
fail silently on error.

**#undef** name
Removes a text substitution macro or a numeric constant declared with
const. The “name” parameter must be the macro “prefix” —the alphanu-
meric part of the macro. Seepage 79for details and examples on text sub-
stitution.

**#warning** message
Signals a “user error” with the specified message. The message is consid-
ered to be a warning only. For a user error that aborts compilation, see the
# errordirective.

### 106

Proposed function library

```
Since PAWNis targeted as an application extension language, most of the functions
that are accessible to PAWNprograms will be specific to the host application. Nev-
ertheless, a small set of functions may prove useful to many environments.
```

Core functions

```
The “core” module consists of a set of functions that support the language itself.
Several of the functions are needed to pull arguments out of a variable argument
list (seepage 67).
```

clamp Force a value inside a range

```
Syntax: clamp(value, min=cellmin, max=cellmax)
value The value to force in a range.
```

```
min The low bound of the range.
max The high bound of the range.
```

```
Returns: valueif it is in the rangemin–max;minifvalueis lower thanmin;
andmaxifvalueis higher thanmax.
See also: max,min
```

```
funcidx Return a public function index
Syntax: funcidx(const name[])
```

Returns: The index of the named public function. If no public function with
the given name exists,funcidxreturns 1.
Notes: A host application runs a public function from the script by pass-
amxExec: see the
“Implementer’s
Guide”

```
ing the functionʼs index to the abstract machine (specifically function
amx_Exec). With this function, the script can query the index of a
public function, and thereby return the “next function to call” to the
application.
```

```
min — 107
```

getarg Get an argument

Syntax: getarg(arg, index=0)

```
arg The argument sequence number, use 0 for first argu-
ment.
index The index, in caseargrefers to an array.
```

Returns: The value of the argument.

Notes: This function retrieves an argument from a variable argument list.
When the argument is an array, theindexparameter specifies the
index into the array. The return value is the retrieved argument.

See also: numargs,setarg

heapspace Return free heap space

Syntax: heapspace()

Returns: The free space on the heap. The stack and the heap occupy a shared
memory area, so this value indicates the number of bytes that is left
for either the stack or the heap.

Notes: In absence of recursion, the PAWNparser can also give an estimate of
the required stack/heap space.

max Return the highest of two numbers

Syntax: max(value1, value2)

```
value1
value2 The two values for which to find the highest number.
```

Returns: The higher value ofvalue1andvalue2.

See also: clamp,min

min Return the lowest of two numbers

Syntax: min(value1, value2)

```
value1
value2 The two values for which to find the lowest number.
```

Returns: The lower value ofvalue1andvalue2.

See also: clamp,max

108 — numargs

numargs Return the number of arguments

Syntax: numargs()

Returns: The number of arguments passed to a function;numargsis useful in-
side functions with a variable argument list.

See also: getarg,setarg

random Return a pseudo-random number

Syntax: random(max)

```
max The limit for the random number.
```

Returns: A pseudo-random number in the range 0 :::max-1.

Notes: The standard random number generator of PAWNis a linear congru-
ential pseudo-random number generator with a range and a period of
231. Linear congruential pseudo-random number generators suffer
from serial correlation (especially in the low bits) and it is unsuitable
for applications that require high-quality random numbers.

setarg Set an argument

Syntax: setarg(arg, index=0, value)

```
arg The argument sequence number, use 0 for first argu-
ment.
index The index, in caseargrefers to an array.
```

```
value The value to set the argument to.
```

Returns: trueon success andfalseif the argument or the index are invalid.

Notes: This function sets the value of an argument from a variable argument
list. When the argument is an array, theindexparameter specifies
the index into the array.

See also: getarg,numargs

```
Console functions — 109
```

swapchars Swap bytes in a cell

Syntax: swapchars(c)

```
c The value for which to swap the bytes.
```

Returns: A value where the bytes in parameter “c” are exchanged (the lowest
byte becomes the highest byte).

tolower Convert a character to lower case

Syntax: tolower(c)

```
c The character to convert to lower case.
```

Returns: The upper case variant of the input character, if one exists, or the
unchanged character code of “c” if the letter “c” has no lower case
equivalent.

Notes: Support for accented characters is platform-dependent.

See also: toupper

toupper Convert a character to upper case

Syntax: toupper(c)

```
c The character to convert to upper case.
```

Returns: The lower case variant of the input character, if one exists, or the
unchanged character code of “c” if the letter “c” has no upper case
equivalent.

Notes: Support for accented characters is platform-dependent.

See also: tolower

Console functions

For testing purposes, the console functions that read user input and that output
strings in a scrollable window or on a standard terminal display are often conve-
nient. Not all terminal types and implementations may implement all functions
—especially the functions that clear the screen, set foreground and background
colours and control the cursor position, require an extended terminal control.

110 — clreol

clreol Clear rest of the line

Syntax: clreol()

Returns: This function always returns 0.

Notes: Clears the line at which the cursor is, from the position of the cursor
to the right margin of the console. This function does not move the
cursor.

See also: clrscr

clrscr Clear screen

Syntax: clrscr()

Returns: This function always returns 0.

Notes: Clears the display and moves the cursor to the upper left corner.

See also: clreol

getchar Read one character

Syntax: getchar(echo=true)

```
echo Iftrue, the character read from the keyboard is echoed
on the display.
```

Returns: The numeric code for the character that is read (this is usually the
ASCII code).

See also: getstring

getstring Read a line

Syntax: getstring(string[], size=sizeof string,
bool:pack=false)
string The line read from the keyboard is stored in this
parameter.
size The size of thestringparameter in cells.
pack Iftruethe function stores the line as apackedstring.

```
gotoxy — 111
```

Returns: The number ofcharactersread, excluding the terminating null char-
acter.

Notes: Functiongetstringstops reading when either the enter key is typed
or the maximum length is reached. The maximum length is incells
(not characters) and it includes a terminating null character. The
function can read both packed and unpacked strings; when reading
a packed string, the function may read morecharactersthan thesize
parameter specifies, since each cell holds multiple characters.

See also: getchar

getvalue Read a number

Syntax: getvalue(base=10, end=`nr', ...)

```
base Must be between 2 and 36, use 10 for decimal or 16 for
hexadecimal.
end The character code that terminates the input. More
than one character may be listed.
pack Iftruethe function stores the line as apackedstring.
```

Returns: The value that is read.

Notes: Read a value (a signed number) from the keyboard. Thegetvalue
function allows you to read in a numeric radix from 2 to 36 (thebase
parameter) with decimal radix by default.
By default the input ends when the user types the enter key, but one
or more different keys may be selected (theendparameter and subse-
quent). In the list of terminating keys, a positive number (like'\r')
displays the key and terminates input, and a negative number termi-
nates input without displaying the terminating key.

See also: getstring

gotoxy Set cursor location

Syntax: gotoxy(x=1, y=1)

```
x
y The new cursor position.
```

Returns: trueif the cursor moved andfalseif the requested position is in-
valid.

112 — print

Notes: Sets the cursor position on the console. The upper left corner is at
(1,1).

See also: clrscr

print Display text on the display

Syntax: print(const string[], foreground=-1, background=-1)

```
string The string to display.
foreground
background Colour codes for the foreground and the background
of the text string; see functionsetattrfor a lost of
colours. When left at -1, the default colours are used.
Note that a terminal or a host application may not
support colours.
```

Returns: This function always returns 0.

See also: printf,setattr

printf Display formatted text on the display

Syntax: printf(const format[], ...)

```
format The string to display, including any (optional) format-
ting codes.
```

Returns: This function always returns 0.

Notes: Prints a string with embedded codes:
%b print a number at this position in binary radix
%c print a character at this position
%d print a number at this position in decimal radix
%f print a floating point number at this position (assuming floating
point support is present)
%q print a fixed point number at this position (assuming fixed point
support is present)
%r print a rational number (either floating point or fixed point de-
pending on which is available) number at this position. If both
floating point and fixed point support are present,%ris equiva-
lent to%f.
%s print a character string at this position
%x print a number at this position in hexadecimal radix

```
Fixed point arithmetic — 113
```

```
Theprintffunction works similarly to theprintffunction of the C
language.
```

See also: print

setattr Set text colours

Syntax: setattr(foreground=-1, background=-1)

```
foreground
background The colour codes for the new foreground and back-
ground colours for text. When either of the two pa-
rameters is negative (or absent), the respective colour
setting will not be changed.
```

Returns: This function always returns 0.

Notes: On most systems, the colour value must be a value between zero and
seven, as per the ANSI Escape sequences, ISO 6429. Predefined con-
stants for the colours areblack(0),red(1),green(2),yellow(3),blue
(4),magenta(5),cyan(6) andwhite(7).

See also: clrscr

Date/time functions

Functions to get and set the current date and time, as well as a millisecond reso-
lution “event” timer are described in an application note entitled “Time Functions
Library” that is available separately.

File input/output

Functions for handling text and binary files, with direct support for UTF-8 text files,
is described in an application note entitled “File I/O Support Library” that is avail-
able separately.

Fixed point arithmetic

The fixed-point decimal arithmetic module for PAWNis described in an application
note entitled “Fixed Point Support Library” that is available separately.

114 — Floating point arithmetic

Floating point arithmetic

The floating-point arithmetic module for PAWNis described in an application note
entitled “Floating Point Support Library” that is available separately.

Process and library call interface

The functions to launch and control external applications and functions to use gen-
eral purpose DLLs or shared libraries are described in an application note entitled
“Process control and Foreign Function Interface” that is available separately.

String manipulation

A general set of string manipulation functions, operating on both packed and un-
packed strings, is described in an application note entitled “String Manipulation
Library”, available separately.

### 115

Pitfalls: differences from C

⋄PAWNlacks the typing mechanism of C. PAWNis an “integer-only” variety of C;
there are no structures or unions, and floating point support must be imple-
mented with user-defined operators and the help of native functions.

⋄The accepted syntax for rational numbers is stricter than that of floating point
values in C. Values like “.5” and “6.” are acceptable in C, but in PAWNone must
write “0.5” and “6.0” respectively. In C, the decimal period is optional if an ex-
ponent is included, so one can write “2E8”; PAWNdoes not accept the upper case
“E” (use a lower case “e”) and it requires the decimal point: e.g. “2.0e8”. Seepage
84 for more information.

⋄PAWNdoes not provide “pointers”. For the purpose of passing function arguments
by reference, PAWNprovides a “reference” argument, seepage 61. The “place-
holder” argument replaces some uses of the NULL pointer (page 63).

⋄Numbers can have hexadecimal, decimal or binary radix. Octal radix is not sup-
ported. See “Constants” onpage 84. Hexadecimal numbers must start with “0x”
(a lower case “x”), the prefix “0X” is invalid.

⋄Escape sequences (“\n”, “\t”, etc.) are the same, except for “\ddd” where “ddd”
represent threedecimaldigits, instead of theoctaldigits that C/C++uses. The
backslash (“\”) may be replaced with another symbol; see#pragma ctrlcharon
page 102.

⋄Cases in aswitchstatement arenot“fall through”. Only a single instruction may
(and must) follow eachcaselabel. To execute multiple instructions, you must
use a compound statement. Thedefaultclause of aswitchstatement must be
the last clause of theswitchstatement. More onpage 99. In C/C++,switch
is a “conditional goto”, akin to Fortranʼs calculated labels. In PAWN,switchis a
structured “if”.

⋄Abreakstatement breaks out of loops only. In C/C++, thebreakstatement also
ends acasein aswitchstatement. Switch statements are implemented differ-
ently in PAWN(seepage 99).

⋄PAWNsupports “array assignment”, with the restriction that both arrays must
have the same size. For example, if “a” and “b” are both arrays with 6 cells,
the expression “a = b” is valid. Next to literal strings, PAWNalso supports lit-
eral arrays, allowing the expression “a = {0,1,2,3,4,5}” (where “a” is an array
variable with 6 elements).

⋄definedis an operator, not a preprocessor directive. Thedefinedoperator in
PAWNoperates on constants (declared withconst), global variables, local vari-
ables and functions.

⋄Thesizeofoperator returns the size of a variable inelements, not in bytes. An
element may be a cell or a sub-array. Seepage 94for details.

116 — Pitfalls: differences from C

⋄The empty instruction is an empty compound block, not a semicolon (page 96).
This modification avoids a frequent error.

⋄Several compiler directives are incompatible with Cʼs preprocessor commands.
Notably, the#definedirective has different semantics than in C/C++, and the
# ifdefand#ifndefare replaced by the more general#ifdirective (see “Direc-
tives” onpage 100). To create numeric constants, see alsopage 87; to create string
constants, see alsopage 79.

⋄Text substitutions (see the#definedirective) are not matchedacrosslines. That
is, the text that you want to match and replace with a#definemacromustappear
on a single line.

⋄A division is carried out in such a way that the remainder after division has (or
would have) the same sign as the denominator. For positive denominators, this
means that the direction for truncation for the operator “/” is always towards the
smaller value, where -2 is smaller than -1, and that the “%” operator always gives
a positive result —regardless of the sign of the numerator. Seepage 89.

⋄There is no unary “+” operator, which is a “no-operation” operator anyway.

⋄Three of the bitwise operators have different precedence than in C. The prece-
dence levels of the “&”, “^” and|operators is higher than the relational operators
(Dennis Ritchie explained that these operators got their low precedence levels in
C because early C compilers did not yet have the logical “&&” and||operators,
so the bitwise “&” and|were used instead).

⋄The “extern” keyword does not exist in PAWN; the current implementation of the
compiler has no “linking phase”. To create a program from several source files,
add all source files the compilers command line, or create one main project script
file that “#includeʼs” all other source files. The PAWNcompiler can optimize out
functions and global variables that you do not use. See pages 53 and 72 for details.

⋄The keywordconstin PAWNimplements theenumfunctionality from C, seepage
87.

⋄In most situations, forward declarations of functions (i.e., prototypes) are not
necessary. PAWNis a two-pass compiler, it will see all functions on the first pass
and use them in the second pass. User-defined operators must be declared before
use, however.
If provided, forward declarations must matchexactlywith the function defini-
tion, parameter names may not be omitted from the prototype or differ from the
function definition. PAWNcares about parameter names in prototypes because of
the “named parameters” feature. One uses prototypes to call forwardly declared
functions. When doing so with named parameters, the compiler must already
know the names of the parameters (and their position in the parameter list). As
a result, the parameter names in a prototype must be equal to the ones in the
definition.

### 117

Assorted tips

Working with characters and strings

Strings can be in packed or in unpacked format. In the packed format, each cell will
typically hold four characters (in common implementations, a cell is 32-bit and a
character is 8 bit). In this configuration, the first character in a “pack” of four is the
highest byte of a cell and the fourth character is in the lowest byte of each cell.

A string must be stored in an array. For an unpacked string, the array must be large
enough to hold all characters in the string plus a terminating zero cell. That is, in
the example below, the variableustringis defined as having five cells, which is
just enough to contain the string with which it is initialized:

LISTING: unpacked string
var ustring[5] = ''test''

In a packed string, each cell contains several characters and the string ends with
a zero character. The example below will allocate enough cells to hold five packed
characters. In a typical implementation, there will be two cells in the array.

LISTING: packed string
var pstring{5} = "test"

In other words, the array is declared to be able to holdat leastthe specified number
of packed characters.

You can design routines that work on strings in both packed and unpacked formats.
To find out whether a string is packed or unpacked, look at the first cell of a string.
If its value is either negative or higher than the maximum possible value of an un-
packed character, the string is a packed string. Otherwise it is an unpacked string.

The code snippet below returnstrueif the input string is packed andfalseother-

See the separate
application note
for proposed na-
tive functions that
operate on both
packed and un-
wise: packed strings

LISTING: ispacked function
bool: ispacked(string[])
return !(0 <= string[0] <= ucharmax)

An unpacked string ends with a full zero cell. The end of a packed string is marked
with only a zero character. Since there may be up to four characters in a 32-bit cell,
this zero character may occur at any of the four positions in the “pack”. The{ }
operator extracts a character from a cell in an array. Basically, one uses the cell
index operator (“[ ]”) for unpacked strings and the character index operator (“{
}”) to work on packed strings.

For example, a routine that returns the length in characters of any string (packed
or unpacked) is:

```
118 — Internationalization
```

```
LISTING: mystrlen function
my_strlen(string[])
{
var len = 0
if (ispacked(string))
while (string{len} != EOS) /* get character from pack */
++len
else
while (string[len] != EOS) /* get cell */
++len
return len
}
```

```
If you make functions to work exclusively on either packed or unpacked strings, it
```

EOS: predefined
constant standing
for End Of String; it
has the value ’n0’

```
is a good idea to add an assertion to enforce this condition:
```

```
LISTING: strupper function
strupper(string[])
{
assert !ispacked(string)
for (var i=0; string[i] != EOS; ++i)
string[i] = toupper(string[i])
}
```

Although, in preceding paragraphs we have assumed that a cell is 32 bits wide and
a character is 8 bits, this should not be relied upon.The size of a cell is implemen-
Predefined con-
stants: 87 tation defined; the maximum and minimum values are in the predefined constants
cellmaxandcellmin. There are similar predefined constants for characters. One
may safely assume, however, that both the size of a character in bytes and the size
of a cell in bytes are powers of two.

```
The predefinedcharbitsandcellbitsconstants allow you to determine how
many packed characters fit in a cell. For example:
```

```
const CharsPerCell = cellbits / charbits
```

Internationalization

```
Programming examples in this manual have used the English language for all out-
put (prompts, messages,:::), and a Latin character set. This is not necessarily so;
one can, for example, modify the first “hello world” program onpage 3to:
```

```
LISTING: “hello world” in Greek
@start()
printf ''Γεια σας κόσμο\n''
```

```
Internationalization — 119
```

PAWNhas basic support for non-Latin alphabets, but it only accepts non-Latin char-
acters in strings and character constants. The PAWNlanguage requires that all key-
words and symbols (i.e. names of functions, variables, tags and other elements) be
encoded in theASCIIcharacter set.

For languages whose required character set is relatively small, a common solution
is to use an 8-bit extendedASCIIcharacter set (theASCIIcharacter set is 7-bit, holding
128 characters). The upper 128 codes of the extended set contain glyphs specific
for the language. For Western European languages, a well known character set is
“Latin-1”, which is standardized as ISO 8859-1 —the same set also goes by the name
“codepage 1252”, at least for Microsoft Windows. Codepages have been defined
for many languages; for example, ISO 8859-2 (“Latin-2”) has glyphs used in Central
and Eastern Europe, and ISO 8859-7 contains the Greek alphabet in the upper half
of the extendedASCIIset.

Unfortunately, codepage selection can be confusing, as vendors of operating sys-
tems typically created their own codepages irrespective of what already existed. As
a result, for most character sets there exist multiple incompatible codepages. For
example, codepage 1253 for Microsoft Windows also encodes the Greek alphabet,
but it is incompatible with ISO 8859-7. When writing texts in Greek, it now becomes
important to check what encoding is used, because many Microsoft Windows ap-
plications support both.

When the character set for a language exceeds 256 glyphs, a codepage does not
suffice. Traditionally, the codepage technique was extended by reserving special
“shift” codes in the base character set that switch to a new set of glyphs. The next
character then indicates the specific glyph. In effect, the glyph is now identified by
a 2-byte index. On the other hand, some characters (especially the 7-bitASCIIset)
can still be indicated by a single byte. The “Shift-JIS” standard, for the Japanese
character set, is an example for the variable length encoding.

Codepages become problematic when interchanging documents or data with peo-
ple in regions that use a different codepage, or when using different languages in
the same document. Codepages that use “shift” characters complicate the matter
further, because text processing must now take into account that a character may
take either one or two bytes. Scanning through a string from right to left may even
become impossible, as a byte may either indicate a glyph from the base set (“un-
shifted”) or it may be a glyph from a shifted set —in the latter case the preceding
byte indicates the shift set, but the meaning of the preceding character depends on
the character beforethat.

The ISO/IEC 10646 “Universal Character Set” (UCS) standard has the ambitious goal
to eventually include all characters used in all the written languages in the world,

```
 Codepage 1252 isnotexactly the same as Latin-1; Microsoft extended the standardized set to in-
clude glyphs at code positions that Latin-1 marks as “reserved”.
```

120 — Internationalization

using a 31-bit character set. This solves both of the problems related to codepages
and “shifted” character sets. However, the ISO/IEC body could not produce a stan-
dard in time, and therefore a consortium of mainly American software manufac-
turers started working in parallel on a simplified 16-bit character set called “Uni-
code”. The rationale behind Unicode was that it would encodeabstract characters,
notglyphs, and that therefore 65,536 would be sufficient.yIn practice, though, Uni-
codedoesencode glyphs and not long after it appeared, it became apparent that
65,536 code points would not be enough. To counter this, later Unicode versions
were extended with multiple “planes” and special codes that select a plane. The
combination of a plane selector and the code pointer inside that plane is called a
“surrogate pair”. The first 65,536 code points are in the “Basic Multilingual Plane”
(BMP) and characters in this set do not need a plane selector.

Essentially, the introduction of surrogate pairs in the Unicode standard is equiva-
lent to the shift codes of earlier character sets —and it carries some of the problems
that Unicode was intended to solve. The UCS-4 encoding by ISO/IEC 10646 doesnot
have/need surrogate pairs.

Support for Unicode/UCS-4 in applications and operating systems has emerged in
two different ways: either the internal representation of characters is multi-byte
(typically 16-bit, or 2-byte), or the application stores strings internally in UTF-8 for-
mat, and these strings are converted to the proper glyphs only when displaying or
printing them. Recent versions of Microsoft Windows use Unicode internally; The
Plan-9 operating system pioneered the UTF-8 encoding approach, which is now
widely used inUNIX/Linux. The advantage of UTF-8 encoding as an internal rep-
resentation is that it isphysicallyan 8-bit encoding, and therefore compatible with
nearly all existing databases, file formats and libraries. This circumvents the need
for double entry-points for functions that take string parameters —as is the case
in Microsoft Windows, where many functions exist in an “A”NSIand a “W”ide ver-
sion. A disadvantage of UTF-8 is that it is a variable length encoding, and many in-
memory string operations are therefore clumsy (and inefficient). That said, with
the appearance of surrogate pairs, Unicode has now also become a variable length
encoding.

The PAWNlanguage requires that symbols names are inASCII, and it allows non-ASCII
characters in strings. There are five ways that a host application could support non-
ASCIIcharacters in strings and character literals:

1 Support codepages: in this strategy the entire complexity of choosing the cor-
rect glyphs and fonts is delegated to the host application. The codepage support
is based on codepage mapping files with a file format of the “cross mapping ta-
bles” distributed by the Unicode consortium.

```
yIf Unicode encodescharacters, an “Unicode font” is a contradictio in terminis —because a font
encodes glyphs.
```

```
Working with tags — 121
```

2 Support Unicode or UCS-4 and let the PAWNcompiler convert scripts that were
written using a codepage to “wide” characters: for this strategy, you need to set
# pragma codepageor use the equivalent compiler option. The compiler will
only correctly translate characters inunpackedstrings.

3 Support Unicode or UCS-4 and let the PAWNcompiler convert scripts encoded
in UTF-8 to “wide” characters: when the source file for the PAWNcompiler is in
UTF-8 encoding, the compiler expands characters to Unicode/UCS-4 inunpacked
strings.

4 Support UTF-8 encoding internally (in the host application) and write the source
file in UTF-8 too: all strings should now bepackedstrings to avoid the compiler
to convert them.

For most internationalization strategies, as you can see, the host application needs
to support Unicode or UCS-4. As a side note, the PAWNcompiler doesnotgenerate
Unicode surrogate pairs. If characters outside the BMP are needed and the host
application (or operating system) does not support the full UCS-4 encoding, the host
application must split the 32-bit charactercellprovided by the PAWNcompiler into
a surrogate pair.

The PAWNcompiler accepts a source file as an UTF-8 encoded text file —seepage

```
Packed and un-
packed strings: 85
```

145. When the source file is in UTF-8 encoding, characters in anunpackedstring
are decoded to Unicode/UCS-4 characters; characters in apackedstring remain in
UTF-8 encoding. To write source files in UTF-8 encoding, you need, of course, a
(programmerʼs) editor that supports UTF-8. Codepage translation does not apply
for files that are in UTF-8 encoding.

For an occasional Unicode character in a literal string, an alternative is that you
Escape sequence:
use an escape sequence. As Unicode character tables are usually documented with 85
hexadecimal glyph indices, thenxhhh;sequence is probably the more convenient
specification of a random Unicode character. For example, the escape sequence
“\x2209” stands for the “̸2” character.

There is a lot more to internationalization than just basic support for extended
character sets, such as formatting date & time fields, reading order (left-to-right
or right-to-left) and locale-dependent translation of system messages. The PAWN
toolkit delegates these issues to the host application.

Working with tags

The tag name system was invented to add a “usage checking” mechanism to PAWN.
A tag denotes a “purpose” of a value or variable, and the PAWNcompiler issues a Tag names:^58
diagnostic message when the tag of an expression does not match the required tag
for the context of the expression.

```
122 — Working with tags
```

```
Many modern computer languages offer variabletypes, where a type specifies the
memory layout and the purpose of the variable. The programming language then
checks the type equivalence; thePASCALlanguage is very strict at checking type
equality, whereas the C programming language is more forgiving. The PAWNlan-
guage does not have types: all variables have the size and the layout of a cell, al-
though bit representations in the cell may depend on the purpose of the variable.
In summary:
⋄a type specifies thememory layoutand the range of variables and function results
⋄a tagname labels thepurposeof variables, constants and function results
```

Tags in PAWNare mostly optional. A program that was “fortified” with tag names on
User-defined opera-
tors: 73 the variable and constant declarations will function identically when all tag names
are removed. One exception is formed by user-defined operators: the PAWNcom-
piler uses the tags of the operands to choose between any user-defined operators
and the standard operator.

```
The snippet below declares three variables and does three assignments, two of
which give a “tag mismatch” diagnostic message:
LISTING: comparing apples to oranges
var apple:elstar /* variable "elstar" with tag "apple" */
var orange:valencia /* variable "valencia" with tag "orange" */
var x /* untagged variable "x" */
elstar = valencia /* tag mismatch */
elstar = x /* tag mismatch */
x = valencia /* ok */
```

The first assignment causes a “tag mismatch” diagnostic as it assigns an “orange”
More tag name
rules: 58 tagged variable to a variable with an “apple” tag. The second assignment puts the
untagged value ofxinto a tagged variable, which causes again a diagnostic. When
the untagged variable is on the left hand of the assignment operator, as in the third
assignment, there is no warning or error message. As variablexis untagged, it can
accept a value of any weak tag.

```
The same mechanism applies to passing variables or expressions to functions as
function operands —seepage 66for an example. In short, when a function expects
a particular tag name on an argument, you must pass an expression/variable with
a matching tag to that function; but if the function expects anuntagged argument,
you may pass in arguments withanyweak tag.
```

```
On occasion, it is necessary to temporarily change the tag of an expression. For
example, with the declarations of the previous code snippet, if you would wish to
compare apples with oranges (recent research indicates that comparing apples to
oranges is not as absurd than popular belief holds), you could use:
if (apple:valencia < elstar)
valencia = orange:elstar
```

```
Working with tags — 123
```

The test expression of theifstatement (between parentheses) compares the vari-
ablevalenciato the variableelstar. To avoid a “tag mismatch” diagnostic, it puts
a tag overrideapple: onvalencia—after that, the expressions on the left and
the right hands of the>operator have the same tag name: “apple:”.The second
lvalue (definition):
line, the assignment ofelstartovalencia, overrides the tag name ofelstaror 89
orange:before the assignment. In an assignment, you cannot override the tag
name of the destination; i.e., the left hand of the=operator. It is an error to write
“apple:valencia = elstar”. In the assignment,valenciais an “lvalue” and you
cannot override the tag name of an lvalue.

As shown earlier, when the left hand of an assignment holds an untagged variable,
the expression on the right hand may have any weak tag name. When used as an
lvalue, an untagged variable is compatible with all weak tag names. Or rather, a
weak tag is silently dropped when it is assigned to an untagged variable or when
it is passed to a function that expects an untagged argument. When a tag name
indicates the bit pattern of a cell, silently dropping a weak tag can hide errors. For
example, the snippet below has an error that is not immediately obvious:

LISTING: bad way of using tags
# pragma rational float
var limit = -5.0
var value = -1.0

if (value < limit)
printf "Value %f below limit %f\n", value, limit
else
printf "Value above limit\n"

Through the “#pragma rational”, all rational numbers receive the “float” tag
name and these numbers are encoded in the 4-byte IEEE 754 format. The snippet
declares two variables,limitandvalue, both of which areuntagged (this is the er-
ror). Although the literal values-5.0and-1.0are implicitly tagged withfloat:,
this weak tag is silently dropped when the values get assigned to the untagged sym-
bolslimitandvalue. Now, theifstatement comparesvaluetolimitas integers,
using the built-in standard<operator (a user-defined operator would be more ap-
propriate to compare two IEEE 754 encoded values). When run, this code snippet
tells us that “Value -1.000000 below limit -5.000000” —which is incorrect, of
course.

To avoid such subtle errors to go undetected, one should usestrongtags. A strong
tag is merely a tag name that starts with an upper case letter, such asFloat:in-
stead offloat:. A strong tag is never automatically “dropped”, but it may still be
explicitly overridden. Below is a modified code snippet with the proposed adapta-
tions:

LISTING: strong tags are safer
# pragma rational Float
var Float:limit = -5.0
var Float:value = -1.0

124 — Working with tags

if (value < limit)
printf "Value %f below limit %f\n", _:value,_:limit
else
printf "Value above limit\n"

Forgetting theFloat:tag name in the declaration of the variableslimitorvalue
gives a “tag mismatch” diagnostic, because the literal values-5.0and-1.0now
have a strong tag name.

printfis a general purpose function that can print strings and values in various for-
mats. To be general purpose,printfaccepts arguments with any weak tag name,
be itapple:ʼs,orange:ʼs, or something else. Theprintffunction does this by ac-
cepting untagged arguments —weak tags are dropped when an untagged argument
is expected. Strong tags, however, are never dropped, and in the above snippet
(which uses the original definition ofprintf), I needed to put an empty tag over-
ride, “_:”, before the variablesvalueandlimitin the firstprintfcall.

There is an alternative to untagging expressions with strong tag names in general
purpose functions: adjust the definition of the function to accept both all weak tags
and a selective set of strong tag names. The PAWNlanguage supports multiple tag
names for every function arguments. The original definition ofprintf(from the
fileCONSOLE.INC) is:

native printf(const format[], ...);

By adding both aFloat:tag and an empty tag in front of the ellipsis (“...”),printf
will accept arguments with theFloat:tag name, arguments without a tag name
and arguments that have a weak tag name. To specify plural tag names, enclose all
tag names without their final colon between braces with a comma separating the
tag names (see the example below). It is necessary to add the empty tag specifi-
cation to the list of tag names, becauseprintfwould otherwiseonlyaccept argu-
ments with aFloat:tag name. Below is the new definition of the functionprintf:

native printf(const format[], {Float, _}: ...);

Plural tags allow you to write a single function that accepts cells with a precisely
specified subset of tags (strong and/or weak). While a function argument may ac-
cept being passedactualarguments with diverse tags, a variable can only have a
single tag —and aformalfunction argument is a local variable in the body of the
function. In the presence of plural tags, the formal function argument takes on the
tag that is listed first.

On occasion, you may want to check which tag anactualfunction argument had,
when the argument accepts plural tags. Checking the tag of the formal argument
(in the body of the function) is of no avail, because it will always have the first tag
in the tag list in the declaration of the function argument. You can check the tag of
the actual argument by adding an extra argument to the function, and set its default
value to be the “tagof” of the argument in question. Similar to thesizeofoperator,

```
A program that generates its own source code — 125
```

thetagofoperator has a special meaning when it is applied in a default value of a
function argument: the expression is evaluated at the point of the functioncall,
instead of at the function definition. This means that the “default value” of the
function argument is the actual tag of the parameter passed to the function.

Inside the body of the function, you can compare the tag to known tags by, again,
using thetagofoperator.

Concatenating lines

PAWNis a free format language, but the parser directives must be on a single line.
Strings may not run over several lines either. When this is inconvenient, you can Directives:^100
use a backslash character (“\”) at the end of a line to “glue” that line with the next
line.

For example:

# define max_path max_drivename + max_directorystring + \
max_filename + max_extension

Another use of the concatenation character is to split long literal strings over mul-
tiple lines. Note that the “\” eats up all trailing white space that comes after it and
leading white space on the next line. The example below prints “Hello world” with
one space between the two words (because there is a space between ”Hello” and
the backslash):

print("Hello \
world")

An alternative way to concatenate literal strings is to separate strings, that are each
enclosed in pairs of double quotes, with an ellipsis. The next example is equivalent
to the previous one:

print("Hello " ...
"world")

A program that generates its own source code

An odd, rather academic, criterion to quantify the “expressiveness” of a program-
ming language is size of the smallest program that, upon execution, regenerates its
own source code. The contention behind this criterion is that the shorter the self-
generating program, the more flexible and expressive the language must be. Pro-
grams of this kind have been made for many programming languages —sometimes
surprisingly small, as for languages that have a built-in reflective capabilities.

Self-generating programs are called “quines”, in honour of the philosopher Willard
Van Orman Quine who wrote self-creating phrases in natural language. The work

126 — A program that generates its own source code

of Van Orman Quine became well known through the books “Gödel, Escher, Bach”
and “Metamagical Themas” by Douglas Hofstadter.

The PAWNquine is in the example below; it is modelled after the famous “C” quine
(of which many variations exist). At 77 characters, it is amongst the smallest ver-
sions for the class of imperative programming languages, and the size can be re-
duced to 73 characters by removing four “space” characters that were left in for
readability.

LISTING: quine.p
var s{}="var s{}=%c%s%c; main() printf s,34,s,34"; main() printf s,34,s,34

```
Error and warning messages — 127
```

Error and warning messages

APPENDIXA

When the compiler finds an error in a file, it outputs a message giving, in this order:
⋄the name of the file
⋄the line number were the compiler detected the error between parentheses, di-
rectly behind the filename
⋄the error class (“error”, “fatal error” or “warning”)
⋄an error number
⋄a descriptive error message

For example:
demo.p(3) : error 001: expected token: ";", but found "{"

Note: the line number given by the compiler may specify a position behind the
actual error, since the compiler cannot always establish an error before having an-
alyzed the complete expression.

After termination, the return code of the compiler is:
0 no errors —there may be warnings, though
1 errors found
2 reserved
3 aborted by user

These return codes may be checked within batch processors or “make” utilities.

Error categories

Errors are separated into three classes:

Errors Describe situations where the compiler is unable to generate correct
code. Errors messages are numbered from 1 to 99.

Fatal errors Fatal errors describe errors from which the compiler cannot recover.
Parsing is aborted. Fatal error messages are numbered from 100 to
199.

Warnings Warnings are displayed for syntaxes that are technically correct, but
may not be what is intended. Warning messages are numbered from
200 to 299.

Errors

001 **expected token:** token **, but found** token
A required token is omitted.

```
128 — Error and warning messages
```

002 **only a single statement (or expression) can follow each “case”**
Pitfalls: 115
Compound state-
ment: 96

Every case in a switch statement can hold exactly one statement. To put
multiple statements in a case, enclose these statements between braces
(which creates a compound statement).
003 **declaration of a local variable must appear in a compound block**
Compound state-
ment: 96 The declaration of a local variable must appear between braces (“{:::}”)
at the active scope level.
When the parser flags this error, a variable declaration appears asthe only
statementof a function orthe only statementbelow anif,else,for,while
ordostatement. Note that, since local variables are accessible only from
(or below) the scope that their declaration appears in, having a variable
declarationas the only statementat any scope is useless.
004 **function** name **is not implemented**
Forward declara-
tion: 70 There is no implementation for the designated function. The func-
tion may have been “forwardly” declared —or prototyped— but the full
function definition including a statement, or statement block, is missing.
005 **function may not have arguments**
The program entry point (function@startormain) may not have argu-
ments.

```
006 must be assigned to an array
String literals or arrays must be assigned to an array. This error message
may also indicate a missing index (or indices) at the array on the right
side of the “=” sign.
```

```
007 operator cannot be redefined
Only a select set of operators may be redefined, this operator is not one
of them. Seepage 73for details.
008 must be a constant expression; assumed zero
The size of arrays and the parameters of most directives must be constant
values.
009 invalid array size (negative, zero or out of bounds)
The number of elements of an array must always be 1 or more. In
addition, an array that big that it does exceeds the range of a cell is invalid
too.
010 illegal function or declaration
The compiler expects a declaration of a global variable or of a function at
the current location, but it cannot interpret it as such.
011 invalid outside functions
The instruction or statement is invalid at a global level. Local labels and
(compound) statements are only valid if used within functions.
```

```
Error and warning messages — 129
```

012 **invalid function call, not a valid address**
The symbol is not a function.

013 **no entry point (no public functions)**
The file does not contain amainfunction or any public function. The
compiled file thereby does not have a starting point for the execution.

014 **invalid statement; not in switch**
The statementscaseanddefaultare only valid inside aswitchstate-
ment.

015 **“default” must be the last clause in switch statement**
PAWNrequires thedefaultclause to be the last clause in aswitch
statement.

016 **multiple defaults in “switch”**
Eachswitchstatement may only have onedefaultclause.

017 **undefined symbol** symbol
The symbol (variable, constant or function) is not declared.

018 **initialization data exceeds declared size** Initialization: 54
An array with an explicit size is initialized, but the number of ini-
tiallers exceeds the number of elements specified. For example, in
“arr[3]={1,2,3,4};” the array is specified to have three elements, but
there are four initiallers.

019 **not a label:** name
Agotostatement branches to a symbol that is not a label.

020 **invalid symbol name**
Symbol name syn-
A symbol may start with a letter, an underscore or an “at” sign (“@”) and tax: 83
may be followed by a series of letters, digits, underscore characters and
“@” characters.

021 **symbol already defined:** identifier
The symbol was already defined at the current level.

022 **must be lvalue (non-constant)**
The symbol that is altered (incremented, decremented, assigned a value,
etc.) must be a variable that can be modified (this kind of variable is called
an lvalue). Functions, string literals, arrays and constants are no lvalues.
Variables declared with the “const” attribute are no lvalues either.

023 **array assignment must be simple assignment**
When assigning one array to another, you cannot combine an arithmetic
operation with the assignment (e.g., you cannot use the “+=” operator).

```
130 — Error and warning messages
```

```
024 “break” or “continue” is out of context
The statementsbreakandcontinueare only valid inside the context of
a loop (ado,fororwhilestatement). Unlike the languages C/C++and
Java,breakdoes not jump out of aswitchstatement.
```

```
025 function heading differs from prototype
The number of arguments given at a previous declaration of the function
does not match the number of arguments given at the current declaration.
```

```
026 no matching “#if...”
The directive#elseor#endifwas encountered, but no matching#if
directive was found.
```

Escape sequence:^027 **invalid character constant**
85 One likely cause for this error is the occurrence of an unknown escape
sequence, like “\x”. Putting multiple characters between single quotes,
as in'abc'also issues this error message. A third cause for this error
is a situation where a character constant was expected, but none (or a
non-character expression) were provided.

```
028 invalid subscript (not an array or too many subscripts): identifier
The subscript operators “[” and “]” are only valid with arrays. The number
of square bracket pairs may not exceed the number of dimensions of the
array.
```

```
029 invalid expression, assumed zero
The compiler could not interpret the expression.
```

```
030 compound statement not closed at the end of file (started at line number )
An unexpected end of file occurred. One or more compound statements
are still unfinished (i.e. the closing brace “}” has not been found). The line
number where the compound statement started is given in the message.
```

```
031 unknown directive
The character “#” appears first at a line, but no valid directive was
specified.
```

```
032 array index out of bounds
The array index is larger than the highest valid entry of the array.
```

```
033 array must be indexed (variable name )
An array as a whole cannot be used in a expression; you must indicate an
element of the array between square brackets.
```

```
034 argument does not have a default value (argument index )
You can only use the argument placeholder when the function definition
specifies a default value for the argument.
```

```
Error and warning messages — 131
```

035 **argument type mismatch (argument** index **)**
The argument that you pass is different from the argument that the func-
tion expects, and the compiler cannot convert the passed-in argument to
the required type. For example, you cannot pass the literal value “1” as
an argument when the function expects an array or a reference.

036 **empty statement**
Empty compound
The line contains a semicolon that is not preceded by an expression. block: 96
PAWNdoes not support a semicolon as an empty statement, use an empty
compound block instead.

037 **invalid string (possibly non-terminated string)**
A string was not well-formed; for example, the final quote that ends a
string is missing, or the filename for the#includedirective was not
enclosed in double quotes or angle brackets.

038 **extra characters on line**
There were trailing characters on a line that contained a directive (a
directive starts with a#symbol, seepage 100).

039 **constant symbol has no size**
A variable has a size (measured in a number of cells), a constant has
no size. That is, you cannot use a (symbolic) constant with thesizeof
operator, for example.

040 **duplicate “case” label (value** value **)**
A preceding “case label” in the list of theswitchstatement evaluates to
the same value.

041 **invalid ellipsis, array size is not known**
You used a syntax like “arr[] = { 1, ... };”, which is invalid, because
the compiler cannot deduce the size of the array from the declaration.

042 **invalid combination of class specifiers**
A function or variable is denoted as both “public” and “native”, which is
unsupported. Other combinations may also be unsupported; for example,
a function cannot be both “public” and “stock” (avariablemay be declared
both “public” and “stock”).

043 **character constant** value **exceeds range for a packed string/array**
When the error occurs on a literal string, it is usually an attempt to store
a Unicode character in a packed string where a packed character is 8-bits.
For a literal array, one of the constants does not fit in the range for packed
characters.

044 **positional parameters must precede all named parameters**
When you mix positional parameters and named parameters in a function
call, the positional parameters must come first.

```
132 — Error and warning messages
```

045 **too many function arguments**
The maximum number of function arguments is currently limited to 64.
046 **unknown array size (variable** name **)**
For array assignment, the size of both arrays must be explicitly defined,
also if they are passed as function arguments.
047 **array sizes do not match, or destination array is too small**
For array assignment, the arrays on the left and the right side of the
assignment operator must have the same number of dimensions. In
addition:
⋄for multi-dimensional arrays, both arrays must have the same size
—note that anunpackedarray does not fit in apackedarray with the
same number of elements;
⋄for single arrays with a single dimension, the array on the left side of
the assignment operator must have a size that is equal or bigger than
the one on the right side.
When passing arrays to a function argument, these rules also hold for the
array that is passed to the function (in the function call) versus the array
declared in the function definition.
When a function returns an array, allreturnstatements must specify an
array with the same size and dimensions.
048 **array dimensions do not match**
For an array assignment, the dimensions of the arrays on both sides of
the “=” sign must match; when passing arrays to a function argument, the
arrays passed to the function (in the function call) must match with the
definition of the function arguments.
When a function returns an array, allreturnstatements must specify an
array with the same size and dimensions.
049 **invalid line continuation**
Single line com-
ment: 83 A line continuation character (a backslash at the end of a line) is at
an invalid position, for example at the end of a file or in a single line
comment.
050 **invalid range**
A numeric range with the syntax “n1..n2”, wheren1andn2are numeric
constants, is invalid. Either one of the values in not a valid number, orn1
is not smaller thann2.
051 **invalid subscript, use “[ ]” operators on major dimensions and for
named indices**
You can use the “character array index” operator (braces: “{ }” only for
the last dimension, and only when indexing the array with a number. For
other dimensions, and when indexing the array with a “symbolic index”

```
Error and warning messages — 133
```

```
(one that starts with a “.”), you must use the cell index operator (square
brackets: “[ ]”).
```

052 **multi-dimensional arrays must be fully initialized**
If an array with more than one dimension is initialized at its declaration,
then there must be equally many literal vectors/sub-arrays at the right of
the equal sign (“=”) as specified for the major dimension(s) of the array.

053 **exceeding maximum number of dimensions**
The current implementation of the PAWNcompiler only supports arrays
with one or two dimensions.

054 **unmatched closing brace**
A closing brace (“}”) was found without matching opening brace (“{”).

055 **start of function body without function header**
An opening brace (“{”) was found outside the scope of a function. This
may be caused by a semicolon at the end of a preceding function header.

056 **arrays, local variables and function arguments cannot be public**
A local variable or a function argument starts with the character “@”,
which is invalid.

057 **Unfinished expression before compiler directive**
Compiler directives may only occurbetweenstatements, notinsidea
statement. This error typically occurs when an expression statement is
split over multiple lines and a compiler directive appears between the
start and the end of the expression. This is not supported.

058 **duplicate argument; same argument is passed twice**
Named versus posi-
tional parameters:
62

```
In the function call, the same argument appears twice, possibly through
a mixture of named and positional parameters.
```

059 **function argument may not have a default value (variable** name **)**
All arguments ofpublic functionsmust be passed explicitly. Public func-
tions are typically called from the host application, who has no knowledge
of the default parameter values. Arguments ofuser defined operatorsare
implied from the expression and cannot be inferred from the default
value of an argument.

060 **multiple “#else” directives between “#if** ::: **#endif**
Two or more#elsedirectives appear in the body between the matching
# ifand#endif.

061 **“#elseif” directive follows an “#else” directive**
All#elseifdirectives must appearbeforethe#elsedirective. This error
may also indicate that an#endifdirective for a higher level is missing.

```
134 — Error and warning messages
```

```
062 number of operands does not fit the operator
When redefining an operator, the number of operands that the operator
has (1 for unary operators and 2 for binary operators) must be equal to
the number of arguments of the operator function.
```

```
063 function result tag of operator name must be name
Logical and relational operators are defined as having a result that is
eithertrue(1) orfalse(0) and having a “bool:” tag. A user defined
operator should adhere to this definition.
```

```
064 cannot change predefined operators
One cannot define operators to work on untagged values, for example,
because PAWNalready defines this operation.
```

```
065 function argument may only have a single tag (argument number )
In a user defined operator, a function argument may not have multiple
tags.
```

```
066 function argument may not be a reference argument or an array
(argument number )
In a user defined operator, all arguments must be cells (non-arrays) that
are passed “by value”.
```

```
067 variable cannot be both a reference and an array (variable name )
A function argument may be denoted as a “reference” or as an array, but
not as both.
```

```
068 invalid rational number precision in #pragma
The precision was negative or too high. For floating point rational
numbers, the precision specification should be omitted.
```

```
069 rational number format already defined
This#pragmaconflicts with an earlier#pragmathat specified a different
format.
```

# pragma rational:^070 **rational number support was not enabled**
104 A rational literal number was encountered, but the format for rational
numbers was not specified.

071 **user-defined operator must be declared before use (function** name **)**
Forward declara-
tion: 70 Like a variable, a user-defined operator must be declared before its first
use. This message indicates that prior to the declaration of the user-
defined operator, an instance where the operator was used on operands
with the same tags occurred. This may either indicate that the program
tries to make mixed use of the default operator and a user-defined
operator (which is unsupported), or that the user-defined operator must
be “forwardly declared”.

```
Error and warning messages — 135
```

072 **“sizeof” operator is invalid on “function” symbols**
You used something like “sizeof MyCounter” where the symbol “My-
Counter” is not a variable, but a function. You cannot request the size of
a function.

073 **function argument must be an array (argument** name **)**
The function argument is a constant or a simple variable, but the function
requires that you pass an array.

074 **#define pattern must start with an alphabetic character**
Any pattern for the#definedirective must start with a letter, an under-
score (“_”) or an “@”-character. The pattern is the first word that follows
the#definekeyword.

075 **input line too long (after substitutions)**
Either the source file contains a very long line, or text substitutions make
a line that was initially of acceptable length grow beyond its bounds. This
may be caused by a text substitution that causes recursive substitution
(the pattern matching a portion of the replacement text, so that this part
of the replacement text is also matched and replaced, and so forth).

076 **syntax error in the expression, or invalid function call**
The expression statement was not recognized as a valid statement (so it
is a “syntax error”). From the part of the string that was parsed, it looks
as if the source line contains a function call in a “procedure call” syntax
(omitting the parentheses), but the function result is used —assigned to a
variable, passed as a parameter, used in an expression:::

077 **malformed UTF-8 encoding, or corrupted file:** filename
The file starts with an UTF-8 signature, but it contains encodings that are
invalid UTF-8. If the source file was created by an editor or converter that
supports UTF-8, the UTF-8 support is non-conforming.

078 **function uses both “return” and “return** < **value** > **”**
The function returns bothwithandwithouta return value. The function
should be consistent in always returning with a function result, or in
never returning a function result.

079 **inconsistent return types (array & non-array)**
The function returns both values and arrays, which is not allowed. If a
function returns an array, allreturnstatements must specify an array (of
the same size and dimensions).

080 **unknown symbol, or not a constant symbol (symbol** name **)**
Where a constant value was expected, an unknown symbol or a non-
constant symbol (variable) was found.

082 **user-defined operators and native functions may not have states**
Only standard and public functions may have states.

```
136 — Error and warning messages
```

```
083 a function or variable may only belong to a single automaton (symbol
name )
There are multiple automatons in the state declaration for the indicated
function or variable, which is not supported. In the case of a function:all
instances of the function must belong to the same automaton. In the case
of a variable: it is allowed to have several variables with the same name
belonging to different automatons, but only in separate declarations
—these are distinct variables.
```

```
084 state conflict: one of the states is already assigned to another imple-
```

State specifiers: (^70) **mentation (symbol** name **)**
The specified state appears in the state specifier of two implementations
of the same function.
085 **no states are defined for symbol** name
Fall-back: (^70) When this error occurs on a function, this function has a fall-back
implementation, but no other states. If the error refers to a variable, this
variable does not have a list of states between the<and>characters. Use
a state-less function or variable instead.
086 **unknown automaton** name
The “state” statement refers to an unknown automaton.
087 **unknown state** name **for automaton** name
The “state” statement refers to an unknown state (for the specified
automaton).
088 **public variables and local variables may not have states (symbol** name **)**
Only standard (global) variables may have a list of states (and an automa-
ton) at the end of a declaration.
089 **state variables may not be initialized (symbol** name **)**
Variables with a state list may not have initializers. State variables
should always be initialized through an assignment (instead of at their
declaration), because their initial value is indeterminate.
090 **public functions may not return arrays (symbol** name **)**
A public function may not return an array. Returning arrays is allowed
only for normal functions.
091 **first constant in an enumerated list must be initialized (symbol** name **)**
Enumerated con-
stants: 58 The first constant in a list of enumerated symbolic constants must be set
to a value. Any subsequent symbol is automatically set the the value of
the preceding symbol+1.
092 **invalid number format**
A symbol started with a digit, but is is not a valid number.

```
Error and warning messages — 137
```

093 **array fields with a size may only appear in the final dimension**
In the final dimension (the “minor” dimension), the fields of an array
may optionally be declared with a size that is different from a single cell.
On the major dimensions of an array, this is not valid, however.

094 **invalid subscript, subscript does not match array definition regarding**
Symbolic subscripts:
**named indices (symbol** name **)** 55
Either the array was declared with symbolic subscripts and you are
indexing it with an expression, or you are indexing the array with a
symbolic subscript which is not defined for the array.

Fatal Errors

100 **cannot read from file:** filename
The compiler cannot find the specified file or does not have access to it.

101 **cannot write to file:** filename
The compiler cannot write to the specified output file, probably caused
by insufficient disk space or restricted access rights (the file could be
read-only, for example).

102 **table overflow:** table name
An internal table in the PAWNparser is too small to hold the required
data. Some tables are dynamically growable, which means that there was
insufficient memory to resize the table. The “table name” is one of the
following:
“staging buffer”: the staging buffer holds the code generated for an
expression before it is passed to the peephole optimizer. The staging
buffer grows dynamically, so an overflow of the staging buffer basically is
an “out of memory” error.
“loop table”: the loop table is a stack used with nesteddo,for, andwhile
statements. The table allows nesting of these statements up to 24 levels.
“literal table”: this table keeps the literal constants (numbers, strings)
that are used in expressions and as initiallers for arrays. The literal table
grows dynamically, so an overflow of the literal table basically is an “out
of memory” error.
“compiler stack”: the compiler uses a stack to store temporary informa-
tion it needs while parsing. An overflow of this stack is probably caused
by deeply nested (or recursive) file inclusion. The compiler stack grows
dynamically, so an overflow of the compiler stack basically is an “out of
memory” error.
“option table”: in case that there are more options on the command line
or in the response file than the compiler can cope with.

```
138 — Error and warning messages
```

```
103 insufficient memory
General “out of memory” error.
```

```
104 incompatible options: option versus option
Two option that are passed to the PAWNcompiler conflict with each other,
or an option conflicts with the configuration of the PAWNcompiler.
```

```
105 numeric overflow, exceeding capacity
A numeric constant, notably a dimension of an array, is too large for the
compiler to handle. For example, when compiled as a 16-bit application,
the compiler cannot handle arrays with more than 32767 elements.
```

106 **compiled script exceeds the maximum memory size (** number **bytes)**
See also #pragma
amxlimit onpage
102

```
The memory size for the abstract machine that is needed to run the script
exceeds the value set with#pragma amxlimit. This means that the script
is too large to be supported by the host.
```

```
You might try reducing the scriptʼs memory requirements by:
⋄setting a smaller stack/heap area —see#pragma dynamicatpage 103;
⋄using packed strings instead of unpacked strings —see pages 85 and
117 ;
⋄using overlays —see pages 104 andpage 146for more information on
overlays.
⋄putting repeated code in separate functions;
⋄putting repeated data (strings) in global variables;
⋄trying to find more compact algorithms to perform the same task.
```

```
107 too many error/warning messages on one line
A single line that causes several error/warning messages is often an
indication that the PAWNparser is unable to “recover” from an earlier
error. In this situation, the parser is unlikely to make any sense of the
source code that follows —producing only (more) inappropriate error
messages. Therefore, compilation is halted.
```

108 **codepage mapping file not found**
# pragma codepage:
102 The file for the codepage translation that was specified with the-c
compiler option or the#pragma codepagedirective could not be loaded.

109 **invalid path:** path name
Configuration file:
149 A path, for example for include files or codepage files, is invalid. Check
the compiler options and, if used, the configuration file.

# assert directive:^110 **assertion failed:** expression

(^100) Compile-time assertion failed.
# error directive: 100 111 **user error:** message
The parser fell on an#errordirective.

```
Error and warning messages — 139
```

112 **overlay function** name **exceeds limit by** value **bytes**
The size of a function is too large for the overlay system. To fix this issue,
you will have to split the function into two (or more) functions.

Warnings

200 **symbol is truncated to** number **characters**
The symbol is longer than the maximum symbol length. The maximum
length of a symbol depends on whether the symbol is native, public or
neither. Truncation may cause different symbol names to become equal,
which may cause error 021 or warning 219.

201 **redefinition of constant/macro (symbol** name **)**
The symbol was previously defined to a different value, or the text
substitution macro that starts with the prefixnamewas redefined with a
different substitution text.

202 **number of arguments does not match definition**
At a function call, the number of arguments passed to the function (actual
arguments) differs from the number of formal arguments declared in the
function heading. To declare functions with variable argument lists, use
an ellipsis (...) behind the last known argument in the function heading;
for example:print(formatstring,...);(seepage 67).

203 **symbol is never used:** identifier
A symbol is defined but never used. Public functions are excluded from
the symbol usage check (since these may be called from the outside).

204 **symbol is assigned a value that is never used:** identifier
A value is assigned to a symbol, but the contents of the symbol are never
accessed.

205 **redundant code: constant expression is zero**
Where a conditional expression was expected, a constant expression
with the value zero was found, e.g. “while (0)” or “if (0)”. The the
conditional code below the test isneverexecuted, and it is therefore
redundant.

206 **redundant test: constant expression is non-zero**
Where a conditional expression was expected, a constant expression with
a non-zero value was found, e.g.if (1). The test is redundant, because
the conditional code isalwaysexecuted.
To create an endless loop, usefor ( ;; )instead ofwhile (1).

207 **unknown “#pragma”**
The compiler ignores the pragma. The#pragmadirectives may change
between compilers of different vendors and between different versions of
a compiler of the same version.

```
140 — Error and warning messages
```

208 **function with tag result used before definition, forcing reparse**
User-defined opera-
tors: 73
Forward declara-
tion: 70

```
When a function is “used” (invoked) before being declared, and that
function returns a value with a tag name, the parser must make an
extra pass over the source code, because the presence of the tag name
may change the interpretation of operators (in the presence of user-
defined operators). You can speed up the parsing/compilation process by
declaring the relevant functions before using them.
209 function should return a value
The function does not have areturnstatement, or it does not have an
expression behind thereturnstatement, but the functionʼs result is used
in a expression.
210 possible use of symbol before initialization: identifier
A local (uninitialized) variable appears to be read before a value is
assigned to it. The compiler cannot determine the actual order of reading
from and storing into variables and bases its assumption of the execution
order on the physical appearance order of statements an expressions in
the source file.
211 possibly unintended assignment
Where a conditional expression was expected, the assignment operator
(=) was found instead of the equality operator (==). As this is a frequent
mistake, the compiler issues a warning. To avoid this message, put
parentheses around the expression, e.g.if ( (a=2) ).
212 possibly unintended bitwise operation
Where a conditional expression was expected, a bitwise operator (&or|)
was found instead of a Boolean operator (&&or||). In situations where
a bitwise operation seems unlikely, the compiler issues this warning. To
avoid this message, put parentheses around the expression.
```

213 **tag mismatch**
Tags are discussed
on page 58 A tag mismatch occurs when:
⋄assigning to a tagged variable a value that is untagged or that has a
different tag
⋄the expressions on either side of a binary operator have different tags
⋄in a function call, passing an argument that is untagged or that has a
different tag than what the function argument was defined with
⋄indexing an array which requires a tagged index with no tag or a wrong
tag name
214 **possibly a “const” array argument was intended:** identifier
Arrays are always passed by reference. If a function does not modify the
array argument, however, the compiler can sometimes generate more
compact and quicker code if the array argument is specifically marked as
“const”.

```
Error and warning messages — 141
```

215 **expression has no effect**
The result of the expression is apparently not stored in a variable or used
in a test. The expression or expression statement is therefore redundant.

216 **nested comment**
PAWNdoes not support nested comments.

217 **loose indentation**
Statements at the same logical level do not start in the same column; that
is, the indents of the statements are different. Although PAWNis a free
format language, loose indentation frequently hides a logical error in the
control flow.

```
The compiler can also incorrectly assume loose indentation if theTAB
size with which you indented the source code differs from the assumed
size. This may happen if the source files use a mixture ofTABand space
characters to indent lines. Sometimes it is then needed to tell the PAWN
parser whatTABsize to use, see#pragma tabsizeonpage 104or the
compiler option-tonpage 146.
```

```
You can also disable this warning with#pragma tabsize 0or the compiler
option-t:0.
```

218 **old style prototypes used with optional semicolon**
forward declaration:
When using “optional semicolons”, it is preferred to explicitly declare 70
forward functions with theforwardkeyword than using terminating
semicolon.

219 **local variable** identifier **shadows a symbol at a preceding level**
A local variable has the same name as a global variable, a function, a
function argument, or a local variable at a lower precedence level. This
is called “shadowing”, as the new local variable makes the previously
defined function or variable inaccessible.

```
Note: if there are also error messages further on in the script about
missing variables (with these same names) or brace level problems, it
could well be that the shadowing warnings are due to these syntactical
and semantical errors. Fix the errors first before looking at the shadowing
warnings.
```

220 **expression with tag override must appear between parentheses**
In acasestatement and in expressions in the conditional operator (“?
: ”), any expression that has a tag override should be enclosed between
parentheses, to avoid the colon to be misinterpreted as a separator of the
casestatement or as part of the conditional operator.

```
142 — Error and warning messages
```

```
221 label name identifier shadows tag name
A code label (for thegotoinstruction) has the same name as a previously
defined tag. This may indicate a faultily applied tag override; a typical
case is an attempt to apply a tag override on the variable on the left of the
=operator in an assignment statement.
```

```
222 number of digits exceeds rational number precision
A literal rational number has more decimals in its fractional part than
the precision of a rational number supports. The remaining decimals are
ignored.
```

```
223 redundant “sizeof”: argument size is always 1 (symbol name )
A function argument has a as its default value the size of another argument
of the same function. The “sizeof” default value is only useful when the
size of the referred argument is unspecified in the declaration of the
function; i.e., if the referred argument is an array.
```

224 **indeterminate array size in “sizeof” expression (symbol** name **)**
# if:::#else:::
# endif: 100 The operand of thesizeofoperator is an array with an unspecified size.
That is, the size of the variable cannot be determined at compile time. If
used in an “if” instruction, consider a conditionally compiled section,
replacingifby#if.

```
225 unreachable code
The indicated code will never run, because an instruction before (above)
it causes a jump out of the function, out of a loop or elsewhere. Look
forreturn,break,continueandgotoinstructions above the indicated
line. Unreachable code can also be caused by an endless loop above the
indicated line.
```

```
226 a variable is assigned to itself (symbol name )
There is a statement like “x = x” in the code. The parser checks for self
assignmentsafterperforming any text and constant substitutions, so the
left and right sides of an assignment may appear to be different at first
sight. For example, if the symbol “TWO” is a constant with the value 2, then
“var[TWO] = var[2]” is also a self-assignment.
```

```
Self-assignments are, of course, redundant, and they may hide an error
(assignment to the wrong variable, error in declaring constants).
```

```
Note that the PAWNparser is limited to performing “static checks” only.
In this case it means that it can only compare array assignments for
self-assignment with constant array indices.
```

```
227 more initiallers than array fields
An array that is declared with sumbolic subscripts contains more values/
fields as initiallers than there are (symbolic) subscripts.
```

```
Error and warning messages — 143
```

228 **length of initialler exceeds size of the array field**
The initialler for an array element contains more values than the size of
that field allows. This occurs in an array that has symbolic subscripts,
and where a particular subscript is declared with a size.

229 **mixing packed and unpacked array indexing or array assignment**
An array is declared as packed (with{and}braces) but indexed as
unpacked (with[and]), or vice versa. Or one array is assigned to another
and one is packed while the other is unpacked.

230 **no implementation for state** name **in function** name **, no fall-back**
A function is lacking an implementation for the indicated state. The
compiler cannot (statically) check whether the function will ever be
called in that state, and therefore it issues this warning. When the
function would be called for the state for which no implementation
exists, the abstract machine aborts with a run time error.

```
Seepage 70on how to specify a fall-back function, andpage 36for a
description and an example.
```

231 **state specification on forward declaration is ignored**
A state specification is redundant on forward declarations. The function State specifiers:^70
signature must be equal for all states. Only the implementations of the
function are state-specific.

232 **native function lacks a predefined index (symbol** name **)**
See alsopage 147
for compiler option
“-N” that triggers
this warning.

```
The PAWNcompiler was configured with predefined indices for native
functions, but it encountered a declaration for which it does not have an
index declaration. This usually means that the script uses include files
that are not appropriate for the active configuration.
```

233 **state variable** name **shadows a global variable**
The state variable has the same name as a global variable (without state
specifiers). This means that the global variable is inaccessible for a
function with one of the same states as those of the variable.

234 **function is deprecated (symbol** name **)**
The script uses a function which as marked as “deprecated”. The host
application can mark (native) functions as deprecated when better al-
ternatives for the function are available or if the function may not be
supported in future versions of the host application.

235 **public function lacks forward declaration (symbol** name **)**
The script defines a public function, but no forward declaration of this
function is present. Possibly the function name was written incorrectly.
The requirement for forward declarations of public functions guards
against a common error.

144 — Error and warning messages

236 **unknown parameter in substitution (incorrect #define pattern)**
A#definepattern contains a parameter in the replacement (e.g. “%1”),
that is not in the match pattern. Seepage 79for the preprocessor syntax.

237 **recursive function** name
The specified function calls itself recursively. Although this is valid in
PAWN, a self-call is often an error. Note that this warning is only generated
when the PAWNparser/compiler is set to “verbose” mode.

238 **mixing string formats in concatenation**
In concatenating literals strings, strings with different formats (such
as packed versus unpacked, and “plain” versus standard strings) were
combined. This is usually an error. The parser uses the format of the first
(left-most) string in the concatenation for the result.

```
The compiler — 145
```

The compiler

APPENDIXB

Many applications that embed the PAWNscripting language use the stand-alone
compiler that comes with the PAWNtoolkit. The PAWNcompiler is a command-line
utility, meaning that you must run it from a “console window”, a terminal/shell, or
a “DOS box” (depending on how your operating system calls it).

Usage

The command-line PAWNcompiler is usually called “pawncc” (or “pawncc.exe” in
Microsoft Windows). The command line syntax is:
pawncc <filename> [more filenames...] [options]

The input file name is any legal filename. If no extension is given, “.pawn” or “.p”
is assumed. The compiler creates an output file with, by default, the same name as
the input file and the extension “.amx”.

After switching to the directory with the sample programs, the command:
pawncc hello
should compile the very first “hello world” example (page 3).Should, because the
command implies that:
⋄the operating system can locate the “pawncc” program —you may need to add it
to the search path;
⋄the PAWNcompiler is able to determine its own location in the file system so that
it can locate the include files —a few operating systems do not support this and
require that you use the-ioption (see below).

Input file

The input file for the PAWNcompiler, the “source code” file for the script/program,
must be a plain text file. All reserved words and all symbol names (names for vari-
ables, functions, symbolic constants, tags,:::) must use theASCIIcharacter set.
Literal strings, i.e text between quotes, may be in extendedASCII, such as one of the
sets standardized in the ISO 8859 norm —ISO 8859-1 is the well known “Latin 1” set.

The PAWNcompiler also supports UTF-8 encoded text files, which are practical in

Packed/unpacked
strings: 85
Character constants:
85
an environment based on Unicode or UCS-4. The PAWNcompiler only recognizes
UTF-8 encoded characters insideunpackedstrings and character constants. The
compiler interprets the syntax rules for UTF-8 files strictly; non-conforming UTF-
8 files are not recognized. The input file may have, but does not require, a “Byte
Order Mark” signature; the compiler recognizes the UTF-8 format based on the fileʼs
content.

146 — The compiler

Options

Options start with a dash (“-”) or, on Microsoft Windows and DOS, with a forward
slash (“/”). In other words, all platforms accept an option written as “-a” (see below
for the purpose of this option) and the DOS/Windows platforms accept “/a” as an
alternative way to write “-a”.

All options should be separated by at least one space.

Many options accept a value —which is sometimes mandatory. A value may be
separated from the option letter by a colon or an equal sign (a “:” and a “=” respec-
tively), or the value may be glued to the option letter. Three equivalent options to
set the debug level to two are thus:
⋄-d2

⋄-d:2
⋄-d=2

The options are:

-Avalue Alignment: the memory address of global and local variables may
optionally be aligned to multiples of the given value. If not set, all
variables are aligned to cell boundaries. The optimal data alignment
depends on the hardware architecture.

-a Assembler: generate a text file with the pseudo-assembler code for
the PAWNabstract machine, instead of binary code.

-Cvalue The size of a cell in bits; valid values are 16, 32 and 64.

-cname Codepage: for translating the source file from extendedASCIIto Uni-
code/UCS-4. The default isno translation. Thenameparameter can
specify a full path to a “mapping file” or just the identifier of the
codepage —in the latter case, the compiler prefixes the identifier
with the letters “cp”, appends the extension “.txt” and loads the
mapping file from a system directory.

-Dpath Directory: the “active” directory, where the compiler should search
for its input files and store its output files.

```
This option is not supported on every platform. To verify whether
the PAWNcompiler supports this option, run the compiler without
any option or filename on the command line. The compiler will then
list its usage syntax and all available options in alphabetical order. If
the-Dswitch is absent, the option is not available.
```

-dlevel Debug level: 0 = none, 1 = bounds checking and assertions only, 2 =
full symbolic information, 3 = full symbolic information and disable
optimizations (same as the combination-d2and-O0).

```
The compiler — 147
```

```
When the debug level is 2 or 3, the PAWNcompiler also prints the
estimated number of cells for the stack/heap space that is required
to run the program.
```

-efilename Error file: set the name of the file into which the compiler must write
any warning and error messages; when set, there is no output to the
screen.

-ipathname Include path: set the path where the compiler can find the include
files. This option may appear multiple times at the command line,
to allow you to set several include paths.

-kkey Set the key for KeeLoq encryption of the binary output file. The key
must be a sequence of up to 16 hexadecimal digits. See the Imple-
menterʼs Guide for details.

-l Listing: perform only the file reading and preprocessing steps; for
example, to verify the effects of macro expansion and the condition-
ally compiled or skipped sections.

-Nname=number
Declare a predefined (fixed) index for a native function. The ap-
pendix “Running scripts from ROM” in the Implementerʼs Guide (and
specifically the example project) has more information on this op-
tion.

-Olevel Optimization level: 0 = no optimizations, 1 = core instructions only,
2 = core & supplemental instructions, 3 = full instruction set (core,
supplemental & packed). Optimization levels 0 and 1 are compati-
ble withanyabstract machine, including JIT implementations (JIT
= “Just In Time” compiler, a high-performance abstract machine).
Levels 2 and 3 arenotcompatible with a JIT.

-ofilename Output file: set the name and path of the binary output file.

-pfilename Prefix file: the name of the “prefix file”, this is a file that is parsed
before the input file (as a kind of implicit “include file”). If used,
this option overrides the default include file “DEFAULT.INC”. The-p
option on its own (without a filename) disables the processing of any
implicit include file.

-rfilename Report: enable the creation of a report that contains the extracted
documentation and a cross-reference. The report is in “XML” for-
mat.
Thefilenameparameter is optional; if not specified, the report file
has the same name as the output file with the extension “.XML”.

-Svalue Stack size: the size of the stack and the heap in cells. #pragma dynamic:
103

```
148 — The compiler
```

```
-svalue Skip count: the number of lines to skip in the input file before start-
ing to compile; for example, to skip a “header” in the source file
which is not in a valid PAWNsyntax.
```

-Tfilename Template file: the name of the configuration file. If no extension is
Configuration file:
149 present, the file extension is “.cfg”; if no path is present, the file is
loaded from the subdirectory “target” below the root directory of
the PAWNcompiler. Seethe next pagefor more information.

```
-tvalue TABsize: the number of space characters to use for aTABcharacter.
Without this option, the PAWNparser will auto-detect theTAB.
-V+/-/value Overlays: generate the tables and instructions for running the code
from overlays. Using overlays reduces the memory requirements
for a script, because code sections can be swapped into and out off
memory dynamically. Since overlay loading takes time, the result-
ing script will also run slower. Use-V+to enable overlays and-V-
to create monolithic code —alternatively, use-Vfollowed by a value
to enable overlays and set the size of the overlay pool. The option-V
without any suffix toggles the current setting.
```

```
-vvalue Verbose: display informational messages during the compilation.
The value can be 0 (zero) for “quiet” compile, 1 (one) for the normal
output and 2 for a code/data/stack usage report.
```

```
-wvalue+/- Warning control: the warning number following the “-w” is enabled
```

Warnings: (^139) or disabled, depending on whether a “+” or a “-” follows the number.
When a “+” or “-” is absent, the warning status is toggled. For exam-
ple,-w225-disables the warning for “unreachable code”,-w225+en-
ables it and-w225toggles between enabled/disabled.
Only warnings can be disabled (errors and fatal errors cannot be dis-
abled). By default, all warnings are enabled.
-Xvalue Limit for the abstract machine: that is, themaximummemory re-
See also #pragma
amxlimit onpage
102
quirements that a compiled script may have, in bytes. This value is
is useful for (embedded) environments where the maximum size of
a script is bound to a hard upper limit.
If there is no setting for the amount of RAM for the data and stack,
this refers to the total memory requirements; if the amount of RAM
is explicitly set, this value only gives the amount of memory needed
for the code and the static data.
-XDvalue RAM limit for the abstract machine: themaximummemory require-
See also #pragma
amxram onpage
102
ments for data and stack that a compiled script may have, in bytes.
This value is is useful for (embedded) environments where the max-
imum data size of a script is bound to a hard upper limit. Especially

```
The compiler — 149
```

```
in the case where the PAWNscript runs from ROM, the sizes for the
code and data sections need both to be set.
```

- n Control characters start with “n” (for the sake of similarity with C,
    C++and Java). This is also the default, but may be overruled in a
    configuration file.

-^ Control characters start with “^” (for compatibility with earlier ver-
sions of PAWN).

-;+/- With-;+every statement is required to end with a semicolon; with
-;-, semicolons are optional to end a statement if the statement is
the last on the line. The option-;(without+orsuffix) toggles the
current setting.

-(+/- With-(+, arguments passed to a functionmustbe enclosed in paren-
theses; with-(-, parentheses are optional if the expression result of
the function call is not used. The option-((without+orsuffix)
toggles the current setting. See the section “Calling functions” on
page 68 for details on the optional parentheses around function ar-
guments.

sym=value define constant “sym” to the given (numeric)value. Thevalueis op-
tional; the constant is set to zero if absent.

@filename read (more) options from the specified “response file”.

Response file

To support operating systems with a limited command line length (e.g., DOS), the
PAWNcompiler supports “response files”. A response file is a text file that contains
the options that you would otherwise put at the command line. With the command:
pawncc @opts.txt prog.pawn
the PAWNcompiler compiles the file “prog.pawn” using the options that are listed
in the response file “opts.txt”.

Configuration file

On platforms that support it (currently Microsoft DOS, Microsoft Windows and
Linux), the compiler reads the options in a “configuration file” on start-up. By de-
fault, the compiler looks for the file “default.cfg” in the directory “target” below
the root directory of the PAWNinstallation. You can specify a different configura-
tion file with the “-T” compiler option.

For backward compatibility, the PAWNcompiler looks for the file “pawn.cfg” in the
directory “bin” below the PAWNroot directory, ifdefault.cfgcould not be found
and no other configuration file was explicitly given.

150 — The compiler

For an explicit configuration file, add the “-T” option on the command line, fol-
lowed by the base name of the configuration file. You cannot add a path, and you
need not specify the file extension. The configuration file itself must be stored in
the “target” directory, and it must have the extension “.cfg”.

In a sense, the configuration file is an implicit response file. Options specified on
the command line may overrule those in the configuration file. One difference
from a response file is that the configuration file may also contain instructions
and options for an IDE, such as Quincy. These auxiliary instructions start with
“#”-character, which the PAWNcompiler treats as a comment. For details of the
instructions supported by Quincy, please see the Quincy manual.

```
Rationale — 151
```

Rationale

APPENDIXC

The first issue in the presentation of a new computer language should be:why a
new language at all?

Indeed, Ididlook at several existing languages before I designed my own. Many
little languages were aimed at scripting the command shell (TCL, Perl, Python).
Other languages were not designed as extension languages, and put the burden to
embedding solely on the host application.

As I initially attempted to use Java as an extension language (rather than build my
own, as I have done now), the differences between PAWNand Java are illustrative
for the almost reciprocal design goals of both languages. For example, Java pro-
motes distributed computing where “packages” reside on diverse machines, PAWN
is designed so that the compiled applets can be easily stored in a compound file to-
gether with other data. Java is furthermore designed to be architecture neutral and
application independent, inversely PAWNis designed to be tightly coupled with an
application; native functions are a taboo to some extent in Java (at least, it is con-
sidered “impure”), whereas native functions are “the reason to be” for PAWN. From
the viewpoint of PAWN, the intended use of Java is upside down: native functions
are seen as an auxiliary library that the application —in Java— uses; in PAWN, na-
tive functions are part of “the application” and the PAWNprogram itself is a set of
auxiliary functions that the application uses.

**A language for scripting applications and devices:** PAWNis targeted as anexten-
sion language, meant to write application-specific macros or sub-programs with.
PAWNis not the appropriate language for implementing business applications or
operating systems in. PAWNis designed to be easily integrated with, and embedded
in, other systems/applications. It is also designed to run in resource-constrained
environments, such as on small micro-controllers.

As an extension language, PAWNprograms typically manipulate objects of the host
application. In an animation system, PAWNscripts deal with sprites, events and
time intervals; in a communication application, PAWNscripts handle packets and
connections. I assume that the host application or the device makes (a subset of)
its resources and functionality available via functions, handles, magic cookies:::
in a similar way that a contemporary operating system provides an interface to
processes written in C/C++—e.g., the Win32 API (“handles everywhere”) or GNUʼs
“glibc”. To that end, PAWNhas a simple and efficient interface to the “native” func-
tions of the host application. A PAWNscript manipulates data objects in the host ap-
plication through function calls, but itcannotaccess the data of the host application
directly.

The first and foremost criteria for the PAWNlanguage were execution speed and
reliability. Reliability in the sense that a PAWNprogram should not be able to crash

152 — Rationale

the application or tool in which it is embedded —at least, not easily. Although this
limits the capabilities of the language significantly, the advantages are twofold:

⋄the application vendor can rest assured that its application will not crash due to
user additions or macros,

⋄the user is free to experiment with the language with no (or little) risk of damag-
ing the application files.

**Speed is essential:** PAWNprograms would probably run in an abstract machine,
and abstract machines are notoriously slow. I had to make a language that has low
overhead and a language for which a fast abstract machine can be written. Speed
should also be reliable, in the sense that a PAWNscript should not slow down over
time or have an occasional performance hiccup. Consequently, PAWNexcludes
any required “background process”, such as garbage collection, and the core of the
abstract machine does not implicitly allocate any system or application resources
while it runs. That is, PAWNdoes not allocate memory or open files, not without the
help of a native function that the script callsexplicitly.

As Dennis Ritchie said, by intent the C language confines itself to facilities that can
be mapped relatively efficiently and directly to machine instructions. The same is
true for PAWN, and this is also a partial explication why PAWNlooks so much like
C. Even though PAWNruns on anabstractmachine, the goal is to keep that abstract
machine small and quick. PAWNis used in tiny embedded systems withRAMsizes of
32 kiB or less, as well as in high-performance games that need every processor cycle
for their graphics engine and game-play. In both environments, a heavy-weight
scripting support is difficult to swallow.

A brief analysis showed that the instruction decoding logic for an abstract machine
would quickly become the bottleneck in the performance of the abstract machine.
The quickest way to dispatch instructions would be to use the opcode as an in-
dex in a jump table. Therefore all opcodes should have the same size (excluding
operands), and the opcode fully specifies the instruction (including the address-
ing methods, size of the operands, etc.). That meant that for each operation on a
variable, the abstract machine needed a separate opcode for every combination of
variable type, storage class and access method (direct, or dereferenced). For even
three types (int,charandunsigned int), two storage classes (global and local)
and three access methods (direct, indirect or indexed), a total of 18 opcodes (3*2*3)
are needed to simply fetch the value of a variable.

To get an abstract machine that is both small and quick, the number of opcodes
should be kept to a minimum:each “virtual instruction” needs to be handled by
the abstract machine, and therefore takes code space. With 18 opcodes to load a
variable in a register, 18 more to store a register into a variable, another 18 to get

```
 136 Opcodes are defined at this writing, plus 20 “macro” opcodes.
```

```
Rationale — 153
```

the address of a variable, etc:::the abstract machine that I envisioned was quickly
growing out of its desired proportions.

The languagesBOBandREXXinspired me to design a typeless language. This saved
me a lot of opcodes. At the same time, the language could no longer be called a
“subset of C”. I was changing the language. Why, then, not go a foot further in
changing the language? This is where a few more design guidelines came into play:
⋄give the programmer a general purpose tool, not a special purpose solution
⋄avoid error prone language constructs; promote error checking
⋄be pragmatic

**A general purpose tool:** PAWNis targeted as an extension language, without speci-
fying exactly what it will extent. Typically, the application or the tool that uses PAWN
for its extension language will provide many, optimized routines or commands to
operate on its native objects, be it text, database records or animated sprites. The
extension language exists to permit the user to do what the application developer
forgot, or decided not to include. Rather than providing a comprehensive library
of functions to sort data, match regular expressions, or draw Bézier splines, PAWN
should supply a (general purpose) means to use, extend and combine the specific
(“native”) functions that an application provides.

PAWNlacks a comprehensive standard library. By intent, PAWNalso lacks features
like pointers, dynamic memory allocation, direct access to the operating system or
to the hardware, that are needed to remain competitive in the field of general pur-
pose application or system programming. You cannot build linked lists or dynamic
tree data structures in PAWN, and neither can you access any memory beyond the
boundaries of the abstract machine. That is not to say that a PAWNprogram can
never use dynamic, sorted symbol tables, or change a parameter in the operating
system; itcando that, but it needs to do so by calling a “native” function that an
application provides to the abstract machine.

In other words, if an application chooses to implement the well knownpeekand
pokefunctions (fromBASIC) in the abstract machine, a PAWNprogram can access
any byte in memory, insofar the operating system permits this. Likewise, an appli-
cation can provide native functions that insert, delete or search symbols in a table
and allows several operations on them. The proposed core functionsgetproperty
andsetpropertyare an example of native functions that build a linked list in the
background.

**Promote error checking:** As you may have noticed, one of the foremost design
criteria of the C language, “trust the programmer”, is absent from my list of de-
sign criteria. Users of script languages may not be experienced programmers; and
even if they are, PAWNwill probably not be theirprimarylanguage. Most PAWNpro-
grammers will keep learning the language as they go, and will even after years not
have become experts. Enough reason, hence, to replace error prone elements from
the C language (e.g. pointers) with saver, albeit less general, constructs (e.g. refer-

154 — Rationale

ences).yReferences are copied from C++. They are nothing else than pointers in
disguise, but they are restricted in various, mostly useful, ways. Turn to a C++book
to find more justification for references.

I find it sad that many, even modern, programming languages have so little built-
in, or easy to use, support for confirming that programs do as the programmer in-
tended. I am not referring to theoretical correctness (which is too costly to achieve
for anything bigger than toy programs), but practical, easy to use (even if limited)
means to verify correct functioning. PAWNprovides both compile time and execu-
tion time assertions to use for preconditions, postconditions and invariants.

The typing mechanism that most programming languages use is also an automatic
“catcher” of a whole class of bugs. By virtue of being a typeless language, PAWN
lacked these error checking abilities. This was clearly a weakness, and I created
the “tag” mechanism as an equivalent for verifying function parameter passing,
array indexing and other operations.

The quality of the tools: the compiler and the abstract machine, also have a great
impact on the robustness of code —whatever the language. Although this is only
very loosely related to the design of the language, I set out to build the tools such
that they promote error checking. The warning system of PAWNgoes a step be-
yond simply reporting where the parser fails to interpret the data according to the
language grammar. At several occasions, the compiler runs checks that are com-
pletely unrelated to generating code and that are implemented specifically to catch
possible errors. Likewise, the “debugger hook” is designed right into the abstract
machine, it is not an add-on implemented as an after-thought.

**Be pragmatic:** The object-oriented programming paradigm has not entirely lived
up to its promise, in my opinion. On the one hand, OOP solves many tasks in an
easier or cleaner way, due to the added abstraction layer. On the other hand, con-
temporary object-oriented languages leave you struggling with the language. The
struggle should be with implementing the functionality for a specific task, not with
the language used for the implementation. Object-oriented languages are attrac-
tive mainly because of the comprehensive class libraries that they come with —but
leaning on a standard library goes against one of the design goal for PAWN. Object-
oriented programming is not a solution for a non-expert programmer with little
patience for artificial complexity. The criterion “be pragmatic” is a reminder to
seek solutions, not elegance.

Practical design criteria

The fact that PAWNlooks so much like C cannot be a coincidence, and it isnʼt. PAWN

```
yYou should see this remark in the context of my earlier assertion that many “Pawn” program-
mers will be novice programmers. In my (teaching) experience, novice programmers make many
pointer errors, as opposed to experienced C/C++ programmers.
```

```
Rationale — 155
```

started as a C dialect and stayed that way, because C has a proven track record. The
changes from C were mostly born out of necessity after rubbing out the features of
C that I did not want in a scripting language: no pointers and no “typing” system.

PAWN, being a typeless language, needed a different means to declare variables. In
the course of modifying this, I also dropped the C requirement that all variables
should be declared at the top of a compound statement. PAWNis a little more like
C++in this respect.

C language functions can pass “output values” via pointer arguments. The standard
functionscanf, for example, stores the values or strings that it reads from the con-
sole into its arguments. You can design a function in C so that it optionally returns
a value through a pointer argument; if the caller of the function does not care for
the return value, it passesNULLas the pointer value. The standard functionstr-
tolis an example of a function that does this. This technique frequently saves you
from declaring and passing dummy variables. PAWNreplaces pointers with refer-
ences, but references cannot beNULL. Thus, PAWNneeded a different technique to
“drop” the values that a function returns via references. Its solution is the use of
an “argument placeholder” that is written as an underscore character (“”); Prolog
programmers will recognize it as a similar feature in that language. The argument
placeholder reserves a temporary anonymous data object (a “cell” or an array of
cells) that is automatically destroyed after the function call.

The temporary cell for the argument placeholder must still have a value, because
the function may see a reference parameters as input/output. Therefore, a func-
tion must specify for each passed-by-reference argument what value it will have
upon entry when the caller passes the placeholder instead of an actual argument.
By extension, I also added default values for arguments that are “passed-by-value”.
The feature to optionally remove all arguments with default values from the right
was copied from C++.

When speaking of BCPL and B, Dennis Ritchie said that C was invented in part to
provide a plausible way of dealing with character strings when one begins with
a word-oriented language. PAWNprovides two options for working with strings,
packed and unpacked strings. In an unpacked string, every character fits in a cell.
The overhead for a typical 32-bit implementation is large: one character would take
four bytes. Packed strings store up to four characters in one cell, at the cost of be-
ing significantly more difficult to handle if you could only access full cells. Mod-
ern BCPL implementations provide two array indexing methods: one to get a word
from an array and one to get a character from an array. PAWNcopies this concept,
although the syntax differs from that of BCPL. The packed string feature also led to
the alternative array indexing syntax.

Unicode applications often have to deal with two characters sets: 8-bit for legacy file

```
Support for Unicode
string literals: 118
```

formats and standardized transfer formats (like many of the Internet protocols) and
the 16-bit Unicode character set (or the 31-bit UCS-4 character set). Although the
PAWNcompiler has an option that makes characters 16-bit (so only two characters

156 — Rationale

fit in a 32-bit cell), it is usually more convenient to store single-byte character strings
in packed strings and multi-byte strings in unpacked strings. This turns a weakness
in PAWN—the need to distinguish packed strings from unpacked strings— into a
strength: PAWNcan make that distinction quite easily. And instead of needing two
implementations for every function that deals with strings (anASCIIversion and
a Unicode version —look at the Win32 API, or even the standard C library), PAWN
enables functions to handlebothpacked and unpacked strings with ease.

Notwithstanding the above mentioned changes, plus those in the chapter “Pitfalls:
differences from C” (page 115), I have tried to keep PAWNclose to C. A final point,
which is unrelated to language design, but important nonetheless, is the license:
PAWNis distributed under a liberal license allowing you to use and/or adapt the
code with a minimum of restrictions —see appendix D.

```
License — 157
```

License

APPENDIXD

The **software product** “PAWN” (the compiler, the abstract machine and the support
routines) is copyright 1997–2024 by CompuPhase, and it is distributed under the
“Apache License” version 2.0 which is reproduced below, plus an exception clause
regarding static linking.

The PAWNcompiler is a derivative of “Small C”. Small C is copyright 1982–1983 J.E.
Hendrix, and copyright 1980 R. Cain. J.E. Hendrix has made the Small C compiler
available for royalty free use in private or commerical endeavors, on the condition
that the original copyright notices of the original authors (Ron Cain, James Hen-
drix) be retained in derivative versions. Ron Cain has put his work in the public
domain.

See the file NOTICES for contributions and their respective licenses.

**EXCEPTION TO THE APACHE 2.0 LICENSE**
As a special exception to the Apache License 2.0 (and referring to the definitions in Section 1 of
this license), you may link, statically or dynamically, the “Work” to other modules to produce an
executable file containing portions of the “Work”, and distribute that executable file in “Object”
form under the terms of your choice, without any of the additional requirements listed in Section
4 of the Apache License 2.0. This exception applies only to redistributions in “Object” form (not
“Source” form) and only if no modifications have been made to the “Work”.

**TERMS AND CONDITIONS FOR USE, REPRODUCTION, AND DISTRIBUTION**

1. **Definitions**
    “License” shall mean the terms and conditions for use, reproduction, and distribution as defined
    by Sections 1 through 9 of this document.
    “Licensor” shall mean the copyright owner or entity authorized by the copyright owner that is grant-
    ing the License.
    “Legal Entity” shall mean the union of the acting entity and all other entities that control, are con-
    trolled by, or are under common control with that entity. For the purposes of this definition, “con-
    trol” means (i) the power, direct or indirect, to cause the direction or management of such entity,
    whether by contract or otherwise, or (ii) ownership of fifty percent (50%) or more of the outstanding
    shares, or (iii) beneficial ownership of such entity.
    “You” (or “Your”) shall mean an individual or Legal Entity exercising permissions granted by this
    License.
    “Source” form shall mean the preferred form for making modifications, including but not limited
    to software source code, documentation source, and configuration files.
    “Object” form shall mean any form resulting from mechanical transformation or translation of a
    Source form, including but not limited to compiled object code, generated documentation, and
    conversions to other media types.
    “Work” shall mean the work of authorship, whether in Source or Object form, made available under
    the License, as indicated by a copyright notice that is included in or attached to the work (an example
    is provided in the Appendix below).
    “Derivative Works” shall mean any work, whether in Source or Object form, that is based on (or
    derived from) the Work and for which the editorial revisions, annotations, elaborations, or other

158 — License

```
modifications represent, as a whole, an original work of authorship. For the purposes of this Li-
cense, Derivative Works shall not include works that remain separable from, or merely link (or
bind by name) to the interfaces of, the Work and Derivative Works thereof.
“Contribution” shall mean any work of authorship, including the original version of the Work and
any modifications or additions to that Work or Derivative Works thereof, that is intentionally sub-
mitted to Licensor for inclusion in the Work by the copyright owner or by an individual or Legal
Entity authorized to submit on behalf of the copyright owner. For the purposes of this definition,
“submitted” means any form of electronic, verbal, or written communication sent to the Licensor or
its representatives, including but not limited to communication on electronic mailing lists, source
code control systems, and issue tracking systems that are managed by, or on behalf of, the Licen-
sor for the purpose of discussing and improving the Work, but excluding communication that is
conspicuously marked or otherwise designated in writing by the copyright owner as “Not a Contri-
bution.”
“Contributor” shall mean Licensor and any individual or Legal Entity on behalf of whom a Contri-
bution has been received by Licensor and subsequently incorporated within the Work.
```

2. **Grant of Copyright License.** Subject to the terms and conditions of this License, each Contributor
    hereby grants to You a perpetual, worldwide, non-exclusive, no-charge, royalty-free, irrevocable
    copyright license to reproduce, prepare Derivative Works of, publicly display, publicly perform,
    sublicense, and distribute the Work and such Derivative Works in Source or Object form.
2. **Grant of Patent License.** Subject to the terms and conditions of this License, each Contributor
    hereby grants to You a perpetual, worldwide, non-exclusive, no-charge, royalty-free, irrevocable
    (except as stated in this section) patent license to make, have made, use, offer to sell, sell, import,
    and otherwise transfer the Work, where such license applies only to those patent claims licensable
    by such Contributor that are necessarily infringed by their Contribution(s) alone or by combination
    of their Contribution(s) with the Work to which such Contribution(s) was submitted. If You institute
    patent litigation against any entity (including a cross-claim or counterclaim in a lawsuit) alleging
    that the Work or a Contribution incorporated within the Work constitutes direct or contributory
    patent infringement, then any patent licenses granted to You under this License for that Work shall
    terminate as of the date such litigation is filed.
3. **Redistribution.** You may reproduce and distribute copies of the Work or Derivative Works thereof
    in any medium, with or without modifications, and in Source or Object form, provided that You
    meet the following conditions:
    a. You must give any other recipients of the Work or Derivative Works a copy of this License; and
    b. You must cause any modified files to carry prominent notices stating that You changed the files;
       and
    c. You must retain, in the Source form of any Derivative Works that You distribute, all copyright,
       patent, trademark, and attribution notices from the Source form of the Work, excluding those
       notices that do not pertain to any part of the Derivative Works; and
    d. If the Work includes a “NOTICE” text file as part of its distribution, then any Derivative Works
       that You distribute must include a readable copy of the attribution notices contained within such
       NOTICE file, excluding those notices that do not pertain to any part of the Derivative Works, in at
       least one of the following places: within a NOTICE text file distributed as part of the Derivative
       Works; within the Source form or documentation, if provided along with the Derivative Works;
       or, within a display generated by the Derivative Works, if and wherever such third-party notices
       normally appear. The contents of the NOTICE file are for informational purposes only and do
       not modify the License. You may add Your own attribution notices within Derivative Works that
       You distribute, alongside or as an addendum to the NOTICE text from the Work, provided that
       such additional attribution notices cannot be construed as modifying the License.
    You may add Your own copyright statement to Your modifications and may provide additional or
    different license terms and conditions for use, reproduction, or distribution of Your modifications,
    or for any such Derivative Works as a whole, provided Your use, reproduction, and distribution of
    the Work otherwise complies with the conditions stated in this License.
4. **Submission of Contributions.** Unless You explicitly state otherwise, any Contribution intentionally
    submitted for inclusion in the Work by You to the Licensor shall be under the terms and conditions

```
License — 159
```

```
of this License, without any additional terms or conditions. Notwithstanding the above, nothing
herein shall supersede or modify the terms of any separate license agreement you may have exe-
cuted with Licensor regarding such Contributions.
```

6. **Trademarks.** This License does not grant permission to use the trade names, trademarks, service
    marks, or product names of the Licensor, except as required for reasonable and customary use in
    describing the origin of the Work and reproducing the content of the NOTICE file.
2. **Disclaimer of Warranty.** Unless required by applicable law or agreed to in writing, Licensor pro-
    vides the Work (and each Contributor provides its Contributions) on an “AS IS” BASIS, WITHOUT
    WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied, including, without limi-
    tation, any warranties or conditions of TITLE, NON-INFRINGEMENT, MERCHANTABILITY, or FIT-
    NESS FOR A PARTICULAR PURPOSE. You are solely responsible for determining the appropriate-
    ness of using or redistributing the Work and assume any risks associated with Your exercise of per-
    missions under this License.
3. **Limitation of Liability.** In no event and under no legal theory, whether in tort (including negli-
    gence), contract, or otherwise, unless required by applicable law (such as deliberate and grossly
    negligent acts) or agreed to in writing, shall any Contributor be liable to You for damages, includ-
    ing any direct, indirect, special, incidental, or consequential damages of any character arising as
    a result of this License or out of the use or inability to use the Work (including but not limited to
    damages for loss of goodwill, work stoppage, computer failure or malfunction, or any and all other
    commercial damages or losses), even if such Contributor has been advised of the possibility of such
    damages.
4. **Accepting Warranty or Additional Liability.** While redistributing the Work or Derivative Works
    thereof, You may choose to offer, and charge a fee for, acceptance of support, warranty, indemnity,
    or other liability obligations and/or rights consistent with this License. However, in accepting such
    obligations, You may act only on Your own behalf and on Your sole responsibility, not on behalf
    of any other Contributor, and only if You agree to indemnify, defend, and hold each Contributor
    harmless for any liability incurred by, or claims asserted against, such Contributor by reason of
    your accepting any such warranty or additional liability.

The PAWN **documentation** is copyright by CompuPhase, and licensed under the
Creative Commons Attribution/ShareAlike 3.0 License. To view a copy of this li-
cence, visit
[http://creativecommons.org/licenses/by-sa/3.0/](http://creativecommons.org/licenses/by-sa/3.0/)
or send a letter to Creative Commons, 171 Second St, Suite 300, San Francisco, CA
94105 USA.

Below is a “human-readable” summary of the Legal Code (the full licence).

You are free:

```
to Share — to copy, distribute and transmit the work
to Remix — to adapt the work
```

Under the following conditions:

```
Attribution — You must attribute the work in the manner specified by the author
or licensor (but not in any way that suggests that they endorse you or your use
of the work).
Share Alike — If you alter, transform, or build upon this work, you may dis-
tribute the resulting work only under the same, similar or a compatible license.
```

160 — License

⋄ Nothing in this license impairs or restricts the authorʼs moral rights.

With the understanding that:

```
Waiver — Any of the above conditions can be waived if you get permission from
the copyright holder.
Other Rights — In no way are any of the following rights affected by the license:
Your fair dealing or fair use rights;
The authorʼs moral rights;
Rights other persons may have either in the work itself or in how the work is
used, such as publicity or privacy rights.
Notice — For any reuse or distribution, you must make clear to others the license
terms of this work.
```

### 161

Index

```
⋄Names of persons (not products) are initalics.
⋄Names of function and constants, and compiler reserved words are intype-
writer font.
```

```
!#, 81
#assert, 100
#define, 79 , 81 , 100 , 116
#endinput, 100
#error, 100
#file, 100
#if, 100
#include, 101
#line, 101
#pragma, 101
#section, 105
#tryinclude, 105
#undef, 81 , 105
#warning, 105
@-symbol, 53 , 71
Łukasiewicz, Jan, 23
```

AActual parameter, 14 , 59

```
Algebraic notation, 23
Alias,seeExternal name
Alignment (variables), 101
Anno Domini, 10
APL, 23
Argument placeholder, 63
Array
symbolic subscripts, 17 , 26 , 55
Array assignment, 91 , 115
Arrays, 56
Progressive initiallers, 55
ASCII, 119 , 120 , 145 , 146
Assertions, 8 , 41 , 88 , 154
Automata theory, 35 , 37 , 38
Automaton, 32 , 70 ,see alsoState
anonymous~, 34 , 98
```

```
BBasic Multilingual Plane, 120
BCPL, 155
Big Endian, 86
Binary Coded Decimals, 88
Binary radix, 84 , 115
Bisection, 66
bitcount, 22
Bitwise operators, 20
BOB, 153
Byte Order Mark, 145
Bytecode,seeP-code
```

```
CCain, Ron, 1
Call by reference, 12 , 14 , 67
Call by value, 11 , 14 , 61 , 76
Callee (functions), 68
Celsius, 12
Chained relational operators, 15 , 92
Character constants, 85
clamp, 106
clreol, 110
clrscr, 110
Codepage, 102 ,119–121, 146
Coercion rules, 67
Comments, 83
documentation~, 43 , 83
Commutative operators, 76
Compiler options, 146
Compound literals,seeLiteral array
Compound statement, 96
Conditional goto, 115
Configuration file, 148 , 149
```

```
162 — Index
```

```
Constants
“const” variables, 54
literals, 84
predefined~, 87
symbolic~, 87
Counting bits, 22
Cross-reference, 43 , 147
```

DData declarations,52–57

```
arrays, 56
default initialization, 54
global~, 52
local~, 52
public~, 53
stock~, 53
Date
~arithmetic,^12
functions, 113
Debug level, 88
Default arguments, 63
Default initialization, 54
Design by contract, 41
Diagnostic, 58 , 59 , 66 , 121 ,see also
Errors and Warnings
Digit group separator, 84
Directives, 81 ,100–105
DLL calls, 114
Documentation comments, 43 , 83
Documentation tags, 147
Dr. Dobbʼs Journal, 1
Dynamic tree, 153
```

```
EEiffel, 42
Ellipsis operator, 55 , 62 , 67 , 86 , 125
Encryption, 147
Endless loop, 97
enum, 116
Eratosthenes, 7
Error,see alsoDiagnostic
Errors, 50 ,127–139
Escape sequences, 85 , 86
Euclides, 5
```

```
Event-driven programming model,
28 , 30 , 31
Extended ASCII, 119 , 145
External name, 73 , 76
```

```
FFaculty, 61
faculty, 61
Fahrenheit, 12
Fall-back (state functions), 36 , 70
Fibonacci, 8
fibonacci, 8
Fibonacci numbers, 8
File input/output, 113
Fixed point arithmetic, 66 , 77 , 113
Floating point arithmetic, 77 , 84 ,
114 , 115
Flow-driven programming model, 28 ,
30
Floyd, Robert, 64
Forbidden user operators, 78
Foreign Function Interface, 114
Formal parameter, 59 , 60
Forward declaration, 60 , 70
FSM,seeAutomaton
funcidx, 106
Function library, 106
Functions,60–73
call by reference, 12 , 14 , 61
call by value, 11 , 14 , 61 , 76
callee, 68
caller, 68
coercion rules, 67
default arguments, 63
forward declaration, 60 , 70
~index,^106
latent~, 98
native~, 9 , 72
public~, 71
standard library~, 106
state classifier, 70
state entry~, 35 , 52
state exit~, 38
static~, 72
stock~, 72
```

```
Index — 163
```

```
variable arguments, 67
```

```
Ggcd, 5
getarg, 107
getchar, 110
getstring, 110
getvalue, 111
Global variables, 52
Golden ratio, 9
gotoxy, 111
Greatest Common Divisor, 5
Gregorian calendar, 9
Gödel, Escher, Bach, 125
```

HHamblin, Charles, 23

```
Hanoi, the Towers of~, 69
heapspace, 107
Hendrix, James, 1
Hexadecimal radix, 84 , 115
Hofstadter, Douglas R., 125
Host application, 53 , 54 , 73 , 97 , 98 ,
106 , 121 , 151
```

```
IIdentifiers, 83
Implicit conversions,seecoercion
rules
Index tag, 58
Indiction Cycle, 9
Infinite loop, 16
Infix notation,seeAlgebraic~
Internationalization, 118
Internet, 155
Intersection (sets), 19
ISO 8859, 86 , 119 , 145
ISO/IEC 10646-1, 119 , 120
ISO/IEC 8824 (date format), 63
ispacked, 117
```

```
JJacquard Loom, 32
Java, 151
JIT, 147
Julian Day number, 9
```

```
KKeeLoq (cipher), 147
Keyword,seeReserved word
```

```
LLatent function, 98
Latin-1,seeISO 8859
Leap year, 60
leapyear, 60
Leonardo of Pisa, 8
Library call, 114
Library functions, 72
License, 157
Line continuation character, 125
Linear congruential genera- tor, 108
Linked lists, 153
Linux, 120 , 151
LISP, 29
Literal array, 62
Literals,seeConstants
Local variables, 52
Logo (programming lang- uage), 29
lvalue, 59 , 89 , 123
```

```
MMacro, 79 , 100
~prefix,^81 ,^105
max, 107
Mealy automata, 37
Metonic Cycle, 9
Meyer, Bertrand, 42
Micro-controllers, 151
Microsoft Windows, 120
min, 107
MIRT, 37
Moore automata, 37
```

```
NNamed parameters, 62
Native functions, 9 , 72
external name, 73 , 76
Newton-Raphson, 66
numargs, 108
```

```
164 — Index
```

OOctal radix, 115

```
opcodeset, 103
Operator precedence, 94
Operators,89–94
commutative~, 76
user-defined~, 73 , 122
Optional semicolons, 83
Options
compiler~, 146
Overlays, 104 , 148
```

```
PP-code, 79 , 147
Packed string, 86 , 117 , 155
Parameter
actual~, 14 , 59
formal~, 59 , 60
Parser, 4
Placeholder,seeArgument~
Plain strings, 86
Plural tag names, 124
Positional parameters, 62
power, 60
Precedence table, 94
Prefix file, 147
Preprocessor,79–82
~macro,^79 ,^100
Prime numbers, 7
print, 112
printf, 13 , 112
Priority queue, 19
Procedure call syntax, 68
Process control, 114
Progressive initiallers, 55
Proleptic Gregorian calendar, 9
Pseudo-random numbers, 108
Public
~functions,^71 ,^106
~variables,^53
```

QQuincy (IDE), 50 , 150

```
Quine, 125
```

```
Rrandom, 108
Random sample, 64
Rational numbers, 12 , 25 , 84
Recursive functions, 69
Reference arguments, 12 , 61 , 67
Report, 147
Reserved words, 83
Response file, 149
Reverse Polish Notation, 23
REXX, 153
Ritchie, Dennis, 116 , 152 , 155
rot13, 14
ROT13 encryption, 14
```

```
SScaliger, Josephus, 9
Semicolons, optional, 83
Set operations, 19
setarg, 108
setattr, 113
Shadowing, 141
Shared libraries, 114
Shift-JIS, 119
sieve, 7
Single line comment, 83
sizeof operator, 94
~in function argument,^64 ,^65
Small C, 1
Solar Cycle, 9
sqroot, 66
Square root, 66
Standard function library, 106
State, 32 , 33
~classifier,^34 ,^38 ,^52 ,^70
conditional~, 34
~diagram,^32 ,^40
~entry function,^35 ,^52
~exit function,^38
fall-back~, 36 , 70
~notation,^40
~operator,^94
unconditional~, 37
~variables,^37 ,^52
```

```
Index — 165
```

```
Statements,96–99
Static
~functions,^72
~variables,^52 ,^53
Stock
~functions,^72
~variables,^53
String
~concatenation,^86
packed~, 86 , 117 , 155
plain~, 86
unpacked~, 85 , 117 , 155
String manipulation, 114
Stringize operator, 81
strtok, 15
Structure, 17 ,see alsoSym- bolic sub-
script
strupper, 118
Surrogate pair, 120 , 121
swap, 61
swapchars, 109
Symbolic constants, 87
Symbolic information, 146
Symbolic subscripts (array), 17 , 26 ,
55
Syntax rules, 83
```

TTag name, 13 , 58 , 121

```
~and enumerated constant,^87
array index, 58
~operator,^124
~override,^58 ,^93 ,^123
plural tags, 124
predefined~, 88
strong~, 59 , 123
~syntax,^88
untag override, 124
weak~, 59 , 122
Tag names, 154
tagof operator, 94
Template file,seeConfiguration file
Text substitution, 79 , 100
The Towers of Hanoi, 69
```

```
Thousands separator, 84
Time
functions, 113
tolower, 109
toupper, 109
Transition (state), 32
Turtle graphics, 29
```

```
UUCS-4, 85 , 119 , 121 , 145
Unicode, 85 , 119 , 121 , 145 , 146 , 155
Union (sets), 19
UNIX, 120
Unpacked string, 85 , 117 , 155
Untag override, 124
User error, 100 , 105
User-defined operators, 73 , 122
forbidden~, 78
UTF-8, 120 , 121 , 135 , 145
```

```
VVan Orman Quine, Willard, 125
Variable arguments, 67
Variables, 52 ,see alsoData declara-
tions
state~, 37
VETAG, 37
Virtual Machine,seeAbstract ma-
chine
```

```
WWarning,see alsoDiagnostic
Warnings,139–144, 148
weekday, 62 , 99
White space, 83
Whitesmithʼs style, 4
Wide character, 121
Word count, 15
```

```
XXML, 43 , 147
XSLT, 43
```

YYear zero, 10

ZZeller, 62
