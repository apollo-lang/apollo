## Project plan and organization

A lot of our initial meetings consisted in deciding what features to include in our language. In particular, we tried our best to make the language as simple and easy to use as possible. We wanted a musical language that does away with complicated and overly technical details to allow users to have fun and let their curiosity roam free. Our meetings in the month of February thus concentrated on coming up with data types and a syntax for our language that would answer these needs.


### Project management

#### Planning 

Our team meetings happened every week, usually on Friday for two hours. The first meetings were dedicated to specifying requirements and taking important design decisions. Later in the semester, we met more regularly to the different parts of the language. We used GitHub for the language white paper, tutorial and reference manual as well as other documents such as the stylesheet so they would be easily accessible to everyone during the development process. We set up on online Google chat so we could freely discuss problems, features left to implement and assign remaining tasks. 


#### Specification 
We took a fairly long time to clarify what our language would look like. Having to write the reference manual and tutorial helped immensely as it forced us to come up with a consistent way to combine our many different ideas. The weeks following the submission of the reference manual allowed us to refine our vision and decide on the necessary changes to make. The language was ultimately made both simpler to use and provided with more powerful elements such as lambda expressions or type coercion. Thus, although the specification in the original reference manual was used as a guide throughout the development process, our concern was always convenience and ease of use of our language.


#### Development 

To be able to effectively work on this large project, we used Git for distributed version control and set up a repository on GitHub. Every team member was assigned a feature to work on either at our team meetings, through our online chat or using GitHub. We also used GitHub to create issues, signaling either bugs in the language, features left to implement, documentation to be added or design questions. Each team member making changes to the language worked on his own branch. Once they were satisfied and after running the unit tests, they would create a pull request. In addition to the continuous integration testing suite that runs for every pull request, other team members would have to review the code, and point out any problems with it. 


#### Testing

As said above, we used both local unit tests and continuous integration tests on GitHub with Travis CI. The former allowed us to verify that new features or fixes did not break the language and that everything worked as expected. A test would be added for every new feature in the language. Features to be implemented would be added in a special folder for future testing, allowing us to plan ahead and have a reference while implementing them. Travis CI was used for continuous integration and allowed us to detect any problem when trying to merge branches into master.  


### Style guide



#### Timeline

![](./img/activity.png)

* February 25: Language white paper
* March 25: LRM and tutorial
* March 27: Compiler front-end working 
* March 30: Basic evaluation and AST set up
* April 12: Back-end (MIDI generation) implemented, REPL working
* April 19: Continuous Integration testing with Travis
* April 26: Initial implementation of Type checking
* May 2: Able to generate MIDI from Apollo
* May 8: Type coercion implemented
* May 10: Final report




### Roles and responsibilities 
* Roberto Jose de Amorim - Tester & Validator
* Benjamin Matthew Kogan - System Architect
* Javier Llaca - Language & Tools Guru
* Reza Nayebi - Project Manager
* Souren Sarkis Papazian - System Integrator

For a detailed breakdown of responsibilities see the corresponding section in Translation architecture


### Project log

b11e94f - Reza Nayebi, 58 minutes ago : Update tutorial.md\
590d9d7 - Ben Kogan, 74 minutes ago : Create testing readme\
3f8fe25 - Ben Kogan, 76 minutes ago : Update Makefile and readme\
b2314d3 - Javier Llaca, 87 minutes ago : Merge branch 'master' of https://github.com/apollo-lang/apollo\
3eb43dd - Javier Llaca, 87 minutes ago : Update LRM\
352b427 - SourenP, 2 hours ago : Merge branch 'master' of https://github.com/apollo-lang/apollo\
3a1a5ad - SourenP, 2 hours ago : added concatMapI and a new example that uses it\
f87056d - Reza Nayebi, 2 hours ago : Update whitepaper.md\
0798d06 - Reza Nayebi, 2 hours ago : Update whitepaper.md\
ca20633 - Reza Nayebi, 2 hours ago : Update whitepaper.md\
2340ae0 - Reza Nayebi, 2 hours ago : Update whitepaper.md\
332bbe1 - SourenP, 4 hours ago : dealt with conflic\
acf9325 - SourenP, 4 hours ago : added intercalate to the prelude\
0067488 - Javier Llaca, 4 hours ago : Merge branch 'master' of https://github.com/apollo-lang/apollo\
7227669 - Javier Llaca, 4 hours ago : Add fibonacci example\
64be6f2 - rja2139, 4 hours ago : Merge branch 'master' of ssh://github.com/apollo-lang/apollo\
636bc3b - rja2139, 4 hours ago : crlf\
154e934 - Souren Papazian, 4 hours ago : Delete constraint.aps\
7f3f4ca - Souren Papazian, 4 hours ago : Delete constraint.ans\
9281bb4 - Javier Llaca, 4 hours ago : Merge branch 'master' of https://github.com/apollo-lang/apollo\
fb60d29 - Javier Llaca, 4 hours ago : Add pseudo random numbers\
18ab876 - Ben Kogan, 4 hours ago : Fix prelude\
00d79b5 - Ben Kogan, 4 hours ago : Add clean quit to repl on ^D\
da6e75f - Ben Kogan, 5 hours ago : Merge branch 'master' of https://github.com/apollo-lang/apollo\
e385673 - Ben Kogan, 5 hours ago : Add usage to repl\
0f315d7 - Javier Llaca, 5 hours ago : Use random number rule in grammar\
7129ab4 - Javier Llaca, 5 hours ago : Merge with origin\
3a2f129 - Javier Llaca, 5 hours ago : Add syntax for random number generation\
45aad96 - Javier Llaca, 5 hours ago : Add show and showPP for typed lambda expressions\
281fb8b - Javier Llaca, 5 hours ago : Update standard library\
3e6294d - Ben Kogan, 5 hours ago : Add :export repl command\
fd74e62 - Ben Kogan, 5 hours ago : Fix typo\
888450d - Ben Kogan, 5 hours ago : Add style guide and reformat code accordingly\
a8fe949 - Reza Nayebi, 8 hours ago : Merge branch 'master' of https://github.com/apollo-lang/apollo\
f04cf95 - Reza Nayebi, 8 hours ago : Added coercion for list expressions\
7f337da - Ben Kogan, 14 hours ago : Delete midi/ dir\
453daba - Ben Kogan, 14 hours ago : Update readme\
79ef10f - Ben Kogan, 14 hours ago : Merge branch 'master' of https://github.com/apollo-lang/apollo\
3e6232d - Ben Kogan, 14 hours ago : Update CLI ui\
78cc168 - Javier Llaca, 14 hours ago : Merge origin\
8db2f26 - Javier Llaca, 14 hours ago : Add zip, replicate, and uniform functions to standard library\
b18533c - Reza Nayebi, 14 hours ago : Merge branch 'master' of https://github.com/apollo-lang/apollo\
d61052c - Reza Nayebi, 14 hours ago : Coercion for Lists of Pitch/Duration\
c9dbcce - Ben Kogan, 14 hours ago : Delete dfasdf lol\
f7b35d7 - Ben Kogan, 15 hours ago : Add to std lib\
b10662e - Javier Llaca, 15 hours ago : Merge branch 'master' of https://github.com/apollo-lang/apollo\
e11dd02 - Javier Llaca, 15 hours ago : Fix build warnings\
1089723 - Ben Kogan, 15 hours ago : Add to std lib\
4db6ae0 - Reza Nayebi, 15 hours ago : Fixed type checking for Atom\
8db33d0 - Javier Llaca, 15 hours ago : Merge branch 'master' of https://github.com/apollo-lang/apollo\
fca0325 - Javier Llaca, 15 hours ago : Fix build warnings\
31dc922 - Souren Papazian, 15 hours ago : Merge pull request #76 from apollo-lang/new_tests\
43fd693 - SourenP, 15 hours ago : Merge branch 'new_tests' of https://github.com/apollo-lang/apollo into new_tests\
b0c363f - SourenP, 15 hours ago : fixed syntax\
8d98465 - SourenP, 15 hours ago : Merge branch 'master' of https://github.com/apollo-lang/apollo into new_tests\
fc3f468 - Reza Nayebi, 15 hours ago : Merge pull request #75 from apollo-lang/fix/atom-eval\
8bc86c2 - Javier Llaca, 15 hours ago : Merge branch 'master' of https://github.com/apollo-lang/apollo\
dbad761 - Javier Llaca, 15 hours ago : Update show and showPP for Expr\
8950796 - rja2139, 15 hours ago : crlf\
deab42d - Reza Nayebi, 15 hours ago : Type coercion for Duration\
ece24bc - rja2139, 15 hours ago : A few extra tests, taken from the LRM\
e8adaab - Reza Nayebi, 16 hours ago : Added comparison op for Pitch and Duration\
ab088e5 - SourenP, 16 hours ago : added pitchmod and added equality to pitchmod\
617455b - Roberto Amorim, 16 hours ago : Corrected some notation and one function\
2da7d16 - Javier Llaca, 16 hours ago : Merge branch 'master' of https://github.com/apollo-lang/apollo\
168b960 - Javier Llaca, 16 hours ago : Update ast string representation of Expr\
0e015fc - Reza Nayebi, 16 hours ago : Merge branch 'master' into fix/atom-eval\
e6b6283 - Ben Kogan, 16 hours ago : Add lambda evaluation with closures\
5b5c65f - Reza Nayebi, 16 hours ago : Added coercion for Atoms at eval time\
71df077 - SourenP, 16 hours ago : fixed comment test\
09df5d8 - SourenP, 16 hours ago : Merge branch 'master' of https://github.com/apollo-lang/apollo into new_tests\
98cd78f - SourenP, 16 hours ago : comment test\
ba40bcf - SourenP, 16 hours ago : new tests literal and pitchmod\
abd7bc1 - rja2139, 16 hours ago : Added a few more test lines\
94efa84 - Ben Kogan, 16 hours ago : Merge branch 'master' of https://github.com/apollo-lang/apollo\
b8da98d - Ben Kogan, 16 hours ago : Fix prelude load errors, remove raw-strings-qq dep\
c4eee7d - Javier Llaca, 17 hours ago : Merge build warning fixes\
3fe17b3 - Javier Llaca, 17 hours ago : Fix build warnings\
4468ca2 - Ben Kogan, 17 hours ago : Add std lib, :browse repl command\
3bce131 - SourenP, 17 hours ago : Merge branch 'new_tests' of https://github.com/apollo-lang/apollo into new_tests\
8ecf5b4 - rja2139, 17 hours ago : Further commenting\
c87adb7 - rja2139, 17 hours ago : Comment tests\
8f70190 - Reza Nayebi, 17 hours ago : Merge pull request #73 from apollo-lang/fix/coerce\
f9e2757 - Reza Nayebi, 17 hours ago : Merge branch 'master' into fix/coerce\
7a06fc6 - Souren Papazian, 17 hours ago : Merge pull request #72 from apollo-lang/new_tests\
63b8277 - Reza Nayebi, 18 hours ago : Fixed coercion for names\
16a3f89 - SourenP, 18 hours ago : moved empty out of future\
4bf1283 - SourenP, 18 hours ago : Merge branch 'master' of https://github.com/apollo-lang/apollo into new_tests\
38d6482 - Javier Llaca, 18 hours ago : Fix merge\
07d9938 - SourenP, 18 hours ago : actually added musicop\
be682c4 - SourenP, 18 hours ago : fixed musicop and moved out of future\
345ef85 - Javier Llaca, 18 hours ago : Merge branch 'master' of https://github.com/apollo-lang/apollo\
745fe94 - Javier Llaca, 18 hours ago : Merge add/closure with master\
6f05dc2 - Souren Papazian, 18 hours ago : Merge pull request #71 from apollo-lang/new_tests\
e631607 - SourenP, 18 hours ago : moved empty test into future (JMIDI) and moved funtypcheck out\
f4d9d1c - SourenP, 18 hours ago : Merge branch 'master' of https://github.com/apollo-lang/apollo into new_tests\
008d7d0 - Javier Llaca, 18 hours ago : Fix multi-line comments in lexer\
6289f1b - Reza Nayebi, 19 hours ago : Solving conflict\
c820bb1 - Javier Llaca, 19 hours ago : Add typed lambda expression and refactor grammar\
080ac86 - Reza Nayebi, 19 hours ago : Merge branch 'add/coerce-int'\
13c53fe - Reza Nayebi, 19 hours ago : Merge branch 'master' into add/coerce-int\
4f93caf - Reza Nayebi, 19 hours ago : Added Nil check for Atom\
18d33df - SourenP, 19 hours ago : Merge branch 'master' of https://github.com/apollo-lang/apollo into new_tests\
943b629 - Ben Kogan, 19 hours ago : Merge pull request #69 from apollo-lang/add/typecheck-functions\
193c9cf - SourenP, 19 hours ago : removed backtick in pitch literal from reference man\
49d09cc - Ben Kogan, 19 hours ago : Add type-checking for functions :bomb:\
043783d - SourenP, 19 hours ago : fixed pitch table\
cf42f01 - Reza Nayebi, 19 hours ago : Added Atom typechecking\
de19609 - Reza Nayebi, 19 hours ago : Can use integers in Atoms\
2023ec7 - Reza Nayebi, 20 hours ago : Merge branch 'new_tests' of https://github.com/apollo-lang/apollo into add/coerce-int\
de255fa - SourenP, 20 hours ago : fixed pitch table closes issue #9\
1087981 - Reza Nayebi, 21 hours ago : Fixed type coercion\
213ee58 - SourenP, 21 hours ago : removed redundant tests\
5853c8e - SourenP, 21 hours ago : Merge branch 'master' of https://github.com/apollo-lang/apollo into new_tests\
6cceae1 - Ben Kogan, 21 hours ago : Add map to firstclass.ap test\
5bc9f74 - SourenP, 22 hours ago : Merge branch 'master' of https://github.com/apollo-lang/apollo into new_tests\
71cf873 - SourenP, 22 hours ago : Merge branch 'new_tests' of https://github.com/apollo-lang/apollo into new_tests\
61d92c8 - SourenP, 22 hours ago : partial comment and musicop\
dca9d83 - Reza Nayebi, 22 hours ago : Fixed 1 warning\
c259b80 - Reza Nayebi, 22 hours ago : Added operations with musical types\
58059f7 - Ben Kogan, 22 hours ago : Merge branch 'add/closure'\
2597c49 - rja2139, 22 hours ago : Test high order functions\
0098cb0 - Ben Kogan, 23 hours ago : Add first-class functions with closures :bomb:\
045aa60 - Ben Kogan, 23 hours ago : Fix return-type of comparison operators\
fdbe19a - SourenP, 24 hours ago : added list test and renamed def to unbound\
295f605 - SourenP, 24 hours ago : Merge branch 'new_tests' of https://github.com/apollo-lang/apollo into new_tests\
fa8ae4c - Souren Papazian, 24 hours ago : Merge pull request #62 from apollo-lang/origin/new_tests\
e5a1234 - SourenP, 24 hours ago : fixed bin and un operator tests\
c25a5e2 - rja2139, 24 hours ago : Removed emptiness test\
78f11dd - SourenP, 24 hours ago : fixed bin and un operator tests\
e31cbb8 - rja2139, 24 hours ago : Boolean logic tests\
f066dff - rja2139, 25 hours ago : Added test to binop, variable assignments and operations\
b4b553c - Souren Papazian, 25 hours ago : Merge pull request #61 from apollo-lang/new_tests\
2cc0dff - SourenP, 25 hours ago : fixed typecheck\
84676e5 - SourenP, 25 hours ago : Merge branch 'master' of https://github.com/apollo-lang/apollo into new_tests\
8df1734 - SourenP, 25 hours ago : added more type checking\
a0d6b51 - Reza Nayebi, 25 hours ago : Merge pull request #56 from apollo-lang/add/head-tail\
53dba52 - Reza Nayebi, 25 hours ago : Turned Music into typedef for [[Atom]]\
acd3637 - Ben Kogan, 25 hours ago : Remove runTypeExpr, refactor interpret\
340b14d - SourenP, 25 hours ago : combined math and logic into binop\
4858115 - SourenP, 26 hours ago : assuming music is [[Atom]], new tests\
0305251 - SourenP, 26 hours ago : Merge branch 'master' of https://github.com/apollo-lang/apollo into new_tests\
b87f8de - SourenP, 26 hours ago : cleaned up and added tests\
7c4200d - Reza Nayebi, 26 hours ago : Added equality test for Lists\
1ade10b - rja2139, 26 hours ago : Removed recursion\
de587c1 - Reza Nayebi, 26 hours ago : Deleted Parts, use [Atom] in place, simplified Music syntax\
0bbf23b - rja2139, 26 hours ago : Added new test for list\
cef6801 - rja2139, 27 hours ago : Merge branch 'master' of ssh://github.com/apollo-lang/apollo\
3409f3f - rja2139, 27 hours ago : Some function tests, liften from language reference\
efbe6a8 - Javier Llaca, 27 hours ago : Merge branch 'add/closure' of https://github.com/apollo-lang/apollo into add/closure\
27e0286 - Souren Papazian, 27 hours ago : Merge pull request #60 from apollo-lang/new_tests\
498c089 - SourenP, 27 hours ago : minor test additions\
41e1472 - rja2139, 27 hours ago : Blank lines are now ignored. Script commenting\
957e06f - Reza Nayebi, 28 hours ago : List cannot be defined as empty\
2966203 - Ben Kogan, 28 hours ago : Add dot to show for lambda\
c699a3f - Ben Kogan, 28 hours ago : Re-add showVal renamed as showPP\
b98ce6d - Reza Nayebi, 30 hours ago : Fixed tail and added empty list type\
4d41315 - Souren Papazian, 30 hours ago : Merge pull request #58 from apollo-lang/new_tests\
f47f704 - SourenP, 30 hours ago : removed python, fixed bash backslash, new tests\
f5b9bad - Javier Llaca, 2 days ago : Merge branch 'master' of https://github.com/apollo-lang/apollo into add/closure\
cd1207c - Javier Llaca, 2 days ago : Add lambdas and syntax for higher-order functions\
01759a8 - SourenP, 2 days ago : not buggy, new tests\
8a46efa - Reza Nayebi, 2 days ago : Oops commited test file\
d409373 - Reza Nayebi, 2 days ago : Added support for Part too but we still need to find a way to make List types empty in a safe way\
f8b996f - Reza Nayebi, 2 days ago : Added head, tail and empty test for Lists\
e2b1a74 - Souren Papazian, 2 days ago : Merge pull request #55 from apollo-lang/new_tests\
0ca7c86 - SourenP, 2 days ago : replaces extension f with .tmp\
3f0a42d - SourenP, 2 days ago : dealt with backslash issue with python script\
6ace772 - Souren Papazian, 2 days ago : Merge pull request #54 from apollo-lang/fix-tempo\
fc649fb - Reza Nayebi, 2 days ago : Merge pull request #53 from apollo-lang/add/cons\
a15cc12 - SourenP, 2 days ago : fixed tempo\
325a387 - Reza Nayebi, 2 days ago : Added a cons operator\
14b38d6 - rja2139, 2 days ago : Updated notation answers file with Atoms\
0d605fd - Roberto Amorim, 2 days ago : Merge pull request #51 from apollo-lang/origin/fix/grammar\
33ecaf1 - Souren Papazian, 2 days ago : Merge pull request #45 from apollo-lang/fix-tempo\
dc8086b - SourenP, 2 days ago : fixed tempo rhs\
a4f2872 - Reza Nayebi, 2 days ago : Merge pull request #42 from apollo-lang/add_tempo\
2a6c269 - Ben Kogan, 2 days ago : Merge pull request #43 from apollo-lang/add/block-scope\
94982cc - Ben Kogan, 2 days ago : Add block scoping in both `eval` and `typecheck`\
ff25ce4 - SourenP, 2 days ago : added tempo to midi creation\
4ff40e3 - Reza Nayebi, 2 days ago : Clean up Main\
fbe10b9 - Reza Nayebi, 2 days ago : Fixed minus operator and added eval for main Music\
2483e7e - Javier Llaca, 2 days ago : Remove unnecessary functions from Util and update Parse and Expr accordingly\
491492c - rja2139, 3 days ago : ooops. Removed empty test again\
c88bbd0 - rja2139, 3 days ago : Merge branch 'master' of ssh://github.com/apollo-lang/apollo\
cab5eac - rja2139, 3 days ago : Empty files no longed crash Apollo. Test returning\
2cfb4ba - Javier Llaca, 3 days ago : Remove shorthand notation for rests\
5b16dd5 - Javier Llaca, 3 days ago : Add negative integers\
dd0caa9 - Javier Llaca, 3 days ago : Merge branch 'master' of https://github.com/apollo-lang/apollo\
7b33c08 - Javier Llaca, 3 days ago : Fix global tempo declaration syntax\
6ec8dcf - Reza Nayebi, 3 days ago : Outputing MIDI doesn't break the tests if main is not defined\
9c3bb58 - Javier Llaca, 3 days ago : Add global tempo declaration\
8a2ce74 - Reza Nayebi, 3 days ago : Started adding MIDI support\
952f09b - Javier Llaca, 3 days ago : Merge branch 'master' of https://github.com/apollo-lang/apollo\
459b389 - Javier Llaca, 3 days ago : Add recursive list types\
19e87cb - Reza Nayebi, 3 days ago : Added support for names in Part and Music\
b6c9eaf - Reza Nayebi, 3 days ago : Removed VNote, VChord and VRest and replaced it with VAtom\
f2401d6 - Javier Llaca, 3 days ago : Unify atoms\
d262229 - Javier Llaca, 3 days ago : Merge branch 'master' of https://github.com/apollo-lang/apollo\
c397d47 - rja2139, 3 days ago : Implemented help interface. My first code in Haskell!!!!\
6178fd9 - Javier Llaca, 3 days ago : Merge branch 'master' of https://github.com/apollo-lang/apollo\
6128a26 - Reza Nayebi, 3 days ago : Add checking for Notes\
6a211ea - Javier Llaca, 3 days ago : Merge branch 'master' of https://github.com/apollo-lang/apollo\
a00742c - Reza Nayebi, 3 days ago : Merge pull request #37 from apollo-lang/fix/grammar\
e86b38b - Reza Nayebi, 3 days ago : Fixed parsing error\
8983d90 - rja2139, 3 days ago : Added divide by zero tests\
c3a883f - Reza Nayebi, 3 days ago : Merge branch 'master' into fix/grammar\
908c3dd - Ben Kogan, 3 days ago : Merge branch 'master' of https://github.com/apollo-lang/apollo\
d10d9b2 - Ben Kogan, 3 days ago : Merge in changes from Master\
97ba415 - Ben Kogan, 3 days ago : Add type-env, generalize Env, improve type-checking\
0db2fc2 - Javier Llaca, 3 days ago : Merge branch 'fix/grammar' of https://github.com/apollo-lang/apollo\
9abac18 - rja2139, 4 days ago : Musical notation tests: Chord, Note, Rest\
69c75f1 - Reza Nayebi, 4 days ago : Use curly braces for Part\
b56e8bd - Reza Nayebi, 4 days ago : Changed Note, Chord and Rest syntax\
2009a83 - Javier Llaca, 4 days ago : Merge changes\
0c79d9e - Reza Nayebi, 4 days ago : Merge pull request #33 from apollo-lang/midi-util\
6e422e7 - Reza Nayebi, 4 days ago : Get rid of warnings\
59887a8 - Reza Nayebi, 4 days ago : Fix merge\
dcdce18 - Reza Nayebi, 4 days ago : Merge branch 'master' into midi-util\
e20bd9d - Reza Nayebi, 4 days ago : Merge branch 'master' of https://github.com/apollo-lang/apollo\
aad92c0 - Reza Nayebi, 4 days ago : Merge branch 'master' into midi-util\
63b98ee - SourenP, 4 days ago : lowered min base version to 4.6\
b8a0116 - Ben Kogan, 4 days ago : Refactor Main.hs\
decb0d5 - Ben Kogan, 4 days ago : Update travis config\
dd5f9b8 - Javier Llaca, 4 days ago : Merge branch 'master' of https://github.com/apollo-lang/apollo\
6c04703 - Reza Nayebi, 4 days ago : small fixes\
76a5c52 - Reza Nayebi, 5 days ago : Cleaning Up\
ace21c1 - Reza Nayebi, 5 days ago : added HCodecs dependency\
ce5bd96 - Reza Nayebi, 5 days ago : Add other constructor for Part\
4a0be62 - Reza Nayebi, 5 days ago : Merge branch 'master' of https://github.com/apollo-lang/apollo into midi-util\
233899f - Reza Nayebi, 5 days ago : Initial version of Part and Music types\
77e941a - Ben Kogan, 5 days ago : Update travis config\
6bd73ca - Reza Nayebi, 5 days ago : Merge branch 'master' of https://github.com/apollo-lang/apollo into midi-util\
d7dd1a8 - Ben Kogan, 5 days ago : Update build tool deps and travis config\
6d49ed6 - Ben Kogan, 5 days ago : Use `cabal build` in build system\
923bdae - Reza Nayebi, 5 days ago : Merge branch 'master' of https://github.com/apollo-lang/apollo\
0b92825 - rjamorim, 5 days ago : Cleaning up some tests\
c10161b - Reza Nayebi, 6 days ago : Merge branch 'master' of https://github.com/apollo-lang/apollo\
f9b4d6b - Javier Llaca, 13 days ago : Merge branch 'master' of https://github.com/apollo-lang/apollo\
60593d4 - Ben Kogan, 13 days ago : Refactor `run_tests` in run.sh\
238ccf0 - rjamorim, 13 days ago : Removing div by zero tests from test suite. Will come back later. Closes issue #28\
e9117dc - rjamorim, 2 weeks ago : Bunch of divide by zero tests\
8f17a04 - rjamorim, 2 weeks ago : Added a bunch of operator precedence checks\
54aee3b - rjamorim, 2 weeks ago : Small improvements\
fd2a3b5 - Ben Kogan, 2 weeks ago : Fix typo\
57b73cb - rjamorim, 2 weeks ago : Merge branch 'master' of github.com:apollo-lang/apollo\
ce80955 - rjamorim, 2 weeks ago : Fixed some issues with checking code, working fine now\
153b61d - Ben Kogan, 2 weeks ago : Fix typo\
e4db09e - rjamorim, 2 weeks ago : Implemented enhancement issue #17\
28d5146 - Reza Nayebi, 2 weeks ago : Merge branch 'master' of https://github.com/apollo-lang/apollo\
cade31b - Javier Llaca, 2 weeks ago : Merge branch 'master' of https://github.com/apollo-lang/apollo\
7dc5677 - Ben Kogan, 2 weeks ago : Add initial implementation of type checking stage\
8977390 - Ben Kogan, 2 weeks ago : Add initial implementation of type checking stage\
e5d21bb - Ben Kogan, 2 weeks ago : Merge branch 'master' of https://github.com/apollo-lang/apollo\
74f3d95 - Ben Kogan, 2 weeks ago : Improve `parseError`\
be1aa76 - Javier Llaca, 2 weeks ago : Add support for rests\
74bd662 - Javier Llaca, 2 weeks ago : Update pitch notation and add note and chord syntax sugar\
f47574b - Javier Llaca, 2 weeks ago : Reformat haskell source code according to style guide\
ea57b45 - Javier Llaca, 2 weeks ago : Update pitch table and pitch notation in reference manual\
82c0528 - Javier Llaca, 2 weeks ago : Add block test and update recursion test\
707b881 - Javier Llaca, 2 weeks ago : Update lexer and parser\
ee7bfba - Ben Kogan, 2 weeks ago : Update documentation\
607cf31 - Ben Kogan, 2 weeks ago : Update readme\
7a8e2b2 - Ben Kogan, 2 weeks ago : Update travis config\
fea44a9 - Ben Kogan, 2 weeks ago : Update travis config\
67a0f6b - Ben Kogan, 2 weeks ago : Merge branch 'master' of https://github.com/apollo-lang/apollo\
1415104 - Ben Kogan, 2 weeks ago : Initialize cabal config\
bec9d15 - Reza Nayebi, 2 weeks ago : Merge pull request #11 from apollo-lang/fix-midi\
4835576 - Reza Nayebi, 2 weeks ago : Removed record syntax to match src/ in midi/\
a7a0021 - Ben Kogan, 2 weeks ago : Expand `typeOf`\
4082999 - Ben Kogan, 2 weeks ago : Add typechecking to `matchI`\
2cd7f80 - Ben Kogan, 2 weeks ago : Unify IntOps using `matchI`\
4249126 - Ben Kogan, 2 weeks ago : Refactor pitch/duration eval to use `applyI`\
e1ce556 - Reza Nayebi, 2 weeks ago : Need to fix pattern matching for pitch/duration ops\
61d1681 - Reza Nayebi, 2 weeks ago : Fixed conflicts\
840d621 - Reza Nayebi, 2 weeks ago : Fix merge conflict\
612a31d - Ben Kogan, 2 weeks ago : Clarify module imports\
38504e0 - Ben Kogan, 2 weeks ago : Add basic scope test\
89cf5ed - Ben Kogan, 2 weeks ago : Change assignment evaluation to return Empty\
cfc3b80 - Ben Kogan, 2 weeks ago : Add comparison operators, fix recursion on int expressions\
27e4e84 - Reza Nayebi, 2 weeks ago : Fixed pattern matching for Pitch and Duration ops\
b9cb727 - Ben Kogan, 2 weeks ago : Add function call evaluation\
b106e9e - Reza Nayebi, 2 weeks ago : Added pitch and duration op and simplified testing for musical constructs\
ca283f7 - Ben Kogan, 2 weeks ago : Change symtab to include type information\
e5b4f84 - Ben Kogan, 2 weeks ago : Add lazy assignment\
871464b - Ben Kogan, 2 weeks ago : Rename Type constructors, copy necessary fns from Types.hs\
66b460e - Ben Kogan, 2 weeks ago : Change tmp/ to midi/ and remove duplicate files\
7e82ef4 - Reza Nayebi, 3 weeks ago : Added eval for Pitch and Duration\
bb94ff4 - Ben Kogan, 3 weeks ago : Fix executable make target\
5efdf28 - Ben Kogan, 3 weeks ago : Merge pull request #8 from apollo-lang/add/symtab\
71ac78e - Ben Kogan, 3 weeks ago : Add block evaluation\
62d4d35 - Ben Kogan, 3 weeks ago : Fix ast printing\
22bb75c - Ben Kogan, 3 weeks ago : Fix `parseAndEval` so it typechecks\
f921195 - Ben Kogan, 3 weeks ago : Add draft of `parseAndEval`\
a7e47eb - Reza Nayebi, 3 weeks ago : Merge branch 'master' of https://github.com/apollo-lang/apollo\
a66a9ba - Reza Nayebi, 3 weeks ago : Ignore MIDI and DS_Store files\
52c1a09 - Ben Kogan, 3 weeks ago : Add symbol table and environment\
c0cc72e - Ben Kogan, 3 weeks ago : Update travis config\
48a6959 - Ben Kogan, 3 weeks ago : Fix typo in travis config\
e51cb84 - Ben Kogan, 3 weeks ago : Add travis-ci config file\
40627a1 - Ben Kogan, 3 weeks ago : Update integration test-runner\
b454eb1 - Ben Kogan, 3 weeks ago : Refactor `main`\
77fe8ca - Ben Kogan, 3 weeks ago : Update Makefile to speed up dev builds\
ef10009 - Ben Kogan, 3 weeks ago : Remove broken test for empty input\
1ba8a09 - Ben Kogan, 3 weeks ago : Merge branch 'master' of https://github.com/apollo-lang/apollo\
88f1399 - Ben Kogan, 3 weeks ago : Refactor `eval` and type system\
c2622b9 - Roberto Amorim, 3 weeks ago : Added Linux instructions\
cf1447d - Reza Nayebi, 3 weeks ago : Few other fixes\
2561ff9 - Reza Nayebi, 3 weeks ago : Merge branch 'master' of https://github.com/apollo-lang/apollo\
bae5261 - Reza Nayebi, 3 weeks ago : Improved Makefile\
104051a - rjamorim, 3 weeks ago : Now we have ulimit to catch infinite loops\
0ea1905 - Reza Nayebi, 3 weeks ago : Merge pull request #6 from apollo-lang/extra_tests\
de31e74 - Reza Nayebi, 3 weeks ago : Added tests for MIDI generation in tmp\
a1b3001 - rjamorim, 3 weeks ago : Added usage documentation (-h / --help)\
0419867 - rjamorim, 3 weeks ago : Command line parameter --quiet/-q to supress error messages\
8a6a201 - SourenP, 3 weeks ago : also need Types for testing\
943e9e4 - SourenP, 3 weeks ago : setup for testing Midi.hs\
7c49ed0 - SourenP, 3 weeks ago : rename javiers old midi file\
5d5aee1 - rjamorim, 3 weeks ago : Less hackish compilation error detection\
4cd81f0 - rjamorim, 3 weeks ago : Now detects compilation errors. Can be improved\
b857b38 - rja2139, 3 weeks ago : Script now returns exit status 1 in case one of the tests fails\
0e4342e - rja2139, 3 weeks ago : Added another test, for robustness\
f6afb7f - rja2139, 3 weeks ago : Changed alert symbols to words as they do not work well on all systems\
1e22898 - Ben Kogan, 3 weeks ago : Merge pull request #5 from apollo-lang/fix/parsing\
7c24d28 - Ben Kogan, 4 weeks ago : Add `showVal` fn, fixes #4\
979d9df - Ben Kogan, 4 weeks ago : Merge Def type into Expr\
bf8568c - Ben Kogan, 4 weeks ago : Add more tests\
88577c0 - Ben Kogan, 4 weeks ago : Simplify `putAst`\
2af009c - Ben Kogan, 4 weeks ago : Update readme\
0abd779 - Ben Kogan, 4 weeks ago : Add repl\
f3aad87 - Ben Kogan, 4 weeks ago : Refactor main and file naming\
3e42f30 - Ben Kogan, 4 weeks ago : Add type reporting to error messages\
3984b04 - Ben Kogan, 4 weeks ago : Add result printing to main\
4aeef46 - Ben Kogan, 4 weeks ago : Merge branch 'master' of https://github.com/apollo-lang/apollo\
a1b1a98 - Ben Kogan, 4 weeks ago : Refactor to use error monad\
4c83ae3 - Reza Nayebi, 4 weeks ago : Update Types.hs\
1b9a7c6 - Reza Nayebi, 4 weeks ago : Update Types.hs\
746dad6 - Reza Nayebi, 4 weeks ago : Update Midi.hs\
9b4f26c - Javier Llaca, 4 weeks ago : Merge branch 'master' of https://github.com/apollo-lang/apollo\
dd90e3b - Javier Llaca, 4 weeks ago : Unify expressions in Expr and Parser\
112c434 - Reza Nayebi, 4 weeks ago : Renamed Midi.hs\
d5b396e - Reza Nayebi, 4 weeks ago : Renamed Midi.hs\
89a354f - SourenP, 4 weeks ago : back end\
4db24a8 - Ben Kogan, 5 weeks ago : Modify `printList` to be more readable\
717943f - Ben Kogan, 5 weeks ago : Add math tests\
5e0fea9 - Ben Kogan, 5 weeks ago : Move blocks.ap and def.ap tests to future dir\
8f941ef - Ben Kogan, 5 weeks ago : Merge branch 'master' of https://github.com/apollo-lang/apollo\
3fbd3e4 - Ben Kogan, 5 weeks ago : Add list evaluation and list test\
c4dad2f - Ben Kogan, 5 weeks ago : Change printing of program result\
6e07b01 - Ben Kogan, 5 weeks ago : Add Show instance for Expr\
1ad23ca - Ben Kogan, 6 weeks ago : Update README.md\
b54eb62 - Ben Kogan, 6 weeks ago : Update Makefile and README\
f2f34dc - Ben Kogan, 6 weeks ago : Add `make test` target, add example tests, update run.sh\
1d3a543 - Ben Kogan, 6 weeks ago : Add script to run integrations tests\
70fed58 - Ben Kogan, 6 weeks ago : Change naming and ordering in Main\
740a0a3 - Ben Kogan, 6 weeks ago : Remove extra `map` from `readExpr`\
dd4ceeb - Ben Kogan, 6 weeks ago : Add unnary operation evaluation\
c1e7b97 - Ben Kogan, 6 weeks ago : Fix typo in Eval.hs (take ii)\
466ae5d - Ben Kogan, 6 weeks ago : Fix typo in Eval.hs\
6088118 - Ben Kogan, 6 weeks ago : Add binary operation evaluation\
5ec1c52 - Ben Kogan, 6 weeks ago : Add `--ast` flag, update readme\
47bda38 - Ben Kogan, 6 weeks ago : Merge branch 'master' of https://github.com/apollo-lang/apollo\
ae7b88d - Ben Kogan, 6 weeks ago : Add basic Eval and simple conditional test\
3057eef - Javier Llaca, 6 weeks ago : Add syntax rules to compiler front end\
f7628f5 - Javier Llaca, 6 weeks ago : Update lexer, parser, and data type modules to read short-hand notation for pitch and duration\
cb45be5 - Javier Llaca, 6 weeks ago : Update LRM\
69179c9 - Javier Llaca, 6 weeks ago : Add precedences to parser and update Expr module\
98b5540 - Ben Kogan, 6 weeks ago : Update Makefile\
0939f94 - Ben Kogan, 6 weeks ago : Merge branch 'master' of https://github.com/apollo-lang/apollo\
9b00430 - Ben Kogan, 6 weeks ago : Fix test typo, organize src/, update Makefile\
ffdb89f - Reza Nayebi, 6 weeks ago : Merge branch 'master' of https://github.com/apollo-lang/apollo\
3d18a84 - Reza Nayebi, 6 weeks ago : Deleted unused argument\
31858f4 - Reza Nayebi, 6 weeks ago : Update Types.hs\
1d9283d - Reza Nayebi, 6 weeks ago : Updated typed\
961b32a - Javier Llaca, 6 weeks ago : Update Apollo tests\
0f35cc9 - Javier Llaca, 6 weeks ago : Add Apollo grammar file\
6a158b1 - Javier Llaca, 6 weeks ago : Update lexer, parser, and data type modules\
2d8dbbb - Javier Llaca, 6 weeks ago : Update lexer and parser\
1e62b2a - Ben Kogan, 7 weeks ago : Delete old ref manual, rename lrm\
57170a0 - Ben Kogan, 7 weeks ago : Merge branch 'master' of https://github.com/apollo-lang/apollo\
271997f - Ben Kogan, 7 weeks ago : Fix typo in tutorial.md\
54f7e3e - Javier Llaca, 7 weeks ago : Merge branch 'master' of https://github.com/apollo-lang/apollo\
e02d4c9 - Javier Llaca, 7 weeks ago : Update LRM\
1a91d51 - Reza Nayebi, 7 weeks ago : Update lrm.md\
b6a3a1d - Ben Kogan, 7 weeks ago : Merge branch 'master' of https://github.com/apollo-lang/apollo\
15274d9 - Ben Kogan, 7 weeks ago : Add tutorial.md\
40ff6e6 - Javier Llaca, 7 weeks ago : Update LRM\
ea8b7ae - Javier Llaca, 7 weeks ago : Update LRM\
a86f4fe - Javier Llaca, 7 weeks ago : Update LRM\
28082f9 - Javier Llaca, 7 weeks ago : Update LRM\
b9190af - Javier Llaca, 7 weeks ago : Update LRM\
fe07132 - Javier Llaca, 7 weeks ago : Merge LRM changes\
80a8a70 - Javier Llaca, 7 weeks ago : Update LRM\
642a695 - Reza Nayebi, 7 weeks ago : Update lrm.md\
8b93110 - Reza Nayebi, 7 weeks ago : Added example description in control flow section\
9420a98 - Reza Nayebi, 7 weeks ago : Update lrm.md\
6bc81bd - Javier Llaca, 7 weeks ago : Update LRM\
ef7fe2f - Javier Llaca, 7 weeks ago : Merge branch 'master' of https://github.com/apollo-lang/apollo\
d18e881 - Javier Llaca, 7 weeks ago : Update LRM and format lines to a max of 80 chars\
61b199f - Ben Kogan, 7 weeks ago : Update LRM\
6b00537 - Ben Kogan, 7 weeks ago : Update LRM\
e5377d6 - Ben Kogan, 7 weeks ago : Add footnote to LRM\
c17fdef - Ben Kogan, 7 weeks ago : Add metadata to LRM, update Makefile\
b4fab49 - Ben Kogan, 7 weeks ago : Update docs readme\
08d413b - Ben Kogan, 7 weeks ago : Update docs readme\
eb61f64 - Ben Kogan, 7 weeks ago : Add LRM and docs Makefile, update gitignore\
531b8fc - Ben Kogan, 7 weeks ago : Update Makefile, tweak apollo.hs to point-free notation\
17e73c0 - Javier Llaca, 8 weeks ago : Update tests and add .gitignore dot file\
751d165 - Javier Llaca, 8 weeks ago : Add Makefile and tests\
83c8755 - Javier Llaca, 8 weeks ago : Add alex/happy lexing/parsing modules, AST data types module, and parser driver.\
f453d16 - Ben Kogan, 9 weeks ago : Update reference manual\
c511fc4 - Ben Kogan, 2 months ago : Fix typo in reference_manual.md\
142d0cc - Ben Kogan, 2 months ago : Update fn-syntax comparison in reference manual\
7dfb259 - Javier Llaca, 2 months ago : Add reference manual\
e67d3ec - Javier Llaca, 2 months ago : Update simple parser\
5df299c - Javier Llaca, 2 months ago : Merge branch 'master' of https://github.com/apollo-lang/apollo\
24c78be - Javier Llaca, 2 months ago : Add simple parser\
128c4b6 - Ben Kogan, 2 months ago : Merge branch 'master' of https://github.com/apollo-lang/apollo\
e096ab3 - Ben Kogan, 2 months ago : Fix whitepaper typo\
6ed3b22 - Javier Llaca, 2 months ago : Add type and midi modules\
0467653 - Ben Kogan, 2 months ago : Finish whitepaper\
41bd329 - Ben Kogan, 3 months ago : Add whitepaper use case (wip)\
44faebc - Ben Kogan, 3 months ago : Add whitepaper language notes\
e8452d8 - Ben Kogan, 3 months ago : Update whitepaper notes\
6f00e50 - Ben Kogan, 3 months ago : Add whitepaper rationale\
a0ef956 - Ben Kogan, 3 months ago : Update whitepaper introduction\
6d5f4ee - Javier Llaca, 3 months ago : Update whitepaper\
87306b0 - Javier Llaca, 3 months ago : Add language whitepaper\
f4f5603 - Javier Llaca, 3 months ago : Add README