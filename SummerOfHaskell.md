## Help CodeWorld by participating in Summer of Haskell 2017

CodeWorld will be participating in Summer of Haskell again 2017!  Please spread the word to college
students you know.  This is an opportunity to earn money over the summer, while contributing to a
project that is open source, working to improve educational opportunities for children, and involves
some cool technology like Haskell, GHCJS, and the possibility for a lot more.

## Writing a Proposal

Students who apply will propose a specific project they are interested in.  To help with this task,
I've collected below some thoughts on promising student projects.

Writing a proposal for Summer of Haskell takes time, so don't plan to do it all at the last minute.
It is an excellent idea to discuss your idea beforehand to build support and work out the kinks. 
Some hallmarks of a strong proposal are:

* A student who understands CodeWorld and has made connections and gotten involved in the community.
* A well-thought-out clearly described goal that offers benefits for existing CodeWorld users.
* A project plan, with enough detail to set reasonable standards of success throughout the summer.

A typical proposal is in the neighborhood of a couple pages.  A paragraph is definitely too short.
There is no such thing as too much information; however, if you take ten pages to get to the good
part, it's possible reviewers will give up before reading that far.

## Project Ideas

Idea: Packaging student programs as mobile applications
-|
Issue #22, and related to #449.  Instead of just running projects in the web browser, the project would be to export a student's program as a tangible result.  The main target would be Android and iOS apps, which can be built using Cordova from the same JavaScript run in the browser today.  Pictures and animations could be converted to images, animated gifs, or videos, and easily shared with Instagram, YouTube, etc.

Idea: Better debugging and editor tooling
-|
The editor tooling for CodeWorld could use improvement.  Smaller tasks include expanding auto-complete to include symbols defined in the current program, continuously highlighting syntax errors, adding documentation on hover, and navigation from definitions to uses.  Possible debugging features include better controls for playing animations or simulations in slow-motion or reverse, rewind and replay, pause on event, or breakpoints.  Another promising idea is to use GHC 8 stack trace information to let students click parts of the drawing and jump to the line of code responsible for the shape.

Idea: Better collaboration features in CodeWorld
-|
Students in CodeWorld often share bits of code or work together on projects.  One possible direction is to build features that make this easier.  Parts of the task might be as easy as integrating Mozilla's together.js library to allow collaborative editing.  Letting students build up libraries of functions that are available across students and projects would be interesting.  Creating galleries for groups of students to share and comment on each other's work is also an exciting possibility.

Idea: Improvements to collaborations
-|
The newest feature of CodeWorld is the API and relay system for collaborations: multi-user networked programs that on a shared state.  This feature works, and is already being incorporated into curriculum and classes!  But there is room for improvement: switching from WebSockets to WebRTC, more features for the "lobby" that connects players together, a spectator option to let others observe games, better state interpolation, desync detection, and more.  A catalog of projects is at https://github.com/google/codeworld/projects/1

Idea: CodeWorld web site features to help students and teachers
-|
A number of changes to the CodeWorld web site are possible that would make it more useful for students and teachers.  Perhaps a well-integrated Q&A feature would make it easier for students to get help on a program that doesn't build.  Perhaps an assignment box would let teachers keep track of their assignments.  Or perhaps these goals are better achieved by integration with existing tools like Google Classroom and StackExchange.  A student who proposed building a set of reasonable class tools might find success.

Idea: Improvements to parsing, compiling, and errors
-|
Error messages remain a mystery to most CodeWorld students.  There is already a system in place for regex-based replacement of error messages, but uses are mostly limited to removing the worst parts of errors.  This project would be to expand efforts to make Haskell more friendly to children and beginners.  This could include: beefing up the error message rewriter with test cases and more advanced rules; preprocessing the code to look for common mistake patterns; and enforcing rules such as use of parentheses around function arguments.

Idea: Providing for customized CodeWorld libraries.
-|
CodeWorld is a great stand-alone environment, but in some cases, teachers may want to integrate it into other curricula.  A physics class might want to use a graphical version of [BeamStack](https://hackage.haskell.org/package/learn-physics-0.6.0.2/docs/Physics-Learn-BeamStack.html), or let students experiment with a port of [hamilton](https://hackage.haskell.org/package/hamilton).  A biology class might start with a library for population simulations.  A student could propose a system for allowing teachers or designers to create customized CodeWorld configurations with a different set of base libraries.  Supporting project-based education integrated into other classes would be very useful.

Idea: Finishing the CodeWorld Blocks UI
-|
There are a large number of outstanding bugs and feature requests in CodeWorld Blocks. A student could propose reviving this project with a new approach, or just a solid summer of big fixes, design changes, and new features.  However, note that this would be a high risk project, which may not yield benefits unless it's done extremely well.  A catalog of outstanding work is at https://github.com/google/codeworld/projects/2
