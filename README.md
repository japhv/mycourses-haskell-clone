# mycourses-clone

Group Members:
	  
    + Japheth Abraham
    + Jason St. George

## Installation
Instructions on how to compile/run/use the project:

Clone repo using
> git clone https://github.com/japhv/mycourses-haskell-clone.git

> stack setup

the stack.yaml file will automatically download and install required dependencies upon building
> stack build

Running the server
> stack exec mycourses-clone-exe

## API Architecture

### Courses
|   Endpoint   | REST   |  Functions			  |
|---	       |--- 	|---				  |
| /courses     | GET    | Gets a list of courses          |  
| /courses     | POST   | Creates a new courses           |
| /courses/:id | GET    | Gets a single course by /:id    |  
| /courses/:id | PUT    | Updates a single course by /:id | 
| /courses/:id | DELETE | Deletes a single course by /:id | 


### Students
|   Endpoint	| REST   |  Functions			    |
|---		|--- 	 |---				    |
| /students     | GET    | Gets a list of students          |  
| /students     | POST   | Creates a new student            |
| /students/:id | GET    | Gets a single student by /:id    |  
| /students/:id | PUT    | Updates a single student by /:id | 
| /students/:id | DELETE | Deletes a single student by /:id | 
| /students/:id/courses/:courseId | POST | Creates a new course entry for a single student |   



Include descriptions of approaches attempted and then abandoned and the reasons why:
	
	We attempted using Yesod initially because it boasted static type-checking for all
	components of a webapp, as well as several custom DSLs, including Persistend. 
	After some experimentation, found that it was very difficult to use up-front 
	and, while interesting, had a large learning curve to learn in the timeframe for 
	this project.  The high complexity led us to look for an alternative, Snap Web 
	Framework for Haskell instead due to simplicity and more abundant usage/simpler 
	documentation.  Snap is more minimal as a framework and easier to implement from 
	scratch without much learning curve, as Yesod felt more like a heavyweight framework 
	which you maybe able to do more with, but required much more code and understanding.  
	The templates that came with Yesod had lots of code that was difficult to 
	differentiate what was needed and removing unnecessary code became a burden. You can
	utilize persistent with Snap, and many of the features of the different big 3 web APIs
	for Haskell are actually relatively interchangable, contrary to popular perception. 
	The template system for Snap, Heist, reats templates at runtime instead of compile-time
	like Hamlet, which Yesod is based on.

Overview of work done and tools used:

	Cabal Sandbox: We used a sandbox initialized with the dependencies specifiec in 
	the project stack.yaml file to separate the project into its own environment, 
	similar to a python venv, so that it does not interfere with global settings and 
	other projects or files.  The cabal executible is used by stack for the resolver 
	and then uses caball the library to build the code for the project.

List any additional Haskell libraries required for the project (i.e., what should be cabal installed):

	+ GHC language features and extensions:
	{-# LANGUAGE
		  OverloadedStrings: Strings are not polymorphic, the extension fixes this.  By 
		    overloading String typeclass to be polymorphic over the IsString typeclass.  
		    Adds IsString to defaultabe typeclass, so you can write String, Text, Bytestring, 
		    in a default declaration.

		, EmptyDataDecls: Allows definition of data types with no constructors, mostly for 
		  use with Phantom parameters

		, GeneralizedNewtypeDeriving:  Allows for newtypes to inherit instances from its 
		    representation.  I.e. not have to write an instance declaration of Num:
			  instance Num Dollars where
			    Dollars a + Dollars b = Dollars (a+b)

			for when you declare
			  newtype Dollars = Dollars Int

		  and just use the Int dictionary, which is faster. 

		, MultiParamTypeClasses: Allows for the definition of typeclasses to have more than 
			one parameter.
		
		, DeriveGeneric: Allows automatic deriving of instances of the Generic typeclass
		
		, GADTs: Generalized Algebraic Data Types, allow explicitly write types of 
			constructors in declaration 
		
		, TypeFamilies: Allows association of type classes and type functions so as to 
			simplifly carrying around declarations and for defining the actual 
			associated data type constructors right with instances.
		
		, TemplateHaskell: A simplified DSL that compiles into full Haskell code, eliminates 
			boiler-plate but is NOT typesafe.
		
		, QuasiQuotes: A QuasiQuoter is a function that takes a string and returns a Q Exp, 
			Q Pat, Q Type or Q [Dec].
		
		, FlexibleInstances: Allow definition of type class instances with arbitrary nested 
			types in the instance head.
		
		, FlexibleContexts: Allow the use of complex constraints in class declaration contexts, 
			(e.g. Show for associated Type Families and Type Functions)
		
		, StandaloneDeriving: Can derive instances for GADTs and other exotic data types, 
			provided that the boilerplate code typechecks.
			E.g. data T a where
   				 	T1 :: T Int
   					T2 :: T Bool
				 deriving instance Show (T a)
			Where you can't write ... deriving( Show ) on the data type declaration 
			for T, because T is a GADT, but you can generate the instance declaration 
			using stand-alone deriving.

		 #-}

	+ Snap Web Framework: A Haskell web server implementation framework that uses left-fold 
	enumerators (for more predictable space usage than Lazy IO) which provides a set of
	combinators for simple and complex routes.  The authors admit it is a slimmed down version
	of Happstack.

	+ Persistent Data storage: The Persistent module in Haskell allows type-safe data-access.  
	To avoid much of the boiler-plate, we utilize TemplateHaskell and it's simplified exposure.  
	The Persistent module is a series of layers that stack together to achieve type-safe 
	communication without serious code overhead for the programmer.  For example, the 
	PersistValue is a datatype that translates relevant values into something the database 
	can understand, like VARCHAR or INTEGER, while the next layer PersistField defines how a 
	Haskell data type can be 'marshaled' into something of PersistValue type.  Essentially, a 
	PersistFiled corresponds to a column in an SQL database.  The third layer is the 
	PersistEntity typeclass, which is the table in a SQL databsase.  runSqlite is a monad that 
	creates a single connection to the database using its argument connection string.  
	runSqlite only runs a single transaction, by binding multiple steps into one transaction, 
	and gives the all-or-nothing update constraint that we are used to having in SQL.  
	It provides a series of functions to query the database written in Haskell.  

	+ Aeson: A parsing and encoding module in Haskell that is designed for high performance.

	+ REST API Specification:
		Student will have GET, POST, PUT, DELETE

	+ MVC specification:
		The model in our MVC is implemented as TemplateHaskell using Persistent, 
		and activated by activating the GHC extensions {# TemplateHaskell, Persistent}. 
		These allow us to speifcy the entities we wish to create in the database once at a 
		high level, which is then compiled into full Haskell by TH [TemplateHaskell], so that 
		the user does not have to construct large amounts of boiler-plate code.  GADTs then 
		encode both the type of the entity and the type of the field when defining our model.  
		The entities we defined are: Student, Course,...

Briefly describe the structure of the code (what are the main components, the module dependency structure):
 
 
    Student json
      firstname String
      lastname String
      email String
      status String
      UniqueEmail email
      deriving Show Generic

    Course json
      title String
      code String
      department String
      numberOfCredits Int
      UniqueCode code
      deriving Show Generic

    CoursePrequisites
      courseId CourseId
      prereqId CourseId Maybe default=NULL

    StudentCourses
      studentId StudentId
      courseId CourseId Maybe default=NULL
      grade  


What was learned by undertaking the project:

	Even though one of us was somewhat familiar with web frameworks, doing this in 
	Haskell had a high enough learning curve to be both frustrating and rewarding.  
	One member had no prior experience and learned much about creation of web services
	applications, REST APIs and protocols, MVC (Model-View-Controller) paradigm, as 
	well as what a RESTful service is, REST API protocols, Postman, Sqlite, and .  
	Both of us learned much about implementing an MVC paradigm in Haskell with
	web APIs with Snap, Haskell Sandboxing, Haskell Templates, Persistence module, Database 
	connectivity in Haskell, as well as various other GHC language extensions that are 
	very helpful, including Type Families, Multi Parameter Type Classes, OverloadedStrings, 
	DeriveGeneric, GADTs, lifting with LiftIO (common functions like fmap are lifting operations), 
	JSON parsing for this specific application, and working with Lazy ByteStrings in Haskell.

Challenges:
  
    + The community is small, so we found we had to come up with solutions after much blood, sweat, and tears.

NOTE:
	+ See mycourses-clone.cabal for a more complete list imports and dependencies