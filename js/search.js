// When the user clicks on the search box, we want to toggle the search dropdown
function displayToggleSearch(e) {
  e.preventDefault();
  e.stopPropagation();

  closeDropdownSearch(e);
  
  if (idx === null) {
    console.log("Building search index...");
    prepareIdxAndDocMap();
    console.log("Search index built.");
  }
  const dropdown = document.querySelector("#search-dropdown-content");
  if (dropdown) {
    if (!dropdown.classList.contains("show")) {
      dropdown.classList.add("show");
    }
    document.addEventListener("click", closeDropdownSearch);
    document.addEventListener("keydown", searchOnKeyDown);
    document.addEventListener("keyup", searchOnKeyUp);
  }
}

//We want to prepare the index only after clicking the search bar
var idx = null
const docMap = new Map()

function prepareIdxAndDocMap() {
  const docs = [  
    {
      "title": "Applicative Effects",
      "url": "/turbolift/advanced/applicative.html",
      "content": "Applicative Effects Parallellism Two independent computations can be combined, giving potential for their parallel execution: val foobar = foo *! bar The *! operator is an alias for zipPar method (see Computation API). The possibility of parallelism, depends on implementation of handlers. Parallelism is possible only when all handlers in the currently used effect stack, are implemented to permit parallelism. If parallelization is possible, 2 fibers1 for foo and bar are implicitly forked. Upon joining, results of each contributing effect are composed, in a similar manner as in composed Applicative Functor2. If parallelization is not possible, zipPar fallbacks to sequential zip. Example: Applicative vs. Monadic error Depending on selected handler, given program will either: Attempt to execute both branches sequentially, but will stop on the first error. Attempt to execute both branches parallelly, and will collect both errors. import turbolift.!! import turbolift.std_effects.ErrorK case object MyError extends ErrorK[List, String] val program = MyError.raise(\"foo\") &amp;! MyError.raise(\"bar\") // program: Computation[Nothing, MyError] = turbolift.Computation@505f589c val result1 = program.handleWith(MyError.handlers.first).run // result1: Either[List[String], Nothing] = Left(value = List(\"foo\")) val result2 = program.handleWith(MyError.handlers.all).run // result2: Either[List[String], Nothing] = Left(value = List(\"foo\", \"bar\"))   Currently, fibers are not exposed to user. 🚧 WIP 🚧 &#8617; Turbolift does not use Applicative typeclass. It’s only mentioned as an analogy. &#8617;"
    } ,    
    {
      "title": "Continuation stored in state",
      "url": "/turbolift/advanced/cont.html",
      "content": "🏗️ 🚧 WIP 🚧"
    } ,    
    {
      "title": "Effect example: File System",
      "url": "/turbolift/custom/file_sys.html",
      "content": "Effect example: File System In the Haskell community, there are many effect systems positioning themselves as an alternative to the mainstream MTL. The File System effect example is often used as their demonstrator of reinterpretation. This is how it looks in Turbolift: Definition 1. Imports import turbolift.{!!, Signature, Effect, Handler} import turbolift.std_effects.{State, Error} 2. Define the signature trait FileSystemSignature extends Signature: def readFile(path: String): String !@! (ThisEffect &amp; FileError) def writeFile(path: String, contents: String): Unit !@! ThisEffect Let’s also instantiate the custom FileError effect, that we used in the signature: case object FileError extends Error[FileErrorCause] type FileError = FileError.type enum FileErrorCause: case NoSuchFile(path: String) def message = this match case NoSuchFile(path) =&gt; s\"No such file found: $path\" 3. Define the effect type trait FileSystemEffect extends Effect[FileSystemSignature] with FileSystemSignature: // Boilerplate: final override def readFile(path: String) = perform(_.readFile(path)) final override def writeFile(path: String, contents: String) = perform(_.writeFile(path, contents)) 4. Define a handler extension (fx: FileSystemEffect) def inMemoryHandler = case object InternalStorage extends State[Map[String, String]] new fx.Proxy[InternalStorage.type] with FileSystemSignature: override def readFile(path: String) = InternalStorage.gets(_.get(path)).flatMap { case Some(contents) =&gt; !!.pure(contents) case None =&gt; FileError.raise(FileErrorCause.NoSuchFile(path)) } override def writeFile(path: String, contents: String) = InternalStorage.modify(_.updated(path, contents)) .toHandler .provideWith(InternalStorage.handler(Map()).dropState) The provideWith method composes 2 dependent handlers, such that the latter handles dependency introduced by the former (InternalStorage effect in this case). Usage Instantiate the effect case object MyFS extends FileSystemEffect // Optional: type MyFS = MyFS.type Run a program using the effect &amp; handler val program = for _ &lt;- MyFS.writeFile(\"hello.txt\", \"Hello world!\") contents &lt;- MyFS.readFile(\"hello.txt\") yield contents // program: Computation[String, ThisEffect &amp; FileError] = turbolift.Computation@1a8c01d1 val result = program .handleWith(MyFS.inMemoryHandler) .handleWith(FileError.handler) .run // result: Either[FileErrorCause, String] = Right(value = \"Hello world!\")"
    } ,    
    {
      "title": "Effect example: Flip",
      "url": "/turbolift/custom/flip.html",
      "content": "Effect example: Flip Flip effect seems to be the “Hello world” of the Algebraic Effects literature. This is how it looks in Turbolift: Definition 1. Imports import turbolift.{!!, Signature, Effect, Handler} 2. Define the signature trait FlipSignature extends Signature: def flip: Boolean !@! ThisEffect def fail: Nothing !@! ThisEffect 3. Define the effect type trait FlipEffect extends Effect[FlipSignature] with FlipSignature: // Boilerplate: final override def flip = perform(_.flip) final override def fail = perform(_.fail) // Auxiliary operations: final def plus[A, U &lt;: ThisEffect](lhs: =&gt; A !! U, rhs: =&gt; A !! U): A !! U = flip &gt;&gt;= (if _ then lhs else rhs) final def select[A](as: Iterable[A]): A !! ThisEffect = if as.isEmpty then fail else plus(!!.pure(as.head), select(as.tail)) The auxiliary operations, plus and select, are not declared in the signature. That’s because they don’t need dynamic semantics, provided by handlers. They are defined entirely in terms of flip and fail. 4. Define a handler Or better, two handlers: extension (fx: FlipEffect) def findAll = new fx.Stateless[Vector] with fx.Sequential with FlipSignature: override def onPure[A](a: A) = Vector(a) override def fail = _ =&gt; !!.pure(Vector()) override def flip = k =&gt; for as &lt;- k(true) bs &lt;- k(false) yield as ++ bs .toHandler extension (fx: FlipEffect) def findFirst = new fx.Stateless[Option] with fx.Sequential with FlipSignature: override def onPure[A](a: A) = Some(a) override def fail = _ =&gt; !!.pure(None) override def flip = k =&gt; k(true).flatMap { case None =&gt; k(false) case some =&gt; !!.pure(some) } .toHandler Usage Instantiate the effect case object MyFlip extends FlipEffect // Optional: type MyFlip = MyFlip.type Run a program using the effect &amp; handlers def isOdd(x: Int) = x % 2 == 1 val program = for x &lt;- MyFlip.select(1 to 4) _ &lt;- !!.when(isOdd(x))(MyFlip.fail) y &lt;- MyFlip.select('a' to 'b') yield s\"$x$y\" // program: Computation[String, MyFlip] = turbolift.Computation@635f9df0 val result1 = program.handleWith(MyFlip.findAll).run // result1: Vector[String] = Vector(\"2a\", \"2b\", \"4a\", \"4b\") val result2 = program.handleWith(MyFlip.findFirst).run // result2: Option[String] = Some(value = \"2a\")"
    } ,    
    {
      "title": "Higher Order Effects",
      "url": "/turbolift/advanced/higher.html",
      "content": "Higher Order Effects a.k.a Scoped Effects. HOEs are problematic New programming languages with native Algebraic Effects, generally don’t support HOEs. Exceptions are: Frank language. Unison language, which implements Frank’s effect system. According to the underlying theory, HOEs are actually non-algebraic ⚠️λ1. The Eff Monad doesn’t support HOEs. Monad Transformers do support HOEs. However, there are some known problems. Such as effect’s semantics being dependent on the order of monad transformers in the stack. More info on the subject: Unresolved challenges of scoped effects ⚠️λ1 video. Effect Semantics Zoo ⚠️λ1 HOEs in Turbolift In this example, we run the given program twice, with 2 orderings of handlers: Error handled before State. State handled before Error. We observe consistent behavior: in both cases, raising the error didn’t cause the State to reset to it’s value from before the catchAll operation. import turbolift.!! import turbolift.std_effects.{Error, State} case object MyError extends Error[String] case object MyState extends State[Int] val program = MyError.catchAll { MyState.put(42) &amp;&amp;! MyError.raise(\"error\") } { case _ =&gt; !!.pure(\"nvm\") } // program: Computation[String, MyState &amp; MyError] = turbolift.Computation@1e13e6e5 val result1 = program .handleWith(MyError.handler) .handleWith(MyState.handler(0)) .run // result1: Tuple2[Either[String, String], Int] = (Right(value = \"nvm\"), 42) val result2 = program .handleWith(MyState.handler(0)) .handleWith(MyError.handler) .run // result2: Either[String, Tuple2[String, Int]] = Right(value = (\"nvm\", 42)) 🎁 Bonus feature The implementation of HOEs in Turbolift has an accidental consequence. It’s possible to write an alternative handler for the Error effect, so that it replicates behavior of Monad Transformers. With such handler, state is transactional, when handled before error. Warning: Haskell code ahead. &#8617; &#8617;2 &#8617;3"
    } ,    
    {
      "title": "Advanced Features",
      "url": "/turbolift/advanced/",
      "content": "Advanced Features Labelled Effects Applicative Effects Higher Order Effects"
    } ,    
    {
      "title": "Defining your own effects and handlers",
      "url": "/turbolift/custom/",
      "content": "Defining your own effects and handlers 1. Define a signature for your effect import turbolift.Signature trait GoogleSignature extends Signature: def countPicturesOf(topic: String): Int !@! ThisEffect 😱 !@! !@![_, ThisEffect] is somewhat similar to F[_] from Tagless Final. The main difference, is that in Turbolift this abstraction is short lived. It’s local to Signature, and it vanishes in Effect. Both !@! and ThisEffect are defined as abstract type members of Signature. In the next step, once we inherit our signature from an Effect, they become (automatically) concretized: !@![_, _] becomes an alias of Computation[_, _] (just like !!) ThisEffect becomes an alias of this.type Effectively, when writing custom signature, A !@! ThisEffect should be regarded as A !! this.type, in temporary disguise. 2. Define your effect type This step is mostly mechanical. For each abstract method we have defined in the signature, we must provide boilerplate implementation stub, using perform (provided by Effect). import turbolift.Effect trait Google extends Effect[GoogleSignature] with GoogleSignature: // Boilerplate: final override def countPicturesOf(topic: String) = perform(_.countPicturesOf(topic)) ⚠️ Notice: The signature is used here twice: first as the type parameter and second as the super trait. 3. Define a handler for your effect Now it’s time to make an important choice. There are 2 ways to assign semantics to our effect’s operations: With Flow interpreter: by using delimited continuations. See Flip effect example for details. With Proxy interpreter: by delegating to other effects (a.k.a “reinterpretation”). See File System example for details. Once the interpreter is defined, we can obtain a handler from it, using toHandler method. More Information See the source of predefined effects and their handlers."
    } ,    
    {
      "title": "Algebraic Effects for Scala 3",
      "url": "/turbolift/",
      "content": "  🚧 WIP 🚧 Turbolift: Algebraic Effects for Scala 3 Highlights   ⭐ Expressive power Turbolift supports constructs rarely found in other effect systems, or in new programming languages with native support for Algebraic Effects. See Advanced Features.   ⭐ High performance Excerpt from Effect Zoo microbenchmark results:   ⭐ Lightweight syntax import turbolift.!! import turbolift.std_effects.{Reader, State, Error} case object MyReader extends Reader[Int] case object MyState extends State[Int] case object MyError extends Error[String] val program = for a &lt;- MyState.get b &lt;- MyReader.ask c &lt;- { if b != 0 then !!.pure(a / b) else MyError.raise(s\"Tried to divide $a by zero\") } _ &lt;- MyState.put(c) yield () // program: Computation[Unit, MyReader &amp; MyError &amp; MyState] = turbolift.Computation@335c6824 val result = program .handleWith(MyState.handler(100).justState) .handleWith(MyReader.handler(3)) .handleWith(MyError.handler) .run // result: Either[String, Int] = Right(value = 33)   Usage libraryDependencies += \"io.github.marcinzh\" %% \"turbolift-core\" % \"0.44.0\"   Credits Turbolift’s syntax and typing of effects and handlers evolved from the predecessor project: Skutek (Eff monad). The monad of delimited continuations is inspired by Scala Effekt. IO related parts (WIP) are inspired by Cats Effect and ZIO."
    } ,    
    {
      "title": "IO",
      "url": "/turbolift/io.html",
      "content": "🏗️ 🚧 WIP 🚧"
    } ,    
    {
      "title": "Labelled Effects",
      "url": "/turbolift/advanced/labelled.html",
      "content": "Labelled Effects Ability to label effects, is very rarely found feature in effect systems. That’s a shame, because without it, there is no true modularity. Related reading: Idris language - Labelled Effects Helium language - Effect Instances In Idris and Helium, effect labelling is optional. In Turbolift, effects are always uniquely labeled, thanks to Scala’s singleton types: import turbolift.std_effects.Error // Unique value: case object MyError extends Error[String] // Unique type: type MyError = MyError.type There is nothing stopping us from instantiating given effect more than once: import turbolift.std_effects.Error case object MyError1 extends Error[String] case object MyError2 extends Error[String] type MyError1 = MyError1.type type MyError2 = MyError2.type Each instance is a fully independent effect: They may be instantiated with different type parameters (e.g. State[Int] and State[String]). They may be used together in a computation. The type of the computation will reflect this, showing 2 distinct effects (e.g. MyError1 &amp; MyError2) They may be handled at different points in program, and with different handlers. Example: Using 2 State effects at the same time import turbolift.!! import turbolift.std_effects.State case object Foo extends State[Int] case object Bar extends State[Int] val program = for x &lt;- Foo.get _ &lt;- Bar.put(-x) yield () // program: Computation[Unit, Foo &amp; Bar] = turbolift.Computation@4f7e4bb3 val result = program .handleWith(Foo.handler(42)) .handleWith(Bar.handler(1337)) .run // result: Tuple2[Tuple2[Unit, Int], Int] = (((), 42), -42) Performance Performance penalty for using multiple effects in Turbolift, is small. Here is comparison with old version of Turbolift, that was based on Monad Transformers:"
    } ,    
    {
      "title": "Overview",
      "url": "/turbolift/overview.html",
      "content": "Overview The main types of Turbolift, and their roles Computation - Monad, parametrized by set of effects, a.k.a “One Monad To Rule Them All” 1. Signature - Trait, where we define our Algebra/Service/DSL (as abstract methods). Effect - Object, through which we can invoke operations of some Algebra/Service/DSL (as concrete methods). Interpreter - Object that assigns semantics to some Algebra/Service/DSL. It produces a Handler. Handler - Object that we can use like generalized try ... catch expression: to delimit scope of effect(s). Computation Usage A value of type Computation[A, U] describes a… computation, that requests a set of effects U that need to be handled, before it can return a value of type A. A type alias !![A, U] is defined for convenient infix syntax. The type-level set of effects is modelled by intersection types: Scala type Meaning as a set of effects Sample computation type Same, using infix syntax Any ∅ Computation[Int, Any] Int !! Any X X Computation[Int, X] Int !! X X &amp; Y X ∪ Y Computation[Int, X &amp; Y] Int !! (X &amp; Y)   Additionally, !! is a value alias of Computation’s companion object: import turbolift.!! val myComputation1 = !!.unit // myComputation1: Computation[Unit, Any] = turbolift.Computation@3b583d3b val myComputation2 = !!.pure(42) // myComputation2: Computation[Int, Any] = turbolift.Computation@60a56613 val myComputation3 = !!.impure { println(\"hello world\") } // myComputation3: Computation[Unit, Any] = turbolift.Computation@f2b1177 For more information, see Computation API Effect Usage To be able to invoke the effect’s operations, we need access to an instance of the effect. We can create such instance ourselves: import turbolift.std_effects.State // Indirectly inherits from Effect: case object MyState extends State[Int] val computation = MyState.put(42) // computation: Computation[Unit, MyState] = turbolift.Computation@202f5075 For more details, see Defining your own effects &amp; handlers and Effect labelling. Handler Usage Application of a handler delimits scope of effect(s). It also transforms type of the computation. In the simplest case, one of effects requested by the computation is eliminated. val myComputation2 = myComputation1.handleWith(myHandler) As soon as all effects are eliminated, the computation’s result can be obtained, using run: val result = myComputation .handleWith(myHandler1) .handleWith(myHandler2) .handleWith(myHandler3) .run In general, a handler of type Handler[F[_], L, N] represents a polymorphic function, that transforms computations: ∀ A, M. Computation[A, M ∪ L] =&gt; Computation[F[A], M ∪ N] Meaning, that application of it, does the following: It eLiminates set of effects L from incoming computation. It iNtroduces set of effects N into outgoing computation (revealing dependencies of the handler, if there are any). It passes aMbient set of effects M unaffected, from incoming to outgoing computation. It applies type constructor F[_] to A. In the example below, myHandler eliminates single MyChoice effect, introduces no effects, and wraps the result type in Vector[_]. import turbolift.Handler import turbolift.std_effects.Choice case object MyChoice extends Choice type MyChoice = MyChoice.type val myHandler: Handler[Vector, MyChoice, Any] = MyChoice.handler Handlers can be transformed or composed in various ways. For example, this sequences 3 independent handlers: val myHandler123 = myHandler1 &amp;&amp;&amp;! myHandler2 &amp;&amp;&amp;! myHandler3 For more operations, see Handler API Signature &amp; Interpreter Usage Those 2 types are used only during Defining your own effects &amp; handlers. Slogan © by Eric Torreborre. &#8617;"
    } ,        
  ];

  idx = lunr(function () {
    this.ref("title");
    this.field("content");

    docs.forEach(function (doc) {
      this.add(doc);
    }, this);
  });

  docs.forEach(function (doc) {
    docMap.set(doc.title, doc.url);
  });
}

// The onkeypress handler for search functionality
function searchOnKeyDown(e) {
  const keyCode = e.keyCode;
  const parent = e.target.parentElement;
  const isSearchBar = e.target.id === "search-bar";
  const isSearchResult = parent ? parent.id.startsWith("result-") : false;
  const isSearchBarOrResult = isSearchBar || isSearchResult;

  if (keyCode === 40 && isSearchBarOrResult) {
    // On 'down', try to navigate down the search results
    e.preventDefault();
    e.stopPropagation();
    selectDown(e);
  } else if (keyCode === 38 && isSearchBarOrResult) {
    // On 'up', try to navigate up the search results
    e.preventDefault();
    e.stopPropagation();
    selectUp(e);
  } else if (keyCode === 27 && isSearchBarOrResult) {
    // On 'ESC', close the search dropdown
    e.preventDefault();
    e.stopPropagation();
    closeDropdownSearch(e);
  }
}

// Search is only done on key-up so that the search terms are properly propagated
function searchOnKeyUp(e) {
  // Filter out up, down, esc keys
  const keyCode = e.keyCode;
  const cannotBe = [40, 38, 27];
  const isSearchBar = e.target.id === "search-bar";
  const keyIsNotWrong = !cannotBe.includes(keyCode);
  if (isSearchBar && keyIsNotWrong) {
    // Try to run a search
    runSearch(e);
  }
}

// Move the cursor up the search list
function selectUp(e) {
  if (e.target.parentElement.id.startsWith("result-")) {
    const index = parseInt(e.target.parentElement.id.substring(7));
    if (!isNaN(index) && (index > 0)) {
      const nextIndexStr = "result-" + (index - 1);
      const querySel = "li[id$='" + nextIndexStr + "'";
      const nextResult = document.querySelector(querySel);
      if (nextResult) {
        nextResult.firstChild.focus();
      }
    }
  }
}

// Move the cursor down the search list
function selectDown(e) {
  if (e.target.id === "search-bar") {
    const firstResult = document.querySelector("li[id$='result-0']");
    if (firstResult) {
      firstResult.firstChild.focus();
    }
  } else if (e.target.parentElement.id.startsWith("result-")) {
    const index = parseInt(e.target.parentElement.id.substring(7));
    if (!isNaN(index)) {
      const nextIndexStr = "result-" + (index + 1);
      const querySel = "li[id$='" + nextIndexStr + "'";
      const nextResult = document.querySelector(querySel);
      if (nextResult) {
        nextResult.firstChild.focus();
      }
    }
  }
}

// Search for whatever the user has typed so far
function runSearch(e) {
  if (e.target.value === "") {
    // On empty string, remove all search results
    // Otherwise this may show all results as everything is a "match"
    applySearchResults([]);
  } else {
    const tokens = e.target.value.split(" ");
    const moddedTokens = tokens.map(function (token) {
      // "*" + token + "*"
      return token;
    })
    const searchTerm = moddedTokens.join(" ");
    const searchResults = idx.search(searchTerm);
    const mapResults = searchResults.map(function (result) {
      const resultUrl = docMap.get(result.ref);
      return { name: result.ref, url: resultUrl };
    })

    applySearchResults(mapResults);
  }

}

// After a search, modify the search dropdown to contain the search results
function applySearchResults(results) {
  const dropdown = document.querySelector("div[id$='search-dropdown'] > .dropdown-content.show");
  if (dropdown) {
    //Remove each child
    while (dropdown.firstChild) {
      dropdown.removeChild(dropdown.firstChild);
    }

    //Add each result as an element in the list
    results.forEach(function (result, i) {
      const elem = document.createElement("li");
      elem.setAttribute("class", "dropdown-item");
      elem.setAttribute("id", "result-" + i);

      const elemLink = document.createElement("a");
      elemLink.setAttribute("title", result.name);
      elemLink.setAttribute("href", result.url);
      elemLink.setAttribute("class", "dropdown-item-link");

      const elemLinkText = document.createElement("span");
      elemLinkText.setAttribute("class", "dropdown-item-link-text");
      elemLinkText.innerHTML = result.name;

      elemLink.appendChild(elemLinkText);
      elem.appendChild(elemLink);
      dropdown.appendChild(elem);
    });
  }
}

// Close the dropdown if the user clicks (only) outside of it
function closeDropdownSearch(e) {
  // Check if where we're clicking is the search dropdown
  if (e.target.id !== "search-bar") {
    const dropdown = document.querySelector("div[id$='search-dropdown'] > .dropdown-content.show");
    if (dropdown) {
      dropdown.classList.remove("show");
      document.documentElement.removeEventListener("click", closeDropdownSearch);
    }
  }
}
