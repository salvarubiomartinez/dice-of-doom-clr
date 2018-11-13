using clojure.lang;
using clojure.clr.api;


namespace DiceOfDoom
{
    class Program
    {
        static void Main(string[] args)
        {
            // for compile dll on repl (compile 'dice-of-doom.core)
            IFn require = Clojure.var("clojure.core", "require");
            require.invoke(Clojure.read("dice-of-doom.core"));
            IFn core = Clojure.var("dice-of-doom.core", "-main");
            core.invoke();
        }
    }
}
