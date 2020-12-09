(* Turns out Standard ML is not used for a reason... all the IO is pretty
awful.  There is a regex engine but I'm not convicted anyone outside of the
maintainers knows how to use it.  There is a scanf implementation but it's also
pretty awkward in that it doesn't handle %*s or anything clever.  Then you're
left with the scan functions, which seem to make little sense to most folks
anyway, so there are not many examples online.  In short, SML is not practical
for AoC.  I rather use assembly. *)

(* Put everything in a let to shut SML up *)
let
    type rules = (string, (int * string) list) HashTable.hash_table

    fun join (x::xs) = List.foldl (fn (a, b) => b^" "^a) x xs
      | join [] = "";
    
    fun mkRules ():rules = HashTable.mkTable (HashString.hashString, op=) (10, Fail "not found")
    fun parse line = let
        val xs = String.tokens (fn x => x = #" " orelse x = #"," orelse x = #".") line;
        fun scanBag xs = let
            fun f (x, (0, acc, ys)) = if x = "bag" orelse x = "bags"
                                      then (1, acc, ys)
                                      else (0, x::acc, ys)
              | f (x, (_, acc, ys)) = (1, acc, x::ys)
            val (_, acc, ys) = List.foldl f (0, [], []) xs
        in
            (join (List.rev acc), List.rev ys)
        end;
        (* TODO exhaust match space *)
        fun scanInt (x::xs) = case Int.fromString x of
                                  SOME(n) => (n, xs);
        val (container, "contain"::rest) = scanBag xs;
        fun scanNested ["no", "other", "bags"] = []
          | scanNested nil = []
          | scanNested xs = let
              val (n, rest') = scanInt xs;
              val (container', rest'') = scanBag rest'
          in
              (n, container')::(scanNested rest'')
          end;
    in
        (container, scanNested rest)
    end;
    
    (* I'm very sleepy.  I know this is going to balloon the stack... OK.  Fine. *)
    fun readLines (input:TextIO.instream):string list = let
        fun read NONE = nil
          | read (SOME(line)) = let
              val trimmed = String.implode (List.filter (fn c => not (c = #"\n")) (explode line))
          in
              trimmed::(readLines input)
          end;
    in
        read (TextIO.inputLine input)
    end;

    fun part1 ht = let
        fun canContainShiny xs =
            List.exists (fn (_, color) =>
                            color = "shiny gold" orelse
                            case HashTable.find ht color of
                                SOME(v) => canContainShiny v
                              | NONE => false) xs;
    in
        HashTable.fold (fn (x, n) => if canContainShiny x then n+1 else n) 0 ht
    end;

    (* Kind of surprised I found a use for mutual recursion... *)
    fun part2 ht = let
        fun f ((n, color), m) = m+(n*(g color))
        and g color = case HashTable.find ht color of
                          SOME(bags) => List.foldl f 1 bags
                        | NONE       => 1
    in
        (g "shiny gold") - 1        (* Ooookayyyyy *)
    end;

    val lines = readLines TextIO.stdIn;
    val xs = map parse (lines);
    val ht = let
        val t = mkRules ()
    in
        map (HashTable.insert t) xs;
        t
    end;
in
    print ((Int.toString (part1 ht)) ^ "\n");
    print ((Int.toString (part2 ht)) ^ "\n");
    OS.Process.exit OS.Process.success
    
end

(* Local Variables: *)
(* compile-command: "sml day07.sml < input.txt" *)
(* End: *)
