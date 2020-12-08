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
end


val lines = readLines TextIO.stdIn;
val xs = map parse (lines);
val ht = let
    val t = mkRules ()
in
    map (HashTable.insert t) xs;
    t
end;
HashTable.numItems ht;
part1 ht
             
(* val _ = OS.Process.exit(OS.Process.success) *)
