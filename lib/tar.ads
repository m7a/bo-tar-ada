with Interfaces;

package Tar is

	subtype U64 is Interfaces.Unsigned_64;

	type Dev_Node    is mod 10 ** 8;
	type Access_Mode is mod  8 ** 7;

	type Tar_Entry_Type is
		(File, Directory, FIFO, Symlink, Hardlink, Char, Block);

	function "="  (A, B: in U64) return Boolean renames Interfaces."=";
	function "<"  (A, B: in U64) return Boolean renames Interfaces."<";
	function "<=" (A, B: in U64) return Boolean renames Interfaces."<=";
	function ">"  (A, B: in U64) return Boolean renames Interfaces.">";
	function ">=" (A, B: in U64) return Boolean renames Interfaces.">=";

	function "+"  (A, B: in U64) return U64     renames Interfaces."+";
	function "-"  (A, B: in U64) return U64     renames Interfaces."-";
	function "*"  (A, B: in U64) return U64     renames Interfaces."*";
	function "/"  (A, B: in U64) return U64     renames Interfaces."/";
	function "mod"(A, B: in U64) return U64     renames Interfaces."mod";
	function "rem"(A, B: in U64) return U64     renames Interfaces."rem";

	function "and"(A, B: in U64) return U64     renames Interfaces."and";
	function "or" (A, B: in U64) return U64     renames Interfaces."or";
	function "xor"(A, B: in U64) return U64     renames Interfaces."xor";
	function "not"(   A: in U64) return U64     renames Interfaces."not";

end Tar;
