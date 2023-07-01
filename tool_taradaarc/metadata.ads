with Tar;
with Tar.Writer;

package Metadata is

	function Set(Ent: in out Tar.Writer.Tar_Entry;
						Path: in String) return Boolean;

private

	procedure Set_Symlink(Ent: in out Tar.Writer.Tar_Entry; Path: in String;
							Size: in Tar.U64);

end Metadata;
