with Interfaces.C;

package body Metadata is

	function pstat( in_path:    in Interfaces.C.char_array;
			out_mode:   access Interfaces.C.unsigned;
			out_uid:    access Interfaces.C.unsigned_long;
			out_gid:    access Interfaces.C.unsigned_long;
			out_major:  access Interfaces.C.unsigned;
			out_minor:  access Interfaces.C.unsigned;
			out_size:   access Interfaces.C.unsigned_long;
			out_mtime:  access Interfaces.C.unsigned_long;
			out_ftype:  access Interfaces.C.unsigned)
		return Interfaces.C.int;
	pragma Import(C, pstat);

	function readlink(in_path:  in  Interfaces.C.char_array;
			out_buf:    out Interfaces.C.char_array;
			in_bufsize: in  Interfaces.C.size_t)
		return Interfaces.C.long;
	pragma Import(C, readlink);

	function Set(Ent: in out Tar.Writer.Tar_Entry;
					Path: in String) return Boolean is
		mode:  aliased  Interfaces.C.unsigned;
		uid:   aliased  Interfaces.C.unsigned_long;
		gid:   aliased  Interfaces.C.unsigned_long;
		major: aliased  Interfaces.C.unsigned;
		minor: aliased  Interfaces.C.unsigned;
		size:  aliased  Interfaces.C.unsigned_long;
		mtime: aliased  Interfaces.C.unsigned_long;
		ftype: aliased  Interfaces.C.unsigned;
		TBL:   constant array (Interfaces.C.unsigned'(1) ..
				Interfaces.C.unsigned'(7)) of Tar.Tar_Entry_Type
			:= (Tar.File, Tar.Directory, Tar.FIFO, Tar.Symlink,
				Tar.Hardlink, Tar.Char, Tar.Block);
		RC:    constant Interfaces.C.int := pstat(
				Interfaces.C.To_C(Path), mode'Access,
				uid'Access, gid'Access, major'Access,
				minor'Access, size'Access, mtime'Access,
				ftype'Access);
		Ent_T: Tar.Tar_Entry_Type;
		Ret:   Boolean;
	begin
		if Interfaces.C."/="(RC, 0) then
			raise Constraint_Error with "Failed to stat " & Path &
					": " & Interfaces.C.int'Image(RC);
		end if;

		Ent.Set_Access_Mode(Tar.Access_Mode(Interfaces.C."and"(mode,
								8#7777777#)));
		Ent.Set_Modification_Time(Tar.U64(mtime));
		Ent.Set_Owner(Tar.U64(uid), Tar.U64(gid));

		if ftype in TBL'Range then
			Ent_T := TBL(ftype);
			Ret   := Tar."="(Ent_T, Tar.File);
		else
			Ent_T := Tar.File;
			Ret   := False;
		end if;

		Ent.Set_Type(Ent_T);
		if Ret then
			Ent.Set_Size(Tar.U64(size));
		end if;

		case Ent_T is
		when Tar.Symlink => Set_Symlink(Ent, Path, Tar.U64(size));
		when Tar.Char | Tar.Block => Ent.Set_Device(Tar.Dev_Node(major),
							Tar.Dev_Node(minor));
		when others => null;
		end case;

		return Ret;
	end Set;

	procedure Set_Symlink(Ent: in out Tar.Writer.Tar_Entry; Path: in String;
							Size: in Tar.U64) is
		SZ:  constant Interfaces.C.size_t := Interfaces.C.size_t(Size);
		Buf: Interfaces.C.char_array(0 .. Interfaces.C."-"(SZ, 1));
		RC:  constant Interfaces.C.long := readlink(
					Interfaces.C.To_C(Path), Buf, SZ);
	begin
		if Interfaces.C."<"(RC, 0) then
			raise Constraint_Error with "Failed to read symlink: " &
									Path;
		end if;
		Ent.Set_Link_Target(Interfaces.C.To_Ada(Buf));
	end Set_Symlink;

end Metadata;
