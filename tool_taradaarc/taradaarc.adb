with Ada.Assertions;
with Ada.Directories;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;
with Ada.Text_IO.Text_Streams;
with Ada.Command_Line;
with Ada.Calendar;
with Ada.Calendar.Formatting;
with Tar;
with Tar.Writer;

with Metadata;

procedure TarAdaArc is

	Stdout: constant access Ada.Streams.Root_Stream_Type'Class :=
		Ada.Text_IO.Text_Streams.Stream(Ada.Text_IO.Standard_Output);

	-- To some extent, this is redundant because the native part may provide
	-- a better interface to get the unix time. This shows how to do it
	-- portably, though.
	UNIX0: constant Ada.Calendar.Time :=
			Ada.Calendar.Formatting.Time_Of(1970, 1, 1, 0.0);
	procedure Set_Modification_Time(Ent: in out Tar.Writer.Tar_Entry;
						TM: in Ada.Calendar.Time) is
	begin
		Ent.Set_Modification_Time(Tar.U64(Ada.Calendar."-"(TM, UNIX0)));
	end Set_Modification_Time;

	procedure Send_Contents(Ent: in out Tar.Writer.Tar_Entry;
							FN: in String) is
		use type Ada.Streams.Stream_Element_Offset;
		FD: Ada.Streams.Stream_IO.File_Type;
		Buf: Ada.Streams.Stream_Element_Array(0 .. 1023);
		Last: Ada.Streams.Stream_Element_Offset;
	begin
		Ada.Streams.Stream_IO.Open(FD,
					Ada.Streams.Stream_IO.In_File, FN);

		while not Ada.Streams.Stream_IO.End_Of_File(FD) loop
			Ada.Streams.Stream_IO.Read(FD, Buf, Last);
			Stdout.Write(Ent.Add_Content(Buf(Buf'First .. Last)));
		end loop;
		if Ada.Streams.Stream_IO.Is_Open(FD) then
			Ada.Streams.Stream_IO.Close(FD);
		end if;
	end Send_Contents;

	procedure Add_File_Inner(FN: in String; TM: in Ada.Calendar.Time;
							Size: in Tar.U64) is
		Ent: Tar.Writer.Tar_Entry := Tar.Writer.Init_Entry(FN);
		CNT: Boolean;
	begin
		Set_Modification_Time(Ent, TM);
		Ent.Set_Size(Size);
		CNT := Metadata.Set(Ent, FN);
		Stdout.Write(Ent.Begin_Entry);
		if CNT then
			Send_Contents(Ent, FN);
		end if;
		Stdout.Write(Ent.End_Entry);
	end Add_File_Inner;

	procedure Add_File(Item: in Ada.Directories.Directory_Entry_Type) is
		BN: constant String := Ada.Directories.Simple_Name(Item);
		FN: constant String := Ada.Directories.Full_Name(Item);
	begin
		-- On Unix systems, these entries belong to the "other"
		-- category and are hence detected here, even though we mean
		-- to include only "file-like" tiems.
		if BN /= "." and BN /= ".." then
			Add_File_Inner(FN,
					Ada.Directories.Modification_Time(Item),
					Tar.U64(Ada.Directories.Size(Item)));
		end if;
	end Add_File;

	procedure Add_File(FN: in String) is
	begin
		Add_File_Inner(FN, Ada.Directories.Modification_Time(FN),
					Tar.U64(Ada.Directories.Size(FN)));
	end Add_File;

	procedure Find(Root: in String);

	procedure Add_Directory(Item: in Ada.Directories.Directory_Entry_Type)
									is
		BN: constant String := Ada.Directories.Simple_Name(Item);
	begin
		if BN = "." or BN = ".." then
			return;
		end if;
		declare
			FN: constant String := Ada.Directories.Full_Name(Item);
			Ent: Tar.Writer.Tar_Entry := Tar.Writer.Init_Entry(FN);
			CNT: Boolean;
		begin
			Ent.Set_Type(Tar.Directory);
			Ent.Set_Size(0);
			Set_Modification_Time(Ent,
				Ada.Directories.Modification_Time(Item));
			CNT := Metadata.Set(Ent, FN);
			Ada.Assertions.Assert(not CNT);
			Stdout.Write(Ent.Begin_Entry);
			Stdout.Write(Ent.End_Entry);
		end;
		Find(Ada.Directories.Full_Name(Item));
	end Add_Directory;

	-- https://rosettacode.org/wiki/Walk_a_directory/Recursively#Ada
	procedure Find(Root: in String) is
	begin
		Ada.Directories.Search(Root, "", (
			Ada.Directories.Directory => False, others => True),
			Add_File'Access);
		Ada.Directories.Search(Root, "",
			(Ada.Directories.Directory => True, others => False),
			Add_Directory'Access);
	end Find;

	procedure Add_By_Name(Name: in String) is
		use type Ada.Directories.File_Kind;
	begin
		if Name = "." or Name = ".." or Ada.Directories.Kind(Name) =
						Ada.Directories.Directory then
			Find(Name);
		else
			Add_File(Name);
		end if;
	end Add_By_Name;

begin
	if Ada.Command_Line.Argument_Count < 1 or else
				Ada.Command_Line.Argument(1) = "--help" then
		Ada.Text_IO.Put_Line("USAGE $0 FILE|DIRECTORY...");
	else
		for I in 1 .. Ada.Command_Line.Argument_Count loop
			Add_By_Name(Ada.Command_Line.Argument(I));
		end loop;
		Stdout.Write(Tar.Writer.End_Tar);
	end if;
end TarAdaArc;
