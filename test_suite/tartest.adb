with Ada.Assertions;
use  Ada.Assertions;
with Ada.Text_IO;
use  Ada.Text_IO;
with Ada.Streams;
use  Ada.Streams;
with Ada.Streams.Stream_IO;
use  Ada.Streams.Stream_IO;
with Ada.Directories;
use  Ada.Directories;
with Tar;
use  Tar;
with Tar.Writer;
use  Tar.Writer;
with Ada.Exceptions;
use  Ada.Exceptions;
with Interfaces.C;

procedure TarTest is

	pragma Assertion_Policy(Pre => Check, Post => Check);

	Test_Failure: exception;

	Long_Path: constant String := "large/path/with/many/subdirectories/which/exceeds/the/limit/of/255/characters/max/in/path/and/has/a/very/long/file/name/that/exceeds/all/typical/limits/it/requires/some/fantasy/to/really/believe/in/such/long/paths/being/relevant/in/practice/but/some/companies/or/ides/may/create/such/long/file/names/info.txt";
	Sample_Content_32: constant Stream_Element_Array(0 .. 31) :=
							(others => 16#1a#);

	--------------------------------------------------------[ Test Cases ]--

	procedure Test_Case_Create_Large_Path(FN: in String) is
		FD: Ada.Streams.Stream_IO.File_Type;
		TE: Tar_Entry := Init_Entry(Long_Path);
	begin
		Create(FD, Out_File, FN);
		TE.Set_Type(File);
		TE.Set_Access_Mode(8#664#);
		TE.Set_Size(U64(Sample_Content_32'Length));
		Write(FD, TE.Begin_Entry);
		Write(FD, TE.Add_Content(Sample_Content_32));
		Write(FD, TE.End_Entry);
		Write(FD, End_Tar);
		Close(FD);
	end Test_Case_Create_Large_Path;

	procedure Check_Large_Path(Directory: in String) is
		-- Cannot compose because it raises name error Long_Path is not
		-- a simple name. This variant here should be portable enough...
		Check_Name: constant String := Directory & "/" & Long_Path;
		Buf: Stream_Element_Array(0 .. Sample_Content_32'Length) :=
								(others => 0);
		Last: Stream_Element_Offset;
	begin
		declare
			FD: Ada.Streams.Stream_IO.File_Type;
		begin
			Open(FD, In_File, Check_Name);
			Read(FD, Buf, Last);
			Close(FD);
		exception
			when others => 
				raise Test_Failure with
					"Unable to open file with long name. " &
					"Maybe it was not created correctly?";
		end;
		if Last /= Sample_Content_32'Last then
			raise Test_Failure with
				"Unable to read back correct number of " &
				"bytes. Expected last byte at offset " &
				Stream_Element_Offset'Image(
				Sample_Content_32'Last) & ", but got " &
				Stream_Element_Offset'Image(Last);
		end if;
		if Buf(0 .. Sample_Content_32'Length - 1) /=
							Sample_Content_32 then
			-- TODO ADD A HEX DUMP HERE
			raise Test_Failure with
				"Read back the correct number of bytes but " &
				"found their content to be corrupted.";
		end if;
	end Check_Large_Path;

	--------------------------------------------[ Test Support Functions ]--

	procedure Extract_Tar(File: in String; Output_Directory: in String;
					Expected_Returncode: Integer := 0) is
		function System(Arg: Interfaces.C.Char_Array) return Integer;
		pragma Import(C, System, "system");
		RC: constant Integer := System(Interfaces.C.To_C("tar -C " &
					Output_Directory & " -xf " & File));
	begin
		if RC /= Expected_Returncode then
			raise Test_Failure with "Extraction of " & File &
				" returned code " & Integer'Image(RC) &
				" instead of the expected " &
				Integer'Image(Expected_Returncode);
		end if;
	end Extract_Tar;

	-- adjust to your platform if needed
	Tmp_Dir: constant String := "/tmp/tartest";

	procedure Run_Test_Create_Large_Path is
		Tar_File: constant String := Compose(Tmp_Dir, "large.tar");
	begin
		Test_Case_Create_Large_Path(Tar_File);
		Extract_Tar(Tar_File, Tmp_Dir);
		Check_Large_Path(Tmp_Dir);
		Delete_Tree(Compose(Tmp_Dir, "large"));
	end Run_Test_Create_Large_Path;

	procedure Run_And_Print(Name: in String; TC: access procedure) is
	begin
		TC.all;
		Ada.Text_IO.Put_Line("[ OK ] " & Name);
	exception
		when Ex: Test_Failure =>
			Ada.Text_IO.Put_Line("[FAIL] " & Name & " -- " &
				Exception_Message(Ex));
	end Run_And_Print;

	--------------------------------------------------------------[ Main ]--

begin

	Create_Directory(Tmp_Dir);

	Run_And_Print("create large path", Run_Test_Create_Large_Path'Access);

	Delete_Tree(Tmp_Dir);

end TarTest;
