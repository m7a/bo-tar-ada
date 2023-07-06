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
with Ada.Strings.Fixed;
use  Ada.Strings.Fixed;
with Tar;
use  Tar;
with Tar.Writer;
use  Tar.Writer;
with Ada.Exceptions;
use  Ada.Exceptions;
with Interfaces;
with Interfaces.C;

procedure TarTest is

	pragma Assertion_Policy(Pre => Check, Post => Check);

	Test_Failure: exception;

	Long_Path: constant String := "large/path/with/many/subdirectories/which/exceeds/the/limit/of/255/characters/max/in/path/and/has/a/very/long/file/name/that/exceeds/all/typical/limits/it/requires/some/fantasy/to/really/believe/in/such/long/paths/being/relevant/in/practice/but/some/companies/or/ides/may/create/such/long/file/names/info.txt";
	Sample_Content_32: constant Stream_Element_Array(0 .. 31) :=
							(others => 16#1a#);

	Large_Test_Size: constant U64 := 9 * 1024 ** 3; -- 9 GiB

	--------------------------------------------------------[ Test Cases ]--

	-- Test Create Large Path --
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

	function Slow_Simple_To_Hex(Bin: in Stream_Element_Array)
								return String is
		package Output_Formatter is new Ada.Text_IO.Modular_IO(
							Stream_Element);
		Hex:    String := "16#00#";
		Result: String(1 .. Bin'Length * 2) := (others => '_');
		Idx:    Integer := Result'First;
	begin
		for I in Bin'Range loop
			Output_Formatter.Put(Hex, Bin(I));
			Result(Idx .. Idx + 1) :=
				(if Hex(1) = ' ' then ("0" & Hex(5 .. 5))
				else Hex(4 .. 5));
			Idx := Idx + 2;
		end loop;
		return Result;
	end Slow_Simple_To_Hex;

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
			raise Test_Failure with
				"Read back the correct number of bytes but " &
				"found their content to be corrupted. " &
				"Expected=<" & Slow_Simple_To_Hex(
				Sample_Content_32) & ">, Got=<" &
				Slow_Simple_To_Hex(Buf(0 ..
				Sample_Content_32'Length - 1)) & ">";
		end if;
	end Check_Large_Path;

	-- Test Create Large File --
	-- https://burtleburtle.net/bob/rand/smallprng.html
	type U32 is mod 2**32;
	type RNG_State is record
		A: U32 := 16#f1ea5eed#;
		B: U32 := 0;
		C: U32 := 0;
		D: U32 := 0;
	end record;
	function RNG_Next(X: in out RNG_State) return U32 is
		function Rot(A: in U32; B: in Natural) return U32 is (U32(
			Interfaces.Rotate_Left(Interfaces.Unsigned_32(A), B)));
		E: constant U32 := X.A - Rot(X.B, 27);
	begin
		X.A := X.B xor Rot(X.C, 17);
		X.B := X.C + X.D;
		X.C := X.D + E;
		X.D := E + X.A;
		return X.D;
	end RNG_Next;

	procedure Test_Case_Create_Large_File(FN: in String) is
		FD: Ada.Streams.Stream_IO.File_Type;
		TE: Tar_Entry := Init_Entry("largefile/random.bin");

		TN: array (0 .. 4095) of U32 := (others => 0);
		TD: Stream_Element_Array(0 .. TN'Length * 4 - 1);
		for TD'Address use TN'Address;
		RNG: RNG_State := (others => <>);
		Have_Cnt: U64 := 0;
	begin
		Create(FD, Out_File, FN);
		TE.Set_Type(File);
		TE.Set_Access_Mode(8#664#);
		TE.Set_Size(Large_Test_Size);
		Write(FD, TE.Begin_Entry);

		while Have_Cnt < Large_Test_Size loop
			for I in TN'Range loop
				TN(I) := RNG_Next(RNG);
			end loop;
			Write(FD, TE.Add_Content(TD));
			Have_Cnt := Have_Cnt + U64(TD'Length);
		end loop;
		
		Write(FD, TE.End_Entry);
		Write(FD, End_Tar);
		Close(FD);
	end Test_Case_Create_Large_File;

	procedure Check_Large_File(Directory: in String) is
		Check_Name: constant String := Compose(Compose(Directory,
						"largefile"), "random.bin");
		FD: Ada.Streams.Stream_IO.File_Type;

		TN: array (0 .. 4095) of U32 := (others => 0);
		TD: Stream_Element_Array(0 .. TN'Length * 4 - 1);
		for TD'Address use TN'Address;
		RNG: RNG_State := (others => <>);
		Have_Cnt: U64 := 0;
		Buf: Stream_Element_Array(TD'Range);
		Last: Stream_Element_Offset := Buf'First;

		procedure Advance is begin
			Read(FD, Buf, Last);
			Have_Cnt := Have_Cnt + U64(Last - Buf'First + 1);
			for I in TN'Range loop
				TN(I) := RNG_Next(RNG);
			end loop;
		end Advance;
	begin
		Open(FD, In_File, Check_Name);
		Advance;
		while Last >= Buf'First and then TD = Buf loop
			Advance;
		end loop;
		Close(FD);
		if Last >= Buf'First then
			raise Test_Failure with "Content mismatch after " &
						U64'Image(Have_Cnt) & " bytes.";
		end if;
		if Have_Cnt /= Large_Test_Size then
			raise Test_Failure with "Expected to process " &
				U64'Image(Large_Test_Size) &
				" bytes but found " & U64'Image(Have_Cnt) &
				" bytes instead.";
		end if;
	end;

	-- Test USTAR Limits - Error Tests for file names --

	generic
		FN:   String;
		Info: String;
	procedure Run_Test_USTAR_Limit_Name;
	procedure Run_Test_USTAR_Limit_Name is
		Test_With_No_Limits: constant Tar_Entry := Init_Entry(FN);
	begin
		declare
			TE: constant Tar_Entry := Init_Entry(FN, True);
		begin
			raise Test_Failure with Info;
		end;
	exception
		when Ex: Tar.Writer.Not_Supported_In_Format => return; -- pass
	end Run_Test_USTAR_Limit_Name;

	procedure Run_Test_USTAR_Limit_Non_ASCII_File_Name is new
		Run_Test_USTAR_Limit_Name("ö", "Should fail to represent " &
					"non-ASCII file name in USTAR mode");
	procedure Run_Test_USTAR_Limit_Basename_Length is new
		Run_Test_USTAR_Limit_Name("test/" & 32 * "long" & ".txt",
				"Should fail to represent too long basename");
	procedure Run_Test_USTAR_Limit_Filename_Length is new
		Run_Test_USTAR_Limit_Name(60 * "test/" & "x.txt",
				"Should fail to represent too long pathname");

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
		Delete_File(Tar_File);
	end Run_Test_Create_Large_Path;

	procedure Run_Test_Create_Large_File is
		Tar_File: constant String := Compose(Tmp_Dir, "lagefile.tar");
	begin
		Test_Case_Create_Large_File(Tar_File);
		Extract_Tar(Tar_File, Tmp_Dir);
		Check_Large_File(Tmp_Dir);
		Delete_Tree(Compose(Tmp_Dir, "largefile"));
		Delete_File(Tar_File);
	end Run_Test_Create_Large_File;

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
	Run_And_Print("create large file", Run_Test_Create_Large_File'Access);
	Run_And_Print("ustar fails for non-ascii file name",
			Run_Test_USTAR_Limit_Non_ASCII_File_Name'Access);
	Run_And_Print("ustar fails for basename > 155 chars in length",
			Run_Test_USTAR_Limit_Basename_Length'Access);
	Run_And_Print("ustar fails for file name > 255 chars in length",
			Run_Test_USTAR_Limit_Filename_Length'Access);

	Delete_Tree(Tmp_Dir);

end TarTest;
