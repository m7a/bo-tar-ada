with Ada.Text_IO;
with Ada.Streams;
use  Ada.Streams;
with Ada.Streams.Stream_IO;
use  Ada.Streams.Stream_IO;
with Ada.Direct_IO;
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
with References;

procedure TarTest is

	pragma Assertion_Policy(Pre => Check, Post => Check);

	Test_Failure: exception;

	Long_Path: constant String := "large/path/with/many/subdirectories/which/exceeds/the/limit/of/255/characters/max/in/path/and/has/a/very/long/file/name/that/exceeds/all/typical/limits/it/requires/some/fantasy/to/really/believe/in/such/long/paths/being/relevant/in/practice/but/some/companies/or/ides/may/create/such/long/file/names/info.txt";
	Sample_Content_32: constant Stream_Element_Array(0 .. 31) :=
							(others => 16#1a#);

	Large_Test_Size: constant U64 := 9 * 1024 ** 3; -- 9 GiB

	-- adjust to your platform if needed
	Tmp_Dir: constant String := "/tmp/tartest";

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

	function To_Hex(Num: in Stream_Element) return String is
		Hex_Tbl: constant String := "0123456789abcdef";
	begin
		return (Hex_Tbl(Integer(Interfaces.Shift_Right(
					Interfaces.Unsigned_32(Num), 4)) + 1),
			Hex_Tbl(Integer(Num and 16#0f#)      + 1));
	end To_Hex;

	function Slow_Simple_To_Hex(Bin: in Stream_Element_Array)
								return String is
		Result: String(1 .. Bin'Length * 2) := (others => '_');
		Idx:    Integer := Result'First;
	begin
		for I in Bin'Range loop
			Result(Idx .. Idx + 1) := To_Hex(Bin(I));
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

	procedure Run_Test_Create_Large_File_Fails_In_USTAR is
		TE: Tar_Entry := Init_Entry("largefile/random.bin", True);
	begin
		TE.Set_Type(File);
		TE.Set_Access_Mode(8#664#);
		TE.Set_Size(Large_Test_Size);
		raise Test_Failure with "USTAR mode claimed to be able to " &
			"process file of size " & U64'Image(Large_Test_Size);
	exception
		when Ignore_Ex: Tar.Writer.Not_Supported_In_Format => return;
	end Run_Test_Create_Large_File_Fails_In_USTAR;

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

	-- Test Special Files --
	procedure Test_Case_Create_Tar_Of_Special_Files(FN: in String) is
		FD: Ada.Streams.Stream_IO.File_Type;

		procedure Add_Device_VHOST_VSOCK is
			TE: Tar_Entry := Init_Entry("dev/vhost-vsock");
		begin
			TE.Set_Type(Char);
			TE.Set_Access_Mode(8#0660#);
			TE.Set_Modification_Time(1688917675);
			TE.Set_Owner(0, 106);
			TE.Set_Owner("root", "kvm");
			TE.Set_Device(10, 241);
			-- NOTE: GNU Tar generates explicit zeroes in these
			--       fields. For comparison with GNU Tar outputs
			--       it is thus useful to set them explicitly.
			--       I am not sure if the standard really requires
			--       this. If yes, it would be possible to add this
			--       to the initialization of the USTAR header such
			--       that zeroes are always present when no
			--       explicit API call about setting the size is
			--       performed.
			TE.Set_Size(0);
			Write(FD, TE.Begin_Entry);
			Write(FD, TE.End_Entry);
		end Add_Device_VHOST_VSOCK;

		procedure Add_Symlink_VMLINUZ is
			TE: Tar_Entry := Init_Entry("vmlinuz");
		begin
			TE.Set_Type(Symlink);
			-- Note: It is slightly unclear if this should probably
			--       be set automatically by the library. For now,
			--       we solve this by setting it explicitly from the
			--       test.
			TE.Set_Access_Mode(8#777#);
			TE.Set_Modification_Time(1687643450);
			TE.Set_Owner(0, 0);
			TE.Set_Owner("root", "root");
			TE.Set_Link_Target("boot/vmlinuz-6.1.0-9-amd64");
			TE.Set_Size(0); -- See GNU Tar note above.
			Write(FD, TE.Begin_Entry);
			Write(FD, TE.End_Entry);
		end Add_Symlink_VMLINUZ;

		procedure Add_Device_VDA is
			TE: Tar_Entry := Init_Entry("dev/vda");
		begin
			TE.Set_Type(Block);
			TE.Set_Access_Mode(8#0660#);
			TE.Set_Modification_Time(1688917675);
			TE.Set_Owner(0, 6);
			TE.Set_Owner("root", "disk");
			TE.Set_Device(254, 0);
			TE.Set_Size(0); -- See GNU Tar note above.
			Write(FD, TE.Begin_Entry);
			Write(FD, TE.End_Entry);
		end Add_Device_VDA;

		procedure Add_Dir_XATTRTEST is
			TE: Tar_Entry := Init_Entry(
					"home/linux-fan/wd/xattrtest/");
		begin
			TE.Set_Type(Directory);
			TE.Set_Access_Mode(8#0755#);
			TE.Set_Modification_Time(1688917956);
			TE.Set_Owner(1000, 1000);
			TE.Set_Owner("linux-fan", "linux-fan");
			TE.Set_Size(0); -- See GNU Tar note above.
			Write(FD, TE.Begin_Entry);
			Write(FD, TE.End_Entry);
		end Add_Dir_XATTRTEST;

		procedure Add_File_HELLOI is
			TE: Tar_Entry := Init_Entry(
				"home/linux-fan/wd/xattrtest/helloi.txt");
			DT: constant Stream_Element_Array := (16#68#, 16#65#,
						16#6c#, 16#6c#, 16#6f#, 16#0a#);
		begin
			TE.Set_Type(File);
			TE.Set_Access_Mode(8#0644#);
			TE.Set_Modification_Time(1688917887);
			TE.Set_Owner(1000, 1000);
			TE.Set_Owner("linux-fan", "linux-fan");
			TE.Set_Size(DT'Length);
			Write(FD, TE.Begin_Entry);
			Write(FD, TE.Add_Content(DT));
			Write(FD, TE.End_Entry);
		end Add_File_HELLOI;

		procedure Add_File_TESTACL is
			TE: Tar_Entry := Init_Entry(
				"home/linux-fan/wd/xattrtest/testacl.txt");
			DT: constant Stream_Element_Array := (16#74#, 16#65#,
				16#73#, 16#74#, 16#61#, 16#63#, 16#6c#, 16#0a#);
			XATTR: constant Stream_Element_Array :=
				(16#02#, 16#00#, 16#00#, 16#00#, 16#01#, 16#00#,
				 16#06#, 16#00#, 16#ff#, 16#ff#, 16#ff#, 16#ff#,
				 16#02#, 16#00#, 16#04#, 16#00#, 16#21#, 16#00#,
				 16#00#, 16#00#, 16#04#, 16#00#, 16#04#, 16#00#,
				 16#ff#, 16#ff#, 16#ff#, 16#ff#, 16#10#, 16#00#,
				 16#04#, 16#00#, 16#ff#, 16#ff#, 16#ff#, 16#ff#,
				 16#20#, 16#00#, 16#04#, 16#00#, 16#ff#, 16#ff#,
				 16#ff#, 16#ff#);
			XSTR: String(1 .. XATTR'Length);
			for XSTR'Address use XATTR'Address;
		begin
			TE.Set_Type(File);
			TE.Set_Access_Mode(8#0644#);
			TE.Set_Modification_Time(1688917957);
			TE.Set_Owner(1000, 1000);
			TE.Set_Owner("linux-fan", "linux-fan");
			TE.Set_Size(DT'Length);
			-- free-form entries are currently unsupported
			--TE.Add_X_Attr("SCHILY.acl.access",
			--	"user::rw-"         & ASCII.LF &
			--	"user:www-data:r--" & ASCII.LF &
			--	"group::r--"        & ASCII.LF &
			--	"mask::r--"         & ASCII.LF &
			--	"other::r--"        & ASCII.LF);
			TE.Add_X_Attr("system.posix_acl_access", XSTR);
			Write(FD, TE.Begin_Entry);
			Write(FD, TE.Add_Content(DT));
			Write(FD, TE.End_Entry);
		end Add_File_TESTACL;
	begin
		Create(FD, Out_File, FN);
		Add_Device_VHOST_VSOCK;
		Add_Symlink_VMLINUZ;
		Add_Device_VDA;
		Add_Dir_XATTRTEST;
		Add_File_HELLOI;
		Add_File_TESTACL;
		Write(FD, End_Tar);
		Close(FD);
	end Test_Case_Create_Tar_Of_Special_Files;

	procedure Check_Compare_Reference_Data(File: in String;
				Reference_Data: in Stream_Element_Array) is
		FD: Ada.Streams.Stream_IO.File_Type;
		Pos: Stream_Element_Offset := Reference_Data'First;
		Buf: Stream_Element_Array(0 .. 31);
		Last: Stream_Element_Offset;
		Max_R: Stream_Element_Offset;
	begin
		Open(FD, In_File, File);
		while Pos <= Reference_Data'Last loop
			Max_R := Stream_Element_Offset'Min(Reference_Data'Last -
							Pos + 1, Buf'Length);
			Read(FD, Buf(Buf'First .. Buf'First + Max_R - 1), Last);
			if Buf(Buf'First .. Last) /= Reference_Data(Pos .. Pos +
							(Last - Buf'First)) then
				raise Test_Failure with
					"Mismatch against reference data ref=<"
					& Slow_Simple_To_Hex(Reference_Data(
					Pos .. Pos + (Last - Buf'First))) &
					" got=<" & Slow_Simple_To_Hex(
					Buf(Buf'First .. Last)) & ">";
			end if;
			Pos := Pos + (Last - Buf'First + 1);
		end loop;
		Read(FD, Buf, Last);
		if Last >= Buf'First then
			raise Test_Failure with
				"Generated TAR is larger than reference data" &
				". Extra Data=<" & Slow_Simple_To_Hex(
				Buf(Buf'First .. Last)) & ">";
		end if;
		Close(FD);
	end;

	-- Test PAX Length Computation --
	procedure Run_Test_PAX_Length_Computation is
		Ent: Tar_Entry := Init_Entry("paxtest-x2" &
							(98 * "/012345678"));
	begin
		declare
			D1:    constant Stream_Element_Array := Ent.Begin_Entry;
			D2:    constant Stream_Element_Array := Ent.End_Entry;
			CMPD2: constant Stream_Element_Array(1 .. 0)
							:= (others => 0);
		begin
			-- Ada.Text_IO.Put_Line(Slow_Simple_To_Hex(D1));
			-- Ada.Text_IO.Put_Line(Slow_Simple_To_Hex(D2));
			if D1 /= References.Tar_Begin_Long_Metadata then
				raise Test_Failure with "Long metadata " &
						"mismatch with reference data";
			end if;
			if D2 /= CMPD2 then
				raise Test_Failure with
					"Zero padding mismatch. D2L=" &
					Stream_Element_Offset'Image(D2'Length) &
					", CMPD2L=" &
					Stream_Element_Offset'Image(
					CMPD2'Length);
			end if;
		end;
	end Run_Test_PAX_Length_Computation;

	-- Test USTAR Split and UID/GID Truncation --
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

	procedure Test_Case_Parametrized_USTAR_Split(Tar_File: in String;
			Is_USTAR: Boolean; Config_Char: in Character;
			Read_Block: in Integer;
			Reference_Block: in Stream_Element_Array) is

		function Path_At_Depth(I: in Stream_Element;
					BS0: in String := "") return String is
			BS: String := Config_Char & "12345x__/";
		begin
			if I = 0 then
				return BS0;
			else
				BS(8 .. 9) := To_Hex(I);
				return Path_At_Depth(I - 1, BS & BS0);
			end if;
		end Path_At_Depth;

		FD:       Ada.Streams.Stream_IO.File_Type;
		Depth:    constant Stream_Element := 23;
		Last_FN:  constant String  := Path_At_Depth(Depth) & "test.dat";
	begin
		Create(FD, Out_File, Tar_File);
		for I in 1 .. Depth loop
			declare
				DNP: constant String := Path_At_Depth(I);
				TE: Tar_Entry := Init_Entry(DNP(DNP'First ..
						DNP'Last - 1), Is_USTAR);
			begin
				TE.Set_Type(Directory);
				TE.Set_Access_Mode(8#755#);
				Write(FD, TE.Begin_Entry);
				Write(FD, TE.End_Entry);
			end;
		end loop;

		declare
			TE: Tar_Entry := Init_Entry(Last_FN, Is_USTAR);
		begin
			TE.Set_Access_Mode(8#644#);
			-- user name exceeds 32 bytes size and should come out
			-- as truncated!
			TE.Set_Owner("0123456789abcdef-0123456789abcdef",
								"simplegroup");
			TE.Set_Size(3);
			Write(FD, TE.Begin_Entry);
			Write(FD, TE.Add_Content((16#30#, 16#31#, 16#0a#)));
			Write(FD, TE.End_Entry);
		end;
		Write(FD, End_Tar);
		Close(FD);
		
		Extract_Tar(Tar_File, Tmp_Dir);

		if not Exists(Tmp_Dir & "/" & Last_FN) then
			raise Test_Failure with "Could not long file name";
		end if;

		Delete_Tree(Compose(Tmp_Dir, Config_Char & "12345x01"));
		
		declare
			subtype Tar_Record is Stream_Element_Array(0 .. 511);
			package RIO is new Ada.Direct_IO(Tar_Record);
			Record_Of_Interest: Tar_Record;
			FD2: RIO.File_Type;
		begin
			RIO.Open(FD2, RIO.In_File, Tar_File);
			RIO.Read(FD2, Record_Of_Interest,
							RIO.Count(Read_Block));
			RIO.Close(FD2);
			if Record_Of_Interest /= Reference_Block then
				raise Test_Failure with
					"Record " & Integer'Image(Read_Block) &
					" <" & Slow_Simple_To_Hex(
					Record_Of_Interest) &
					"> could not be detected to be the " &
					"expected truncated group record <" &
					Slow_Simple_To_Hex(Reference_Block) &
					">.";
			end if;
		end;
	end Test_Case_Parametrized_USTAR_Split;

	-- Test Long Group Names --
	procedure Run_Test_Long_Group_Name is
		function Create_Long_Group_Entry(Force_USTAR: Boolean)
							return Tar_Entry is
		begin
			return TE: Tar_Entry := Init_Entry((155 / 5) * "test/" &
					10 * "0123456789", Force_USTAR) do
				TE.Set_Access_Mode(8#644#);
				TE.Set_Owner("simpleuser",
					"0123456789abcdef-0123456789abcdef");
			end return;
		end Create_Long_Group_Entry;

		Ent_PAX: Tar_Entry := Create_Long_Group_Entry(False);
		Ent_TAR: Tar_Entry := Create_Long_Group_Entry(True);
		HDR_PAX: constant Stream_Element_Array := Ent_PAX.Begin_Entry;
		HDR_TAR: constant Stream_Element_Array := Ent_TAR.Begin_Entry;
	begin
		if HDR_PAX /= References.Long_Group_Name_PAX then
			raise Test_Failure with
				"Mismatching PAX header generated for long " &
				"group name. Expected=<" & Slow_Simple_To_Hex(
				References.Long_Group_Name_PAX) & ">, Got=<" &
				Slow_Simple_To_Hex(HDR_PAX) & ">";
		end if;
		if HDR_TAR /= References.Long_Group_Name_USTAR then
			raise Test_Failure with
				"Mismatching USTAR header generated for long " &
				"group name. Expected=<" & Slow_Simple_To_Hex(
				References.Long_Group_Name_USTAR) & ", Got=<" &
				Slow_Simple_To_Hex(HDR_TAR) & ">";
		end if;
	end Run_Test_Long_Group_Name;

	-- Test Long Link Target Names --
	procedure Run_Test_Long_Link is
		function Create_Long_Link_Entry(Force_USTAR: Boolean)
							return Tar_Entry is
		begin
			return TE: Tar_Entry := Init_Entry("testlink",
								Force_USTAR) do
				TE.Set_Type(Symlink);
				TE.Set_Access_Mode(8#777#);
				TE.Set_Link_Target(12 * "/0123456678");
			end return;
		end Create_Long_Link_Entry;

		OK_TE: Tar_Entry := Create_Long_Link_Entry(False);
		HDR: constant Stream_Element_Array := OK_TE.Begin_Entry;
	begin
		if HDR /= References.Long_Link_PAX_Prefix then
			raise Test_Failure with
				"PAX Header generated for long link data " &
				"does not match reference. Expected=<" &
				Slow_Simple_To_Hex(
				References.Long_Link_PAX_Prefix) & ">, Got=<" &
				Slow_Simple_To_Hex(HDR) & ">";
		end if;

		begin
			declare
				Ignore_TE: constant Tar_Entry :=
						Create_Long_Link_Entry(True);
			begin
				raise Test_Failure with
					"USTAR-mode claimed to be able to " &
					"represent link target of 120 " &
					"characters but should have failed " &
					"since the format only supports 100 " &
					"characters max for links";
			end;
		exception
			-- OK
			when Ignore_Ex: Tar.Writer.Not_Supported_In_Format =>
									return;
		end;
	end Run_Test_Long_Link;

	-- Test USTAR Limits - Error Tests for file names --
	procedure Run_Test_USTAR_Limit_Name(FN: in String;
			Proposed_FN: in String; Msg: in String) is
		Tar_File: constant String := Compose(Tmp_Dir, FN);
		Lim_Dir:  constant String := Compose(Tmp_Dir, "testlim");
		Ent_Name: constant String := "testlim/" & Proposed_FN;
		TE: Tar_Entry := Init_Entry(Ent_Name);
		FD: Ada.Streams.Stream_IO.File_Type;
	begin
		Create(FD, Out_File, Tar_File);
		TE.Set_Access_Mode(8#644#);
		Write(FD, TE.Begin_Entry);
		Write(FD, TE.End_Entry);
		Write(FD, End_Tar);
		Close(FD);
		
		Extract_Tar(Tar_File, Tmp_Dir);

		-- cannot use compose here since proposed fn may not be simple
		if not Exists(Lim_Dir & "/" & Proposed_FN) then
			raise Test_Failure with "Could not extract PAX variant";
		end if;

		Delete_Tree(Lim_Dir);
		Delete_File(Tar_File);

		begin
			declare
				Ignore_Entry: constant Tar_Entry :=
						Init_Entry(Ent_Name, True);
			begin
				raise Test_Failure with Msg;
			end;
		exception
			-- OK
			when Ignore_Ex:
				Tar.Writer.Not_Supported_In_Format => return;
		end;
	end Run_Test_USTAR_Limit_Name;

	procedure Run_Test_USTAR_Limit_Non_ASCII_File_Name is
	begin
		Run_Test_USTAR_Limit_Name("testoe.tar", "ö",
					"Should fail to represent " &
					"non-ASCII file name in USTAR mode");
	end Run_Test_USTAR_Limit_Non_ASCII_File_Name;

	procedure Run_Test_USTAR_Limit_Basename_Length is
	begin
		Run_Test_USTAR_Limit_Name("testlong.tar",
				"test/" & 32 * "long" & ".txt",
				"Should fail to represent too long basename");
	end Run_Test_USTAR_Limit_Basename_Length;

	procedure Run_Test_USTAR_Limit_Filename_Length is
	begin
		Run_Test_USTAR_Limit_Name(
				"testlong2.tar", 60 * "test/" & "x.txt",
				"Should fail to represent too long pathname");
	end Run_Test_USTAR_Limit_Filename_Length;

	procedure Run_Test_USTAR_Limit_Extended_Attributes is
		TE: Tar_Entry := Init_Entry("testxattr", True);
	begin
		TE.Add_X_Attr("system.posix_acl_access", "test");
		raise Test_Failure with "USTAR-mode claimed to be able to " &
			"store an extended attribute but that should be " &
			"unsupported";
	exception
		-- OK
		when Ignore_Ex: Tar.Writer.Not_Supported_In_Format => return;
	end Run_Test_USTAR_Limit_Extended_Attributes;

	--------------------------------------------[ Test Support Functions ]--

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

	procedure Run_Test_Create_Tar_Of_Special_Files is
		Tar_File: constant String := Compose(Tmp_Dir, "special.tar");
	begin
		Test_Case_Create_Tar_Of_Special_Files(Tar_File);
		Check_Compare_Reference_Data(Tar_File,
					References.Tar_Of_Special_Files);
		Delete_File(Tar_File);
	end Run_Test_Create_Tar_Of_Special_Files;

	procedure Run_Test_USTAR_Split is
		Tar_File: constant String := Compose(Tmp_Dir, "split.tar");
	begin
		Test_Case_Parametrized_USTAR_Split(Tar_File, True, '0', 24, 
					References.Truncated_User_Record);
		Delete_File(Tar_File);
	end Run_Test_USTAR_Split;

	procedure Run_Test_Long_User_Name is
		Tar_File: constant String := Compose(Tmp_Dir, "longuser.tar");
		PAX_Group_Record: constant Stream_Element_Array(0 .. 511) := (
			16#34#, 16#33#, 16#20#, 16#75#, 16#6e#, 16#61#, 16#6d#,
			16#65#, 16#3d#, 16#30#, 16#31#, 16#32#, 16#33#, 16#34#,
			16#35#, 16#36#, 16#37#, 16#38#, 16#39#, 16#61#, 16#62#,
			16#63#, 16#64#, 16#65#, 16#66#, 16#2d#, 16#30#, 16#31#,
			16#32#, 16#33#, 16#34#, 16#35#, 16#36#, 16#37#, 16#38#,
			16#39#, 16#61#, 16#62#, 16#63#, 16#64#, 16#65#, 16#66#,
			16#0a#, others => 0
		);
	begin
		Test_Case_Parametrized_USTAR_Split(Tar_File, False, 'p', 25, 
							PAX_Group_Record);
		Delete_File(Tar_File);
	end Run_Test_Long_User_Name;

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
	Run_And_Print("create large file fails in ustar",
			Run_Test_Create_Large_File_Fails_In_USTAR'Access);
	Run_And_Print("create tar of special files",
				Run_Test_Create_Tar_Of_Special_Files'Access);
	Run_And_Print("trigger pax meta overflow logic",
				Run_Test_PAX_Length_Computation'Access);
	Run_And_Print("ustar split name", Run_Test_USTAR_Split'Access);
	Run_And_Print("long user name", Run_Test_Long_User_Name'Access);
	Run_And_Print("long group name", Run_Test_Long_Group_Name'Access);
	Run_And_Print("long link targets", Run_Test_Long_Link'Access);
	Run_And_Print("ustar fails for non-ascii file name",
			Run_Test_USTAR_Limit_Non_ASCII_File_Name'Access);
	Run_And_Print("ustar fails for basename > 155 chars in length",
			Run_Test_USTAR_Limit_Basename_Length'Access);
	Run_And_Print("ustar fails for file name > 255 chars in length",
			Run_Test_USTAR_Limit_Filename_Length'Access);
	Run_And_Print("ustar cannot represent extended attributes",
			Run_Test_USTAR_Limit_Extended_Attributes'Access);

	Delete_Tree(Tmp_Dir);

end TarTest;
