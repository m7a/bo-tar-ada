with Ada.Streams;
with Ada.Text_IO;
with Ada.Text_IO.Text_Streams;
with Tar;
with Tar.Writer;

procedure TarAdaHello is

	Stdout: constant access Ada.Streams.Root_Stream_Type'Class :=
		Ada.Text_IO.Text_Streams.Stream(Ada.Text_IO.Standard_Output);

	Cnt: constant String := ("Hello, world." & ASCII.LF);

	Cnt_Ar: Ada.Streams.Stream_Element_Array(0 ..
		Ada.Streams.Stream_Element_Offset(Cnt'Length - 1));
	for Cnt_Ar'Address use Cnt'Address;

	Ent: Tar.Writer.Tar_Entry := Tar.Writer.Init_Entry("hello.txt");

begin

	Ent.Set_Type(Tar.File);
	Ent.Set_Access_Mode(8#1644#);
	Ent.Set_Size(Cnt'Length);
	Ent.Set_Owner(1000, 1000);

	Stdout.Write(Ent.Begin_Entry);
	Stdout.Write(Ent.Add_Content(Cnt_Ar));
	Stdout.Write(Ent.End_Entry);
	Stdout.Write(Tar.Writer.End_Tar);

end TarAdaHello;
