---
section: 32
x-masysma-name: tar_ada
title: Tar Writer Library for Ada
date: 2023/06/11 22:05:11
lang: en-US
author: ["Linux-Fan, Ma_Sys.ma (Ma_Sys.ma@web.de)"]
keywords: ["tar", "archive", "ada", "library"]
x-masysma-version: 1.0.0
x-masysma-website: https://masysma.net/32/tar_ada.xhtml
x-masysma-repository: https://www.github.com/m7a/bo-tar-ada
x-masysma-owned: 1
x-masysma-copyright: (c) 2023 Ma_Sys.ma <info@masysma.net>.
---
Abstract
========

This repository provides an Ada library for writing Tar and PAX archives
(typical file extension `.tar`).

The intended use case is creating archives from some other format and then
streaming them to any target. As a result, this API does not currently provide
any “file”-centric calls, i.e. there is no way to add a file from the file
system to a Tar archive.

Instead, the API cernters around creating entries -- think of e.g. a single
file -- and formatting their metadata and data such that they become valid
Tar blocks that can be streamed.

To put it another way: This library does not in itself have any side effects:
The using applications are responsible for reading and writing the actual file
data and metadata. This library only helps with formatting them according to Tar
format requirements.

The library supports two output formats:

 * USTAR
 * PAX

Both formats are interpreted as specified by POSIX aka.
_the Open Group Base Specifications Issue 7, 2018 edition_.

The entire library works under the assumtion that `Stream_Element` is a byte of
8 bits.

License
=======

This library is licensed under GPL 3 or later.
See `/usr/share/common-licenses/GPL-3` on any Debian system.

	Ma_Sys.ma Tar Writer Library for Ada
	(c) 2023 Ma_Sys.ma <info@masysma.net>
	
	This program is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.
	
	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

Compiling
=========

To compile this library, `ant` and `gnatmake` tools should be available
alongside with a GNAT compiler. If these dependencies are present, the
library can be compiled by entering the following command:

	ant

If the necessary dependencies for building Debian packages are also installed,
the following command can be used to create an installable Debian package:

	ant package

Alternatively, see `lib/build.xml` for a command that can be used to compile
the source files directly. This way it is possible to compile even without
having `ant` installed. A minimal compilation might then work as follows:

	cd lib
	gnatmake -fPIC -fstack-protector-strong -c tar-writer.adb
	gcc -shared -o libtarada.so *.o

Repository Structure
====================

The repsitory file structure is as follows:

_TODO INCOMPLETE_

~~~
bo-tar-ada/
   │
   ├── lib/                           *** This is the implementation. ***
   │    ├── tar-writer.adb
   │    ├── tar-writer.ads
   │    ├── tar.ads
   │    └── build.xml
   │
   ├── tool_taradaarc/                Minimal example of an “archiver”
   │    ├── build.xml                 program that can be used to create TAR
   │    ├── metadata.adb              archives out of file system trees. It
   │    ├── metadata.ads              demonstrates the usage of the library for
   │    ├── pstat.c                   a non-trivial use-case.
   │    └── taradaarc.adb
   │
   ├── tool_taradahello/
   │    ├── build.xml
   │    └── taradahello.adb           “Hello World” example using this library
   │
   ├── README.md                      This file
   ├── debian-changelog.txt           Changelog information for .deb build
   └── build.xml                      Top-level build instructions
~~~

Example Program
===============

The following example program `taradahello.adb` can be found in the directory
`tool_taradahello` in the repository.

~~~{.ada}
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
	Ent.Set_Access_Mode(8#644#);
	Ent.Set_Size(Cnt'Length);
	Ent.Set_Owner(1000, 1000);

	Stdout.Write(Ent.Begin_Entry);
	Stdout.Write(Ent.Add_Content(Cnt_Ar));
	Stdout.Write(Ent.End_Entry);
	Stdout.Write(Tar.Writer.End_Tar);

end TarAdaHello;
~~~

The program creates a tar archive with a single file entry (no directory) and
name `hello.txt` with the traditional content of `Hello, world.` followed by
a newline. The tar data is directly sent to the standard output in this example.

If you are intrested in a more complex example, check the files under
`tool_taradaarc` especially `tool_taradaarc/taradaarc.adb` which implements
an archiver that traverses file system trees and then writes the data to
stdout as .tar files.

Using the Library
=================

_TODO MISSING CONTENT IN THIS SECTION_

## Installed

## Without Installation

Tar Datatypes (`tar.ads`)
=========================

~~~{.ada}
subtype U64 is Interfaces.Unsigned_64;

type Dev_Node    is mod 10 ** 7;
type Access_Mode is mod  8 ** 7;

type Tar_Entry_Type is
	(File, Directory, FIFO, Symlink, Hardlink, Char, Block);
~~~

Package `Tar` specifies data types which may be useful for all kinds of Tar
processing.

 * `U64` is a standard Unsigned_64 integer. To permit using it without
   explicitly using `Interfaces`, the package also specifies most of the
   meaningful operators on it as a renamed version of their `Interfaces`
   counterpart.
 * `Dev_Node` is a modular type representing the supported data range
   for device node entries. It consists of all non-negative decimal numbers up
   to 7 digits.
 * `Access_Mode` is a modular type representing the supported file modes.
   It consists of all non-negative ocatl numbers up to 7 digits.
 * `Tar_Entry_Type` distinguishes the various kinds of entries that Tar data
   can represent.

Note: The restriction on the values of `Dev_Node` and `Access_Mode` are not
strictly mandated by the standard for PAX outputs. They are defined this way
here because this is (1) easier to implement and (2) should be sufficient for
most use cases. Please tell me if there are real use cases where these ranges
are insufficient, because using PAX it is possible to lift them by extending
the implementation appropriately.

Tar Writer API (`tar-writer.ads`)
=================================

This is the core API for creating Tar archives.

~~~{.ada}
function Init_Entry(Name: in String; Force_USTAR_Format: Boolean := False)
							return Tar_Entry;

procedure Set_Type       (Ent: in out Tar_Entry; Typ:  in Tar_Entry_Type);
procedure Set_Access_Mode(Ent: in out Tar_Entry; Mode: in Access_Mode);
procedure Set_Size       (Ent: in out Tar_Entry; SZ:   in U64);
procedure Set_Modification_Time(Ent: in out Tar_Entry; M_Time: in U64);
procedure Set_Owner      (Ent: in out Tar_Entry; UID,    GID:    in U64);
procedure Set_Owner      (Ent: in out Tar_Entry; U_Name, G_Name: in String);
procedure Set_Link_Target(Ent: in out Tar_Entry; Target: in String);
procedure Set_Device     (Ent: in out Tar_Entry; Major,  Minor:  in Dev_Node);
procedure Add_X_Attr     (Ent: in out Tar_Entry; Key,    Value:  in String);

function Begin_Entry(Ent: in out Tar_Entry) return Stream_Element_Array;
function Add_Content(Ent: in out Tar_Entry; Cnt: in Stream_Element_Array)
						return Stream_Element_Array;
function End_Entry(Ent: in out Tar_Entry) return Stream_Element_Array;

function End_Tar return Stream_Element_Array;
~~~

The lifecycle of an entire _archive_ is as follows:

 1. Stream any number of entries
 2. Send a single archive “footer” through `End_Tar` which is really just 1K
    of zeroes.

Each _entry_ inside the archive is created as follows:

 1. Call `Init_Entry` to obtain a context
 2. Set all the metadata and add extended attributes as needed.
 3. Call `Begin_Entry` (once) to receive a header to stream.
 4. Call `Add_Content` (any number of times) to stream the file contents.
 5. Call `End_Entry` (once) to write the entry footer (think of zero-padding)

For all functions that return a `Stream_Element_Array`, it is intended to
stream the returned data to the output in order to obtain a valid Tar or PAX
archive by the concatenation.

## Initialization

### `function Init_Entry(Name; Force_USTAR_Format) return Tar_Entry`

Prepares a `Tar_Entry` from the entry name which is the path inside the
archive. This may be an absolute path like e.g. `/tmp/test.txt` or a relative
path like `lib/build.xml`. A slash must be used to separate the path components.
The encoding must either be valid UTF-8.

By default, the entry is created as a valid USTAR entry if the metdata
can be represented in that format. If metadata exceeds the limits of USTAR,
a _PAX Extended Header_ is automatically created as necessary. This behavior can
be disabled by setting `Force_USTAR_Format := True`. In this case, instead of
creating a _PAX Extended Header_, exception `Not_Supported_In_Format` is raised.

Important cases where the USTAR limits are exceeded are e.g. any of the
following:

 * Path names longer than 255 characters.
 * File sizes greater than 8 GiB.

## Metadata

The following procedures can be used to configure the metadata of the archive
entry. They are only valid to be called after `Init_Entry`.
The `Begin_` routines must not have been called on the same entry before.

### `Set_Type(Ent; Typ: in Tar_Entry_Type)`

This procedure defines what kind of entry is to be produced. Most of the
enum values directly correspond to the classic unix file types.

There is one pecuilarity: The `Hardlink` type can be used to create links to
existing entries from the same Tar as follows:

 * The entry's size must be set to 0.
 * The `Set_Link_Target` procedure must be called with a path that exists in the
   same Tar archive.

It is recommended to call this procedure at least once for each entry.

### `Set_Access_Mode(Ent; Mode: in Access_Mode)`

This procedure configures the entry's access mode which is often written in
octal like e.g. `8#644#` for a typical file that can be read and written by
its owner and read by all other groups and users.

### `Set_Size(Ent; SZ: in U64)`

This procedure defines the data sizeo of the entry to be created in bytes.

It is recommended to call this procedure at least once for each entry.

### `Set_Modification_Time(Ent; M_Time: in U64)`

This procedure defines the modification time as UNIX timestamps i.e. in seconds
since the epoch (1970-01-01 00:00:00 UTC). Earlier file dates are not supported.

### `Set_Owner(Ent; UID, GID: in U64)`

This procedure configures the owner of the entry as a numeric user id
(UID) and group id (GID). Typical values on desktop systems are e.g.
(1000, 1000) for user-created files and (0, 0) for root-owned files.

### `Set_Owner(Ent; U_Name, G_Name: in String)`

This procedure configures the owner of the entry by giving the user and
group name as strings. These values are stored independently of the given
numeric fields and upon extraction, tar-compatible applications are expected
to _prefer_ these names over the numeric IDs and only use the numeric values
when the respective (named) owner does not exist on the current system.

Please consider the intended use case before blindly storing the user and group
names here: For some users, the login name may correspond to their actual name
and archives may be uploaded to online targets, breaching the users' anonymity.

Universal archivers like e.g. GNU Tar provide the user with options to change
the default behaviour of storing the owner names (called `--numeric-owner`
there). POSIX does not seem to prescribe such an option for conformant `pax`
archivers, though.

### `Set_Link_Target(Ent; Target: in String)`

This procedure defines the target of a symlink or a hardlink.

### `Set_Device(Ent; Major, Minor: in Dev_Node)`

If the entry to be created corresponds to a device node, this procedure sets
the associated `Major` and `Minor` numbers.

Note: While PAX could represent arbitrarily long numbes here, this
implementation limits the device node major and minor numbers to the limits
defined for USTAR since that seems to cover all practical use cases already.

### `Add_X_Attr(Ent; Key, Value: in String)`

This procedure adds an extended attribute as a free-form key/value pair.

Note that the storage of extended attributes is not defined by PAX and thus
the extended attributes can only be restored by archivers that support
the convention implemented here aka. `SCHILY.xattr`, cf.
<https://man.freebsd.org/cgi/man.cgi?query=star&sektion=5>.

## Content

After having configured all metadata for an entry, the associated header
can be obtained with `Begin_Entry`. Then, any number of calls to `Add_Content`
can be used to format data to be added for this entry and finally, the entry
is concluded by calling `End_Entry`. If no further entries appear in this TAR,
obtain the TAR Footer from `End_Tar`.

### `Begin_Entry(Ent) return Stream_Element_Array`

This function returns all the metadata configured for the current entry as
a readily streamable binary blob. It allows that subsequently, `Add_Content`
can be called to process the actual file contents.

### `Add_Content(Ent; Cnt: in Stream_Element_Array) return Stream_Element_Array`

This function may seem like an identity function because it returns the same
data as being input. In the course, it counts the number of bytes and keeps
track of the alignment to TAR blocks (512 bytes each) which is necessary to
properly end the entry.

### `End_Entry(Ent) return Stream_Element_Array`

When all content has been added, `End_Entry` concludes the entry by returning
suitable padding as to fill the 512 byte blocks. This padding may be empty when
the entry size is a multiple of 512 bytes.

### `End_Tar return Stream_Element_Array`

When all entries have been added, `End_Tar` can be used to obtain the end of
archive marker which is a fancy way of getting 1 KiB of zero bytes btw.

Performance
===========

_TODO NEED TO CREATE A BENCHMARK FIRST MAYBE WRITE A LITTLE ARCHIVER?_

Rationale and Usage Recommendation
==================================

_TODO MISSING SECTION_

Changes
=======

_TODO MISSING SECTION_
