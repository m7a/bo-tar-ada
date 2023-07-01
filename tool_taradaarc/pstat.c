/*
 * Portable interface to UNIX' `stat` function. The problem with using
 * `struct stat` directly from Ada is that we have to specify this struct
 * without relying on any preprocessor header magic whatsoever. This would
 * result in a specific instantiation that only works on one architecture and
 * one OS.
 *
 * To work around this, this `pstat` (for portable stat) is provided and
 * intended to be interfaced with from the Ada side.
 *
 * Compile on Ĺinux:
 * gcc -Wall -Werror -Wextra -Wconversion -Wpedantic -std=c89 -c pstat-unix.c
 *
 * Windows interop described here:
 * https://docs.adacore.com/gnat_ugn-docs/html/gnat_ugn/gnat_ugn/platform_specific_information.html#using-dlls-with-gnat
 * It is pretty similar to the Linux one...
 */

/* Portable representation of file types aligned with Ada code */
enum file_type {
	TYPE_FILE      = 1,
	TYPE_DIRECTORY = 2,
	TYPE_FIFO      = 3,
	TYPE_SYMLINK   = 4,
	TYPE_HARDLINK  = 5,  /* unsupported in this program */
	TYPE_CHAR      = 6,
	TYPE_BLOCK     = 7,
	TYPE_OTHER     = 8
};

#ifdef _WIN32
/* --------------------------------------------------------[ Windows Code ]-- */

/* TODO SKELETON ONLY */
int pstat(const char *in_path, unsigned *out_mode, unsigned long *out_uid,
				unsigned long *out_gid, unsigned *out_major,
				unsigned *out_minor, unsigned long *out_size,
				unsigned long *out_mtime, unsigned *out_ftype)
{
	return -1; /* unsupported means unsupported */
}

long readlink(const char *in_path, char *out_buf, size_t in_bufsize)
{
	return -1;
}

#elif __unix__
/* ----------------------------------------------------------[ Linux Code ]-- */

#include <assert.h>
#include <sys/stat.h>
#include <sys/sysmacros.h>

int pstat(const char *in_path, unsigned *out_mode, unsigned long *out_uid,
				unsigned long *out_gid, unsigned *out_major,
				unsigned *out_minor, unsigned long *out_size,
				unsigned long *out_mtime, unsigned *out_ftype)
{
	struct stat st;
	int rc = stat(in_path, &st);
	if (rc == 0) {
		assert(st.st_size  >= 0);
		assert(st.st_mtime >= 0);
		*out_mode  = st.st_mode;
		*out_uid   = st.st_uid;
		*out_gid   = st.st_gid;
		*out_major = major(st.st_rdev);
		*out_minor = minor(st.st_rdev);
		*out_size  = (unsigned long)st.st_size;
		*out_mtime = (unsigned long)st.st_mtime;

		if      (S_ISREG(st.st_mode))  *out_ftype = TYPE_FILE;
		else if (S_ISDIR(st.st_mode))  *out_ftype = TYPE_DIRECTORY;
		else if (S_ISFIFO(st.st_mode)) *out_ftype = TYPE_FIFO;
		else if (S_ISLNK(st.st_mode))  *out_ftype = TYPE_SYMLINK;
		else if (S_ISCHR(st.st_mode))  *out_ftype = TYPE_CHAR;
		else if (S_ISBLK(st.st_mode))  *out_ftype = TYPE_BLOCK;
		else                           *out_ftype = TYPE_OTHER;
	}
	return 0;
}

#else
#error "Your OS seems to be unsupported by the native code. Remove call to Metadata.Set in taradaarc.adb and set types File/Directory in Add_File_File and Add_Directory respectively. Alternatively adjust the C source code in pstat.c and retry."
#endif
