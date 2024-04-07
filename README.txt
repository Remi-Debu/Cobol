
		                 GnuCOBOL
		https://www.gnu.org/software/gnucobol/
		https://sourceforge.net/projects/gnucobol
		https://savannah.gnu.org/projects/gnucobol

GnuCOBOL is a free (like both in "free speech" and in "free beer")
COBOL compiler, formerly known as OpenCOBOL.
It implements a substantial part of the COBOL 85, COBOL 2002 and COBOL 2014
standards, as well as many extensions included in other COBOL compilers.

GnuCOBOL translates COBOL into C and compiles the translated code
using the native C compiler on various platforms, including Unix/Linux,
Mac OS X, and Microsoft Windows.

This package contains the following subdirectories:

    cobc        COBOL compiler
    libcob      COBOL run-time library
    bin         COBOL driver program
    build_aux   Helper scripts
    lib         Helper routines for missing OS functionality
    config      Configuration files
    po          International messages
    doc         'info' and 'pdf' files
    tests       Test suite (GnuCOBOL and framework for COBOL85)
    extras      useful COBOL programs

All programs except those in lib and libcob are distributed under
the GNU General Public License.  See COPYING for details.

Programs in lib and libcob are distributed under the GNU Lesser
General Public License.  See COPYING.LESSER for details.

For any copyright year range specified as YYYY-ZZZZ in this package,
that the range specifies every single year in that closed interval.

Although many have participated, most development thanks go to

    Roger While
    Keisuke Nishida

See AUTHORS for the author of each file.

============

This package (MinGW based) is intended for testing purposes on Windows systems
and has everything needed to run the compiler and runtime, including the
necessary C compiler.

Version details:
GnuCOBOL 3.2.0 (Jul 28 2023 16:07:51), (MinGW) "13.1.0"
GMP 6.2.1, libxml2 2.11.4, cJSON 1.7.15, PDCursesMod 4.3.7, BDB 18.1.40

It is NOT optimized and may have some minor bugs other binaries created from the
same source tarball don't have.

Important: See BUGS.txt for possible known issues in this distribution!

For running GnuCOBOL simply double-click set_env.cmd found next to this file, or,
if already in cmd, call set_env.cmd once.
You can use cobc and cobcrun in the command prompt afterwards.

