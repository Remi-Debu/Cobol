#!/usr/bin/python3

################################################################################
# Copyright (c) 2019-2022 COBOLworx and contributors
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
#    * Redistributions of source code must retain the above copyright
#      notice, this list of conditions and the following disclaimer.
#    * Redistributions in binary form must reproduce the above copyright
#      notice, this list of conditions and the following disclaimer in
#      the documentation and/or other materials provided with the
#      distribution.
#    * Neither the name of the COBOLworx Corporation nor the names of its
#      contributors may be used to endorse or promote products derived
#      from this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE
# FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
# OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
################################################################################
# pylint: disable=line-too-long, unused-argument, unused-wildcard-import, import-error, invalid-name
#
# flake8 settings used
# [flake8]
# ignore = E402
# max-line-length = 160
################################################################################

#   This module implements the gdb extension commands cprint and cwatch

# Catalog of environment variables interpreted by this module:
#
# os.environ.get('CPRINT_D', '0')  When non-zero, causes verbose output
# os.environ.get('CPRINT_V', '0')  Default formatting p/v N value
# os.environ.get('CPRINT_P', '')   Default pretty_print switch.
# os.environ.get('CPRINT_R', '6')) Default p/? range
# os.environ.get('CPRINT_RAISE, 1) 1 means gdb.error, 0 means just print and continue

from __future__ import print_function

import re
import sys
import platform
import os
from os import path
import traceback
import subprocess
from inspect import currentframe, getframeinfo
from struct import calcsize
import copy
import time
import gdb

GV_reads = 0
GV_bytes_read = 0

GV_noisy_registration = False

# Version to be printed upon request - Don't mess with it!
# It is updated by a Linux bash script, the text "Version x.x" has to appear in square brackets.
COBCD_VERSION = '[Version 4.28.6]'

#   This module implements the gdb extension commands like cprint and cwatch
#   It is executed by GDB.
#
#   Optionally it is read by the COBST program and incorporated into a
#   C module that gets compiled and linked into the final executable.
#
#   Decoding a COBOL variable will require the following information,
#   taken from ...\gnucobol-3.0-rc1\libcob\common.h
#
#   This structure will have an f_ symbol pointing to it.
#
#       typedef struct __cob_field
#           {
#           size_t                  size;       /* Field size */
#           unsigned char           *data;      /* Pointer to field data */
#           const cob_field_attr    *attr;      /* Pointer to attribute */
#           } cob_field;
#
#   cob_field.attr points to this structure, which will have an 'a_' symbol
#
#       typedef struct __cob_field_attr
#           {
#           unsigned short          type;       /* Field type */
#           unsigned short          digits;     /* Digit count */
#           signed short            scale;      /* Field scale */
#           unsigned short          flags;      /* Field flags */
#           const cob_pic_symbol    *pic;       /* Pointer to picture string */
#           } cob_field_attr;
#
#   cob_field_attr.pic, if present, will have a 'p_' symbol pointing to
#
#       typedef struct __cob_pic_symbol
#           {
#           char    symbol;
#           int     times_repeated;
#           } cob_pic_symbol;

#
#   /* Field types */
#
COB_TYPE_UNKNOWN             = 0x00    # 0
COB_TYPE_GROUP               = 0x01    # 1
COB_TYPE_BOOLEAN             = 0x02    # 2
COB_TYPE_NUMERIC             = 0x10    # 16
COB_TYPE_NUMERIC_DISPLAY     = 0x10    # 16
COB_TYPE_NUMERIC_BINARY      = 0x11    # 17
COB_TYPE_NUMERIC_PACKED      = 0x12    # 18
COB_TYPE_NUMERIC_FLOAT       = 0x13    # 19
COB_TYPE_NUMERIC_DOUBLE      = 0x14    # 20
COB_TYPE_NUMERIC_L_DOUBLE    = 0x15    # 21
COB_TYPE_NUMERIC_FP_DEC64    = 0x16    # 22
COB_TYPE_NUMERIC_FP_DEC128   = 0x17    # 23
COB_TYPE_NUMERIC_FP_BIN32    = 0x18    # 24
COB_TYPE_NUMERIC_FP_BIN64    = 0x19    # 25
COB_TYPE_NUMERIC_FP_BIN128   = 0x1A    # 26
COB_TYPE_NUMERIC_COMP5       = 0x1B    # 27
COB_TYPE_ALNUM               = 0x20    # 32
COB_TYPE_ALPHANUMERIC        = 0x21    # 33
COB_TYPE_ALPHANUMERIC_ALL    = 0x22    # 34
COB_TYPE_ALPHANUMERIC_EDITED = 0x23    # 35
COB_TYPE_NUMERIC_EDITED      = 0x24    # 36
COB_TYPE_NATIONAL            = 0x40    # 48
COB_TYPE_NATIONAL_EDITED     = 0x41    # 49

#
#   /* Field flags */
#
COB_FLAG_HAVE_SIGN      = (1 << 0)   # /* 0x0001 */
COB_FLAG_SIGN_SEPARATE  = (1 << 1)   # /* 0x0002 */
COB_FLAG_SIGN_LEADING   = (1 << 2)   # /* 0x0004 */
COB_FLAG_BLANK_ZERO     = (1 << 3)   # /* 0x0008 */
COB_FLAG_JUSTIFIED      = (1 << 4)   # /* 0x0010 */
COB_FLAG_BINARY_SWAP    = (1 << 5)   # /* 0x0020 */
COB_FLAG_REAL_BINARY    = (1 << 6)   # /* 0x0040 */
COB_FLAG_IS_POINTER     = (1 << 7)   # /* 0x0080 */
COB_FLAG_NO_SIGN_NIBBLE = (1 << 8)   # /* 0x0100 */
COB_FLAG_IS_FP          = (1 << 9)   # /* 0x0200 */
COB_FLAG_REAL_SIGN      = (1 << 10)  # /* 0x0400 */
COB_FLAG_BINARY_TRUNC   = (1 << 11)  # /* 0x0800 */
COB_FLAG_CONSTANT       = (1 << 12)  # /* 0x1000 */

COB_FOLD_NONE = 0
COB_FOLD_UPPER = 1
COB_FOLD_LOWER = 2

R_VALID_CHARS = re.compile('^[a-zA-Z0-9_]')

HEXVAL = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F']

# Unlike GnuCOBOL, we are going to replace characters we don't recognize
NON_PRINTABLE_CHAR = '^'

#  regexp's for ALL, ZERO, SPACE and numeric handling.
R_ALL   = re.compile(r"^ALL\b", re.IGNORECASE)
R_SPACE = re.compile(r'\bSPACE(S)?\b', re.IGNORECASE)
R_ZERO  = re.compile(r'\bZERO(S|ES)?\b', re.IGNORECASE)
# The following works for signed or unsigned integer or floating=point numbers
# See Rside.FromString()
R_NUMERICALCONSTANT = re.compile(r"^([+]|[-])?(\d*)[.,]?(\d*)(e|e[+]|e[-])?(\d*)$", re.IGNORECASE)

R_ALL_ZERO_SPACE = re.compile(r'ALL|ZERO(S|ES)?|SPACE(S)?', re.IGNORECASE)
R_ZERO_SPACE = re.compile(r'ZERO(S|ES)?|SPACE(S)?', re.IGNORECASE)

R_ALLSPACE = re.compile(r'ALL[ ]*SPACE(S)?', re.IGNORECASE)
R_ALLZERO = re.compile(r'ALL[ ]*ZERO(S|ES)?', re.IGNORECASE)

R_INTEGER = re.compile(r'^[-]?[0-9]+$')

R_HEXADECIMAL = re.compile(r'.*(0x[0-9a-fA-F]+).*')

# executable available flags
READELF_AVAILABLE = True
COBCDRW_AVAILABLE = True

# Note: Replace "readelf" with path to binary if it is not in your PATH.
READELF_BINARY = os.getenv('READELF', 'readelf')
COBCDRW_BINARY = os.getenv('COBCDRW', 'cobcd-rw')
COBCRUN_NAME = os.getenv('COBCRUN', 'cobcrun')  # _only_ the name, also: no extension

WATCHPOINT_NOTE = "echo note: watchpoint for "
WATCHPOINT_NOTE_PATTERN = WATCHPOINT_NOTE + r'(.*)(\\n)?'

# optional start unix path / or windows path x:\ or unc path start \\server
# any amount of folders
# filename . extension    [#2]
# optional line number    [#3]
FILE_PATTERN = r'((?:/|[a-zA-Z]:\\|\\\\)?(?:(?:\w+|\.|\.\.)[/\\])*(\w+\.\w+)(:\d+)?)'
FILE_AND_REST = r'^' + FILE_PATTERN + r'\s*(.*)$'

# This table is used by the GlobalVariable.NumberOfDigits routine
SIZE_TO_DIGITS = {1: len(str(2**(8*1))),
                  2: len(str(2**(8*2))),
                  3: len(str(2**(8*3))),
                  4: len(str(2**(8*4))),
                  5: len(str(2**(8*5))),
                  6: len(str(2**(8*6))),
                  7: len(str(2**(8*7))),
                  8: len(str(2**(8*8)))
                 }

got_complex_stop_event = True

VARSTR_LEN_SIZE = 9  # as used by cobcd-st

LENADDR_FLAG_BIAS = 10000000
LENADDR_FLAG_ADDRESS = 1
LENADDR_FLAG_LENGTH = 2

def GdbExecute(curframe, command):
    # It is up to the caller to handle exceptions
    limiting_debug_level = 1
    if GV_GlobalVariables.debug_level >= limiting_debug_level:
        print("#### ", getframeinfo(curframe).lineno, ": gdb.execute: '", command, "'", sep='')
    retval = gdb.execute(command, False, True)
    if GV_GlobalVariables.debug_level >= limiting_debug_level:
        lines = retval.split('\n')
        for line in lines:
            if len(line) > 0:
                print("####       retval:", line)
    return retval


def get_memoryview(address, length):
    # GDB creates memoryview objects for Python 3, because Python 2.6 only has buffer
    # and that is the documented API for Python 2;
    # but as Python 2.7 (which is a dependency for cbl-gdb) has backported memoryview
    # we can check for this type and convert the buffer if necessary, because conveniently
    # memoryview a constructor that takes a buffer object

    if address == 0:
        ConditionalRaise("We're not going to try to access memory with a NULL pointer")
        return None

    try:
        obj = gdb.selected_inferior().read_memory(address, length)
        if isinstance(obj, memoryview):
            return obj
    except gdb.MemoryError as ex:
        # pass on as user error to make it visible without a stacktrace
        ConditionalRaise("MemoryError: " + str(ex))
        return None

    return memoryview(obj)


def get_max_completions():
    # GDB reasonably limits the number of entries listed in the completions.
    # we want to return 1 entry more to let GDB indicate there is a limit overflow.
    return gdb.parameter("max-completion") or 9999999      # happy scrolling

def get_element_size():
    # GDB reasonably limits the output used for strings(=char arrays) and (common) arrays
    # so the user can actually see reasonable output instead of scrolling to nirvana
    # We try to follow GDBs rules here using the same parameter, but need to care to do
    # something reasonable when the setting is "unlimited", where a "None" is returned
    # Note: GC max for a single field is 2 GiB
    return gdb.parameter("print elements") or 1000000000  # 1 GB - happy scrolling


def get_repeats_size():
    # GDB reasonably combines the output for repeating characters, allowing both to "see more"
    # and to "better corelate" the data
    # We try to follow GDBs rules here using the same parameter, but need to care to do
    # something reasonable when the setting is "unlimited", where a "None" is returned
    return gdb.parameter("print repeats") or 1000000      # happy scrolling


class GlobalVariables():
    def __init__(self):
        # Establish the version of GDB
        m = re.search(r'[^0-9]*([0-9]+)\.([0-9]+)(\.([0-9]+))?', gdb.VERSION)
        self.gdb_version = int(m.group(1)) * 10000 + int(m.group(2)) * 100
        if m.group(4):
            self.gdb_version = self.gdb_version + int(m.group(4))

        if self.gdb_version < 80300:
            if self.gdb_version < 70600:
                # Sanity check with expected stack trace - as everything will be broken in this case
                raise Exception("The COBOL extension cannot work with GDB-{0}.{1} (GDB 7.6+ is required)!".format(
                    self.gdb_version / 10000, self.gdb_version % 10000))
            if self.gdb_version < 70700:
                print(" !! Warning: frame-filters and cbacktrace are not available with GDB-{0}.{1} (GDB 7.7+ is required)\n{2}".format(
                    self.gdb_version / 10000, self.gdb_version % 10000,
                    "cwatch handling will be severely limited"))
            if self.gdb_version >= 80000:
                print(" !! Warning: there are known problems with GDB-8.1 and GDB-8.2")

        # Establish what we do on ConditionalRaise:
        conraise = int(os.environ.get('CPRINT_RAISE', '1'))
        if conraise in ('1', 1):
            self.raise_error = True
        else:
            self.raise_error = False

        # Establish the debugging verbosity level:
        self.debug_level = int(os.environ.get('CPRINT_D', '0'))

        # Let's figure out some stuff about the environment
        self.SizeofPointer = calcsize('P')  # Returns 4 or 8
        self.byteorder = sys.byteorder  # Returns "big" or "little"

        if self.debug_level >= 1:
            print("#### SizeofPointer", self.SizeofPointer)
            print("#### byteorder", self.byteorder)

        self.ShortTermMemory = []

        # These are used in "cprint this=that"
        self.VarLeft = None
        self.VarLeftIndex = None


GV_GlobalVariables = GlobalVariables()


def NoNulls(s):
    ss = ""
    for ch in s:
        if ord(ch) < 32:
            ch = '.'
        ss += ch
    return ss


def OurPrint(s, end='\n'):
    # Use this routine when GDB might halt with a
    #    --Type <RET> for more, q to quit, c to continue without paging--

    # The following code catches the 'q' Quit option
    retval = None
    try:
        print(s, end=end)
    except KeyboardInterrupt as ki:
        retval = str(ki)
    except:   # TODO: use appropriate exception
        traceback.print_exc()
    return retval


def LeftRight(s, splitchar):
    """Splits what is presumably a numeric string with a possible
    splitting character into left and right pieces"""
    left = ""
    right = ""
    i = 0
    while i < len(s):
        ch = s[i]
        i += 1
        if ch == splitchar:
            break
        left += ch
    while i < len(s):
        ch = s[i]
        i += 1
        right += ch
    return left, right


def Picture(s):
    """ Expands a 9(3)V9(2) to 999V999"""
    r = ""
    lch = ''
    ch = ''
    i = 0
    ncount = 0
    while i < len(s):
        ch = s[i].upper()
        i += 1
        if ch == '(':
            ncount = 0
            while i < len(s):
                ch = s[i]
                i += 1
                if ch == ')':
                    break
                ncount *= 10
                ncount += ord(ch) - ord('0')
            ncount -= 1
            while ncount > 0:
                ncount -= 1
                r += lch
        else:
            lch = ch
            r += ch
    # To avoid certain types of twitching on the part of your poor beleagured
    # programmer, replace the digrams 'CR' and 'DB' with 'c' and 'd'.  Note
    # that CR or DB, if present, are the final characters of the string.

    n = r.find("DB")
    if n > -1:
        r = r[:n] + 'd'
    n = r.find("CR")
    if n > -1:
        r = r[:n] + 'c'

    return r


def MachineInterfaceBody(s):

    #  This sequence:
    #  variables=[{name="L00L",value="{_groupvalue=\"Sn\\\"ap!\",fullname=\"Robert\\\"Dubner\",L02={_groupvalue=\"Crackle!\",firstname = \"Robert\", lastname = \"\\\"Dubner\\\"\",L03={_groupvalue=\"Pop!\",firstname = \"Judy\", lastname = \"Ruderman\"}}}"}]
    #
    #  Results in a hierarchical display:
    #
    #  L00L: Sn"ap!
    #     fullname : Robert"Dubner
    #        L02: Crackle!
    #            firstname: Robert
    #            lastname: "Dubner"
    #            L03: Pop!
    #                firstname: Judy
    #                lastname: Ruderman
    #
    #  This one:  variables=[{name="L01",value="===\">\\/<\"==="}]
    #
    #  Results in      ===">\/<"===
    #
    #  Counting up the backslashes is, to say the least, tricky

    # Replace double-quotes and backslashes
    retval = ""
    for ch in s:
        if ch == '"':
            retval += r'\\\"'
        elif ch == '\\':
            retval += r'\\\\'
        else:
            retval += ch

    return retval


def MachineInterfaceBodyAlt(s):

    # Replace double-quotes and backslashes
    retval = ""
    for ch in s:
        if ch == '"':
            retval += r'\"'
        elif ch == '\\':
            retval += r'\\'
        else:
            retval += ch

    return retval


def ReplaceWithRepeatCountQuoted(s):
    # We know this guy has a leading quote, and presumably a trailing one
    # print("ReplaceWithRepeatCountQuoted(1)", s)
    elements = get_element_size()
    retval = ""
    # Strip off the leading and trailing quotes
    s = s[1:-1]
    rabbit = 0
    fox = 0
    prior_contraction = False
    in_a_quoted_run = False
    early_termination = False
    repeat_count = get_repeats_size()
    while rabbit < len(s):
        # Arriving here means:
        # We're here for the first time, or we just finished a run
        # So, we are going to pick up 'char', which is a candidate
        # for the start of a new run
        chunk = ""

        fox = rabbit
        char = s[rabbit]
        rabbit += 1
        while rabbit < len(s) and s[rabbit] == char:
            # This is where we proceed in a run.  Swallow up characters
            # that are the same as our candidate:
            rabbit += 1

        # We have either run out of characters, or else
        # rabbit points to a character other than char
        if rabbit - fox > repeat_count:  # This duplicates GDB behavior
            if in_a_quoted_run:
                # We have to end the current ongoing string
                # and start this contraction with a prelude
                chunk += "\", "
            # and now we can start the contraction:
            chunk += "\'"
            chunk += char
            chunk += "\' <repeats "
            chunk += str(rabbit - fox)
            chunk += ' times>, '
            prior_contraction = True
            in_a_quoted_run = False
        else:
            # The run is short enough that we just put the characters
            # into place
            prior_contraction = False
            if not in_a_quoted_run:
                # We have to start the quote:
                chunk += '"'
                in_a_quoted_run = True
            # Copy over characters until the fox catches up with the rabbit
            while fox < rabbit:
                fox += 1
                chunk += char
                if len(retval) + len(chunk) > elements:
                    # We have run out of GDB-allowed room for this string:
                    early_termination = True
                    break
        retval += chunk
        if len(retval) > elements:
            # We have run out of GDB-allowed room for this string:
            early_termination = True
            break

    # We have reached the end
    if prior_contraction:
        # strip off the unnecessary ', '
        retval = retval[:-2]
    if in_a_quoted_run:
        retval += '"'

    if early_termination and rabbit < len(s):
        retval += "..."

    # print("ReplaceWithRepeatCountQuoted(2)", retval)
    return retval


def ReplaceWithRepeatCountBracketed(s):
    if (len(s) % 2) == 1:
        # We don't know what to do with an odd-length string.  We just don't
        return s
    # We know this guy has a leading bracket, and presumably a trailing one
    # In between are a stream of undecorated hex characters.  So, this is
    # a lot like the quoted version, except we operate in pairs, rather than characters
    # print("ReplaceWithRepeatCountBracketed(1)", s)
    retval = "["
    # Strip off the leading and trailing brackets
    s = s[1:-1]
    rabbit = 0
    fox = 0
    prior_contraction = False
    in_a_quoted_run = False
    repeat_count = get_repeats_size()
    while rabbit < len(s):
        # Arriving here means:
        # We're here for the first time, or we just finished a run
        # So, we are going to pick up 'char', which is a candidate
        # for the start of a new run

        fox = rabbit
        char = s[rabbit:rabbit + 2]
        rabbit += 2
        while rabbit < len(s) and s[rabbit:rabbit + 2] == char:
            # This is where we proceed in a run.  Swallow up characters
            # that are the same as our candidate:
            rabbit += 2

        # We have either run out of characters, or else
        # rabbit points to a character other than char
        if (rabbit - fox) // 2 > repeat_count:  # See the comment in ReplaceWithRepeatCountQuoted()
            if in_a_quoted_run:
                # We have to end the current ongoing string
                # and start this contraction with a prelude
                retval += '", '
            # and now we can start the contraction:
            retval += char
            retval += " <repeats "
            retval += str((rabbit - fox) // 2)
            retval += ' times>, '
            prior_contraction = True
            in_a_quoted_run = False
        else:
            # The run is short enough that we just put the characters
            # into place
            if not in_a_quoted_run:
                in_a_quoted_run = True
            # Copy over characters until the fox catches up with the rabbit
            while fox < rabbit:
                fox += 2
                retval += char
            prior_contraction = False

    # We have reached the end
    if prior_contraction:
        # strip off the unnecessary ', '
        retval = retval[:-2]
    retval += ']'

    # print("ReplaceWithRepeatCountBracketed(2)", retval)
    return retval


def WithRepeatCount(s):
    try:
        if s and (s[0] == '"' or s[0] == '<'):
            return ReplaceWithRepeatCountQuoted(s)
        if s and s[0] == '[':
            return ReplaceWithRepeatCountBracketed(s)
        return s
    except:   # TODO: use appropriate exception
        traceback.print_exc()
        sys.exit(1)


class VarTrieNode():
    def __init__(self):
        self.parent = None
        self.children = {}
        self.piece_of_name = ""
        self.count = 0
        self.payload_index = None


def GetBytesAt(address, length):
    # Pick up <length> bytes from <address>, return them as a list of ints

    # TODO should actually raise an error, remove and handle the issue in the caller
    if not address:
        return []
        # length = 1
    # zero length literal or variable - no data here
    if not length:
        return []

    memlist = get_memoryview(address, length).tolist()
    # Note: Python2 returns a list of ints here, Python3 a list of bytes,
    #       which results in problems later - convert if necessary:
    if not isinstance(memlist[0], int):
        retval = []
        for byte in memlist:
            retval.append(ord(byte))
        return retval

    return memlist


def PutBytesAt(address, list_of_values, length):
    # Not surprisingly, this works differently in Python2 than in Python3:

    if sys.version[0] == '2':
        # Put <length> elements of list_of_values into <address> as bytes
        # We expect list_of_values to be bytes or ints
        #
        # Converting to a string is necessary for Python2
        vals = ""
        for i in range(length):
            vals += chr(list_of_values[i])
        gdb.selected_inferior().write_memory(address, vals, length)
    else:
        # Put <length> elements of list_of_values into <address> as bytes
        # We expect list_of_values to be bytes or ints
        vals = bytearray(length)
        for i in range(length):
            vals[i] = list_of_values[i]
        gdb.selected_inferior().write_memory(address, vals, length)


def GetValueAt(address, nr_bytes):
    """This routine takes a numerical address and returns the value there.

    'bytes' is usually 1, 2, 4, or 8.
    The returned value is adjusted for big- versus little-endian."""

    try:
        data_bytes = GetBytesAt(address, nr_bytes)
        if GV_GlobalVariables.byteorder == "little" and bytes:
            data_bytes.reverse()
        retval = 0
        for byte in data_bytes:
            retval = retval * 256 + byte
        return retval

    except gdb.error:
        print("Failure in GetValueAt()")
        traceback.print_exc()
        return None

def GetPointerTo(name):
    # This routine returns the address of memory referred to by 'name'.  It
    # sorts out whether the generated C declared name as "char *name" or
    # "char name[22]" or "cob_field name"

    # when name is not in context, this throws an error; it's up to our
    # caller to take care of it.

    # It turns out that we can't actually afford to parse_and_eval(name),
    # if name is a string, along with the address of the string (which is what
    # we want), it also returns the entire string.  This is bad for at least
    # two reasons: it is potentially a lot of data we don't want at this point,
    # and errors can be thrown when the string is Unicode.

    # So, we just ask for the address of name.  Sometimes that's what we want.
    # If the type is a pointer to a pointer, we have to dive down that one
    # level of indirection; we do that by directly picking up the pointer
    # that result points to:

    result = gdb.parse_and_eval('&' + name)

    if result.type.code == gdb.TYPE_CODE_PTR \
                    and result.dereference().type.code == gdb.TYPE_CODE_PTR:
        # 'result' is a pointer to a pointer, so we need to pick up
        # the value of the pointer:
        if sys.version[0] == '2':
            result = result.referenced_value()
            pointer  = int(str(result).split()[0], 16)
        else:
            pointer = int(gdb.parse_and_eval(name))
    else:
        # Otherwise, it was defined as the name of a block of some kind:
        # " char name[22] " or "cob_field name", or "int".  In that case,
        # 'result' already is the answer we need
        if sys.version[0] == '2':
            pointer  = int(str(result).split()[0], 16)
        else:
            pointer = int(result)

    return pointer

def AddEmUp(expression):
    try:
        # This works for expressions made up of constants and variables for
        # which full information is available.
        retval = int(gdb.parse_and_eval(expression or "0"))
        return retval
    except gdb.error as ex:
        if GV_GlobalVariables.debug_level >= 1:
            print("error while parsing the expression \'{0}\'".format(expression))
            print(str(ex))
        else:
            print("WARNING: Can't completely parse VARIABLE_STRING for", expression,"- not all access to sub-fields will work")
        # Returning 0, because that allows parsing to continue.
        return 0

def ExpressionEvaluate(expression):
    retval = 0
    if expression.find("b_") == -1:
        retval = AddEmUp(expression)
    else:
        #
        # There is a "b_" variable in the expression, so we have to go through
        # the effort of extracting its value.  This is critical, for example,
        # for dealing with DEPENDING ON variables, where the effective length
        # of an array can look like
        #
        #       10*(*(cob_u8_ptr)(b_8))
        #
        #print("The expression is", expression)
        #
        # The first order of business is to eliminate the "(*(cob_u8_ptr)",
        # which for our purposes is irrelevant.
        #
        # It turns out there are other things we need to get rid of:
        expr = re.sub("[*][()]cob.*_ptr[)]", "", expression)
        expr = re.sub("[(]unsignedint[)]COB_BSWAP_32", "", expr)
        expr = re.sub("[(]unsignedshort[)]COB_BSWAP_16", "", expr)
        expr = re.sub(r"[*][(]short[*][)]", "", expr)
        expr = re.sub(r"[(]short[)]COB_BSWAP_16", "", expr)
        expr = re.sub(r"[(]int[)]COB_BSWAP_32", "", expr)
        #print("The final expr is", expr)

        # We are going to walk expr, replacing each b_xxx with its current
        # numerical value:
        while True:
            match = re.search(r"^(.*)(b_[0-9])(.*)$", expr)
            if not match:
                break

            the_name = None
            for payload in GV_ModuleInformation.var_trie.storage_list:
                if payload.Bname == match.group(2):
                    the_name = payload.Name
                    break;
            if the_name:
                the_value, limit_reached = IntegerOrVariable(the_name)
                # We now reassemble the expr using the integer:
                expr = match.group(1) + str(the_value) + match.group(3)
            break
        retval = AddEmUp(expr)

    return retval

def process_left_right_args(args):
    # We accept a string, and we return one or two lists of strings
    # canonicalized to our internal COBOL variable name format

    # Either way, the results are canonicalized to our internal COBOL name format
    left_side = []
    right_side = []

    # This can be an assignment, so check for it.
    leftright = args.split('=')

    if len(leftright) > 0:
        left_side = TokenizeCanonicalName(leftright[0])

    if len(leftright) > 1:
        if len(leftright[1]) == 0:
            ConditionalRaise("Syntax error: There is nothing after the \"=\".")
            left_side = []
            right_side = []
            return left_side, right_side
        right_side = leftright[1].split()

    if right_side:
        # There is a rightside.
        right_side = leftright[1].strip()

        # Check to see if it is a quoted string
        if right_side[0] == '"':
            if len(right_side) < 2 or right_side[-1] != right_side[0]:
                ConditionalRaise("Unterminated string")
                left_side = []
                right_side = []
                return left_side, right_side
            right_side = [right_side]
        # Check for various literal integer forms:
        elif right_side[0:2].lower().replace('"', "'") == "x'":
            right_side = FromHexAlphaString(right_side)
            right_side = [right_side]
        elif right_side[0:2].lower().replace('"', "'") == "h'":
            right_side = FromHexLiteralNumber(right_side)
            right_side = [right_side]
        elif right_side[0:2].lower().replace('"', "'") == "b'":
            right_side = FromBinaryLiteralNumber(right_side)
            right_side = [right_side]
        else:
            right_side = TokenizeCanonicalName(leftright[1])

    return left_side, right_side


def get_left_right_args(args):
    left_side = []
    right_side = []
    condition = ""

    # This can be an assignment, so check for it.
    leftright = re.split('(![=]*|=[=~<>]*[i]*|~|<[=>]*[i]*|>[=]*)', args)

    # Pick up the left and right sides, and turn them into
    # "normalized" COBOL variable names.
    if len(leftright) > 0:
        if re.match(r"if\b", leftright[0], re.IGNORECASE):
            left_side = TokenizeCanonicalName(leftright[0][2:].strip())
            left_side.insert(0, "if")
        else:
            left_side = TokenizeCanonicalName(leftright[0])

    if len(leftright) > 2 and leftright[2] != '':
        # There is a rightside.
        right_side = leftright[2].strip()

        # Check to see if it is a quoted string
        if right_side[0] == '"':
            if len(right_side) < 2 or right_side[-1] != right_side[0]:
                print("A syntax error in >>{0}<<.".format(right_side))
                left_side = []
                right_side = []
                return left_side, left_side, ""
            right_side = [right_side]
        # Check for various literal integer forms:
        elif right_side[0:2].lower().replace('"', "'") == "x'":
            right_side = FromHexAlphaString(right_side)
            right_side = [right_side]
        elif right_side[0:2].lower().replace('"', "'") == "h'":
            right_side = FromHexLiteralNumber(right_side)
            right_side = [right_side]
        elif right_side[0:2].lower().replace('"', "'") == "b'":
            right_side = FromBinaryLiteralNumber(right_side)
            right_side = [right_side]
        else:
            # It's not a quoted string, nor is it a numeric literal in the form of X", H", or B"
            right_side = TokenizeCanonicalName(leftright[2])

        condition = leftright[1]
    elif len(leftright) > 2 and leftright[2] == '':
        print("Syntax error: There is nothing after the \"{0}\".".format(leftright[1]))
        left_side = []
        right_side = []
        return left_side, left_side, ""

    return left_side, right_side, condition


def PreprocessSubscriptsAndRefmods(subscripts):
    # We know this came from inside parentheses.  Although the
    # the straight CPRINT treats "ONE TWO THREE" as "ONE OF TWO OF THREE",
    # we do not.  The processing after this routine expects commas for
    # subscripts and colons for refmods.  Commas, spaces, and semicolons
    # all get treated the same.
    #
    # The logic below turns ONE TWO THREE into ONE, TWO, THREE
    # and it turns ONE OF TWO THREE into ONE TWO, THREE

    separator = ','
    if subscripts.find(':') > -1:
        # We are doing a refmod.  There won't be a separator in
        # a properly-formed refmod.
        separator = ''

    subscripts = subscripts.upper()
    subscripts = subscripts.replace(';', ' ')
    subscripts = subscripts.replace(',', ' ')
    subscripts = subscripts.replace('/', " IN ")
    subscripts = subscripts.replace('.', " IN ")
    subscripts = subscripts.replace(" OF ", " IN ")
    tokens = subscripts.split()
    i = 1
    while i < len(tokens):
        if tokens[i] != "IN" and tokens[i - 1] != "IN":
            tokens[i - 1] += separator
        i += 1
    subscripts = ' '.join(tokens)
    subscripts = subscripts.replace(" IN ", " ")
    return subscripts


def ConditionalRaise(msg):
    # I have a problem.  When running test scripts, I don't want to invoke gdb.error, because
    # it causes the scripts to stop dead.  I know of no way to cleanly avoid that.  So, I
    # collect all raises here, so I can do something about it.
    if GV_GlobalVariables.raise_error:
        raise gdb.GdbError(msg)

    print(msg)


def GetNumericArgument(argument, Negative=False, Raise=True):
    number = 1
    if argument:
        try:
            number = int(argument)
            if not Negative and number < 0:
                ConditionalRaise("argument may not be negative")
                return None
        except ValueError:
            # not int
            if Raise:
                ConditionalRaise("argument must be an integer, not " + argument)
            return None
    return number


def TokenizeCanonicalName(desired):
    # We expect a string, and we return a list of strings.

    # We allow "A of B in C", where [of|in] is not case sensitive
    # We also allow "A/B/C" and "A B C"

    # We assume that anything after a '(' is subscripts and/or refmods.  We
    # peel them off here, and tack them onto the final element of the list
    # of strings for later processing

    # At this point, if there are any apostrophes, it's time to get rid of them.
    # They get inserted by the TAB-completion process, and there can be one, or two.
    # So, just hammer them down:
    desired = desired.replace("'", '')

    suffix = ""
    nparen = desired.find('(')
    nparen2 = desired.find(')')

    if nparen > -1 and nparen2 < nparen:
        ConditionalRaise("Unmatched parentheses")
        return None

    if -1 < nparen2 < nparen:
        ConditionalRaise("Unmatched parentheses")
        return None

    if nparen2 > -1 and nparen == -1:
        ConditionalRaise("Unmatched parentheses")
        return None

    if nparen > -1:
        suffix = desired[nparen:]
        desired = desired[0:nparen]

    if desired:
        if desired[0] == "'":
            # This can happen as a result of TAB completion:
            desired = desired[1:]
            if desired[-1] == "'":
                # This can happen as a result of TAB completion:
                desired = desired[:-1]

        # turn IN and OF into spaces
        pattern = re.compile(r"^(.*) (IN|OF) (.*)$", re.IGNORECASE)
        while True:
            match = re.search(pattern, desired)
            if not match:
                break

            desired = match.group(1) + " " + match.group(3)

        desired = desired.replace("/", " ").strip()
        desired += suffix
    retval = desired.split()
    return retval


def atoi(s):
    # This isn't very bright.  But in Dubner's opinion, it doesn't need to be.
    # It ignores a leading double quote; it accepts a leading '-' as a minus sign,
    # and it converts ASCII digits to a decimal number until it hits
    # something that isn't a digit
    #
    retval = 0
    sign = 0
    i = 0
    if s[0] == '"':        # skip leading quote
        s = s[1:]
    max_pos = s.find('"')  # An embedded quote ends the conversion
    if max_pos == -1:
        max_pos = len(s)
    while i < max_pos:
        ch = s[i]
        i += 1
        if ch in ('-', '+'):
            if sign:
                # a consecutive sign ends the conversion
                break
            # A sign even with retval is legitimate
            # COBOL SIGN TRAILING --> "123-"
            # For this code it means "000-0003" will be interpreted as minus 3,
            # for now we don't care.
            if ch == '-':
                sign = -1
            else:
                sign = +1
            continue
        n = ord(ch) - ord('0')
        if n < 0 or n > 9:
            # A non-digit ends the conversion
            break
        # This is a digit 0 through 9, so accumulate it into the result
        retval *= 10
        retval += n
    if sign:
        return retval * sign
    return retval


def IsInteger(s):
    # This routine is designed to provide an indication of whether the
    # input string is nothing but digits, and return either None or the
    # relevant number.  It's intended for use where the input might be
    # an integer literal, or might be a cobol variable.

    # I don't want to use the Python isdigit(), which goes False on
    # negative numbers.

    # And I have to be careful with my atoi routine, because it will
    # return 10 for "10-DOWNING-STREET", which is a perfectly valid
    # COBOL identifier.

    return R_INTEGER.match(s)


def IntegerOrVariable(s):
    # Let's see if the string s is a simple integer
    if IsInteger(s):
        return atoi(s), False  # False means no limit overrun

    # It is not.  Let's see if it represents a gdb history/convenience variable
    if s[0] == '$':
        conv_id = s[1:]
        # Pick up the history index:
        if IsInteger(conv_id):
            try:
                val = gdb.history(atoi(conv_id))  # may raise a gdb.Error (if not yet reached) which we just pass
            except gdb.error as ex:
                raise gdb.GdbError(ex)
        else:
            if GV_GlobalVariables.gdb_version >= 80200:
                val = gdb.convenience_variable(conv_id)
            else:
                try:
                    val = str(gdb.parse_and_eval("$"+conv_id))
                    if val == "void":
                        val = None
                except gdb.error:
                    val = None
            if not val:
                raise gdb.GdbError("unknown convenience variable " + s)
        pyval = str(val)
        if pyval and len(pyval) > 2 and pyval[0] == '"' and pyval[-1] == '"':
            pyval = pyval[1:-1]
        if IsInteger(pyval):
            return atoi(pyval), False  # False means no limit overrun
        raise gdb.GdbError("only numeric gdb variables may be used with cprint, {0} is: {1}".format(s, pyval))

    # It is not.  Let's see if it represents a unique in-context COBOL variable
    possible_name = TokenizeCanonicalName(s)
    possibilities, limit_reached = get_sorted_possibilities(possible_name, 1)
    if not possibilities:
        # It's not an integer.  But there are no candidates in the variable
        # list that uniquely match.  We return None, and it is up to the caller
        # to take appropriate action.
        return None, limit_reached

    # We know that there is but one possibility because of the limit specified above:
    index = possibilities[0]
    flag = 0
    if index < 0:
        flag  = (-index) // LENADDR_FLAG_BIAS
        index = (-index) %  LENADDR_FLAG_BIAS

    payload = GV_ModuleInformation.var_trie.storage_list[index]

    if flag == 0:
        return payload.NumericValue(), False  # False means no limit overrun
    elif flag == LENADDR_FLAG_ADDRESS:
        return payload.ActualLocation, False  # False means no limit overrun
    elif flag == LENADDR_FLAG_LENGTH:
        return payload.ActualLength, False  # False means no limit overrun
    else:
        traceback.print_exc()
        raise gdb.error("Unknown LENADDR_FLAG")

class VarTrie():
    def __init__(self):
        self.storage_list = []
        self.storage_trie = VarTrieNode()
        self.program_ids = set()

        self.IndexType = 0
        self.IndexLevel = 1
        self.IndexName = 2
        self.IndexFname = 3
        self.IndexBname = 4
        self.IndexAname = 5
        self.IndexOffset = 6
        self.IndexLength = 7
        self.IndexOccursMin = 8
        self.IndexOccursMax = 9

        self.StackOfNames = []
        self.StackOfLevels = []

        # When processing a line like "05 T-ENTRIES OCCURS 90 INDEXED BY TAB-ADR-IND."
        # it turns out that TAB-ADR-IND shows up the line *before* the 05 T-ENTRIES line
        # in the generated C code.  In order to make the name hierarchy work properly,
        # I am stashing that line here until the OCCURS line (presumably the next line)
        # is seen.
        self.stashed_line = None

    @staticmethod
    def AddToTrie(trie, name_tokens, payload_index):
        our_node = trie

        for name in name_tokens:
            if name not in our_node.children:
                new_child = VarTrieNode()
                new_child.piece_of_name = name
                new_child.parent = our_node
                our_node.children[name] = new_child
            our_node = our_node.children[name]

        # We're all the way at the end:
        our_node.payload_index = payload_index
        our_node.count = 1

        # Propogate the count back up to the root:
        while our_node.parent:
            our_node.parent.count += our_node.count
            our_node = our_node.parent
        # and we are done


    def PopulateIndexOfParent(self):
        # This routine walks the self.storage_list[] and establishes the index_of_parent and
        # index_of_granddaddy for each CobolVariable element.

        # Top-level variables have no parent.  Every variable has a granddaddy; a top-level variable's
        # granddaddy is itself.

        # Variables of LEVEL 01-49, 66, 77, 78 are what you expect.
        # LEVEL 00 variables are odd ducks; they are INDEXED BY variables and need special
        # processing, because we want them to appear hierarchically within their LEVEL 01 tree,
        # but their storage isn't part of the LEVEL 01 UNION.  Their parent lies within the tree,
        # but they are their own granddaddy, because their data isn't in the UNION.

        # At the present time, there is no information in VARIABLE_STRING about FD and SD records;
        # there simply isn't anything available in a sensible way from the generated .c and .h
        # files.

        # TODO: It is permitted for LEVEL 01 variables subordinate to an FD record to have the REDEFINES
        # characteristic.  At the present time, I have no idea what will happen with code like that.

        if len(self.storage_list) > 0:
            # We know the very first element has to be his own granddaddy:
            payload = self.storage_list[0]
            payload.index_of_granddaddy = 0
            payload.SetIsDirty()

        for payload_index in range(1, len(self.storage_list)):
            payload = self.storage_list[payload_index]

            # By default, a variable is its own granddaddy.  (A LEVEL 00, 01 or >66 variable
            # doesn't have a parent.  Even so, it is its own granddaddy; that's so it can easily find
            # its own grand_data[] and grand_dirty flag
            payload.index_of_granddaddy = payload_index

            if payload.Level == 0:
                # Level 00 is an INDEXED BY variable whose parent is
                # the immediate preceding entry.  See self.LoadFromLine
                payload.index_of_parent = payload_index - 1

            elif payload.Level >= 1 and payload.Level <= 49 or payload.Level == 66 or payload.Level == 88:
                # LEVELs 02-49 obviously have parents.  LEVEL 01 usually doesn't have a parent

                # We will, however, run this code in part because we are piggybacking the
                # search for REDEFINES here.  We'll filter out payload.Level == 1 down below

                i = payload_index - 1

                # Find our parent:

                # TODO:  A better way of finding our parent.
                # Note that this following code has, inherently, O(N-squared) time complexity.
                # In practice it may not be much of a problem, because in general you will never
                # have to look very far back before finding your parent.  But O(N-squared) always
                # makes me nervous.
                while i >= 0:
                    ancestor = self.storage_list[i]
                    candidate_level = ancestor.Level
                    if candidate_level == payload.Level:
                        if payload.Level > 1:
                            # Our older relative has the same level we do.  That means we
                            # share a parent.  That value might be None, but that's okay.
                            payload.index_of_parent = ancestor.index_of_parent
                            payload.index_of_granddaddy = ancestor.index_of_granddaddy

                        # If the ancestor and payload have the same base and offset,
                        # then this is a REDEFINES.
                        if ancestor.Bname == payload.Bname and ancestor.ReferenceOffset == payload.ReferenceOffset:
                            payload.IsRedefines = True
                        break
                    if candidate_level != 0 and candidate_level < payload.Level:
                        # Because it is non-zero and smaller than we are, [i] has to be our parent.
                        # And our parent's granddaddy is also our granddaddy
                        if payload.Level > 1:
                            payload.index_of_parent = i
                            payload.index_of_granddaddy = ancestor.index_of_granddaddy
                        break
                    # Arriving here means that candidate_level is either zero, making it
                    # an INDEXED BY variable who is nobody's parent, or else candidate_level
                    # bigger than we are, making it an uncle who isn't our parent.  So,
                    # we keep looking:
                    i -= 1
            #else:
            # LEVEL 01 and anything over 49 has no parent, and is its own granddaddy, which
            # we already set by default up above.

            # Since this is a new variable, whose data has never been read, flag it as dirty:
            payload.SetIsDirty()

        if False:
            # Dump the list
            for payload_index in range(len(self.storage_list)):
                payload = GV_ModuleInformation.var_trie.storage_list[payload_index]
                parent = "No parent"
                granddaddy = "No grandparent"
                if payload.index_of_parent is not None:
                    parent = GV_ModuleInformation.var_trie.storage_list[payload.index_of_parent].Name
                if payload.index_of_granddaddy is not None:
                    granddaddy = GV_ModuleInformation.var_trie.storage_list[payload.index_of_granddaddy].Name
                print("DEBUGDUMP   ", payload.Level, payload.Name, parent, granddaddy, payload.IsDirty())

    def LoadTrieFromLine(self, tokens):
        try:
            if len(tokens) == 11:
                # We have enough in the way of tokens, so proceed
                if tokens[self.IndexType] == 'P':
                    # We are entering a new PROGRAM-ID, so reset our stacks
                    self.StackOfNames = [tokens[self.IndexName]]
                    self.StackOfLevels = [-2]

                if len(self.StackOfNames) == 0:
                    print("Please save your program for Bob Dubner at support@COBOLworx.com")
                    print("self.StackOfNames is empty, and it shouldn't be")
                    sys.exit(1)

                if tokens[self.IndexType] in ('F', 'X', 'W', "WB", 'L', 'I'):
                    # We are working with a variable.  We need to erode the
                    # stack of names down to the variable who is our parent.

                    if tokens[self.IndexLevel] == "":
                        tokens[self.IndexLevel] = '0'
                    our_level = int(tokens[self.IndexLevel])
                    if our_level in (77, 78):
                        our_level = 1
                    if our_level > 0:  # When our_level is zero, it means we are a table index,
                                       # so we don't touch the stacks.
                                       # We want the element above us to be our parent.
                        while self.StackOfLevels[-1] >= our_level or self.StackOfLevels[-1] == 0:
                            # The guy above us is either our brother(==), or a nephew(>),
                            # or he's our crazy INDEXED inlaw (zero) that nobody wants as a parent:
                            self.StackOfNames.pop(-1)
                            self.StackOfLevels.pop(-1)

                    # There is a caveat:
                    # When the GnuCOBOL compiler encounters:
                    #
                    #      05 T-ENTRIES OCCURS 90 INDEXED BY TAB-ADR-IND.
                    #
                    # The variables generated look like this:

#  /* cob_dump_field_ext ( 0, "TAB-ADR-IND", COB_SET_FLD(f0, 4, (cob_u8_t *)&b_40, &a_2), 0, 0); */
#  {
#    /* int max_1 = 90; */
#    {
#      /* cob_dump_field_ext ( 5, "T-ENTRIES", COB_SET_FLD(f0, 1037, b_36 + 10, &a_4), 0, 1, i_1, 1037UL); OCCURS 1 90 */
                    #
                    # We need to somehow associate  TAB-ADR-IND with T-ENTRIES
                    #
                    # I am going to do this by stashing the LEVEL 00 line and not processing
                    # it until right after the OCCURS line

                    if our_level == 0 and self.stashed_line is None:
                        # This is a LEVEL 00 indexed variable, and we aren't processing it yet.
                        self.stashed_line = tokens[:]
                        return

                    # and now we put ourselves into the position of
                    # most favored child
                    name_element = tokens[self.IndexName]
                    self.StackOfNames.append(name_element)
                    self.StackOfLevels.append(our_level)

                    rev_StackOfNames = self.StackOfNames[:]
                    rev_StackOfNames.reverse()

                    payload = CobolVariable()
                    payload.Program          = self.StackOfNames[0]
                    payload.Section          = tokens[self.IndexType]
                    payload.Level            = int(tokens[self.IndexLevel])
                    payload.Name             = '/'.join(rev_StackOfNames)
                    payload.Fname            = tokens[self.IndexFname]
                    payload.Bname            = tokens[self.IndexBname]
                    payload.Aname            = tokens[self.IndexAname]
                    payload.ReferenceOffset  = AddEmUp(tokens[self.IndexOffset])
                    payload.LengthExpression = tokens[self.IndexLength]
#                    payload.Length           = ExpressionEvaluate(payload.LengthExpression)
                    payload.OccursMin        = int(tokens[self.IndexOccursMin] or "0")
                    payload.OccursMax        = int(tokens[self.IndexOccursMax] or "0")

                    # print("Loading", payload.Level, payload.Name, payload.index_of_parent)
                    payload.payload_index = len(self.storage_list)
                    self.storage_list.append(payload)
                    self.AddToTrie(self.storage_trie,
                                   rev_StackOfNames,
                                   payload.payload_index)
                    self.program_ids.add(payload.Program)

                    # We just added a line.  Was it an OCCURS line?
                    if payload.OccursMax:
                        # It was an OCCURS line.
                        # Do we have a stashed LEVEL 00 INDEXED BY line?
                        if self.stashed_line:
                            # We do.  It's time to process it:
                            # (Recursion is a beautiful thing)
                            self.LoadTrieFromLine(self.stashed_line)
                            self.stashed_line = None
        except:   # TODO: use appropriate exception
            traceback.print_exc()
            sys.exit(1)

    def NameMatcher(self,
                    boys,
                    boy_index,
                    trie,
                    paths):
        # Nomenclature:  We have a list of lost_boys: A/B/C
        # We have a forest, where some of the trees are labeled.
        # The labels might be
        # A.B.C (Just one valid path)
        # X.A.B.C.Y (Just one valid path)
        # A.B.C.X.Y.Z (Just one valid path)
        # A.X.B.Y.C.Z (Just one path)
        # A.B.C.X and A.B.C.Y (two valid paths)
        # A.B.X.C and A.B.Y.C (two valid paths)
        # And so on.
        #
        # We want to wander through the forest, creating a list of paths that
        # leave off each boy at his own tree.
        #
        # In classic recursive-entry style, this routine took a lot of thinking
        # but ends up with very little code that is swamped by explanatory
        # comments.
        #
        # The intent is for this to be unimportant to the user.  It should
        # just work *right*.  If the user asks "show me FRED" and there is
        # only one possibility, then the user gets that.  If there are two
        # possibilities, then the user gets shown those possibilities with a
        # simple option for selecting one.
        #
        if boy_index < len(boys):
            # we still have at least one boy to leave off at his tree
            boy = boys[boy_index]
            # print("DEBUG boy", boy)
            # print("DEBUG trie.children.keys", trie.children.keys())
            # Do a case-insensitive match:
            bFound = False
            for child in trie.children:
                if boy.upper() == child.upper():
                    bFound = True
                    boy = child
                    break
            if bFound:
                # We have found this boy's tree.  Advance both the boy
                # and the trie.
                next_trie = trie.children[boy]
                boy_index += 1
                # Continue along that path.
                self.NameMatcher(boys, boy_index, next_trie, paths)
                # and terminate.
                return

            # We can't leave that boy at this node.  So, we have to
            # check for that boy on all possible paths from here:
            child_tries = list(trie.children.values())
            for next_trie in child_tries:
                self.NameMatcher(boys, boy_index, next_trie, paths)
            # and terminate.
            return

        # we have dropped off the whole string of boys.  Now it's a
        # question of finding every possible path to the edge of the
        # forest from that point.
        if len(trie.children) > 0:
            # We have not yet reached the edge of the forest:
            child_tries = list(trie.children.values())
            for next_trie in child_tries:
                self.NameMatcher(boys, boy_index, next_trie, paths)
            # and terminate.
            return

        # This node has no children, which means we have reached the
        # edge of the forest.

        # Log it:
        paths.append(trie.payload_index)
        #
        # and terminate
        return

    def GetAllPossibilities(self):
        paths = []
        for index in range(len(self.storage_list)):
            paths.append(index)
        return paths

    def GetMatchingPossibilities(self, desired):
        # Arriving here means that desired has an asterisk in it.
        # We will convert '*' to ".*" and leverage regex:
        pattern = '^' + desired.replace('*', ".*").upper() + '$'
        matcher = re.compile(pattern)

        paths = []
        for index in range(len(self.storage_list)):
            fullname = self.storage_list[index].Name
            for piece_of_name in fullname.split('/')[:-1]:
                matched = re.search(matcher, piece_of_name.upper())
                if matched:
                    paths.append(index)
                    break
        return paths

    def GetListOfExactPossibilities(self, token):
        # We are going to return variables that contain the given token
        # in its exact, case-insensitive, form.  This is typically used
        # by the 'cprint ?' form

        # Wander through the forest, looking for possible paths:
        paths = []
        self.NameMatcher([token], 0, self.storage_trie, paths)

        return paths

    def GetListOfPossibilities(self, cobol_variable_name):
        # We know that cobol_variable_name is a list of tokens that has already
        # been through TokenizeCanonicalName
        # We were handed a bunch of pieces.  Let's
        # glue them back together into a single space-separated string:

        # TODO: also handle GDB escaping of symbols here:
        recombined = " ".join(cobol_variable_name).upper()

        # This is the return value:
        paths = []

        # There is the possibility array subscripts (contained in parentheses,
        # and not having any colons, and/or refmods (two values, in parentheses,
        # separated by a colon.)

        subscripts = None
        refmod     = None

        while True:
            n1 = recombined.find('(')
            n2 = recombined.find(')')
            if n1 == -1 and n2 == -1:
                # No more paired parentheses
                break
            if n1 != -1 and n2 < n1 or n1 == -1 and n1 != -1:
                # The parentheses are either mismatched, or in the wrong order
                ConditionalRaise("A syntax error in >>{0}<<".format('/'.join(cobol_variable_name)))
                return paths

            # We have a pair of parenthesis.  Extract them and their contents:
            parenthesized = recombined[n1:n2 + 1]

            # and remove them from the recombined string:
            recombined = recombined[0:n1] + ' ' + recombined[n2 + 1:]

            if parenthesized.find(':') != -1:
                # This must be a refmod.
                if refmod:
                    # There can be only one refmod
                    ConditionalRaise("Too many 'refmods' in >>{0}<<".format('/'.join(cobol_variable_name)))
                    return paths
                refmod = parenthesized
            else:
                # This must be a subscripts specification.
                if subscripts:
                    # There can be only one subscripts specification
                    ConditionalRaise("Too many table references in >>{0}<<".format('/'.join(cobol_variable_name)))
                    return paths
                subscripts = parenthesized

        # At this point, we have set aside any table subscripts or refmods from the cobol_variable_name list

        name_tokens = recombined.split()

        # Wander through the forest, looking for possible paths:

        if len(name_tokens) == 0:
            # if there's nothing, there is nothing
            return paths

        self.NameMatcher(name_tokens, 0, self.storage_trie, paths)

        if len(paths) != 1:
            # A strict attempt at matching what we were given has come up
            # with bupkis.  So far we have either no possibilities, or
            # more than one possibility.

            # Put what we have so far into a set:
            set1 = set(paths)

            # When given something like FIRST IN HOLDER, we want
            # to find 10 FIRSTNAME/NAME/ACCOUNTHOLDER
            #
            # But we don't want that request to pull up
            #  10 HOLDER/NAME/FIRST
            #
            # So, we are going to march through all of the names
            # in storage_list, and then we will make sure all of
            # the pieces show up in left-to-right order

            for index in range(len(self.storage_list)):
                whole_name = self.storage_list[index].Name.upper()
                prior_index = -1
                for fragment in name_tokens:
                    fragment = fragment.upper()
                    current_index = whole_name.find(fragment, prior_index + 1)
                    if current_index == -1:
                        # This fragment isn't in that name, so, sayonara
                        prior_index = -1
                        break
                    if current_index < prior_index:
                        # Going backward isn't permitted
                        prior_index = -1
                        break
                    # Rightward, ho!
                    prior_index = current_index
                if prior_index != -1:
                    # We have found a contender!
                    # Make sure that there is no subscripts or refmod for this variable:
                    self.storage_list[index].Subscripts = None
                    self.storage_list[index].Refmod = None
                    # And add it to the set
                    set1.add(index)

            # Turn the set back into a list:
            paths = list(set1)

        # If, and only if, the length of paths is 1, then
        # we append any modifiers back onto the variable.

        # Also: If this variable has subscripts or refmods, those refmods might
        # be variables themselves.  Since at this point we can't know if
        # those subscript variables might be dirty (or have been changed through
        # a cprint a=b assignment), we have to assume we need to read our data

        if len(paths) == 1:
            payload = self.storage_list[paths[0]]
            if subscripts or payload.Subscripts:
                payload.SetIsDirty()
            payload.Subscripts = subscripts

            if refmod or payload.Refmod:
                payload.SetIsDirty()
            payload.Refmod = refmod
        return paths

    def FlagAllAsDirty(self):
        # We need to flag every variables as "needs to be read".  This typically
        # happens after execution stops because of a trap, when we can't possibly
        # know which variables have had their inferior data change:

        i = 0
        while i < len(self.storage_list):
            self.storage_list[i].SetIsDirty()
            i += 1

class LineList():
    """List of starting lines of ProgramNames"""

    # The point of this list is to be able to figure out, when a trap occurs,
    # which COBOL routine the trap occurred in.  We can't just look at the
    # information from the gdb "frame" command, because the .c code doesn't
    # use the exact same name. It's similar, but it's transmogrified.  But
    # we can know the line number where the trap occurred, and scanning
    # the .tab file lets us convert line numbers to the original COBOL
    # program-id

    def __init__(self):
        self.line_list = []  # This will be a list of (int, str) tuples
        self.EndOfTheWorld = 10000000

    def Insert(self, line_number, program_id):
        self.line_list.append((line_number, program_id))

    def Find(self, line_number):
        # returns the program-id that covers line_number

        if len(self.line_list) == 0:
            return ""

        if line_number < self.line_list[0][0] or line_number >= self.line_list[-1][0]:
            return ""

        left = 0
        right = len(self.line_list)

        while True:
            middle = (right + left) // 2
            if self.line_list[middle][0] <= line_number < self.line_list[middle + 1][0]:
                break
            if self.line_list[middle][0] > line_number:
                right = middle
            else:
                left = middle

        return self.line_list[middle][1]

    def LoadFromLine(self, tokens):
        if len(tokens) > 0 and tokens[0] == "P":
            if len(tokens) >= 3:
                program_id = tokens[2]
                line_number = int(tokens[6])
                self.Insert(line_number, program_id)
        elif len(tokens) > 0 and tokens[0] == "C":
            if len(tokens) > 2:
                GV_ModuleState.cstart_entry_name = tokens[2]

class CobolVariable():
    """Manages the field, attributes, flags, and pointers of a GnuCOBOL COBOL variable"""
    def __init__(self):
        # This block of data comes from the VariableString
        self.Program    = ""
        self.Section    = ""
        self.Level      = -1
        self.Name       = ""
        self.Fname      = ""
        self.Bname      = ""
        self.Aname      = ""
        self.ReferenceOffset = 0 # This offset is the one reported in VariableString
#        self.Length     = 0
        self.LengthExpression = ""
        self.OccursMin  = 0
        self.OccursMax  = 0
        self.AttrType   = 0
        self.AttrDigits = 0
        self.AttrScale  = 0
        self.AttrFlags  = 0
        self.IsRedefines= False
        self.Picture    = None
        self.payload_index = -1
        self.standalone = False

        # These values are calculated at runtime from the information up above
        #
        # location_of_data is the numerical address in the inferior of our grand_data
        # For working-storage and linkage, it is the value of self.Bname
        # For local-storage, it is the value of cob_local_ptr + self.ReferenceOffset
        # In some circumstances -- I don't remember which -- it can't be determined
        # from symbolic information, in which case we try to get it from Fname.data
        #
        # For working-storage and linkage, self.data_offset is equal to self.ReferenceOffset
        #
        # Local-storage variables are all based on cob_local_ptr.  Top-level granddaddy
        # variables (which happen to be declared on 16-byte boundaries, but that's
        # handled by cobcd-st) are found at different ReferenceOffsets from *cob_local_ptr.
        # We normalize here by setting location_of_data to each variable's grand_data, and
        # adjusting self.data_offset so that it is zero for grand_data.
        #
        # There is a huge exception when there is a table_offset.  The problem we have to address
        # is that a top-level variable can itself be a table, or it can have sub-levels that are
        # themselves tables, and those tables can contain other tables.  So, we simply calculate
        # one single table_offset that takes all potential offsets into account, and that's the
        # offset to our desired table element from the given base address.  So, we treat subscripted
        # elements as unique items rather than something which is sub-portion of grand_data, because
        # we don't want to be reading in huge tables just to access individual elements of them.

        self.location_of_data   = 0     # value of Fname->data, when available
        self.data_offset        = 0     # offset from location_of_data to our portion of the union.
        self.field              = None  # Inferior numerical location Fname, when available
        self.attribute          = None  # Inferior numerical location of Fname->attr, when available

        # These elements are based on parenthesized subscripts after the
        # name:  name(array_index1 , array_index2 , ...)(refmod start : refmod run)
        # Note the commas and colons, which determine which is which
        self.Subscripts         = ""    # This is the user's requested subscripts, integers or variables
        self.ResolvedSubscripts = []    # These are the resolved values (offset is one-based)
        self.table_offset       = 0     # These are additional offset bytes based on the table Subscripts

        self.Refmod             = ""    # This is the user's requested refmod
        self.RefmodOffset       = 0     # This is the zero-based Offset from (Offset:Length)
        self.RefmodLength       = 0     # This is the Length from (Offset:Length)

        self.ActualLocation     = 0     # This is location_of_data qualified by data_offset, table_offset
                                        # and table_offset
        self.ActualLength       = 0     # This is the length after applying refmod

        # Here is the string information decoded from the attributes and
        # the data we get from gdb:
        self.not_in_context = False
        self.display_body = ""

        # This flags are mostly to allow meaningful error messages when the user inputs of
        # table subscripts and/or refmods don't make sense
        self.SubscriptFailure = False  # Set to True when subscripts don't match the number of tables
        self.RefmodFailure    = False  # Set to True when something is wrong with the refmod length

        # In COBOL, variables are frequently elements of a UNION.  The top member of the UNION is
        # a LEVEL 01 variable.  We call that one the granddaddy.  When we access the granddaddy, we
        # read a copy of the granddaddy's bytes from the inferior into self.grand_data.  When
        # we access a subordinate member of the union, we don't read in the bytes again; we instead
        # access the bytes in the granddaddy CobolVariable.  In this way, we minimize the number of
        # times we transfer the data from the inferior to here.

        # Direct access of grand_data should be minimized, mainly because it is meaningless for
        # any but the top-level variable.  Instead, use the CobolVarible.GetDataBytes() function.

        # Likewise, when it is known that a variable's data needs to be re-read, the
        # granddaddy's self.grand_dirty flag is set True.  Don't access it directly; use the
        # various IsDirty functions:

        self.grand_dirty = None
        self.grand_data  = [] # list of Length bytes

        # When the VARIABLE_STRING from the generated C code is read and parsed, the variables
        # are placed, in order, in GV_ModuleInformation.var_trie.storage_list[].  Every variable
        # of LEVEL 02-49 has a parent.  Every variable has a granddaddy; top level variables
        # are their own grandpa.

        self.index_of_parent = None
        self.index_of_granddaddy = None  # Our ultimate ancestor (a LEVEL 01 variable, or ourself)

        # These are established at the run-time executable instance level.
        # on creating a new CobolVariable we are in the matching frame so can directly access its setup
        try:
            cob_module = gdb.parse_and_eval("*module")
            # CHECKME: There is likely a better way than this split...
            self.decimal_point = str(cob_module['decimal_point']).split("'")[1]
            self.comma_separator = str(cob_module['numeric_separator']).split("'")[1]
            self.currency_symbol = str(cob_module['currency_symbol']).split("'")[1]
        except gdb.error:
            self.decimal_point = '.'
            self.comma_separator = ','
            self.currency_symbol = '$'

    def IsDirty(self):
        return GV_ModuleInformation.var_trie.storage_list[self.index_of_granddaddy].grand_dirty

    def SetIsDirtyTo(self, tf):
        GV_ModuleInformation.var_trie.storage_list[self.index_of_granddaddy].grand_dirty = tf

    def SetIsDirty(self):
        self.SetIsDirtyTo(True)

    def ClearIsDirty(self):
        self.SetIsDirtyTo(False)

    def DumpJustAboutEverything(self):
        print("           Name:", self.NameToStringFull())
        if self.OccursMax:
            print(" OCCURS min/max: {0}/{1}".format(self.OccursMin, self.OccursMax))
        if self.Subscripts:
            print("     Subscripts:", self.Subscripts)
            resolved = "(" + ",".join(self.ResolvedSubscripts) + ")"
            if resolved != self.Subscripts:
                print("     (resolved):", resolved)
        if self.Refmod:
            print("         refmod:", self.Refmod)
        print("        Section:", self.Section)
        print("        Program:", self.Program)
        print("          Fname:", self.Fname or "f0")
        print("          Bname:", self.Bname)
        print("          Aname:", self.Aname)
        print("ReferenceOffset:", self.ReferenceOffset)
        if self.table_offset:
            print("    TableOffset:", self.table_offset)
        if self.Refmod:
            print("   RefmodOffset:", self.RefmodOffset)
            print("   RefmodLength:", self.RefmodLength)
        print("         Length:", ExpressionEvaluate(self.LengthExpression))
        if self.index_of_granddaddy is not None:
            print(" len(self.data):", len(self.Data()))
        print("       AttrType:", self.AttrType)
        print("     AttrDigits:", self.AttrDigits)
        print("      AttrScale:", self.AttrScale)
        print("      AttrFlags:", self.AttrFlags)
        print("    loc_of_data:", hex(self.location_of_data))
        print("    data_offset:", self.data_offset)
        print("          field:", self.field)
        print("           attr:", self.attribute)

    def IsAlphanumeric(self):
        retval = False
        if self.AttrType in (COB_TYPE_UNKNOWN,
                             COB_TYPE_GROUP,
                             COB_TYPE_BOOLEAN,
                             COB_TYPE_ALNUM,
                             COB_TYPE_ALPHANUMERIC,
                             COB_TYPE_ALPHANUMERIC_ALL,
                             COB_TYPE_ALPHANUMERIC_EDITED,
                             COB_TYPE_NUMERIC_EDITED,
                             COB_TYPE_NATIONAL,
                             COB_TYPE_NATIONAL_EDITED):
            retval = True
        return retval

    def DataFromRside(self, rside):
        data = []

        # The following routines return self.Length bytes in a bytearray
        if self.Refmod:
            # This is a little bold.  But if he wants to set a group from a string,
            # that's what is going to happen.  Of course, if the group is made up
            # of binary values the results will be unsurprising.  Useless, but
            # unsurprising.
            data = self.StringToAlphanumeric(rside)
        elif self.AttrType == COB_TYPE_NUMERIC_DISPLAY:
            data = self.StringToNumericDisplay(rside)
        elif self.AttrType == COB_TYPE_NUMERIC_BINARY:
            data = self.StringToNumericBinary(rside)
        elif self.AttrType == COB_TYPE_NUMERIC_COMP5:
            data = self.StringToNumericBinary(rside)
        elif self.AttrType == COB_TYPE_NUMERIC_PACKED:
            data = self.StringToPackedDecimal(rside)
        elif self.AttrType == COB_TYPE_NUMERIC_FLOAT:
            data = self.StringToIeeeBinary(rside)
        elif self.AttrType == COB_TYPE_NUMERIC_DOUBLE:
            data = self.StringToIeeeBinary(rside)
        elif self.AttrType == COB_TYPE_NUMERIC_FP_DEC64:
            print("DataFromRside(): Need to implement: COB_TYPE_NUMERIC_FP_DEC64")
        elif self.AttrType == COB_TYPE_NUMERIC_FP_DEC128:
            print("DataFromRside(): Need to implement: COB_TYPE_NUMERIC_FP_DEC128")
        elif self.AttrType == COB_TYPE_NUMERIC_FP_BIN32:
            print("DataFromRside(): Need to implement: COB_TYPE_NUMERIC_FP_BIN32")
        elif self.AttrType == COB_TYPE_NUMERIC_FP_BIN64:
            print("DataFromRside(): Need to implement: COB_TYPE_NUMERIC_FP_BIN64")
        elif self.AttrType == COB_TYPE_NUMERIC_FP_BIN128:
            print("DataFromRside(): Need to implement: COB_TYPE_NUMERIC_FP_BIN128")
        elif self.AttrType == COB_TYPE_ALPHANUMERIC:
            data = self.StringToAlphanumeric(rside)
        elif self.AttrType == COB_TYPE_GROUP:
            # This is a little bold.  But if he wants to set a group from a string,
            # that's what is going to happen.  Of course, if the group is made up
            # of binary values the results will be unsurprising.  Useless, but
            # unsurprising.
            data = self.StringToAlphanumeric(rside)
        elif self.AttrType == COB_TYPE_NUMERIC_EDITED:
            data = self.StringToNumericEdited(rside)
        else:
            print("DataFromRside(): Unimplemented attribute type: {0}.".format(self.AttrType))
        return data

    def PutDataFromRside(self, rside):
        # Make sure we actually have a location
        if not self.ActualLocation:
            return 0

        new_data = self.DataFromRside(rside)

        if new_data:
            # Send the new data to the inferior
            PutBytesAt(self.ActualLocation, new_data, self.ActualLength)
            self.SetIsDirty()

        return 1

    def GetDataAsBinary(self):
        retval = ""
        elements = get_element_size() // 4
        # Because "0b00000000" is so much longer than 0x00, we divide the
        # number of elements down to keep output length under control
        if self.ActualLength > 0:
            retval += "0b"
            output_bytes = self.Data()[0:elements]
            if GV_GlobalVariables.byteorder == "little" and not self.Refmod:
                if (self.AttrFlags & COB_FLAG_BINARY_SWAP) == 0:  # If this is ON, then it is already in display order
                    if (self.AttrFlags & (COB_FLAG_REAL_BINARY | COB_FLAG_IS_POINTER | COB_FLAG_IS_FP)) != 0:
                        # These three types are little-endian; flip them so they display with the big byte to the left
                        output_bytes.reverse()
            # The decimal value 65 gets converted to "0b01000001"
            #
            # '_bit' is a loop counter; there are eight generated bits per input byte.
            #
            # The "if byte >= 128" statement is equivalent to "if byte & 0x80"  So, it's
            # just a test to see if the high-order bit of byte is on.  If so, the string
            # grows by "1", otherwise it grows by "0".  At that point, 'byte' is
            # left-shifted by one (byte *= 2), and then bits to the left of 0x80
            # trimmed away (with byte %= 256).
            for byte in output_bytes:
                for _bit in range(8):
                    if byte >= 128:
                        retval += '1'
                    else:
                        retval += '0'
                    byte *= 2
                    byte %= 256
            if len(output_bytes) < len(self.Data()):
                retval += "...({0} total bytes)".format(len(self.Data()))
        return retval

    def GetDataAsHex(self):
        retval = ""
        elements = get_element_size()
        output_bytes = self.Data()[0:elements]
        if self.ActualLength > 0:
            retval = "0x"
            if GV_GlobalVariables.byteorder == "little" and not self.Refmod:
                if (self.AttrFlags & COB_FLAG_BINARY_SWAP) == 0:  # If this is ON, then it is already in display order
                    if (self.AttrFlags & (COB_FLAG_REAL_BINARY | COB_FLAG_IS_POINTER | COB_FLAG_IS_FP)) != 0:
                        # These three types are little-endian; flip them so they display with the big byte to the left
                        output_bytes.reverse()
            for byte in output_bytes:
                hexout = hex(byte)[-2:]
                if hexout[0:1] == 'x':
                    hexout = '0' + hexout[-1:]
                retval += hexout
            if len(output_bytes) < len(self.Data()):
                retval += "...({0} total bytes)".format(len(self.Data()))
        return retval

    def CalculateTableOffset(self):
        ResolvedSubscripts = []
        TableOffset = 0
        self.SubscriptFailure = False
        if self.Subscripts:
            # Strip off the leading parenthesis (we know it is there
            # from GetListOfPossibilities
            subscripts = self.Subscripts[1:]
            # If there is a trailing parenthesis, get rid of it.  It ought
            # to be there, but if somebody typed "(1,2,3" we are going to be
            # magnamimous about it.
            nfound = subscripts.find(')')
            if nfound != -1:
                subscripts = subscripts[:nfound]

            subscripts = PreprocessSubscriptsAndRefmods(subscripts)
            list_of_subscripts = subscripts.split(',')

            # The list of subscripts is just that: a list of subscripts.

            # With that list in hand, we are going to walk the hierarchical tree
            # of this variable back to the root.  Everywhere we hit an OCCURS, we
            # will apply the current list_of_subscripts[-1] to that portion of the
            # hierarchy.

            # We'll keep track along the way, to make sure the number of subscripts
            # is the same as the number of subscripted variables:

            needed = 0
            available = len(list_of_subscripts)
            variable = self
            while variable:
                if variable.OccursMax:
                    # This is a table variable:
                    needed += 1
                    if len(list_of_subscripts) == 0 or not list_of_subscripts[-1]:
                        # Empty becomes 1.  It's convenient for debugging
                        subscript = 1
                    else:
                        subscript, limit_reached = IntegerOrVariable(list_of_subscripts[-1].strip())
                        if subscript is None and limit_reached:
                            self.SubscriptFailure = True
                            ConditionalRaise("Multiple symbols match \"{0}\"".format(list_of_subscripts[-1].strip()))
                            break
                        if subscript is None:
                            self.SubscriptFailure = True
                            ConditionalRaise("No symbol matches \"{0}\"".format(list_of_subscripts[-1].strip()))
                            break
                    ResolvedSubscripts = [subscript] + ResolvedSubscripts
                    list_of_subscripts = list_of_subscripts[0:-1]
                    if subscript < 1 or subscript > variable.OccursMax:
                        msg = "The subscript '{0}({1})' is out of range (1 through {2})".format(variable.Name,
                                                                                                subscript,
                                                                                                variable.OccursMax)
                        # TODO: make the result depending on gdb.parameter("check range")
                        # check = gdb.parameter("check range")
                        # if check == "on" or check == "auto":
                        #    ConditionalRaise - and setup
                        # elif check == "warn":
                        #    print ("warning: " + msg) - and setup
                        # else
                        #    pass
                        available = 0  # This will prevent an additional superfluous "too many subscripts" message
                        needed = 0
                        self.SubscriptFailure = True
                        ConditionalRaise(msg)
                        break
                    TableOffset += (subscript - 1) * ExpressionEvaluate(variable.LengthExpression)
                if not variable.index_of_parent:
                    break
                variable = GV_ModuleInformation.var_trie.storage_list[variable.index_of_parent]

            available -= needed
            if available:
                self.SubscriptFailure = True
                if needed == 1:
                    ConditionalRaise("{0} needs {1} subscript".format(self.Name, needed))
                else:
                    ConditionalRaise("{0} needs {1} subscripts".format(self.Name, needed))
        if self.SubscriptFailure:
            ResolvedSubscripts = []
            TableOffset = 0
        if TableOffset != self.table_offset:
            # The subscript has changed
            self.SetIsDirty()
        self.ResolvedSubscripts = ResolvedSubscripts
        self.table_offset = TableOffset

    def CalculateRefmodValues(self):
        self.RefmodOffset = 0
        self.RefmodLength = 0
        self.RefmodFailure = False
        if self.Refmod:
            # There is a refmod
            # we know it starts and ends with parentheses, so strip them off
            refmod = self.Refmod[1:-1]
            refmod = PreprocessSubscriptsAndRefmods(refmod)
            refmod = refmod.split(':')
            if len(refmod) != 2:
                # There have to be two, and only two elements:
                self.RefmodFailure = True
                ConditionalRaise("The refmod {0} has to have two elements".format(self.Refmod))
                return

            if refmod[0]:
                offset, limit_reached = IntegerOrVariable(refmod[0].strip())
                if offset is None and limit_reached:
                    self.RefmodFailure = True
                    ConditionalRaise("Multiple symbols match \"{0}\"".format(refmod[0].strip()))
                    return
                if offset is None:
                    self.RefmodFailure = True
                    ConditionalRaise("No symbol matches \"{0}\"".format(refmod[0].strip()))
                    return
            else:
                offset = 1

            if offset is None:
                offset = 1

            # TODO: make the result depending on gdb.parameter("check range")
            # for "warn" and "off": use "bad" refmod as-is

            if offset < 1:
                self.RefmodFailure = True
                msg = "The refmod {0} offset has to be greater than zero".format(self.Refmod)
                ConditionalRaise(msg)
                return

            if refmod[1]:
                length, limit_reached = IntegerOrVariable(refmod[1].strip())
                if length is None and limit_reached:
                    self.RefmodFailure = True
                    ConditionalRaise("Multiple symbols match \"{0}\"".format(refmod[1].strip()))
                    return
                if length is None:
                    self.RefmodFailure = True
                    ConditionalRaise("No symbol matches \"{0}\"".format(refmod[1].strip()))
                    return
            else:
                # No value was specified, so make it the maximum
                length = ExpressionEvaluate(self.LengthExpression) - offset + 1

            if length < 1 or length > ExpressionEvaluate(self.LengthExpression):
                self.RefmodFailure = True
                msg = "The refmod {0} length has to be between 1 and {1}".format(self.Refmod, ExpressionEvaluate(self.LengthExpression))
                # msg = "The length {0} has to be between 0 and {1}".format(length, ExpressionEvaluate(self.LengthExpression))
                ConditionalRaise(msg)
                return

            if offset - 1 >= ExpressionEvaluate(self.LengthExpression):
                self.RefmodFailure = True
                msg = "The refmod {0} offset has to be less than {1}".format(self.Refmod, ExpressionEvaluate(self.LengthExpression))
                # msg = "The refmod {0} length has to be between 1 and {1}".format(self.Refmod, ExpressionEvaluate(self.LengthExpression))
                ConditionalRaise(msg)
                return
            if offset - 1 + length > ExpressionEvaluate(self.LengthExpression):
                # The requested refmod would overrun the end of the variable.
                # Silently trim it for the user:
                # FIXME: this should never be done, either "ok"/"warn" and use as-is, or ConditionalRaise
                # msg = "The refmod {0} exceeds the field size {1}".format(self.Refmod, ExpressionEvaluate(self.LengthExpression))
                length = ExpressionEvaluate(self.LengthExpression) - (offset - 1)

            self.RefmodOffset = offset - 1  # Note that RefmodOffset is zero-based
            self.RefmodLength = length
        return

    def FetchVariableData(self, raise_error):
        try:
            return self.FetchVariableDataInternal()
        except:
            if raise_error:
                raise
            return False

    def FetchVariableDataInternal(self):
        #print("FetchVariableData:", self.Name, self.Section)
        #print("FetchVariableData", self.Name, self.IsDirty(), self.ReferenceOffset, ExpressionEvaluate(self.LengthExpression))

        # We have to do this work every time.  This might be the first time
        # through, or something might have changed underneath us, like a table subscript
        # or refmod parameter.  Most significantly, the IsDirty flag might have been on,
        # and turned off by somebody else accessing another of a union we might be part
        # of and causing a re-read of the data.

        self.location_of_data = 0  # value of p_variable, or location on stack
        self.data_offset = 0
        self.field = None
        self.attribute = None
        self.display_body = ""
        self.not_in_context = False
        self.ActualLocation = 0
        self.ActualLength = 0

        # Let's update everything we might need:
        # Calculate the additional table offsets
        self.CalculateTableOffset()

        # In spite of our desire to avoid unnecessary reads, we are treating table-subscripted
        # variables as atomic.  Said another way, even in the case of
        #   01 TOP OCCURS <something>
        #      02 MIDDLE OCCURS <something>
        #         03 BOTTOM OCCURS <something>
        # asking for BOTTOM(2) will cause the data for BOTTOM(2) to be read as if it
        # were a top-level variable.  This tradeoff comes about because we don't want to
        # be reading in possibly huge tables when we just want to see one element.
        self.standalone = False

        if self.table_offset or self.Level == 0:
            self.SetIsDirty()
            self.standalone = True

        # Calculate refmod offset:length
        self.CalculateRefmodValues()

        if self.SubscriptFailure or self.RefmodFailure:
            # One of the Calculate... routines ran into trouble.  They
            # displayed an error message.  It's at this point we bail out.
            self.Subscripts = None
            self.Refmod = None
            return False

        # Before proceeding, we need to make sure our granddaddy (who might be us!)
        # has been processed.  If we are our own grandpa, then index_of_parent will be None,
        # and we shouldn't recurse, because that would lead to infinite regression.
        granddaddy = GV_ModuleInformation.var_trie.storage_list[self.index_of_granddaddy]
        if granddaddy.location_of_data == 0 and self.index_of_parent is not None and self.Level != 0:
            self.SetIsDirty()
            if not granddaddy.FetchVariableData(False):
                # If granddad can't be found, neither can we:
                self.not_in_context = True
                return False

        if self.Section in 'F':
            # Annoyingly, it has been empirically determined that SORT and FILE records can result in
            # subordinate b_ locations that aren't the same as the top-level b_ locations
            self.SetIsDirty()

        if self.location_of_data == 0:
            # When the COBOL code doesn't reference a variable, there is no f_ cob_field.  But
            # often the cobcd-st routine was able to determine the b_ + offset reference.  Likewise, sometimes
            # there is an f_value, but at this point in the code the cob_field->data value hasn't yet
            # been set up.  But, again, sometimes we have advance knowledge of what is going to be
            # there because we got information from the compiler.

            # We have seen situations where the granddaddy.Bname isn't the same as self.Bname
            # See test005, and compare b_8 and b_32 from
            #       01  SortedQtrlyPyrlDataRec.
            #         05  QPRDREmpKey
            # When that happens, we switch gears and use self.Bname

            if granddaddy.Bname != self.Bname:
                self.standalone = True

            if self.Bname:
                #print("FetchVariableData Bname", self.Bname)
                try:
                    self.location_of_data = GetPointerTo(self.Bname)
                    # print("(from Bname) self.location_of_data", hex(self.location_of_data))
                except gdb.error:
                    self.not_in_context = True
                    return False
                self.data_offset = self.ReferenceOffset
            else:
                pass

        if self.Aname:
            self.attribute = gdb.parse_and_eval(self.Aname)
            if not self.attribute:
                traceback.print_exc()
                self.not_in_context = True
                return False
        else:
            # print(" There is no self.Aname")
            pass

        if self.attribute:
            #   typedef struct __cob_field_attr
            #       {
            #       unsigned short       type;       /* Field type */
            #       unsigned short       digits;     /* Digit count */
            #       signed short         scale;      /* Field scale */
            #       unsigned short       flags;      /* Field flags */
            #       const cob_pic_symbol *pic;       /* Pointer to picture string */
            #       } cob_field_attr;

            # CHECKME: can/should we keep the GDB values here instead of the current int value?
            self.AttrType   = int(self.attribute['type'])
            self.AttrDigits = int(self.attribute['digits'])
            self.AttrScale  = int(self.attribute['scale'])
            self.AttrFlags  = int(self.attribute['flags'])
            self.Picture = ""

            curr_pic        = self.attribute['pic']

            if curr_pic:
                # There is PICTURE information; we need to build it up as a string.
                # The data are stored as described in cob_pic_symbol above.
                # As we operate on the gdb Values we are safe in concern to compilers and padding.
                offset = 0
                while True:
                    # Pick up the character:
                    ch = curr_pic['symbol']
                    if ch == 0:
                        break
                    if ch < 32:
                        # This should never happen; we're inside a PICTURE string
                        # ch = '^'
                        traceback.print_exc()
                        break

                    ch = chr(ch)

                    # Pick up the integer count:
                    count = curr_pic['times_repeated']

                    while count > 0:
                        self.Picture += ch
                        count -= 1

                    offset += 1
                    curr_pic = self.attribute['pic'] + offset

                # Just to make life easier down the road, replace DB and CR
                # with something easier to work with.
                self.Picture = self.Picture.replace("DB", "d")
                self.Picture = self.Picture.replace("CR", "c")

        if self.Section in ('F', 'X', 'W', "WB", 'I'):
            pass
        elif self.Section == 'L':
            try:
                # We are looking for something in LOCAL-STORAGE.  Local
                # storage is kept in a calloc-ed memory location named
                # cob_local_ptr.  The complication is that if we have
                # several program-ids that each have their own cob_local_ptr
                # we have to make sure we know which one we are dealing
                # with.

                program_id = GV_ModuleInformation.line_list.Find(GV_ModuleState.current_trapped_line)
                # print("in :", program_id)

                #
                # program-id is where the program-under-test is
                # trapped.  Let's see if that program-id matches
                # the final place of the desired variable:
                #
                # print( self.Name )
                split_name = self.Name.split('/')
                if split_name[-1] != program_id:
                    self.not_in_context = True
                    return False

                # To recap:  We are trapped in the routine of the desired
                # LOCAL-STORAGE variable, so we can look for it in cob_local_ptr
                # we need GDB's help finding out where cob_local_ptr + offset is:
                self.location_of_data = GetPointerTo("cob_local_ptr") + self.ReferenceOffset
                self.data_offset = self.ReferenceOffset - granddaddy.ReferenceOffset

            except gdb.error:
                self.not_in_context = True
                return False
        else:
            print("We don't know how to handle", self.Section, self.Name)
            self.not_in_context = True
            return False

        if self.location_of_data != 0:
            # With those calculations in hand, we can calculate the actual location and length:

            # print("self.Name            ", self.Name)
            # print("self.location_of_data", self.location_of_data)
            # print("self.table_offset    ", self.table_offset)
            # print("self.data_offset     ", self.data_offset)
            # print("self.RefmodOffset    ", self.RefmodOffset)

            self.ActualLocation = self.location_of_data + self.table_offset + self.data_offset + self.RefmodOffset
            self.ActualLength = self.RefmodLength or ExpressionEvaluate(self.LengthExpression)

            # Unlikely as it seems, arriving here means having successfully
            # run the gauntlet of all the reasons up above as to why you
            # can't read this data.  So, if the dirty flag is on, read the data
            # from the inferior.  (If the dirty flag isn't on, then we know our
            # granddaddy's grand_data is believed still to be valid.)

            if self.IsDirty():
                global GV_reads
                global GV_bytes_read
                GV_reads += 1

                if self.Section in ('F') and not self.standalone:
                    GV_bytes_read += ExpressionEvaluate(self.LengthExpression)
                    granddaddy.grand_data = GetBytesAt(self.location_of_data, ExpressionEvaluate(self.LengthExpression))
                    # print("We just read {0} bytes from {1}".format(ExpressionEvaluate(self.LengthExpression), hex(self.location_of_data)))
                elif self.table_offset == 0 and not self.standalone:
                    GV_bytes_read += ExpressionEvaluate(granddaddy.LengthExpression)
                    granddaddy.grand_data = GetBytesAt(granddaddy.location_of_data, ExpressionEvaluate(granddaddy.LengthExpression))
                    # print("We just read {0} bytes from {1}".format(ExpressionEvaluate(granddaddy.LengthExpression), hex(granddaddy.location_of_data)))
                else:
                    # Variables with table_offset are treated as atomic, and not part of a union:
                    GV_bytes_read += ExpressionEvaluate(self.LengthExpression)
                    granddaddy.grand_data = GetBytesAt(self.ActualLocation, self.ActualLength)
                    # print("We just read {0} bytes from {1}".format(self.ActualLength, hex(self.ActualLocation)))

                # At this point we clear the IsDirty bit...unless there is a table offset.
                #
                # Consider:
                #   01 TOP occurs 3
                # if we ask for CPRINT TOP(3) and then CPRINT TOP, we expect to see TOP(1)  The only way to
                # make that happen is to leave the IsDirty bit on at this point
                if self.table_offset:
                    self.SetIsDirty()
                else:
                    self.ClearIsDirty()

            self.display_body = self.DataToString()
        return True

    def Data(self):
        granddaddy = GV_ModuleInformation.var_trie.storage_list[self.index_of_granddaddy]

        if self.standalone:
            # Table elements with non-zero subscripts have their data starting at grand_data
            n1 = 0
        else:
            n1 = self.data_offset + self.RefmodOffset
        n2 = n1 + self.ActualLength
        return granddaddy.grand_data[n1: n2]

    def NameWithSubscripts(self):
        # Start with full name.
        if GV_ModuleState.current_display_mode in ('0', '1', '2', '6', '7', '8'):
            # Remove the program name from the right side:
            name = self.Name.split('/')
            name = '/'.join(name[0:-1])
        else:
            name = self.Name

        # Tack on table subscripts:
        if self.Subscripts:
            name += "(" + ",".join(map(str, self.ResolvedSubscripts)) + ")"
        elif self.OccursMax:
            # This is a table, but no subscript was supplied.  Assume 1:
            name += "(1)"
        # Tack on reference modifications:
        if self.Refmod:
            name += "({0}:{1})".format(self.RefmodOffset + 1, self.RefmodLength)
        return name

    def NameToStringFull(self):
        storage_type = ""
        if self.Section == 'F':
            storage_type = "I/O"
        elif self.Section == 'X':
            storage_type = "EXT"
        elif self.Section == 'W':
            storage_type = "W-S"
        elif self.Section == "WB":
            storage_type = "W-B"
        elif self.Section == 'L':
            storage_type = "L-S"
        elif self.Section == 'I':
            storage_type = "LNK"
        return "{1:02} {2} [{0}]".format(storage_type, self.Level, self.NameWithSubscripts())

    def NameWithLevel(self):
        return "{0:02} {1} ".format(self.Level, self.NameWithSubscripts())

    def NameToString(self):
        display_mode = GV_ModuleState.current_display_mode
        if display_mode == 'd':
            display_mode = GV_ModuleState.sticky_display_mode
        if display_mode in ('0', '1', '2'):
            n_str = self.NameWithLevel()        # LEVEL
        if display_mode in ('3', '4', '5'):
            n_str = self.NameToStringFull()     # LEVEL NAME/PROGRAM-ID
        if display_mode in ('6', '7', '8'):
            n_str = self.NameWithSubscripts()   # Just NAME
        return n_str

    def ShowFields(self):
        elements = get_element_size()
        try:
            bname_location = GetPointerTo(self.Bname)
        except:   # TODO: use appropriate exception
            bname_location = 0
        bname_text = ""
        if bname_location:
            bname_text = "( %s )" % hex(bname_location)

        print("Name:       ", self.NameToStringFull())
        if self.OccursMax:
            print("OCCURS:      min: {0} max: {1}".format(self.OccursMin, self.OccursMax))

        if self.Subscripts:
            print("Subscripts: ", self.Subscripts, end='')
            resolved = "(" + ",".join(map(str, self.ResolvedSubscripts)) + ")"
            if resolved != self.Subscripts:
                print("; resolves to {0}".format(resolved), end='')
            print("")

        if self.Refmod:
            print("Refmod:     ", self.Refmod, end='')
            resolved = "({0}:{1})".format(self.RefmodOffset + 1, self.RefmodLength)
            if resolved != self.Refmod:
                print("; resolves to {0}".format(resolved), end='')
            print("")

            print("Display:     ", end="")
            if self.IsAlphanumeric():
                data = self.Data()[0:elements]
                for byte in data:
                    if byte >= 32:   # valid ASCII characters
                        print(chr(byte), end="")
                    else:
                        print('^', end="")
            else:
                print("[", end="")
                data = self.Data()[0:elements]
                for byte in data:
                    print(hex(256 + byte)[-2:], end="")
                print("]", end="")

            print("")
        else:
            print("Display:    ", WithRepeatCount(self.display_body))

        print("Hex:        ", self.GetDataAsHex())

        print("Field:      ", self.Fname or "<none>")

        if self.Section == 'L':
            # This is local-storage
            print("Base:       ", "cob_local_ptr")
        else:
            print("Base:       ", self.Bname, '  ', bname_text)

        if self.IsRedefines:
            print("RefOffset:   {0} [REDEFINES]".format(self.ReferenceOffset))
        else:
            print("RefOffset:  ", self.ReferenceOffset)
        print("data_offset:", self.data_offset)
        if self.Subscripts:
            print("TableOffset:", self.table_offset)
        print("Length:     ", ExpressionEvaluate(self.LengthExpression))
        if self.Refmod:
            print("RefmodOff:  ", self.RefmodOffset)
            print("RefmodRun:  ", self.RefmodLength)
        print("DataLoc:    ", hex(self.ActualLocation))

        print("Attr:       ", self.Aname)
        print("AttrType:   ", self.AttrType, end="")
        print(" (", hex(self.AttrType), ") ", sep='', end="")
        if self.AttrType == 0:
            print(" (Unknown)", end="")
        elif self.AttrType == COB_TYPE_UNKNOWN:
            print("UNKNOWN            ", end="")
        elif self.AttrType == COB_TYPE_GROUP:
            print("GROUP              ", end="")
        elif self.AttrType == COB_TYPE_BOOLEAN:
            print("BOOLEAN            ", end="")
        elif self.AttrType == COB_TYPE_NUMERIC_DISPLAY:
            print("NUMERIC_DISPLAY    ", end="")
        elif self.AttrType == COB_TYPE_NUMERIC_BINARY:
            print("NUMERIC_BINARY     ", end="")
        elif self.AttrType == COB_TYPE_NUMERIC_PACKED:
            print("NUMERIC_PACKED     ", end="")
        elif self.AttrType == COB_TYPE_NUMERIC_FLOAT:
            print("NUMERIC_FLOAT      ", end="")
        elif self.AttrType == COB_TYPE_NUMERIC_DOUBLE:
            print("NUMERIC_DOUBLE     ", end="")
        elif self.AttrType == COB_TYPE_NUMERIC_L_DOUBLE:
            print("NUMERIC_L_DOUBLE   ", end="")
        elif self.AttrType == COB_TYPE_NUMERIC_FP_DEC64:
            print("NUMERIC_FP_DEC64   ", end="")
        elif self.AttrType == COB_TYPE_NUMERIC_FP_DEC128:
            print("NUMERIC_FP_DEC128  ", end="")
        elif self.AttrType == COB_TYPE_NUMERIC_FP_BIN32:
            print("NUMERIC_FP_BIN32   ", end="")
        elif self.AttrType == COB_TYPE_NUMERIC_FP_BIN64:
            print("NUMERIC_FP_BIN64   ", end="")
        elif self.AttrType == COB_TYPE_NUMERIC_FP_BIN128:
            print("NUMERIC_FP_BIN128  ", end="")
        elif self.AttrType == COB_TYPE_NUMERIC_COMP5:
            print("NUMERIC_COMP5      ", end="")
        elif self.AttrType == COB_TYPE_ALNUM:
            print("ALNUM              ", end="")
        elif self.AttrType == COB_TYPE_ALPHANUMERIC:
            print("ALPHANUMERIC       ", end="")
        elif self.AttrType == COB_TYPE_ALPHANUMERIC_ALL:
            print("ALPHANUMERIC_ALL   ", end="")
        elif self.AttrType == COB_TYPE_ALPHANUMERIC_EDITED:
            print("ALPHANUMERIC_EDITED", end="")
        elif self.AttrType == COB_TYPE_NUMERIC_EDITED:
            print("NUMERIC_EDITED     ", end="")
        elif self.AttrType == COB_TYPE_NATIONAL:
            print("NATIONAL           ", end="")
        elif self.AttrType == COB_TYPE_NATIONAL_EDITED:
            print("NATIONAL_EDITED    ", end="")
        print()

        print("AttrDigits: ", self.AttrDigits)
        print("AttrScale:  ", self.AttrScale)

        print("AttrFlags:  ", hex(self.AttrFlags), end='')
        if (self.AttrFlags & COB_FLAG_HAVE_SIGN) != 0:
            print(" HAVE_SIGN", end='')
        if (self.AttrFlags & COB_FLAG_SIGN_SEPARATE) != 0:
            print(" SIGN_SEPARATE", end='')
        if (self.AttrFlags & COB_FLAG_SIGN_LEADING) != 0:
            print(" SIGN_LEADING", end='')
        if (self.AttrFlags & COB_FLAG_BLANK_ZERO) != 0:
            print(" BLANK_ZERO", end='')
        if (self.AttrFlags & COB_FLAG_JUSTIFIED) != 0:
            print(" FLAG_JUSTIFIED", end='')
        if (self.AttrFlags & COB_FLAG_BINARY_SWAP) != 0:
            print(" BINARY_SWAP", end='')
        if (self.AttrFlags & COB_FLAG_REAL_BINARY) != 0:
            print(" REAL_BINARY", end='')
        if (self.AttrFlags & COB_FLAG_IS_POINTER) != 0:
            print(" IS_POINTER", end='')
        if (self.AttrFlags & COB_FLAG_NO_SIGN_NIBBLE) != 0:
            print(" NO_SIGN_NIBBLE", end='')
        if (self.AttrFlags & COB_FLAG_IS_FP) != 0:
            print(" IS_FP", end='')
        if (self.AttrFlags & COB_FLAG_REAL_SIGN) != 0:
            print(" REAL_SIGN", end='')
        if (self.AttrFlags & COB_FLAG_BINARY_TRUNC) != 0:
            print(" BINARY_TRUNC", end='')
        if (self.AttrFlags & COB_FLAG_CONSTANT) != 0:
            print(" CONSTANT ", end='')
        print()
        if self.Picture:
            print("Picture:    ", self.Picture)

        # At this point, we want to show the raw data in byte order.  Note that
        # the hex representation isn't necessarily the same as the Hex: up above;
        # that one is corrected for little-endian, and this one isn't
        raw_data = self.Data()[0:elements]
        print("Data: (HEX)   [", end=" ")
        for byte in raw_data:
            hexout = hex(256 + byte)[-2:]
            print(hexout, end=" ")
        if elements < len(self.Data()):
            print("...", end="")
        print("]")

        print("Data: (ASCII) \"", end="")
        for byte in raw_data:
            if byte >= 32:   # valid ASCII characters
                print(chr(byte), end="")
            else:
                print('.', end="")

        print('"', end="")
        if elements < len(self.Data()):
            print("...", end="")
        print("")

    def NumericValue(self):
        return atoi(self.display_body)

### In June of 2021, a decision was made to undo a set of changes made
### in September of 2020.  Specifically, it was back in 2020 that the
### Python routines for outputting COBOL variables were replaced with
### a call to cob_get_field_str.  In 2021, we discovered that was a
### mistake, because the cob_get_field_str uses stdio.h routines that
### not async-signal-safe.  It is not safe for GDB to force the
### inferior to call routines that are not async-signal-safe.  This
### problem actually cropped up when Simon was debugging some complex
### code.

    def IeeeBinaryToString(self):
        data_bytes = self.Data()
        length = len(data_bytes)

        exponent_bits = 0
        fraction_bits = 0
        bias = 0
        valid_digits = 0

        if length == 2:
            # binary16
            exponent_bits = 5
            fraction_bits = 10
            bias = 15
            valid_digits = 4
        elif length == 4:
            # binary32
            exponent_bits = 8
            fraction_bits = 23
            bias = 127
            valid_digits = 8
        elif length == 8:
            # binary64
            exponent_bits = 11
            fraction_bits = 52
            bias = 1023
            valid_digits = 16
        elif length == 16:
            # binary128
            exponent_bits = 15
            fraction_bits = 112
            bias = 16383
            valid_digits = 35
        # Handleing binary256 means doubling the 76-digit expansion below
        # elif length == 32:
        #     # binary256
        #     exponent_bits = 19
        #     fraction_bits = 236
        #     bias = 262143
        #     valid_digits = 72
        else:
            message = "Don't know how to handle IEEE 754 floating point with a length of "
            message += str(length)
            message += " bytes"
            raise Exception(message)

        # At one point I thought it useful to append this information
        # type = "  [ IEEE binary"
        # type += str(length*8)
        # type += " ]"

        # Now I don't
        data_type = ""

        exponent_mask = ((1 << exponent_bits) - 1)
        unit_point = 1 << fraction_bits
        fraction_mask = unit_point - 1

        # combine the bytes into a single value
        value = 1   # This is a guard bit; it ensure that, for example,
                    # a bunch of bytes of zeros have at least the number
                    # of bits we're going to need.

        for i in range(length - 1, -1, -1):
            value <<= 8
            value += data_bytes[i]

        # extract the various IEEE 754 fields:
        sign = (value >> exponent_bits + fraction_bits) & 1
        exponent = (value >> fraction_bits) & exponent_mask
        fraction = value & fraction_mask

        # start creating the output string:

        body = ""
        pic = ""

        if sign == 1:
            body += '-'

        if ((exponent + 1) & exponent_mask) == 0:
            # exponent is the maximum possible value
            if fraction == 0:
                body += "Inf"
                body += data_type
                return body, pic
            body = "NaN"
            body += data_type
            return body, pic

        if exponent == 0:
            if fraction == 0:
                body += "0"  # This is how GnuCOBOL prints a floating-point zero.  I think it should be "0.0"
                body += data_type
                return body, pic

            # faction is != 0, so this is a denormal number
            # fraction stays the same, with the implied binary
            # point fraction_bits to the left of the whole value
            # exponent becomes -N, where N is -(bias-1)
            exponent = -(bias - 1)
            # now, adjust fraction to look like a regular number:
            while (fraction & (1 << fraction_bits)) == 0:
                fraction <<= 1
                exponent -= 1
        else:
            # At this point, the exponent is in the range of 1 - exponent_mask-1,
            # meaning that this is an ordinary floating point number.
            # subtract bias from the exponent, as per the specification
            exponent -= bias
            # and add in the implied "1" bit to the left edge of the fraction
            fraction += unit_point

        # Now it gets interesting.  We have an exponent, which multiplies fraction
        # by a power of 2^exponent.  And we have fraction, which currently has
        # an implied binary point between bits fraction_bits+1 and fraction_bits
        #
        # our rather daunting goal is to convert this to 1.234E10
        #
        # fraction is a value ranging from 1.0000 to 1.999999....
        #
        # Take advantage of python's unlimited integer capability to find the
        # decimal equivalent of the unit place of the fraction:

        # The biggest number we anticipate working with is binary128 quadruple precision,
        # which is 34.02 digits.  So, we bump that to 35, and thus we need a 76-digit
        # value of 1.000 as our 2^0 power_of_two:

        # The power-of-two starts at 2^0, and gets divided by two each time through the loop:
        expanded = 0
        power_of_two = 1000000000000000000000000000000000000000000000000000000000000000000000000000
        mask = 1 << fraction_bits
        while mask:
            # print(power_of_two, end = ' ')
            if fraction & mask:
                # print("***", end = '')
                expanded += power_of_two
            # print(" ")
            power_of_two >>= 1
            mask >>= 1

        # expanded is still a 76-digit decimal 1.something number.  We now multiply or divide by 2

        decimal_exponent = 0
        while exponent != 0:
            if exponent < 0:
                # We want to increase the exponent, which means we want to divide
                # the fraction by two.  To avoid losing bits, we're going to
                # do that by multiplying it by 5, and dividing it by ten via
                # the decimal exponent:
                exponent += 1  # increase exponent towards zero
                expanded *= 5  # multiply by five
                decimal_exponent -= 1  # divide by ten
            else:
                # Decrease the exponent, and multiply by two
                exponent -= 1
                expanded <<= 1

            # Multiply or divide by ten to normalize the fractional part to
            # have 76 digits

            excess = len(str(expanded)) - 76

            while excess > 0:
                excess -= 1
                expanded //= 10
                decimal_exponent += 1
            while excess < 0:
                excess += 1
                expanded *= 10
                decimal_exponent -= 1

        # print(self.Name, "expanded", expanded)

        # Expanded is a 76-digit decimal expansion of the final result,
        # with the implied decimal point to the right of the left-most
        # digit.

        # Extract all the digits we need, plus one for rounding:
        expanded = int(str(expanded)[0:valid_digits + 1])
        # print(self.Name, "expanded", expanded)

        # Add five to that shortened extraction, in order to round up
        expanded += 5

        # Extract the rounded portion:
        expanded = str(expanded)[:-1]

        # Here's a tricky bit.  If valid_digits is 4, and expanded started off as 99999
        # then it became 100004 after the rounding addition, and expanded is now "10000"
        #
        # Note that we can detect this because len(expanded) is 5, not 4.
        if len(expanded) != valid_digits:
            # discard final zero
            expanded = expanded[0:-1]
            # Adjust the decimal_exponent:
            decimal_exponent += 1

        # Build up the body of the number
        body += expanded[0]
        body += self.decimal_point
        body += expanded[1:]

        # eliminate trailing zeroes:
        while body[len(body) - 1] == '0':
            body = body[:len(body) - 1]

        # Make sure there is at least one '0' to the right of the decimal point
        if body[len(body) - 1] == '.':
            body += '0'

        # Appended the exponent part of the number:
        if decimal_exponent != 0:
            body += 'E'
            if decimal_exponent > 0:
                body += '+'
            body += str(decimal_exponent)
        body += data_type
        return body, pic

    def StringToIeeeBinary(self, rside):
        retval = bytearray(ExpressionEvaluate(self.LengthExpression))
        length = ExpressionEvaluate(self.LengthExpression)

        if length == 2:
            # binary16
            exponent_bits = 5
            fraction_bits = 10
            bias = 15
            valid_digits = 4
        elif length == 4:
            # binary32
            exponent_bits = 8
            fraction_bits = 23
            bias = 127
            valid_digits = 8
        elif length == 8:
            # binary64
            exponent_bits = 11
            fraction_bits = 52
            bias = 1023
            valid_digits = 16
        elif length == 16:
            # binary128
            exponent_bits = 15
            fraction_bits = 112
            bias = 16383
            valid_digits = 34
#        elif length == 32:
#            # binary256
#            exponent_bits = 19
#            fraction_bits = 236
#            bias = 262143
#            valid_digits = 72
        else:
            message = "Don't know how to handle IEEE 754 floating point with a length of "
            message += str(length)
            message += " bytes"
            raise Exception(message)

        if rside.original_string.lower() == "inf" or rside.original_string.lower() == "+inf":
            # Positive infinity
            sign = 0
            exponent = (1 << exponent_bits) - 1
            significand = 0
        elif rside.original_string.lower() == "-inf":
            # Negative infinity
            sign = 1
            exponent = (1 << exponent_bits) - 1
            significand = 0
        elif rside.original_string.lower() == "nan" or rside == "snan":
            # signalling rside.original_string.lower()
            sign = 0
            exponent = (1 << exponent_bits) - 1
            significand = 1
        elif rside.original_string.lower() == "qnan":
            # quiet NaN
            sign = 0
            exponent = (1 << exponent_bits) - 1
            significand = (1 << (fraction_bits - 1)) + 1
        elif int(rside.left_of_decimal or "0") == 0 and int(rside.right_of_decimal or "0") == 0:
            # We've got us a zero
            sign = 0
            exponent = 0
            significand = 0
        else:
            # No way around it, now.  We're going to do the hard work of a conversion:
            if rside.is_negative:
                sign = 1
            else:
                sign = 0
            exponent = 0
            significand = 0

            decimal_exponent = rside.exponent

            # convert rside's decimal digits to that same normalized form:
            ilside = int(rside.left_of_decimal or "0")
            if ilside > 0:
                # strip off any leading zeroes:
                decdigits = str(ilside)
                decimal_exponent += (len(decdigits) - 1)
                decdigits += rside.right_of_decimal
            else:
                irside = int(rside.right_of_decimal)
                # strip off any leading zeroes
                decdigits = str(irside)
                decimal_exponent -= len(rside.right_of_decimal) - len(decdigits) + 1
            # decdigits is the combined decimal representation of rside
            # decdigits has its implied decimal point just to the right of the biggest digit
            # The decimal exponent is in decimal_exponent


            # We want to scale this number so that its representation has 76 decimal digits:
            binrep = int(decdigits)

            excess = len(str(binrep)) - 76
            while excess > 0:
                excess -= 1
                binrep //= 10
            while excess < 0:
                excess += 1
                binrep *= 10

            # binrep is now a really big binary integer whose decimal representation has
            # 76 digits, with the implied decimal place just to the right of the leftmost digit.

            # We are now going to adjust it by the decimal_exponent.  The tricky thing is we
            # are going to keep multiplying or dividing the result by two to keep it in the
            # the range of 76 digits:

            while decimal_exponent:
                if decimal_exponent > 0:
                    binrep *= 10
                    decimal_exponent -= 1
                else:
                    binrep //= 10
                    decimal_exponent += 1

                while len(str(binrep)) > 76:
                    binrep //= 2
                    exponent += 1
                while len(str(binrep)) < 76:
                    binrep *= 2
                    exponent -= 1
            # We know that at this point binrep is a value with 76 decimal digits.  The
            # implied decimal point is to the right of the left-most digit.
            #
            # We will divide by two until the left-most digit is "1"

            while str(binrep)[0] != '1':
                binrep //= 2
                exponent += 1

            # At this point, believe it or not, we are actually kind of done.  The
            # 76-digit string representation of binrep will display as 100000.... through 1999999
            # We just need to peel off the binary bits that we need in order to create the
            # significand of the floating-point number

            # Let's round, shall we?  Because that's the way we roll:

            # We have a 76-digit number.  We want to add 5 to the proper place:
            rounding_zeroes = 76 - valid_digits - 1
            binrep += int('5' + '0' * rounding_zeroes)

            # Get rid of the biggest '1', because it's hidden/implicit/implied in IEEE 754
            big_unity = 1000000000000000000000000000000000000000000000000000000000000000000000000000
            binrep -= big_unity

            significand = 0
            for i in range(fraction_bits):
                binrep *= 2
                significand *= 2
                if binrep >= big_unity:
                    binrep -= big_unity
                    significand += 1
            exponent += bias
            if exponent < 0:
                ConditionalRaise("Value is out of range; exponent too small")
                sign = 0
                exponent = 0
                significand = 0
            elif exponent >= (1 << exponent_bits):
                ConditionalRaise("Value is out of range; exponent too large")
                sign = 0
                exponent = 0
                significand = 0

        # We can now build the retval bytearray:

        mask = 0x80
        offset = 0
        if sign == 1:
            retval[offset] |= mask
        mask >>= 1
        if mask == 0:
            mask = 0x80
            offset += 1

        for i in range(exponent_bits):
            if exponent & 1 << (exponent_bits - 1 - i) != 0:
                retval[offset] |= mask
            mask >>= 1
            if mask == 0:
                mask = 0x80
                offset += 1

        for i in range(fraction_bits):
            if significand & 1 << (fraction_bits - 1 - i) != 0:
                retval[offset] |= mask
            mask >>= 1
            if mask == 0:
                mask = 0x80
                offset += 1
        retval.reverse()
        return retval

    def FpNormalize(self, coefficient, exponent):
        # convert anything to 1.234E5
        if coefficient == 0:
            exponent = 0

        str_coefficient = str(coefficient)
        # make sure it's at least two characters
        if len(str_coefficient) == 1:
            str_coefficient += '0'
            exponent -= 1
        places = len(str_coefficient) - 1   # One digit to the left of the
                                            # decimal point; places to the right
        exponent += places
        # insert the decimal point:
        str_coefficient = str_coefficient[0:1] + '.' + str_coefficient[1:]

        # strip off trailing zeroes
        while str_coefficient[-1:] == '0':
            str_coefficient = str_coefficient[:len(str_coefficient) - 1]

        # when left with "3." make it "3.0"
        if len(str_coefficient) == 2:
            str_coefficient += '0'

        return str_coefficient, exponent

    def GnuCobolDecimal128ToString(self):
        # This decodes the GnuCOBOL version of the decimal64 format.  This is
        # based on a misreading of the actual IEEE format:
        # See https://en.wikipedia.org/wiki/Decimal64_floating-point_format
        # Either that, or they were using an early draft version.  Anyway,
        # follow along to see how it works:

        # Convert the bytes of data to a single 128-bit value:
        our_bytes = self.Data()
        if self.AttrFlags & COB_FLAG_BINARY_SWAP:
            our_bytes.reverse()
        value = 1   # This is a guard bit.  Without it, we wouldn't get a
                    # 128-bit zero, for example
        for i in range(ExpressionEvaluate(self.LengthExpression) - 1, -1, -1):
            value <<= 8
            value += our_bytes[i]

        if ExpressionEvaluate(self.LengthExpression) == 16:
            # decimal128
            # The format is 1 S, 4 combination,  14 exponent, 110 coefficient
            data_type = "decimal64"
            coefficient_bits1 = 111  # Top 4 are 100x
            coefficient_bits2 = 113  # Top 4 are 0xxx
            exponent_loc1 = 128 - 1 - 2 - 14
            exponent_loc2 = 128 - 1 - 0 - 14
            exponent_bits = 14
            bias = 6176

        sign = (value >> (128 - 1)) & 1
        combination = (value >> (128 - 5)) & 0xF

        coefficient1 = value & ((1 << coefficient_bits1) - 1)
        coefficient2 = value & ((1 << coefficient_bits2) - 1)
        exponent1 = (value >> exponent_loc1) & ((1 << exponent_bits) - 1)
        exponent2 = (value >> exponent_loc2) & ((1 << exponent_bits) - 1)

        body = ""
        pic = ""
        if combination == 0b1111:  # It's NaN/Inf
            body = "NaN/Inf"
        else:
            # It's a number
            if sign == 1:
                body += '-'
            if combination >= 0b1100:
                # Mode 1:
                exponent = exponent1
                coefficient = coefficient1 + (1 << 113)
            else:
                exponent = exponent2
                coefficient = coefficient2

            exponent -= bias
            str_coefficient, exponent = self.FpNormalize(coefficient, exponent)
            body += str_coefficient

            if exponent:
                body += 'e'
                body += str(exponent)

            pic += "  [ GnuCOBOL "
            pic += data_type
            pic += " ]"
        return body, pic

    def GnuCobolDecimal64ToString(self):
        # This decodes the GnuCOBOL version of the decimal64 format.  This is
        # based on a misreading of the actual IEEE format:
        # See https://en.wikipedia.org/wiki/Decimal64_floating-point_format
        # Either that, or they were using an early draft version.  Anyway,
        # follow along to see how it works:

        # Convert the bytes of data to a single 64-bit value:
        our_bytes = self.Data()
        if self.AttrFlags & COB_FLAG_BINARY_SWAP:
            our_bytes.reverse()
        value = 1   # This is a guard bit.  Without it, we wouldn't get a
                    # 64-bit zero, for example
        for i in range(ExpressionEvaluate(self.LengthExpression) - 1, -1, -1):
            value <<= 8
            value += our_bytes[i]

        if ExpressionEvaluate(self.LengthExpression) == 8:
            # decimal64
            # The format is 1 S, 4 combination, 8 exponent, 50 coefficient
            data_type = "decimal64"
            coefficient_bits1 = 51  # Top 4 are 100x
            coefficient_bits2 = 53  # Top 4 are 0xxx
            exponent_loc1 = 64 - 1 - 2 - 10
            exponent_loc2 = 64 - 1 - 0 - 10
            exponent_bits = 10
            bias = 398

        sign = (value >> (64 - 1)) & 1
        combination = (value >> (64 - 5)) & 0xF

        coefficient1 = value & ((1 << coefficient_bits1) - 1)
        coefficient2 = value & ((1 << coefficient_bits2) - 1)
        exponent1 = (value >> exponent_loc1) & ((1 << exponent_bits) - 1)
        exponent2 = (value >> exponent_loc2) & ((1 << exponent_bits) - 1)

        body = ""
        pic = ""
        if combination == 0b1111:  # It's NaN/Inf
            body = "NaN/Inf"
        else:
            # It's a number
            if sign == 1:
                body += '-'
            if combination >= 0b1100:
                # Mode 1:
                exponent = exponent1
                coefficient = coefficient1 + (1 << 53)
            else:
                exponent = exponent2
                coefficient = coefficient2

            exponent -= bias
            str_coefficient, exponent = self.FpNormalize(coefficient, exponent)
            body += str_coefficient

            if exponent:
                body += 'e'
                body += str(exponent)

            pic += "[ GnuCOBOL "
            pic += data_type
            pic += " ]"
        return body, pic

    def IeeeDecimal64ToString(self):
        # NOTE: This code is more-or-less untested.  This is legitimate code
        # for IEEE decimal64.  As of this writing, version 3.0- of GnuCOBOL
        # has a flawed implementation.  It is internally consistent, but it
        # isn't IEEE.  See GnuCobolDecimal64ToString

        # This is the IEEE decimal64/128 format
        # See https://en.wikipedia.org/wiki/Decimal64_floating-point_format
        # GnuCOBOL uses the Binary Integer Decimal option, rather than
        # the Densely Packed Decimal option.

        # Convert the bytes of data to a single 64-bit value:
        our_bytes = self.Data()
        if self.AttrFlags & COB_FLAG_BINARY_SWAP:
            our_bytes.reverse()
        value = 1   # This is a guard bit.  Without it, we wouldn't get a
                    # 64-bit zero, for example
        for i in range(ExpressionEvaluate(self.LengthExpression) - 1, -1, -1):
            value <<= 8
            value += our_bytes[i]

        if ExpressionEvaluate(self.LengthExpression) == 8:
            # decimal64
            # The format is 1 S, 5 combination, 8 exponent, 50 coefficient
            data_type = "binary64"
            coefficient_bits = 50
            exponent_bits = 8
            combination_bits = 5
            bias = 398

        coefficient = value & ((1 << coefficient_bits) - 1)
        exponent = (value >> coefficient_bits) & ((1 << exponent_bits) - 1)
        combination = (value >> (coefficient_bits + exponent_bits)) \
                       & ((1 << combination_bits) - 1)
        sign = (value >> (coefficient_bits + exponent_bits + combination_bits)) & 1

        print(hex(value))
        print(bin(combination))
        print(hex(exponent))
        print(hex(coefficient))

        body = ""
        pic = ""
        if combination == 0b11111:  # It's NaN
            if (exponent & (1 << (exponent_bits - 1))) != 0:
                body += 's'
            else:
                body += 'q'
            body += "NaN"
        elif combination == 0b11110:  # It's Infinity
            if sign == 1:
                body += '-'
            else:
                body += '+'
            body += "Inf"
        else:
            # It's a number
            if sign == 1:
                body += '-'
            if combination >= 0b11000:
                ms_exponent = (combination >> 1) & 0x03
                ms_coefficient = 8 + (combination & 1)
            else:
                ms_exponent = (combination >> 3) & 0x03
                ms_coefficient = combination & 0x07

            exponent = (ms_exponent << exponent_bits) + exponent
            exponent -= bias
            coefficient = (ms_coefficient << coefficient_bits) + coefficient
            body += str(coefficient)

            if exponent:
                body += 'e'
                body += str(exponent)

            pic += "[ IEEE "
            pic += data_type
            pic += " ]"
        return body, pic

    def PackedDecimalToString(self):
        body = ""
        pic = ""

        # We might be dealing with a P-scaled number.  Unfortunately
        # GnuCOBOL's internal representation of packed-decimal numbers is
        # incorrect (as of version 3.1.2).  So this routine isn't going to
        # bother with it.
        #
        # When investigating the problem, note how the bytes of a p-scaled
        # number are stored outside of the range of location_of_data + field.size
        pscale = 0

        # These comments aren't consistent.  I don't actually remember
        # where they came from.  F,A,C,E traditionally means positive.  GnuCOBOL
        # has its own logic.  We'll reconcile this as best we can:

        # We have to figure out if this guy is signed, and whether
        # or not there is a sign nybble.  (comp-3 and comp-6 aren't the same)

        # The sign nybble: 0x0C = positive  I think these came an analysis of GnuCOBOL
        #                  0x0D = negative
        #                  0x0F = unsigned
        has_sign_nybble = (self.AttrFlags & COB_FLAG_NO_SIGN_NIBBLE) == 0
        signed = (self.AttrFlags & COB_FLAG_HAVE_SIGN) != 0

        # Extract all the nybbles.  The actual value is right-justified in the binary stream:
        nybbles = []
        data = self.Data()
        for i in range(ExpressionEvaluate(self.LengthExpression)):
            nybbles.append(data[i] >> 4)
            nybbles.append(data[i] & 0x0F)

        negative = False
        if has_sign_nybble:
            # Extract and remove the sign nybble from nybbles
            sign_nybble = nybbles[-1]
            nybbles = nybbles[:-1]

            if sign_nybble not in (0xf, 0xa, 0xc, 0xe):
                negative = True

            if sign_nybble == 0xf:
                signed = False

        # Convert the nybbles to a string of digits:
        body = ""
        for nybble in nybbles:
            body += chr(nybble + ord('0'))

        # Trim the string to the number of digits:
        body = body[len(body) - self.AttrDigits:]

        if pscale > 0:
            # The extra zeroes are to the left of the decimal point
            body += '0' * pscale
        elif pscale < 0:
            # The extra decimal points are between the decimal point and the digits
            body = ('0' * -pscale) + body
            body = self.decimal_point + body
        elif self.AttrScale > 0:
            body = body[:self.AttrDigits - self.AttrScale] + \
                   self.decimal_point + \
                   body[self.AttrDigits - self.AttrScale:]

        if negative:
            body = '-' + body

        if signed and not negative:
            body = '+' + body

        pic += "[ "
        if signed:
            pic += "SIGNED"
        else:
            pic += "UNSIGNED"
        pic += " PACKED DECIMAL ("
        pic += str(self.AttrDigits - self.AttrScale)
        pic += ','
        pic += str(self.AttrScale)
        pic += ") ]"
        return body, pic

    def StringToPackedDecimal(self, rside):
        # The sign nybble: 0x0C = positive
        #                  0x0D = negative
        #                  0x0F = unsigned
        # Note: Those values are from the internet, and match a number of
        # documents.  I haven't strictly verified what GNUCobol does, except
        # that I have seen 0x0D for negative numbers and I have seen 0x0C
        # for positives.

        if rside.is_negative:
            sign_nybble = 0x0D  # Indicates negative
        else:
            sign_nybble = 0x0C  # Indicates positive

        if (self.AttrFlags & COB_FLAG_HAVE_SIGN) == 0:
            sign_nybble = 0x0F  # Indicates unsigned

        digits = self.RsideToInteger(rside)

        print("digits", digits)
        # Reverse the string, because we will build it up
        # from low-order to high-order
        digits = digits[::-1]

        # We can now put together the retval bytearray
        retval = bytearray(ExpressionEvaluate(self.LengthExpression))
        offset = len(retval) - 1
        mask = 0
        if (self.AttrFlags & COB_FLAG_NO_SIGN_NIBBLE) == 0:
            retval[offset] |= ((sign_nybble & 0xF) << (mask * 4))
            mask += 1
            offset -= mask // 2
            mask %= 2

        for ch in digits:
            nch = ord(ch) - ord('0')
            nch &= 0x0F
            nch <<= (mask * 4)
            retval[offset] |= nch
            mask += 1
            offset -= mask // 2
            mask %= 2
        return retval

    def NumericDisplayToString(self):
        # numeric digits, but with a decimal point:
        body = ""
        pic = ""

        negative = False
        data = self.Data()
        if len(data) > 0:
            # DISPLAY keeps the negative bit in an oddball place:
            if (data[len(data) - 1] & 0x40) != 0:
                negative = True

            # Convert the binary value in self.Data() to a string
            for _i, d in enumerate(data):
                digit = d & 0x3F
                if digit < 32:
                    body += '^'
                else:
                    body += chr(digit)

            # We might be dealing with a P-scaled number:
            if self.AttrDigits > len(data):
                if self.AttrScale > 0:
                    pscale = -(self.AttrDigits - len(data))
                else:
                    pscale = +(self.AttrDigits - len(data))
            else:
                pscale = 0

            if pscale > 0:
                body += '0' * pscale
            elif pscale < 0:
                # scaling to the right of the decimal point
                body = ('0' * -pscale) + body
                body = self.decimal_point + body
            elif self.AttrScale > 0:
                radix_point = len(data) - self.AttrScale
                body = body[:radix_point] + self.decimal_point + body[radix_point:]

            if negative:
                body = '-' + body
            else:
                if (self.AttrFlags & COB_FLAG_HAVE_SIGN) != 0:
                    body = '+' + body

            pic += "[ DISPLAY ("
            pic += str(self.AttrDigits - self.AttrScale)
            pic += ','
            pic += str(self.AttrScale)
            pic += ") ]"
        return body, pic

    def RsideToInteger(self, rside):
        """ Returns a string of decimal digits, with the implicit
            decimal point in the correct place"""
        # Try to live with the idiocy of users:
        left  = rside.left_of_decimal
        right = rside.right_of_decimal
        exp = rside.exponent
        while exp > 0:
            exp -= 1
            if right:
                left += right[0]
                right = right[1:]
            else:
                left += '0'
            if len(left) == 0 and right[-self.AttrDigits:] == '0' * self.AttrDigits:
                # This will handle absurd exponents without taking geologic time
                break
        while exp < 0:
            exp += 1
            if left:
                right = left[-1] + right
                left = left[0:-1]
            else:
                right = '0' + right
            if len(right) == 0 and left[0:self.AttrDigits] == '0' * self.AttrDigits:
                break

        # left and right now contain the digits we need, with the decimal fraction
        # and exponents taken into account.

        # Build fences:
        number_of_digits = self.NumberOfDigits()
        left  = '0' * (number_of_digits - len(left) ) + left
        right = right + '0' * (number_of_digits - len(right))

        if self.AttrType != COB_TYPE_NUMERIC_DISPLAY or self.AttrDigits <= ExpressionEvaluate(self.LengthExpression):
            # start off with the rightmost digits of left:
            data = left[-(number_of_digits - self.AttrScale):]
            # and continue with the leftmost digits of right:
            data += right[0:self.AttrScale]
        else:
            # This is a P-scaled value:
            padding = self.AttrDigits - ExpressionEvaluate(self.LengthExpression)
            if self.AttrScale > 0:
                # This is a ppp9999 scaled value.  Return the required
                # fractional digits
                data = right[padding:padding + ExpressionEvaluate(self.LengthExpression)]
            else:
                # Return the required decimal digits
                data = left[-self.AttrDigits : -(self.AttrDigits - ExpressionEvaluate(self.LengthExpression))]
        return data

    def StringToNumericDisplay(self, rside):
        # Get the normalized self.AttrDigits digits:
        digits = self.RsideToInteger(rside)

        # Turn the string (+-0-9 only) into bytes:
        data = bytearray(digits, "ascii")

        if rside.is_negative and (self.AttrFlags & COB_FLAG_HAVE_SIGN):
            data[self.AttrDigits - 1] |= 0x40

        return data

    def NumericBinaryToString(self):
        # Generic Binary

        if (self.AttrFlags & COB_FLAG_IS_POINTER) != 0:
            return self.GetDataAsHex(), ""

        pcount = 0
        for ch in Picture(self.Picture):
            if ch == 'P':
                pcount += 1

        # we can be handed binary data in either little-endian
        # or big-endian.  We will operated little-endian:

        our_bytes = self.Data()

        if (self.AttrFlags & COB_FLAG_BINARY_SWAP) != 0:
            # Unless told otherwise
            our_bytes.reverse()

        # we need to figure out if we are dealing with a signed
        # and negative number.

        body = ""
        pic = ""

        if self.Refmod:
            # Special case of refmod.  Just dump the desired bytes in in hex
            i = self.RefmodOffset
            data = self.Data()
            # Converts the array of bytes returned by self.Data() to
            # a stream of ASCII hexadecimal values.  j is a loop counter so that
            # the conversion code is executed once for every byte in the list.
            #
            # The python hex() function returns a variable number of characters, for
            # example, hex(1) returns 0x1.  Need two hex digits no matter what.
            # 0x100 is added to every byte, which means that I know the output will be in the
            # range 0x100 through 0x1FF.  And then strip off the final two characters of
            # that with [-2:], and append them to the growing string named 'body'.
            for _i in range(self.RefmodLength):
                body += hex(256 + data[i])[-2:]    # Add 0x100 to get leading zeroes
            return body, pic

        negative = False
        if (self.AttrFlags & COB_FLAG_HAVE_SIGN) != 0:
            # This is a binary number that can be signed; let's
            # see if it is.
            if (our_bytes[ExpressionEvaluate(self.LengthExpression) - 1] & 0x80) != 0:
                # and it is negative
                negative = True

                # we need to do a complement & increment.  We will
                # be fighting Python the whole way:

                carry = 1
                for i in range(ExpressionEvaluate(self.LengthExpression)):
                    our_bytes[i] = ~our_bytes[i]
                    our_bytes[i] &= 0xFF
                    our_bytes[i] += carry
                    carry = our_bytes[i] >> 8
                    our_bytes[i] &= 0xFF

        # at this point, the negative flag has been set, and
        # our_bytes is the absolute value of the output
        # build up the little-endian value:

        value = 0
        for i in range(ExpressionEvaluate(self.LengthExpression) - 1, -1, -1):
            value <<= 8
            value += our_bytes[i]

        length = self.NumberOfDigits()

        # Do an initial conversion to a string, keeping in mind that because of
        # COBOL truncation issues, value may have more digits to the left than
        # we actually want to see.
        body += str(value)
        body = body[-length:]

        # We need to expand it by adding leading zeroes to get up
        # to the appropriate length

        while len(body) < length:
            body = '0' + body

        if pcount > 0:
            if self.AttrScale < 0:
                for i in range(pcount):
                    body = body[1:] + '0'
            else:
                for i in range(pcount):
                    body = '0' + body[1:]
                body = '.' + body
        else:
            # Let's put in a decimal point, if necessary
            if self.AttrScale > 0:
                breakat = len(body) - self.AttrScale
                body = body[:breakat] + self.decimal_point + body[breakat:]

        pic += '['
        if (self.AttrFlags & COB_FLAG_HAVE_SIGN) != 0:
            if negative:
                body = '-' + body
            else:
                body = '+' + body
            pic += " signed "
        else:
            pic += " unsigned "

        pic += "binary-"
        pic += str(ExpressionEvaluate(self.LengthExpression) * 8)
        if self.AttrScale != 0:
            pic += " ("
            pic += str(self.AttrDigits - self.AttrScale)
            pic += ','
            pic += str(self.AttrScale)
            pic += ")"

        pic += ' ]'
        return body, pic

    def StringToNumericBinary(self, rside):
        # Generic binary

        # Get self.AttrDigits decimal digits:
        digits = self.RsideToInteger(rside)

        value = int(digits)
        if rside.is_negative:
            value = -value

        # Build up that value as little-endian
        retval = bytearray(ExpressionEvaluate(self.LengthExpression))
        for i in range(ExpressionEvaluate(self.LengthExpression)):
            retval[i] += value & 0xFF
            value = value // 256

        if self.AttrFlags & COB_FLAG_BINARY_SWAP:
            retval.reverse()
        return retval

    def AlphanumericToString(self):
        # ALPHANUMERIC
        data = self.Data()
        body = '"'
        for _i, d in enumerate(data):
            if d < 32:
                body += '^'
            else:
                body += chr(d)
        body += '"'
        pic = '[ alphanumeric-'
        pic += str(ExpressionEvaluate(self.LengthExpression))
        pic += ' ]'
        return body, pic

    def StringToAlphanumeric(self, rside):
        data = ""
        i = 0

        # if has_all, then we just keep repeating the input
        # string until the left side is filled:
        if rside.has_all:
            limit2 = ExpressionEvaluate(self.LengthExpression)
        else:
            limit2 = len(rside.original_string)

        while i < ExpressionEvaluate(self.LengthExpression) and i < limit2:
            data += rside.original_string[i % len(rside.original_string)]
            i += 1

        data += ' ' * (ExpressionEvaluate(self.LengthExpression) - i)
        # convert to bytearray, encoding hack for now:
        try:
            array = bytearray(data, "utf8")
        except UnicodeError:
            array = bytearray(data, "ISO-8859-1")
        return array

    def NumericEditedToString(self):
        # NUMERIC(EDITED)
        body = ""
        pic = ""
        data = self.Data()
        for _i, d in enumerate(data):
            if d < 32 or d >= 128:
                body += '.'
            else:
                body += chr(d)

        if False:  # False means "Match observed GnuCOBOL v3.00rc1 behavior"

            # There seems to be an inconsistency in GnuCOBOL's behavior.

            # Given the value 1234.56,
            # a variable declared 9,999V99 will display as 1,23456
            # a variable declared 9,999.99 will display as 1,234.56
            # Both of those are represented internally as NUMERIC-EDITED

            # Meanwhile,
            # a variable declared 9999V99 will display as 1234.56
            # a variable declared 9999.99 will display as 1234.56
            # Both of those are represented internally as NUMERIC-DISPLAY

            # I haven't asked Simon Sobisch about this, and I haven't found
            # anything definitive in documentation avaialable on the Web; the
            # topic just seems too danged complex and there are too many
            # possibilities to enumerate them all.

            # I put in the "if False" up above to make cprint match cobc
            # behavior, mainly so that my automated selfcheck program can
            # work properly.  Making it "if True" will put the decimal point
            # back on display, which really seems like the right thing. I
            # have the feeling cobc should be changed, or at least the
            # inconsistency explained.

            # If self.Picture does not contain the decimal point character,
            # AND self.AttrScale indicates there are characters to the right
            # of a decimal point, then we need to insert one.

            if self.Picture.find(self.decimal_point) == -1 and self.AttrScale > 0:
                body = body[:len(body) - self.AttrScale] + self.decimal_point + body[-self.AttrScale:]

        pic += '[ PIC '
        pic += self.Picture
        pic += ' ]'
        body = '"' + body + '"'
        return body, pic

    def NumberOfDigits(self):
        retval = self.AttrDigits
        if not (self.AttrFlags & COB_FLAG_BINARY_TRUNC) and ((self.AttrFlags & COB_FLAG_REAL_BINARY) or (self.AttrType == COB_TYPE_NUMERIC_BINARY)):
            if ExpressionEvaluate(self.LengthExpression) in SIZE_TO_DIGITS:
                retval = SIZE_TO_DIGITS[ExpressionEvaluate(self.LengthExpression)]
            else:
                ConditionalRaise("We don't know how to handle a REAL_BINARY with a size of {0}.".format(ExpressionEvaluate(self.LengthExpression)))
                retval = 3
        return retval

    def StringToNumericEdited(self, rside):
        retval = bytearray()

        number_of_digits = self.NumberOfDigits()

        data_input = self.RsideToInteger(rside)
        # We have to look for "floating insertion" symbols
        # A floating insertion range is indicated by duplicated '$', '+', or
        # '-' characters.  One caveat: the floating range can have embedded
        # commas in it.

        pic_without_commas = ""
        for ch in self.Picture:
            if ch != self.comma_separator:
                pic_without_commas += ch

        floating_insertion = None
        floating_insertion_start = None
        floating_insertion_end = None

        currency_doubled = self.currency_symbol + self.currency_symbol
        if pic_without_commas.find(currency_doubled) > -1:
            floating_insertion = self.currency_symbol
        elif pic_without_commas.find("++") > -1:
            floating_insertion = '+'
        elif pic_without_commas.find("--") > -1:
            floating_insertion = '-'

        # find the start and end of the floating sequence in self.Picture
        if floating_insertion:
            for i in range(len(self.Picture)):
                if self.Picture[i] == floating_insertion:
                    if floating_insertion_start is None:
                        floating_insertion_start = i
                    floating_insertion_end = i

        # We need to convert leading zeroes to the 'z' character:
        # zero-flag the integer portion
        i = 0
        while i < number_of_digits - self.AttrScale:
            if data_input[i] == '0':
                data_input = data_input[:i] + 'z' + data_input[i + 1:]
            else:
                break
            i += 1
        if i >= number_of_digits - self.AttrScale:
            # The entire integer part is zero.  How about the fractional part?
            if self.AttrScale > 0:
                if int(data_input[-self.AttrScale:]) == 0:
                    # The fractional part is also zero; flag them, too
                    data_input = data_input[0:number_of_digits - self.AttrScale]
                    for i in range(self.AttrScale):
                        data_input += 'z'

        # Recap:  input is now abs(numeric input), with leading
        # zeroes to the left of the decimal position replaced with 'z'
        # (Or everthing is 'z' if the value was zero to begin with)

        # We will now walk the picture and input strings from right
        # to left, building the resulting string as we go

        pindex = len(self.Picture) - 1
        dindex = len(data_input) - 1

        # This is, perforce, a state machine.  At this stage of development
        # is is mostly an ad hoc state machine.

        body = ""

        rightmost = True
        did_floating = False

        while pindex >= 0:
            pch = self.Picture[pindex]
            pindex -= 1

            if rightmost:
                rightmost = False

                if pch == '+':
                    if rside.is_negative:
                        body += '-'
                    else:
                        body += '+'
                    continue

                if pch == '-':
                    if rside.is_negative:
                        body += '-'
                    else:
                        body += '+'
                    continue

                if pch == 'c':
                    # This is a CR (See the Picture() function)
                    if rside.is_negative:
                        body += 'RC'    # We are building body backwards
                    else:
                        body += "  "
                    continue

                if pch == 'd':
                    # This is a DB
                    if rside.is_negative:
                        body += 'BD'
                    else:
                        body += "  "
                    continue

            if did_floating:
                # Everything to the left of the floating_insertion is
                # supposed to be a blank
                body += ' '
                continue

            if not did_floating and pindex + 1 == floating_insertion_start:
                # The leftmost character of the floating_insertion has
                # to be set, even at the cost of truncating the input
                did_floating = True
                if floating_insertion == '-':
                    if rside.is_negative:
                        body += '-'
                    else:
                        body += ' '
                    continue
                if floating_insertion == '-':
                    if rside.is_negative:
                        body += '-'
                    else:
                        body += '+'
                    continue
                if floating_insertion == self.currency_symbol:
                    body += self.currency_symbol
                    continue

            if "PV".find(pch) > -1:
                # We ignore scaling P characters here, because we processed
                # them earlier by scaling the input string
                # We also ignore the implicate decimal point 'V'
                continue

            inside_floating = False
            if floating_insertion_start is not None:
                inside_floating = (pindex + 1 >= floating_insertion_start and pindex + 1 <= floating_insertion_end)

            include_literal = "B0/,."
            if not inside_floating:
                include_literal += self.currency_symbol
            if include_literal.find(pch) > -1:
                body += pch
                continue

            # After this point, we need to pull out an input character
            dch = data_input[dindex]
            dindex -= 1

            if inside_floating:
                if dch == 'z':
                    did_floating = True
                    body += floating_insertion
                    continue

            if pch == '9' or inside_floating:
                # Explicit characters are expressed explicitly
                if dch == 'z':
                    dch = '0'
                body += dch
                continue

            if pch == 'Z':
                # Convert leading zeroes to blanks
                if dch == 'z':
                    dch = ' '
                body += dch
                continue

            if pch == '*':
                # Convert leading zeroes to asterisks
                if dch == 'z':
                    dch = '*'
                body += dch
                continue

        # We've built body.  Flip it right-side around
        body = body[::-1]

        # Depending on the numbers, we could have some interesting
        # entries.  Let's just power through them.

        # Get rid of commas preceded by spaces
        wipe_me = ' ' + self.comma_separator
        while True:
            i = body.find(wipe_me)
            if i == -1:
                break
            body = body[:i] + "  " + body[i + 2:]

        # Get rid of commas preceded by asterisks:
        wipe_me = '*' + self.comma_separator
        while True:
            i = body.find(wipe_me)
            if i == -1:
                break
            body = body[:i] + "**" + body[i + 2:]

        # Get rid of commas preceded by currency symbols:
        wipe_me = self.currency_symbol + self.comma_separator
        while True:
            i = body.find(wipe_me)
            if i == -1:
                break
            body = body[:i] + ' ' + self.currency_symbol + body[i + 2:]

        if (body[-2:] == 'CR' or body[-2:0] == 'DB') and body[-3] == ' ':
            body = body[:-2] + "  "

        if (body[-2:] == 'CR' or body[-2:0] == 'DB') and body[-4:-2] == " .":
            body = body[:-2] + "  "

        if (body[-1:] == '+' or body[-1] == '-') and body[-2] == ' ':
            body = body[:-1] + " "

        if (body[-1:] == '-' or body[-1] == '+') and body[-3:-1] == " .":
            body = body[:-1] + " "

        # If we ended up with just a decimal point, get rid of that:
        if body.strip() == self.decimal_point:
            i = body.find(self.decimal_point)
            body = body[:i] + ' ' + body[i + 1:]

        # CHECKME: What happens with invalid data here or above?
        retval = bytearray(body.encode("utf8"))
        return retval

    def DataToString(self):
        noisy = False

        if noisy:
            self.DumpJustAboutEverything()

        value = ""
        pic = "Unknown"
        if self.Refmod:
            if noisy:
                print("COB_TYPE_GROUP (REFMOD)")
                # note: cp /d packed(1:) still show the "wrong" type and attributes
                #       but the normal cp and cp/x _seem_ to work fine
            value, pic = self.AlphanumericToString()
        elif self.AttrType == COB_TYPE_NUMERIC_DISPLAY:
            if noisy:
                print("COB_TYPE_NUMERIC_DISPLAY")
            value, pic = self.NumericDisplayToString()
        elif self.AttrType == COB_TYPE_NUMERIC_BINARY:
            if noisy:
                print("COB_TYPE_NUMERIC_BINARY")
            value, pic = self.NumericBinaryToString()
        elif self.AttrType == COB_TYPE_NUMERIC_COMP5:
            if noisy:
                print("COB_TYPE_NUMERIC_COMP5")
            value, pic = self.NumericBinaryToString()
        elif self.AttrType == COB_TYPE_NUMERIC_PACKED:
            if noisy:
                print("COB_TYPE_NUMERIC_PACKED")
            value, pic = self.PackedDecimalToString()
        elif self.AttrType == COB_TYPE_NUMERIC_FLOAT:
            if noisy:
                print("COB_TYPE_NUMERIC_FLOAT")
            value, pic = self.IeeeBinaryToString()
        elif self.AttrType == COB_TYPE_NUMERIC_DOUBLE:
            if noisy:
                print("COB_TYPE_NUMERIC_DOUBLE")
            value, pic = self.IeeeBinaryToString()
        elif self.AttrType == COB_TYPE_NUMERIC_FP_DEC64:
            if noisy:
                print("COB_TYPE_NUMERIC_FP_DEC64")
            value, pic = self.GnuCobolDecimal64ToString()
        elif self.AttrType == COB_TYPE_NUMERIC_FP_DEC128:
            if noisy:
                print("COB_TYPE_NUMERIC_FP_DEC128")
            value, pic = self.GnuCobolDecimal128ToString()
        elif self.AttrType == COB_TYPE_NUMERIC_FP_BIN32:
            if noisy:
                print("COB_TYPE_NUMERIC_FP_BIN32")
            value, pic = self.IeeeBinaryToString()
        elif self.AttrType == COB_TYPE_NUMERIC_FP_BIN64:
            if noisy:
                print("COB_TYPE_NUMERIC_FP_BIN64")
            value, pic = self.IeeeBinaryToString()
        elif self.AttrType == COB_TYPE_NUMERIC_FP_BIN128:
            if noisy:
                print("COB_TYPE_NUMERIC_FP_BIN128")
            value, pic = self.IeeeBinaryToString()
        elif self.AttrType == COB_TYPE_ALPHANUMERIC:
            if noisy:
                print("COB_TYPE_ALPHANUMERIC")
            value, pic = self.AlphanumericToString()
        elif self.AttrType == COB_TYPE_NUMERIC_EDITED:
            if noisy:
                print("COB_TYPE_NUMERIC_EDITED")
            value, pic = self.NumericEditedToString()
        elif self.AttrType == COB_TYPE_GROUP:
            if noisy:
                print("COB_TYPE_GROUP")
            value, pic = self.AlphanumericToString()
        else:
            # We were handed an unknown type.  Let's look at the data:
            if noisy:
                print("Unknown COB_TYPE", self.AttrType, hex(self.AttrType))
            non_printable = 0
            data = self.Data()
            for _i, d in enumerate(data):
                if d < 32:
                    non_printable += 1

            if non_printable >= 1:
                value = "["
                for byte in data:
                    hexout = hex(byte)[-2:]
                    if hexout[0:1] == 'x':
                        hexout = '0' + hexout[-1:]
                    value += hexout
                value += "]"
            else:
                value = '"'
                for byte in data:
                    if byte >= 32:   # valid extended ASCII characters
                        value += chr(byte)
                    else:
                        value += '^'
                value += '"'

        pcount = 0
        for ch in Picture(self.Picture):
            if ch == 'P':
                pcount += 1
        pcount = 0

        if pcount > 0:
            if self.AttrScale < 0:
                for _i in range(pcount):
                    value += '0'
            else:
                stash = ''
                if value[0] == '+' or value[0] == '-':
                    stash = value[0]
                    value = value[1:]
                for _i in range(pcount):
                    value = '0' + value
                value = stash + self.decimal_point + value

        return value

    # special method to "fake" a constant - out of the common tree
    def set_constant_attributes (self, name, value):
        self.isConstant = True
        self.constantValue = value
        self.Name = name

    # method that GDB would call to get the value, for example in the backtrace and "frame" command
    def value(self):
        # can return a gdb.Value...
        if self.isConstant:
            return self.constantValue

        # ... or a python value (TODO in general, this code is not executed yet)
        # TODO: for numeric and pointer COBOL types return the value as appropriate python type
        #     or, possibly better as a self-created gdb.Value
        if self.FetchVariableData(True):
            return CPrintWorker().GetDisplayBody(self)
        return None

    # method that GDB would call to get the name
    def symbol(self):
        return self.Name


class Rside():
    """Universal value extractor"""
    def __init__(self):
        self.original_string = ""
        self.is_symbol = False
        self.is_negative = False
        self.left_of_decimal = ""   # Note that these are strings, and not integers
        self.right_of_decimal = ""
        self.exponent = 0           # Note that this is an integer, and not a string
        self.has_all = False
        self.is_address_of = False
        self.is_length_of = False

    def __str__(self):
        retval = ""
        retval += "original_string     : {0}\n".format('"' + self.original_string + '"')
        retval += "is_symbol           : {0}\n".format(self.is_symbol)
        retval += "is_negative         : {0}\n".format(self.is_negative)
        retval += "left_of_decimal     : {0}\n".format(self.left_of_decimal)
        retval += "right_of_decimal    : {0}\n".format(self.right_of_decimal)
        retval += "exponent            : {0}\n".format(self.exponent)
        retval += "has_all             : {0}\n".format(self.has_all)
        retval += "is_address_of       : {0}\n".format(self.is_address_of)
        retval += "is_length_of        : {0}\n".format(self.is_length_of)
        return retval

    def FromString(self, rside):
        self.__init__()
        quoted = False
        if rside:
            # Accepts rside input.  These are *not* expressions; these are
            # "atomic" values, with the exception of the ALL modifier
            rside = rside.strip()

            if re.match(R_ALL, rside):
                self.has_all = True
                rside = rside[3:].strip()

            if len(rside) > 0 and rside[0] == '\'':
                # An apostrophe means that the text is believed to be a symbol;
                # we see this happen when using TAB-completion.  And a user might have
                # to disambiguate a symbol like '0XAB' or '1E00', which can be either
                # constants or symbols unless put into single quotes.
                self.original_string = rside
                self.is_symbol = True
                return
            if len(rside) > 0 and rside[0] == '\"':
                # A double-quote has to start a string ending with a double-quote
                if len(rside) == 1:
                    # The string has only one character
                    ConditionalRaise("Unterminated string")
                    return
                if rside[-1] != '\"':
                    # The last character needs to be another double-quote
                    ConditionalRaise("Unterminated string")
                    return
                # We have "something".
                # Clean off the bracketing double-quotes:
                quoted = True
                rside = rside[1:-1]
                #
                # We are now going to fall through.  This is because in
                # COBOL-land, it is possible to assign a string to, say, an integer
                # variable.  The conversion i="123" is the same as i=123
            self.original_string = rside
            #
            # Just for completeness, it must be observed that COBOL naming allows for
            # some unresolvable ambiguities at this point.  For example, it's perfectly
            # possible, although (in my opinion) quite stupid, to have a variable named
            # "1e2".  This means that the statement
            #
            #           cprint a = 1e2
            #
            # has two possible meanings.  The user can disambiguate:
            #
            #           a=1.e2
            #           a='1e2'
            #
            # are unambiguous, the second being GDB-speak for a symbol
            #
            # In my opinion, interpreting 1e2 as a floating-point number is more useful, so
            # that's what I am going to do here.  My reasoning: although "1e2 is a legitimate
            # COBOL name, it would be unusual.  "00-SOMETHING" and 00SOMETHING are more common.
            #
            # If a user does use "1e2", then they'll have to live with the consequences and
            # disambiguate with apostrophes.
            #
            # That concept is also true for 0x<hex digits> and 0b<binary>: The numerical
            # interpretation supercedes the symbolic one
            #
            # Hence the extended and rather convoluted logic at this point
            #

            # We now contend with COBOL's figurative constants: ALL ZERO BLANK

            if re.search(R_SPACE, rside):
                self.original_string = " "
                return

            if re.search(R_ZERO, rside):
                self.original_string = "0"
                self.left_of_decimal = "0"
                return

            # We are now dealing with the possibility of a decimal number.  It might or might
            # not be negative, or have a decimal fraction, or a floating-point exponent.
            #
            # This is a job for regular expressions!
            #
            # It might lead off with '+' '-'
            # followed by zero or more digits
            # followed by zero or one decimal points
            # followed by zero or more digits
            # followed by one of "" "e" "e+" or "e-"
            # That's followed by zero or more exponent digits
            #
            m = re.search(R_NUMERICALCONSTANT, rside)
            if m:
                # print("We have a match!")
                # print("   Prefix:  ",m.group(1))
                # print("   LDigits: ",m.group(2))
                # print("   RDigits: ",m.group(3))
                # print("   EFlag:   ",m.group(4))
                # print("   EDigits: ",m.group(5))

                if m.group(1) == '-':
                    self.is_negative = True
                self.left_of_decimal = m.group(2)
                self.right_of_decimal = m.group(3)
                if m.group(5):
                    self.exponent = int(m.group(5))
                if m.group(4) and len(m.group(4)) > 1 and m.group(4)[1] == '-':
                    self.exponent = -self.exponent

                if m.group(5):
                    # He specified an exponent
                    if int(self.left_of_decimal or "0") == 0 and int(self.right_of_decimal or "0") == 0:
                        # it was of the form "e6"
                        # We know what he wants, so let's give it to him:
                        self.left_of_decimal = '1'
                return

            # Arriving here means it doesn't start with an apostrophe, might
            # be surrounded by double-quotes, isn't hex or binary,
            # fails the R_NUMERICALCONSTANT regular expression, and doesn't have has_all

            if not quoted and not self.has_all:
                self.original_string = rside
                self.is_symbol = True
        return

class ModuleInformation():
    """Information about the source code module being debugged.

    Much of the information here is extracted from the VARIABLE_STRING global
    variable that was compiled into the .c module generated by GnuCOBOL.
    """
    def __init__(self):
        # These variables contain information extracted from the VARIABLE_STRING_xxx
        self.var_trie = VarTrie()
        self.line_list = LineList()
        self.perform_commands = {}

        # These three dictionaries stash those elements, indexed by
        # VARIABLE_STRING_xxx.  For small programs running locally, the time
        # saved by doing this is not relevant.  But when debugging large programs
        # remotely, the time spent reading the lengthy VARIABLE_STRING_xxx and
        # then parsing it can grow significant and annoying when moving back and
        # forth between large modules.
        #
        # A test program with 30,000 variables produced this debugging output:
        #
        #  The time to get_memory_view of VARIABLE_STRING is: 0.0022429270000000057
        #  The time to convert the string tobytes:            0.002386868000000042
        #  The time to parse 1778932 bytes of VARIABLE_STRING_vtest2Ecbl was: 1.07074325
        #
        # by stashing the results of the parsing in these maps, we avoid unnecessary
        # processing.  At the time of this writing, the costs in memory are unknown
        self.stashed_var_trie  = {}
        self.stashed_line_list = {}
        self.stashed_commands  = {}

    @staticmethod
    def ReadAndParseVariableString(string_name):
        try:
            the_address = GetPointerTo(string_name)
        except:   # TODO: use appropriate exception
            the_address = None

        if the_address:
            try:
                if string_name in GV_ModuleInformation.stashed_var_trie:
                    # We have seen and parsed this string before
                    #print("RELOAD", string_name)
                    GV_ModuleInformation.var_trie         = GV_ModuleInformation.stashed_var_trie[string_name]
                    GV_ModuleInformation.line_list        = GV_ModuleInformation.stashed_line_list[string_name]
                    GV_ModuleInformation.stashed_commands = GV_ModuleInformation.stashed_commands[string_name]
                else:
                    # We haven't seen these before, so we need to get the
                    # VARIABLE_STRING and parse it:
                    len_val = gdb.parse_and_eval("*(char (*)[{0}]) {1}".format(VARSTR_LEN_SIZE, the_address))
                    total_length = int(len_val.string())

                    #time1 = time.process_time()
                    variable_string_memview = get_memoryview(the_address + VARSTR_LEN_SIZE, total_length - VARSTR_LEN_SIZE)
                    #time2 = time.process_time()
                    #print("The time to get_memory_view of VARIABLE_STRING is: {0}".format(time2-time1))

                    #time1 = time.process_time()
                    variable_string = variable_string_memview.tobytes().decode('ascii')  # plain string generate by cobcd
                    #time2 = time.process_time()
                    #print("The time to convert the string tobytes:            {0}".format(time2-time1))

                    #time1 = time.process_time()
                    GV_ModuleInformation.ParseVariableString(variable_string)
                    #time2 = time.process_time()
                    #print("The time to parse {0} bytes of {1} was: {2}".format(total_length, string_name, time2-time1))

                    GV_ModuleInformation.stashed_var_trie[string_name]  = GV_ModuleInformation.var_trie
                    GV_ModuleInformation.stashed_line_list[string_name] = GV_ModuleInformation.line_list
                    GV_ModuleInformation.stashed_commands[string_name]  = GV_ModuleInformation.stashed_commands
                return True
            except Exception:
                traceback.print_exc()
        return False

    def ParseVariableString(self, variable_string):

        # We have a new variable string
        # Clear out the tables:
        self.var_trie = VarTrie()
        self.line_list = LineList()
        self.perform_commands = {}

        # ensure GDB can resolve dynamic field references
        if GV_GlobalVariables.gdb_version < 80000 or GV_GlobalVariables.gdb_version > 80200:
            # With RedHat 7 and GDB-8.2, this next statement throws an error
            gdb.execute("macro define COB_SET_DATA(x, z) (x.data = z, &x)", True, False)

        # Load the new tables
        # Pull apart the variable_string
        variable_lines = variable_string.split('~')
        for variable_line in variable_lines:
            tokens = variable_line.split('|')
            self.line_list.LoadFromLine(tokens)
            self.var_trie.LoadTrieFromLine(tokens)
            self.LoadPerformFromLine(tokens)

        self.var_trie.PopulateIndexOfParent()

        # Put a fencepost at the end of the line_list
        self.line_list.Insert(self.line_list.EndOfTheWorld, "Ragnarok")

    def LoadPerformFromLine(self, tokens):
        if len(tokens) > 0 and tokens[0] == "p":
            if len(tokens) > 3:
                self.perform_commands[tokens[3]] = tokens[2]

    def FlagAllAsDirty(self):
        self.var_trie.FlagAllAsDirty()

DISPLAY_SUBMODE_NONE = None
DISPLAY_SUBMODE_BINARY = 'b'
DISPLAY_SUBMODE_HEX = 'x'
DISPLAY_SUBMODE_Q = 'q'


class ModuleState():
    """State of the inferior as it is debugged"""
    def __init__(self):
        # These variables contain static information that changes depending on the user request
        self.question_mark_range = int(os.environ.get("CPRINT_R", '6'))
        self.sticky_display_mode = '0'
        self.current_display_mode = '0'
        self.current_display_submode = DISPLAY_SUBMODE_NONE
        self.sticky_pretty_mode = ''
        self.cstart_entry_name = None

    def reset(self):
        # These variables contain state information that changes as the inferior
        # is debugged.
        self.old_dogs = set()  # One level of retained former variables when doing 'cprint ?'
        self.current_trapped_routine = ""
        self.current_trapped_file = ""
        self.current_trapped_frame = -1
        self.current_trapped_line = 0
        self.current_full_path = ""
        self.Stack = False
        self.current_pretty_mode = ''

    def ProcessStack(self):
        """ Figures out which routine we are currently trapped in"""

        # That's seems innocent enough.  But we're dealing with a multitude of sins.

        # In the simplest incarnation, we have a simple program with a simple trap, in which
        # case the results of the "frame" and "info stack" commands look like this:

        #    (gdb) frame
        #    #0  MAIN_ (entry=0) at MAIN.cbl:11

        #    (gdb) info stack
        #    #0  MAIN_ (entry=0) at MAIN.cbl:11
        #    #1  0x0000555555554cc9 in MAIN () at MAIN.cbl:8
        #    #2  0x0000555555554cb4 in main (argc=1, argv=0x7fffffffe368) at MAIN.cbl:8

        # But we can trap through copybooks.  That can look like this:

        #    (gdb) frame
        #    #0  C_ (entry=0, b_11=0x7ffff35172f0 <b_8> "B   4") at INDENTER.cpy:2

        #    (gdb) info stack
        #    #0  C_ (entry=0, b_11=0x7ffff35172f0 <b_8> "B   4") at INDENTER.cpy:2
        #    #1  0x00007ffff3112ddf in C (b_11=0x7ffff35172f0 <b_8> "B   4") at C.cbl:10
        #    #2  0x00007ffff331622d in B_ (entry=0, b_11=0x555555756130 <b_8> "MAIN2") at B.cbl:18
        #    #3  0x00007ffff3315e3f in B (b_11=0x555555756130 <b_8> "MAIN2") at B.cbl:10
        #    #4  0x0000555555554fa3 in MAIN_ (entry=0) at MAIN.cbl:11
        #    #5  0x0000555555554cc9 in MAIN () at MAIN.cbl:8
        #    #6  0x0000555555554cb4 in main (argc=1, argv=0x7fffffffe368) at MAIN.cbl:8

        # Our problem with that is there is no VARIABLE_STRING_INDENTER_CPY; we have to look down
        # through the stack until we find VARIABLE_STRING_C_CBL in order to have a hope of being
        # able to show the right variables.

        # The third case is when the user wants to look at variables in a different frame.  As
        # an example, suppose the user trapped at INDENTER.cpy:2 issues a "frame 3" command.  Here's
        # what we see:

        #    (gdb) frame
        #    #3  0x00007ffff3315e3f in B (b_11=0x555555756130 <b_8> "MAIN2") at B.cbl:10

        #    (gdb) info stack
        #    #0  C_ (entry=0, b_11=0x7ffff35172f0 <b_8> "B   4") at INDENTER.cpy:2
        #    #1  0x00007ffff3112ddf in C (b_11=0x7ffff35172f0 <b_8> "B   4") at C.cbl:10
        #    #2  0x00007ffff331622d in B_ (entry=0, b_11=0x555555756130 <b_8> "MAIN2") at B.cbl:18
        # --> #3  0x00007ffff3315e3f in B (b_11=0x555555756130 <b_8> "MAIN2") at B.cbl:10
        #    #4  0x0000555555554fa3 in MAIN_ (entry=0) at MAIN.cbl:11
        #    #5  0x0000555555554cc9 in MAIN () at MAIN.cbl:8
        #    #6  0x0000555555554cb4 in main (argc=1, argv=0x7fffffffe368) at MAIN.cbl:8

        # The algorithm that handles those cases seems to be this:
        #   * Pull the file.name from the "frame" results.
        #   * Walk through "info stack" results until the first instance of file.name is found;
        #     that becomes current_trapped_file and current_trapped_line
        #   * Starting at that "info stack" line, walk downward until a valid VARIABLE_STRING
        #     is found.
        # That might give a bogus result.  For example, if the frame is set to a module that
        # wasn't compiled with debugging, then the walk will take you to a module that does
        # have debugging.  That's not what you want, but I don't see how to avoid the problem
        # if you also want to be able to handle code in COPYBOOKs.

        try:
            # When working with RHEL8/GDB-8.2, test004 of CWATCH D caused a
            # trap inside libcob, which happens not to have debugging information.
            # This code is an empirical attempt at getting past that.

            # It's possible for us to trap in a frame without debugging information.
            # If necessary, walk up the stack looking for a frame with debugging information
            current_frame = gdb.selected_frame()
            while True:
                st = current_frame.find_sal().symtab
                if st:
                    # We have found a frame with debugging information
                    break
                current_frame = current_frame.older()
                current_frame.select()

            # Get the frame number and filename from current frame:
            frame_from_frame = CFrameWorker.get_frame_level(current_frame)
            st = current_frame.find_sal().symtab
            if st is None:
                # current frame has no debugging info - we can't do anything here
                return False

            filename_frame = st.filename
            filename_frame_full = st.fullname()

            # We now march through stack, looking for the
            # first entry whose filename matches filename:

            # TODO: wouldn't it make more sense to use the current frame or one above for the start?
            # frame = current_frame
            frame = gdb.newest_frame()
            while frame:
                st = frame.find_sal().symtab
                if st is None:
                    # don't screw parsing with missing debug info, just skip to next frame
                    frame = frame.older()
                    continue
                if st.filename == filename_frame:
                    break
                frame = frame.older()

            if frame is None:
                print("Bad things are happening.  Save this program for Dubner, and tell him")
                print("that \"frame\" and \"info stack\" don't match in ProcessStack()")
                return False

            self.current_full_path = filename_frame_full
            frame_from_stack = CFrameWorker.get_frame_level(frame)
            # Unexpected and confusing things happen when the frame is set
            # to a PROCEDURE DIVISION line (outer COBOL frame).
            # So, if an adjustment is necessary, we make it here:
            if frame != current_frame:
                frame.select()

            # TODO: CFrameWorker.Go(0, False, True) = "cdown-silently 0" seems more robust than the above code

            self.current_trapped_line = frame.find_sal().line

            if filename_frame != self.current_trapped_file or frame_from_frame != frame_from_stack:
                # We are in a new file, or perhaps a new frame
                self.current_trapped_file = filename_frame
                self.current_trapped_frame = frame_from_stack

                # clear out the persistent 'p ?' memory; it has suddenly
                # become obsolete
                self.old_dogs = set()

                # Because it's possible that we are trapped in a copybook,
                # we can't assume that VARIABLE_STRING has the name
                # of the current_trapped_file.  We need to work our
                # way through the stack:

                # Find the address of the VARIABLE_STRING for this trap point

                # Note: it definitely makes more sense to use the now selected frame for the start...
                # frame = gdb.newest_frame()   --> no adjustment means we still have the current one
                while frame:
                    st = frame.find_sal().symtab
                    if st is None:
                        # don't screw parsing with missing debug info, just skip to next frame
                        continue
                    possible_file = st.filename
                    name_root = possible_file.split('/')[-1]

                    name_suffix = ""
                    for ch in name_root:
                        if ch.isalnum():
                            name_suffix += ch
                        else:
                            hh = hex(ord(ch))[-2:]
                            name_suffix += hh.upper()

                    # check if a variable with this string name can be read:
                    if GV_ModuleInformation.ReadAndParseVariableString("VARIABLE_STRING_" + name_suffix):
                        break
                    # otherwise go on with next frame
                    frame = frame.older()

                if frame is None:
                    # For whatever reasons, we couldn't find any information in
                    # the stack above the "frame" point
                    return False

        except:   # TODO: use appropriate exception
            print("ProcessStack exception")
            traceback.print_exc()
            self.current_trapped_routine = ""
            self.current_trapped_file = ""
            self.current_trapped_frame = -1
            self.current_trapped_line = 0
            return False

        return True

    def EstablishModuleState(self, announce):
        # sanity check first
        try:
            gdb.selected_frame()
        except gdb.error as ex:
            # Depending on the scenario commonly raises one of "No stack.", "No frame currently selected." or others
            ConditionalRaise(str(ex))
            return

        # Go read the frame and stack, and, if necessary, fetch the VARIABLE_STRING
        try:
            self.Stack = self.ProcessStack()
        except gdb.GdbError:
            if announce:
                # pass user errors unchanged
                raise
            return

        if self.Stack:
            # Figure out what routine we are trapped in, from
            # the line number
            for line in GV_ModuleInformation.line_list.line_list:
                if line[0] > self.current_trapped_line:
                    # We've gotten past self.current_trapped_line, so the prior entry
                    # was the one we wanted
                    break
                # Keep looking
                self.current_trapped_routine = line[1]
        # Simon: looks wrong
        # else:
        #     if announce:
        #         ConditionalRaise("No stack.")


class HNode():
    def __init__(self):
        self.storage_table_index = -1
        self.Level = -666   # The Node of the Beast
        self.BaseName = ""  # Just A of A/B/C
        self.display_body = ""
        self.parent = None
        self.children = []


def TrimComma(t_str):
    if t_str and t_str[-1] == ',':
        t_str = t_str[:-1]  # Strip off final comma
    return t_str


def BuildMachineHString(node):
    h_str = ""
    if node.Level == -666:
        # This is the root.  We live only for our children
        for child in node.children:
            h_str += BuildMachineHString(child)
    else:
        if node.Level in (1, 77, 78):
            # Top level variables are treated this way:
            h_str += r'{name="'
            h_str += node.BaseName  # + "/" + str(node.storage_table_index)
            h_str += r'",value="'
            if len(node.children) == 0:
                # There are no children, which means this is just
                # a simple variable:
                h_str += r'\"'
                h_str += MachineInterfaceBody(node.display_body)
                h_str += r'\"'
                h_str += r'"'
            else:
                # This is a top-level node with children:
                h_str += r'{_groupvalue=\"'
                h_str += MachineInterfaceBody(node.display_body)
                h_str += r'\",'
                for child in node.children:
                    h_str += BuildMachineHString(child)
                h_str = TrimComma(h_str)
                h_str += r'}"'
            h_str += '},'
        else:
            # This is a node whose level is 0, 02-49, 66
            if len(node.children) == 0:
                h_str += node.BaseName  # + "/" + str(node.storage_table_index)
                h_str += r'=\"'
                h_str += MachineInterfaceBody(node.display_body)
                h_str += r'\",'
            else:
                h_str += node.BaseName  # + "/" + str(node.storage_table_index)
                h_str += r'={_groupvalue=\"'
                h_str += MachineInterfaceBody(node.display_body)
                h_str += r'\",'
                for child in node.children:
                    h_str += BuildMachineHString(child)
                h_str = TrimComma(h_str)
                h_str += r'},'
    return h_str


def BuildHumanHString(node, level):
    h_str = ""
    if node.Level == -666:
        # This is the root.  We live only for our children
        for child in node.children:
            h_str += BuildHumanHString(child, 0)
    else:
        # Prefix with some number of spaces per level:
        for _i in range(level):
            h_str += "  "
        if GV_ModuleState.sticky_display_mode in ('0', '1', '2', '3', '4', '5'):
            # We are in a mode that wants the level displayed:
            h_str += "{0:02} ".format(node.Level)

        name = node.BaseName
        if GV_ModuleState.sticky_display_mode in ('3', '4', '5'):
            pass
        else:
            # This mode doesn't want the program name, if it is there

            # There might be a "(xx)" suffix, indicating OCCURS.  Make sure
            # we keep it:
            paren_args = name.split('(')
            name = paren_args[0]
            nfound = name.find('/')
            if nfound > -1:
                name = name[0:nfound]
            if len(paren_args) > 1:
                paren_args[0] = name
                name = '('.join(paren_args)
        h_str += name

        if len(node.children):
            # We have children, so we don't display anything:
            h_str += '\n'
            for child in node.children:
                h_str += BuildHumanHString(child, level + 1)
        else:
            # We have no children, so we must be elementary, which
            # means it's time to display something:
            h_str += " : {0}\n".format(node.display_body)
    if not h_str:
        return '\n'  # shouldn't we return an error message or raise here?
    return h_str


def BuildHierarchicalTree(possibilities, is_machine_mode):
    # We will use a stack to build a tree from the ordered variables
    root = HNode()
    stack = [root]
    stack_map = {}

    for payload_index in possibilities:
        payload = GV_ModuleInformation.var_trie.storage_list[payload_index]
        if payload.Level == 66:
            # We simply cannot, at this time, handle LEVEL 66.  The variable string
            # doesn't have enough information because the COBC compiler didn't pass
            # along enough information.
            continue
        parent_index = payload.index_of_parent
        # payload_index and parent_index are indices into storage_list
        payload.FetchVariableData(False)
        if not payload.not_in_context:
            # Create the new node
            new_node = HNode()
            new_node.storage_table_index = payload_index
            new_node.Level = payload.Level
            split_name = payload.Name.split('/')
            if len(split_name) == 2:
                # If it's just the two pieces, then use the whole name/program
                # pair.  (This is a file record or 01/77, and we might need
                # the whole name to accurately handle a "print var=x request")
                new_node.BaseName = payload.Name
            else:
                # Just use the lowest level.  When necessary, the VSC code will
                # be able to walk up through the containers to get the full name
                new_node.BaseName = split_name[0]

            # When an element is a table, indicate that
            if payload.OccursMin != payload.OccursMax and not is_machine_mode:
                # When the VSC extension is changed to permit parentheses,
                # get rid of the "not is_machine_mode"
                new_node.BaseName += "(1)"

            # Flag that a variable is a redefines
            if payload.IsRedefines and not is_machine_mode:
                # When the VSC extension is changed to permit brackets,
                # get rid of the "not is_machine_mode"
                new_node.BaseName += " [R]"

            new_node.display_body = WithRepeatCount(payload.display_body)

            if parent_index in stack_map:
                # Our parent is in the stack map; make the association:
                new_node.parent = stack[stack_map[parent_index]]

            # Put our index into the stack map, in case we are somebody's parent:
            stack_map[payload_index] = len(stack)

            # Put this new node onto the stack of nodes:
            stack.append(new_node)

            if new_node.parent:
                # Let our parent know they have another child
                new_node.parent.children.append(new_node)
            else:
                # Nodes with no parent become wards of the state:
                root.children.append(new_node)
    return root


def MachineInterfaceHierarchical(possibilities):
    # This is the machine interface mode for returning the
    # the variables in a flat manner.
    # Our mission is to emulate
    # '-stack-list-variables --all-values'
    # e.g.,
    # variables=[{name="L00",value="\"Standalone\""},{name="L01",value="{fullname = \"RobertDubner\", L02 = {firstname = \"Robert\", lastname = \"Dubner\"}}"},{name="L01_2",value="{fullname = \"JudyRuderman\", L02 = {firstname = \"Judy\", lastname = \"Ruderman\"}}"}]

    root = BuildHierarchicalTree(possibilities, True)

    # We now have a tree that can be used to create the variables string
    retval = "variables=["

    retval += BuildMachineHString(root)

    retval = TrimComma(retval)
    retval += ']'
    retval = NoNulls(retval)
    # retval = r'variables=[{name="L01",value="{_groupvalue=\"Snap!\",fullname=\"RobertDubner\",L02={_groupvalue=\"Crackle!\",firstname = \"Robert\", lastname = \"Dubner\",L03={_groupvalue=\"Pop!\",firstname = \"Judy\", lastname = \"Ruderman\"}}}"}]'
    return retval


def get_sorted_possibilities(args, limit, no_data=False):
    # We take a list of strings as args
    possibilities_1 = []
    possibilities = []
    prefix = None

    # It's possible that the user is asking for LENGTH OF or ADDRESS OF

    # This is one of the places we handle that:
    if len(args) > 0 and args[0].lower() in ("length", "address" ):
        prefix = args[0].lower()
        args = args[1:]

    if len(args) >= 1 and GV_ModuleState.current_trapped_line > 0:
        if args[0] == '*':
            # If the first, presumably single, argument is an asterisk, then
            # he is asking for all variables:
            possibilities_1 = GV_ModuleInformation.var_trie.GetAllPossibilities()
        elif args[0].find('*') != -1:
            # If the first, presumably single, argument contains an asterisk, then
            # he is asking for a list of variables that match the pattern:
            possibilities_1 = GV_ModuleInformation.var_trie.GetMatchingPossibilities(args[0])
        elif args[0].find('?') != -1:
            if not COBCDRW_AVAILABLE:
                print("Function not available: cobcd-rw not available")
                return possibilities, False

            # If the first, presumably single, argument contains a question mark,
            # then we need to round up the usual suspects.

            # But first, we are going to keep a rogue's gallery of the current list
            # suspects.  The idea here is that sometimes MOVE A TO B is the last line
            # of a PARAGRAPH, so as soon as it is executed control jumps elsewhere and
            # B falls out of context.  Or perhaps the next GDB command is a CONTINUE,
            # rather than NEXT.  By keeping one level of stickiness, the display of '?' variables
            # will be a bit more like what the user probably expects.
            if len(GV_ModuleState.current_full_path) > 0:
                if len(args) > 1 and IsInteger(args[1]):
                    # an explicit, one-time range has been requested
                    qmrange = int(args[1])
                    # Clear out the old_dogs, because the parameter was
                    # specific:
                    GV_ModuleState.old_dogs = set()
                else:
                    # without that request, use the established +/- range
                    qmrange = GV_ModuleState.question_mark_range
                if qmrange == -1:
                    qmrange = 1000000000  # equivalent to "all"
                if qmrange <= -2:
                    # -2 means there should be no "cprint ?" response
                    return_string = ""
                else:
                    return_string = subprocess.check_output([COBCDRW_BINARY,
                                                             GV_ModuleState.current_full_path,
                                                             str(GV_ModuleState.current_trapped_line),
                                                             str(qmrange)])
                tokens = return_string.split()
                new_dogs = set()
                for token in tokens:
                    token = token.decode("utf-8")
                    new_dogs = new_dogs.union(set(GV_ModuleInformation.var_trie.GetListOfExactPossibilities(token)))
                possibilities_1 = list(GV_ModuleState.old_dogs.union(new_dogs))
                GV_ModuleState.old_dogs = new_dogs
        elif args[0][0] == '#':
            # if the first, presumably single, argument after # is a number, then we are looking
            # for a short-term memory entry.

            # Pick up the 1-based value indexed by the argument:
            try:
                index = int(args[0][1:]) - 1
            except ValueError:
                ConditionalRaise("A stored variable index must be in format #nnn but is {0}".format(args[0]))
                return possibilities, False  # False means there was no 'limit' overflow
            maxIndex = len(GV_GlobalVariables.ShortTermMemory)
            if maxIndex == 0:
                ConditionalRaise("There are no stored COBOL variables")
                return possibilities, False  # False means there was no 'limit' overflow
            if 0 <= index < maxIndex:
                # If index is valid, pick up the short-term memory entry
                possibilities_1 = [GV_GlobalVariables.ShortTermMemory[index]]
            else:
                ConditionalRaise("The specified stored variable index ({0}) is out of range (1 to {1})".format(index + 1, maxIndex))
                return possibilities, False  # False means there was no 'limit' overflow
        else:
            # There are one or more arguments.  Use our clever routine
            # to come up with the possibilities_1:
            possibilities_1 = GV_ModuleInformation.var_trie.GetListOfPossibilities(args)
            # If, and only if, there is exactly one result, then modify the
            # that single result
            if len(possibilities_1) == 1:
                if prefix and prefix == "address":
                    possibilities_1[0] = -(possibilities_1[0] + LENADDR_FLAG_BIAS*LENADDR_FLAG_ADDRESS)
                elif prefix and prefix == "length":
                    possibilities_1[0] = -(possibilities_1[0] + LENADDR_FLAG_BIAS*LENADDR_FLAG_LENGTH)

    if no_data and not limit:
        return possibilities_1, False  # False means there was no 'limit' overflow

    # We now have a list of possibilities_1 based on the names of the variables
    # typically, some of them will not be in context.
    target_limit = 0
    for payload_index in possibilities_1:
        if no_data:
            add_to_list = True
        else:
            # The only way of knowing which variables are in context
            # is to try to read them.  If we can't read them,
            # then we don't add them to our list:
            payload = GV_ModuleInformation.var_trie.storage_list[abs(payload_index)%LENADDR_FLAG_BIAS]
            payload.FetchVariableData(False)
            add_to_list = not payload.not_in_context
        if add_to_list:
            possibilities.append(payload_index)
            target_limit = target_limit + 1

            # If there are too many possibilities_1, then return nothing.  limit will usually
            # be zero or one.  Zero means return everything; one means return only the possibilities
            # when there is just one, which is useful in situations like "cprint A = B", which can only
            # work if A and B are unambiguous (we actually return 2 if there are too much to distinguish
            # between "not found" and "too much")

            if 0 < limit < target_limit:
                possibilities = []
                return possibilities, True  # True means there was a 'limit' overflow

    # Sorting them makes the display much more sensible:
    possibilities.sort()

    return possibilities, False  # False means there was no 'limit' overflow


def get_completion_possibilities(word):
    GV_ModuleState.EstablishModuleState(False)
    pending_completions = get_max_completions()
    possibilities = []

    if word == '*':
        possibilities_1, _limit_reached = get_sorted_possibilities(['*'], 0, no_data=True)
        for index in possibilities_1:
            name = GV_ModuleInformation.var_trie.storage_list[abs(index)%LENADDR_FLAG_BIAS].Name.upper()
            possibilities.append(name)
            pending_completions -= 1
            if pending_completions < 0:
                break
    else:
        word = word.upper()
        for index in range(len(GV_ModuleInformation.var_trie.storage_list)):
            name = GV_ModuleInformation.var_trie.storage_list[index].Name.upper()
            if name.startswith(word):
                possibilities.append("'" + name + "'")
                pending_completions -= 1
                if pending_completions < 0:
                    break
    return possibilities


def get_cobol_breakpoints(bp_filter, internal=False):
    possibilities = []
    # for gdb_breakpoint in gdb.breakpoints():
    for gdb_breakpoint in CBLDBG_CBREAK_WORKER.cobol_breaks:
        if gdb_breakpoint.number < 0 and not internal:
            continue
        bp_number = str(gdb_breakpoint.number)
        if bp_filter and not bp_number.startswith(bp_filter):
            continue
        possibilities.append(bp_number)
    return possibilities


def break_completion(text, word, condition):
    try:
        if condition == 1:
            # no input so far: provide list of COBOL breakpoints
            if text == "":
                return get_cobol_breakpoints(None)
            # input at number: provide list of matching COBOL breakpoints
            if text == word and IsInteger(word):
                return get_cobol_breakpoints(word)

        text_split = text.split()
        word_cnt = len(text_split)
        if condition == 0:
            # gdb only completes this with "i" typed, disabled for now
            # if word_cnt == 1:
            #     return ["if"]

            if word_cnt in (3, 4) and word == "":
                return ["=", "!=", "<", "=<", ">", ">=", "<>", "=~"]
        else:
            if word_cnt in (2, 3) and word == "":
                return ["=", "!=", "<", "=<", ">", ">=", "<>", "=~"]

        # note: modifier / slash is no word, so in this case word is None
        if word and (word[0] in ('*', '?', '"', '\'') or word[0].isdigit()):
            # if it doesn't look like a word then just return nothing
            return gdb.COMPLETE_NONE

        if word is None or word == "" or word[0] in ('(', ')', ',', ':'):
            # gdb returns everything, do the same here
            word = '*'

        possibilities = get_completion_possibilities(word)
        return possibilities
    except Exception:
        traceback.print_exc()


def encode_program_id(name, fold_case):
    if not name:
        return None
    return_name = ""
    list_count = 0

    name_list = list(name)
    if name_list[0].isdigit():
        return_name = '_'

    while list_count < len(name_list):
        if not re.search(R_VALID_CHARS, name_list[list_count]):
            return_name = return_name + '_'
            if name_list[list_count] == '-':
                return_name = return_name + '_'
            else:
                return_name = return_name + HEXVAL[int(ord(name_list[list_count]) / 16)]
                return_name = return_name + HEXVAL[int(ord(name_list[list_count]) % 16)]
        else:
            return_name = return_name + name_list[list_count]

        list_count += 1

    if fold_case == COB_FOLD_LOWER:
        return_name = return_name.lower()
    elif fold_case == COB_FOLD_UPPER:
        return_name = return_name.upper()

    return return_name


def get_section_spec(arg):
    module_name = ""
    mod_sec = arg.split(":")
    if len(mod_sec) == 1:
        section_name = encode_program_id(mod_sec[0], 1)
    elif len(mod_sec) == 2:
        section_name = encode_program_id(mod_sec[1], 1)
        if mod_sec[0] != "":
            module_name = encode_program_id(mod_sec[0], 0)

    if not module_name:
        return "SECTION_{0}".format(section_name)

    return "{0}_:SECTION_{1}".format(module_name, section_name)


def FromHexAlphaString(conv_str):
    """Converts a COBOL hexadecimal alphanumeric string to an ordinary string"""
    retval = conv_str
    if conv_str[0:2].lower().replace('"', "'") == "x'" and conv_str[-1] in ("'", '"') and (len(conv_str) % 2) == 1:
        # This is correctly formed
        # Trim off the front and back:
        conv_str = conv_str[2:-1]
        i = 0
        retval = '"'
        while i < len(conv_str):
            try:
                conv_char = int(conv_str[i:i + 2], 16)
            except:   # TODO: use appropriate exception
                break
            retval += chr(conv_char)
            i += 2
        retval += '"'
    return retval


def FromHexLiteralNumber(conv_str):
    """Converts a COBOL hexadecimal numeric literal string to an ordinary string"""
    retval = 0
    if conv_str[0:2].lower().replace('"', "'") == "h'" and conv_str[-1] in ("'", '"'):
        # This is correctly formed
        # Trim off the front and back:
        conv_str = conv_str[2:-1]
        i = 0
        while i < len(conv_str):
            try:
                nch = int(conv_str[i], 16)
            except:   # TODO: use appropriate exception
                break
            retval *= 16
            retval += nch
            i += 1
    return str(retval)


def FromBinaryLiteralNumber(conv_str):
    """Converts a binary literal string to an ordinary string"""
    retval = 0
    if conv_str[0:2].lower().replace('"', "'") == "b'" and conv_str[-1] in ("'", '"'):
        # This is correctly formed
        # Trim off the front and back:
        conv_str = conv_str[2:-1]
        i = 0
        while i < len(conv_str):
            try:
                nch = int(conv_str[i], 2)
            except:   # TODO: use appropriate exception
                break
            retval *= 2
            retval += nch
            i += 1
    return str(retval)


class CPrintWorker():
    """Does most of the non-gdb work of CPrint()"""

    # By creating this worker, we can use a Python debugging package to
    # handle everything that doesn't actually need the gdb object
    def __init__(self):
        self.current_source_file = ""
        GV_ModuleState.sticky_display_mode = os.environ.get("CPRINT_V", '0')
        GV_ModuleState.sticky_pretty_mode = os.environ.get("CPRINT_P", '')
        self.machine_interface_mode = False

        # ensure GDB can resolve dynamic field references in parse_and_eval
        try:
           gdb.execute("macro define COB_SET_DATA(x, z) (x.data = z, &x)", True, False)
        except:
            pass   # failed in some cases (GDB version and/or already defined?) - ignore


    @staticmethod
    def ProcessSlashV(args):
        # We know the first two characters of args[0] are "/v"
        v_code = None
        if len(args[0]) == 3:
            # This is a single-digit display v-code:
            v_code = args[0][2]
            args.pop(0)
        if v_code and v_code in "012345678":
            # It's a valid display_mode.  Make it the one-time mode:
            GV_ModuleState.current_display_mode = v_code
            if len(args) == 0:
                # There are no additional arguments, so also make it the session mode
                GV_ModuleState.sticky_display_mode = v_code
        else:
            ConditionalRaise("The /v code is invalid - it must be /v0 through /v8")
            args = []
        return args

    @staticmethod
    def ProcessSlashR(args):
        # We know the first two characters of args[0] are "/r"
        #
        # We want "/rxxx" to be the same as "/r xxx":
        first_arg = args[0][2:]  # Might be empty

        args.pop(0)
        if first_arg:
            args = [first_arg] + args

        r_code = ""
        if len(args) == 0:
            # '/r' was the only argument, so it is a request to display the range:
            if int(GV_ModuleState.question_mark_range) <= -2:
                print("The 'cprint ?' range is currently suppressed (-2)")
            elif int(GV_ModuleState.question_mark_range) == -1:
                print("The 'cprint ?' range is currently the same as 'cprint *' (-1)")
            else:
                print("The 'cprint ?' range is currently +/-", GV_ModuleState.question_mark_range, "lines")
            args = []
            return args

        if len(args) > 0:
            r_code = args[0]
            args.pop(0)

        if IsInteger(r_code):
            # This is a request to change the question_mark_range.  We will make
            # the change and reset the display by deleting the old_dogs, even
            # if the value hasn't actually changed:
            GV_ModuleState.question_mark_range = int(r_code)
            GV_ModuleState.old_dogs = set()
        else:
            ConditionalRaise("The /r command is malformed")
            args = []

        return args

    def ProcessDisplayMode(self, args):
        args = args.split(' ')

        # Start off with the current display modes the same as the session modes
        self.machine_interface_mode = False
        GV_ModuleState.current_display_mode = GV_ModuleState.sticky_display_mode
        GV_ModuleState.current_display_submode = DISPLAY_SUBMODE_NONE
        GV_ModuleState.current_pretty_mode = GV_ModuleState.sticky_pretty_mode

        # Generalized parsing of /<mode><param> or /<mode> <param>
        mode = None
        if len(args) > 0:
            if len(args[0]) > 0 and args[0][0] == '/':
                # The first character of the first argument is a slash

                # The immediately following character is the mode:
                if len(args[0]) > 1:
                    mode = args[0][1]

                # Note: At the present time, param isn't used; I am keeping this code
                # around because I have it in the back of my head to re-write the
                # parameter processing.
                # We now have a choice.  We allow things like "/r6" and "/r 6"
                # We don't allow "/v 6", so we have to keep that in mind
                # if len(args[0]) > 2:
                #     # Anything after the /<mode><param> is a parameter
                #     param = args[0][2:]
                # elif len(args) > 1:
                #     # We are dealing with /v<mode> <something>
                #     # <something> might be a parameter:
                #     param = args[1]
                # else:
                #     param = None

                if mode in "mvdxbq":
                    # Any of these switches suppress the hierarchical pretty printing
                    GV_ModuleState.current_pretty_mode = ''

                if mode == 'm':
                    self.machine_interface_mode = True
                    args.pop(0)
                elif mode == 'r':
                    args = self.ProcessSlashR(args)
                elif mode == 'v':
                    args = self.ProcessSlashV(args)
                elif mode == 'p':
                    GV_ModuleState.current_pretty_mode = 'p'
                    args.pop(0)
                    if len(args) == 0:
                        # There are no additional arguments, so also make it the session mode
                        GV_ModuleState.sticky_pretty_mode = GV_ModuleState.current_pretty_mode
                elif mode == 'P':
                    GV_ModuleState.current_pretty_mode = ''
                    args.pop(0)
                    if len(args) == 0:
                        # There are no additional arguments, so also make it the session mode
                        GV_ModuleState.sticky_pretty_mode = GV_ModuleState.current_pretty_mode
                elif mode == 'd':
                    GV_ModuleState.current_display_mode = 'd'
                    args.pop(0)
                elif mode in "xbq":
                    GV_ModuleState.current_display_submode = mode
                    args.pop(0)
                elif mode == 'h':
                    global GV_reads
                    global GV_bytes_read
                    print("There were {0} reads of {1} bytes".format(GV_reads,GV_bytes_read))
                    args.pop(0)
                else:
                    ConditionalRaise("Undefined output format \"{0}\"".format(args[0][1:]))
                    args = []

            # else:
            # The first parameter doesn't start with a slash character
            #
            # usually, this means that the first argument is a variable.  However, it has
            # been deemed desirable to handle COBOL's LENGTH OF and ADDRESS OF "registers".
            #
            # We are going to let that through to be handled in a different place.

        if args:
            args = ' '.join(args)
        return args

    def GetDisplayBody(self, payload):
        elements = get_element_size()
        body = ""
        data = payload.Data()[0:elements]
        if payload.Refmod:
            if GV_ModuleState.current_display_submode == DISPLAY_SUBMODE_BINARY:
                body = "0b" + payload.GetDataAsBinary()
            elif not payload.display_body.startswith('"') or GV_ModuleState.current_display_submode == DISPLAY_SUBMODE_HEX:
                # The refmod payload is not alphabetic (or the submode is x for hex) and so we are
                # going to display the refmodded payload in hex:
                body = "[0x"
                for _i, d in enumerate(data):
                    # This is a trick. By adding 256 to the byte, we ensure that
                    # 0, for example, becomes 0x100 instead of 0x0.  Thus we can
                    # strip off the final two characters for our hexadecimal string
                    body += hex(256 + d)[-2:]
                body += ']'
            else:
                # We are dealing with something that libcob thinks is alphabetic, so
                # we are going to extract the refmodded characters from the data buffer
                # and display them as ASCII
                body = '"'
                for _i, d in enumerate(data):
                    nch = d
                    if nch < 32:
                        nch = ord(NON_PRINTABLE_CHAR)
                    body += chr(nch)
                body += '"'
            body = WithRepeatCount(body)
        else:
            # There is no refmod involved:
            if GV_ModuleState.current_display_submode == DISPLAY_SUBMODE_HEX:
                body = payload.GetDataAsHex()
            elif GV_ModuleState.current_display_submode == DISPLAY_SUBMODE_BINARY:
                body = payload.GetDataAsBinary()
            else:
                body = WithRepeatCount(payload.display_body)

        return body

    def FormattedDisplay(self, payload):
        body = self.GetDisplayBody(payload)

        # The following code catches the 'q' Quit option
        retval = None
        try:
            if GV_ModuleState.current_display_mode in ('0', '3', '6'):
                print(payload.NameToString(), " : ", body)
            elif GV_ModuleState.current_display_mode in ('1', '4', '7'):
                print(body)
            elif GV_ModuleState.current_display_mode in ('2', '5', '8'):
                print(payload.NameToString())
                print(body)
            else:  # GV_ModuleState.current_display_mode ==  'd':
                payload.ShowFields()
        except KeyboardInterrupt as ki:
            retval = str(ki)
            # print("Quit")
            print(retval)  # is there a caller that does not like the print?
        except Exception:
            traceback.print_exc()
        return retval

    @staticmethod
    def set_var_value(value):
        # Value is an Rside() object

        # The left-hand-side of the assignment is found in GV_GlobalVariables.VarLeft
        # Its index in storage_list is .VarLeftIndex.  See ProcessArguments

        # value is a list.  It should have only one element, which is a string.
        # That string will be a literal constant, a numerical constant.

        # Just a reminder:  We use .VarLeft to handle the situation
        # where the same table name is on the left and right: "cprint A(2)=A(3).  Because
        # storage_list has only one entry for A, the processing for A(3) overwrites the
        # Subscripts (and Refmod) for A(2).  We get around this by processing the left
        # side first, and making a copy of the object in .VarLeft.  Then we process the
        # right side, and get its printable data string, which appears here as value_

        # This routine deals with the various cases of "cprint <this>=<that>"
        # The ability to cope with refmods complicates things considerably.  Try to keep up.
        var_left = GV_GlobalVariables.VarLeft

        if not var_left.Refmod:
            # Because we are not dealing with a refmod situation, we put the complete
            # data to the inferior
            try:
                # in rare cases we may operate on read-only memory (coredump, rr, ...)
                return_value = var_left.PutDataFromRside(value)
            except gdb.MemoryError as ex:
                # pass on as user error to make it visible without a stacktrace
                ConditionalRaise("MemoryError: " + str(ex))
                return 0

            if return_value == 0:
                return 0
        else:
            # var_left has a refmod.

            # There is a refmod on the left side, so arrange to replace the
            # data on the left

            values = []
            i = 0
            while i < var_left.ActualLength and i < len(value.original_string):
                values.append(ord(value.original_string[i]))
                i += 1

            # If the left wants more than the right provided, right-fill with zeroes or
            # spaces, as specified.
            if var_left.IsAlphanumeric():
                ch = ' '
            else:
                ch = chr(0)
            while i < var_left.ActualLength:
                values.append(ord(ch))
                i += 1

            try:
                # in rare cases we may operate on read-only memory (coredump, rr, ...)
                PutBytesAt(var_left.ActualLocation, values, var_left.ActualLength)
            except gdb.MemoryError as ex:
                # pass on as user error to make it visible without a stacktrace
                ConditionalRaise("MemoryError: " + str(ex))
                return 0

        payload = GV_ModuleInformation.var_trie.storage_list[GV_GlobalVariables.VarLeftIndex]

        # Adjust the modifiers in the storage_list to match var_left, so that subsequent
        # fetch/display shows what we just changed:
        payload.Subscripts = var_left.Subscripts
        payload.Refmod = var_left.Refmod

        # Flag the element in storage_list as IsDirty so that the next fetch reflects the change
        payload.SetIsDirty()

        return 1

    def ConditionalClearSubscripts(self, leftside_possibilities):
        if len(leftside_possibilities) > 1:
            # Because there is more than one possibility, we need to clear
            # out any subscripts and refmods.  If we don't, then we can have situations
            # where doing a CPRINT * after a CPRINT VAR(3) results in an improper display
            for index in leftside_possibilities:
                if GV_ModuleInformation.var_trie.storage_list[index].Subscripts or GV_ModuleInformation.var_trie.storage_list[index].Refmod:
                    GV_ModuleInformation.var_trie.storage_list[index].Subscripts = None
                    GV_ModuleInformation.var_trie.storage_list[index].Refmod = None

    def ProcessArgumentsMachineMode(self, leftside, rightside, leftside_possibilities, rightside_possibilities):
        """We arrive here when processing a cprint/m"""
        self.ConditionalClearSubscripts(leftside_possibilities)
        if len(leftside_possibilities) == 0:
            # just return quietly.  A "variable not found" message
            # confuses Visual Studio Code
            return

        if len(leftside_possibilities) == 1 and not rightside and leftside[0] != '*' and leftside[0] != '?':
            # Very special case.  This was a `cprint/m something`, which is what happens when
            # the -data-evaluate-expression command is issued by Visual Studio Code.  I considered creating
            # a unique 'print/e ' command, but then I realized that VSC never issues a 'cprint/m something' unless there
            # is also an 'rside'.  So, this code is in response to a `-data-evalute-expression` equivalent command
            payload_index = leftside_possibilities[0]
            flag = 0
            if payload_index < 0:
                flag = abs(payload_index)          // LENADDR_FLAG_BIAS
                payload_index = abs(payload_index)  % LENADDR_FLAG_BIAS

            payload = GV_ModuleInformation.var_trie.storage_list[payload_index]
            if flag == 0:
                disp_str = 'value="' + MachineInterfaceBodyAlt(WithRepeatCount(payload.display_body)) + '"'
            elif flag == LENADDR_FLAG_ADDRESS:
                disp_str = 'value="' + MachineInterfaceBodyAlt(hex(payload.ActualLocation)) + '"'
            else: # flag == LENADDR_FLAG_LENGTH:
                disp_str = 'value="' + MachineInterfaceBodyAlt(str(payload.ActualLength)) + '"'

            print(disp_str)
            return

        if len(leftside_possibilities) == 1 and rightside and leftside[0] != '*' and leftside[0] != '?':
            payload_index = leftside_possibilities[0]
            if payload_index < 0:
                return
            payload = GV_ModuleInformation.var_trie.storage_list[payload_index]
            if self.set_var_value(rightside) == 0:
                # The assignment failed
                return

            if payload.FetchVariableData(True):
                disp_str = 'value="' + MachineInterfaceBodyAlt(WithRepeatCount(payload.display_body)) + '"'
                print(disp_str)

            payload.SetIsDirty()

            # Eliminate stale modifiers, if necessary
            if payload.Subscripts or payload.Refmod:
                payload.Subscripts = None
                payload.Refmod = None

            return

        # In machine_interface mode, we just return the possibility(ies).
        # Because we are going to be showing the data in a hierarchical way,
        # we want to be sure every sub-level of a 01 Level variable is visible:
        ppp = set(leftside_possibilities)
        for i in leftside_possibilities:
            tokens = GV_ModuleInformation.var_trie.storage_list[i].Name.split('/')
            var = tokens[-2]
            prog = tokens[-1]
            u = i - 1
            while u >= 0:
                tokens = GV_ModuleInformation.var_trie.storage_list[u].Name.split('/')
                if tokens[-2] != var or tokens[-1] != prog:
                    break
                ppp.add(u)
                u -= 1
            d = i + 1
            while d < len(GV_ModuleInformation.var_trie.storage_list):
                tokens = GV_ModuleInformation.var_trie.storage_list[d].Name.split('/')
                if tokens[-2] != var or tokens[-1] != prog:
                    break
                ppp.add(d)
                d += 1

        leftside_possibilities = list(ppp)
        leftside_possibilities.sort()

        disp_str = MachineInterfaceHierarchical(leftside_possibilities)
        print(disp_str)

    def ProcessArgumentsHumanHierarchical(self, possibilities_1):
        # Because we are doing a hierarchical display, we need to
        # expand possibilities_1 to include all its children

        all_parents = set(possibilities_1)

        possibilities = []

        for index in range(len(GV_ModuleInformation.var_trie.storage_list)):
            if index in all_parents:
                # If we are a founding father, we go into the list
                # Make sure our data are available
                GV_ModuleInformation.var_trie.storage_list[index].FetchVariableData(False)
                possibilities.append(index)
                continue
            if GV_ModuleInformation.var_trie.storage_list[index].index_of_parent in all_parents:
                # We aren't in the list, but our parent is, so we go onto the list
                GV_ModuleInformation.var_trie.storage_list[index].FetchVariableData(False)
                # Make sure our data are available, including AttrType
                possibilities.append(index)
                # And if we have children, we go onto the parents list:
                if GV_ModuleInformation.var_trie.storage_list[index].AttrType == COB_TYPE_GROUP:
                    all_parents.add(index)

        root = BuildHierarchicalTree(possibilities, False)
        result = BuildHumanHString(root, 0)
        OurPrint(result, end='')

    def ProcessArgumentsHumanMode(self, leftside_possibilities, rightside):
        self.ConditionalClearSubscripts(leftside_possibilities)
        # If it happens that there is only one leftside_possibility, and it
        # happens to have a Refmod, then we can't do hierarchical:
        payload_okay = True
        if len(leftside_possibilities) == 1:
            payload = GV_ModuleInformation.var_trie.storage_list[abs(leftside_possibilities[0])%LENADDR_FLAG_BIAS]
            if payload.Refmod:
                payload_okay = False
        if payload_okay and GV_ModuleState.current_pretty_mode == 'p':
            return self.ProcessArgumentsHumanHierarchical(leftside_possibilities)

        if len(leftside_possibilities) > 1:
            GV_GlobalVariables.ShortTermMemory = []

            line_counter = 1
            for payload_index in leftside_possibilities:
                payload = GV_ModuleInformation.var_trie.storage_list[payload_index]
                if not payload.not_in_context:
                    if GV_ModuleState.current_display_mode == 'd':
                        if payload.FetchVariableData(True):
                            self.FormattedDisplay(payload)
                    else:
                        if GV_ModuleState.current_display_submode == DISPLAY_SUBMODE_Q:
                            print_str = payload.NameWithSubscripts()
                        else:
                            GV_GlobalVariables.ShortTermMemory.append(payload_index)
                            print_str = "{0:2} : ".format(line_counter)
                            line_counter += 1
                            print_str += payload.NameToString()
                            print_str += " : "

                            if GV_ModuleState.current_display_submode == DISPLAY_SUBMODE_HEX:
                                body = payload.GetDataAsHex()
                            else:
                                body = WithRepeatCount(payload.display_body)

                            print_str += body
                        retval = OurPrint(NoNulls(print_str))
                        if retval:
                            print(retval)
                            break
        elif len(leftside_possibilities) == 1:
            # There is but one possibility
            # Pick up the payload from the list
            index = leftside_possibilities[0]
            flag  = 0
            if index < 0:
                flag  = abs(index) // LENADDR_FLAG_BIAS
                index = abs(index)  % LENADDR_FLAG_BIAS

            payload = GV_ModuleInformation.var_trie.storage_list[index]

            if rightside:
                if self.set_var_value(rightside) == 0:
                    # The assignment failed
                    return

            # Having done the assignment, if any,
            # now display the left-side data that was changed or requested to be printed:
            if payload.FetchVariableData(True):
                if flag == 0:
                    self.FormattedDisplay(payload)
                elif flag == LENADDR_FLAG_ADDRESS:
                    print(hex(payload.ActualLocation))
                elif flag == LENADDR_FLAG_LENGTH:
                    print(payload.ActualLength)

            if rightside and payload.table_offset:
                # We just did an assignment to a table element, and then we displayed it.
                # This means that the self.grand_data for the parent element is stale, but
                # because we just did FetchVariableData the IsDirty bit is False.  We need
                # it to be True:
                payload.SetIsDirty()

            # And having displayed it, let's reset subscripts and refmods (if necessary) so that
            # subsequent displays don't carry around the stale modifiers
            if payload.Subscripts or payload.Refmod:
                payload.Subscripts = None
                payload.Refmod = None

    def ProcessArguments(self, arguments):
        if not arguments or len(arguments) == 0:
            return

        # Special case:
        # If the final character is '!', we will list all ambiguous names
        if arguments[-1] == '!':
            just_count_ambiguities = False
            arguments = arguments[0:-1]
        else:
            just_count_ambiguities = True

        # Look for /[dxvmvr] switches:
        args = self.ProcessDisplayMode(arguments)

        # At this point, we have args[], which should be
        #    <var>
        # or
        #    <var> = literal
        # or
        #    <var1> = <var2>
        #
        # <var> is a COBOL variable, which can be
        #   A
        #   A of B
        #   A in B
        #   A/B
        #   A.B

        # It is possible for args[] to be empty. The user might have supplied
        # no arguments, or ProcessDisplayMode could have, properly, eaten all
        # of them.  In any case, no arguments means do nothing:

        if not args or not args[0]:
            return

        if args[0][0] == '"':
            # The user is talking to himself for some reason.  Just play along.
            self.HandleDoubleQuote(arguments)
            return
        if IsInteger(args) or args[0][0] == '$':
            # The user is talking to himself for some reason.  Just play along.
            num, _ret = IntegerOrVariable(args)
            if num:
                print (num)
            return

        # Having processed display switches, we can now check to if we are in
        # a frame, and process accordingly:
        GV_ModuleState.EstablishModuleState(True)

        if not GV_ModuleState.Stack:
            # "No stack." is the message GDB reports when debugging statements
            # are issued when the program hasn't been run, which is checked in the
            # caller, we get here for "internal" reasons during resolve.
            ConditionalRaise("This doesn't look like a COBOL frame.")
            return

        # Initialize for subsequent processing:
        leftside_possibilities = []
        rightside_possibilities = []
        left_side = ""
        right_side = ""

        # At this point, the forms
        #   cprint VAR
        #   cprint VAR=literal
        #   cprint VAR=ANOTHER_VAR
        # are allowed.

        # Go split it up on the '=', if there is one, and return
        # the canonicalized strings:
        left_side, right_side = process_left_right_args(args)

        if not left_side:
            return

        if right_side and (GV_ModuleState.current_display_submode or GV_ModuleState.sticky_display_mode != GV_ModuleState.current_display_mode):
            # FIXME: should be just "right_side and any-modifier-given" but that var does not exist
            ConditionalRaise("Invalid arguments, cprint with modifier may not include a right-hand-side.")
            return

        # From this point on, the left_side, and right_side (if there
        # is one), are handled as lists of strings

        # But before going general, We need to handle an obscure situation,
        # but one that can happen.
        #
        # We handle array/table entries by creating a CobolVariable using its base symbol,
        # but also carrying along the Subscripts and the table_offset created by those subscripts.
        #
        # The rare case is when the operator specifies an assignment: "cp table(2) = table(4)", because
        # The second evaluation stomps on the first one. So, we are going to save copies of both of them
        # one at a time, here, so we can use them later, if needed, in PutDataAsVar()

        GV_GlobalVariables.VarLeft = None
        GV_GlobalVariables.VarLeftIndex = None

        # note: if we do have a right-side then we do not need the actual data, maybe add , no_data=True here
        # ... but we do need to know if this variable is in the current context
        leftside_possibilities, limit_reached = get_sorted_possibilities(left_side, 0)
        if not leftside_possibilities:
            # There are no leftside possibilities.  Let him know why, and then bug out
            if left_side[0] == '?' or left_side[0].isdigit():
                # When '*' or '?' match nothing, then just return nothing
                return
            if left_side[0].find('*') == -1:
                ConditionalRaise('No symbol matches "{0}" in current context'.format('/'.join(left_side)))
            else:
                ConditionalRaise('No symbols match "{0}" in current context'.format('/'.join(left_side)))
            return

        if len(leftside_possibilities) > 1 and left_side[0].find('*') == -1 and left_side[0][0] != '?' and just_count_ambiguities:
            ConditionalRaise('Multiple ({0}) symbols match "{1}" in current context'.format(len(leftside_possibilities), '/'.join(left_side)))
            return

        if len(leftside_possibilities) == 1:
            # because there is but a single leftside possibility, we might be in a "cprint var1=var2" situation
            if leftside_possibilities[0]<0 and right_side:
                ConditionalRaise("Invalid assignment")
                return
            GV_GlobalVariables.VarLeft = copy.copy(GV_ModuleInformation.var_trie.storage_list[abs(leftside_possibilities[0])%LENADDR_FLAG_BIAS])
            GV_GlobalVariables.VarLeftIndex = abs(leftside_possibilities[0])%LENADDR_FLAG_BIAS
            if GV_GlobalVariables.VarLeft.SubscriptFailure or GV_GlobalVariables.VarLeft.RefmodFailure:
                return
        elif right_side:
            # Our user wants to do an assignment: "cprint leftside=rightside"
            # Alas, we cannot accommodate that request:
            ConditionalRaise('Multiple symbols match the left-hand-side "{0}" in current context.'.format('/'.join(left_side)))
            return

        rside = None
        if right_side:
            # We have something on the right side.
            #
            # Let's see if it is a number:
            rside = Rside()
            rside.FromString(' '.join(right_side))
            #if rside.is_address_of or rside.is_length_of:
            #    right_side = rside.original_string.split()

            if rside.is_symbol:
                rightside_possibilities, limit_reached = get_sorted_possibilities(right_side, 1)
                if not rightside_possibilities and not limit_reached:
                    ConditionalRaise('No symbol matches "{0}" in current context'.format('/'.join(right_side)))
                    return
                if limit_reached:
                    ConditionalRaise('Multiple symbols match the right-hand-side "{0}" in current context'.format('/'.join(right_side)))
                    return
                # right_side seems to refer to one, and only one, variable.
                # Let's fetch its data:
                index = rightside_possibilities[0]
                flag = 0
                if index < 0:
                    flag  = (-index) // LENADDR_FLAG_BIAS
                    index = (-index)  % LENADDR_FLAG_BIAS
                payload = GV_ModuleInformation.var_trie.storage_list[index]
                payload.FetchVariableData(False)
                if flag == LENADDR_FLAG_LENGTH:
                    # The length of the data is desired as a RHS variable
                    rside.FromString(str( payload.ActualLength))
                elif flag == LENADDR_FLAG_ADDRESS:
                    # The address of the data is desired as a RHS variable
                    rside.FromString(str( payload.ActualLocation))
                else:
                    # And now we fetch its data as a string, and use that for subsequent processing
                    rside.FromString(payload.DataToString())

        if self.machine_interface_mode:
            self.ProcessArgumentsMachineMode(left_side,
                                             rside,
                                             leftside_possibilities,
                                             rightside_possibilities)
        else:
            self.ProcessArgumentsHumanMode(leftside_possibilities,
                                           rside)

    @staticmethod
    def HandleDoubleQuote(arguments):
        # We know arguments has an early double-quote
        nfound = arguments.find('"')
        if nfound > -1:
            arguments = arguments[nfound:]
            nfound = arguments.find('"', 1)
            if nfound == -1:
                print("Unterminated string")
            else:
                print(arguments[:nfound + 1])
        return


class CobolAfterWatchBreakpoint(gdb.Breakpoint):
    def __init__(self, creating_wp):
        if GV_GlobalVariables.gdb_version < 80300:
            # sanity check, this code is only executed on manual function call
            raise gdb.GdbError("pure COBOL cwatch needs GDB 8.3+")

        # frame we are currently in, to get the resume address
        frame = CFrameWorker.Frame(True, 0)
        if not frame:
            frame = gdb.selected_frame()

        # the "actual correct" address would be the following, but we'd need to until there, not break
        # resume_address = '*' + str(hex(frame.find_sal().last)) # last instruction of the current statement,
        # so we use the return address of the current function which often will lead to a twice reached BP,
        # "seemingly" at the end of the same COBOL line, worked around by "one more"
        #resume_address = '*' + str(hex(frame.pc)    # current return instruction
        resume_address = '*' + str(hex(frame.pc() + 1))    # current return instruction + 1 _seems_ to always work

        super(CobolAfterWatchBreakpoint, self).__init__(resume_address, temporary=True, internal=True)

        # we fake the output of the original watchpoint + COBOL in CobolWatchpointHandler
        self.silent = True
        self.creating_wp = creating_wp
        # reference to allow the creating_wp to control this temporary breakpoint
        creating_wp.after_watch = self

        # copy commands as necessary ("real watchpoint" never halts and therefore commands are never executed)
        # note: we do need to place "cup 0" here as the stop method is not allowed to adjust the current frame
        if creating_wp.commands:
            # remove notice we've previously added
            more_commands = re.sub(WATCHPOINT_NOTE_PATTERN, "", creating_wp.commands)
        else:
            more_commands = None

        if more_commands:
            self.commands = "cup 0" + "\n" + more_commands
        else:
            self.commands = "cup 0"

    def stop(self):
        return_value = self.creating_wp.handler.check_and_output_wp()
        if return_value:
            # if no output was done then we can keep the AfterWatchpoint
            self.creating_wp.activate_stop()
        # note: as this is a temporary breakpoint returning "True" will delete it
        return return_value  # _now_ we commonly do want to stop (with the exception of an unchanged watchpoint)

    def out_of_scope(self):
        # we never have something to do out of scope
        self.creating_wp.activate_stop()
        if self.is_valid():
            self.delete()


class CobolWatchpointHandler():
    def __init__(self, creating_wp, payload):
        print("... for " + payload.NameWithSubscripts())
        self.creating_wp = creating_wp
        self.payload = payload
        self.old_value = self.payload.DataToString()

    def check_and_output_wp(self):
        if not (self.creating_wp.frame.is_valid() and CFrameWorker.has_cobol_frame()):
            new_value = "<unreadable>"
            if self.old_value != new_value:
                self.old_value = new_value
            else:
                return False
        title = {
            # gdb.BP_WATCHPOINT : "Watchpoint",
            gdb.BP_HARDWARE_WATCHPOINT: "Hardware watchpoint",
            gdb.BP_READ_WATCHPOINT: "Hardware read watchpoint",
            gdb.BP_ACCESS_WATCHPOINT: "Hardware access (read/write) watchpoint",
        }
        bp_type = self.creating_wp.type
        title_string = title.get(bp_type, "Unknown watchpoint " + str(bp_type))
        payload = self.payload
        if bp_type in (gdb.BP_HARDWARE_WATCHPOINT, gdb.BP_ACCESS_WATCHPOINT):
            # Set IsDirty and refetch, with a temporary selection of the originating frame:
            current_frame = gdb.selected_frame()
            try:
                self.creating_wp.frame.select()
                self.payload.SetIsDirty()
                self.payload.FetchVariableData(False)
                new_value = self.payload.DataToString()
            finally:
                current_frame.select()
        else:
            new_value = self.old_value

        if bp_type == gdb.BP_HARDWARE_WATCHPOINT and new_value == self.old_value:
            return False

        print("{0} {1}: {2} from {3}\n".format(title_string,
                                               self.creating_wp.number,
                                               payload.NameWithSubscripts(),
                                               payload.Program))

        if new_value != self.old_value:
            print("Old value: {0}".format(self.old_value))
            print("New value: {0}".format(new_value))
            self.old_value = new_value
        else:
            print("Value: {0}".format(self.old_value))
        return True


class CobolWatchBreakpoint(gdb.Breakpoint):
    def __init__(self, payload, bp_spec, bp_action):
        if GV_GlobalVariables.gdb_version < 80300:
            # sanity check, this code is only executed on manual function call
            raise gdb.GdbError("pure COBOL cwatch needs GDB 8.3+")
        try:
            super(CobolWatchBreakpoint, self).__init__(bp_spec, gdb.BP_WATCHPOINT, bp_action)
            CBLDBG_CBREAK_WORKER.cobol_breaks.append(self)
        except Exception as ex:
            ConditionalRaise(ex)
            return
        if self.type == gdb.BP_WATCHPOINT:
            self.delete()
            ConditionalRaise("Expression cannot be implemented as hardware watchpoint.\n... and therefore removed")
            return
        self.handler = CobolWatchpointHandler(self, payload)
        self.after_done = 0
        self.frame = gdb.selected_frame()
        # we can drop a note here (for "info breakpoints") as we remove it later in the afterwatch
        self.commands = WATCHPOINT_NOTE + payload.NameWithSubscripts() + "\\n"
        self.after_watch = None

    def stop(self):
        if self.after_done == 0:
            # in case of startup we may not have a single COBOL frame
            if not CFrameWorker.has_cobol_frame():
                return False
            self.after_done = 1
            CobolAfterWatchBreakpoint(self)

        # we never want the COBOL watchpoints to end directly because:
        # * this would interfer internal multi-step-changes like memmove_sse_back
        # * stopping here will also show the "C-like" output
        # * commands would be executed (is done in the AfterWatchBreakPoint)
        return False

    def out_of_scope(self):
        # remove the after watchpoint and its reference
        if self.after_watch:
            if self.after_watch.is_valid():
                self.after_watch.delete()
            self.after_watch = None
        self.after_done = 0

    def activate_stop(self):
        self.after_done = 0


class CWatchWorker():
    """Does most of the non-gdb work of CWatch()"""

    def __init__(self):
        pass

    @staticmethod
    def set_watchpoint(payload, wp_type):
        if payload.Refmod:
            wp_startloc = payload.location_of_data + payload.table_offset + payload.RefmodOffset
            wp_length = payload.RefmodLength
        else:
            wp_startloc = payload.location_of_data + payload.table_offset
            wp_length = ExpressionEvaluate(payload.LengthExpression)

        spec = "*(char(*)"
        spec += '[' + str(wp_length) + '])'
        spec += '(' + hex(wp_startloc) + ')'

        if False and GV_GlobalVariables.gdb_version >= 80300:
            #
            # I have effectively removed this code because I have determined that it doesn't work
            # as intended.  For the CWATCH test program compiled with GnuCOBOL 3.1.2, it seems to
            # work once, but somehow memory gets corrupted and a second attempt to run through
            # the program fails.  When compiled with 3.2, the breakpoint appears to be set but
            # the trap never occurs, and, again, memory is corrupted.
            #
            # I saw the corruption with 3.1 by setting a CWATCH and then continuing my way
            # through the program (TEST006 is an example) until the inferior completed.  A
            # subsequent RUN command results in an SIGILL illegal instruction trap.  With
            # 3.2, the trap never occurs, and the second attempt at RUN results in SIGILL.
            #
            # Using the gdb.Breakpoint method seems to work.  The display isn't as nice, because
            # it appears the more complex CobolWatchBreakpoint processing apparently tries to
            # find the frame with the relevant COBOL source code, but I don't regard this as
            # a problem; the user can always use backtrace to see where in the COBOL source code
            # the access was initiated.  Dubner, 2022-01-10
            #
            break_point = CobolWatchBreakpoint(payload, spec, wp_type)
        else:
            break_point = gdb.Breakpoint(spec, gdb.BP_WATCHPOINT, wp_type)
            if break_point.type == gdb.BP_WATCHPOINT:
                break_point.delete()
                ConditionalRaise("Expression cannot be implemented as hardware watchpoint.\n... and therefore removed")
                return
            # Note: GDB 7.6 has no writable commands attribute, gdb.execute with multi-lines does not work either)
            # break_point.commands = "echo Watched: {0} from {1}\\n \n cup 0".format(payload.NameWithSubscripts(), payload.Program)

    def process_watch_arguments(self, arguments):
        left_side = []
        right_side = []

        args = arguments.split()
        if not args:
            print("No variable given\n")
            return

        wp_type = gdb.WP_WRITE
        if args[0] == '/r':
            wp_type = gdb.WP_READ
            del args[0]
        elif args[0] == '/w':
            del args[0]
        elif args[0] == '/rw':
            wp_type = gdb.WP_ACCESS
            del args[0]
        elif args[0].startswith('/'):
            ConditionalRaise("Option not recognized\n")
            return

        while len(args) >= 3 and args[1].upper() in ('OF','IN'):
            args[0] = args[0] + "/" + args[2]
            del args[2]
            del args[1]

        if len(args) > 3:
            ConditionalRaise("Too many arguments")
            return

        left_side, right_side, _condition = get_left_right_args(''.join(args))
        if not left_side:
            return

        if right_side:
            ConditionalRaise('No rightside arguments implemented')
            return

        possibilities, limit_reached = get_sorted_possibilities(left_side, 1)
        if not possibilities and not limit_reached:
            ConditionalRaise('No symbol matching "{0}" in current context'.format(''.join(left_side)))
            return

        if limit_reached:
            ConditionalRaise('More than one symbol matching "{0}" in current context.'.format(''.join(left_side)))
            return

        # Pick up the payload from the list
        if possibilities[0] < 0:
            ConditionalRaise("Invalid syntax")
            return
        payload = GV_ModuleInformation.var_trie.storage_list[possibilities[0]]
        payload.FetchVariableData(False)

        # And go set the Watchpoint
        self.set_watchpoint(payload, wp_type)


class CobolBreakpoint(gdb.Breakpoint):
    def __init__(self, bp_spec, t_flag, i_flag):
        try:
            if GV_GlobalVariables.gdb_version >= 70700:
                super(CobolBreakpoint, self).__init__(bp_spec, type=gdb.BP_BREAKPOINT, temporary=t_flag, internal=i_flag)
            else:
                super(CobolBreakpoint, self).__init__(bp_spec, type=gdb.BP_BREAKPOINT, internal=i_flag)
            CBLDBG_CBREAK_WORKER.cobol_breaks.append(self)
        except gdb.error:
            # pass errors as is, most likely: malformed bp_spec
            raise
        except Exception as ex:
            ConditionalRaise(ex)
            return

        self.t_flag = t_flag
        self.comp_op = ""
        self.case_ins = False
        self.payload_f = None
        self.payload_s = None
        self.is_lit = False
        self.r_value = ""

    def stop(self):
        retval = True
        r_value = ""
        l_value = ""

        if self.comp_op == "":
            if GV_GlobalVariables.gdb_version < 70700 and self.t_flag:
                gdb.post_event(BPRemover(self))
            return True

        self.payload_f.SetIsDirty()
        self.payload_f.FetchVariableData(False)
        l_value = self.payload_f.DataToString()

        if self.is_lit:
            r_value = self.r_value
        elif self.payload_s is not None:
            self.payload_s.SetIsDirty()
            self.payload_s.FetchVariableData(False)
            r_value = self.payload_s.DataToString()[1:-1]

        if l_value.isdigit() and r_value.isdigit():
            expression = "{0} {1} {2}".format(float(l_value), self.comp_op, float(r_value))
        elif self.comp_op != "=~" and r_value != "ALL_SPACES":
            if self.case_ins:
                # expression = '"' + l_value[1:len(r_value) + 1].upper() + '"' + self.comp_op + '"' + r_value.upper() + '"'
                expression = '"' + l_value[1:-1].strip().upper() + '"' + self.comp_op + '"' + r_value.strip().upper() + '"'
            else:
                # expression = '"' + l_value[1:len(r_value) + 1] + '"' + self.comp_op + '"' + r_value + '"'
                expression = '"' + l_value[1:-1].strip() + '"' + self.comp_op + '"' + r_value.strip() + '"'
        elif self.comp_op != "=~" and r_value == "ALL_SPACES":
            expression = "{0}.isspace()".format(l_value)
        elif self.comp_op == "=~":
            if self.case_ins:
                expression = "re.match('{0}', '{1}', re.IGNORECASE)".format(r_value, l_value[1:-1])
            else:
                expression = "re.match('{0}', '{1}')".format(r_value, l_value[1:-1])

        exp_result = eval(expression)
        if self.comp_op == "=~":
            if exp_result is None:
                retval = False
        else:
            retval = exp_result

        if retval:
            print("\n{0}: {1}".format(self.payload_f.Name, l_value))

        if retval and GV_GlobalVariables.gdb_version < 70700 and self.t_flag:
            gdb.post_event(BPRemover(self))

        return retval


class CBreakWorker():
    def __init__(self):
        try:
            self.strtype = basestring
        except NameError:
            self.strtype = str

        self.section_buffer = None
        self.module_buffer = None
        self.cobol_breaks = []

    @staticmethod
    def set_entry_breakpoint(module_name, t_flag, i_flag):
        if not module_name:
            raise Exception("module name missing")  # never happens from user-code
        module_name = encode_program_id(module_name, 0)
        # Note: the following does NOT work reliable as cobc 3.0+ have a bug in generating the correct line reference for the ENTRY_prog label
        #       when DECLARATIVES are used (it always points to the PROCEDURE DIVISION, not to the first (possibly implied) "real" section)
        # return CobolBreakpoint("-function {0}_ -label ENTRY_{1}".format(module_name,module_name.upper()), t_flag, i_flag)
        return CobolBreakpoint(module_name + "_", t_flag, i_flag)

    @staticmethod
    def module_available(module):
        try:
            spec = "{0}_".format(module)
            spec_ref = gdb.parse_and_eval(spec)
            # TODO: come back here later, we did check for the module here but there was an issue with that.
            return True
        except gdb.error:
            return False

    # FIXME: this function should only "get module name", the check for loaded and loading is a different thing
    def get_module_name(self, module):
        return_value = None

        if module == "":
            return return_value

        module = encode_program_id(module, 0)
        if self.module_available(module):
            return_value = module
        else:
            # if READELF_AVAILABLE:
            # TODO Modules loaded via readelf miss different information, commands not working: "adv", "cp"
            if False:
                modules_loaded = CBLDBG_CLOAD_WORKER.load_modules(module.split())
                if modules_loaded is not None and self.module_available(module):
                    return_value = module
            else:
                return_value = module

        return return_value

    @staticmethod
    def get_var_payload(search_args):
        payload = None
        possibilities = []

        GV_ModuleState.EstablishModuleState(True)
        # HACK to let cbreak work until it is refactored
        # TODO do the refactoring or at least document the issue
        possibilities, limit_reached = get_sorted_possibilities(search_args, 1)

        if limit_reached:
            ConditionalRaise('More than one symbol matching "{0}" in current context.'.format(''.join(search_args)))
            return payload

        if not possibilities and not limit_reached:
            ConditionalRaise('No symbol matching "{0}" in current context'.format(" ".join(search_args)))
            return payload

        payload_number = possibilities[0]
        if payload_number < 0:
            payload_number = (-payload_number) % LENADDR_FLAG_BIAS

        # Pick up the payload from the list
        payload = GV_ModuleInformation.var_trie.storage_list[payload_number]
        payload.FetchVariableData(False)

        return payload

    @staticmethod
    def set_section_breakpoint(args, t_flag, i_flag):
        spec = get_section_spec(args[0])
        CobolBreakpoint(spec, t_flag, i_flag)

    def set_cond_break(self, left_side, right_side, condition, break_point, is_lit, t_flag, i_flag):
        payload_s = None
        case_ins = False
        comp_op = ""

        if condition[-1:] == "i":
            case_ins = True
            condition = condition[:-1]

        if condition in ("=", "=="):
            comp_op = "=="
        elif condition in ("<>", "!="):
            comp_op = "!="
        elif condition in (">=", "<=", ">", "<", "=~"):
            comp_op = condition
        else:
            ConditionalRaise("Condition {0} not implemented".format(condition))
            return

        line_nr = left_side[0]
        del left_side[0]

        payload_f = self.get_var_payload(left_side)
        if payload_f is None:
            # note: error handling already in get_var_payload
            return

        if comp_op == "=~" and payload_f.AttrType not in (1, 33, 34, 35):
            ConditionalRaise("Not yet implemented (Attribute type " + payload_f.AttrType + ")")
            return

        if is_lit == 0 and not right_side[0].isdigit():
            payload_s = self.get_var_payload(right_side)
            if payload_s is None:
                # note: error handling already in get_var_payload
                return

        if break_point is None:
            break_point = CobolBreakpoint(line_nr, t_flag, i_flag)
        else:
            print("Set condition for breakpoint nr: {0}".format(break_point.number))

        break_point.payload_f = payload_f
        break_point.payload_s = payload_s
        break_point.is_lit = is_lit
        break_point.comp_op = comp_op
        break_point.case_ins = case_ins

        if R_ALLSPACE.match(right_side[0]) or R_SPACE.match(right_side[0]):
            break_point.r_value = "ALL_SPACES"
            break_point.is_lit = 1
        elif R_ALLZERO.match(right_side[0]) or R_ZERO.match(right_side[0]):
            break_point.r_value = 0
            break_point.is_lit = 1
        elif is_lit == 1:
            break_point.r_value = right_side[0][1:-1]
        elif right_side[0].isdigit():
            break_point.r_value = right_side[0]
            break_point.is_lit = 1

    def process_break_arguments(self, arguments, t_flag, i_flag):
        is_lit = 0
        left_side = []
        right_side = []
        condition = ""

        if arguments and arguments[-1] in ('"', '\''):
            is_lit = 1

        if not arguments:
            # no position specified - create simple one at "" -> current place
            CobolBreakpoint("", t_flag, i_flag)
            return

        # ensure that a file path /path/to/file.cob is not broken in get_left_right_args
        # because it looks like a var path A/B/C
        mat = re.match(FILE_AND_REST, arguments)
        if mat:
            source_reference = mat.group(1)
            left_side, right_side, condition = get_left_right_args(mat.group(4))
        else:
            source_reference = None
            left_side, right_side, condition = get_left_right_args(arguments)

        if not left_side:
            if source_reference:
                left_side = [source_reference]
            else:
                return
        else:
            if source_reference:
                left_side = [source_reference] + left_side

        if not right_side:
            if len(left_side) >= 1 and isinstance(left_side[0], self.strtype) and (left_side[0].upper() == "SECTION" or left_side[0].upper() == "SEC"):
                left_side[0] = "SECTION"

            if len(left_side) == 1 and left_side[0].isdigit():
                # assume line number   --> cbreak 15
                CobolBreakpoint(left_side[0], t_flag, i_flag)
            elif len(left_side) == 1 and isinstance(left_side[0], self.strtype) and left_side[0].find(':') != -1:
                if source_reference:
                    # assume file source    --> cbreak prog.cob:42
                    CobolBreakpoint(left_side[0], t_flag, i_flag)
                else:
                    # assume MODULE:SECTION
                    self.set_section_breakpoint(left_side, t_flag, i_flag)
            elif len(left_side) == 1 and left_side[0] != "SECTION":
                module_name = self.get_module_name(left_side[0])
                if module_name is None:
                    print("Module not found: {0}".format(left_side[0]))
                else:
                    self.set_entry_breakpoint(module_name, t_flag, i_flag)
            elif len(left_side) == 2 and left_side[0] == "SECTION" and isinstance(left_side[1], self.strtype):
                del left_side[0]
                self.set_section_breakpoint(left_side, t_flag, i_flag)
            else:
                ConditionalRaise('Breakpoint parameters not recognized')
                return
        elif right_side and condition != "":
            if len(left_side) < 2:
                ConditionalRaise('Breakpoint parameters not recognized')
                return

            if len(left_side) >= 2 and not left_side[0].isdigit():
                #print("No line specifier, using current location")
                left_side.insert(0, '-1')

            if left_side[1] != "if":
                ConditionalRaise('Second argument should be "if"')
                return

            del left_side[1]

            self.set_cond_break(left_side, right_side, condition, None, is_lit, t_flag, i_flag)
        else:
            ConditionalRaise('Breakpoint parameters not recognized')
            return

    @staticmethod
    def find_breakpoint(bp_nr):
        return_breakpoint = None

        #for gdb_breakpoint in CBLDBG_CBREAK_WORKER.cobol_breaks:
        for gdb_breakpoint in gdb.breakpoints():
            if gdb_breakpoint.number == int(bp_nr):
                return_breakpoint = gdb_breakpoint
                break

        if return_breakpoint is None:
            ConditionalRaise("No breakpoint number {0}.format(bp_nr)")

        return return_breakpoint

    def process_condition_arguments(self, arguments):
        left_side = []
        right_side = []
        condition = ""
        is_lit = 0

        if arguments[-1] in ('"', '\''):
            is_lit = 1

        if arguments:
            left_side, right_side, condition = get_left_right_args(arguments)
            if not left_side:
                return

        if not left_side[0].isdigit():
            ConditionalRaise('First argument is not a breakpoint number')
            return

        break_point = self.find_breakpoint(left_side[0])
        if break_point is None:
            return

        if isinstance(break_point, CobolBreakpoint):
            if len(left_side) == 1:
                print("Breakpoint {0} now unconditional.".format(left_side[0]))
                break_point.comp_op = ""
            elif len(left_side) >= 2 and len(left_side) <= 4:
                self.set_cond_break(left_side, right_side, condition, break_point, is_lit, False, False)
            else:
                ConditionalRaise('Breakpoint parameters not recognized')
                return
        else:
            ConditionalRaise('Breakpoint is not a COBOL breakpoint')
            return


class BPRemover():
    def __init__(self, break_point):
        self.bp = break_point

    def __call__(self):
        self.bp.delete()


class CPrint(gdb.Command):
    """Prints COBOL-NAMED variables when debugging cobcd (via cobst) processed object files.

    COBOL cprint command usage:
    cprint VAR          displays VAR using session-default formatting
    cprint VAR!         when VAR is ambiguous, all the possibilities are listed instead of counted
    cprint VAR(N)       displays the Nth element of table <arg>. Multiple subscripts are allowed
    cprint VAR(S:R)     reference modification: displays R characters starting at S in <arg>
    cprint/v0           session formatting is single line, with LEVEL & NAME and VALUE
    cprint/v1           session formatting is single line, just the value
    cprint/v2           session formatting two lines: LEVEL & NAME followed by VALUE
    cprint/v3           like /v0, but with LEVEL & NAME/PROGRAM-ID [STORAGE] and VALUE
    cprint/v4           same as /v1
    cprint/v5           like /v2, but with LEVEL & NAME/PROGRAM-ID [STORAGE] then VALUE
    cprint/v6           like /v0, but with just NAME and VALUE
    cprint/v7           same as /v1
    cprint/v8           like /v2, but with just NAME then VALUE
    cprint/v<n> <arg>   displays <arg> with a one-time formatting override
    cprint/p            enable  'pretty print' (one-time override if there is an argument)
    cprint/P            disable 'pretty print'
    cprint/x            show numeric value in hexadecimal
    cprint/b            show numeric value in binary
    cprint/d            show expanded information
    cprint/v<n>         with no arguments sets the session formatting
    cprint/m            generate output compatible with '-stack-list-variables --simple-values
    cprint *            display all variables that are currently in context
    cprint #N           display the Nth variable in the context list
    cprint/r N          set the range for 'cprint ?' to plus/minus N (default 6; -1 means 'all'; -2 'none')
    cprint/r            show the current value of N
    cprint ?            display in-context variables within +/- N lines of current line
    cprint ? R          display in-context variables within +/- R lines of current line
    cprint VARIABLE="STRING"    set variable to string
    cprint VARIABLE='STRING'    set variable to string
    cprint VARIABLE=NUMERIC     set variable to numeric
    cprint VARIABLE=VARIABLE2   set variable to variable2

    cprint set repeat N sets the display <repeats> threshold to N characters

    The default v-level can be set with the environment variable CPRINT_V=<n>
    The default 'pretty print' mode can be turned on with        CPRINT_P=p
    The default r-level can be set with the environment variable CPRINT_R=<n>"""

    def __init__(self):
        global COBCDRW_AVAILABLE

        try:
            subprocess.check_output([COBCDRW_BINARY, '-v'])
        except subprocess.CalledProcessError as ex:
            # cobcd-rw until 4.26 release returned "1", assume this is fine
            if ex.returncode != 1:
                print("WARNING: \"cprint ?\" not available: {0} not available, consider setting COBCDRW".format(COBCDRW_BINARY))
                COBCDRW_AVAILABLE = False
        # note: specific errors were only added with Python 3.3, so we catch everything...
        # except subprocess.CalledProcessError:
        except:   # TODO: use appropriate exception
            print("WARNING: \"cprint ?\" not available: {0} not available, consider setting COBCDRW".format(COBCDRW_BINARY))
            COBCDRW_AVAILABLE = False

        self.worker = CPrintWorker()

        if GV_noisy_registration:
            print('Registering CPrint ' + COBCD_VERSION)
        super(CPrint, self).__init__("cprint", gdb.COMMAND_DATA)

        # set alias, ignore errors (most likely because already defined)
        try:
            gdb.execute("alias -a cp = cprint", False, True)
        except gdb.error:
            pass

    def invoke(self, arguments, from_tty):  # invoke cprint (this comment is for finding this invoke)
        try:
            self.worker.ProcessArguments(arguments)
        # pass user errors unchanged
        except gdb.GdbError:
            raise
        except:   # TODO: use appropriate exception
            traceback.print_exc()

    def complete(self, text, word):
        # We expect COBOL variables which GDB doesn't have, so the fallback
        # gdb.COMPLETE_SYMBOL can't be used.

        try:
            # HACK: because of missing support for hypens: in identifiers
            # we have to look at the text for that
            # but the hack doesn't work because GDB won't complete at - either...
            # if text and text.find('-'):
            #    word = text # split, last part

            # note: modifier / slash is no word, so in this case word is None
            if word and (word[0] in ('*', '?', '"', '\'') or word[0].isdigit()):
                # if it doesn't look like a word then just return nothing
                return gdb.COMPLETE_NONE

            if word is None or word == "" or word[0] in ('(', ')', ',', ':'):
                # gdb returns everything, do the same here
                word = '*'

            possibilities = get_completion_possibilities(word)

            return possibilities
        except:   # TODO: use appropriate exception
            traceback.print_exc()


class CWatch(gdb.Command):
    """Used for creating COBOL-NAMED watchpoints when debugging cobst processed object files.

Set a watchpoint for an expression.
Usage: cwatch [/r|/w|/rw] VARIABLE [(start:length)]
A watchpoint stops execution of your program whenever the value of an expression changes.

    cwatch VARIABLE      set a (write) watch on this variable
    cwatch/w VARIABLE    set a write watch on this variable
    cwatch/r VARIABLE    set a read watch on this variable
    cwatch/rw VARIABLE   set a read/write watch on this variable

    if (start:length) is given the VARIABLE is watched with this startpoint and length"""

    def __init__(self):
        super(CWatch, self).__init__("cwatch", gdb.COMMAND_BREAKPOINTS)
        # set alias, ignore errors (most likely because already defined)
        try:
            gdb.execute("alias -a cw = cwatch", False, True)
        except gdb.error:
            pass
        self.worker = CWatchWorker()

    def invoke(self, arguments, from_tty):
        self.dont_repeat()
        try:
            GV_ModuleState.EstablishModuleState(True)
            self.worker.process_watch_arguments(arguments)
        # pass user errors unchanged
        except gdb.GdbError:
            raise
        except:
            traceback.print_exc()

    def complete(self, text, word):
        try:
            # note: modifier / slash is no word, so in this case word is None
            if word and (word[0] in ('*', '?', '"', '\'') or word[0].isdigit()):
                # if it doesn't look like a word then just return nothing
                return gdb.COMPLETE_NONE

            if word is None or word == "" or word[0] in ('(', ')', ',', ':'):
                # gdb returns everything, do the same here
                word = '*'

            possibilities = get_completion_possibilities(word)
            return possibilities
        except:
            traceback.print_exc()


class CBreak(gdb.Command):
    """Used for creating COBOL-NAMED breakpoints when debugging cobst processed object files.

Set a breakpoint for an expression.
Usage: cbreak
A breakpoint stops execution of your program whenever the expression matches

    cbreak                                       set a breakpoint on the current line
    cbreak MODULE                                set a breakpoint on MODULE entry
    cbreak section SECTION                       set a breakpoint on SECTION entry
    cbreak section MODULE:SECTION                set a breakpoint on SECTION entry in MODULE
    cbreak LINENR                                set a breakpoint on line number (LINENR)
    cbreak LINENR if VARIABLE =[i] MSTRING       set a breakpoint on line number (LINENR)
                                                 for string VARIABLE equals MSTRING
    cbreak LINENR if VARIABLE <>[i] MSTRING      set a breakpoint on line number (LINENR)
                                                 for string VARIABLE not equal MSTRING
    cbreak LINENR if VARIABLE =~ REGEXP          set a breakpoint on line number (LINENR)
                                                 for string VARIABLE when matching REGEXP
    cbreak LINENR if NUMVAR condition VALUE      set a breakpoint on line number (LINENR)
                                                 for numeric VARIABLE condition and value
                                                 where condition is == < <= > >= <>
    cbreak LINENR if VARIABLE condition VARIABLE set a breakpoint on line number (LINENR)
                                                 for variable condition and second variable
                                                 where condition is = <> >= <= > <

    if (start:length) is given the VARIABLE is checked with this startpoint and length

    MSTRING may also be: (ALL)SPACE/SPACES, ZERO/ZEROS/ZEROES

    Examples:
    cbreak 28 VARIABLE = "breakstring"
    cbreak 28 VARIABLE (2:3) = "breakstring"
    cbreak 28 VARIABLE =i "breakstring"          option i for ignore case
    cbreak 28 VARIABLE =~ ^break*

    cbreak 29 VARIABLE > 12"""

    def __init__(self):
        self.worker = CBLDBG_CBREAK_WORKER

        if GV_noisy_registration:
            print('Registering commands cwatch, cbreak, ctbreak, ccondition')
        super(CBreak, self).__init__("cbreak", gdb.COMMAND_BREAKPOINTS)
        # set alias, ignore errors (most likely because already defined)
        try:
            gdb.execute("alias -a cb = cbreak", False, True)
        except gdb.error:
            pass
        CTBreak()
        CCondition()
        self.bp_initialized = False

    def invoke(self, arguments, from_tty): # invoke cbreak
        GV_ModuleState.EstablishModuleState(True)
        if not self.bp_initialized:
            self.bp_initialized = True
            if GV_GlobalVariables.gdb_version >= 70700:
                if GV_noisy_registration:
                    print('Registering silent maintenance breakpoint for showing note about ACCEPT')
                bp = CobolBreakpoint("cob_accept", False, True)
                if bp:
                    # in an ideal world we would know what the last command executed
                    # was and would for step/next/cnext do a "finish", otherwise a "continue"
                    # bad as that command can come from "everywhere" (python/guile/gdbscript, mi, console)
                    # ["direct console only" would have been solved by looking at the GDB history]
                    # we play safe stopping after the accept and let the user re-execute "continue"
                    # if this is what he wanted
                    bp.commands = "silent \n cup 0 \n down-silently \n finish"

        self.dont_repeat()
        self.worker.process_break_arguments(arguments, False, False)

    def complete(self, text, word):
        # TODO: if we are behind "section" or the current word is "section",
        #       then complete section names
        # TODO: if we are behind "if" or the current word is "if",
        #       then issue break_completion (text, word, 1)
        possibilities = break_completion(text, word, 0)
        return possibilities


class CTBreak(gdb.Command):
    """Set a temporary breakpoint.
Like "cbreak" except the breakpoint is only temporary,
so it will be deleted when hit.  Equivalent to "break" followed
by using "delete" on the breakpoint number.

Do "help cbreak" for info on COBOL expressions."""

    def __init__(self):
        self.worker = CBLDBG_CBREAK_WORKER
        super(CTBreak, self).__init__("ctbreak", gdb.COMMAND_BREAKPOINTS)

    def invoke(self, arguments, from_tty):
        self.dont_repeat()
        self.worker.process_break_arguments(arguments, True, False)

    def complete(self, text, word):
        # TODO: if we are behind "section" or the current word is "section",
        #       then complete section names
        # TODO: if we are behind "if" or the current word is "if",
        #       then issue break_completion (text, word, 1)
        possibilities = break_completion(text, word, 0)
        return possibilities


class CCondition(gdb.Command):
    """Specify breakpoint number N to break only if COND is true.

Usage is `ccondition N COND', where N is an integer and COND is a COBOL
expression to be evaluated whenever breakpoint N is reached.

Do "help cbreak" for info on COBOL expressions."""

    def __init__(self):
        self.worker = CBLDBG_CBREAK_WORKER
        super(CCondition, self).__init__("ccondition", gdb.COMMAND_BREAKPOINTS)

    def invoke(self, arguments, from_tty):
        self.dont_repeat()
        self.worker.process_condition_arguments(arguments)

    def complete(self, text, word):
        possibilities = break_completion(text, word, 1)
        return possibilities


class CLoadWorker():
    """Does most of the non-gdb work of CLoad(),
    managing a list of (likely) COBOL modules and paths found via COB_LIBRARY_PATH
    For remote/cross-debugging: needs local access to the executables along with the readelf binary
    and possibly COB_MODULE_EXT set."""

    def __init__(self):
        # TODO: try to get this "target" info from GDB
        self.objext = os.getenv('COB_MODULE_EXT')
        if self.objext is None:
            # FIXME: this is not necessarily the same as the GDBs that executes this python...
            debugged_system = platform.system()
            if debugged_system == 'Windows':
                self.objext = '.dll'
            else:
                self.objext = '.so'

        # note: try to get the value from libcob won't help for renotte-/cross-debugging, as the local
        #       python iterates over the folders
        library_path = os.getenv('COB_LIBRARY_PATH', '.')
        self.module_paths = library_path.split(path.pathsep)

        self.modules = []
        for path_entry in self.module_paths:
            try:
                if not path.isdir(path_entry):
                    print("CloadWorker warning: entry in COB_LIBRARY_PATH skipped, is not a path:", path_entry)
                    continue
                files = os.listdir(path_entry)
            except:
                print("CloadWorker warning: entry in COB_LIBRARY_PATH is not a path:", path_entry)
                continue
            for f in files:
                parts = path.splitext(f)
                if parts[1] == self.objext:
                    self.modules.append(parts[0])
        self.modules = set(self.modules)  # removing possible duplicates and order

    def get_module_ext(self):
        # returns the module extension
        return self.objext

    def get_module_paths(self):
        # returns the paths in COB_LIBRARY_PATH
        return self.module_paths

    def query_modules(self, filters):
        # returns the modules found on start, but only the ones matching the given regex filter(s)
        if not filters:
            filters = {""}
        result_entries = []
        # that's a query, tab completions don't apply here (neither in max entries nor in formatting)
        # and it is too less important to add an own parameter
        # max_entries = get_max_completions()
        # pending_entries = max_entries
        for filter_entry in filters:
            pattern = re.compile(filter_entry, re.IGNORECASE)
            for module in self.modules:
                if pattern.match(module) is not None:
                    result_entries.append(module)
                    # pending_entries -= 1
                    # if pending_entries < 0:
                    #     result_entries = set(result_entries)  # removing duplicates
                    #     number_of_entries = len(result_entries)
                    #     if number_of_entries > max_entries:
                    #         return "  ".join(result_entries) + "\n*** List may be truncated, max-completions reached. ***"
                    #     else:
                    #         pending_entries = max_entries - number_of_entries
        # Note: in the case of completions GDB returns a nice table, we may add that someday
        return "  ".join(set(result_entries))  # removing duplicates and order

    def load_modules(self, module_names):
        return_list = []

        for module_name in module_names:
            solibpath = None
            paths = self.get_module_paths()
            for path_entry in paths:
                module_path = path.join(path_entry, module_name + CBLDBG_CLOAD_WORKER.get_module_ext())
                if path.isfile(module_path):
                    solibpath = module_path
                    break

            if solibpath is None:
                print("{0}: module not found in {1}" .format(module_name, paths))
                continue

            text_addr, symbol_addrs = CBLDBG_CLOAD_WORKER.get_symbol_addrs(solibpath)
            try:
                gdb_cmd = "add-symbol-file " + solibpath
                if not text_addr:
                    ConditionalRaise("no text section found")
                    return return_list
                # GDB 7.6 has text-address as mandatory
                gdb_cmd = gdb_cmd + " " + text_addr
                for s_section, s_address in symbol_addrs.items():
                    gdb_cmd = gdb_cmd + " -s {0} {1}".format(s_section, s_address)
                gdb.execute(gdb_cmd, False, True)
                print("Reading symbols {0}...".format(solibpath))
                return_list.append(solibpath)
            except gdb.error:
                print("Error in loading:", solibpath)
                # print("Done:", gdb_cmd)

        return return_list

    @staticmethod
    def get_symbol_addrs(solibpath):
        symbol_addrs = dict()
        elfres = subprocess.check_output([READELF_BINARY, "-WS", solibpath])
        for line in elfres.splitlines():
            line_split = line.split()
            if len(line_split) >= 5 and line_split[1][0] == '.':
                symbol_addrs.update({line_split[1]: "0x" + line_split[4]})
        return symbol_addrs['.text'], symbol_addrs

    def get_module_completion(self, word):
        # returns the modules found on start, but only the ones starting with the given filter
        if not word:
            return self.modules
        self.search = word.upper()
        return filter(self.__filterCompletions, self.modules)

    def __filterCompletions(self, word):
        if word.upper().startswith(self.search):
            return True
        else:
            return False


CBLDBG_CLOAD_WORKER = CLoadWorker()

# actual reading originally taken from https://stackoverflow.com/questions/20380204/how-to-load-multiple-symbol-files-in-gdb/62471062#62471062


class CLoad(gdb.Command):
    """Load symbols from COBOL module in COB_LIBRARY_PATH or query for available modules.
Usage: add-symbol-file-cobol MODULE           try to load given COBOL module into GDB
       add-symbol-file-cobol /q [REGEX ..]    query available modules
       add-symbol-file-cobol /c               re-read modules"""

    def __init__(self):
        global READELF_AVAILABLE

        try:
            subprocess.check_output([READELF_BINARY, '--version'])
        # note: specific errors were only added with Python 3.3, so we catch everything...
        # except subprocess.CalledProcessError:
        except:
            READELF_AVAILABLE = False
            # No raise here as we don't want a backtrace
            print("WARNING: {0} not found on path, not registering add-symbol-file-cobol, consider setting READELF".format(READELF_BINARY))
            return

        if GV_noisy_registration:
            print('Registering add-symbol-file-cobol')
        super(CLoad, self).__init__("add-symbol-file-cobol", gdb.COMMAND_FILES)
        self.search = ''

    def invoke(self, user_input, from_tty):
        self.dont_repeat()

        if user_input:
            module_names = user_input.split()
        else:
            print("module name missing")
            return

        if module_names[0][0:2] == '/c':
            global CBLDBG_CLOAD_WORKER
            CBLDBG_CLOAD_WORKER = CLoadWorker()
            print("Modules re-read")
            return

        if module_names[0][0:2] == '/q':
            del module_names[0]
            print(CBLDBG_CLOAD_WORKER.query_modules(module_names))
            return

        modules_loaded = CBLDBG_CLOAD_WORKER.load_modules(module_names)
        if modules_loaded is None:
            print("No modules found")

    def complete(self, text, word):
        return CBLDBG_CLOAD_WORKER.get_module_completion(word)


class CStart(gdb.Command):
    """(Re-)start the debugged program stopping at the beginning of the first COBOL statement.
Any arguments after the cstart command are passed to the executable.  If present,
they override any that might have been passed to GDB with --args."""

    def __init__(self):
        if GV_noisy_registration:
            print('Registering CStart')
        super(CStart, self).__init__("cstart", gdb.COMMAND_RUNNING)

    def invoke(self, arguments, from_tty):
        self.dont_repeat()
        bp = None
        try:
            obj_files = gdb.objfiles()
            if len(obj_files) < 1:
                ConditionalRaise("No symbol table loaded.  Use the \"file\" command.")
                return

            cstart_entry_name = None

            # I tried for two hours to figure out some way of listing the variables
            # in this inferior's global symbols block without using "info variables".
            # I was unsuccessful.  We risk this next part taking a while when
            # there are many variables.

            # Get all the variables that this block knows about
            variables = gdb.execute("info variables ^VARIABLE_STRING_", False, True)

            # Extract VARIABLE_STRING_xxx
            match = re.match(".*(VARIABLE_STRING_[^[]*).*", variables, re.DOTALL)
            if match:
                string_name = match.group(1)
                try:
                    # Read the first 200 characters of VARIABLE_STRING_xxx
                    the_address = GetPointerTo(string_name)
                    len_val = gdb.parse_and_eval("*(char (*)[{0}]) {1}".format(VARSTR_LEN_SIZE, the_address))
                    total_length = int(len_val.string())
                    total_length = min(200, total_length)
                    variable_string_memview = get_memoryview(the_address + VARSTR_LEN_SIZE, total_length)
                    variable_string = variable_string_memview.tobytes().decode('ascii')
                    match = re.match(".*~C[|][|]([^|]*).*", variable_string, re.DOTALL)
                    if match:
                        # Strip off final underscore; it'll be replaced later
                        cstart_entry_name = match.group(1)[:-1]
                except:   # TODO: use appropriate exception
                    pass

            line1 = gdb.objfiles()[0].filename
            line1 = line1.replace('\\', '/').replace(".exe", "")    # This is a hack for Windows
            main_program = line1.split('/')[-1].split('"')[0]
            # Note: cobcrun may be renamed or contain version numbers - is there a better approach?
            if main_program == COBCRUN_NAME:
                # Arguments passed by CSTART override any that came from the command line:
                if not arguments:
                    args = gdb.parameter("args").split()
                    if len(args) < 1 or not args[0]:
                        # especially after attaching to a program args is empty
                        ConditionalRaise("No args set.  Specify program to call or use the \"set args\" command.")
                        return
                    cobol_program = args[0]
                    arguments = ' '.join(args)
                else:
                    args = arguments.split()
                    cobol_program = args[0]
            else:
                # Use the entry point from VARIABLE_STRING, assuming we got one
                print("CSTART ", cstart_entry_name)
                cobol_program = cstart_entry_name or main_program

            # If there is no cobol program, just run
            if cobol_program:
                # set temporary maintenance breakpoint
                bp = CBreakWorker.set_entry_breakpoint(cobol_program, True, True)
                # CHECKME: Do we still want to run if breakpoint was not set?
                command = "run " + arguments
            else:
                # "start" is not expected to actually "run", so we use start in the unknown case
                command = "start " + arguments

            # And proceed to run the program:
            gdb.execute(command, True, False)

        except gdb.error as error:
            print("cstart failed: " + str(error))
            if bp and bp.is_valid():
                print("... and therefore breakpoint {0} was removed".format(bp.number))
                bp.delete()

        # pass user errors unchanged
        except gdb.GdbError:
            raise
        except:
            traceback.print_exc()


class CNext(gdb.Command):
    """The cnext command behaves like GDB 'next', but it is smart enough to
attempt to pass over PERFORMs.  It optionally takes a count parameter.

It does the best it can, but occasionally it behaves a little strangely when a
compiler optimization removes the JUMP TO PARAGRAPH and JUMP BACK FROM PARAGRAPH
logic and simply puts the paragraph in-line, which CNEXT doesn't detect."""

    def __init__(self):
        super(CNext, self).__init__("cnext", gdb.COMMAND_RUNNING)
        if GV_noisy_registration:
            print('Registering CNext')

    def invoke(self, argument, from_tty):
        ncount = GetNumericArgument(argument)
        try:
            GV_ModuleState.EstablishModuleState(True)
            while ncount > 0:
                sal = gdb.selected_frame().find_sal()
                key = "{0}:{1}".format(sal.symtab, sal.line)
                if key in GV_ModuleInformation.perform_commands:
                    command = "until " + GV_ModuleInformation.perform_commands[key]
                    gdb.execute(command, True, False)

                    # If we went to a specific line location, then the UNTIL leaves
                    # us in a nice place.  If we target a local address, then
                    # we have stopped at a l_xx: target in the C code that shows
                    # up as being before where the user expects to be.  So,
                    # in that case, we issue an extra step to get GDB and the
                    # COBOL source code line in sync.

                    if GV_ModuleInformation.perform_commands[key].find(':') == -1:
                        gdb.execute("step", True, False)

                else:
                    gdb.execute("next", True, False)
                ncount -= 1
        # pass user errors unchanged, for example "No stack."
        except gdb.GdbError:
            raise
        except:
            traceback.print_exc()


class CFrameWorker:
    """Does all of the gdb work related to frames, especially for CUp() and CDown()"""

    def __init__(self):
        if GV_noisy_registration:
            print('Registering commands cup, cdown, cup-silently, cdown-silently, cfinish, until-cobol, finish-module, finish-out-of-line-perform, local-backtrace, list-section')
        CUp()
        CDown()
        CUpS()
        CDownS()
        CUntilCobol()
        CFinish()
        CFinishModule()
        CFinishOutOfLinePerform()
        CLocalBacktrace()
        CListSection()
        # Note: frame filter support in python API only with GDB 7.7+
        if GV_GlobalVariables.gdb_version > 70700:
            if GV_noisy_registration:
                print('Registering frame-filters COBOL-FrameFilter (disabled), COBOL-Only-FrameFilter (disabled)')
            CobolOnlyFrameFilter()
            CobolFrameFilter()
            if GV_noisy_registration:
                print('Registering command cbacktrace')
            CBacktrace()
            # set aliases, ignore errors (most likely because already defined)
            try:
                gdb.execute("alias -a cbt = cbacktrace", False, True)
            except gdb.error:
                pass
            try:
                gdb.execute("alias cwhere = cbacktrace", False, True)
            except gdb.error:
                pass

    @staticmethod
    def is_cobol_frame(frame):
        """Return a Tuple as follows: COBOL-Frame, inner COBOL-Frame (actual COBOL code)"""
        if not isinstance(frame, gdb.Frame):
            raise gdb.error("bad argument passed, frame is " + str(type(frame)))

        if not frame.is_valid():
            return False, False
        try:
            block = frame.block()
        except:
            # no debugging symbols, just skip
            return False, False

        # available in all cobc generated functions (static variable) - check if it is reachable
        # and at least in the same object file as the frame [TODO: limit lookup to _local_ variables]
        symbol = gdb.lookup_symbol("cob_module_path", block)[0]
        if not (symbol and str(symbol.type) == "const char *" and symbol.symtab.objfile == frame.find_sal().symtab.objfile):
            return False, False

        # check for COBOL "inner" frame
        symbol = gdb.lookup_symbol("module", block)[0]
        if symbol and str(symbol.type) == "cob_module *":
            # quite likely an "inner" COBOL frame
            return True, True

        # quite likely an "outer" COBOL frame
        return True, False

    @staticmethod
    def is_cobol_inner_frame(frame):
        return CFrameWorker.is_cobol_frame(frame)[1]

    @staticmethod
    def Frame(up, number):

        try:
            frame = gdb.selected_frame()
        except gdb.error as ex:
            # Depending on the scenario commonly raises one of "No stack.", "No frame currently selected." or others
            ConditionalRaise(str(ex))
            return

        while True:
            if number != 0:
                if up:
                    frame = frame.older()
                    if frame is None:
                        return None
                else:
                    old_frame = frame
                    frame = frame.newer()
                    if frame is None:
                        # on the way down we may see an outer COBOL frame last, then take that
                        if old_frame != gdb.selected_frame() and CFrameWorker.is_cobol_frame(old_frame)[0]:
                            return old_frame
                        return None

            else:
                number = -1

            if not CFrameWorker.is_cobol_inner_frame(frame):
                continue

            # quite likely a COBOL frame
            if number <= 0:
                return frame
            number = number - 1

    @staticmethod
    def nearest_cobol_frame():
        try:
            frame = CFrameWorker.Frame(True, 0)
            if frame is None:
                frame = CFrameWorker.Frame(False, 0)
        except gdb.GdbError:
            frame = None
        return frame

    @staticmethod
    def has_cobol_frame():
        if CFrameWorker.nearest_cobol_frame() is not None:
            return True

        return False

    @staticmethod
    def Go(argument, up, silent):

        number = GetNumericArgument(argument)
        frame = CFrameWorker.Frame(up, number)
        if frame is None:
            # note: we raise here to prevent hookpost-defines to be triggered
            if up:
                ConditionalRaise("Initial COBOL frame selected; you cannot go up.")
            else:
                ConditionalRaise("Bottom (innermost) COBOL frame selected; you cannot go down.")
            return

        if silent:
            # we need to mimic up-silently / down-silently here:
            # so only want the frame to be selected without being user-visible in the console
            # and without letting GDB sending out a mi-event for a frame-selection
            frame.select()
        else:
            # let GDB do whatever it does for frame switching / showing [case: cup 0]
            level = str(CFrameWorker.get_frame_level(frame))
            gdb.execute("frame " + level, True, False)

    @staticmethod
    def get_frame_level(frame):
        if GV_GlobalVariables.gdb_version >= 110000:
            return frame.level()

        level = 0
        frame_counter = gdb.newest_frame()
        while frame_counter != frame:
            frame_counter = frame_counter.older()
            level = level + 1
        return level


class CUp (gdb.Command):
    """Select and print COBOL stack frame that called this one.
An argument says how many frames up to go, the special value 0
may be used to also stop if the current frame is a COBOL frame."""
    def __init__(self):
        super(CUp, self).__init__("cup", gdb.COMMAND_STACK)

    def invoke(self, argument, from_tty):
        CFrameWorker.Go(argument, True, False)


class CDown (gdb.Command):
    """Select and print COBOL stack frame called by this one.
An argument says how many frames down to go, the special value 0
may be used to also stop if the current frame is a COBOL frame."""
    def __init__(self):
        super(CDown, self).__init__("cdown", gdb.COMMAND_STACK)

    def invoke(self, argument, from_tty):
        CFrameWorker.Go(argument, False, False)


class CUpS (gdb.Command):
    """Select COBOL stack frame that called this one silenthly.
An argument says how many frames up to go, the special value 0
may be used to also stop if the current frame is a COBOL frame."""
    def __init__(self):
        super(CUpS, self).__init__("cup-silently", gdb.COMMAND_STACK)

    def invoke(self, argument, from_tty):
        CFrameWorker.Go(argument, True, True)


class CDownS (gdb.Command):
    """Select COBOL stack frame called by this one silently.
An argument says how many frames down to go, the special value 0
may be used to also stop if the current frame is a COBOL frame."""
    def __init__(self):
        super(CDownS, self).__init__("cdown-silently", gdb.COMMAND_STACK)

    def invoke(self, argument, from_tty):
        CFrameWorker.Go(argument, False, True)


class CFinish(gdb.Command):
    """Execute until selected stack frame's current "logical" COBOL frame is finished.
Effectively a combination of commands "finish-out-of-line-perform" and "finish-module"."""

    def __init__(self):
        super(CFinish, self).__init__("cfinish", gdb.COMMAND_RUNNING)

    def invoke(self, argument, from_tty):
        if argument:
            ConditionalRaise('The "cfinish" command does not take any arguments.')
            return
        # sanity check: Do we have any frame at all?
        try:
            frame = gdb.selected_frame()
        except gdb.error:
            ConditionalRaise("No stack.")
            return

        cobol, inner = CFrameWorker.is_cobol_frame(frame)
        if not cobol:
            ConditionalRaise("cfinish only applies to COBOL")
            return

        if inner:
            out_of_line_perform = gdb.parse_and_eval("frame_ptr->perform_through")
            if out_of_line_perform:
                gdb.execute("until *frame_ptr->return_address_ptr", False, False)
                return
            frame = frame.older()
            frame.select()

        gdb.execute("finish", False, False)


class CFinishOutOfLinePerform(gdb.Command):
    """Execute until selected stack frame's current internal PERFORM returns.
Applies to any out-of-line PERFORM."""

    def __init__(self):
        super(CFinishOutOfLinePerform, self).__init__("finish-out-of-line-perform", gdb.COMMAND_RUNNING)

    def invoke(self, argument, from_tty):
        if argument:
            ConditionalRaise('The "finish-out-of-line-perform" command does not take any arguments.')
            return
        # sanity check: Do we have any frame at all?
        try:
            frame = gdb.selected_frame()
        except gdb.error as ex:
            # Depending on the scenario commonly raises one of "No stack.", "No frame currently selected." or others
            ConditionalRaise(str(ex))
            return

        if not CFrameWorker.is_cobol_inner_frame(frame):
            ConditionalRaise("finish-out-of-line-perform only applies to COBOL")
            return

        out_of_line_perform = gdb.parse_and_eval("frame_ptr->perform_through")
        if not out_of_line_perform:
            ConditionalRaise("Already in the initial part of the COBOL module, no out-of-line PERFORM to finish.")
            return
        gdb.execute("until *frame_ptr->return_address_ptr", False, False)

class CFinishModule(gdb.Command):
    """Execute until current COBOL module exits."""

    def __init__(self):
        super(CFinishModule, self).__init__("finish-module", gdb.COMMAND_RUNNING)

    def invoke(self, argument, from_tty):
        if argument:
            ConditionalRaise('The "finish-module" command does not take any arguments.')
            return
        # sanity check: Do we have any frame at all?
        try:
            frame = gdb.selected_frame()
        except gdb.error as ex:
            # Depending on the scenario commonly raises one of "No stack.", "No frame currently selected." or others
            ConditionalRaise(str(ex))
            return

        cobol, inner = CFrameWorker.is_cobol_frame(frame)
        if not cobol:
            ConditionalRaise("finish-module only applies to COBOL")
            return

        if inner:
            frame = frame.older()
            frame.select()

        gdb.execute("finish", False, False)


class CUntilCobol(gdb.Command):
    """Execute until most current COBOL module"""

    def __init__(self):
        super(CUntilCobol, self).__init__("until-cobol", gdb.COMMAND_RUNNING)

    def invoke(self, argument, from_tty):
        if argument:
            ConditionalRaise('The "until-cobol" command does not take any arguments.')
            return
        # sanity check: "Do we have any frame at all?" is already done in CFrameWorker)
        #               the only reason also done here is the use of ConditionalRaise
        try:
            frame = gdb.selected_frame()
        except gdb.error as ex:
            # Depending on the scenario commonly raises one of "No stack.", "No frame currently selected." or others
            ConditionalRaise(str(ex))
            return

        cob_frame = CFrameWorker.Frame(True, 0)

        if CFrameWorker.is_cobol_frame(frame)[0] or cob_frame is None:
            ConditionalRaise("until-cobol only applies to non-COBOL called by a COBOL frame.")
            return

        cob_frame.newer().select()
        gdb.execute("finish", False, False)


class CListSection(gdb.Command):
    """Lists a COBOL section."""

    def __init__(self):
        super(CListSection, self).__init__("list-section", gdb.COMMAND_FILES)
        self.last_arg = None

    def complete(self, text, word):
        # TODO: complete section names
        return gdb.COMPLETE_NONE

    def invoke(self, argument, from_tty):

        if not argument:
            ConditionalRaise('Argument required.')
            return

        # Check for re-execution by last argument and just go on "list"'ing as with other commands:
        if self.last_arg == argument:
            self.last_arg = None
            gdb.execute("list", True, False)
            return
        self.last_arg = argument

        try:
            gdb.execute("list " + get_section_spec(argument), False, False)
        except gdb.error:
            args = argument.split(':')
            if len(args) > 1:
                module = args[0]  # may also be empty
                section = args[1].upper()
            else:
                module = ""
                section = args[0].upper()
            if not module:
                ConditionalRaise('Section {0} not found in current frame.'.format(section))
            else:
                ConditionalRaise('Section {0} not found in module {1}.'.format(section, module))
            return


class CLocalBacktrace(gdb.Command):
    """Output the "local" backtrace of the selected COBOL module."""

    def __init__(self):
        super(CLocalBacktrace, self).__init__("local-backtrace", gdb.COMMAND_STACK)
        # match everything between modern free format: "495  TESTME section." and
        # historic fixed-form with sequence number segment  "495 000495 TESTME SECTION 00 ."
        # also cater for PROCEDURE DIVISION in case we don't have a SECTION
        self.section_pattern = re.compile(r"^\d*\s+\d*\s+(.*)\s+(SECTION|DIVISION)\s*(\d*\.|USING)", re.IGNORECASE)
        self.section_pattern_gdb = r"^\d*\s\+.*\s\+\([sS][eE][cC][tT][iI][oO][nN]\s*\d*\.\|\[dD][iI][vV][iI][sS][iI][oO][nN]\s*\(\.\|[uU][sS][iI][nN][gG]\)\)"
        # set alias, ignore errors (most likely because already defined)
        try:
            gdb.execute("alias -a clbt = local-backtrace", False, True)
        except gdb.error:
            pass

    def get_section_for_sal(self, sal):
        # note: the listsize was temporarily set to 1, so we only get one source line here,
        # possibly with an additional "header" line

        # set the list position to the specified address (may already be a section header)
        try:
            output = gdb.execute("list *" + hex(sal.pc), False, True)
            lines = output.split('\n', 2)
            if len(lines) > 2:
                line = lines[1]
            else:
                line = lines[0]
        except gdb.error:
            # depending on GDB version may error if no match found
            return "unknown"
        except UnicodeDecodeError as ex:
            # the former may raise an UTF8 error but if this is the case then hopyfully in an inline comment - get content before:
            line = ex.object[:ex.start]

        try:
            while True:

                if line == "Expression not found":
                    break
                match = re.search(self.section_pattern, line)
                if match:
                    retval = match.group(1) + " " + match.group(2)
                    if retval.upper() == "PROCEDURE DIVISION":
                        retval = "<MAIN SECTION>"
                    return retval

                try:
                    line = gdb.execute("reverse-search " + self.section_pattern_gdb, False, True).split('\n', 1)[0]
                except gdb.error:
                    # depending on GDB version may error if no match found
                    return "unknown"
                except UnicodeDecodeError as ex:
                    # the former may raise an UTF8 error but if this is the case then hopyfully in an inline comment - get content before:
                    line = ex.object[:ex.start]
        # leave the loop upon request
        except KeyboardInterrupt:
            raise
        # we expect an error if the current source does not contain a section name
        # ... or we did not catched it
        except gdb.error:
            pass
        except Exception:
            traceback.print_exc()

        return "unknown"

    def invoke(self, argument, from_tty):
        if argument:
            number = GetNumericArgument(argument, Negative=True, Raise=False) # bt ignores bad arguments
        else:
            number = None

        # sanity check: Do we have any frame at all?
        try:
            frame = gdb.selected_frame()
        except gdb.error as ex:
            # Depending on the scenario commonly raises one of "No stack.", "No frame currently selected." or others
            ConditionalRaise(str(ex))
            return

        if not CFrameWorker.is_cobol_inner_frame(frame):
            ConditionalRaise("local-backtrace only applies to COBOL")
            return

        frame_ptr_txt = None
        level = CFrameWorker.get_frame_level(frame)

        sanitize_place = True
        sal = None

        listsize = gdb.parameter("listsize")
        gdb.execute("set listsize 1", False, False)
        try:
            # if requested to only show last entries we need to know how much we have
            if number is not None and number < 0:
                steps_max = 0
                while sanitize_place:
                    steps_max += 1
                    frame_ptr_txt = "(frame_ptr - {0})".format(steps_max)
                    sanitize_place = gdb.parse_and_eval(frame_ptr_txt + "->perform_through")
                steps_max += 1  # one too far
                steps_taken = steps_max + number
                number = None
                sal = True
                sanitize_place = True

            while sanitize_place:
                if number is not None and number == 0:
                    break

                # the only way in GC 3.1 to get the section name is to query manually,
                # done below (actually works quite fine)
                name = None
                if not sal:
                    steps_taken = 0
                    sal = frame.find_sal()
                    name = self.get_section_for_sal(sal)
                else:
                    frame_ptr_txt = "(frame_ptr - {0})".format(steps_taken)
                    steps_taken += 1
                    sanitize_place = gdb.parse_and_eval(frame_ptr_txt + "->perform_through")
                    if not sanitize_place:
                        sal = gdb.decode_line(str(frame.function()))[1][0]
                        name = str(frame.older().function())
                    else:
                        sal = gdb.decode_line("*(" + frame_ptr_txt + "->return_address_ptr)")[1][0]
                        name = self.get_section_for_sal(sal)
                print("#{0}.{1}  {2} at {3}:{4}".format(level, steps_taken, name, sal.symtab, sal.line))
                if number is not None:
                    number -= 1
            if sanitize_place:
                print("(More stack frames follow...)")

        # we just quit the loop as requested
        # pass user errors unchanged
        except (KeyboardInterrupt, gdb.GdbError):
            raise
        # that exception is unexpected, but we never know...
        except Exception:
            traceback.print_exc()
        # don't leave without cleanup...
        finally:
            if listsize is None:
                gdb.execute("set listsize unlimited", False, False)
            else:
                gdb.execute("set listsize {0}".format(listsize), False, False)


class CBacktrace (gdb.Command):
    """Print backtrace of all COBOL stack frames (common backtrace, filtered).
For detailed usage see 'help backtrace'."""
    def __init__(self):
        super(CBacktrace, self).__init__("cbacktrace", gdb.COMMAND_STACK)

    def invoke(self, arguments, from_tty):
        remove_filter = gdb.frame_filters["COBOL-Only-FrameFilter"]
        old_enable = remove_filter.enabled
        remove_filter.enabled = True
        try:
            command = "backtrace " + ' '.join(arguments).replace("- ", "-")
            gdb.execute(command)
        except gdb.error as ex:
            raise gdb.GdbError(ex)
        finally:
            remove_filter.enabled = old_enable


class ElidingDecorator(gdb.FrameDecorator.FrameDecorator):
    def __init__(self, frame, elided_frames=[]):
        super(ElidingDecorator, self).__init__(frame)
        self.frame = frame
        self.elided_frames = elided_frames

    def elided(self):
        return iter(self.elided_frames)


class ElideNonCobolIterator():
    def __init__(self, ii):
        self.input_iterator = ii
        self.next_frame = None

    def __iter__(self):
        return self

    # python3 iteration
    def __next__(self):
        if self.next_frame is None:
            frame = next(self.input_iterator)
        else:
            frame = self.next_frame
            self.next_frame = None

        real_frame = frame.inferior_frame()
        if CFrameWorker.is_cobol_inner_frame(real_frame):
            try:
                next_frame = next(self.input_iterator)
            except StopIteration:
                return frame

            # check if we have an inner frame next - likely because of
            # another filter with higher priority
            if CFrameWorker.is_cobol_inner_frame(next_frame.inferior_frame()):
                self.next_frame = next_frame
                return frame
            # next frame is, as expected, a contained frame
            return ElidingDecorator(frame, [next_frame])

        # pass non-COBOL-inner-frames as-is
        return frame

    # python2 iteration
    def next(self):
        return self.__next__()


class CobolFrameFilter():
    def __init__(self):
        self.name = "COBOL-FrameFilter"
        self.priority = 99
        # self.enabled = True   disabled until #58 is solved
        self.enabled = False
        gdb.frame_filters[self.name] = self

    def filter(self, frame_iter):
        return ElideNonCobolIterator(frame_iter)


class CobolDecoration(gdb.FrameDecorator.FrameDecorator):
    """Decorades the name and address (both optional, the later currently removed isntead of decorated),
the frame args (currently unfinished and therefore replaced by NUMBER-OF-CALL-PARAMETERS register)
and the local args (currently removed)"""
    def __init__(self, filter_frame, frame, decorate_name_and_address=True):
        super(CobolDecoration, self).__init__(filter_frame)
        self.frame = frame
        self.decorate_name_and_address = decorate_name_and_address

    def function(self):
        # actual function name of the stack frame
        if not self.decorate_name_and_address:
            return str(self.frame.function())
        # or the COBOL module name
        cob_module = self.frame.read_var("module")
        return cob_module['module_name'].string()

    def address(self):
        # actual address value of the stack frame
        if not self.decorate_name_and_address:
            return str(self.frame.pc())
        # the ADDRESS OF ENTRY "prog" which is 'meant' here is the address of the wrapper,
        # which is the calling frame's address
        # TODO: check how that is done for nested programs
        # return self.frame.older().pc()
        # Nearly never useful for a COBOL programmer, so until we know we want that: just remove it from the frame decoration
        return None

    def frame_args(self):
        amount_args_passed = self.frame.read_var("cob_call_params")
        amount_args_left = int(amount_args_passed)
        if amount_args_left == 0:
            return None
        args = []
        # TODO: get the COBOL name and start of the values here
        #       that are in current active USING
        # GV_ModuleState.EstablishModuleState(True)
        # # Iterate over all symbols in a block.  Only add
        #  # symbols that are arguments and also drop the "entry one".
        #  # --> coming from   "const int entry, cob_u8_t *b_903, cob_u8_t *b_10225"
        #  try:
        #      block = self.frame.block()
        #  except:
        #      return None
        #  for sym in block:
        #     if not sym.is_argument or sym.name == "entry":
        #         continue
        #     if not amount_args_left:
        #         break
        #     amount_args_left -= 1
        #     # check internal list of LINKAGE items, which is not a redefine
        #     # and has that base address b_903 / b_10225
        #     value = self.frame.read_var(sym).dereference().address
        #     payload = GV_ModuleState.get_payload_from_base_address(value)
        #     args.append(CobolVariable)
        # ... until that work is done create a synthetic variable passing the number of arguments
        payload = CobolVariable()
        payload.set_constant_attributes("NUMBER-OF-CALL-PARAMETERS", amount_args_passed)
        args.append(payload)
        return args

    def frame_locals(self):
        # even simple COBOL programs consist of a huge amount of data
        # in theory we could issue internally a "cprint *"" (reduced to the level 01 / 77 entries)
        # here and return the list, but this definitely should only be done depending on a parameter
        # that is only related to cobcd.py
        return None


class RemoveNonCobolIterator():
    def __init__(self, ii):
        self.input_iterator = ii
        self.first_iteration = True

    def __iter__(self):
        return self

    # python3 iteration
    def __next__(self):
        frame = next(self.input_iterator)

        real_frame = frame.inferior_frame()
        cobol, inner = CFrameWorker.is_cobol_frame(real_frame)

        if inner:
            self.first_iteration = False
            # yay, an inner COBOL frame...
            return CobolDecoration(frame, real_frame)
        if cobol and self.first_iteration:
            # we have nothing returned yet, but an outer frame --> return as-is
            return frame

        return self.__next__()

    # python2 iteration
    def next(self):
        return self.__next__()


class CobolOnlyFrameFilter():
    def __init__(self):
        self.name = "COBOL-Only-FrameFilter"
        self.priority = 100
        self.enabled = False
        gdb.frame_filters[self.name] = self

    def filter(self, frame_iter):
        return RemoveNonCobolIterator(frame_iter)


def exit_handler(event):
    #
    for cb in CBLDBG_CBREAK_WORKER.cobol_breaks:
        # breakpoints included in our list may be invalid
        # commonly by being deleted by the user, or being temporary
        if not cb.is_valid():
            CBLDBG_CBREAK_WORKER.cobol_breaks.remove(cb)
            continue
        if isinstance(cb, CobolWatchBreakpoint):
            cb.out_of_scope()


def stop_handler(event):
    # When the inferior has been running, and is now stopped, we
    # need to flag all COBOL variables as IsDirty
    GV_ModuleInformation.FlagAllAsDirty()


def reset_handler(event):
    # In case of exit/re-attach we need a clean state
    init_me()

gdb.events.exited.connect(reset_handler)
try:
    gdb.events.inferior_deleted.connect(reset_handler)
except AttributeError:
    pass # that's a GDB 11+ event registry, no problem if not available

def init_me():
    # In case of exit/re-attach we need a clean state
    global GV_ModuleInformation
    GV_ModuleInformation = ModuleInformation()
    GV_ModuleState.reset()

GV_ModuleState = ModuleState()
init_me()


class CmdAutoStep (gdb.Command):
    """Auto-Step through the code until something happens or manually interrupted.
An argument says how fast auto stepping is done (see parameter "auto-step")."""
    def __init__(self):
        if GV_noisy_registration:
            print('Registering command and parameter auto-step')
        super(CmdAutoStep, self).__init__("auto-step", gdb.COMMAND_RUNNING)
        self.defaultSpeed = ParameterAutoStep()
        gdb.events.stop.connect(stop_handler_auto_step)
        gdb.events.exited.connect(stop_handler_auto_step)
        try:
            gdb.events.inferior_deleted.connect(stop_handler_auto_step)
        except AttributeError:
            pass # that's a GDB 11+ event registry, no problem if not available

    def invoke(self, argument, from_tty):
        # sanity check - are we even active, prevents a spurious "no registers" exception
        try:
            gdb.newest_frame()
        except gdb.error:
            raise gdb.GdbError("The program is not being run.")

        # calculate sleep time
        if argument:
            number = ParameterAutoStep.validate(argument) # raises an error if not valid
        else:
            number = self.defaultSpeed.value
        if number:
            sleep_time = 3.0 / (1.4 ** number)
        else:
            sleep_time = 0

        # activate GDB scrolling, otherwise we'd auto-step only one page
        pagination = gdb.parameter("pagination")
        if pagination:
            gdb.execute("set pagination off", False, False)

        # recognize the kind of stop via stop_handler_auto_step
        global got_complex_stop_event
        got_complex_stop_event = False

        # actual auto-stepping
        try:
            # note: the end condition may get in "late" as it is handled via asynchronous events,
            #       especially when running at a high rate
            while not got_complex_stop_event:
                gdb.execute("step")
                time.sleep(sleep_time)
        # we just quit the loop as requested
        # pass keyboard and user errors unchanged
        except (KeyboardInterrupt, gdb.GdbError):
            raise
        # that exception is unexpected, but we never know...
        except Exception:
            traceback.print_exc()
        # never leave without cleanup...
        finally:
            if pagination:
                gdb.execute("set pagination on", False, False)


class ParameterAutoStep (gdb.Parameter):
    """auto-step default speed (0-19, default 5)"""
    def __init__(self):
        self.set_doc = """Set speed for "auto-step", internally used to calculate sleep time between "step"s.
set "auto-step 0" causes there to be no sleeping."""
        self.show_doc = "Speed value for auto-step."
        super(ParameterAutoStep, self).__init__("auto-step", gdb.COMMAND_RUNNING, gdb.PARAM_UINTEGER)
        self.value = 5
        self.backup = self.value

    def get_set_string (self):
        try:
            self.value = int(ParameterAutoStep.validate(self.value))
        except gdb.GdbError:
            self.value = int(self.backup)
            raise
        self.backup = self.value
        return ""

    @staticmethod
    def validate (argument):
        """validation for auto-step speed"""
        try:
            speed = int(argument)
            if speed < 0 or speed > 19:
                raise ValueError()
        except (TypeError, ValueError):
            raise gdb.GdbError("speed-argument must be an integer between 1 and 19, or 0")
        return speed

def stop_handler_auto_step(event):
    # check the _type_ of stop, _not_ the instance:
    # the following is the common one after step/next, a more complex one would be a subclass
    # (for example breakpoint or signal) which we say "that is complex" -> stop auto step
    # Note that we can't set the value directly because we never should set it to False here
    # when it is already True, as we may get flooded by a lot of events, so we otherwise
    # ending with scenarios like  False, False, False, True, False  otherwise
    global got_complex_stop_event
    if not type(event) is gdb.StopEvent:
        got_complex_stop_event = True


gdb.events.exited.connect(exit_handler)
gdb.events.stop.connect(stop_handler)
try:
    gdb.events.inferior_deleted.connect(stop_handler)
except AttributeError:
    pass # that's a GDB 11+ event registry, no problem if not available

CFrameWorker()
CBLDBG_CBREAK_WORKER = CBreakWorker()
CPrint()
CWatch()
CBreak()
CLoad()
CNext()
CmdAutoStep()
CStart()

if not GV_noisy_registration:
    print("Registering the CBL-GDB debugger {0}.  Help is available for".format(COBCD_VERSION))
    print("   cprint cwatch cbreak ctbreak ccondition add-symbol-file-cobol cstart cnext")
    print("   cup cup-silently cdown cdown-silently cfinish")
    print("   finish-out-of-line-perform finish-module until-cobol list-section local-backtrace cbacktrace auto-step")
