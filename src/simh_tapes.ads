--  Copyright ©2022 Stephen Merrony
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU Affero General Public License as published
--  by the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU Affero General Public License for more details.
--
--  You should have received a copy of the GNU Affero General Public License
--  along with this program.  If not, see <https://www.gnu.org/licenses/>.

with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Interfaces;            use Interfaces;

package Simh_Tapes is

   --  tape image markers
   Mtr_Tmk     : constant Unsigned_32 := 0;
   Mtr_EOM     : constant Unsigned_32 := 16#ffff_ffff#;
   Mtr_Gap     : constant Unsigned_32 := 16#ffff_fffe#;
   Mtr_Max_Len : constant Unsigned_32 := 16#00ff_ffff#;
   Mtr_Erf     : constant Unsigned_32 := 16#8000_0000#;

   --  status codes
   type Mt_Stat is
      (OK, Tmk, Unatt, IOerr, InvRec, InvFmt, BOT, EOM, RecErr, WrOnly);

   type Mt_Rec is array (Natural range <>) of Unsigned_8;

   procedure Read_Meta_Data    (Img_Stream : Stream_Access; Meta_Data : out Unsigned_32);
   --  Read_Meta_Data reads a four byte (one doubleword) header, trailer, or other metadata record
   --  from the supplied tape image file

   procedure Write_Meta_Data   (Img_Stream : Stream_Access; Meta_Data : Unsigned_32);
   --  Write_Meta_Data writes a 4-byte header/trailer or other metadata

   procedure Read_Record_Data  (Img_Stream : Stream_Access; Num_Bytes : Natural; Rec : out Mt_Rec);
   --  Read_Record_Data attempts to read a data record from SimH tape image, fails if wrong number of bytes read
   --  N.B. does not read the header and trailer

   procedure Write_Record_Data (Img_Stream : Stream_Access; Rec : Mt_Rec);
   --  Write_Record_Data writes the actual data - not the header/trailer

   procedure Rewind            (Img_File   : in out File_Type);

   procedure Space_Forward     (Img_Stream : Stream_Access; Num_Recs : Integer; Stat : out Mt_Stat);
   --  Space_Forward advances the virtual tape by the specified amount
   --  N.B. 0 means 1 whole file

   function  Scan_Image        (Img_Filename : String) return String;
   --  Scan_Image - a higher-level function which attempts to read a whole tape image ensuring headers,
   --  record sizes, and trailers match

   procedure Dump_All_Files    (Img_Filename : String);
   --  Dump_All_Files - a higher-level procedure which attempts to dump each file from a tape image
   --  into successively-numbered files in the current directory

   File_Already_Exists,
   Header_Trailer_Mismatch : exception;

private

   procedure Space_Forward_1_Rec (Img_Stream : Stream_Access; Stat : out Mt_Stat);

end Simh_Tapes;
