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

with Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Simh_Tapes is

   procedure Read_Meta_Data (Img_Stream : Stream_Access; Meta_Data : out Unsigned_32)
   is
      Tmp_Dw : Unsigned_32;
   begin
      Unsigned_32'Read (Img_Stream, Tmp_Dw);
      Meta_Data := Tmp_Dw;
   end Read_Meta_Data;

   procedure Write_Meta_Data (Img_Stream : Stream_Access; Meta_Data : Unsigned_32) is
   begin
      Unsigned_32'Write (Img_Stream, Meta_Data);
   end Write_Meta_Data;

   procedure Read_Record_Data (Img_Stream : Stream_Access; Num_Bytes : Natural; Rec : out Mt_Rec) is
      Tmp_Rec : Mt_Rec (1 .. Num_Bytes);
      Out_Rec_Ix : Integer := Rec'First;
   begin
      for C in 1 .. Num_Bytes loop
         Unsigned_8'Read (Img_Stream, Tmp_Rec (C));
         Rec (Out_Rec_Ix) := Tmp_Rec (C);
         Out_Rec_Ix := Out_Rec_Ix + 1;
      end loop;
   end Read_Record_Data;

   procedure Write_Record_Data (Img_Stream : Stream_Access; Rec : Mt_Rec) is
   begin
      for C in Rec'Range loop
         Unsigned_8'Write (Img_Stream, Rec (C));
      end loop;
   end Write_Record_Data;

   procedure Rewind (Img_File : in out File_Type) is
   begin
      Set_Index (Img_File, 1);
   end Rewind;

   --  internal function
   procedure Space_Forward_1_Rec (Img_Stream : Stream_Access; Stat : out Mt_Stat) is
      Header, Trailer : Unsigned_32;
   begin
      Read_Meta_Data (Img_Stream, Header);
      if Header = Mtr_Tmk then
         Stat := Tmk;
         return;
      end if;
      --  read and discard 1 record
      declare
         Dummy_Rec : Mt_Rec (1 .. Natural (Header));
      begin
         Read_Record_Data (Img_Stream, Natural (Header), Dummy_Rec);
      end;
      --  check trailer
      Read_Meta_Data (Img_Stream, Trailer);
      if Header /= Trailer then
         Stat := InvRec;
      else
         Stat := OK;
      end if;
   end Space_Forward_1_Rec;

   procedure Space_Forward (Img_Stream : Stream_Access; Num_Recs : Integer; Stat : out Mt_Stat) is
      Simh_Stat    : Mt_Stat := IOerr;
      Sf_Stat      : Mt_Stat;
      Done         : Boolean := False;
      Header, Trailer : Unsigned_32;
      Rec_Cnt      : Integer := Num_Recs;
   begin
      if Num_Recs = 0 then
         --  one whole file
         while not Done loop
            Read_Meta_Data (Img_Stream, Header);
            if Header = Mtr_Tmk then
               Simh_Stat := OK;
               Done := True;
            else
               --  read and discard 1 record
               declare
                  Rec : Mt_Rec (1 .. Natural (Header));
               begin
                  Read_Record_Data (Img_Stream, Natural (Header), Rec);
               end;
               --  check trailer
               Read_Meta_Data (Img_Stream, Trailer);
               if Header /= Trailer then
                  Stat := InvRec;
                  return;
               end if;
            end if;
         end loop;
      else
         --  otherwise word count is a negative number and we space fwd that many records
         while Rec_Cnt /= 0 loop
            Rec_Cnt := Rec_Cnt + 1;
            Space_Forward_1_Rec (Img_Stream, Sf_Stat);
            if Sf_Stat /= OK then
               Stat := Sf_Stat;
               return;
            end if;
         end loop;
      end if;
      Stat := Simh_Stat;
   end Space_Forward;

   function Scan_Image (Img_Filename : String) return String is
      Result     : Unbounded_String;
      Img_File   : File_Type;
      Img_Stream : Stream_Access;
      Header,
      Trailer    : Unsigned_32;
      File_Count : Integer := -1;
      File_Size,
      Mark_Count,
      Record_Num : Integer := 0;
      Dummy_Rec  : Mt_Rec (1 .. 32768);
   begin
      Open (File => Img_File, Mode => In_File, Name => Img_Filename);
      Img_Stream := Stream (Img_File);
      Record_Loop : loop
         Read_Meta_Data (Img_Stream, Header);
         case Header is
            when Mtr_Tmk =>
               if File_Size > 0 then
                  File_Count := File_Count + 1;
                  Result := Result & Character'Val (10) & "File" & Integer'Image (File_Count) &
                            " :" & Integer'Image (File_Size) & " bytes in" &
                            Integer'Image (Record_Num) & " block(s)";
                  File_Size := 0;
                  Record_Num := 0;
               end if;
               Mark_Count := Mark_Count + 1;
               if Mark_Count = 3 then
                  Result := Result & Character'Val (10) & "Triple Mark (old End-of-Tape indicator)";
                  exit Record_Loop;
               end if;

            when Mtr_EOM =>
               Result := Result & Character'Val (10) & "End of Medium";
               exit Record_Loop;

            when Mtr_Gap =>
               Result := Result & Character'Val (10) & "Erase Gap";
               Mark_Count := 0;

            when others =>
               Record_Num := Record_Num + 1;
               Mark_Count := 0;
               Read_Record_Data (Img_Stream, Natural (Header), Dummy_Rec);
               Read_Meta_Data (Img_Stream, Trailer);
               if Header = Trailer then
                  File_Size := File_Size + Integer (Header);
               else
                  Result := Result & Character'Val (10) & "Non-matching trailer found.";
               end if;
         end case;
      end loop Record_Loop;
      Close (Img_File);
      return To_String (Result);
   end Scan_Image;

   procedure Dump_All_Files (Img_Filename : String) is
      Img_File     : File_Type;
      Img_Stream   : Stream_Access;
      Out_Prefix   : constant String := "file";
      Out_Filename : Unbounded_String;
      Out_File     : File_Type;
      Out_Stream   : Stream_Access;
      Header,
      Trailer      : Unsigned_32;
      File_Count   : Integer := 0;
      File_Size,
      Mark_Count,
      Record_Num   : Integer := 0;
      Tape_Record  : Mt_Rec (1 .. 32768);

      procedure Open_File is
      begin
         Out_Filename := To_Unbounded_String (Out_Prefix) &
                         Trim (To_Unbounded_String (Integer'Image (File_Count)), Ada.Strings.Left);
         if Ada.Directories.Exists (To_String (Out_Filename)) then
            raise File_Already_Exists with "File " & To_String (Out_Filename) & " already exists";
         end if;
         Create (File => Out_File, Name => To_String (Out_Filename));
         Out_Stream := Stream (Out_File);
         File_Size  := 0;
         Record_Num := 0;
      end Open_File;

   begin
      Open (File => Img_File, Mode => In_File, Name => Img_Filename);
      Img_Stream := Stream (Img_File);
      Open_File;

      Record_Loop : loop
         Read_Meta_Data (Img_Stream, Header);
         case Header is
            when Mtr_Tmk =>
               if File_Size > 0 then
                  Close (Out_File);
                  File_Count   := File_Count + 1;
                  Open_File;
               end if;
               Mark_Count := Mark_Count + 1;
               exit Record_Loop when Mark_Count = 3;

            when Mtr_EOM =>
               exit Record_Loop;

            when Mtr_Gap =>
               Mark_Count := 0;

            when others =>
               Record_Num := Record_Num + 1;
               Mark_Count := 0;
               Read_Record_Data (Img_Stream => Img_Stream, Num_Bytes => Natural (Header), Rec => Tape_Record);
               Read_Meta_Data   (Img_Stream => Img_Stream, Meta_Data => Trailer);
               if Header = Trailer then
                  for Byte_Ix in 1 .. Positive (Header) loop
                     Unsigned_8'Write (Out_Stream, Tape_Record (Byte_Ix));
                     File_Size := File_Size + 1;
                  end loop;
               else
                  raise Header_Trailer_Mismatch with "Tape file header and trailer do not match";
               end if;

         end case;
      end loop Record_Loop;

      Close (Out_File);
      if File_Size = 0 then
         Ada.Directories.Delete_File (To_String (Out_Filename));
      end if;
      Close (Img_File);
   end Dump_All_Files;

end Simh_Tapes;