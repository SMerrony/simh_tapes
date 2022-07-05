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
with Ada.Exceptions;
with Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Text_IO;             use Ada.Text_IO;

with Interfaces;              use Interfaces;

with Parse_Args;              use Parse_Args;

with Simh_Tapes;              use Simh_Tapes;

procedure Simhtapetool is
   AP : Argument_Parser;
begin
   AP.Add_Option (Make_Boolean_Option (False), "help",       'h', Usage => "Display this help text");
   AP.Add_Option (Make_String_Option (""),     "create",     'c', Usage => "Create a new SimH Magnetic Tape Image file");
   AP.Add_Option (Make_String_Option (""),     "definition", 'd', Usage => "CSV Definition for a new image");
   AP.Add_Option (Make_String_Option (""),     "extract",    'e', Usage => "Extract each file from the tape image");
   AP.Add_Option (Make_String_Option (""),     "scan",       's', Usage => "Scan SimH Magnetic Tape Image file for correctness, listing files");
   --  AP.Add_Option (Make_Boolean_Option (False), "verbose",    'v', Usage => "Be more verbose");

   AP.Parse_Command_Line;
   if not AP.Parse_Success or else AP.Boolean_Value ("help") then
      AP.Usage;
   end if;

   declare
      Extract_Filename     : constant String := AP.String_Value ("extract");
      Scan_Filename        : constant String := AP.String_Value ("scan");
      Create_Filename      : constant String := AP.String_Value ("create");
      Definition_Filename  : constant String := AP.String_Value ("definition");
   begin
      if Scan_Filename /= "" then
         --  SCAN --
         Put_Line ("Scanning SimH magnetic tape image: " & Scan_Filename);
         Put_Line (Scan_Image (Scan_Filename));
      elsif Extract_Filename /= "" then
         --  EXTRACT  --
         Put_Line ("Extracting files from tape image: " & Extract_Filename);
         Dump_All_Files (Extract_Filename);
         Put_Line ("Extraction complete");
      elsif Create_Filename /= "" then
         --  CREATE  --
         if Definition_Filename = "" then
            Put_Line ("ERROR: You must specify a CSV file defining the new image with '--definition' or '-d'");
         else
            declare
               CSV_File          : File_Type;         --  Contains the description of the tape image
               CSV_Line          : Unbounded_String;
               Tape_Image_File   : Ada.Streams.Stream_IO.File_Type;  --  The new tape image we are creating
               Tape_Image_Stream : Ada.Streams.Stream_IO.Stream_Access;
               Comma_Pos         : Natural;
               Src_Filename      : Unbounded_String;  --  Each source file to be written to the tape image
               Src_Block_Size    : Positive;

               Invalid_Definition : exception;
            begin
               --  Open the CSV description for the new image
               Open (CSV_File, In_File, Definition_Filename);
               if Ada.Directories.Exists (Create_Filename) then
                  raise File_Already_Exists with "File " & Create_Filename & " already exists";
               end if;
               --  Create/Open the new tape image file
               Ada.Streams.Stream_IO.Create (File => Tape_Image_File, Name => Create_Filename);
               Tape_Image_Stream := Ada.Streams.Stream_IO.Stream (Tape_Image_File);

               while not End_Of_File (CSV_File) loop
                  CSV_Line := To_Unbounded_String (Get_Line (CSV_File));
                  Comma_Pos := Index (CSV_Line, ",");
                  if Comma_Pos = 0 then
                     raise Invalid_Definition with "No comma in CSV definition line";
                  end if;
                  Src_Filename := Unbounded_Slice (CSV_Line, 1, Comma_Pos - 1);
                  Src_Block_Size := Integer'Value (Slice (CSV_Line, Comma_Pos + 1, Length (CSV_Line)));
                  Put_Line ("INFO: Processing file: " & To_String (Src_Filename) &
                            " with block size:" & Src_Block_Size'Image);
                  declare
                     Src_File   : Ada.Streams.Stream_IO.File_Type;
                     Src_Stream : Ada.Streams.Stream_IO.Stream_Access;
                     Bytes_Read : Natural := 0;
                     Block      : Mt_Rec (1 .. Src_Block_Size) := (others => 0);
                  begin
                     Ada.Streams.Stream_IO.Open (Src_File, Ada.Streams.Stream_IO.In_File, To_String (Src_Filename));
                     Src_Stream := Ada.Streams.Stream_IO.Stream (Src_File);
                     while not Ada.Streams.Stream_IO.End_Of_File (Src_File) loop
                        for B in 1 .. Src_Block_Size loop
                           if Ada.Streams.Stream_IO.End_Of_File (Src_File) then
                              Block (B) := 0;
                           else
                              Unsigned_8'Read (Src_Stream, Block (B));
                              Bytes_Read := Bytes_Read + 1;
                           end if;
                        end loop;
                        if Bytes_Read > 0 then
                           Write_Meta_Data   (Tape_Image_Stream, Unsigned_32 (Bytes_Read)); --  Block header
                           Write_Record_Data (Tape_Image_Stream, Block (1 .. Bytes_Read));  --  Block of data
                           Write_Meta_Data   (Tape_Image_Stream, Unsigned_32 (Bytes_Read)); --  Block trailer
                           Bytes_Read := 0;
                        end if;
                     end loop;
                     Ada.Streams.Stream_IO.Close (Src_File);
                     Write_Meta_Data (Tape_Image_Stream, Mtr_Tmk);
                  exception
                     when Name_Error =>
                        Put_Line ("ERROR: Cannot open file to write to tape image: " & To_String (Src_Filename));
                        raise;
                  end;
               end loop;
               Close (CSV_File);
               Write_Meta_Data (Tape_Image_Stream, Mtr_EOM);
               Ada.Streams.Stream_IO.Close (Tape_Image_File);
               Put_Line ("INFO: Magnetic tape image created");
            exception
               when Name_Error =>
                  Put_Line ("ERROR: Cannot open/use CSV definition file");
            end;
         end if;
      else
         AP.Usage;
      end if;

   end;

   exception
      when E : others =>
         Put_Line ("ERROR: " & Ada.Exceptions.Exception_Information (E));

end Simhtapetool;