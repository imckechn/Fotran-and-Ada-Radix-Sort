-- Program: Radix Sort
-- Assignment 4 for cis3190 at The University of Guelph
-- Written by Ian McKechnie (imckechn | 1051662)
-- Written on April 5th, 2021

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;

procedure newSort is

    type numberArray is array(0..5000) of integer;

    numbers : numberArray := (others => 0);
    filename : unbounded_string;
    

    --      PROCEDURES

    -- The file reader prodecure
    -- Adapted from code written by Michael Wirth at https://craftofcoding.wordpress.com/2018/04/04/coding-ada-reading-lines-from-files/
    -- Taken on April 5th, 2021  
    procedure fileReader( numbers : in out numberArray ) is
        infp : file_type;
        sline : unbounded_string;
        fname : unbounded_string;
        i, temp : integer;
        

    begin
        put_line ("Enter the filename: ");
        fname := To_Unbounded_String("inputs.txt");
        --get_line(fname);


        open(infp, in_file, to_string(fname));

        i := 0;
        loop
        exit when end_of_file(infp);
            -- Process each line from the file
            get_line(infp,sline);
            put_line(to_string(sline));

            put_line("A");
            temp := Integer'Value( to_string(sline) );
            put_line("B");

            put_line("Number = ");
            put_line(Integer'image(temp));
        
            -- do something with the line of text
        end loop;
    end fileReader;


    procedure Main is
        T : Unbounded_String := To_Unbounded_String ("613");
        I : Integer;
        infp : file_type;
        sline : unbounded_string;
        fname : unbounded_string;
        
    begin
        fname := To_Unbounded_String("inputs.txt");
        --get_line(fname);


        open(infp, in_file, to_string(fname));

        i := 0;
        loop
        exit when end_of_file(infp);
            -- Process each line from the file
            get_line(infp,sline);
            put_line(to_string(sline));

            put_line("A");
            I := Integer'Value (To_String (sline));
            put_line("B");

            put_line("Number = ");
            put_line(Integer'image(I));
        
            -- do something with the line of text
        end loop;

        I :=  Integer'Value (To_String (T));
        Put_Line (I'Image);
    end Main;

begin 
    put_line("Program start");

    put_line("numbers = ");
    --put_line(integer'image(numbers));

    Main;

    
    put_line("Numbers = ");
    --put_line(Integer'Image(numbers(0)));
    --put_line(Integer'Image(numbers(5000)));


end newSort;
