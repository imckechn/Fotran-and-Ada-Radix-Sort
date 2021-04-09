-- Program: Radix Sort
-- Assignment 4 for cis3190 at The University of Guelph
-- Written by Ian McKechnie (imckechn | 1051662)
-- Written on April 5th, 2021

with ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;

procedure newSort is

    type numberArray is array(0..5000) of integer;

    numbers : numberArray;
    filename : unbounded_string;
    i : integer;

    --      PROCEDURES

    -- The file reader prodecure
    -- Adapted from code written by Michael Wirth at https://craftofcoding.wordpress.com/2018/04/04/coding-ada-reading-lines-from-files/
    -- Taken on April 5th, 2021  
    procedure fileReader( numbers : in out integer ) is

    begin
        put_line("Test number 2");
    end fileReader;


begin 
    put_line("TEST");

    numbers := (0);

    fileReader(numbers);

end newSort;
