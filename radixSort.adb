-- Program: Radix Sort
-- Assignment 4 for cis3190 at The University of Guelph
-- Written by Ian McKechnie (imckechn | 1051662)
-- Written on April 5th, 2021

with ada.Text_IO; use Ada.Text_IO;
with ada.numerics.discrete_random;
with Ada.Numerics.Generic_Elementary_Functions;


procedure Radix is
    numbers : float(1..5000);
    filename, sline : unbounded_string;
    nameOK : boolean := false;
    infp : file_type;
    i : integer;



    --      PROCEDURES

    -- The file reader prodecure
    -- Adapted from code written by Michael Wirth at https://craftofcoding.wordpress.com/2018/04/04/coding-ada-reading-lines-from-files/
    -- Taken on April 5th, 2021  
    procedure fileReader( numbers : float(1..5000); filename : string(1..20) ) is
    begin   
        put_line("What is the input file name (include .txt)");
        loop
        exit when nameOK;
            get_line(filename);
            nameOK := exists(to_string(filename));
        end loop;

        open(infp, in_file, to_string(filename));

        i = 1;
        loop
            exit when end_of_file(infp);
            -- Process each line from the file
            get_line(infp,sline);
            numbers(i) = Real'value(sline);
            i = i + 1;
        end loop;
    end fileReader;


    -- Get the highest value in a set of numbers
    procedure max(numbers: real(1..5000); highest: real) is
    i : integer;
    begin
        i = 1;
        highest = number(i);

        loop
            exit when i = 5000;
            if (highest < numbers(i)) then 
                highest = number(i);
            end if
        end loop;
    end max;

    --Radix sort helper procedure
    procedure countingSort(nums : real(1..5000);  digit : integer; base: real) is
        i, j : integer;
        digit_of_Ai : real;
        c : real(1..Integer'Value(base));
        b : real(1..5000);

    begin
        i = 1;

        loop
            exit when i = 5000;
            digit_of_Ai = (nums(i)/base ** digit) mod base;
            

    end countingSort;


    -- Main radix sort procedure
    procedure radixSort(numbers: real(1..5000)) is
    output: real(1..5000);
    highest, numDigits: real;
    digits, i : integer;

    begin
        max(numbers, highest);
        display("Max = " + max);

        output = numbers;
        numDigits = 4;
        digits = Float'Floor(Log(highest, numDigits));

        i = 0;
        loop
            exit when i = 5000;
            countingSort(output, i, numDigits);
        end loop;

        i = 0;
        loop
            exit when i = 5000;
            display("numbers(i) = " + output(i);
        end loop;

        numbers = output;

    end radixSort;


    --      BEGIN PROGRAM
begin 
    -- Read in data from the file
    fileReader(filename, numbers);

    -- Perform Radix Sort
    radixSort(numbers);


    -- Output data to a file


    -- Close the files and quit

end Radix
