-- Program: Radix Sort
-- Assignment 4 for cis3190 at The University of Guelph
-- Written by Ian McKechnie (imckechn | 1051662)
-- Written on April 5th, 2021

with ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;

procedure radixSort is
    
    type numberArray is array(0..5000) of integer;

    numbers : numberArray;
    filename : unbounded_string;
    i : integer;

    --      PROCEDURES

    -- The file reader prodecure
    -- Adapted from code written by Michael Wirth at https://craftofcoding.wordpress.com/2018/04/04/coding-ada-reading-lines-from-files/
    -- Taken on April 5th, 2021  
    procedure fileReader( numbers : in out integer ) is
        infp : file_type;
        sline : unbounded_string;
        fname : unbounded_string;
        nameOK : boolean := false;
        i : integer;

    begin   
        put_line("What is the input file name (include .txt)");
        get_line(fname);
    
        open(infp, in_file, to_string(fname));

        i := 1;
        loop
            exit when end_of_file(infp);
            -- Process each line from the file
            get_line(infp,sline);
            --numbers(i) := Integer'Value(sline);
            numbers(i) := sline;
            i := i + 1;
        end loop;
    end fileReader;


    -- Get the highest value in a set of numbers
    procedure max(numbers, highest: in out integer) is
    i : integer;
    begin
        i := 1;
        highest := numbers(i);

        loop
            exit when i = 5000;
            if (highest < numbers(i)) then 
                highest := numbers(i);
            end if;

            i := i + 1;
        end loop;
    end max;

    --Radix sort helper procedure
    procedure countingSort(digit, nums : in out integer) is
        
        output : numberArray := 0;
        i, length : integer;
        count : integer(1..10);

    begin
        i := 0;
        length := 5000;
        
        while i < length loop
            count( (nums(i) /digit) mod 10) := count( (nums(i) /digit) mod 10) + 1;
            i := i + 1;
        end loop;

        i := 1;
        while i < 10 loop
                count(i) :=  count(i-1);
            i := i + 1;
        end loop;

        i := length - 1;
        while i >= 0 loop
            output( count( (nums(i)/digit) mod 10) - 1) := nums(i);
            count( (nums(i)/digit) mod 10):= count( (nums(i)/digit) mod 10) - 1;
            i := i - 1;
        end loop;

        i := 0;
        while i < length loop
            nums(i) := output(i);
            i := i + 1;
        end loop;
            

    end countingSort;


    -- Main radix sort procedure
    procedure radixSort(numbers: in out integer) is
    output: integer(0..5000);
    highest: integer;
    i : integer;

    begin
        max(numbers, highest);
        write("Max = " + highest);

        output := numbers;

        i := 1;
        loop
            exit when highest / i > 0;
            countingSort(output, i);

            i := i * 10;
        end loop;

        i := 0;
        loop
            exit when i = 5000;
            display("numbers(i) = " + output(i));
        end loop;

        numbers := output;

    end radixSort;


    --      BEGIN PROGRAM
begin 
    -- Read in data from the file
    fileReader(numbers);

    -- Perform Radix Sort
    radixSort(numbers);


    -- Output data to a file


    -- Close the files and quit

end radixSort;
