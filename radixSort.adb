-- Program: Radix Sort
-- Assignment 4 for cis3190 at The University of Guelph
-- Written by Ian McKechnie (imckechn | 1051662)
-- Written on April 5th, 2021

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;
with Ada.Calendar; use Ada.Calendar;

procedure radixsort is

    --Declare and create the array of numbers
    type numberArray is array(1..5000) of integer;
    numbers : numberArray;
   

    --      PROCEDURES

    --This procedure helps parse the numbers recieved from the file into the array if numbers.
    -- Was having issues with the string lengths and being converted so I used this to try every length
    -- of string until it comprised all the integers and worked.
    procedure getNumber(numbers : in out numberArray; i, len : in integer; line : String) is
    begin
        numbers(i) := Integer'Value(line(line'first..len));

        exception
            when Constraint_Error => numbers(i) := 0;

    end getNumber; 



    -- The file reader prodecure, reads the file into the numbers array
    -- Adapted from code written from Rosseta Code at https://www.rosettacode.org/wiki/File_input/output#Ada
    -- Taken on April 9th, 2021  
    procedure fileReader( numbers : in out numberArray ) is

        fname : unbounded_string;
        Input : File_Type;
        i, len : integer;

    begin
        put_line("What is the input file called (include.txt): ");
        get_line(fname);

        -- Open the file
        begin
            Open (File => Input,
                    Mode => In_File,
                    Name => to_string(fname));
        exception
            when others =>

                Put_Line (Standard_Error,
                        "Can not open the file '" & to_string(fname) & "'. Does it exist?");
                return;
        end;
        
        -- Loop through all the lines in the file and store them in the array
        i := 1;
        loop
            declare
                Line : constant String := Ada.Text_IO.Get_Line(Input);
            begin
                
                len := 6;
                loop 
                    exit when numbers(i) /= 0;

                        getNumber(numbers, i, len, Line);

                    len := len - 1;
                end loop;

                i := i + 1;
            end;
        end loop;

    --When end of file is reached, close it
    exception
        when End_Error =>
            if Is_Open(Input) then 
                Close (Input);
            end if;
    end fileReader;


    --Find the maximum value in the array
    procedure max( numbers : in numberArray; highest : out integer) is
        
        i : integer;

    begin
        highest := numbers(1);
        i := 2;

        loop
            exit when i = 5000;

            if ( numbers(i) > highest) then
                highest := numbers(i);
            end if;

            i := i + 1;
        end loop;
    end max; 

    -- The main sorting algorithm, it works its way through the number columns
    -- sorting each number one digit at a time (ones, tens, hundreds for example)
    procedure sort( numbers : in out numberArray; digit : in integer) is
        
        output : numberArray;
        i, temp, length : integer;
        count : array(1..10) of integer;

    begin
        count := (1..10 => 0);
        length := 5000;

        i := 1;
        loop
            exit when i = length + 1;
            temp := (numbers(i) / digit mod 10) + 1;
            
            count(temp) := count(temp) + 1;

            i := i + 1;
        end loop;


        i := 2;
        loop
            exit when i = 11;
            count(i) := count(i) + count(i - 1);

            i := i + 1;
        end loop;

        i := length;
        loop
            exit when i = 0;

                temp := (numbers(i) / digit mod 10) + 1;

                output( count( temp)) := numbers(i);

                count(temp) := count(temp) - 1;

            i := i - 1;
        end loop;

        numbers := output;
    end sort;

    --The radix sort interface procedure. This sets everything up for being 
    -- searched through and is what calls the sort procedure
    procedure radixSort( numbers : in out numberArray ) is

        output : numberArray;
        highest, i : integer;

    begin
        output := numbers;

        max(output, highest);

        i := 1;
        while highest / i > 0 loop
            sort(output, i);        

            i := i * 10;
        end loop;

        numbers := output;
    end radixSort;


    -- The file writter prodecure
    -- Adapted from code written from Rosseta Code at https://www.rosettacode.org/wiki/File_input/output#Ada
    -- Taken on April 9th, 2021  
    procedure fileWriter( numbers : in numberArray ) is
        Write_To  : constant String := "sortedA.txt";
        i : integer;
        
        Output : File_Type;
    begin
       begin
            Create (File => Output,
                    Mode => Out_File,
                    Name => Write_To);
        exception
            when others =>
                Put_Line (Standard_Error,
                        "Can not create a file named '" & Write_To & "'.");
                return;
        end;
        
        i := 1;
        loop
            exit when i > 5000;
                put_line(output, integer'image(numbers(i)));
            i := i + 1;
        end loop;
        Close (Output);
    exception
        when End_Error =>
            if Is_Open(Output) then 
                Close (Output);
            end if;
    end fileWriter;

    -- This runs and times the radix sort and returns the time in nanoseconds
    procedure Query_Performance( numbers : in out numberArray ) is
        type Proc_Access is access procedure(numbers : in out numberArray);
        function Time_It(Action : in out Proc_Access; Arg : in out  numberArray) return Duration is
            Start_Time : constant Time := Clock;
            Finis_Time : Time;
            Func_Arg : constant numberArray := Arg;
        begin
            Action(Func_Arg);
            Finis_Time := Clock;
            return Finis_Time - Start_Time;
        end Time_It;
        
        procedure Identity(numbers : in out numberArray) is
        begin
            radixSort(numbers);
        end Identity;
        
        
        Id_Access : Proc_Access := Identity'access;
    
    begin
        Put_Line("Identity(4) takes" & Duration'Image(Time_It(Id_Access, numbers)) & " seconds.");
    end Query_Performance;


-- *** Main Program ***
begin 
    --Initialize the Array
    numbers := (1..5000 => 0);    

    -- Read in data from file
    fileReader(numbers);

    -- Perform Radix Sort and time it
    Query_Performance(numbers);
    --radixSort(numbers);
    


    -- Output data to a file
    fileWriter(numbers);

end radixsort;
