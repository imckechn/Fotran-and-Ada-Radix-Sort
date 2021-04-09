! Radix Sort in Fortran 
! Written by Ian McKechnie
! For CIS 3190 at the University of Guelph
! Written on Arpil 3, 2021

program radix
    ! Variable Declaration
    integer :: numbers(5000)
    character(len = 30) :: filename
    write (*,*) "A"

    numbers = 0

    ! Read in data from file
    call getInputs(numbers)
    write (*,*) "B"

    !Perform Radix Sort
    call radixSort(numbers)
    write (*,*) "C"

    write (*,*) "numbers(0) =", numbers(1)


    ! Output data to a file
    write (*,*) "What file name would you like to write to? (include .txt): "
    !read (*,*) filename
    filename = "o.txt"
    
    write (*,*) "D"
    call fileWriter(numbers, filename)
    write (*,*) "E"

    ! Close files and quit

    end

! --- Functions ---
    subroutine getInputs(numbers)
        
        integer :: numbers(5000)
        character (len=25) :: fname
        integer :: i

        write (*,*) "Please give an input file name: "
        !read (*,*) fname
        fname = "inputs.txt"

        Open( 10, file = 'inputs.txt' )
        i = 1
        Do
            Read( 10, *, End = 1 ) data
            numbers(i) = int(data)
            i = i + 1
        End Do
        1 write (*,*) "End of file reached";

        close(10)

    end    

    
    subroutine radixSort(numbers)

        ! Variable Declaration
        integer :: numbers(5000), highest, output(5000), numDigits        

        call max(numbers, highest)
        write (*,*) "Max = ", highest


        output = numbers
        numDigits = 4

        write (*,*) "1"

        i = 1
        do while (highest / i > 0)
            call countingSort(output, 5000, i)
            i = i * 10

            write (*,*) "completed loop"
        end do
        write (*,*) "3"

        numbers = output

    end

    subroutine countingSort(nums, arrLength, digit)
        integer :: arrLength, temp, nums(arrLength), output(arrLength), count(10)
        integer :: i, digit
        
        write (*,*) "STARTING, digit = "
        count = 0 ! Initialize the whole array to zeros



        !write (*,*) "1"
        do i = 1, arrLength
            temp = mod( nums(i) / digit, 10) + 1    ! plus one as arrays start at element 1

            count(temp) =  count(temp) + 1
        end do
        write (*,*) count



        do i = 2, 10
            count(i) = count(i) + count(i - 1)
        end do
        !write (*,*) "Count after ", count



        write (*,*) "3"
        do i = arrLength, 1, -1

            temp = mod( nums(i) / digit, 10) + 1

            if ( count(temp) > 5000) then
                write (*,*) "temp = ", temp
                write (*,*) "count at temp", count( temp )
            
            else if ( count(temp) < 1) then
                write (*,*) "temp = ", temp
                write (*,*) "count at temp", count( temp )
            end if

            output( count( temp)) = nums(i)

            
            count(temp) = count(temp) - 1
        end do
        write (*,*) "4"

        nums = output
        write (*,*) "FINISHED"
    end

    subroutine max(numbers, highest)

        integer :: numbers(5000)
        integer :: highest
        integer :: i

        highest = numbers(1)

        do i = 2, 4999 
            if (numbers(i) > highest) then 
                highest = numbers(i)
            end if

        end do
    end

    ! File writer adapted from code written by Michael Wirth
    ! From https://craftofcoding.wordpress.com/2018/02/22/coding-fortran-file-i-o/
    ! On April 4th, 2021 (Happy Easter!)
    subroutine fileWriter(numbers, fname)

        integer :: numbers(5000)
        character (len=30) :: fname
        integer :: i,n,fsize
        logical :: lexist
        character :: formL

        n = 5000

        inquire(file=fname, exist=lexist, form=formL, size=fsize)
        if (lexist) then
            write(*,*) 'File exists and is ', formL
            write(*,*) 'The file is ',fsize,' bytes in size'
            write(*,*) 'It will be overwritten'
            open(unit=20,file=fname,status='replace',action='write')
        else
            write (*,*) 'File does not exist - will be created.'
            open(unit=20,file=fname,status='new',action='write')
        end if

        do i = 1,n
            write(20,*) numbers(i)
        end do
        close(20)
    end 