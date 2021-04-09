! Radix Sort in Fortran 
! Written by Ian McKechnie
! For CIS 3190 at the University of Guelph
! Written on Arpil 3, 2021

program radix
    ! Variable Declaration
    real :: numbers(5000)
    character(len = 30) :: filename
    write (*,*) "A"

    numbers = 0

    ! Read in data from file
    call getInputs(numbers)
    write (*,*) "B"

    !Perform Radix Sort
    call radixSort(numbers)
    write (*,*) "C"


    ! Output data to a file
    write (*,*) "What file name would you like to write to? (include .txt): "
    read (*,*) filename
    
    write (*,*) "D"
    call fileWriter(numbers, filename)
    write (*,*) "E"

    ! Close files and quit

    end

! --- Functions ---
    subroutine getInputs(numbers)
        
        real :: numbers(5000)
        character (len=25) :: fname
        integer :: i

        write (*,*) "Please give an input file name: "
        !read (*,*) fname
        fname = "inputs.txt"

        Open( 10, file = 'inputs.txt' )
        i = 0
        Do
            Read( 10, *, End = 1 ) data
            numbers(i) = real(data)
            i = i + 1
        End Do
        1 write (*,*) "End of file reached";

        close(10)

        end
        

    
    subroutine radixSort(numbers)

        ! Variable Declaration
        real :: numbers(5000), highest, output(5000), numDigits
        integer :: digits
        

        call max(numbers, highest)
        write (*,*) "Max = ", highest


        output = numbers
        numDigits = 4

        write (*,*) "1"
        digits = FLOOR( LOG(highest)/LOG(numDigits))
        write (*,*) "digits = ", digits
        do i = 1, digits 
            call countingSort(output, i, numDigits)
        end do
        write (*,*) "3"

        do i = 1, 5000
            write (*,*) output(i)
        end do

    end

    subroutine countingSort(nums, digit, base)
        real :: base, temp, nums(5000), output(5000), count(10)
        integer :: i, digit, arraySize
        
        write (*,*) "STARTING"

        arraySize = 5000
        count = 0 ! Initialize the whole array to zeros

        write (*,*) "1"
        do i = 1, arraySize
            write (*,*) "A.1"
            if (nums == null) then
                continue
            end if


            temp = mod( nums(i) / digit, base) + 1
            write (*,*) "B.1, temp= ", temp
            write (*,*) "nums(i)= ", nums(i)
            write (*,*) "digit= ", digit
            write (*,*) "i= ", i
            
            if (temp <= 0 ) then
                exit
            else if (temp > 10) then
                exit
            end if
            
            write (*,*) "C.1, count(temp) = ", count(int(temp))
            count(int(temp)) =  count(int(temp)) + 1
            write (*,*) "D.1"
        end do

        write (*,*) "2"
        do i = 1, int(base)
            count(i) = count(i - 1)
        end do

        write (*,*) "3"
        do i = arraySize, 1, -1
            write (*,*) "A.3"
            write (*,*) "nums = ", i
            temp = mod(nums(i) / digit, base)
            write (*,*) "B.3"
            output( int(count( int(temp))) - 1) = nums(i)
            write (*,*) "C.3"

            count(int(temp)) = count(int(temp)) - 1
            write (*,*) "D.3"
        end do

        write (*,*) "4"
        do i = 1, arraySize
            nums(i) = output(i)
        end do

        write (*,*) "FINISHED"
    end

    subroutine max(numbers, highest)

        real :: numbers(5000)
        real :: highest
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

        real :: numbers(5000)
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

        do i = 1,n -1
            write(20,*) numbers(i)
        end do
        close(20)
    end 