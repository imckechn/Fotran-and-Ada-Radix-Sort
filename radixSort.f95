! Radix Sort in Fortran 
! Written by Ian McKechnie
! For CIS 3190 at the University of Guelph
! Written on Arpil 3, 2021

program radix
    ! Variable Declaration
    integer :: numbers(5000)
    integer(kind=8) :: count1, count2, count_rate, count_max

    
    numbers = 0

    ! Read in data from file
    call getInputs(numbers)

    !Perform Radix Sort
    call system_clock(count1, count_rate, count_max)
    call radixSort(numbers)
    call system_clock(count2, count_rate, count_max)


    write (*,*) "Time taken = "
    print *, count2 - count1
    write (*,*) "Nanoseconds"


    ! Output data to a file   
    call fileWriter(numbers)

    ! Close files and quit

    end

! --- Functions ---

    ! This gets the inputs from the given file and stores it in the numbers array
    subroutine getInputs(numbers)
        
        integer :: numbers(5000)
        character (len=25) :: fname
        integer :: i

        write (*,*) "Please give an input file name: "
        read (*,*) fname

        Open( 10, file = 'inputs.txt' )
        i = 1
        Do
            Read( 10, *, End = 1 ) data
            numbers(i) = int(data)
            i = i + 1
        End Do
        1 close(10)

    end    

    ! This is the radix sort interface. It sets everything up for the main subroutine, countingSort, 
    ! to do the actual number moving. 
    subroutine radixSort(numbers)

        ! Variable Declaration
        integer :: numbers(5000), highest, output(5000), numDigits        

        call max(numbers, highest)

        output = numbers
        numDigits = 4

        i = 1
        do while (highest / i > 0)
            call countingSort(output, 5000, i)
            i = i * 10

        end do

        numbers = output

    end

    ! This does all the actual sorting in the algorithm. It movies through all the numbers, one digit at a time
    ! sorting each one until it is completed.
    subroutine countingSort(nums, arrLength, digit)
        integer :: arrLength, temp, nums(arrLength), output(arrLength), count(10)
        integer :: i, digit
        
        count = 0 ! Initialize the whole array to zeros

        do i = 1, arrLength
            temp = mod( nums(i) / digit, 10) + 1    ! plus one as arrays start at element 1

            count(temp) =  count(temp) + 1
        end do

        do i = 2, 10
            count(i) = count(i) + count(i - 1)
        end do

        do i = arrLength, 1, -1

            temp = mod( nums(i) / digit, 10) + 1
            output( count( temp)) = nums(i)
            count(temp) = count(temp) - 1

        end do

        nums = output
    end

    ! This finds the maximum value in the array of numbers. 
    subroutine max(numbers, highest)

        integer :: numbers(5000)
        integer :: highest
        integer :: i

        highest = numbers(1)

        do i = 2, 5000 
            if (numbers(i) > highest) then 
                highest = numbers(i)
            end if

        end do
    end

    ! File writer adapted from code written by Michael Wirth
    ! From https://craftofcoding.wordpress.com/2018/02/22/coding-fortran-file-i-o/
    ! On April 4th, 2021 (Happy Easter!)
    subroutine fileWriter(numbers)

        integer :: numbers(5000)
        character (len=30) :: fname
        integer :: i,n,fsize
        logical :: lexist
        character :: formL

        n = 5000

        fname = "sortedF.txt"

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