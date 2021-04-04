! Radix Sort in Fortran 
! Written by Ian McKechnie
! For CIS 3190 at the University of Guelph
! Written on Arpil 3, 2021

program radix
    ! Variable Declaration
    real :: numbers(5000)
    character(len = 30) :: filename
    
    


    ! Read in data from file
    call getInputs(numbers)
    write (*,*) "FInished"

    !Perform Radix Sort
    call radixSort(numbers)


    ! Output data to a file
    write (*,*) "What file name would you like to write to? (include .txt): "
    read (*,*) filename

    call fileWriter(numbers, filename)

    !Close files and quit

    end

! --- Functions ---
    subroutine getInputs(numbers)
        
        real :: numbers(5000)
        character (len=25) :: fname
        integer :: i, end

        write (*,*) "Please give an input file name: "
        !read (*,*) fname
        fname = "inputs.txt"

        !inquire(file=fname, exist=lexist, form=formL, size=fsize)
        !if (lexist eq. False) then
        !    open (2, file = 'inputs.txt', status = 'old')

        !end if

        Open( 10, file = 'inputs.txt' )
        i = 0
        Do
            Read( 10, *, end = 1 ) data
           i = i + 1
         End Do

        close(10)

        end
        

    
    subroutine radixSort(numbers)

        ! Variable Declaration
        real :: numbers(5000), highest, numDigits, output(5000)
        integer :: digits
        

        call max(numbers, highest)
        write (*,*) "Max = ", highest


        output = numbers
        numDigits = 4
        digits = FLOOR( LOG(highest)/LOG(numDigits))

        do i = 1, digits 
            call countingSort(output, i, numDigits)
        end do

        do i = 1, 5000
            write (*,*) output(i)
        end do

    end

    subroutine countingSort(nums, digit, base)
        real:: b(5000), c(INT(base)), digit_of_Ai, nums(5000)
        integer :: i, j, digit
        

        do i = 0, 5000
            digit_of_Ai = MODULO( (nums(i)/base ** digit), base)
            c(INT(digit_of_Ai)) = c(INT(digit_of_Ai)) + 1
        end do

        do j = 1, INT(base)
            c(j) = c(j) + c(j-1)
        end do

        i = 5000
        do while (i >= 0)
            digit_of_Ai = modulo(nums(i)/base**digit, base)
            c(INT(digit_of_Ai)) = c(INT(digit_of_Ai)) - 1
            b(INT(c(INT(digit_of_Ai)))) = nums(i)

            i = i -1
        end do

        nums = b

    end

    subroutine max(numbers, highest)

        real :: numbers(5000)
        real :: highest
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
    subroutine fileWriter(numbers, fname)

        character (len=25) :: fname
        integer :: i,n,fsize
        logical :: lexist
        character :: formL

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