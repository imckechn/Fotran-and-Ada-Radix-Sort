! Radix Sort in Fortran 
! Written by Ian McKechnie
! For CIS 3190 at the University of Guelph
! Written on Arpil 3, 2021

program radix
    ! Variable Declaration
    real :: numbers(5000)
    
    


    ! Read in data from file
    call getInputs(numbers)
    write (*,*) "FInished"

    !Perform Radix Sort
    call radixSort(numbers)


    ! Output data to a file



    !Close files and quit

    end

! --- Functions ---
    subroutine getInputs(numbers)
        
        real :: numbers(5000)
        character (len=25) :: fname
        integer :: i

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
            Read( 10, *) numbers(i)
            i = i + 1
        End Do

        close(10)

        end
        

    
    subroutine radixSort(numbers)

        ! Variable Declaration
        real :: numbers(5000)
        real :: highest, numDigits, output(5000)
        integer :: digits
        

        call max(numbers, highest)
        write (*,*) "Max = ", highest


        output = numbers
        numDigits = 4
        digits = FLOOR( LOG(highest)/LOG(numDigits))

        do i = 1, digits 
            output = countingSort(output, i, numDigits)
        end do

    end

    real function countingSort(nums, digit, base)
        real:: b(5000), c(INT(base)), digit_of_Ai, num(5000)
        integer :: i, j

        do i = 0, 5000
            digit_of_Ai = MODULO( (nums(i)/base ** digit), base)
            c(INT(digit_of_Ai)) = c(INT(digit_of_Ai)) + 1
        end do

        do j = 1, base
            c(j) = c(j) + c(j-1)
        end do

        i = 5000
        do while (i >= 0)
            digit_of_Ai = modulo(nums(m)/radix**digit, radix)
            c(INT(digit_of_Ai)) = c(INT(digit_of_Ai)) - 1
            b(INT(c(INT(digit_of_Ai)))) = nums(m)

            i = i -1
        end do

        countingSort = b

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
