! The fire danger program
! Meant to replace the National Fire Danger Rating Index, first introduced in 1964 in the U.S
! Written by Ian McKechnie during the week of February the 5th, 2021
! Written for CIS*3190 Assignment 1

program fire
    
    real :: dry, wet, isnow, precip, wind, buildUpIndex, iherb, dryingFact, FireFuelMoisture, adjustedFuelMoisture, &
    grass, timber, fireLoad
    
!   Get the inputs
    write (*,*) "Dry bulb temperature: "
    read (*,*) dry
    
    write (*,*) "Wet bulb temperature: "
    read (*,*) wet
    
    write (*,*) "Is there snow(positive, non-zero number for snow on the ground): "
    read (*,*) isnow
    
    write (*,*) "Wind speed (in mph)?: "
    read (*,*) wind
    
    write (*,*) "build up index (The last value): "
    read (*,*) buildUpIndex
    
    write (*,*) "herb state (1 = Cured, 2 = Transition, 3 = Green): "
    read (*,*) iherb
    
    write (*,*) "Precipitation: "
    read (*,*) precip

!   Call the main function
    
    call danger (dry, wet, isnow, precip, wind, buildUpIndex, iherb, dryingFact, FireFuelMoisture, adjustedFuelMoisture, &
     grass, timber, fireLoad)

    write (*,*) ""
    write (*,*) "Output"
    write (*,*) "--------------------------------------------"
    write (*,*) "Fine Fuel Moisture      = ", FireFuelMoisture
    write (*,*) "Adjusted Fuel Moisture  = ", adjustedFuelMoisture
    write (*,*) "Fine Fuel Spread        = ", grass
    write (*,*) "Timber Fuel Spread      = ", timber
    write (*,*) "Fire Load Index         = ", fireLoad
    write (*,*) "Build Up Index          = ", buildUpIndex
    
    end
    
!   The danger subroutine: Calculates and prints all the relivant data
!   Input: The dry bulb value, the wet bulb value, if there is snow, the precipitation, the wind speed,
!   contined: The build up index, the herb state, the drying factor, the fire fuel moisture, the adjusteed fuel moisture,
!   contined: the grass (fine fuel spread), the timber fuel spread, and the fire load index (fireLoad)
    subroutine danger(dry, wet, isnow, precip, wind, buildUpIndex, iherb, dryingFact, FireFuelMoisture, adjustedFuelMoisture, &
        grass, timber, fireLoad)

    
!   Data needed for calculations are:
!   ------------------------------------------------------------------
!   fry,    Dry bulb tempurature
!   wet,    Wet bulb tempurature
!   isnow,  Some positive non-zero numbers if there is snow on the ground
!   precip, The amount of precipitation recieved
!   wind,   The current wind speed in miles per hour
!   buildUpIndex,    The last value of the build up index
!   iherb,  The current herb state of the district 1 = cured, 2 = transition, 3 = green
!   ------------------------------------------------------------------

    
!   Data returned from the subroutine is:
!   ------------------------------------------------------------------
!   Drying factor as                            dryingFact
!   Adjusted (10 day lag) fuel moisture as      FireFuelMoisture
!   Grass spread index will be returned as      grass
!   Timber spread index will be returned as     timber
!   Fire load rating will be returned as        fireLoad
!   Build up index will be returned as          buildUpIndex
!   ------------------------------------------------------------------

!   Variable type declarations
    real :: dry, wet, isnow, precip, wind, buildUpIndex, iherb, dryingFact, FireFuelMoisture, adjustedFuelMoisture, &
     grass, timber, fireLoad
    real :: difference, a(4), b(4), c(3), d(6)
    integer  :: i, j
    

!   Default values
    FireFuelMoisture = 99.    
    adjustedFuelMoisture = 99.
    dryingFact = 0.
    fireLoad = 0.

    
!   These are the table values used in computing the danger ratings:
    a = (/ -0.185900, -0.85900, -0.059660, -0.077373/)
    b = (/30.0, 19.2, 13.8, 22.5/)
    c = (/ 4.5, 12.5, 27.5/) 
    d = (/16.0, 10.0, 7.0, 5.0, 4.0, 3.0/)  
    
    
!   Check if there is snow on the ground. If there is then return.
    if ( isnow >  0.) then 
        call isnowing(precip, buildUpIndex)
        return
    end if
    
!   Find the difference between the dry and wet bulbs and use that to find the Fire Fuel Mositure
    difference = dry - wet
    do j = 1, 3
        if (difference - c(j) <= 0 ) then
            i = j
            exit
    
        else if (j == 3) then
            i = 4   
        end if
        continue
    end do     
    
    FireFuelMoisture = b(i) * exp( a(i) * difference)
    
    do i = 1,6
        if ( FireFuelMoisture - d(i) > 0) then
            dryingFact = i - 1
            exit
        else if ( i == 6 ) then
            dryingFact = 7
        end if
    
        dryingFact = 7
    enddo
    
    if ( FireFuelMoisture - 1.0 < 0) FireFuelMoisture = 1
    
    FireFuelMoisture = FireFuelMoisture + ( IHERB-1 ) * 5.

!   Use the Fire Fuel Moisture to find the Adjusted Fuel Moisture and the Build Up Index
    buildUpIndex = getBuildUpIndex(buildUpIndex, precip, dryingFact)
    
    adjustedFuelMoisture = .9 * FireFuelMoisture +.5 + 9.5 * exp( -buildUpIndex / 50. )

!   Use the Adjusted Fuel Moisture to find the timber and gass consumption
    if ( adjustedFuelMoisture - 30.0 >= 0) then
        if (FireFuelMoisture - 30.0 >= 0) then
            grass = 1.0
            timber = 1.0
            return
    
        else
            timber = 1.0
    
            if ( wind - 14.0 >= 0.) then
                grass  = .00918 * (wind + 14.0 ) * (33.0 - FireFuelMoisture) ** 1.65 - 3.0
    
                if ( grass - 99.0 > 0.) then
                    grass = 99.0
    
                    if ( timber - 99.0 > 0.) then
                        timber = 99.0
                    endif
                endif
            else
                grass = 0.01312 * (wind + 6.0) * (33.0 - FireFuelMoisture) ** 1.65 - 3.0
    
                if (timber - 1.0 <= 0.) then
                    timber = 1.0
                    
                    if (grass - 1.0 < 0.) then
                        grass = 1.0
                    endif
                endif
            endif
    
            if (checkVals(timber, buildUpIndex) == 1) return
        endif
    else
        if ( wind - 14.0 < 0) then
            timber = 0.01312 * (wind + 6.0) * (33.0 - adjustedFuelMoisture)** 1.65 - 3.0
            grass = 0.01312 * ( wind + 6.0) * ( 33.0 - FireFuelMoisture ) ** 1.65 - 3.0
    
            if (timber - 1.0 <= 0.) then
                timber = 1.0
                
                if ( grass - 1.0 < 0.) then
                    grass = 1.0
                endif
            endif
        else
            timber = 0.00918 * ( wind + 14.0) * (33.0 - adjustedFuelMoisture) ** 1.65 - 3.0
            grass  = 0.00918 * ( wind + 14.0) * (33.0 - FireFuelMoisture) ** 1.65 - 3.0
    
            if ( grass - 99.0 > 0.) then
                grass = 99.0
    
                if ( timber - 99.0 > 0.) then
                    timber = 99.0
    
                endif
            endif
        endif
    
        if (checkVals(timber, buildUpIndex) == 1) return
    endif
    
!   Calculate the fire index
    fireLoad = 1.75 * log10( timber ) + 0.32 * log10( buildUpIndex ) - 1.640
    
    if ( fireLoad  <= 0.) then
        fireLoad = 0.
    else 
        fireLoad = 10. ** fireLoad  
    end if
    
    return
    end

!   The check values is used when finding the timber value. It checks if it's invalid and if it is it will end the main subroutine
!   Input: The timber values, and build up index
!   Output: 1 on failure, 0 on success
    real function checkVals(timber, buildUpIndex)
        real :: timber, buildUpIndex
        checkVals = 0.
    
        if ( timber <= 0.) then
            checkVals = 1.
        else
            if ( buildUpIndex <= 0.) then
                checkVals = 1.
            
            endif
        endif
        return
    end 
    
!   Updates the build up index (BUO) for the main subroutine
!   Input: the precipitation and build up index, and the drying factor
!   Output: the build up index
    real function getBuildUpIndex(buildUpIndex, precip, dryingFact)
        real :: buildUpIndex, precip, dryingFact
        getBuildUpIndex = buildUpIndex
      
        if ( precip - 0.1 > 0) then 
            getBuildUpIndex =-50.*alog(1.-(1.-exp(-getBuildUpIndex/50.))*exp(-1.175*(precip-.1)))
    
        if ( getBuildUpIndex < 0.) then
            getBuildUpIndex = 0.0
            end if
        end if
    
        getBuildUpIndex = getBuildUpIndex + dryingFact
    
        return
    end
    
!   If it's snowing, this subroutine runs
!   Input: the precipitation and build up index
!   Output: NA
    subroutine isnowing(precip, buildUpIndex)
        real :: buildUpIndex, precip
    
        grass = 0.
        timber = 0.
    
        write (*,*) "It's snowing, no risk of fire"
    
        if ( precip - .1 <= 0.) then
            return
        end if
    
        buildUpIndex = -50.0 *alog(1.0 - (1.0 - exp (-buildUpIndex/50.0)) * exp( -1.175 * ( precip - 0.1)))
    
        if (buildUpIndex < 0.) then 
            buildUpIndex = 0.
        end if    
        return
    end