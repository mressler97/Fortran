	!Name:		Michael Ressler
	!Class:		CSCI305 Programming Languages and Concepts
	!Assignment:	Fortran Program 
	!Date:		1/21/19

	

	program firstFortran 
		implicit none
		
		integer :: amount, ierror
		integer :: q = 0, d = 0 , n = 0, p = 0
		character(LEN = 8) :: DateINFO !ccyymmdd 
		character(LEN = 4) :: Year, Month*2, Day*2
		character(LEN = 10) :: timeINFO, PrettyTime*12 !hhmmss
		character(LEN = 2) :: Hour, Minute

		CALL DATE_AND_TIME(DateINFO, TimeINFO)
			Year = DateINFO(1:4)
			Month = DateINFO(5:6)
			Day = DateINFO(7:8)
			Hour = TimeINFO(1:2)
			Minute = TimeINFO(5:6)

		WRITE(*,*)
		WRITE(*,*) Month, "-", Day, "-", Year, ",", Hour, ":", Minute
 		WRITE(*,*)

		write(*,*) "Enter amount to make change: "
		read(*,*,iostat=ierror) amount

		amount = change(amount)	

		IF (q .NE. 0) THEN
			write(*,'(i3,A)', advance = "no") q, " Quarter "
		END IF
		IF ( d .NE. 0) THEN	
			write(*,'(i3,A)', advance = "no") d, " Dime "
		END IF 
		IF ( n .NE. 0) THEN
			write(*,'(i3,A)', advance = "no") n, " Nickel "
		END IF
		IF (p .NE. 0) THEN		

			write(*,'(i3,A)', advance = "no") p, " Penny " 
		END IF
		write(*,*)

		CONTAINS

		INTEGER FUNCTION change(i)
		implicit none
		INTEGER, INTENT(INOUT) :: i
	 	
		DO
			IF (i == 0) EXIT
			IF (i > 99) THEN
				write(*,*) "ERROR: Enter an integer between 1-99" 
				i = 0 
			ELSE IF (i < 0) THEN
				write(*,*) "ERROR: Enter an integer between 1-99" 
				i = 0 
			ELSE IF (i >= 25) THEN
				i = i - 25
				q = q + 1  
			ELSE IF (i >= 10) THEN 
				i = i - 10 
				d = d + 1
			ELSE IF (i >= 5) THEN
				i = i - 5
				n = n + 1  
			ELSE IF (i >= 1) THEN 
				i = i - 1
				p = p + 1 
			END IF	
		END DO
			
		END FUNCTION change
 

	end program firstFortran
