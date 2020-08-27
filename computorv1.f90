PROGRAM computorv1
	! Arg variables
	CHARACTER(len=:), ALLOCATABLE :: arg
	INTEGER :: arglen
	! Polynomes from X^0 to X^9 (Starting by 10 it returns parsing error)
	REAL, DIMENSION(0:9) :: polynome
	REAL :: nb
	! Parsing variables
	INTEGER :: mode
	LOGICAL, DIMENSION(0:3) :: signe
	INTEGER :: ancre
	INTEGER :: degree
	! Resolve variables
	REAL :: delta
	INTEGER :: psize
	LOGICAL :: tmpfound
	! Temporary variables
	CHARACTER(len=:), ALLOCATABLE :: copy
	CHARACTER(20) :: tmp
	INTEGER :: t
	REAL, DIMENSION(0:1) :: k
	REAL, DIMENSION(0:1) :: c
	
	! Parse arg, allogate the character and store argv[1] in it, iargc() exclude the program name
	IF (IARGC() /= 1) THEN
		PRINT *, "Usage : ./computorv1 [equation]"
		CALL EXIT(0)
	END IF
	CALL GET_COMMAND_ARGUMENT(1,length=arglen)
	ALLOCATE(CHARACTER(arglen) :: arg)
	ALLOCATE(CHARACTER(arglen + 1) :: copy)
	CALL GET_COMMAND_ARGUMENT(1,value=arg)
	
	! Initialise the parsing variables (+ the polynome)
	mode = 0
	signe(0) = .TRUE. ! positive
	signe(1) = .FALSE. ! sign not found yet
	signe(2) = .FALSE. ! equal not found yet
	signe(3) = .FALSE. ! dot not found yet
	ancre = 1 ! copy index for float parsing
	degree = 0 ! Polynome grade, originaly set to 0
	nb = 0 ! Number to be add to the polynome
	t = 0 ! Number of loops to wait (Mode 5)
	polynome = 0 ! Init polynome array
	
	! Parse the string to reduce the equation
	! Different modes : 
	! 0 : None (At first) : Search for number, dot, sign or =, if sign we stay in None but set a check
	! 1 : Found number (or dot) : Store the first number index, and store the last number index (just before a space, a * or a X), then search (space, *, X, -, +)
	! 2 : Number done, searching * or X
	! 3 : (Optionnal) found * : Search for X
	! 4 : found X : If end of string or followed by (space, +, -) : X^1, else check number 2 char further and get the polynome degree, then go after the number and return to None
	DO i = 1,arglen

		! Mode 0 : None
		IF (mode == 0) THEN
			IF (arg(i:i) == "." .OR. (arg(i:i) >= "0" .AND. arg(i:i) <= "9")) THEN
				mode = 1
			ELSE IF (arg(i:i) == "-" .AND. (signe(1) .EQV. .FALSE.)) THEN
				signe(0) = .FALSE. ! negative
				signe(1) = .TRUE. ! sign found
			ELSE IF (arg(i:i) == "+" .AND. (signe(1) .EQV. .FALSE.)) THEN
				signe(0) = .TRUE.
				signe(1) = .TRUE.
			ELSE IF (arg(i:i) == "=" .AND. (signe(2) .EQV. .FALSE.) .AND. (signe(1) .EQV. .FALSE.)) THEN
				signe(2) = .TRUE. ! = found, all the following operands will be inverted
			ELSE IF (arg(i:i) == "X") THEN
				nb = 1
				IF (signe(0) .EQV. .FALSE.) THEN
					nb = -nb
					signe(0) = .TRUE.
				END IF
				IF (signe(2) .EQV. .TRUE.) THEN
					nb = -nb
				END IF
				mode = 4
			ELSE IF (arg(i:i) /= " ") THEN
				PRINT *, "Error : [", arg(i:i), "] - Invalid Token (Mode 0)"
				DEALLOCATE(arg)
				DEALLOCATE(copy)
				CALL EXIT(1)
			END IF
		END IF

		! Mode 3 : X intermediate searching
		IF (mode == 3) THEN
			IF (arg(i:i) == "X") THEN
				mode = 4
			ELSE IF (arg(i:i) /= " ") THEN
				PRINT *, "Error : [", arg(i:i), "] - Invalid Token (Mode 3)"
				DEALLOCATE(arg)
				DEALLOCATE(copy)
				CALL EXIT(1)
			END IF
		END IF

		! Mode 1 : Number parsing
		IF (mode == 1) THEN
			signe(1) = .FALSE.
			IF (arg(i:i) == "." .AND. (signe(3) .EQV. .FALSE.)) THEN
				copy(ancre:ancre) = arg(i:i)
				ancre = ancre + 1
				signe(3) = .TRUE.
			ELSE IF (arg(i:i) >= "0" .AND. arg(i:i) <= "9") THEN
				copy(ancre:ancre) = arg(i:i)
				ancre = ancre + 1
			ELSE IF (arg(i:i) == " ") THEN	
				mode = 2
			ELSE IF (arg(i:i) == "*") THEN
				mode = 3
			ELSE IF (arg(i:i) == "X") THEN
				mode = 4
			! Particular X^0 (Return to mode 0) situations
			ELSE IF (arg(i:i) == "-" .AND. (signe(1) .EQV. .FALSE.)) THEN
				copy(ancre:ancre) = " "
				IF (ancre > 1 .AND. (copy(1:1) /= "." .OR. ancre > 2)) THEN
					READ(copy,*) nb
				ELSE
					nb = 0
				END IF
				IF (signe(0) .EQV. .FALSE.) THEN
					nb = -nb
					signe(0) = .TRUE.
				END IF
				IF (signe(2) .EQV. .TRUE.) THEN
					nb = -nb
				END IF
				polynome(0) = polynome(0) + nb
				signe(0) = .FALSE. ! negative
				signe(1) = .TRUE. ! sign found
				signe(3) = .FALSE.
				nb = 0
				mode = 0
				ancre = 1
			ELSE IF (arg(i:i) == "+" .AND. (signe(1) .EQV. .FALSE.)) THEN
				copy(ancre:ancre) = " "
				IF (ancre > 1 .AND. (copy(1:1) /= "." .OR. ancre > 2)) THEN
					READ(copy,*) nb
				ELSE
					nb = 0
				END IF
				IF (signe(0) .EQV. .FALSE.) THEN
					nb = -nb
					signe(0) = .TRUE.
				END IF
				IF (signe(2) .EQV. .TRUE.) THEN
					nb = -nb
				END IF
				polynome(0) = polynome(0) + nb
				signe(0) = .TRUE.
				signe(1) = .TRUE.
				signe(3) = .FALSE.
				nb = 0
				mode = 0
				ancre = 1
			ELSE IF (arg(i:i) == "=" .AND. (signe(2) .EQV. .FALSE.) .AND. (signe(1) .EQV. .FALSE.)) THEN
				copy(ancre:ancre) = " "
				IF (ancre > 1 .AND. (copy(1:1) /= "." .OR. ancre > 2)) THEN
					READ(copy,*) nb
				ELSE
					nb = 0
				END IF
				IF (signe(0) .EQV. .FALSE.) THEN
					nb = -nb
					signe(0) = .TRUE.
				END IF
				IF (signe(2) .EQV. .TRUE.) THEN
					nb = -nb
				END IF
				polynome(0) = polynome(0) + nb
				signe(2) = .TRUE. ! = found, all the following operands will be inverted
				signe(3) = .FALSE.
				nb = 0
				mode = 0
				ancre = 1
			ELSE
				PRINT *, "Error : [", arg(i:i), "] - Invalid Token (Mode 1)"
				DEALLOCATE(arg)
				DEALLOCATE(copy)
				CALL EXIT(1)
			END IF
			! Next step (searching for X) situations
			IF (arg(i:i) == " " .OR. arg(i:i) == "*" .OR. arg(i:i) == "X" .OR. i == arglen) THEN
				copy(ancre:ancre) = " "
				IF (ancre > 1 .AND. (copy(1:1) /= "." .OR. ancre > 2)) THEN
					READ(copy,*) nb
				ELSE
					nb = 0
				END IF
				IF (signe(0) .EQV. .FALSE.) THEN
					nb = -nb
					signe(0) = .TRUE.
				END IF
				IF (signe(2) .EQV. .TRUE.) THEN
					nb = -nb
				END IF
				ancre = 1
				signe(3) = .FALSE.
			END IF
		END IF
		
		! Mode 2 : * OR X intermediate searching
		IF (mode == 2) THEN
			IF (arg(i:i) == "*") THEN
				mode = 3
			ELSE IF (arg(i:i) == "X") THEN
				mode = 4
			! Particular X^0 (Return to mode 0) situations
			ELSE IF (arg(i:i) == "-" .OR. arg(i:i) == "+") THEN
				polynome(0) = polynome(0) + nb
				mode = 0
				signe(0) = .TRUE.
				signe(1) = .TRUE.
				IF (arg(i:i) == "-") THEN
					signe(0) = .FALSE.
				END IF
			ELSE IF (arg(i:i) == "=" .AND. (signe(2) .EQV. .FALSE.) .AND. (signe(1) .EQV. .FALSE.)) THEN
				polynome(0) = polynome(0) + nb
				signe(2) = .TRUE. ! = found, all the following operands will be inverted
				signe(3) = .FALSE.
				nb = 0
				mode = 0
			ELSE IF (arg(i:i) /= " ") THEN
				PRINT *, "Error : [", arg(i:i), "] - Invalid Token (Mode 2)"
				DEALLOCATE(arg)
				DEALLOCATE(copy)
				CALL EXIT(1)
			END IF
		END IF
		
		! Mode 4 : Polynome calculation
		IF (mode == 4) THEN
			IF (i + 2 <= arglen) THEN
				IF (arg(i+1:i+1) == "^" .AND. (arg(i+2:i+2) >= "0" .AND. arg(i+2:i+2) <= "9")) THEN
					READ(arg(i+2:i+2),*) degree
					polynome(degree) = polynome(degree) + nb
					mode = 5
					t = 3
				ELSE IF (arg(i+1:i+1) /= "^") THEN
					polynome(1) = polynome(1) + nb
					mode = 0
					signe(1) = .FALSE.
				ELSE
					PRINT *, "Error : [", arg(i:i), "] - Invalid Token (Mode 4)"
					DEALLOCATE(arg)
					DEALLOCATE(copy)
					CALL EXIT(1)
				END IF
			ELSE
				polynome(1) = polynome(1) + nb
				mode = 0
				signe(1) = .FALSE.
			END IF
		END IF
		
		! Mode 5 : Wait for offset
		IF (mode == 5) THEN
			t = t - 1
			IF (t <= 0) THEN
				mode = 0
				signe(1) = .FALSE.
			END IF
		END IF
		
	END DO
	
	! Free the allocated memory so no leaks :)
	DEALLOCATE(arg)
	DEALLOCATE(copy)
	
	! Edge cases
	IF (signe(2) .EQV. .FALSE.) THEN
		PRINT *, "Error : No [=] found"
		CALL EXIT(1)
	ELSE IF ((signe(1) .EQV. .TRUE.) .OR. mode == 3) THEN
		PRINT *, "Error : Can't end by operation "
		CALL EXIT(1)
	ELSE IF (mode == 1 .OR. mode == 2) THEN
		polynome(0) = polynome(0) + nb
	END IF
	
	! Print reduced and get polynome degree
	psize = 0
	tmpfound = .FALSE.
	PRINT *, "Reduced form:"
	DO i = SIZE(polynome)-1, 0, -1
		IF (polynome(i) /= 0 .AND. (tmpfound .EQV. .FALSE.)) THEN
			tmpfound = .TRUE.
			psize = i
		END IF
		IF (i == 0 .OR. (tmpfound .EQV. .TRUE.)) THEN
			WRITE (tmp, '(I8)')i
			IF (i /= 0) THEN
				IF (i == 1) THEN
					PRINT *, polynome(i), " ", "X"
				ELSE
					PRINT *, polynome(i), " ", "X^", TRIM(ADJUSTL(TRIM(tmp))), " +"
				END IF
			ELSE
				PRINT *, polynome(i), " = 0"
			END IF
			
		END IF
	END DO
	WRITE (tmp, '(I8)')psize
	PRINT *, "Polynomial degree: ", TRIM(ADJUSTL(TRIM(tmp)))
	
	! Resolve
	IF (psize > 2) THEN
		PRINT *, "Error: Degree too high"
	ELSE IF (psize == 0) THEN
		IF (polynome(0) == 0) THEN
			PRINT *, "Solutions: All real numbers"
		ELSE
			PRINT *, "Solutions: None"
		END IF
	ELSE IF (psize == 1) THEN
		k(0) = polynome(0) / polynome(1) ! x = b/a with ax + b = 0
		PRINT *, "Solutions: 1 real solution"
		PRINT *, "X = ", k(0)
	ELSE IF (psize == 2) THEN
		delta = polynome(1) * polynome(1) - 4 * polynome(2) * polynome(0) ! (-b)² - 4ac with ax² + bx + c = 0
		IF (delta == 0) THEN
			k(0) = -polynome(1) / (2 * polynome(2));
			PRINT *, "Solutions: Null discriminant, 1 real solution"
			PRINT *, "X = ", k(0)
		ELSE IF (delta < 0) THEN
			! real part
			k(0) = -polynome(1) / (2 * polynome(2));
			! imaginary part
			c(0) = SQRT(-delta) / (2 * polynome(2));
			c(1) = -SQRT(-delta) / (2 * polynome(2));
			PRINT *, "Solutions: Negative discriminant, 2 complex solutions"
			PRINT *, "X1 = ", k(0), " + ", c(0), "i" 
			PRINT *, "X2 = ", k(0), " + ", c(1), "i" 
		ELSE
			k(0) = (-polynome(1) + SQRT(delta)) / (2 * polynome(2));
			k(1) = (-polynome(1) - SQRT(delta)) / (2 * polynome(2));
			PRINT *, "Solutions: Positive discriminant, 2 real solutions"
			PRINT *, "X1 = ", k(0)
			PRINT *, "X2 = ", k(1)
		END IF
	END IF

END PROGRAM computorv1