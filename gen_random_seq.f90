PROGRAM gen_random_seq

INTEGER (kind=8), PARAMETER ::samples_per_line=5    ! Number of samples/line
INTEGER (kind=8), PARAMETER ::total_lines=5         ! Total lines

INTEGER (kind=8)  :: I , J , K

REAL(kind=8)	    :: random_n (samples_per_line)
CHARACTER (LEN =3)  ::   my_format_str_1
CHARACTER (LEN =40) ::  my_format_str_2

WRITE (my_format_str_1,'(I3)') samples_per_line


my_format_str_2="(" // my_format_str_1 // "(F12.2,2X)" // ")"

DO I = 1, total_lines
    DO J = 1 , samples_per_line
        CALL RANDOM_NUMBER(random_n(J))
        random_n(J)= 1.D05 * random_n(J)
    END DO
!   WRITE (*,FMT='(samples_per_line(F12.2,2X))')  random_n ( 1:samples_per_line )
    WRITE (*,my_format_str_2)  random_n ( 1:samples_per_line )
END DO


END PROGRAM gen_random_seq    
