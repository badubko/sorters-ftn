MODULE declarations_ordic
INTEGER (kind=8), PARAMETER ::samples_per_line=1    ! Number of samples/line
INTEGER (kind=8), PARAMETER ::total_lines=20    ! Total lines

REAL(kind=8)	    :: random_n (samples_per_line * total_lines)


END MODULE declarations_ordic
! ***********************************************************
SUBROUTINE my_sort ()
USE declarations_ordic

END SUBROUTINE my_sort

! ***********************************************************
PROGRAM sort_ordic
USE declarations_ordic

IMPLICIT NONE

INTEGER (kind=8)  :: I , J , K


CHARACTER (LEN =3)  :: my_format_str_1
CHARACTER (LEN =40) :: my_format_str_2

WRITE (my_format_str_1,'(I3)') samples_per_line


my_format_str_2="(" // my_format_str_1 // "(F12.2,2X)" // ")"

DO I = 1, total_lines
!    DO J = 1 , samples_per_line
!        CALL RANDOM_NUMBER(random_n(J))
!        random_n(J)= 1.D05 * random_n(J)
!    END DO

!    WRITE (*,my_format_str_2)  random_n ( 1:samples_per_line )

 READ (* , my_format_str_2 ) random_n(I)  ! Won't work if (samples_per_line != 1) !!!
 WRITE (*,my_format_str_2)  random_n (I) 
END DO

CALL my_sort()

END PROGRAM sort_ordic    
