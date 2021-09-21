MODULE declarations_ordic
INTEGER (kind=8), PARAMETER ::samples_per_line=1    ! Number of samples/line
INTEGER (kind=8), PARAMETER ::total_lines=25000    ! Total lines
INTEGER (kind=8), PARAMETER ::max_values=samples_per_line*total_lines   ! Total lines 

REAL(kind=8)	    :: random_n (samples_per_line * total_lines)



CONTAINS
! ***********************************************************
SUBROUTINE my_sort ()

IMPLICIT NONE

INTEGER (kind=8)  :: I , J , K , L

REAL (kind=8)  :: tmp_val
LOGICAL           :: XCHANGE=.TRUE.

DO WHILE (XCHANGE .EQV. .TRUE. )
    XCHANGE=.FALSE.
    DO I= 1, max_values-1, 2
        IF (random_n(I) .GT. random_n(I+1) ) THEN
            tmp_val= random_n(I)
            random_n(I)=random_n(I+1)
            random_n(I+1)=tmp_val
            XCHANGE=.TRUE.
        END IF
    END DO
        
    DO I= 2, max_values-2, 2
        IF (random_n(I) .GT. random_n(I+1) ) THEN
            tmp_val= random_n(I)
            random_n(I)=random_n(I+1)
            random_n(I+1)=tmp_val
            XCHANGE=.TRUE.
        END IF
    END DO    
END DO


END SUBROUTINE my_sort
END MODULE declarations_ordic
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

 READ (* , my_format_str_2 ) random_n(I)  ! Won't work if (samples_per_line != 1) !!!

END DO

PRINT *
CALL my_sort()

DO I = 1, total_lines
    WRITE (*, my_format_str_2 ) random_n(I) 
END DO

END PROGRAM sort_ordic    
