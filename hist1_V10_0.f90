MODULE declaraciones_V10_0

! En la version 9 llevamos todos los datos a la subrutina
! de la determinacion de los valores que caen el los intervalos
! Los datos van al vector random_n(Muestras_x_lote)


INTEGER (kind=8), PARAMETER ::Cant_Interv=10
INTEGER (kind=8), PARAMETER ::Cant_lotes=1000
INTEGER (kind=8), PARAMETER ::Cant_interv_histogr=7
INTEGER (kind=8), PARAMETER ::Muestras_x_lote=20000000
INTEGER (kind=4), PARAMETER ::Max_time_points=10

INTEGER (kind=4)          :: etime (0:Max_time_points)=0
INTEGER (kind=4)          :: etime_p (0:Max_time_points)=0

INTEGER (kind=8)          :: freq_count(Cant_lotes,Cant_Interv)=0
INTEGER (kind=8)          :: Min_frec_x_interv(Cant_Interv)=0
INTEGER (kind=8)          :: Max_frec_x_interv(Cant_Interv)=0
INTEGER (kind=8)          :: Distrib_frec(Cant_interv_histogr,Cant_Interv)=0
INTEGER (kind=8)          :: Limite_histograma(Cant_interv_histogr)=0
INTEGER (kind=8)          :: Intervalo, Lote , Middle_interv, Start_interv 
INTEGER (kind=8)          :: Max_abs, Min_abs, Interval_size_Histogr

                             
                               
REAL (kind=8), PARAMETER  :: Val_Max=1.0
REAL (kind=8), PARAMETER  :: Val_Min=0.0                            
REAL(kind=8)              :: Interv_Limit (0:Cant_Interv)=0.0, Interval_Size

REAL(kind=8)              :: Time_dif (Max_time_points)=0.0
REAL(kind=8)              :: Time_dif_p (Max_time_points)=0.0
! ***********************************************************

CONTAINS
SUBROUTINE Determinar_Intervalo ()


REAL(kind=8)					:: random_n (Muestras_x_lote)
INTEGER  (kind=8)				:: Interv

INTEGER (kind=8) 				:: I , J , K


	
DO I = 1, Cant_lotes
CALL take_time_p (0)		! <<-------------------------------- TIME	
! !$opm parallel do
    DO J = 1 , Muestras_x_lote
        CALL RANDOM_NUMBER(random_n(J))
    END DO     
! !$omp end parallel do

CALL take_time_p (1)		! <<-------------------------------- TIME	
	
	DO J = 1 , Muestras_x_lote
		Start_interv= Middle_interv

		IF ( random_n(J) .LE. Interv_Limit(Start_interv) ) THEN
			Start_interv = Start_interv -1
			DO K = Start_interv , 0, -1
				IF ( random_n(J) .LE. Interv_Limit(K) ) THEN
					CYCLE
				ELSE 
					Interv=K+1
					EXIT
				END IF
			END DO
		ELSE
			DO K = Start_interv+1, Cant_Interv
				IF ( random_n(J) .LE. Interv_Limit(K) ) THEN
				 Interv=K
				 EXIT
				END IF
			END DO
		END IF
		freq_count(I, Interv) = freq_count(I, Interv)+1
    END DO
    
END DO
CALL take_time_p (2)   ! <<-------------------------------- TIME
RETURN
END SUBROUTINE Determinar_Intervalo

! ****************************************************************
SUBROUTINE Determinar_Max_Min_Intervalo (Interv,Max_Intervalo, Min_Intervalo)

INTEGER  (kind=8), INTENT (IN) :: Interv
INTEGER  (kind=8), INTENT (OUT) :: Max_Intervalo, Min_Intervalo

INTEGER (kind=8) :: I

Max_Intervalo=freq_count(1,Interv)
Min_Intervalo=freq_count(1,Interv)

DO I = 2, Cant_lotes
	IF ( freq_count(I, Interv) .GT. Max_Intervalo ) THEN
	   Max_Intervalo= freq_count(I, Interv) 
	ELSE
	   IF ( freq_count(I, Interv) .LT. Min_Intervalo ) THEN
	     Min_Intervalo= freq_count(I, Interv) 
	   END IF
	END IF
END DO

RETURN
END SUBROUTINE Determinar_Max_Min_Intervalo 

!! *****************************************
SUBROUTINE determinar_histograma ()

INTEGER (kind=8) 			:: I, J , Interv,  Numero_de_Lote

DO Interv= 1, Cant_Interv
		DO Numero_de_Lote = 1, Cant_lotes
! Determina en que intervalo del histograma cae cada valor	           
		           DO I = 1, Cant_interv_histogr
		           IF (freq_count(Numero_de_Lote,Interv) .LE. Limite_histograma(I) ) THEN
						Distrib_frec(I,Interv)=Distrib_frec(I,Interv) + 1 
						EXIT  
				   END IF
				   END DO      
		END DO
END DO

END SUBROUTINE determinar_histograma

!! *****************************************

SUBROUTINE Take_time ( Point_nbr )
IMPLICIT NONE

INTEGER  (kind=4), INTENT (IN) 		:: Point_nbr
INTEGER (kind=4)    				:: cnt, cnt_rate, cnt_max

call system_clock (cnt, cnt_rate, cnt_max)
etime(Point_nbr)=cnt

!IF ( Point_nbr .EQ. 0 ) THEN
!    etime(Point_nbr)=cnt
!    Time_dif (Point_nbr)= (cnt / cnt_rate)
!    return
!ELSE
!    etime(Point_nbr)=cnt - etime(Point_nbr-1)
!    Time_dif (Point_nbr)= (cnt / cnt_rate) - Time_dif (Point_nbr - 1)
!    Time_dif (Point_nbr)= (cnt ) - Time_dif (Point_nbr - 1)
!END IF
RETURN

END SUBROUTINE Take_time

!! *****************************************

SUBROUTINE Take_time_p ( Point_nbr )
IMPLICIT NONE

INTEGER  (kind=4), INTENT (IN) 		:: Point_nbr
INTEGER (kind=4)    				:: cnt, cnt_rate, cnt_max

call system_clock (cnt, cnt_rate, cnt_max)
etime_p(Point_nbr)=cnt

RETURN

END SUBROUTINE Take_time_p


!! *****************************************
SUBROUTINE print_time ()

INTEGER (kind=4)    :: I

DO I= 1, Max_time_points
!	IF ( Time_dif(I) .EQ. 0.0 ) THEN
	IF ( etime(I) .EQ. 0.0 ) THEN
		   RETURN
	ELSE
!	  WRITE (*,FMT='(A,I3,2X,I10,2X,F10.3)') 'Time_point: ', I , etime(I), Time_dif_p(I)
      WRITE (*,FMT='(A,I3,2X,I16,2X,I16,2x,I16)') 'Time_point: ', I , etime(I-1), etime(I), (etime(I)-etime(I-1))
	  CONTINUE  
	ENDIF
END DO

RETURN

END SUBROUTINE print_time

!! *****************************************
SUBROUTINE print_time_p ()

INTEGER (kind=4)    :: I

DO I= 1, Max_time_points
	IF ( etime_p(I) .EQ. 0.0 ) THEN
		   RETURN
	ELSE
      WRITE (*,FMT='(A,I3,2X,I16,2X,I16,2x,I16)') 'Time_point_p: ', I , etime_p(I-1), etime_p(I), (etime_p(I)-etime_p(I-1))
	  CONTINUE  
	ENDIF
END DO

RETURN

END SUBROUTINE print_time_p


END MODULE declaraciones_V10_0


!! ********************************************************************
!! ********************************************************************
PROGRAM hist1_V10_0
USE declaraciones_V10_0

IMPLICIT NONE


INTEGER (kind=8) 			:: I, J , Interv, Max_Intervalo, Min_Intervalo, Numero_de_Lote

CHARACTER(len=100)        	:: Prog_name
REAL(kind=8)             	:: r 			! Random number

call get_command_argument (0, Prog_name)
print *, "Program name:  ", TRIM (Prog_name)


WRITE (*,FMT='(A,I8,2x,A,I8)') 'Cant lotes:  ', Cant_lotes,  'Muestras x Lote:        ' , Muestras_x_lote
WRITE (*,FMT='(A,I8,2X,A,I8)') 'Cant Interv: ', Cant_Interv, 'Cant Interv Histograma: ' , Cant_interv_histogr

CALL take_time (0) 				 ! <<-------------------------------- TIME

Interval_Size= (Val_Max-Val_Min)/ DBLE(Cant_Interv)
Middle_interv= Cant_Interv / 2
Interv_Limit (0) = Val_Min

! PRINT * , Start_interv

DO I = 1, Cant_Interv
    Interv_Limit(I)= Val_Min + (Interval_Size * DBLE(I))
END DO


!DO I = 1, Cant_lotes
!    Do J = 1 , Muestras_x_lote
!          CALL RANDOM_NUMBER(r)
          CALL Determinar_intervalo ()
!          freq_count(I, interv) = freq_count(I,interv)+1
!    END DO
!END DO

CALL take_time (1)		! <<-------------------------------- TIME
CALL take_time (2)		! <<-------------------------------- TIME Truchada para no cambiar orden

DO I = 1 , Cant_lotes
	WRITE (*,FMT='(10(I8,2X))') Freq_count(I,:)
END DO

CALL take_time (3)		! <<-------------------------------- TIME
! Max_frec_x_interv=MAXVAL(freq_count,2)

DO Intervalo= 1 , Cant_Interv
    CALL Determinar_Max_Min_Intervalo (Intervalo,Max_Intervalo,Min_Intervalo)
    Max_frec_x_interv(Intervalo)= Max_Intervalo
    Min_frec_x_interv(Intervalo)= Min_Intervalo
END DO
CALL take_time (4)		! <<-------------------------------- TIME

PRINT *
WRITE (*,FMT='(10(I8,2X))') Max_frec_x_interv
WRITE (*,FMT='(10(I8,2X))') Min_frec_x_interv

CALL take_time (5)		! <<-------------------------------- TIME

Max_abs=MAXVAL(Max_frec_x_interv,1)
Min_abs=MINVAL(Min_frec_x_interv,1)

CALL take_time (6)		! <<-------------------------------- TIME

WRITE (*,FMT='(2(I8,2X))') Max_abs,Min_abs

Interval_size_Histogr=(Max_abs - Min_abs) / Cant_interv_histogr

Limite_histograma(Cant_interv_histogr)= Max_abs  ! Este intervalo sera mas grande que los otros
												 ! Por ahora...

DO I = 1, Cant_interv_histogr-1
    Limite_histograma(I)= Min_abs + (Interval_size_Histogr * I)
END DO


CALL determinar_histograma  ()
    
CALL take_time (7)		! <<-------------------------------- TIME



print *, "Program name:  ", TRIM (Prog_name)

WRITE (*,FMT='(A,I8,2x,A,I8)') 'Cant lotes:  ', Cant_lotes,  'Muestras x Lote:        ' , Muestras_x_lote
WRITE (*,FMT='(A,I8,2X,A,I8)') 'Cant Interv: ', Cant_Interv, 'Cant Interv Histograma: ' , Cant_interv_histogr

CALL print_time ()

CALL print_time_p ()

PRINT *

DO I = 1 , Cant_interv_histogr 
!	DO J = 1 , Cant_Interv
!		WRITE (*,FMT='(I5,2X)', ADVANCE ="NO")  Distrib_frec (   I   ,    J  )
		WRITE (*,FMT='(10(I5,2X))')  Distrib_frec (   I   ,    1:Cant_Interv  )
!        print *, Distrib_frec (   I   ,    :  )
!	END DO
	PRINT *
END DO

END PROGRAM hist1_V10_0

