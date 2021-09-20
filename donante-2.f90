MODULE definiciones_V1_0

CHARACTER (LEN =40) :: prog_name

REAL (kind=8), PARAMETER  :: PI=4.D0 * DATAN(1.D0)
REAL (kind=8), PARAMETER  :: delta=1.D-06

REAL (kind=8)             :: delta_t

REAL(kind=8)   :: carrera
REAL(kind=8)   :: cig
REAL(kind=8)   :: bie
REAL (kind=8)  :: rpm
REAL (kind=8)  :: omega=0.0D0     ! Vel angular
REAL (kind=8)  :: omega_sqr=0.0D0 ! Vel angular al cuadrado

REAL(kind=8)   :: dist_al_pms ! Esta medida es cig + bie; es la distancia desde el centro
                              ! del cigüeñal al PMS

INTEGER (kind=8), PARAMETER ::Cant_Puntos=72+1  !Despues acomodamos y calculamos
                                                !Solucion transitoria y horrible!
INTEGER                     :: angulo_inic=180
INTEGER                     :: angulo_final=360
INTEGER                     :: increm_ang=5
INTEGER (kind=8)            :: puntos(Cant_Puntos)=0 
INTEGER (kind=8)            :: indice=0, indice_inical=0, indice_final=0


REAL(kind=8)                :: ang_deg (Cant_Puntos)=0.0
REAL(kind=8)                :: ang_rad (Cant_Puntos)=0.0
REAL(kind=8)                :: y_f_x_arr (Cant_Puntos)=0.0
REAL(kind=8)                :: y_prima_arr_an (Cant_Puntos)=0.0
REAL(kind=8)                :: y_segunda_arr_an (Cant_Puntos)=0.0

REAL(kind=8)                :: y_x_mas_d_arr (Cant_Puntos)=0.0
REAL(kind=8)                :: y_x_men_d_arr (Cant_Puntos)=0.0

REAL(kind=8)                :: y_prima_arr_num (Cant_Puntos)=0.0
REAL(kind=8)                :: y_segunda_arr_num (Cant_Puntos)=0.0


CONTAINS
!***********************************************************************
FUNCTION mi_funcion (x) RESULT (y)
IMPLICIT NONE
REAL(kind=8)  , INTENT (IN)       :: x
REAL(kind=8)                      :: y

y=cig*DCOS(x) + DSQRT(bie**2 - (cig * DSIN(x))**2) - dist_al_pms

RETURN
END FUNCTION mi_funcion
!***********************************************************************
FUNCTION derivada_1a (x) RESULT (y_prima)
IMPLICIT NONE
REAL(kind=8)  , INTENT (IN)             :: x
REAL(kind=8)                      :: y_prima
REAL(kind=8)                      :: alpha_rad

y_prima=-cig*DSIN(x)-( (cig**2 * DSIN (2.D0 *X)) / (2.D0 * DSQRT(bie**2-(cig*DSIN(X))**2)))


! La velocidad es y' por la velocidad angular...
! 1.0D-03 es para que sea en m/seg
! (Tendria que haber estudiado mas en la materia de Burman!)
! Se calcula en el programa principal.


RETURN
END FUNCTION derivada_1a
!***********************************************************************
FUNCTION derivada_2a (x) RESULT (y_segunda)
IMPLICIT NONE
REAL(kind=8)  , INTENT (IN)       :: x
REAL(kind=8)                      :: y_segunda
REAL(kind=8)                      :: y_segunda_f
REAL(kind=8)                      :: alpha_rad
REAL(kind=8)                      :: d
REAL(kind=8)                      :: den1,den2,num1,num2


d=bie**2-(cig*DSIN(x))**2

num1=  (4.D0 *DCOS(2.D0*X)) * d
num2=  (cig * DSIN(2.D0*X))**2
den1= 4.D0 * d
den2= DSQRT(d)

y_segunda= -cig*DCOS(x) -( cig**2 * (num1+num2) / (den1*den2) )

RETURN
END FUNCTION derivada_2a
!***********************************************************************
FUNCTION derivada_1a_num (x) RESULT (y_prima_num)
IMPLICIT NONE

REAL(kind=8)  , INTENT (IN)             :: x 
REAL(kind=8)                      :: y_prima_num

y_x_mas_d_arr(indice)=mi_funcion(x+delta)

y_x_men_d_arr(indice)=mi_funcion(x-delta)


!y_prima_num=(y_x_mas_d_arr(indice)-y_x_men_d_arr(indice)) / (2.0D0 * delta)

! Velocidad en m/seg
y_prima_num=(y_x_mas_d_arr(indice)-y_x_men_d_arr(indice)) / (delta_t) * 1.0D-03

END FUNCTION derivada_1a_num

!***********************************************************************
FUNCTION to_radians(angle_deg) RESULT (angle_rad)
IMPLICIT NONE

REAL(kind=8)  , INTENT (IN)       :: angle_deg
REAL(kind=8)                      :: angle_rad

angle_rad=angle_deg/180.D0*PI

END FUNCTION to_radians
!***********************************************************************
SUBROUTINE get_parms()
INTEGER             :: nargs
CHARACTER (LEN =40) :: buffer
INTEGER             ::first_char=0, last_char=0

nargs = COMMAND_ARGUMENT_COUNT ()
  buffer=""
  CALL GET_COMMAND_ARGUMENT(0,buffer)
  prog_name=TRIM(buffer)
  first_char=INDEX(prog_name,"/",.TRUE.)+1
  last_char=LEN_TRIM(prog_name)

!  prog_name=prog_name(first_char:last_char)
  
IF(nargs < 3) THEN
 PRINT *, prog_name(first_char:last_char)//":"
 
 PRINT *,"Uso: Carrera(mm) ddd.00, Long Biela(mm) ddd.00 , RPM ddddd.0"
 STOP
ELSE
  buffer=""
  CALL GET_COMMAND_ARGUMENT(1,buffer)
  buffer=TRIM(buffer)
  READ ( buffer ,FMT="(F8.2)") carrera
  cig=carrera / 2.0D0
  buffer=""
  CALL GET_COMMAND_ARGUMENT(2,buffer)
  buffer=TRIM(buffer)
  READ ( buffer,FMT ="(F8.2)") bie
  buffer=""
  CALL GET_COMMAND_ARGUMENT(3,buffer)
  buffer=TRIM(buffer)
  READ ( buffer,FMT ="(F8.1)") rpm
  
  
  
END IF


RETURN
END SUBROUTINE get_parms
!***********************************************************************
END MODULE definiciones_V1_0

!***********************************************************************
PROGRAM biela_M
USE definiciones_V1_0

IMPLICIT NONE

INTEGER         :: angulo
REAL(kind=8)    :: angulo_rad
REAL(kind=8)    :: y
REAL(kind=8)    :: y_prima
REAL(kind=8)    :: y_segunda


CALL get_parms ()

delta_t= (2.0D0 * delta * 30.0D0) / (rpm * PI)

omega= (rpm * PI / 30.0D0) * 1.0D-3
omega_sqr= omega * omega

dist_al_pms= cig + bie

! PRINT * , "Menor Numero: " , TINY(y), delta

WRITE(*,FMT="('Carrera(mm): ', F7.2,2X, 'Long Biela(mm): ',F7.2,2X,'RPM: ',F7.1)") carrera,bie,rpm

!PRINT *, "Carrera: ", carrera , "Long Biela: " , bie
!PRINT * , "RPM: ", rpm, "delta: ", delta, "delta_t: ", delta_t

indice=0
DO angulo=angulo_inic, angulo_final,increm_ang

    ang_deg(indice)=angulo
    angulo_rad=to_radians(DBLE(angulo))
    
    ang_rad(indice)=angulo_rad !Todavia no se para que hago esto!

    y=mi_funcion(angulo_rad)
    y_f_x_arr(indice)=y
  
    y_prima=derivada_1a(angulo_rad)
    y_prima_arr_an(indice)=y_prima * omega
    
    y_segunda=derivada_2a(angulo_rad)
    y_segunda_arr_an(indice)=y_segunda * omega_sqr
    
    y_prima_arr_num(indice)=derivada_1a_num(angulo_rad)
    
    indice=indice+1
    

END DO

indice_final=indice-1

DO indice=indice_inical, indice_final,1

  !PRINT *, angulo, y , y_prima, y_segunda
! PRINT * , ang_deg(indice), y_f_x_arr(indice),y_prima_arr_an(indice),y_prima_arr_num(indice),y_segunda_arr_an(indice)
WRITE (*,FMT="(F6.1,2X,3(F6.2,2X))")ang_deg(indice),y_f_x_arr(indice),y_prima_arr_an(indice),y_segunda_arr_an(indice)

!  PRINT * , ang_deg(indice), y_f_x_arr(indice),y_prima_arr_an(indice),y_prima_arr_num(indice), &
!   y_prima_arr_num(indice)/y_prima_arr_an(indice)
   
!     PRINT * , ang_deg(indice), y_f_x_arr(indice),y_prima_arr_an(indice),y_prima_arr_num(indice)

!  PRINT * , ang_deg(indice), y_f_x_arr(indice),y_prima_arr_an(indice),y_prima_arr_num(indice), &
! ( (y_prima_arr_an(indice)-y_prima_arr_num(indice))*100.0D0/y_prima_arr_an(indice))

END DO

END PROGRAM biela_M


