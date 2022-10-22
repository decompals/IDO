        Program ART
	real*4 t1,t2(2),t3(2)

	n = 40
	m = 102

        t1 = etime(t2)
	do i = 1, 500
	    call ARTDIF( n, m)
        enddo

        t1 = etime(t3)       
	print *, "Time = " , t3(1) - t2(1)
	stop
	end


	SUBROUTINE ARTDIF(NQ, MQ1)
*
*
***********************************************************************
*
*     IN THIS SUBROUTINE THE ARTIFICIAL
*     DIFFUSION COEFFICENT IS COMPUTED
*
***********************************************************************
*
*     VALUES REQUIRED:
*        -  VCT
*        -  DT, DZ,DR
*        -  VZ1,VR2       ( VELOCITY AT STAGGERED GRIDS     )
*     VALUES COMPUTED:
*        -  VC1,VC2       ( AVERAGED DIFFUSION COEFFICENTS  )
*
***********************************************************************
*
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
C      IMPLICIT REAL*8 (A-H, O-Z)
*
*     In the mainframe version, this was done by compiler switch
*
*
      PARAMETER  ( MP   =      102 ,    NP   =       40 )
      PARAMETER  ( M1   =   MP - 1 ,   N1   =   NP - 1 )
*
      COMMON /PRMT/   TS, DT, GAM, CFL,VCT,
     &                TZ,TR, FTZ,FTR, BSZ,BSR, QTZ,QTR, IS
      COMMON /ADVC/   ROBMQ, ROBNQ,
     &                MQFLG, NQFLG
      COMMON /GRID/   Z(MP), ZB(0:MP), DZ(MP), DBZ(0:MP), FZ(M1),
     &                R(NP), RB(0:NP), DR(NP), DBR(0:NP), FR(N1),
     &                DUM(8)
* Dum is a dummy             a.w.
* DU3 is a dummy             a.w.
      COMMON /VARH/   VZ1(0:MP,NP), VR2(MP,0:NP),
     &                VC1(0:MP,NP), VC2(MP,0:NP),
     &                DU3(288)
C$$$NEW BY O.HAAN, 6.4.87
      DIMENSION DRIN(NP),DZIN(MP)
C$$$NEW
C$$$ MOVE VCT*DT/DZ(I) AND VCT*DT/DR(J) OUT OF LOOPS
C$$$
*
*
***********************************************************************
*     COMPUTE Z-COMPONENT
***********************************************************************
*
*
      DO  100  J = 1,NQ
      DO  100  I = 1,MQ1
         VC1(I,J) =        DZIN(I) *
     &                  (  ABS( VZ1(I+1,J) - VZ1(I-1,J) )     +
     &                     0.5D0 * ABS(   VR2(I+1,J) + VR2(I+1,J-1)
     &                                - VR2(I  ,J) - VR2(I  ,J-1) )  )
  100 CONTINUE
	return
	end
