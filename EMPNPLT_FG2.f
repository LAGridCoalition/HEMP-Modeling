C     PROGRAM CONTRL (INPUT,OUTFLT,PLOT)                                CNTL  10
      PROGRAM CONTRL                                                    CNTL  11
C                                                                       CNTL  20
C     THIS PROGRAM CCNTROLS THE SUBROUTINES                             CNTL  30
C                                                                       CNTL  40
       INTEGER OUX                                                      CNTL  60
       COMMON OUX, AP, BP, RNP, TOP                                     CNTL  50
       DIMENSION E(192),TIMX(192),STORE2(500)                           CNTL  70
C                                                                       CNTL  80
C      X,Y,Z IS THL TARGET LCCATION IN METERS                           CNTL  90
C           FOR THE NORTHERN HEMISPHERE                                 CNTL 100
C           X IS MAGNETIC WEST                                          CNTL 110
C           Y IS MAGNETIC SOUTH                                         CNTL 120
C           Z IS ALTITUDE                                               CNTL 130
C                                                                       CNTL 140
C      HOB IS HEIGHT CF BURST IN KILOMETERS > 50KM                      CNTL 150
C                                                                       CNTL 160
C      GAMYLD IS GAMMA YIELD OF BURST IN KILOTONS                       CNTL 170
C                                                                       CNTL 180
C      BFIELD IS THE MAGNITLDE OF EARTHS MAGNETIC FIELD IN THE          CNTL 190
C      ABSORPTICN REGICN BELOW THE BURST IN WEBERS/SQUARE METER         CNTL 200
C                                                                       CNTL 210
C      BANGLE IS THE DIP ANGLE OF THE MAGNETIC FIELD IN DEGREES         CNTL 220
C                                                                       CNTL 230
C      NDELR IS NUMBER OF STEPS BETWEEN RMIN AND RMAX                   CNTL 240
C      50<=NDELR<=500                                                   CNTL 250
C                                                                       CNTL 260
C      OUX IS THE OUTPUT CONTROL PARAMETER                              CNTL 270
C      OUX=0 ==> PRINT PEAK VALUE AND ARRAYS                            CNTL 280
C      OUX=1 ==> PRINT PEAK VALUE AND MAKE PLOT                         CNTL 290
C      OUX=2 ==> PRINT EVERYTHING AND MAKE PLOT                         CNTL 300
C      OUX=3 ==> PRINT EVERYTHING                                       CNTL 310
C                                                                       CNTL 320
       READ 1001,X,Y,Z,HOB,GAMYLD,BFIELD,BANGLE,NDELR,OUX               CNTL 330
       R=SQRT(X*X+Y*Y+(HOB*1000.-Z)**2)                                 CNTL 340
       PRINT 2006,GAMYLD,HOB,X,Y,Z,R                                    CNTL 350
C                                                                       CNTL 360
C         SET UP DEFAULT VALUES                                         CNTL 370
C                                                                       CNTL 380
      IF(BANGLE.EQ.0.) BANGLE=40.                                       CNTL 390
      IF(BFIELD.EQ.0.) BFIELD=0.00002                                   CNTL 400
      IF(NDELR.EQ.0) NDELR=50                                           CNTL 410
C                                                                       CNTL 420
C         CONVERT DATA TO MKS UNITS                                     CNTL 430
C                                                                       CNTL 440
      HOB=HOB*1000.                                                     CNTL 450
      GAMYLD=2.61625E25*GAMYLD                                          CNTL 460
      BANGLE=0.017453295*BANGLE                                         CNTL 470
      OMEGA=1.6E-19*BFIELD/(3.505*9.11E-31)                             CNTL 480
C                                                                       CNTL 490
C         PRINT TYPE OF CALCULATICh                                     CNTL 500
C                                                                       CNTL S10
      REFLCT=49000.                                                     CNTL 520
      IF(Z.GT.REFLCT) PRINT 2007                                        CNTL 530
      IF(Z.LT.0.0) PRINT 2008                                           CNTL 540
      IF(Z.LE.REFLCT.AND.Z.GE.0.0) PRINT 2009                           CNTL 550
C                                                                       CNTL 560
C         REFLECTED WAVE CALCULATICN ASSUMES 100% REFLECTION            CNTL 570
C         AND USES MIRROR IMAGE OF TARGET BELOW GROUND                  CNTL 580
C         SET Z = -Z IF REFLECIED WAVE IS TO BE USED                    CNTL 590
C                                                                       CNTL 600
      IF(Z.GT.REFLCT) Z=-Z                                              CNTL 610
      IF(Z.GT.HOB-1000.) PRINT 2007                                     CNTL 620
      IF(Z.GT.HOB-1000.) Z=-Z                                           CNTL 630
C                                                                       CNTL 640
C        DETERMINE ANGLES                                               CNTL 650
C                                                                       CNTL 660
      R=SQRT(X*X+Y*Y+(HOB-Z)**2)                                        CNTL 670
      A=ACOS((HOB-Z)/R)                                                 CNTL 680
      THETA=ACOS(COS(BANGLE)*Y/R+SIN(BANGLE)*(Z-HOB)/R)                 CNTL 690
C                                                                       CNTL 700
C          DETERMINE RMIN AND RMAX                                      CNTL 710
C                                                                       CNTL 720
      ZRMIN=5.E4                                                        CNTL 730
      IF(HOB.LT.ZRMIN) ZRMIN=HOB                                        CNTL 740
      TA=(ZRMIN-HOB)/(Z-HOB)                                            CNTL 750
      XRMIN=TA*X                                                        CNTL 760
      YRMIN=TA*Y                                                        CNTL 770
      RMIN=SQRT(XRMIN**2+YRMIN**2+(ZRMIN-HOB)**2)                       CNTL 780
      ZRMAX=Z                                                           CNTL 790
      IF(Z.LT.2.E4) ZRMAX=2.E4                                          CNTL 800
      TB=(ZRMAX-HOB)/(Z-HOB)                                            CNTL 810
      XRMAX=TB*X                                                        CNTL 820
      YRMAX=TB*Y                                                        CNTL 830
      RMAX=SQRT(XRMAX**2+YRMAX**2+(ZRMAX-HOB)**2)                       CNTL 840
C                                                                       CNTL 850
C         CALCULATE EFIELD AT BOTTOM OF ABSORBTICN REGION               CNTL 860
C                                                                       CNTL 870
      CALL EFIELD(E,TIMX,RMIN,RMAX,NDELR,HOB,A,THETA,OMEGA,GAMYLD,      CNTL 889
     1STORE2,NMAX)                                                      CNTL 891
C                                                                       CNTL 900
C          CALCULATE EFIELD AT TARGET                                   CNTL 910
C                                                                       CNTL 929
       IF(R.LE.RMAX) GOTO 3                                             CNTL 930
       DO 1 I=1,190                                                     CNTL 940
 1     E(I)=E(I)*RMAX/R                                                 CNTL 950
C                                                                       CNTL 960
C          FIND PEAK VALUE OF EFIELD                                    CNTL 970
C                                                                       CNTL 980
 3     BIG=E(1)                                                         CNTL 991
       IT=1                                                             CNTL 992
       DO 2 I=2,190                                                     CNTL1001
       IF(E(I).LT.BIG) GOTO 2                                           CNTL1011
       BIG=E(I)                                                         CNTL1022
       IT=I                                                             CNTL1023
 2     CONTINUE                                                         CNTL1030
C                                                                       CNTL1040
C          PRINT OUTPUT                                                 CNTL1050
C                                                                       CNTL1060
      PRINT 2010,TIMX(IT), IT                                           CNTL1071
      PRINT 2005,BIG                                                    CNTL1080
      PRINT 2001                                                        CNT11090
      PRINT 2002,(TIMX(I),I=1,NMAX)                                     CNTL1101
      PRINT 2003                                                        CNTL1110
      PRINT 2004,(E(I),I=1,NMAX)                                        CNTL1121
C                                                                       CNTL1130
C           IF DESIRED, MAKE PLOT                                       CNTL1140
C                                                                       CNTL1150
      IF(OUX.LE.0.OR.OUX.GE.3) STOP                                     CNTL1160
C     CALL ELGPLT (E,TIMX,BIG)                                          CNTl1170
      STOP                                                              CNTL1180
 1001 FORMAT(7F10.0,2I5)                                                CNTL1190
 2010 FORMAT(//5X,"PEAK OCCURRED AT",1PE11.4," SHAKES",4X'Step Number:',CNTL1201
     11X, I4)
 2009 FORMAT(5X,"DIRECT WAVE IS BEING CALCULATED"/////)                 CNTL1210
 2008 FORMAT(5X,"REFLECTED WAVE IS BEING CALCULATED"/////)              CNTL1220
 2007 FORMAT(5X,"TARGET IS ABOVE ABSORPTION REGION SO REFLECTED WAVE IS CNTL1230
     1BEING CALCULATED"/////)                                           CNTL1240
 2006 FORMAT("1    THE BLAST WITH GAMMA YIELD OF",1PE10.3," KILOTCNS"   CNTL1250
     1    /5X,"IS AT AN ALTITUDE OF",1PE10.3," KILOMETERS."             CNTL1260
     2    //5X,"THE TARGET IS AT COORDINATES",3(5X,1PE10.3)             CNTL1270
     3     /5X,"WHICH IS",1PE10.3," METERS FROM THE BURST"/////)        CNTL1280
 2005 FORMAT(//5X,"* * * * * * * * * * * * * * * * * * * * * * * * * *" CNTL1290
     1      /5X,"* PEAK EFIELD AT TARGET IS ",1PE10.3," VOLTS/METER *"  CNTL1300
     2      /5X,"* * * * * * * * * * * * * * * * * * * * * * * * * *"//)CNTL1310
 2004 FORMAT(19(10(3X,1PE10.3)/))                                       CNTL1320
 2003 FORMAT(///5X,"EFIELD VALUES AT TARGET (IN V/M) ARE"//)            CNTL1330
 2002 FORMAT(19(10(4X,F5.1,4X)/))                                       CNTL1340
 2001 FORMAT("1  TIMES USED (IN SHAKES) ARE"//)                         CNTL1350
      END                                                               CNTL1360
C
C
      SUBROUTINE EFIELD(E,TIMX,RMIN,RMAX,NDELR,HOB,A,THETA,OMEGA,GAMYLD,EFLD  10
     1STORE2,NMAX)                                                      EFLD  21
C                                                                       EFLD  30
C                                                                       EFLD  40
C          CALCULATE THE EFIELD IN THE ABSORPTION REGION                EFLD  50
C                                                                       EFLD  60
      DIMENSION E(190),TIMX(190),STORE2(500)                            EFLD  71
      REAL JTHETA,JPHI                                                  EFLD  80
      INTEGER OUX                                                       EFLD  90
      COMMON OUX,AP,BP,RNP,TOP                                          EFLD 100
C                                                                       EFLD 110
C     ITER IS TIME OF ITERATION IN SHAKES 10<=ITER<=100                 EFLD 120
C     READ ITER AND CHANGE IT TO NUMBER OF TIME STEPS                   EFLD 130
C                                                                       EFLD 140
      READ 100,ITER                                                     EFLD 150
      ITER=100+(ITER-10)                                                ELFD 160
C                                                                       EFLD 170
C         INITIALIZE ARRAYS AND CONSTANTS                               EFLD 180
C                                                                       EFLD 190
      READ 101,AP,BP,RNP,TOP                                            EFLD 200
  101 FORMAT(4F10.0)                                                    EFLD 210
      DO 61 J=1,100                                                     EFLD 220
      E(J)=0.0                                                          EFLD 230
      TIMX(J)=0.1*J                                                     EFLD 231
  61  CONTINUE                                                          EFLD 240
      DO 71 J=101,190                                                   EFLD 250
      E(J)=0.0                                                          EFLD 260
      TIMX(J)=10.+(J-100.)                                              EFLD 261
  71  CONTINUE                                                          EFLD 270
      DO 51 L=1,NDELR                                                   ELFD 280
      STORE2(L)=0.                                                      EFLD 281
  51  CONTINUE                                                          EFLD 290
      ETHE=0.                                                           EFLD 300
      DELRN=NDELR                                                       EFLD 301
      T=0.                                                              EFLD 302
      DT=1.0                                                            EFLD 303
      EPHI=0.                                                           EFLD 304
      DELTAR=(RMAX-RMIN)/DELRN                                          EFLD 310
      R=RMIN+DELTAR                                                     EFLD 311
      RNP=1.E-8*RNP                                                     EFLD 312
C                                                                       EFLD 320
C          START INTEGRATIONS                                           EFLD 330
C          OUTSIOE LOOP IS FOR CALCULATION IN RETARDED TIME             EFLD 340
C          INSIDE LOOP IS FOR INTEGRATION IN R AT EACH TIME STEP        EFLD 350
C                                                                       EFLD 360
      DO 21 I=1,190                                                     EFLD 370
      T=T+(1.E-9)*DT                                                    EFLD 371
      TIMX(I)=T*(1.E8)                                                  EFLD 372
      IT=I                                                              EFLD 380
      IF(I.GT.ITER) GOTO 42                                             EFLD 390
      TP=-DELTAR/2.88E8                                                 EFLD 400
      DTP=TP                                                            EFLD 401
      SIGMA=0.                                                          EFLD 402
      STORE1=0.                                                         EFLD 403
      DO 31 K=1,NDELR                                                   EFLD 410
      CALL COMPTN(JTHETA,JPHI,T,R,A,THETA,OMEGA,HOB,GAMYLD,TP,PRI,PRI2) EFLD 420
      CALL CONDCT(SIGMA,PRI,DTP,DT,HOB,R,A,STORE1,STORE2,K,NDELR,PRI2)  EFLD 430
      CALL RNGKUT(ETHENW,ETHE,R,DELTAR,SIGMA,JTHETA)                    EFLD 440
      CALL RNGKUT(EPHINW,EPHI,R,DELTAR,SIGMA,JPHI)                      EFLD 450
      ETHE=ETHENW                                                       EFLD 460
      EPHI=EPHINW                                                       EFLD 461
      R=R+DELTAR                                                        EFLD 462
      TP=TP+DTP                                                         EFLD 470
 31   CONTINUE                                                          EFLD 480
      NMAX=IT
C                                                                       EFLD 490
C          FIND MAGNITUDE CF EFIELD                                     EFLD 500
C                                                                       EFLD 510
      E(I)=SQRT(ETHE**2+EPHI**2)                                        EFLD 520
C                                                                       EFLD 530
C          CHECK FOR DIVERGENCE OF SOLUTION                             EFLD 540
C                                                                       EFLD 550
      IF(E(I) .GT. 1.E15) GOTO 52                                       EFLD 560
      IF (I.EQ.100) DT=10.                                              EFLD 570
      R=RMIN+DELTAR                                                     EFLD 580
C                                                                       EFLD 590
C         IF DESIRED, PRINT OUTPUT                                      EFLD 600
C                                                                       EFLD 610
      IF (OUX-1 .LE. 0) GOTO 21                                         EFLD 620
      PRINT 5,I,TIMX(I),E(I),SIGMA                                      EFLD 630
 21   CONTINUE                                                          EFLD 640
C                                                                       EFLD 650
C         PRINT MESSAGE AFTER TERMINATION OF TIME LOOP                  EFLD 660
C                                                                       EFLD 670
 42   PRINT 201,TIMX(ITER)                                              EFLD 680
      RETURN                                                            EFLD 690
C                                                                       EFLD 700
C          PRINT MESSAGE AFTER ABNORMAL TERMINATICN OF TIME LOOP        EFLD 710
C                                                                       EFLD 720
 52   PRINT 301                                                         ELFD 730
      PRINT 201,TIMX(IT)                                                EFLD 740
      IF(IT.LT.10)RETURN                                                EFLD 750
C                                                                       EFLD 760
C         SET LAST 5 VALUES OF EFIELD TO 0.0 TO AVOID INCORRECT PEAK    EFLD 770
C                                                                       EFLD 780
      E(IT)=0.0                                                         EFLD 790
      E(IT-1)=0.0                                                       EFLD 791
      E(IT-2)=0.0                                                       EFLD 792
      E(IT-3)=0.0                                                       EFLD 793
      E(IT-4)=0.0                                                       EFLD 794
      E(IT-5)=0.0                                                       EFLD 795
      RETURN                                                            EFLD 800
 100  FORMAT (I3)                                                       EFLD 810
 5    FORMAT("   I =",I4,"     TIME =",F6.1," SHAKES     E(T,RMAX) =",  EFLD 820
     11PE10.3," VOLTS/METER     SIGMA =",1PE10.3," MHO/METER")          EFLD 830
 201  FORMAT(//5X,"ITERATION TERMINATED AFTER",1PE11.4, " SHAKES"//)    EFLD 840
 301  FORMAT(//15X,"*****"/15X,"***** SOLUTICN HAS GONE UNSTABLE"       EFLD 850
     1/15X,"*****"//)                                                   EFLD 860
      END                                                               EFLD 870
C
C
      SUBROUTINE CONDCT(SIGMA,PRI,DTP,DT,HOB,R,A,STORE1,STORE2,K,NDELR, CNCT  10
     1PRI2)                                                             CNCT  20
C                                                                       CNCT  30
C         CALCULATES SIGMA AFTER FINDING                                CNCT  40
C         NSECCNOARY FROM NPRIMARV                                      CNCT  50
C                                                                       CNCT  60
C         STORE1 CCNTAINS INTEGRAL FOR NEGATIVE TAU                     CNCT  70
C         STORE2 CCNTAINS INTEGRAL FOR POSITIVE TAU                     CNCT  80
C                                                                       CNCT  90
      DIMENSION STORE2(NDELR)                                           CNCT 100
      COLISN=4.E12*EXP((R*COS(A)-HOB)/7000.)                            CNCT 110
      STORE1=STORE1-PRI*DTP                                             CNCT 120
      STORE2(K)=STORE2(K)+PRI2*DT*(1.0E-9)                              CNCT 130
      SEC=STORE2(K)-STORE1                                              CNCT 140
      SIGMA=(1.6E-19**2)*SEC/(COLISN*9.11E-31)                          CNCT 150
      RETURN                                                            CNCT 160
      END                                                               CNCT 161
C
C
      SUBROUTINE COMPTN(JTHETA,JPHI,T,R,A,THETA,OMEGA,HOB,GAMYLD,TP,PRI,CMTN  10
     1PRI2)                                                             CMTN  20
C                                                                       CMTN  30
C         CALCULATE THE TWO COMPONENTS OF THE                           CMTN  40
C         COMPTON CURRENT AT GIVEN T AND R                              CMTN  50
C         CALCULATE NUMBER OF PRINARY ELECTRCNS                         CMTN  60
C                                                                       CMTN  70
C         JTHETA IS THETA CONPCNENT CF COMPTON CLRRENT                  CMTN  80
C         JPHI IS PHI COMPONENT CF COMPTON CURRENT                      CMTN  90
C         TMAX IS COMPTON LIFETIME                                      CMTN 100
C         PATH IS ALTITUDE SCALED COMPTON MEAN FREE PATH                CMTN 110
C         TP IS NEGATIVE RETARDED TIME                                  CHTN 120
C         T IS POSITIVE RETARDED TIME                                   CHTN 130
C         PRI IS NUMBER CF PRIMARY ELECTRONS GENERATED DURING TP        CMTN 140
C         PRI2 IS NUMBER CF PRIMARY ELECTRONS GENERATED DURING T        CMTN 150
C         TPRIME IS VARIABLE OF INTEGRATION                             CMTN 160
C                                                                       CMTN 170
C         INITIALIZE CONSTANTS                                          CMTN 180
C                                                                       CMTN 190
      REAL JTHETA,JPHI                                                  CMTN 200
      JTHETA=0.                                                         CMTN 210
      JPHI=0.                                                           CMTN 211
      TPRIME=(5.E-9)                                                    CMTN 212
      TMAX=CLIFE(R,A,HOB)                                               CMTN 213
      PRI=0.                                                            CMTN 214
      DT=TMAX/10.                                                       CMTN 220
      PATH=309.*EXP((HOB-R*COS(A))/7000.)                               CMTN 221
      TPRIME=DT                                                         CMTN 222
      W=DT/2.                                                           CMTN 223
      PRI2=0.                                                           CMTN 230
C                                                                       CMTN 240
C         RUNGE-KUTTA INTEGRATICN CF COMPTON CURRENT                    CMTN 250
C                                                                       CMTN 260
      DO 31 K=1,10                                                      CMTN 270
      RK1=DT*CMTHET(HOB,R,A,THETA,OMEGA,PATH,T,TPRIME,GAMYLD)           CMTN 280
      RK2=DT*CMTHET(HOB,R,A,THETA,OMEGA,PATH,T,TPRIME+W,GAMYLD)         CMTN 290
      RK3=RK2                                                           CMTN 300
      RK4=DT*CMTHET(HOB,R,A,THETA,OMEGA,PATH,T,TPRIME+DT,GAMYLD)        CMTN 310
      JTHETA=JTHETA+(RK1+2.*(RK2+RK3)+RK4)/6.                           CMTN 320
      RK5=DT*CMPHI (HOB,R,A,THETA,OMEGA,PATH,T,TPRIME,GAMYLD)           CMTN 330
      RK6=DT*CMPHI (HOB,R,A,THETA,OMEGA,PATH,T,TPRIME+W,GAMYLD)         CMTN 340
      RK7=RK6                                                           CMTN 350
      RK8=DT*CMPHI (HOB,R,A,THETA,OMEGA,PATH,T,TPRIME+DT,GAMYLD)        CMTN 360
      JPHI=JPHI+(RK5+2.*(RK6+RK7)+RK8)/6.                               CMNT 370
C                                                                       CMNT 380
C         RUNGE-KUTTA INTEGRATION OF PRIMARIES                          CMTN 390
C                                                                       CMTN 400
      RKP1=DT*RKCMTN(R,THETA,OMEGA,TP,TPRIME)                           CMNT 410
      RKP2=DT*RKCMTN(R,THETA,OMEGA,TP,TPRIME+W)                         CMNT 420
      RKP3=RKP2                                                         CMNT 430
      RKP4=DT*RKCMTN(R,THETA,OMEGA,TP,TPRIME+DT)                        CMNT 440
      PRI=PRI+(RKP1+2.*(RKP2+RKP3)+RKP4)/6.                             CMNT 450
      RP1=DT*RKCMTN(R,THETA,OMEGA,T,TPRIME)                             CMNT 460
      RP2=DT*RKCMTN(R,THETA,OMEGA,T,TPRIME+W)                           CMNT 470
      RP3=RP2                                                           CMNT 480
      RP4=DT*RKCMTN(R,THETA,OMEGA,T,TPRIME+DT)                          CMNT 490
      PRI2=PRI2+(RP1+2.*(RP2+RP3)+RP4)/6.                               CMNT 500
      TPRIME=TPRIME+DT                                                  CMNT 510
 31   CONTINUE                                                          CMNT 520
C                                                                       CMNT 530
C         MULTIPLY PRIMARIES BY Q*G(R)/TMAX                             CMNT 540
C         TO OBTAIN RATE OF FRODUCTION OF SECONDARIES                   CMNT 545
C                                                                       CMNT 550
      PRI=PRI*5.000E4*GOFR(R,A,HOB,PATH,GAMYLD)/TMAX                    CMNT 560
      PRI2=PRI2*5.0E4*GOFR(R,A,HOB,PATH,GAMYLD)/TMAX                    CMNT 570
      RETURN                                                            CMNT 580
      END                                                               CMNT 581
C
C
      FUNCTION CMPHI(HOB,R,A,THETA,OMEGA,PATH,T,TPRIME,GAMYLD)          CMPH1010
C                                                                       CMPH1020
C         CALCULATES F(T,P) FOR RUNGE-KUTTA INTEGRATION                 CMPH1030
C         OF PHI COMPONENT OF COMPTON CURRENT                           CMPH1040
C                                                                       CMPH1050
      SOLVE=TOFT(T,TPRIME,THETA,OMEGA)                                  CMPH1060
      SOLVE=FOFT(SOLVE)                                                 CMPH1070
      SOLVE=SOLVE*(-4.608E-11)*GOFR(R,A,HOB,PATH,GAMYLD)                CMPH1080
      CMPHI=SOLVE*SIN(THETA)*SIN(OMEGA*TPRIME)                          CMPH1090
      RETURN                                                            CMPH1100
      END                                                               CMPH1101
C
C
      FUNCTION CMTHET(HOB,R,A,THETA,OMEGA,PATH,T,TPRIME,GAMYLD)         CMTH1010
C                                                                       CMTH1020
C         CALCULATES F(T,F) FOR RUNGE-KUTTA INTEGRATION                 CMTH1030
C         OF THETA COMPONENT OF COMPTON CURRENT                         CMTH1040
      SOLVE=TOFT(T,TPRIME,THETA,OMEGA)                                  CMTH1050
      SOLVE=FOFT(SOLVE)                                                 CMTH1060
      SOLVE=SOLVE*(-4.608E-11)*GOFR(R,A,HOB,PATH,GAMYLD)                CMTH1070
      CMTHET=SOLVE*SIN(THETA)*COS(THETA)*(COS(OMEGA*TPRIME)-1.)         CMTH1080
      RETURN                                                            CMTH1090
      END                                                               CMTH1091
C
C
      SUBROUTINE RNGKUT (E1,E,R,H,SIGMA,COMPTJ)                         RNKT1010
C                                                                       RNKT1020
C         E(I+1) IS CALCULATED FROM E(I)                                RNKT1030
C         USING THE RUNGE-KUTTA METHOD                                  RNKT1040
C                                                                       RKKT1050
      DATA C/3.0E8/,RMUO/12.56637E-7/                                   RNKT1060
      EFUN(R,E)=-(1./R+C*RMUO*SIGMA/2.)*E-COMPTJ*C*RMUO/2.              RNKT1070
      RK1=H*EFUN(R,E)                                                   RNKT1080
      RK2=H*EFUN(R+H/2.,E+RK1/2.)                                       RNKT1090
      RK3=H*EFUN(R+H/2.,E+RK2/2.)                                       RNKT1100
      RK4=H*EFUN(R+H,E+RK3)                                             RNKT1110
      E1=E+(RK1+2.*(RK2+RK3)+RK4)/6.                                    RNKT1120
      RETURN                                                            RNKT1130
      END                                                               RNKT1131
C
C
      FUNCTION RKCMTN(R,THETA,OMEGA,TP,TPRIME)                          RKCM1010
C                                                                       RKCM1020
C          CALCULATES F(T) FOR RUNGA-KUTTA                              RKCM1030
C          INTEGRATION OF PRIMARY ELECTRONS                             RKCM1040
C                                                                       RKCM1050
      SOLVE=TOFT(TP,TPRIME,THETA,OMEGA)                                 RKCM1060
      RKCMTN=FOFT(SOLVE)                                                RKCM1070
      RETURN                                                            RKCM1080
      END                                                               RKCM1080
C
C
      FUNCTION GOFR (R,A,HOB,PATH,GAMYLD)                               GOFR1010
C                                                                       GOFR1020
C         SOLVES VIRGIN TRANSPORT AND USES REACTION RATE TO             GOFR1030
C         CALCULATE THE NUMBER DENSITY OF RADIAL ELECTRONS              GOFR1040
C                                                                       GOFR1050
      SOLVE=(.0226275/COS(A))*(-1.+EXP(R*COS(A)/7000.))*EXP(-HOB/7000.) GOFR1060
      DENOM=12.56637*R*R*PATH*1.5                                       GOFR1070
      GOFR=EXP(-SOLVE)*GAMYLD/DENOM                                     GOFF1080
      RETURN                                                            GOFR1090
      END                                                               GOFR1091
C
C
      FUNCTION CLIFE (R,A,HOB)                                          CLIF1010
C                                                                       CLIF1020
C         CALCULATES COMPTON LIFETIME AT RADIUS = R                     CLIF1030
C         MAX ACCEPTABLE LIFETIME = 100 SHAKES FOR                      CLIF1040
C         THE KARZAS-LATTER HIGH FREQUENCY APPROX                       CLIF1050
C                                                                       CLIF1060
      CLIFE=1.041667E-8*EXP((HOB-R*COS(A))/7000.)                       CLIF1070
      IF(CLIFE.GT.1.E-6) CLIFE=(1.E-6)                                  CLIF1080
      RETURN                                                            CLIFI090
      END                                                               CLIFI091
C
C
      FUNCTION TOFT (T,TPRIME,THETA,OMEGA)                              TOFT1010
C                                                                       TOFT1020
C         T(T) IS TIME TRANSFORMED TO KARZAS-LATTER FORM                TOFT1030
C                                                                       TOFT1040
      B=0.958                                                           TOFT1050
      FIRST=T-(1.-B*(COS(THETA)**2))*TPRIME                             TOFT1060
      SECOND=B*(SIN(THETA)**2)*SIN(OMEGA*TPRIME)/OMEGA                  TOFT1070
      TOFT=FIRST+SECOND                                                 TOFT1080
      RETURN                                                            TOFT1090
      END                                                               TOFT1091
C
C
      FUNCTION FOFT(T)                                                  FOFT1010
C                                                                       FOFT1020
C         F(T) IS THE POMRANNING MODEL FOR TIME DEPENDENCE              FOFT1030
C         OF NUCLEAR WEAPON YIELD IN RETARDED TIME                      FOFT1040
      INTEGER OUX                                                       FOFT1050
      COMMON OUX,AP,BP,RNP,TOP                                          FOFT1060
      TSHAKE=1.E8*T                                                     FOFT1070
      DENOM=(BP+AP*EXP((AP+BP)*(TSHAKE-TOP)))*RNP                       FOFT1080
      FOFT=(AP+BP)*EXP(AP*(TSHAKE-TOP))/DENOM                           FOFT1090
      RETURN                                                            FOFT1100
      END                                                               FOFT1101
