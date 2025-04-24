      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER*4 (I-N)
      REAL*8 KM,KH,KQ,L
      PARAMETER (IM=594,KB=101,KS=20,IMM1=IM-1,KBM1=KB-1)
      PARAMETER (IMM2=IM-2,KBM2=KB-2)
      PARAMETER (IM2=IM*2,IMK=IM*KB,IMKM2=IM*KBM2)
      PARAMETER (IMKM1=IM*KBM1)
      COMMON/BLKCON/
     1          IINT,IPRINT,DTE,DTI,TPRNU,UMOL,
     2          GRAV,TIME,RAMP,XLAT
C---------------- 1-D ARRAYS --------------------------------------
      COMMON/BLK1D/
     1      DZR(KB),Z(KB),ZZ(KB),DZ(KB),DZZ(KB)
C---------------- 2-D ARRAYS --------------------------------------
      COMMON/BLK2D/H(IM),DX(IM),D(IM),DT(IM),
     1     ART(IM),ARU(IM),ARV(IM),CBC(IM),
     2     ALON(IM),ALAT(IM),ANG(IM),
     3     DUM(IM),DVM(IM),FSM(IM),COR4(IM),CURV42D(IM),
     4     WUSURF(IM),WVSURF(IM),WUBOT(IM),WVBOT(IM),
     5     WTSURF(IM),WSSURF(IM),TPS(IM),AAM2D(IM),
     6     UAF(IM),UA(IM),UAB(IM),VAF(IM),VA(IM),
     7     VAB(IM),ELF(IM),EL(IM),ELB(IM),PSI(IM),
     8     ETF(IM),ET(IM),ETB(IM),FLUXUA(IM),FLUXVA(IM),
     9     EGF(IM),EGB(IM)
C---------------- 3-D ARRAYS --------------------------------------
      COMMON/BLK3D/
     1     A(IM,KB),C(IM,KB),VH(IM,KB),VHP(IM,KB),
     1     UF(IM,KB),VF(IM,KB),
     2     KM(IM,KB),KH(IM,KB),KQ(IM,KB),L(IM,KB),
     3     Q2(IM,KB),Q2B(IM,KB),AAM(IM,KB),
     4     Q2L(IM,KB),Q2LB(IM,KB),
     5     U(IM,KB),UB(IM,KB),W(IM,KB),
     6     V(IM,KB),VB(IM,KB),
     7     T(IM,KB),TB(IM,KB),
     8     S(IM,KB),SB(IM,KB),
     9     RHO(IM,KB),DTEF(IM,KB),RMEAN(IM,KB)
      common/ave/
     1     umi(im,kb),vmi(im,kb),wmi(im,kb),psimi(im,kb),
     2     tmi(im,kb),smi(im,kb),kmmi(im,kb),khmi(im,kb)
C----------- 1 AND 2-D BOUNDARY VALUE ARRAYS ------------------------
      COMMON/BDRY/
     1     TBE(KB),TBN(IM,KB),TBS(IM,KB),SBN(IM,KB),SBE(KB),
     2     SBS(IM,KB),VABN(IM),VABS(IM),UGS(KB),VGS(IM,KB),
     3     ELN(IM),ELS(IM),COVRHN(IM)
