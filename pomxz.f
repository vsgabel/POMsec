c
c  program pom2dbr10.f - POM2D for the Brazilian Transect - Version.10
c
c       PS: The routines TERMS and MOORING are commented.
C
C
C-----  Time history of the model and alterations performed: -------
C
C  This is a 2-D version of pom.f created I know not when.
C  I don't remember what the set up is suppose to do nor
C  how close it is to the 3-D version of pom.f. A 2-D version of
C  pom.f can be created by elimination of all J arguements.
C  If anyone does so, He or she should substitute for this
C  version.
C                               G.Mellor 05/19/94
C
C     This version was given to Jose Lima by Patrick Marchesiello
C     in March/1996. It was used previously in the Sydney coast,
C     and he implemented features to simulate a surface current
C     jet. The file was renamed by Jose as pom2dc.f, and will be
C     kept in the directory pom2d.       J.Lima 16/Mar/96
C
C     All the alterations performed in the code by Jose Lima
C     will be preceded by the comment line "cJL"
C
C     The file pom2dc.f was copied to pom2dbCr.f. The later will
C     be adapted to a transect of the SE Brazilian coast about
C     22.42S. The following alterations were done:
C       - modify the parameters IM=145,KB=50,KS=20
C       - read the bathymetric data from file pom2bra8.grd,
C         the sigma levels from file pom2bra3.sgm and temperature
C         profile from file p366.tmp.
C       - the coriolis parameter for the transect is f=-0.556D-4 s-1.
C         Thus, the COR4 variable was modified to -0.556D-4/4.
C         The variable BETA was also adjusted to 2.116D-11.
C    ??   Check with Patrick if BETA should be negative or not.
C       - The grid has a coarse resolution (dx = 1000ms).
C         To satisfy the CFL condition DTI=180 and
C         DTI/DTE=ISPLIT=70 (see Mellor 1993 - formulae 29-30).
C    ?? - Check with Patrick the value for the Turbulent Prandtl
C         number TRPNU. He is using 4, but O'Connor recommends 1.
C         Check also the value for the horizontal eddy diffusivity
C         constant HORCON. Patrick is using 0.2, O'Connor presents
C         the range (0.01-0.1), and Mellor (1989) discusses it.
C         If the grid is very fine, HORCON should tend to smaller
C         values.                         J.Lima 19/Mar/96
C
C       - The grid resolution was improved to dx=500ms, but the
C         bottom goes to 1500 ms only. IM=237, KB=50, file with
C         bathymetry pom2bra6.grd. Wind Stress associated with
C         a wind with vel=5 m/s going North. WVSURF=0.35D-4m2/s2.
C         An improvement of routine PRXZ was done in order to
C         print maximum and minimum values of the respective field.
C         Each call of the PRXZ routine with IM=237 and KB=50 uses
C         0.0448 seconds of CPU time.
C         J.Lima 20/Mar/96
C
C       - The output for fort.80 was modified to print the original
C         variables(U,V,T,etc) instead of the averaged ones (UM,VM,
C         etc).The averaged variables are calculated now as a function
C         of the printing time IPRTD1.  J.Lima 20/Mar/96
C
C       - The variables UBOT,UINT,USURF will hold the bottom, interior
C         and surface U velocities, and the VBOT,VINT,VSURF will
C         hold the temperature at the same levels of U. A cosine
C         taper was applied in the seaward boundary of the wind
C         stress.                       J.Lima 21/Mar/96
C
C       - The results with tapered wind were not appropriate. The
C         discontinuity in the taper caused oscillations in the
C         vertical stream-function. Thus it was commented out.
C                                       J.Lima 25/Mar/96
c
C       - All the above alterations was done previoulsy in the
C         file pom2dbr1.f, that was adapted from the the archive
C         pom2dw.f. A problem in the evolution of high values of
C         KM and KH close to the bottom made us change the code
C         to the pom2dc.f (that was the last version from
C         Patrick). This code will de called pom2dbr.f. J.Lima 26/03
C
C       - An extrapolation problem was identified in the first CTD
C         profile used (p366.tmp). Thus, the program was changed to
C         use the profile p500.tmp.          J.Lima 27/03/96
C
C       - The subroutine BCOND was modified in order to open the
C         seaward boundary. It was used similar condition that were
C         applied by Simon Evans in CTW application. The grid was
C         stretched over the final 10 grid points, a spounge layer
C         was implemented, the external mode has zero gradient
C         condition and the internal mode has Orlanski radiative
C         bc. The first tests with new bc's were positive J.Lima 29/3
C
C       - The real T-S field were introduced in the model from CTD
C         cruise 45. The field was not appropriate and further
C         smoothing was required.    J.Lima 5/4
C
c       - The smoothed field was initially runned in diagnostic mode
C         starting with zero velocity and elevation. The model run
c         all right until the third inertial day, but with strange
c         oscillations in the MKE field. It was blowing after 5
C         days.                      J.Lima 10/4
c
c       - Patrick recommended to impose a gausssian surface jet.
C         The model was not blowing anymore, but the Brazil Current
C         and AIA levels were having problems. A further change
C         was recommended by Patrick to impose the level of no
C         motion at 400 ms as a more realistic approach.  J.Lima 12/4
C
c       - Setup the file to reinitialise the model with data from
c         the previous run. Run the model to 17 days without wind
c         in prognostic mode and introduce the wind with no ramp,
c         running the model for more 5 days   J.Lima 15/04/96
c
c       - Initialise vectors to simulate moorings and save the
c         currents at specific sites: bottom (MA100,MA400,MA800),
c         shelf(S65) and slope(M1250).        J.Lima 17/04/96
c
c       - Reading the file pom2d.sgm with a better resolution
c         for the sigma spacing in the BBL. The KB has to be
c         changed for KB=74. The files with initial T-S fields
c         are temp74.dat and sal74.dat.       J.Lima 23/04/96
c
c       - Mount file fort.74 in order to permit reinitialise
c         the model. The routine INIT was elaborated to
c         hold the initialisation steps. To restart the model,
c         fort.74 should be moved to fort.70 and fort.80 should
c         be moved to fort.40. The version 3 (pom2dbr3.f) does
c         not have the routine INIT           J.Lima 25/04/96
c
c       - Change boundary conditions on the coast for freD-slip
c         in the alongshore direction (dV/dx = 0) and keep the
c         bathimetric depth H(1)=30.          J.Lima 26/04/96
c
c       - After the tests, it was concluded to keep the previous
c         landward BC's with H(1)=1. Another 12 grid points were
c         included in the grid, with the beginning at isobath
c         10 ms. IM=192 , and the files temp742.dat e sal742.dat.
c         The new bathimetry file is pom2d.grd. J.Lima 26/04/96
c
c       - The program with IM=192, KB=74 was renamed to version
c         pom2dbr5.f. It was tested with flat stratification
c         from file p500m.tmp and no forcing. The results show
c         that spurious residual U and V velocities are smaller
c         than 0.005 m/s. The routine MOORING was adjusted for
c         the topographic data file pom2d.grd.  J.Lima 28/04/96
c
c       - The routine INIT was slightly modified in the way that
c         the initial velocity field was estimated from the
c         density field. Now, there's an option to add a barotropic
c         velocity if it's required. The level of no motion will
c         be 500 ms with a barotropic velocity of +0.10m/s. 30/04
c
c       - The real windstress is initialised reading the file
c         windstress.m. Version POM2DBR5.F doesn't permit cross-
c         shore variation of alongshelf windstress J.Lima 01/05/96
c
c       - The version POM2DBR6.F was created to permit a linear
c         interpolation between the alongshelf wind stress read
c         from two data files: one in the coast (grid i=1) and
c         the other located somewhere in the X-domain (for our
c         dx=1000ms grid, it was selected i=73 as the position
c         to apply the wind stress from the buoy). J.Lima 02/05/96
c
c       - The routine TERMS was created to print out the momentum
c         equation terms to file TERMS.DAT, using the same frequency
c         as used for the mooring file. The program file was renamed
c         pom2dbr7.f.                           J.Lima 20/05/96
c
c       - The variables uvdif(IM,KB) and vvdif(IM,KB) were created
c         to store the vertical diffusion contribution to the
c         velocity field                        J.Lima 21/05/96
c
c       - The version 8 is the version of the pom2dbr7.f adapted
c         from the Cray to the Digital Alpha Workstation. The
c         common block INITI was modified from the previous
c         version, and a test will be made with flat stratification
c         from p500m.tmp,no initial velocity field and constant
c         wind stress. The goto 300 in routine ADVT was commented,
c         and all the outputs to fort.80 were also commented.
c                                               J.Lima 01/03/97
c
c       - The tests were removed from the code. The name of routine
c         DENS was changed to routine SIGMAT, because in this program
c         the density is calculated for pressure=0. J.Lima 17/05/97
c
c       - The pom2dbr9.f (Version 9) was mounted from version 8 in
c         CENPES. It was commentted the routine PUTCDF2D. The wind
c         input was also commentted. The baroclinic pressure gradient
c         DRHOX was ramped inside routine BAROPG. The RAMP in TRNU
c         inside the external loop was removed. The routine SIGMAT
c         was modified to calculate the in-situ density and was
c         renamed DENS.
c                                                   J.Lima 26/05/98
c       - It was mounted five files to be used by MATLAB: X.DAT,
c         Y.DAT,TEMP.DAT,SAL.DAT,DENS.DAT and VEL.DAT.
c                                                   J.Lima 27/05/98
c
c       - The routine INIT was modified to read files with
c         interpolated temperature and salinity data from CTD
c         stations                                  J.Lima 08/06/98
c
c       - The input and output files were numbered in such
c         way to avoid problems with input files    J.Lima 01/10/98
c
c       - It was generated a relaxation scheme for temperature
c         and salinity, just after the boundary conditions are
c         applied in the main program               J. Lima 30/10/98
c
c       - The routine MOORING was modified to generate profiles
c         for specific P-1000. The old routine MOORING from
c         previous versions were modified to MOORING1.
c                                                   J. Lima 9/11/98
c
c       - The version was update to version 10 (pom2dbr10.f),
c         because major alterations were done: substitution of
c         COMMON blocks in the routines by a single include
c         'comblk2d.h', elimination of COMMON/INITI, modification
c         of routine pot_vort. All the program is on double
c         precision.                               J. Lima 27/11/98
c
c ------------------------------------------------------------------
c
      PROGRAM MAIN
C
	include 'comblk2d.h'

      dimension  UTB(IM),VTB(IM),UTF(IM),VTF(IM),
     2       ADVUA(IM),ADVVA(IM),TSURF(IM),SSURF(IM),
     3       DRHOX(IM,KB),TRNU(IM),
     4       TMEAN(IM,KB),SMEAN(IM,KB),ADVUU(IM),ADVVV(IM),
     5       X(IM),RX(KB),RHOX(IM,KB)

      DIMENSION ZLEV(KS),RHOZ(KB),
     1       ZD(KB),ZZD(KB)
cpm
      dimension  ENERGY(500),
     &           UBOT(500),UINT(500),USURF(500),
     &           VBOT(500),VINT(500),VSURF(500)
C      dimension  UM(IM,KB),VM(IM,KB),TM(IM,KB),SM(IM,KB),
C     &           KMM(IM,KB),KHM(IM,KB),RHOM(IM,KB),Q2M(IM,KB)

      REAL*8 MKE,MPE,chave
cjl	Potential vorticity matrix, the x-derivative of potential vorticity
c     and the squared Brunt-Vaisala frequency
      dimension potvort(im,kb),dpvortdx(im,kb),bv2(im,kb)
cJL   Vectors for alongshelf component of wind stress and terms routine
c      dimension  wndy1(500),wndy2(500),uvdif(IM,KB),vvdif(IM,KB)
cpm
      real*8 ISPI,ISP2I,KEYPRINT
      real*8 DIA,DIA2,DIA3,DIA4,DIA5
cvc
      CHARACTER*2 COMEN
      real*8 VENTOSN
      real*8 PRTD1, PRENERGY
cjl
c     Initializing CPU time
c     using IMSL library
c      cput0=CPSEC()
c	write(*,*) cput0
c     using PORTLIB
c      USE PORTLIB
c	cput0=time()
c
C     NAMELIST/PRMTR/MODE,TIME,DTI,NREAD,IPRTD1,ISWTCH,IPRTD2,IDAYS,
C   1       ISPLIT,ISPADV,HORCON,TPRNU,SMOTH
C
C     DESCRIPTION OF WORKING FILES OF POM2D:
C
C     INPUT FILES:
C     Unit    Name or purpose of file
C      5      POM2D.IN Input parameters (ASCII)
C     11      Grid data files (*.sgm=sigma Z(i) level; *.xhd=bathimetry) (ASCII)
C     12      Temperature and Salinity initial conditions (ASCII)
C     13      Wind Stress files (ASCII)
c     40      Input grid data (Binary)
C     70      Initial conditions input data to be read when NREAD=1 (Binary)
C
C    OUTPUT FILES:
C    Unit    Name or purpose of file
C      6      POM2D.OUT General output data (ASCII)
C      7      MOORING.DAT Store output as mooring points (ASCII)
C      8      DIAGNOSE.OUT Energy diagnose file (ASCII)
C      9      TERMU.DAT x-momentum equation terms (ASCII)
C     10      TERMV.DAT y-momentum equation terms (ASCII)
C     22      X.DAT x-coordinate grid to plot using MATLAB (ASCII)
C     23      Z.DAT z-coordinate grid to plot using MATLAB (ASCII)
C     24      TEMP.DAT Output temperature field to plot using MATLAB (ASCII)
C     25      SAL.DAT Output salinity field to plot using MATLAB (ASCII)
C     26      DENS.DAT Output density field to plot using MATLAB (ASCII)
C     27      V_VEL.DAT Alongshelf velocity field to plot using MATLAB (ASCII)
C     28      POTVORT.DAT Potential vorticity field to plot using MATLAB (ASCII)
C     75      Output final conditions to be read as unit 70 later (Binary)
C     80      Output grid data information (Binary)
c
C
c      OPEN(6,FILE='pom2d.out',STATUS='UNKNOWN')
c      OPEN(9,FILE='termu.dat',STATUS='UNKNOWN')
c      OPEN(10,FILE='termv.dat',STATUS='UNKNOWN')
C--------------------------------------------------------------------------
C     READ NAMELIST FOR PROBLEM PARAMETERS
C       DTI = INTERNAL TIME STEP
C       ISPLIT = DTI/DTE
C       DTI = INTERNAL TIME STEP
C       MODE = 2; 2-D CALCULATION (BOTTOM STRESS CALCULATED IN ADVAVE)
C              3; 3-D CALCULATION (BOTTOM STRESS CALCULATED IN PROFU,V)
C              4; 3-D CALCULATION WITH T AND S HELD FIXED
C      READ(5,PRMTR)
C
C     Program Control variables:
C     NREAD=0 No restart input file  NREAD=1 restart files fort.40 fort.70
C--------------------------------------------------------------------------
c      MODE=3, inibido MODE=3 (prognostico) para MODE =4 (diagnostico) EM 19/10/98

      OPEN(11,FILE='pomxz.in',STATUS='UNKNOWN')
      READ(11,'(A2)') COMEN
      READ(11,'(A2)') COMEN
      READ(11,'(A2)') COMEN
      READ(11,'(A2)') COMEN
      READ(11,*) FDAYS
      READ(11,'(A2)') COMEN
      READ(11,*) DIA, DIA2, DIA3, DIA4, DIA5
      READ(11,'(A2)') COMEN
      READ(11,*) MODE1, MODE2, MODE3, MODE4, MODE5, MODE6
      READ(11,'(A2)') COMEN
      READ(11,*) DTI
      READ(11,'(A2)') COMEN
      READ(11,*) ISPLIT
      READ(11,'(A2)') COMEN
      READ(11,*) HORCON
      READ(11,'(A2)') COMEN
      READ(11,*) AAA
      READ(11,'(A2)') COMEN
      READ(11,*) XLAT
      READ(11,'(A2)') COMEN
      READ(11,*) KEYPRINT
      READ(11,'(A2)') COMEN
      READ(11,*) PRTD1, PRENERGY
      READ(11,'(A2)') COMEN
      READ(11,*) VENTOSN
      close(11)



      TIME=0.
C      PRTD1=1
      ISWTCH=60
      IPRTD2=1
      ISPADV=1
      TPRNU=4.D0
      SMOTH=.1D0
      IINT=0
      ITIME=0
      NREAD=0

cvc       AAA=2.D0
cvc      DTI=170.D0
cvc      ISPLIT=50
cvc      HORCON=.2D0

      MODE=mode1


cJL   Setting parameters for the Brazilian transect
      ISKP=ifix(float(IM)/24.)+1
C     Setup program variables

cvc***********Alterei o DFLOAT***********
cvc   DTE=DTI/DFLOAT(ISPLIT)

      DTE=DTI/FLOAT(ISPLIT)
      DTE2=DTE*2
      DTI2=DTI*2
cJL   Alteration performed by J.Lima to permit print values at
c     inertial-day intervals (for the Brazilian transect =1.318 day)
c     Comment next line, and uncomment the other if you want to
c     print in 24hour-day intervals.
CMC      IPRINT=IDINT(1.318*24.*3600.)/IDINT(DTI)

cvc***********Alterei o DFLOAT e o IDINT***********
cvc      IPRINT=IDINT(DFLOAT(PRTD1)*24.*3600./DTI)

       boo=(PRTD1)*24.*3600./DTI
       IPRINT=int(boo)
       boo=(PRENERGY)*24.*3600./DTI
       IPRENERGY=int(boo)

      ISWTCH=ISWTCH*24*3600/int(DTI)
c      IEND=IDAYS*24*3600/ifix(DTI)
      IEND=int(FDAYS*24*3600)/int(DTI)
c
c     Write the initial program control variables
      write(6,'(//,32h POM2D for the Brazilian Coast  ,//)')
c      WRITE(6,7030) MODE,DTE,DTI,ISPLIT,IEND,ISPADV,IPRINT,SMOTH,HORCON,
c     1  AAA,TPRNU,NREAD
c23456789012345678901234567890123456789012345678901234567890123456789012
 7030 FORMAT(/,' PROGRAM PARAMETERS :',//,' MODE =',I3,/,' DTE =',F7.2,/
     1,' DTI =',F7.2,/,' ISPLIT =',I6,/,' IEND =',I6,/,' ISPADV =',I6,/,
     2' IPRINT =',I6,/,' SMOTH =',F7.2,/,' HORCON =',F7.3,/,' AAA =',F8.
     32,/,' TPRNU =',F6.2,/,' NREAD =',I5)
c
c     Setting null the working vectors
      DATA UAF/IM*0.D0/,UA/IM*0.D0/,UAB/IM*0.D0/
      DATA VAF/IM*0.D0/,VA/IM*0.D0/,VAB/IM*0.D0/
      DATA UTB/IM*0.D0/,VTB/IM*0.D0/UTF/IM*0.D0/,VTF/IM*0.D0/
      DATA ELF/IM*0.D0/,EL/IM*0.D0/,ELB/IM*0.D0/,ETB/IM*0.D0/
      DATA ET/IM*0.D0/,ETF/IM*0.D0/,ADVUA/IM*0.D0/,AAM2D/IM*50.D0/
      DATA ADVVA/IM*0.D0/,TRNU/IM*0.D0/
      DATA ADVUU/IM*0.D0/,ADVVV/IM*0.D0/,FLUXUA/IM*0./
      DATA FLUXVA/IM*0.D0/,WUSURF/IM*0.D0/
      DATA WVSURF/IM*0.D0/,WTSURF/IM*0.D0/,WSSURF/IM*0.D0/
      DATA WUBOT/IM*0.D0/,WVBOT/IM*0.D0/
      DATA A/IMK*0.D0/,C/IMK*0.D0/,VH/IMK*0.D0/,VHP/IMK*0.D0/
      DATA UF/IMK*0.D0/,U/IMK*0.D0/,UB/IMK*0.D0/,W/IMK*0.D0/
      DATA VF/IMK*0.D0/,V/IMK*0.D0/,VB/IMK*0.D0/
      DATA T/IMK*0.D0/,TB/IMK*0.D0/,S/IMK*0.D0/,SB/IMK*0.D0/
      DATA RHO/IMK*0.D0/,Q2B/IMK*0.D0/,Q2LB/IMK*0.D0/
      DATA TMEAN/IMK*0.D0/,SMEAN/IMK*0.D0/,RMEAN/IMK*0.D0/
      DATA DRHOX/IMK*0.D0/
      DATA PI/3.1416D0/,RAMP/1.D0/,SMALL/1.D-10/,TIME0/0.D0/
      DATA BETA/2.111D-11/,GRAV/9.806D0/,UMOL/1.D-6/
      DATA DDX/2.0D30/
C
C     AVERAGED DIAGNOSTICS ARRAYS
c      DATA UM/IMK*0.D0/,VM/IMK*0.D0/,TM/IMK*0.D0/,SM/IMK*0.D0/
c      DATA KMM/IMK*0.D0/,KHM/IMK*0.D0/,RHOM/IMK*0.D0/,Q2M/IMK*0.D0/
      DATA ENERGY/500*0.D0/,UBOT/500*0.D0/,UINT/500*0.D0/
      DATA USURF/500*0.D0/,VBOT/500*0.D0/,VINT/500*0.D0/,VSURF/500*0.D0/

C
C----------------------------------------------------------------------
C             ESTABLISH PROBLEM CHARACTERISTICS
C          ****** ALL UNITS IN M.K.S. SYSTEM ******
C      F,BLANK AND B REFERS TO FORWARD,CENTRAL AND BACKWARD TIME LEVELS.
C----------------------------------------------------------------------
      DAYI=1.D0/86400.D0
      ISPI=1.D0/dfloat(ISPLIT)
      ISP2I=1.D0/(2.D0*dfloat(ISPLIT))
C
C --------------- BEGIN OF SETUP ---------------------------------------
C     Option to start setting up all the initial conditions
C     (subroutine INIT) or restart the program with previous
C     run data
      IF(NREAD.EQ.0) THEN
c
c        Routine to build all initial conditions
         CALL INIT(AAA,ISKP,BOTTOM,UTB,VTB,UTF,VTF,
     2       ADVUA,ADVVA,TSURF,SSURF,DRHOX,TRNU,TMEAN,
     3       SMEAN,ADVUU,ADVVV,X,RX,RHOX)
      ELSE
C
C        READ IN GRID DATA AND INITIAL CONDITIONS
         REWIND(40)
C        REWIND(50)
C        REWIND(60)
         REWIND(70)
C        READING INITIAL GRID DATA (SAVED IN THE FILE FORT.80)
         READ(40)IMM,KBB
         READ(40) Z,ZZ,DZ,DZZ,DX,H,COR4,
     1      ART,ARU,ARV,TMEAN,SMEAN,RMEAN,
     2      ALAT,ALON,
     4      FSM,DUM,DVM
C        TEST THE GRID DIMENSIONS
         IF( (IMM.NE.IM) .OR. (KBB.NE.KB) ) THEN
            WRITE(6,'(70H ERROR: DIMENSIONS READ FROM FILE FORT.40 DO NO
     1T AGREE WITH IM AND KB )')
            STOP
         ENDIF
C        CALL VABFIX
C        READ(50) TIME0,UB,VB,UAB,VAB,TB,SB,ELB,Q2B,Q2LB,KM,KH
C        READ(60) WUSURF,WVSURF,WTSURF
C        READING LAST STATE OF THE PREVIOUS RUN (SAVED IN FILE FORT.74)
         DO 10 N=1,NREAD
            READ(70) TIME0,
     1           WUBOT,WVBOT,AAM2D,UA,UAB,VA,VAB,EL,ELB,ET,ETB,EGB,
     2           UTB,VTB,U,UB,W,V,VB,T,TB,S,SB,RHO,ADVUU,ADVVV,ADVUA,
     3           ADVVA,KM,KH,KQ,L,Q2,Q2B,AAM,Q2L,Q2LB
 10      CONTINUE
         BOTTOM=H(IM)
         X(1)=0.
         do i=2,IM
            X(i)=X(i-1)+DX(i)
         enddo
         DO 20 K=1,KBM1
 20         DZR(K)=1./DZ(K)
         DO 82 I=1,IM
            D(I)=H(I)+EL(I)
            DT(I)=H(I)+ET(I)
            TSURF(I)=TB(I,1)
  82     SSURF(I)=SB(I,1)
         CLOSE(40)
         CLOSE(70)
      ENDIF
c
cpm   Read time dependent surface wind stress.
cJL   Adapted to read wind stress (in Pa) from
c     the Brazilian coast. The vectors were setup in such a
c     way that wndy1 is the wind stress from the coast and
c     wndy2 is the windstress offshore. To use a constant
c     cross-shore wind stress, just uncomment wndy1=wndy2 and
c     comment the second do loop.          J.Lima 20/05/96
c      open(13,file='windmac100.dat',status='old')
c      IMW=161
c      DTWND=6./24.  !data time step in days
c      do I=1,IMW
c         read(13,*) rtime,wndstrx,wndy1(I)  !Pascals
c         wndy2(I)=wndy1(I)
c      enddo
c      close(13)
c      open(13,file='windbm100.dat',status='old')
c      do I=1,IMW
c         read(13,*) rtime,wndstrx,wndy2(I)  !Pascals
c      enddo
c      close(13)
C
c --------------------  END OF SETUP  -------------------------------
C
C     Calculate inertial period
      PERIOD=ABS(DAYI*(2.D0*PI)/(COR4((IM+1)/2)*4.D0))
      write(6,'(32H LOCAL INERTIAL PERIOD (DAYS) : ,f6.2)') PERIOD
      DO 21 I=1,IM
  21  CURV42D(I)=COR4(I)
C
C     Specification of Roughness Parameter Z0B and
c     Bottom Friction Coefficient CBC
      DO 45 I=1,IM
c        Z0B=.01D0*(1.D0+100.D0/2000.)
         Z0B=.00001D0
         CBCMIN=.0025D0
cvc
c  45  CBC(I)=MAX(CBCMIN,.16/log((ZZ(KBM1)-Z(KB))*H(I)
c     1        /Z0B)**2)
cvc

cvc Atrito de fundo CBC constante e minimo (entre lama e argila)
  45    CBC(I)=.0018D0
cvc
      DO 46 I=1,IM
   46   CBC(I)=CBC(I)*FSM(I)
      DO I=1,IM
        IF (H(I).GE.BOTTOM) CBC(I)=0.
      ENDDO
c
c     CFL condition for the external time step (DTE < TPS(i))
       tpsmax=1.D10
       DO 47 I=1,IM
          TPS(I)=0.5D0/SQRT(1.D0/DX(I)**2)
     1         /SQRT(GRAV*H(I))*FSM(I)
	    if (tps(i).lt.tpsmax .and. tps(i).ne.0.D0) tpsmax=tps(i)
  47   continue
       write(6,'(/,33H Maximum DTE for CFL condition : ,f8.2)')
     1       tpsmax*0.9D0
       write(6,'(33H Maximum DTI for used ISPLIT   : ,f8.2,//)')
     1       (tpsmax*0.9D0)*ISPLIT

       write(*,*)'Sequencia de entrada dos modos:',
     1 mode1, mode2, mode3, mode4, mode5, mode6
       write(*,*)'Entrada do mode2  no dia : ', DIA
       write(*,*)'Entrada do mode3  no dia : ', DIA2
       write(*,*)'Entrada do mode4  no dia : ', DIA3
       write(*,*)'Entrada do mode5  no dia : ', DIA4
       write(*,*)'Entrada do mode6  no dia : ', DIA5
c      CALL PRXZ(' CFL DEL TIME   ',TIME,TPS,IM,1   ,1 ,0.,DT,ZZ)
c      CALL PRXZ(' CBC  drag coef ',TIME,CBC,IM,ISKP,1 ,0.,DT,ZZ)
C
C      Gravity wave speed (commented in routine BCOND)
C      DO 48 I=1,IM
C   48 COVRHN(I)=.1D0*SQRT(GRAV/H(I))
C
C     Number of internal steps to calculate kinetic energy
c     and other diagnostic variables
c      IPRENERGY=6*3600/DTI  ! every 6 hours
      IENERGY=1
c     Estimating the initial Mean Kinetic Energy (MKE)
      VTOT=0.D0
      MKE=0.D0
      DO K=1,KBM1
         DO I=1,IM
            DVTOT=DX(I)*DT(I)*DZ(K)
            VTOT=VTOT+DVTOT
            MKE=MKE+(UB(I,K)**2+VB(I,K)**2)*DVTOT
         ENDDO
      ENDDO
      MKE=MKE/VTOT
      ENERGY(IENERGY) = MKE
cvc
      if(keyprint.eq.1) then
      write(6,'(/,17H Energy time 0 = ,g11.3)') MKE
      endif
cvc
c
cJL  The variable IDIAG stores the I-position that
c    is used to save U-velocity and Temperature data
c    to be plotted later. It was chosen IDIAG=89,
c    because it corresponds to a water depth about
c    100 ms in the 500-m grid of the Brazilian coast.
c    For the grid with 1000-m spacing, IDIAG=57 (pom2d.grd)
      IDIAG=57
      UBOT(IENERGY)=UB(IDIAG,KB-2)
      UINT(IENERGY)=UB(IDIAG,KB/2)
      USURF(IENERGY)=UB(IDIAG,2)
      VBOT(IENERGY)=TB(IDIAG,KB-2)+10.
      VINT(IENERGY)=TB(IDIAG,KB/2)+10.
      VSURF(IENERGY)=TB(IDIAG,2)+10.
c
C     Parameters to control averaged variables
C
cJL  PS: This part of the original code was improved in
c        order to avoid problems if someone decides to
c        print the averaged variables with frequency
c        IPRTD1. The variable PERIODM is the period to
c        average, and should be chosen as an integer
c        fraction of IPRTD1. If you prefer to use an
c        average period equal to the inertial period,
c        you should also choose a printing interval
c        multiple of the inertial period. J.Lima 22/Mar/96
c
c      PERIODM=dfloat(IPRTD1)
CMC      PERIODM=1.318
CCCCCCCCCC PERIODM=2. CCCCCCCCCCCC
      PERIODM=1.
      COPER=DTI*DAYI/PERIODM
c      IMEAN=ifix(PERIODM)*24*3600/ifix(DTI)
      IMEAN=IPRINT
      IXMEAN=0
C
C    Save Grid data and Initial state to file FORT.80
c      write(6,'(33H Writing to fort.80 TIME ITIME : ,f6.2,i6,/)')
c     1       TIME,ITIME
c      WRITE(80)IM,KB
c      WRITE(80) Z,ZZ,DZ,DZZ,DX,H,COR4,
c     1      ART,ARU,ARV,TMEAN,SMEAN,RMEAN,
c     2      ALAT,ALON,
c     4      FSM,DUM,DVM
c      WRITE(80) TIME
c      WRITE(80) WUSURF,WVSURF,WTSURF,
c     1  UA,VA,EL,PSI
c      WRITE(80)  U,V,T,S,KM,KH,RHO,Q2
c      WRITE(80)  ENERGY,UBOT,UINT,USURF,VBOT,VINT,VSURF
c
cJL   File 82 mounted only to save a new grid configuration
c     and be moved to file 40 later. Keep commented during
c     normal runs
c      WRITE(82)IM,KB
c      WRITE(82) Z,ZZ,DZ,DZZ,DX,H,COR4,
c     1      ART,ARU,ARV,TMEAN,SMEAN,RMEAN,
c     2      ALAT,ALON,
c     4      FSM,DUM,DVM
c
cjl
c     Routine to mount file 'pom.cdf' to plot using
c     Matlab routines
c      call putcdf2d(time)
c
c     Arquivos ASCII montados para serem lidos em MATLAB
c
	do k=1,KBM1
	   write(22,'(800f15.0)') (X(i),i=2,IM)
	   write(23,'(800f15.2)') (ZZ(k)*H(i),i=2,IM)
      enddo
c
      CALL VERTVL(DTI2)
      CALL BAROPG(DRHOX,TRNU)
c      CALL PRXZ('  DRHOX     ',0.,DRHOX,IM,ISKP,KB,0.,DT,ZZ)
c      CALL PRXZ('  TRNU      ',0.,TRNU ,IM,ISKP,1 ,0.,DT,ZZ)
      CALL ADVAVE(ADVUA,ADVVA,MODE)
c
cjl   Calculate the potential vorticity field
c      call pot_vort(potvort,dpvortdx,bv2)
cJL   The routine MOORING was designed to save current and
c     temperature grid points and store them in file MOORING.DAT
c      CALL MOORING1(X,potvort,dpvortdx,bv2)
c
cjl   Getting the CPU time associated with the setup
c     using IMSL library
c      cput1=CPSEC()
c     using PORTLIB
c	cput1=time()
c 	WRITE (6,*) '/CPU time during setup (seconds) = ',cput1-cput0
c
c     Print initial fields into ASCII output file
cvc      CALL PRXZ('  ELB    ',0.,ELB,IM,ISKP,1,0.,DT,ZZ)
cvc      CALL PRXZ('  VAB    ',0.,VAB,IM,ISKP,1,0.,DT,ZZ)
cvc      CALL PRXZ('  VB     ',0.,VB,IM,ISKP,KB,0.,DT,ZZ)
C
C***********************************************************************
C                                                                      *
C                BEGIN NUMERICAL INTEGRATION                           *
C                                                                      *
C***********************************************************************
C
C                  INTERNAL TIME STEP LOOP
c
                   DO 9000 IINT=1,IEND
C
cvc   A proxima linha deve ser usada para inicializar o modelo em um
c     MODE, e depois passar para outro MODE depois de certo tempo
c      IF(TIME.GT.PERIOD) MODE=4
!       print*,IINT,TIME,TIME-DIA
!       print*,(DIA/(DTI/86400)),int(DIA/(DTI/86400))+1

      open(5,file='chave',status='unknown')
      read(5,*) chave
      if (chave.gt.0) then
      goto 9020
      endif
      close(5)

      if (IINT.eq.1.or.MOD(IINT,100).eq.0) then
      write(*,'(15H TIME,DX,MODE : ,f12.3,i12,g12.3)')TIME,IINT,MODE
      ENDIF
cvc
!         if(keyprint.eq.1) then
!         if( mod(IINT,4).EQ.0) then
! C        write(*,'(17H TIME,IINT,RAMP : ,f12.3,i12,g12.3)')TIME,IINT,RAMP
! 	endif
! C        write(*,'(8H MODE = ,i5)') mode
!         endif
cvc
      TIME=DAYI*DTI*dfloat(IINT)+TIME0
      RAMP=ABS((TIME-TIME0)/PERIOD)
C      RAMP=tanh(ABS((TIME-TIME0)/PERIOD))
      IF(RAMP.GT.1.) RAMP=1.
cvc
      if(keyprint.eq.1) then
      IF( MOD(IINT,int(4.*3600./DTI)).EQ.0 .and. RAMP.ne.1.0) THEN
         write(6,'(12H IINT,RAMP : ,i12,g12.3)') IINT,RAMP
      ENDIF
      ENDIF
cvc
cvc Troca do mode3 para o mode 4
c      if (TIME.gt.0.4) then
c         mode=4
c      endif
      ! if (TIME.gt.DIA) then
      !    mode=mode2
      ! endif

      if (TIME.gt.DIA) then
         mode=mode2
      endif

      if (TIME.gt.DIA2) then
         mode=mode3
      endif

      if (TIME.gt.DIA3) then
         mode=mode4
      endif

      if (TIME.gt.DIA4) then
         mode=mode5
      endif

      if (TIME.gt.DIA5) then
         mode=mode6
      endif

cvc

c      RAMP=1.D0
c     IF(TIME.GT.60.) MODE=3
C*SB INTRODUCE SIMPLE WIND STRESS, VALUE IS NEGATIVE FOR WESTERLY WIND
c  PS: There are two options of setting up the wind stress. Loop 18
c      will provide a constant wind stress for the whole model
c      run. The loop in IMW will provide the option for time
c      dependent wind stress read from file windstrees.in
c
      IF (VENTOSN.EQ.1) THEN
ccccccccccccccccccccccccccccccccccccc
c     SEM VENTO NO MODO DIAGNOSTICO c
ccccccccccccccccccccccccccccccccccccc
        IF (MODE.EQ.4) THEN
          DO 18 I=1,IM
             WUSURF(I)=0.
             WVSURF(I)=0.
  18      CONTINUE
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     INTRODUZ VENTO DEPENDENTE DO TEMPO NO MODO PROGNOSTICO c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        ELSEIF (MODE.EQ.3) THEN
          CALL UPDATE_VENTO(DIA,IEND)
        END IF

      ELSEIF (VENTOSN.EQ.0) THEN
        DO I=1,IM
          WUSURF(I)=0.
          WVSURF(I)=0.
        END DO
      END IF

cJL   Tapering the wind close to the seaward boundary
c     It will be used a cosine  taper applied over the
c     last 30 Km of the grid.  J.Lima 22/Mar/96
c      ITAPER=ifix(30000./DX(IM-1))
c      IPASS=IM-ITAPER+1
c      write(6,'(16H ITAPER,IPASS = ,2i5)')ITAPER,IPASS
c      DO I=IPASS,IM
c         WEIGHT=0.5*( 1+COS(PI*(I-IPASS)/(IM-IPASS)) )
c         write(6,'(13H I, Weight = ,i5,g12.3)')I, WEIGHT
c         WUSURF(I)=WUSURF(I)*WEIGHT
c         WVSURF(I)=WVSURF(I)*WEIGHT
c      ENDDO
c      CALL PRXZ(' WVSURF   ',TIME,WVSURF,IM,ISKP ,1 ,0.,DT,ZZ)
c
cpm ---------------------------------------
cpm  Set time dependent surface wind stress
cpm ---------------------------------------
c      do IW=1,IMW
c         TWND2=DTWND*IW + TIME0
c         TWND1=DTWND*(IW-1) + TIME0
c         if (TIME.LE.TWND2) then
c            do I=1,IM
cJL            Interpolation between land wndy1 and offshore
c              wndy2  wind stress. The values 1000 and 72000
c              are associated with X-grid spacing and distance
c              offshore that the wndy2 is located. They should
c              be changed for other configurations. J.Lima 20/05/96
c               if (I.le.73) then
c                  wndIW1=wndy1(IW+1)+(I-1)*1000*(wndy2(IW+1)-
c     &                   wndy1(IW+1))/72000
c                  wndIW=wndy1(IW)+(I-1)*1000*(wndy2(IW)-
c     &                   wndy1(IW))/72000
c               else
c                  wndIW1=wndy2(IW+1)
c                  wndIW=wndy2(IW)
c               endif
c               WVSURF(I)=(wndIW1*(TIME-TWND1)+
c     &                   wndIW*(TWND2-TIME))/DTWND  !Pa
c               WVSURF(I)=-1.D-3*WVSURF(I)  !m2/s2 and opp. sign for pom
c            enddo
c            goto 66
c         endif
c      enddo
c 66   continue
c
cJL   Apply the ramping factor RAMP to the wind stress
      do i=1,IM
         WUSURF(I)=RAMP*WUSURF(I)
         WVSURF(I)=RAMP*WVSURF(I)
      enddo
C------------------------------------------------------------------------
C     SET TIME DEPENDENT, SURFACE AND LATERAL BOUNDARY CONDITIONS.
C     THE LATTER WILL BE USED IN SUBROUTINE BCOND .
C------------------------------------------------------------------------
      DO 85 I=1,IM
   85 WTSURF(I)=0.D0
C
C------------------------------------------------------------------------
C
      IF(MODE.NE.2) THEN
        CALL BAROPG(DRHOX,TRNU)
        DO 87 I=1,IM
   87   TRNU(I)=TRNU(I)+ADVUU(I)-ADVUA(I)
      ENDIF
C**********************************************************************
C     HOR VISC = CONST*DX(I)*DY*SQRT((DU/DX(I))**2+(DV/DY)**2
C                                 +.5*(DU/DY+DV/DX(I))**2)
C**********************************************************************
C######################################
CSmagorinsk
c      DO 95 K=1,KBM1
c      DO 95 I=2,IMM1
c      AAM(I,K)=HORCON*DX(I)**2
c     1          *SQRT( ((U(I+1,K)-U(I,K))/DX(I))**2
c     5       +.5D0*(.5D0*(V(I+1,K)-V(I-1,K))
c     6              /DX(I)) **2)
c 95   CONTINUE
C######################################
      do k=1,kb
        AAM(1,k)=AAM(2,k)
      enddo
      DO 96 I=1,IM
   96 AAM2D(I)=0.D0
      DO 199 K=1,KBM1
      DO 199 I=1,IM
      AAM2D(I)=AAM2D(I)+AAM(I,K)*DZ(K)
 199  CONTINUE
C
      DO 399 I=1,IM
  399 EGF(I)=EL(I)*ISPI
      DO 999 I=2,IM
      UTF(I)=UA(I)*(D(I)+D(I-1))*ISP2I
 999  VTF(I)=2.D0*VA(I)*D(I)*ISP2I
C********** BEGIN EXTERNAL MODE ***************************************
                      DO 8000 IEXT=1,ISPLIT
      DO 405 I=2,IM
 405  FLUXUA(I)=.5*(D(I)+D(I-1))*UA(I)
C
      DO 410 I=1,IMM1
  410 ELF(I)=ELB(I)
     1    -DTE2*(FLUXUA(I+1)-FLUXUA(I))
     2                    /ART(I)
C
      CALL BCOND(1)
C
      IF(MOD(IEXT,ISPADV).EQ.0) CALL ADVAVE(ADVUA,ADVVA,MODE)
C
      ALPHA=0.225D0
C
CJL  ????????????????????????????????????????????????????
C      Check the expression of UAF (the WUSURF(i)+WUSURF(i-1)).
c
      DO 420 I=3,IM
  420 UAF(I)=ADVUA(I)
     1    -ARU(I)*(2.D0*CURV42D(I)*D(I)*VA(I)
     2              +CURV42D(I-1)*D(I-1)*2.D0*VA(I-1) )
     3         +.5D0*GRAV*(D(I)+D(I-1))
     4             *( (1.D0-2.D0*ALPHA)*(EL(I)-EL(I-1))
     4            +ALPHA*(ELB(I)-ELB(I-1)+ELF(I)-ELF(I-1)) )
c     5           +RAMP*TRNU(I)
c     Ramp of baroclinic structure is made inside the routine
c     BAROPG (ramp*dhrox)
     5            +TRNU(I)
     6      - ARU(I)*(-.5D0*(WUSURF(I)+WUSURF(I-1))+WUBOT(I))
      DO 425 I=3,IM
  425 UAF(I)=
     1         ((H(I)+ELB(I)+H(I-1)+ELB(I-1))*ARU(I)*UAB(I)
     2                -4.D0*DTE*UAF(I))
     3        /((H(I)+ELF(I)+H(I-1)+ELF(I-1))*ARU(I))
      PGRAD=-.5D-7  !pm
      DO 430 I=1,IMM1
 430  VAF(I)=ADVVA(I)
     1    +ARV(I)*(  CURV42D(I)*D(I)*(UA(I+1)+UA(I))
     2                +CURV42D(I)*D(I)*(UA(I+1)+UA(I)) )
     6    + ARV(I)*(WVSURF(I)-WVBOT(I)   )
c    &    +GRAV*ARV(I)*D(I)*PGRAD  !pm add dEL/dy
      DO 435 I=1,IM
  435 VAF(I)=
     1 (2.D0*(H(I)+ELB(I))*VAB(I)*ARV(I)
     2              -4.D0*DTE*VAF(I))
     3       /(2.D0*(H(I)+ELF(I))*ARV(I))
      CALL BCOND(2)
C
      IF(IEXT.LT.(ISPLIT-2)) GO TO 440
CSB**
      IF(IEXT.EQ.(ISPLIT-2))THEN
        DO 431 I=1,IM
  431   ETF(I)=.25*SMOTH*ELF(I)
       ENDIF
      IF(IEXT.EQ.(ISPLIT-1)) THEN
        DO 432 I=1,IM
  432   ETF(I)=ETF(I)+.5*(1.-.5*SMOTH)*ELF(I)
       ENDIF
      IF(IEXT.EQ.(ISPLIT-0)) THEN
        DO 433 I=1,IM
  433   ETF(I)=(ETF(I)+.5*ELF(I))*FSM(I)
       ENDIF
 440  CONTINUE
C  TEST FOR CFL VIOLATION. IF SO, PRINT AND STOP
CSB**
      VAMAX=-1.D10
      VAMIN=1.D10
      DO 442 I=1,IM
      IF(VAF(I).GE.VAMAX)VAMAX=VAF(I)
      IF(VAF(I).LE.VAMIN)VAMIN=VAF(I)
  442 CONTINUE
      IF(ABS(VAMAX).GT.100.D0.OR.ABS(VAMIN).GT.100.D0) GO TO 9001
C
C     APPLY ASSELIN FILTER TO REMOVE TIME SPLIT AND RESET TIME SEQUENCE
      DO 445 I=1,IM
      UA(I)=UA(I)+.5D0*SMOTH*(UAB(I)-2.D0*UA(I)+UAF(I))
      VA(I)=VA(I)+.5D0*SMOTH*(VAB(I)-2.D0*VA(I)+VAF(I))
      EL(I)=EL(I)+.5D0*SMOTH*(ELB(I)-2.D0*EL(I)+ELF(I))
      ELB(I)=EL(I)
      EL(I)=ELF(I)
      D(I)=H(I)+EL(I)
      UAB(I)=UA(I)
      UA(I)=UAF(I)
      VAB(I)=VA(I)
      VA(I)=VAF(I)
  445 CONTINUE
C
      IF(IEXT.EQ.ISPLIT) GO TO 8000
      DO 450 I=1,IM
      EGF(I)=EGF(I)+EL(I)*ISPI
      UTF(I)=UTF(I)+UA(I)*(D(I)+D(I-1))*ISP2I
 450  VTF(I)=VTF(I)+VA(I)*(D(I)+D(I))*ISP2I
C
C
 8000                    CONTINUE
C---------------------------------------------------------------------
C          END EXTERNAL (2-D) MODE CALCULATION
C     AND CONTINUE WITH INTERNAL (3-D) MODE CALCULATION
C---------------------------------------------------------------------
      IF(IINT.EQ.1) GO TO 8200
      IF(MODE.EQ.2) GO TO 8200
      DO 777 I=1,IM
 777   TPS(I)=0.D0
      DO 299 K=1,KBM1
      DO 299 I=1,IM
 299  TPS(I)=TPS(I)+U(I,K)*DZ(K)
      DO 302 K=1,KBM1
      DO 302 I=2,IM
 302  U(I,K)=(U(I,K)-TPS(I))+
     1     (UTB(I)+UTF(I))/(DT(I)+DT(I-1))
      DO 3021 I=1,IM
 3021 TPS(I)=0.D0
      DO 304 K=1,KBM1
      DO 304 I=1,IM
 304  TPS(I)=TPS(I)+V(I,K)*DZ(K)
      DO 306 K=1,KBM1
      DO 306 I=1,IM
 306  V(I,K)=(V(I,K)-TPS(I))+
     2     (VTB(I)+VTF(I))/(2.D0*DT(I))
C
C----------------------------------------------------------------
C     VERTVL INPUT = U,V,DT(=H+ET),ETF,ETB; OUTPUT = W
C----------------------------------------------------------------
      CALL VERTVL(DTI2)
      CALL BCOND(5)
C
CSB**
      DO 307 K=1,KB
      DO 307 I=1,IM
      UF(I,K)=0.D0
  307 VF(I,K)=0.D0
c----------------------------------------------------------------
C     COMPUTE Q2F AN Q2LF USING UF AND VF AS TEMPORARY VARIABLES
C----------------------------------------------------------------
      CALL ADVQ(Q2B,Q2,DTI2,UF)
      CALL ADVQ(Q2LB,Q2L,DTI2,VF)
      CALL PROFQ(DTI2)
      CALL BCOND(6)
      DO 367 I=1,IM
      DO 367 K=1,KB
      Q2(I,K)=Q2(I,K)+.5*SMOTH*(UF(I,K)+Q2B(I,K)-2.D0*Q2(I,K))
      Q2L(I,K)=Q2L(I,K)+.5*SMOTH*(VF(I,K)+Q2LB(I,K)-2.D0*Q2L(I,K))
      Q2B(I,K)=Q2(I,K)
      Q2(I,K)=UF(I,K)
      Q2LB(I,K)=Q2L(I,K)
      Q2L(I,K)=VF(I,K)
 367  CONTINUE
C----------------------------------------------------------------
C     COMPUTE TF AN SF USING UF AND VF AS TEMPORARY VARIABLES
C----------------------------------------------------------------
      IF(MODE.EQ.4) GO TO 360
      CALL ADVT(TB,T,TMEAN,DTI2,UF)
      CALL ADVT(SB,S,SMEAN,DTI2,VF)
      CALL PROFT(UF,WTSURF,TSURF,1,DTI2)
      CALL PROFT(VF,WSSURF,SSURF,1,DTI2)
      CALL BCOND(4)
C
      DO 355 K=1,KB
      DO 355 I=1,IM
      T(I,K)=T(I,K)+.5*SMOTH*(UF(I,K)
     1  +TB(I,K)-2.D0*T(I,K))
      S(I,K)=S(I,K)+.5*SMOTH*(VF(I,K)
     1  +SB(I,K)-2.D0*S(I,K))
      TB(I,K)=T(I,K)
      T(I,K)=UF(I,K)
      SB(I,K)=S(I,K)
      S(I,K)=VF(I,K)
c  355 RHO(I,K)=T(I,K)
  355 continue
C
      CALL DENS

cpm  Provide hydrostatic stability
cpm  in case the turbulent closure model is off
c      do i=1,IM
c      do k=1,KB
c        KH(i,k) = 0.001
c      enddo
c      enddo
c     do i=1,IM
c     do k=1,KBM1
c       if (RHO(i,k+1).lt.RHO(i,k)) then
c          KH(i,k) = 1.
c          KH(i,k+1) = 1.
c       endif
c     enddo
c     enddo
C
  360 CONTINUE
      DO 678 I=1,IM
 678  EGB(I)=EGF(I)
C----------------------------------------------------------------
C     COMPUTE UF AND VF
C----------------------------------------------------------------
      CALL ADVU(DRHOX,ADVUU,DTI2)
      CALL ADVV(ADVVV,DTI2)
cJL   Store UF and VF without contribution of vertical diffusion
c      IF(MOD(IINT,IPRENERGY).EQ.0) THEN
c         do i=1,IM
c            do k=1,KBM1
c               uvdif(i,k)=uf(i,k)
c               vvdif(i,k)=vf(i,k)
c            enddo
c         enddo
c      ENDIF
      CALL PROFU(DTI2)
      CALL PROFV(DTI2)
cJL   Save the contribution of vertical diffusion
c      IF(MOD(IINT,IPRENERGY).EQ.0) THEN
c       do i=2,IM
c        do k=1,KBM1
c        The differentiation for U and V is different, because in the
c        Arakawa C grid, U is not in the same position of V and ETF.
c         uvdif(i,k)=(0.5*(H(I)+ETF(I)+H(I-1)+ETF(I-1))*ARU(I)*UF(I,K)
c     1     -0.5*(H(I)+ETF(I)+H(I-1)+ETF(I-1))*ARU(I)*UVDIF(I,K))/DTI2
c         vvdif(i,k)=( (H(I)+ETF(I))*ARV(I)*VF(I,K)
c     1     -(H(I)+ETF(I))*ARV(I)*VVDIF(I,K) )/DTI2
c        enddo
c       enddo
c      ENDIF
      CALL BCOND(3)
c
cJL   Routine to estimate and save the terms of momentum equation
      IF(MOD(IINT,IPRENERGY).EQ.0) THEN
c         CALL TERMS(uvdif,vvdif,drhox,x)
      ENDIF
c
c    ????????? Check the following lines ??????????????
c     They were not in the original code pom2d.f
c     do i=1,IM
c     do k=1,KBM1
c       VF(i,k)=V(i,k)
c     enddo
c     enddo
cpm  Add a fixed cross shelf velocity
c      do i=1,IM
c        UAF(i)=0.
c        do k=1,KBM1
c          if(H(i)*ZZ(k).lt.-50.) then
c            UF(i,k)=UF(i,k)+0.05*exp(H(i)*ZZ(k)/100.)
c          endif
c        enddo
c      enddo
c      do k=1,KBM1
c      do i=1,IM
c        UAF(i)=UAF(i)+UF(i,k)*DZ(k)
c      enddo
c      enddo
c
cJL   The next lines were the original code from pom2d.f
c     I=10
c     K=1
c     PGRAD=-(VF(I,K)-VB(I,K))/(DTI2*GRAV)
c     DO 365 K=1,KBM1
c     DO 365 I=1,IM
c 365 VF(I,K)=VB(I,K)-PGRAD*DTI2*GRAV
c     DO 366 I=1,IM
c 366 VAF(I)=0.
c     DO 368 K=1,KBM1
c     DO 368 I=1,IM
c 368 VAF(I)=VAF(I)+VF(I,K)*DZ(K)
C
CSB**
      DO 369 I=1,IM
  369 TPS(I)=0.D0
      DO 370 K=1,KBM1
      DO 370 I=1,IM
  370 TPS(I)=TPS(I)+(UF(I,K)+UB(I,K)-2.D0*U(I,K))*DZ(K)
      DO 372 K=1,KBM1
      DO 372 I=1,IM
  372 U(I,K)=U(I,K)+.5*SMOTH*(UF(I,K)+UB(I,K)-2.D0*U(I,K)
     1        -TPS(I))
      DO 3721 I=1,IM
 3721 TPS(I)=0.D0
      DO 374 K=1,KBM1
      DO 374 I=1,IM
  374 TPS(I)=TPS(I)+(VF(I,K)+VB(I,K)-2.D0*V(I,K))*DZ(K)
      DO 376 K=1,KBM1
      DO 376 I=1,IM
  376 V(I,K)=V(I,K)+.5*SMOTH*(VF(I,K)+VB(I,K)-2.D0*V(I,K)
     1        -TPS(I))
      DO 377 K=1,KB
      DO 377 I=1,IM
      UB(I,K)=U(I,K)
      U(I,K)=UF(I,K)
      VB(I,K)=V(I,K)
  377 V(I,K)=VF(I,K)
c
cJL   Relaxing to climatological values
c      if(time .gt. 1.5) then
c       tri=1.*86400.
c       do k=1,KBM1
c        do i=2,IMM1
c        Estimating the phase speed of tangential waves using
c        Orlanski's implicit formulation
c         denom=(uf(i,k)+ub(i,k)-2.*u(i,k))
c         if(denom.EQ.0.)denom=0.01
c         cl=(ub(i,k)-uf(i,k))/denom
c         if(cl.gt.1.)cl=1.
c         if(cl.lt.0.) then
c           cl=0.
c         endif
c         cx=cl*dx(i)/dti
c ******* comment the following line if you want the radiative scheme
c         cx=0
c        Aplying the relaxation scheme into TF
c         t(i,k)=tb(i,k)- dti*( cx*(tb(i+1,k)-tb(i-1,k))/(2*dx(i)) +
c     1          (tb(i,k)-tmean(i,k))/tri )
c        Aplying the relaxation scheme into SF
c         s(i,k)=sb(i,k)- dti*( cx*(sb(i+1,k)-sb(i-1,k))/(2*dx(i)) +
c     1          (sb(i,k)-smean(i,k))/tri )
c        end do
c       end do
c      endif
C
 8200 CONTINUE
CSB**
      DO 8210 I=1,IM
      EGB(I)=EGF(I)
      ETB(I)=ET(I)
      ET(I)=ETF(I)
      DT(I)=H(I)+ET(I)
      UTB(I)=UTF(I)
 8210 VTB(I)=VTF(I)

C##############################################
cvc
CFiltro de nove pontos baroclinico (Asselin)
      ni=0.5
      ni2=ni*ni

      DO 1013 i=2,im-1
      DO 1013 k=2,kb-1

      U(i,k)=U(I,K)+
     . 0.5*ni*(1-ni)*(U(I-1,K)+U(I+1,K)+U(I,K-1)+U(I,K+1)
     .  -4*U(I,K)) + 0.25*ni2*( U(I+1,K-1)+U(I-1,K-1)+U(I-1,K+1)
     .  +U(I+1,K+1)-4*U(I,K) )


      V(i,k)=V(I,K)+
     . 0.5*ni*(1-ni)*(V(I-1,K)+V(I+1,K)+V(I,K-1)+V(I,K+1)
     .  -4*V(I,K)) + 0.25*ni2*( V(I+1,K-1)+V(I-1,K-1)+V(I-1,K+1)
     .  +V(I+1,K+1)-4*V(I,K) )



      IF(H(I).EQ.0.)then
      U(I,K)=0.
      V(I,K)=0.

      END IF

 1013 CONTINUE

      DO I=1,IM
      U(I,1)=U(I,1)
      U(I,KB)=U(I,KB)

      V(I,1)=V(I,1)
      V(I,KB)=V(I,KB)
      END DO

      DO K=1,Kb
      U(1,K)=U(1,K)
      U(IM,K)=U(IM,K)

      V(1,K)=V(1,K)
      V(IM,K)=V(IM,K)

      END DO

C##############################################
cvc


C--------------------------------------------------------------------
C          Average Variable Section
C--------------------------------------------------------------------
c  Mean kinetic (MKE) and Mean Potential (MPE) energy diagnostic
      IF(MOD(IINT,IPRENERGY).EQ.0) THEN
         IENERGY=IENERGY+1
         VTOT=0.D0
         MKE=0.D0
	       MPE=0.D0
Caninha da Rossa


         DO 8310 K=1,KBM1
            DO 8310 I=1,IM
               DVTOT=DX(I)*DT(I)*DZ(K)
               VTOT=VTOT+DVTOT
               MKE=MKE+(UB(I,K)**2+VB(I,K)**2)*DVTOT
	         MPE=MPE+GRAV*ZZ(K)*DT(I)*(RHO(I,K)-RMEAN(I,K))*DVTOT
 8310    CONTINUE
         MKE=MKE/VTOT
    	   MPE=MPE/VTOT
         ENERGY(IENERGY) = MKE
         write(8,'(f9.4,2g16.6)') TIME,MKE,MPE

c
C  Cross-shore current and temperature diagnostic
         UBOT(IENERGY)=UB(IDIAG,KB-2)
         UINT(IENERGY)=UB(IDIAG,KB/2)
         USURF(IENERGY)=UB(IDIAG,2)
         VBOT(IENERGY)=TB(IDIAG,KB-2)+10.
         VINT(IENERGY)=TB(IDIAG,KB/2)+10.
         VSURF(IENERGY)=TB(IDIAG,2)+10.
c        VBOT(IENERGY)=VB(IDIAG,KB-2)
c        VINT(IENERGY)=VB(IDIAG,KB/2)
c        VSURF(IENERGY)=VB(IDIAG,1)
c
      ENDIF

c Calculating averaged variables over a prescribed period PERIODM
      IXMEAN = IXMEAN + 1
      DO K=1,KBM1
      DO I=1,IM
          wmi(i,k)  = wmi(i,k)  + w(i,k)*coper
          umi(i,k)  = umi(i,k)  + u(i,k)*coper
          vmi(i,k)  = vmi(i,k)  + v(i,k)*coper
          tmi(i,k)  = tmi(i,k)  + t(i,k)*coper
          smi(i,k)  = smi(i,k)  + s(i,k)*coper
          kmmi(i,k) = kmmi(i,k) + km(i,k)*coper
          khmi(i,k) = khmi(i,k) + kh(i,k)*coper
cmc        UM(I,K)  = UM(I,K)  + U(I,K)*COPER
cmc        VM(I,K)  = VM(I,K)  + V(I,K)*COPER
cmc        TM(I,K)  = TM(I,K)  + T(I,K)*COPER
cmc        SM(I,K)  = SM(I,K)  + S(I,K)*COPER
cmc        KMM(I,K) = KMM(I,K) + KM(I,K)*COPER
cmc        KHM(I,K) = KHM(I,K) + KH(I,K)*COPER
cmc        RHOM(I,K) = RHOM(I,K) + RHO(I,K)*COPER
cmc        Q2M(I,K) = Q2M(I,K) + Q2(I,K)*COPER
      ENDDO
      ENDDO
      if (ixmean.eq.imean) then
        ixmean = 0
c        if (mod(iint,iprint).ne.0) then

  	do k=1,kbm1
	   write(34,'(800f6.2)') (tmi(i,k)+10.,i=2,IM)
	   write(35,'(800f7.3)') (smi(i,k)+35.,i=2,IM)
	   write(37,'(800g11.3)') (vmi(i,k),i=2,IM)
        enddo

         do k=1,kbm1
          do i=1,im
           wmi(i,k)  = 0.
           umi(i,k)  = 0.
           vmi(i,k)  = 0.
           tmi(i,k)  = 0.
           smi(i,k)  = 0.
           kmmi(i,k) = 0.
           khmi(i,k) = 0.
          enddo
         enddo
        endif
c       endif


C-----------------------------------------------------------------
C                   Begin Print Section
C ----------------------------------------------------------------

C Checking the printing condition (IINT multiple of IPRINT)
      IF(MOD(IINT,IPRINT).NE.0) GO TO 7000
 9001 CONTINUE
        IF(IINT.GE.ISWTCH) IPRINT=IPRTD2*24*3600/int(DTI)
c
c       printing variables to the formatted file fort.6
c23456789012345678901234567890123456789012345678901234567890123456789012
cvc        WRITE(6,'(//40H          ##############################)')
cvc        WRITE(6,'(/7H TIME =,F10.2,9H    IINT =,I8,9H   IEXT =,I8,11H   I
cvc     1PRINT =,I5,9H   MODE =,I5//)')TIME,IINT,IEXT,IPRINT,MODE
C
c      CALL PRXZ(' DRHOX ',TIME,DRHOX,IM,ISKP,KB,0.,DT,ZZ)
c      CALL PRXZ('  TRNU ',TIME,TRNU,IM,ISKP,1,0.,DT,0.)
c      CALL PRXZ('  AAM  ',TIME,AAM,IM,ISKP,KB,0.,DT,ZZ)
c      CALL PRXZ('  ELB  ',TIME,ELB,IM,ISKP,1,0.,DT,0.)
c      CALL PRXZ('  UAB  ',TIME,UAB,IM,ISKP,1,0.,DT,0.)
c      CALL PRXZ('  VAB  ',TIME,VAB,IM,ISKP,1,0.,DT,0.)
c      CALL PRXZ('  RHO  ',TIME,RHO,IM,ISKP,KB,.0001,DT,ZZ)
c      CALL PRXZ('  UB   ',TIME,UB,IM,ISKP,KB,0.,DT,ZZ)
C      CALL PRXZ('  U    ',TIME,U,IM,ISKP,KB,0.,DT,ZZ)
c      CALL PRXZ('  UM   ',TIME,UM,IM,ISKP,KB,0.,DT,ZZ)
c      CALL PRXZ(' WUBOT ',TIME,WUBOT,IM,ISKP,1,0.,DT,ZZ)
c      CALL PRXZ(' WVBOT ',TIME,WVBOT,IM,ISKP,1,0.,DT,ZZ)
c      CALL PRXZ('  VB   ',TIME,VB,IM,ISKP,KB,0.,DT,ZZ)
c     WRITE(6,'(''   PGRAD ='',E12.3)') PGRAD
c      CALL PRXZ(' WVSURF ',TIME,WVSURF,IM,ISKP,1,0.,DT,0.)
c
c      Printing variables to the unformatted file fort.80
c      WRITE(80) TIME,Z,ZZ,H,UB,VB,UAB,VAB,TB,SB,ELB,Q2B,Q2LB,KM,KH
      ITIME=ITIME+1
c       write(6,'(/,33H Writing to fort.80 TIME ITIME : ,f6.2,i6,/)')
c     1       TIME,ITIME
c      WRITE(80) TIME
c      WRITE(80) WUSURF,WVSURF,WTSURF,
c     1  UA,VA,EL,PSI
c
cJL   Introduced in experiment 34 to print averaged variables
c     at itimes 14 and 18. Otherwise, comment the if structure.
c      if( itime.EQ.14 .or. itime.EQ.15 ) then
c         WRITE(80) UM,VM,TM,SM,KMM,KHM,RHOM,Q2M
c      else
c         WRITE(80) U,V,T,S,KM,KH,RHO,Q2
c      endif
c      WRITE(80) ENERGY,UBOT,UINT,USURF,VBOT,VINT,VSURF
c
cjl
c     Routine to mount file 'pom.cdf' to plot using
c     Matlab routines
c      call putcdf2d(time)
c
      VTOT=0.D0
      TTOT=0.D0
      STOT=0.D0
      MKE=0.D0
      MPE=0.D0
      DO 8888 K=1,KBM1
      DO 8888 I=1,IM
      DVTOT=DX(I)*DT(I)*DZ(K)
      VTOT=VTOT+DVTOT
      TTOT=TTOT+TB(I,K)*DVTOT
      STOT=STOT+SB(I,K)*DVTOT
      MKE=MKE+(UB(I,K)**2+VB(I,K)**2)*DVTOT
      MPE=MPE+GRAV*ZZ(K)*DT(I)*(RHO(I,K)-RMEAN(I,K))*DVTOT
 8888 CONTINUE
      TTOT=TTOT/VTOT
      STOT=STOT/VTOT
      MKE=MKE/VTOT
      MPE=MPE/VTOT
cvc
      if(keyprint.eq.1) then
      WRITE(6,'(8H  VTOT =,E20.8,10H    TAVE =,E20.8,10H    SAVE =,E20.8
     1,11H   MKEAVE =,E20.8,/,11H   MPEAVE =,E20.8)') VTOT,TTOT,STOT,MKE
     2,MPE
      endif
cvc
 7000 CONTINUE
c
cJL   File 74 to reinitialise the model (will be file 70 to read)
c      if ( IINT.EQ.ifix(13*PERIOD*24.*3600/DTI) ) then
c         write(6,'(24H Saving fort.74 TIME = ,g11.3)')TIME
c         WRITE(74) TIME,
c     1       WUBOT,WVBOT,AAM2D,UA,UAB,VA,VAB,EL,ELB,ET,ETB,EGB,
c     2       UTB,VTB,U,UB,W,V,VB,T,TB,S,SB,RHO,ADVUU,ADVVV,ADVUA,
c     3       ADVVA,KM,KH,KQ,L,Q2,Q2B,AAM,Q2L,Q2LB
c
c         CALL PRXZ('   VAB   ',TIME,VAB,IM,ISKP,1,0.,DT,0.)
c         CALL PRXZ('    V    ',TIME,V ,IM,ISKP,KB,0.,DT,ZZ)
c         CALL PRXZ('    VM    ',TIME,VW ,IM,ISKP,KB,0.,DT,ZZ)
c         write(6,'(/,16H IXMEAN,IMEAM : ,2i10,/)') IXMEAN,IMEAN
c      endif
c
cJL   Alteration performed to avoid problems when
c     printing averaged variables with frequency
c     IPRTD1.     J.Lima 21/Mar/96
cvc-CMC
c
c      IF (IXMEAN.EQ.IMEAN) THEN
cvc          write(6,'(13H int,imean : ,2i6)') iint,imean
cvc          write(6,'(14H U,UM(89,4) : ,2g12.3)')U(89,4),UM(89,4)
cvc          write(6,'(14H T,TM(89,4) : ,2g12.3)')T(89,4),TM(89,4)
c          IXMEAN = 0
c          DO K=1,KBM1
c          DO I=1,IM
c            UM(I,K)  = 0.
c            VM(I,K)  = 0.
c            TM(I,K)  = 0.
c            SM(I,K)  = 0.
c            KMM(I,K) = 0.
c            KHM(I,K) = 0.
c          ENDDO
c          ENDDO
c      ENDIF
cvc-CMC
C--------------------------------------------------------------------
C             END PRINT SECTION
C--------------------------------------------------------------------

      IF(ABS(VAMAX).GT.100.D0.OR.ABS(VAMIN).GT.100.D0) THEN
      WRITE(6,'(///,''ABNORMAL END'')')
cvc Grava quando estoura e finaliza a simulacao
       write(*,*) ' ABNORMAL END DAY: ', time
       goto 9020

      STOP
      ENDIF
c
cjl   Printing to debug the code. Put in comment later
      if (mod(iint,4).EQ.0.) then
c         call putcdf2d(time)
      endif
c
9000               CONTINUE
9020  continue
c             END OF INTERNAL TIME STEP LOOP
c  -------------------------------------------------------------------
c
c
cjl   Getting the CPU time associated with the setup
c     using IMSL library
c      cput2=CPSEC()
c     using PORTLIB
c	cput2=time()
c 	WRITE (6,*) '/Overall CPU time spent (seconds) = ',cput2-cput0
c
c
cjl   Calculate the potential vorticity field
      call pot_vort(potvort,dpvortdx,bv2)


cvc Impressao final
cvc      CALL PRXZ(' B-V FREQ.  ',TIME,BV2 ,IM,ISKP,KB,0.D0,DT,ZZ)
cvc

cvc Final da simulacao
      write(*,*) '*******************************************'
      write(*,*) '*****************THE END*******************'
      write(*,*) '*******************************************'

cJL   The routine MOORING was designed to save current and
c     temperature grid points and store them in file MOORING.DAT
      CALL MOORING1(X,potvort,dpvortdx,bv2)
      close(7)
c
c   Printing file FORT.75 to reinitialize the model
c      WRITE(75) TIME,
c     1           WUBOT,WVBOT,AAM2D,UA,UAB,VA,VAB,EL,ELB,ET,ETB,EGB,
c     2           UTB,VTB,U,UB,W,V,VB,T,TB,S,SB,RHO,ADVUU,ADVVV,ADVUA,
c     3           ADVVA,KM,KH,KQ,L,Q2,Q2B,AAM,Q2L,Q2LB
c
	do k=1,kbm1
	   write(24,'(800f6.2)') (T(i,k)+10.,i=2,IM)
	   write(25,'(800f7.3)') (S(i,k)+35.,i=2,IM)
	   write(26,'(800f6.2)') (rho(i,k)*1000+1025,i=2,IM)
	   write(27,'(800g11.3)') (vb(i,k),i=2,IM)
	   write(28,'(800g14.5)') (potvort(i,k),i=2,IM)
      enddo
cjl	Mount an ASCII Buoyancy frequency file
      open(29,file='bvfreq.DAT',status='unknown')
      do k=1,kbm1
	  write(29,'(800g14.5)') (bv2(i,k),i=2,IM)
	enddo
	close(29)
      open(29,file='dpotdx.DAT',status='unknown')
      do k=1,kbm1
	  write(29,'(800g14.5)') (dpvortdx(i,k),i=2,IM)
	enddo
	close(29)
      STOP
      END
C****************************************************************************
      SUBROUTINE INIT(AAA,ISKP,BOTTOM,UTB,VTB,UTF,VTF,
     2       ADVUA,ADVVA,TSURF,SSURF,DRHOX,TRNU,TMEAN,
     3       SMEAN,ADVUU,ADVVV,X,RX,RHOX)
C
C    ROUTINE TO SETUP INITIAL CONDITIONS FOR THE POM-2D
C    MODEL. THE PARTICULAR SETUP OF THIS ROUTINE IS FOR
C    A TRANSECT OF THE SOUTHEAST BRAZILIAN COAST
C
C    J.LIMA 24/04/96
c
	include 'comblk2d.h'
c
cvc
      CHARACTER*26 FILENAME1,FILENAME2,FILENAME3,FILENAME4,
     1 FILENAME5,FILENAME6,FILENAME7,FILENAME8,FILENAME9,
     2 FILENAME10,FILENAME11,FILENAME12,FILENAME13,
     3 FILENAME14, FILENAME15, FILENAME16
cvc
      DIMENSION UTB(IM),VTB(IM),UTF(IM),VTF(IM),
     2       ADVUA(IM),ADVVA(IM),TSURF(IM),SSURF(IM),
     3       DRHOX(IM,KB),TRNU(IM),
     4       TMEAN(IM,KB),SMEAN(IM,KB),ADVUU(IM),ADVVV(IM),
     5       X(IM),RX(KB),RHOX(IM,KB)
      DIMENSION TEMP(151),SAL(151),P(151)
      DATA PI/3.1416D0/,SMALL/1.D-10/,BETA/2.111D-11/
C
      DAYI=1.D0/86400.D0
C
C*SB* SETUP GRID AND TEST INITIAL CONDITIONS
      WO=100.D3
      DO=4000.
c      CALL  DEPTH(Z,ZZ,DZ,DZZ,DZR,KB,KB-1)

      open(10,file='namesxz.in',status='unknown')
      READ(10,'(A2)') COMEN
      READ(10,'(A2)') COMEN
      READ(10,'(A2)') COMEN
      READ(10,'(A26)') FILENAME1
      READ(10,'(A2)') COMEN
      READ(10,'(A2)') COMEN
      READ(10,'(A26)') FILENAME2
      READ(10,'(A2)') COMEN
      READ(10,'(A2)') COMEN
      READ(10,'(A26)') FILENAME3
      READ(10,'(A2)') COMEN
      READ(10,'(A2)') COMEN
      READ(10,'(A26)') FILENAME4
      READ(10,'(A2)') COMEN
      READ(10,'(A2)') COMEN
      READ(10,'(A2)') COMEN
      READ(10,'(A2)') COMEN
      READ(10,'(A26)') FILENAME5
      READ(10,'(A26)') FILENAME6
      READ(10,'(A26)') FILENAME7
      READ(10,'(A26)') FILENAME8
      READ(10,'(A26)') FILENAME9
      READ(10,'(A26)') FILENAME10
      READ(10,'(A26)') FILENAME11
      READ(10,'(A26)') FILENAME12
      READ(10,'(A26)') FILENAME13
      READ(10,'(A26)') FILENAME14
      READ(10,'(A26)') FILENAME15
      READ(10,'(A26)') FILENAME16
      close(10)
cJL
C     Setup the sigma levels for the SE Brazilian coast
cvc      open(11,file='p1000s.sgm',status='old')
      open(11,file=FILENAME1,status='old')
      do i=1,KB
         read(11,*) z(i)
      enddo
      do i=1,KB-1
         zz(i)=(z(i)+z(i+1))/2.
      enddo
      zz(KB)=zz(KB-1)+(zz(KB-1)-zz(KB-2))
      do i=1,KB-1
         dz(i)=abs(z(i+1)-z(i))
         dzz(i)=abs(zz(i+1)-zz(i))
         dzr(i)=1/dz(i)
      enddo
      dz(KB)=0.
      dzz(KB)=0.
      dzr(KB)=0.
c      write(6,'(31H Sigma Levels: Z,ZZ,DZ,DZZ,DZR )')
c      do i=1,KB
c        write(6,'(4f11.7,f8.2)') z(i),zz(i),dz(i),dzz(i),dzr(i)
c      enddo
      close(11)
cJL Reading bathymetric data for the SE Brazilian coast
C  The correct way to calculate DX for the C-stagered grid is
C  to evaluate the X-length of the grid cell. If the X is given
C  in the same position of the H (in the center of the cell), the
C  correct DX should be DX(i)=(x(i)+x(i+1))/2 - (x(i)+x(i-1))/2
C
cvc
cvc     .,status='old')
       open(11,file=FILENAME2,status='old')

c      write(6,'(25H Topography Data: X,H,DX )')
      do 100 i=1,im
         read(11,*) X(i),H(i),DX(i)
         dx(i)=dx(i)*1000
         x(i)=x(i)*1000
c         write(6,'(3f8.1)') X(i),H(i),DX(i)
c         write(6, '(i4)') i


 100  continue
      close(11)
      H(1)=1.
      bottom=H(IM)
c-------------------------------------------------------------
c
c     Setting the vectors with masks for the free surface, temperature,
c     salinity and turbulent variables (FSM), for the U-velocity (DUM)
c     and for the V-velocity (DVM).
c     For the coast (or land), it is assumed H(i)=1. and the mask
c     vectors will be set to 0.
      DO 30 I=1,IM
         FSM(I)=1.D0
         DUM(I)=1.D0
         DVM(I)=1.D0
      IF(H(I).GT.1.D0) GO TO 30
         FSM(I)=0.D0
         DUM(I)=0.D0
         DVM(I)=0.D0
 30   CONTINUE
c
c     IMPORTANT: The following lines are setting the mask vectors
c                to 0. at the seaward boundary. It means that all
c                variables will be set to zero there and this
c                corresponds to a no-flux BC. If it is desired
c                to use another BC's (gradient,radiation,etc.),
c                the following three lines must be commented.
c      FSM(IM)=0.D0
c      DUM(IM)=0.D0
c      DVM(IM)=0.D0
      FSM(IM)=1.D0
      DUM(IM)=1.D0
      DVM(IM)=1.D0
c
c      DO 35 I=1,IM
c      IF(FSM(I).EQ.0.D0) DVM(I)=0.D0
c 35   CONTINUE
      DO 40 I=1,IMM1
      IF(FSM(I).EQ.0.D0.AND.FSM(I+1).NE.0.D0) DUM(I+1)=0.D0
 40   CONTINUE
cvc suavizacao da batimetria
c      CALL SLPMIN(H,IM,FSM)
CSB**
c      CALL PRXZ(' FSM ',TIME,FSM,IM,ISKP,1,0.,DT,0.)
c      CALL PRXZ(' DUM ',TIME,DUM,IM,ISKP,1,0.,DT,0.)
c      CALL PRXZ(' DVM ',TIME,DVM,IM,ISKP,1,0.,DT,0.)
 400  CONTINUE
      DO 11 K=1,KB
      DO 11 I=1,IM
         KH(I,K)=1.D-3
         KM(I,K)=KH(I,K)
  11  AAM(I,K)=AAA
      DO 12 I=1,IM
         ART(I)=DX(I)
         ARU(I)=DX(I)
         ARV(I)=DX(I)
  12  CONTINUE
cJL   Setting the Coriolis parameter
      DO 14 I=1,IM
	   FCOR=2.*7.292D-5*sin(XLAT*pi/180.)
         COR4(I)=FCOR/4.       ! = COR/4 comment PM
  14  CONTINUE
cJL
c     Reading temperature profile for the SE Brazilian transect
c     with flat stratification (T-S horizontal field)
c      open(12,file='p500m.tmp',status='old')
c      write(6,'(32H Temperature Profile: P,TEMP,SAL)')
c      do kk=1,132
c         read(12,'(3f10.4)') p(kk),temp(kk),sal(kk)
c         write(6,'(3f10.4)') p(kk),temp(kk),sal(kk)
c      enddo
c
c     Interpolating the temperature profile in the grid spacing
c     The loop was not extended to KB because the program does not
c     use the level KB. See O'Connor POM manual pag.49.
c      do i=1,im
c         do k=1,kbm1
c            z0=-H(i)*ZZ(k)
c            if(z0.lt.p(1))then
c               T(i,k)=temp(1)
c               S(i,k)=sal(1)
c            elseif(z0.gt.p(132))then
c               T(i,k)=temp(132)
c               S(i,k)=sal(132)
c            else
c               do kk=1,131
c                  if(z0.ge.p(kk) .and. z0.le.p(kk+1)) then
c                     T(i,k)=temp(kk)+(z0-p(kk))*
c     1                      (temp(kk+1)-temp(kk))/(p(kk+1)-p(kk))
c                     S(i,k)=sal(kk)+(z0-p(kk))*
c     1                      (sal(kk+1)-sal(kk))/(p(kk+1)-p(kk))
c                  endif
c               enddo
c            endif
c
c           Estimating the salinity by polynomial fitting (ref:Signorini)
C           PS: This fitting works Ok until 1100ms, but doesn't
C               work for greater depths.
c            S(i,k)=35.096-4.02214D-1*T(i,k)+7.39458D-2*T(i,k)**2-
c     1          5.11149D-3*T(i,k)**3+1.73336D-4*T(i,k)**4-
c     2          0.22322D-5*T(i,k)**5
c         enddo
c      enddo
c
cJL
c     Reading Temperature and Salinity structure from files

cvc     .,status='old')
         open(12,file=FILENAME3,status='old')
c      do i=1,im
         do kk=1,kb
            read(12,*) (T(i,kk),i=1,IM)
         enddo
c         T(i,kb)=0.0
c      enddo
      close(12)
cvc     .,status='old')
      open(12,file=FILENAME4,status='old')
c      do i=1,im
         do kk=1,kb
            read(12,*) (S(i,kk),i=1,IM)
         enddo
c         S(i,kb)=0.
c      enddo
      close(12)
c
cJL   Checking the CPU time for printing routine
c      t0=second()
c      overhead=second()-t0
c      before=second()
c      CALL PRXZ(' Initial Temp.  ',TIME,T,IM,ISKP,KB ,0.  ,DT,ZZ)
c      CALL PRXZ(' Initial Sal.   ',TIME,S,IM,ISKP,KB ,0.  ,DT,ZZ)
c      after=second()
c      cputime=(after-before)-overhead
c      write(6,'(25H CPU Time for printing = ,g12.4)') cputime
c     Adjusting the values for POM use
      do i=1,IM
          do k=1,KB
c            the next two lines were included to simulate a barotropic
c            ocean. Comment it in the normal runs.

cvc
C Campo barotropico medio
C Expressoes do ajuste exponencial a a partir do campo medio do
c levitus et. al. (1994), feito pelo programa tsfit.m
c      T(I,K)=3.04+18.1734*EXP(ZZ(K)*H(I)/415.9556)
c      IF(T(I,K).LT.(11.-10)) T(I,K)=11.-10
c
c      S(I,K)=33+2.8317*EXP(ZZ(K)*H(I)/0.6974e03)
c     .+0.2546*EXP(ZZ(K)*H(I)/-1.1176e03 )
c      S(i,k)=S(i,k)-35.
cvc

cvc
c             T(i,k)=20.0
c             S(i,k)=35.5
cvc

             T(i,k)=T(i,k)-10.
             S(i,k)=S(i,k)-35.
          enddo
      enddo
c
c     Initializing the array DT(i) that will be used in routine DENS
      do i=1,IM
	  DT(i)=H(i)
	enddo
C     Calculating the density
      CALL DENS
C
C     Initializing temperature and salinity vectors
      DO 81 K=1,KB
      DO 81 I=1,IM
         TB(I,K)=T(I,K)
         SB(I,K)=S(I,K)
c	   write(6,'(2i5,2f12.5)') i,k,T(i,k),S(i,k)
         TMEAN(I,K)=T(I,K)
         SMEAN(I,K)=S(I,K)
  81  RMEAN(I,K)=RHO(I,K)
      DO 82 I=1,IM
         DT(I)=H(I)
         TSURF(I)=TB(I,1)
  82  SSURF(I)=SB(I,1)



cvc Arquivos de saida
      OPEN(22,FILE=FILENAME5,STATUS='UNKNOWN')
      OPEN(23,FILE=FILENAME6,STATUS='UNKNOWN')
      OPEN(24,FILE=FILENAME7,STATUS='UNKNOWN')
      OPEN(25,FILE=FILENAME8,STATUS='UNKNOWN')
      OPEN(26,FILE=FILENAME9,STATUS='UNKNOWN')
      OPEN(27,FILE=FILENAME10,STATUS='UNKNOWN')
      OPEN(28,FILE=FILENAME11,STATUS='UNKNOWN')
      OPEN(7,FILE=FILENAME12,STATUS='UNKNOWN')
      OPEN(8,FILE=FILENAME13,STATUS='UNKNOWN')
      OPEN(34,FILE=FILENAME14,STATUS='UNKNOWN')
      OPEN(35,FILE=FILENAME15,STATUS='UNKNOWN')
      OPEN(37,FILE=FILENAME16,STATUS='UNKNOWN')
c      OPEN(34,FILE='Tmean.DAT',STATUS='UNKNOWN')
c      OPEN(35,FILE='Smean.DAT',STATUS='UNKNOWN')
c      OPEN(37,FILE='Vmean.DAT',STATUS='UNKNOWN')
cvc
c---------------------------------------------------------
c
cpm  Initialisation of an alongshore velocity
cpm  in geostrophic balance
      CALL BAROPG(DRHOX,TRNU)
cpm  Parameters to simulate a gaussian jet on the surface
c     CALL PRXZ('  DRHOX     ',0.,DRHOX,IM,1,KB,0.,DT,ZZ)
c      I0=73       ! center of the jet
c      gap=22.**2  ! half width of the jet **2
c      V0=-1.0     ! max jet's velocity
      do I=1,IM
cpm     the next line should be uncommented if using jet
c        VB(I,1)=V0*exp(-(dfloat(I)-dfloat(I0))**2/gap)
        VB(I,1)=0.
        VAB(I)=0.
c       velocity with level O (reference level in the surface)
        do K=2,KBM1
          VB(I,K)=VB(I,1)
c     &   + .5*(DRHOX(I,K)+DRHOX(I-1,K))/(4.*COR4(I)*DT(I)*ARU(I))
        enddo
      enddo
cJL
c PS: The next two blocks of DO loops calculated the adjusted
c     geostrophic velocities for a specific level of no motion
c     The first loop does not include any barotropic field,
c     and the second does. You should select the one that is
c     more appropriate for your problem, and comment the other.
c
cJL   adjusting the velocity field to level of no motion at 350 ms
c      do i=1,IM
c        For water depths less than the depth of no motion
c         if(H(i).le.350) then
c            do k=1,KBM1
c               VB(i,k)=VB(i,k)-VB(i,KBM1)
c            enddo
c         else
c           For water depths greater than level of no motion,
cJL         interpolate the velocity to find V at 350 ms
c           zz350 is used for the baroclinic field
c            zz350=-350./H(i)
c            j=1
c            do while (zz(j).gt.zz350)
c               j=j+1
c            enddo
c            kref=j-1
c            if (zz(kref).EQ.zz350) then
c                VREF=VB(i,kref)
c            else
c                VREF=VB(i,kref-1)+ (VB(i,kref)-VB(i,kref-1))*
c     1               (ZZ350-Z(kref-1))/(Z(kref)-Z(kref-1))
c            endif
c            do k=1,KBM1
c               VB(i,k)=VB(i,k)-VREF
c            enddo
c         endif
c      enddo
c
c     The other do loop that includes the barotropic velocity
cJL   adjusting the velocity field to level of no motion at 500 ms
c      do i=1,IM
c        For water depths less than the depth of no motion
c         if(H(i).le.500) then
c            do k=1,KBM1
c              barotropic current vbar
c               if( x(i).le.55000.0) then
c                  vbar=0.005
c               elseif( x(i).gt.55000.0 .and. x(i).le.70000.0 ) then
c                  vbar=0.005 + (x(i)-55000.0)*0.095/15000.0
c                  vbar=0.005 + (x(i)-55000.0)*0.045/15000.0
c               else
c                  vbar=0.10
c                  vbar=0.05
c               endif
c              For depths between 400-500 ms, use vbar=0.10
c               if (abs(zz(k)*H(i)).ge.400.0) then
c                  if( x(i).le.106000.0) then
c                     vbar=0.10
c                  elseif (x(i).gt.116000 .and. x(i).le.138000)then
c                  elseif (x(i).gt.106000 .and. x(i).le.128000)then
c                     vbar=0.10 + (x(i)-116000)*(-0.095/22000)
c                     vbar=0.10 + (x(i)-106000)*(-0.095/22000)
c                  else
c                     vbar=0.005
c                  endif
c               endif
c               VB(i,k)=VB(i,k)-VB(i,KBM1)+vbar
c            enddo
c         else
c           For water depths greater than level of no motion,
cJL         interpolate the velocity to find V at 500 ms
c           zz500 is used for the baroclinic field
c            zz500=-500./H(i)
c            j=1
c            do while (zz(j).gt.zz500)
c               j=j+1
c            enddo
c            kref=j-1
c            if (zz(kref).EQ.zz500) then
c                VREF=VB(i,kref)
c            else
c                VREF=VB(i,kref-1)+ (VB(i,kref)-VB(i,kref-1))*
c     1               (ZZ500-Z(kref-1))/(Z(kref)-Z(kref-1))
c            endif
c            do k=1,KBM1
c              Upper layer Brazil Current
c               if (zz(k).gt.zz500) then
c                 Barotropic velocity component vbar
c                  if( x(i).le.106000.0) then
c                     vbar=0.10
c                     vbar=0.05
c                  elseif (x(i).gt.116000 .and. x(i).le.138000)then
c                  elseif (x(i).gt.106000 .and. x(i).le.128000)then
c                     vbar=0.10 + (x(i)-116000)*(-0.095/22000)
c                     vbar=0.05 + (x(i)-116000)*(-0.045/22000)
c                     vbar=0.05 + (x(i)-106000)*(-0.045/22000)
c                  else
c                     vbar=0.005
c                  endif
c                 For depths between 400-500 ms, use vbar=0.10
c                  if (abs(zz(k)*H(i)).ge.400.0) then
c                     if( x(i).le.106000.0) then
c                        vbar=0.10
c                     elseif (x(i).gt.116000 .and. x(i).le.138000)then
c                        vbar=0.10 + (x(i)-116000)*(-0.095/22000)
c                     elseif (x(i).gt.106000 .and. x(i).le.128000)then
c                        vbar=0.10 + (x(i)-106000)*(-0.095/22000)
c                     else
c                        vbar=0.005
c                     endif
c                  endif
c               else
c                 Lower layer AIA
c                  if( x(i).le.106000.0) then
c                     vbar=0.15
c                  elseif (x(i).gt.116000 .and. x(i).le.138000)then
c                     vbar=0.15 + (x(i)-116000)*(-0.145/22000)
c                  elseif (x(i).gt.106000 .and. x(i).le.128000)then
c                     vbar=0.15 + (x(i)-106000)*(-0.145/22000)
c                  else
c                     vbar=0.005
c                  endif
c               endif
c               VB(i,k)=VB(i,k)-VREF + vbar
c            enddo
c         endif
c      enddo
c
C     integrating along the vertical
      do K=1,KBM1
         do I=1,IM
           VAB(I)=VAB(I)+VB(I,K)*DZ(K)
         enddo
      enddo
cpm  Surface elevation
      ELB(IM)=0.
      DO I=IM,2,-1
      ELB(I-1)=ELB(I)-.5*(DX(I)+DX(I-1))*(COR4(I)+COR4(I-1))
     &                  *(VB(I,1)+VB(I-1,1))/GRAV
      ENDDO
      ACCUM=0.
      ACCUM1=0.
      DO I=1,IM
         ELB(I)=ELB(I)*FSM(I)
         ACCUM1=ACCUM1+ART(I)*FSM(I)
         ACCUM=ACCUM+ELB(I)*ART(I)
      ENDDO
      DO I=1,IM
         ELB(I)=(ELB(I)-ACCUM/ACCUM1)*FSM(I)
      ENDDO
c
c      CALL PRXZ('  ELB    ',0.,ELB,IM,ISKP,1,0.,DT,ZZ)
c      CALL PRXZ('  VB     ',0.,VB,IM,ISKP,KB,0.,DT,ZZ)
c      CALL PRXZ('  VAB    ',0.,VAB,IM,ISKP,1,0.,DT,ZZ)
C------------------------------------------------------------------------
C     Initialize values of the centered time step to be used in
C     the Leapfrog scheme for the first time step
C------------------------------------------------------------------------

      DO 50 I=1,IM
         L(I,1)=0.D0
   50 L(I,KB)=0.D0
      DO 51 I=1,IM
         UA(I)=UAB(I)
         VA(I)=VAB(I)
         EL(I)=ELB(I)
         ET(I)=ETB(I)
         ETF(I)=ET(I)
         D(I)=H(I)+EL(I)
   51 DT(I)=H(I)+ET(I)
      DO 52 K=1,KB
         DO 52 I=1,IM
            L(I,K)=Q2LB(I,K)/(Q2B(I,K)+SMALL)
            KQ(I,K)=0.2D0*L(I,K)*SQRT(Q2B(I,K))
            Q2(I,K)=Q2B(I,K)
            Q2L(I,K)=Q2LB(I,K)
            T(I,K)=TB(I,K)
            S(I,K)=SB(I,K)
            U(I,K)=UB(I,K)
   52 V(I,K)=VB(I,K)
C
C     END OF SETUP
      RETURN
      END
C******************************************************************
      SUBROUTINE MOORING1(X,potvort,dpvortdx,bv2)
C
C     ROUTINE TO STORE VALUES OF CURRENT AND TEMPERATURE AT
C     SPECIFIC GRID POINTS, AS IF THEY WERE MOORINGS.
C     J.LIMA 9/11/98
      include 'comblk2d.h'
c
      DIMENSION X(IM),potvort(im,kb),BV2(IM,KB),dpvortdx(im,kb),
     1          xest(10),imo(10)
C
C     POSITION "xest" ALONG THE X-AXIS OF SPECIFIC MOORING SITES
c     nm = number of points to print values as mooring points
c     imo = i-grid position associated with the xest(i) points
      nm=6
      xest(1)=140546.6D0
      xest(2)=153569.7D0
      xest(3)=166305.8D0
      xest(4)=179519.8D0
      xest(5)=192825.8D0
      xest(6)=205806.4D0
	do m=1,nm
	  xmin=1.D10
	  imo(m)=1
	  do i=2,im
	    xd=sqrt((x(i)-xest(m))**2)
	    if (xd.lt. xmin) then
		  xmin=xd
	      imo(m)=i
	    endif
	  enddo
	enddo
c     Printing velocity, temperature and Brunt-Vaisala frequency profiles
      do i=1,nm
	 do k=2,kb
        WRITE(7,5) TIME,(X(imo(i))+X(imo(i)+1))/2,
     1   H(imo(i))*(ZZ(k-1)+ZZ(k))/2.,
	2   ((UB(imo(i),k-1)+UB(imo(i)+1,k-1))/2+
     3   (UB(imo(i),k)+UB(imo(i)+1,k))/2)/2.,VB(imo(i),k),
     4   ((TB(imo(i),k-1)+TB(imo(i),k))/2.)+10.D0,
     5   ((SB(imo(i),k-1)+SB(imo(i),k))/2.)+35.D0,
	6   ((RHO(imo(i),k-1)+RHO(imo(i),k))/2.)*1000.+1025.,
     7   BV2(imo(i),k),potvort(imo(i),k),
	8   4*cor4(imo(i))*(-bv2(imo(i),k))/grav,dpvortdx(imo(i),k)
	 enddo
	enddo

C
 5    FORMAT(F8.3,F8.0,F10.3,4F8.3,F10.3,4G15.6)
      RETURN
      END
C #############################################################################
      subroutine pot_vort(potvort,dpvortdx,bv2)
c
c     Subroutine to calculate the Brunt-Vaisala Frequency
c     and the potential vorticity
c
c     IMPORTANT
c       In this code, it is adopted a z-reference axis pointing
c     upwards. The values of ZZ(k) are negative, and all the
c     vertical derivatives are backwards differences such as
c     (ZZ(k-1)-ZZ(k)).
c
c     Jose A. Lima - 26/11/98
c
      include 'comblk2d.h'
c
      dimension xflux(im,kb),drhods(im,kb),drhodx(im,kb),aux(im,kb),
     1          duds(im,kb),dvds(im,kb),potvort(im,kb),rvortbrc(im,kb),
     2          dpvortdx(im,kb),bv2(im,kb),sigtheta(im,kb)
c     Initialize the matrices
      DO I=1,IM
       DO k=1,kb
          rvortbrc(I,k)=0.
	  potvort(i,k)=0.
	  dpvortdx(i,k)=0.
	  bv2(i,k)=0.
       ENDDO
      ENDDO
c
c     Calculate sigma-theta to be used in the calculus of Brunt-Vaisala
c     Frequency
      call sigmat(sigtheta)
c
c     Convert the RHO array into its real dimensions
      do i=1,IM
	  do k=1,kb
	    rho(i,k)=rho(i,k)*1000.D0+1025.D0
	  enddo
	enddo
c
c     Relative vorticity (for POM-2D is only dv/dx)
      do i=2,imm1
       do k=1,kbm1
        rvortbrc(I,k)=((VB(I,k)-VB(I-1,k))/(0.5*(DX(I)+DX(I-1)))
     1    +(VB(I+1,k)-VB(I,k))/(0.5*(DX(I+1)+DX(I))))/2.
       ENDDO
       rvortbrc(i,kb)=rvortbrc(i,kbm1)
      enddo
c
c     Calculate the Squared Brunt-Vaisala Frequency
c     (units of (rad/sec)**2 )
c     obs: Note that RHO is potential density
      do i=2,im
        DO K=2,KB-1
         BV2(i,K)=-GRAV*(sigtheta(i,K-1)-sigtheta(i,K))/
     1            (H(i)*(ZZ(K-1)-ZZ(K)))
         BV2(i,k)=BV2(i,k)/((RHO(i,K-1)+RHO(i,K))/2)
        enddo
        BV2(i,1)=BV2(i,2)
        BV2(i,KB)=BV2(i,KBM1)
      enddo

cjl
c     Estimate the potential vorticity using Ertel's theorem
C     PS: The piece of code below was modified from the routine
c         VORTZE in code POM97BR.f
C     DENSITY ADVECTION FLUXES
      DO 530 K=1,KBM1
       DO 530 I=2,IM
        XFLUX(I,K)=.5D0*(sigtheta(I,K)+sigtheta(I-1,K))
 530  continue
C
C     DENSITY VERTICAL ADVECTION
      DO I=2,IMM1
        do k=2,kbm1
         drhods(i,K)=(sigtheta(i,K-1)-sigtheta(i,K))/
     1            (H(i)*(ZZ(K-1)-ZZ(K)))
        enddo
        drhods(i,1)=drhods(i,2)
        drhods(i,kb)=drhods(i,kbm1)
      enddo
c
C     ADD NET HORIZONTAL FLUXES
      DO 120 K=1,KBM1
       DO 120 I=2,IMM1
        drhodx(i,k)=(XFLUX(I+1,K)-XFLUX(I,K))/ART(I)
 120  CONTINUE
c
c     VERTICAL DERIVATIVES OF HORIZONTAL VELOCITIES
c     U-velocity
      DO K=1,KBM1
       DO I=2,IMM1
        aux(i,k)=(UB(I,K)+UB(I+1,K))/2.
       enddo
      enddo
      do i=2,IMM1
       DO K=2,KBM1
        duds(I,K)=(aux(I,K-1)-aux(I,K))/
     1            (H(i)*(ZZ(K-1)-ZZ(K)))
       enddo
       duds(i,1)=duds(i,2)
       duds(i,kb)=duds(i,kbm1)
      enddo
c     V-velocity
      DO K=1,KBM1
       DO I=2,IMM1
        aux(i,k)=VB(I,K)
       enddo
      enddo
      DO I=2,IMM1
       DO K=2,KBM1
        dvds(I,K)=(aux(I,K-1)-aux(I,K))/
     1            (H(i)*(ZZ(K-1)-ZZ(K)))
       enddo
       dvds(i,1)=dvds(i,2)
       dvds(i,kb)=dvds(i,kbm1)
      enddo
c
C     Potential vorticity (Cushman-Roisin pag.174-eq.12-19)
c     converted from z-coordinate to sigma-coordinate
      do i=2,imm1
       DO K=2,KBM1
        potvort(I,K)=4.*cor4(i)*drhods(i,k)+
c     1     4.*cor4(i)*(-BV2(i,k)/((RHO(i,K-1)+RHO(i,K))/2))/grav+
     1            drhods(i,k)*rvortbrc(i,k)-
     2            dvds(i,k)*drhodx(i,k)
	  potvort(i,k)=potvort(i,k)/((RHO(i,K-1)+RHO(i,K))/2)
       enddo
       potvort(i,1)=potvort(i,2)
       potvort(i,kb)=potvort(i,kbm1)
      enddo
c
c     Calculate the x-derivative of potential vorticity
      DO K=1,KBM1
       DO I=2,IM
        XFLUX(I,K)=.5D0*(potvort(I,K)+potvort(I-1,K))
       enddo
	enddo
	DO I=2,IMM1
       do k=1,kbm1
       	  dpvortdx(i,k)=(XFLUX(I+1,K)-XFLUX(I,K))/ART(I)
       ENDDO
	 dpvortdx(i,kb)=dpvortdx(i,kbm1)
	enddo
c
c     Print values during debug. Comment later.
c      do k=1,kb
c	   write(6,'(8g15.6)') H(60)*ZZ(k),
c     1        4.*cor4(60)*drhods(60,k),drhods(60,k)*rvortbrc(60,k),
c	2        dvds(60,k)*drhodx(60,k),drhods(60,k),rvortbrc(60,k),
c	3        dvds(60,k),rho(i,k)
c	enddo
c
c
c     Re-convert the RHO array into its dimensions in POM
      do i=1,IM
	  do k=1,kb
	    rho(i,k)=(rho(i,k)-1025.D0)/1000.D0
	  enddo
	enddo
      return
      end
c##############################################################################
      subroutine pot_old(potvort,dpvortdx,bv2)
c
c     Subroutine to calculate the Brunt-Vaisala Frequency
c     and the potential vorticity
c
c     Jose A. Lima - 11/11/98
c
      include 'comblk2d.h'
c
      dimension xflux(im,kb),drhods(im,kb),drhodx(im,kb),aux(im,kb),
     1          duds(im,kb),dvds(im,kb),potvort(im,kb),rvortbrc(im,kb),
     2          dpvortdx(im,kb),bv2(im,kb),sigtheta(im,kb)
c     Initialize the matrices
      DO I=1,IM
       DO k=1,kb
        rvortbrc(I,k)=0.
	  potvort(i,k)=0.
	  dpvortdx(i,k)=0.
	  bv2(i,k)=0.
       ENDDO
      ENDDO
c
c     Calculate sigma-theta to be used in the calculus of Brunt-Vaisala
c     Frequency
      call sigmat(sigtheta)
c
c     Convert the RHO array into its real dimensions
      do i=1,IM
	  do k=1,kb
	    rho(i,k)=rho(i,k)*1000.D0+1025.D0
	  enddo
	enddo
c
c     Relative vorticity (estimated as in routine CURL from the
c     original code VORT.f)
      do k=1,kbm1
       DO I=2,IM
         rvortbrc(I,k)=(VB(I,k)-VB(I-1,k))/(0.5*(DX(I)+DX(I-1)))
       ENDDO
	enddo
c
c     Calculate the Squared Brunt-Vaisala Frequency
c     (units of (rad/sec)**2 )
c     obs: Note that RHO is potential density
      do i=2,im
        DO K=2,KB-1
         BV2(i,K)=-GRAV*(sigtheta(i,K-1)-sigtheta(i,K))/
     1            (H(i)*(ZZ(K-1)-ZZ(K)))
         BV2(i,k)=BV2(i,k)/((RHO(i,K-1)+RHO(i,K))/2)
        enddo
        BV2(i,1)=BV2(i,2)
        BV2(i,KB)=BV2(i,KBM1)
      enddo

cjl
c     Estimate the potential vorticity using Ertel's theorem
C     PS: The piece of code below was modified from the routine
c         VORTZE in code POM97BR.f
C     DENSITY ADVECTION FLUXES
      DO 530 K=1,KBM1
       DO 530 I=2,IM
c        XFLUX(I,K)=.5D0*(rho(I,K)+rho(I-1,K))
        XFLUX(I,K)=.5D0*(sigtheta(I,K)+sigtheta(I-1,K))
 530  continue
C
C     DENSITY VERTICAL ADVECTION
      DO 505 I=2,IMM1
c 505    drhods(I,1)=-.5*(rho(I,1)+rho(I,2))/DZ(1)
 505    drhods(I,1)=-.5*(sigtheta(I,1)+sigtheta(I,2))/DZ(1)
      DO 520 K=2,KBM1
       DO 520 I=2,IMM1
c 520    drhods(I,K)=.5*((rho(I,K-1)+rho(I,K))
c     1             -(rho(I,K)+rho(I,K+1)))/DZ(K)
 520    drhods(I,K)=.5*((sigtheta(I,K-1)+sigtheta(I,K))
     1             -(sigtheta(I,K)+sigtheta(I,K+1)))/DZ(K)
C     ADD NET HORIZONTAL FLUXES
      DO 120 K=1,KBM1
       DO 120 I=2,IMM1
        drhodx(i,k)=(XFLUX(I+1,K)-XFLUX(I,K))/ART(I)
 120  CONTINUE
c     VERTICAL DERIVATIVES OF HORIZONTAL VELOCITIES
      DO K=1,KBM1
       DO I=1,IMM1
        aux(i,k)=(UB(I,K)+UB(I+1,K))/2.
       enddo
	enddo
      DO I=2,IMM1
        duds(I,1)=-.5*(aux(I,1)+aux(I,2))/DZ(1)
      enddo
      DO K=2,KBM1
       DO I=2,IMM1
        duds(I,K)=.5*((aux(I,K-1)+aux(I,K))
     1             -(aux(I,K)+aux(I,K+1)))/DZ(K)
       enddo
      enddo

      DO K=1,KBM1
       DO I=1,IMM1
        aux(i,k)=VB(I,K)
       enddo
      enddo
      DO I=2,IMM1
        dvds(I,1)=-.5*(aux(I,1)+aux(I,2))/DZ(1)
      enddo
      DO K=2,KBM1
       DO I=2,IMM1
        dvds(I,K)=.5*((aux(I,K-1)+aux(I,K))
     1             -(aux(I,K)+aux(I,K+1)))/DZ(K)
       enddo
      enddo
c
C     Potential vorticity (Cushman-Roisin pag.174-eq.12-19)
c     converted from z-coordinate to sigma-coordinate
      DO K=1,KBM1
       DO I=2,IMM1
        potvort(I,K)=4.*cor4(i)*drhods(i,k)+
c     0            4.*cor4(i)*(-BV2(i,k)/((RHO(i,K-1)+RHO(i,K))/2))/grav+
     1            drhods(i,k)*rvortbrc(i,k)-
	2            dvds(i,k)*drhodx(i,k)
	  potvort(i,k)=potvort(i,k)/rho(i,k)
       enddo
      enddo
c
c     Calculate the x-derivative of potential vorticity
      do k=1,kbm1
       DO I=2,IM
       dpvortdx(I,k)=(potvort(I,k)-potvort(I-1,k))/(0.5*(DX(I)+DX(I-1)))
       ENDDO
	enddo
c
c     Substitute the values at sigma=0 and sigma=-1 from previous values
      do i=2,IMM1
	   bv2(i,1)=bv2(i,2)
	   bv2(i,kb)=bv2(i,kbm1)
	   potvort(i,1)=potvort(i,2)
	   potvort(i,kb)=potvort(i,kbm1)
	   dpvortdx(i,1)=dpvortdx(i,2)
	   dpvortdx(i,kb)=dpvortdx(i,kbm1)
	enddo
c
c      do k=1,kb
c	   write(6,'(8g15.6)') H(60)*ZZ(k),
c     1        4.*cor4(60)*drhods(60,k),drhods(60,k)*rvortbrc(60,k),
c     2         dvds(60,k)*drhodx(60,k),drhods(60,k),rvortbrc(60,k),
c     3        dvds(60,k),rho(i,k)
c      enddo
c
c
c     Re-convert the RHO array into its dimensions in POM
      do i=1,IM
	  do k=1,kb
	    rho(i,k)=(rho(i,k)-1025.D0)/1000.D0
	  enddo
	enddo
      return
	end
C#####################################################################################
C
      SUBROUTINE TERMS(uvdif,vvdif,drhox,x)
C
C     Routine to estimate the terms of the momentum equations
c     and write to the file TERMS.DAT. It was used the same
c     equations two-dimensional equations presented in Allen,J.S.
c     Newberger,P.A. and Federiuk,J.,1995,"Upwelling Circulation
c     on the Oregon Continental Shelf-Part I",JPO,vol.25,pp.1843-
c     1866.
c
c     Terms: loct= local acceleration term
c            hadv= horizontal advective term
c            vadv= vertical advective term
c            advt= non-linear advective term (hadv+vadv)
c            corl= coriolis term
c            barp= barotropic pressure gradient term
c            barc= baroclinic pressure gradient term
c            vdif= vertical diffusion term
c            hdif= horizontal diffusion term
c     Other variables: NI= Vector with horizontal positions I
c                          along the grid to be saved
c                      NIMAX= Maximun number of I positions
c                             to save
c
c     PS: All the above terms have dimensions of velocity (m/s),
c         because the original flux terms for the 2D model (m3/s2)
c         were multiplied by a factor (fac) with dimensions (s/m2).
c
c      Jose A. Lima 20/05/96
c
      include 'comblk2d.h'
c
      dimension drhox(im,kb),x(im)
      DATA PI/3.1416D0/,SMALL/1.D-10/,BETA/2.111D-11/
C
      DIMENSION XF1(IM,KB),XF2(IM,KB),AUF(IM,KB),CURV4(IM,KB),
     1          NI(IM),uvdif(IM,KB),vvdif(IM,KB)
      EQUIVALENCE (VH,CURV4)
      REAL*8 locl,hadv
C
      DTI2=DTI*2
      DO 60 K=1,KBM1
      DO 60 I=1,IM
  60     CURV4(I,K)=COR4(I)
c
c     Positions along the horizontal grid to calculate the
c     terms
      NIMAX=1
      NI(1)=57
c      NI(2)=90
c
c     Loop in the horizontal
      do j=1,NIMAX
c        Estimating the terms for the U-momentum equation
         do i=NI(j)-1,NI(j)
C           U-HORIZONTAL ADVECTION
            DO 100 K=1,KBM1
 100           XF1(I,K)=.125D0*((DT(I+1)+DT(I))*
     1                   U(I+1,K)+(DT(I)+DT(I-1))
     2                   *U(I,K))*(U(I+1,K)+U(I,K))
C           U-VERT. ADVECTION
            AUF(I,1)=0.D0
            AUF(I,KB)=0.D0
            DO 140 K=2,KBM1
 140           AUF(I,K)=.25D0*(W(I,K)+W(I-1,K))*(U(I,K)+U(I,K-1))
            DO 145 K=1,KBM1
 145           AUF(I,K)=DZR(K)*(AUF(I,K)-AUF(I,K+1))*ARU(I)
C           U-HORIZONTAL DIFFUSION
            DO 700 K=1,KBM1
  700          XF2(I,K)=-DT(I)*AAM(I,K)*2.D0*
     1                  (UB(I+1,K)-UB(I,K))/DX(I)
         enddo
         i=NI(j)
c        Multiply by fac to convert terms from m3/s2 to m/s
         fac=DTI2/(0.5*(H(I)+ETF(I)+H(I-1)+ETF(I-1))*ARU(I))
C
C        Loop in the vertical to print the terms
         DO  K=1,KBM1
c            U-horizontal advection
             hadv=(XF1(i,k)-XF1(i-1,k))*fac
c            U-vertical advection
             vadv=AUF(i,k)*fac
c            U-total advection
             advt=hadv+vadv
c            U-horizontal diffusion
             hdif=(XF2(i,k)-XF2(i-1,k))*fac
c            U-vertical diffusion
             vdif=-uvdif(i,k)*fac
c            U-coriolis term
             corl=(-ARU(I)*(CURV4(I,K)*DT(I)*2.*V(I,K)
     1             +CURV4(I-1,K)*DT(I-1)*2.*V(I-1,K)))*fac
c            U-barotropic pressure gradient term
             barp=(GRAV*.25D0*(DT(I)+DT(I-1))
     1        *(EGF(I)-EGF(I-1)+EGB(I)-EGB(I-1)))*fac
c            U-baroclinic pressure gradient term
             barc= DRHOX(I,K)*fac
c            U-local term from UF and UB
             locl=(0.5*(H(I)+ETF(I)+H(I-1)+ETF(I-1))*ARU(I)*UF(I,K)
     1       -0.5*(H(I)+ETB(I)+H(I-1)+ETB(I-1))*ARU(I)*UB(I,K))/DTI2
             locl=locl*fac
c            Residual sum of all terms (must be a very small number
c            such as D-15)
             resd=locl+advt+hdif+vdif+corl+barp+barc
c            test to check if the uf is all right: test should be
c            equal to uf(i,k)
c             test=( 0.5*(H(I)+ETB(I)+H(I-1)+ETB(I-1))*ub(i,k)*ARU(I)
c     1             - DTI2*(advt+hdif+vdif+corl+barp+barc)/fac )/
c     2              (0.5*(H(I)+ETF(I)+H(I-1)+ETF(I-1))*ARU(I))
c            Save to file TERMU.DAT
             write(9,10) TIME,(X(I)+X(I+1))/2,H(i)*ZZ(k),locl,hadv,
     1       vadv,advt,hdif,vdif,corl,barp,barc,resd
         enddo
c
c        Estimating the terms for the V-momentum equation
         do i=NI(j),NI(j)+1
C           V-HORIZONTAL ADVECTION
            DO 101 K=1,KBM1
 101           XF1(I,K)=.25D0*((DT(I)+DT(I-1))*U(I,K))
     2                        *(V(I,K)+V(I-1,K))
C           V-VERT. ADVECTION
            AUF(I,1)=0.
            AUF(I,KB)=0.D0
            DO 361 K=2,KBM1
 361           AUF(I,K)=.5D0*W(I,K)*(V(I,K)+V(I,K-1))
            DO 401 K=1,KBM1
 401           AUF(I,K)=DZR(K)*(AUF(I,K)-AUF(I,K+1))*ARV(I)
C           V-HORIZONTAL DIFFUSION
            DO  701 K=1,KBM1
               DTAAM=(DT(I)+DT(I-1))
     1               *(AAM(I,K)+AAM(I-1,K))
               XF2(I,K)=-.5D0*DTAAM*(VB(I,K)-VB(I-1,K))
     1         /(DX(I)+DX(I-1))
  701 CONTINUE
         enddo
         i=NI(j)
c        Multiply by fac to convert terms from m3/s2 to m/s
         fac=DTI2/((H(I)+ETF(I))*ARV(I))
C
C        Loop in the vertical to print the terms
         DO  K=1,KBM1
c            V-horizontal advection
             hadv=(XF1(i+1,k)-XF1(i,k))*fac
c            V-vertical advection
             vadv=(AUF(i,k))*fac
c            V-total advection
             advt=hadv+vadv
c            V-horizontal diffusion
             hdif=(XF2(i+1,k)-XF2(i,k))*fac
c            V-vertical diffusion
             vdif=-vvdif(i,k)*fac
c            V-coriolis term
             corl=(ARV(I)*(CURV4(I,K)*DT(I)*(U(I+1,K)+U(I,K))
     1             +CURV4(I,K)*DT(I)*(U(I+1,K)+U(I,K))))*fac
c            Since the model is two-dimensional, the pressure
c            gradient terms in the alongshore direction y can
c            not be estimated using y derivatives. However, it
c            can be assigned: Mellor(1986) divides the alongshore
c            pressure gradient in two terms (the elevation or
c            barotropic gradient, equivalent to barp, and the
c            density or baroclinic gradient, equivalent to barc).
c            Allen(1995) and Patrick(1996) use only one term for
c            the pressure gradient PGRAD, assigned as a constant
c            to the model. For the NSW coast, Patrick uses:
c            PGRAD=-0.5D-7. If you want to use, check if it the
c            expression is dimensionally correct (m3/s2)
c            pgrd=(GRAV*ARV(I)*DT(I)*PGRAD)*fac
             pgrd=0.0
c            V-local term from VF and VB
             locl=( (H(I)+ETF(I))*ARV(I)*VF(I,K)
     1            -(H(I)+ETB(I))*ARV(I)*VB(I,K))/DTI2
             locl=locl*fac
c            Residual sum of all terms (must be a very small number
c            such as D-15)
             resd=locl+advt+hdif+vdif+corl+pgrd
c            test to check if the vf is all right: test should be
c            equal to vf(i,k)
c             test=( (H(I)+ETB(I))*vb(i,k)*ARV(I)
c     1             - DTI2*(advt+hdif+vdif+corl+pgrd)/fac )/
c     2              ((H(I)+ETF(I))*ARU(I))
c            Save to file TERMV.DAT
             write(10,20) TIME,(X(I)+X(I+1))/2,H(i)*ZZ(k),locl,hadv,
     1       vadv,advt,hdif,vdif,corl,pgrd,resd
         enddo
      enddo
 10   FORMAT(F8.3,F8.0,F10.3,10g12.4)
 20   FORMAT(F8.3,F8.0,F10.3,9g12.4)
      RETURN
      END
c
      SUBROUTINE ADVAVE(ADVUA,ADVVA,MODE)
C
      include 'comblk2d.h'
c
      DIMENSION ADVUA(IM),ADVVA(IM)
C---------------------------------------------------------------------
C      CALCULATE U-ADVECTION & DIFFUSION
C---------------------------------------------------------------------
C
C-------- ADVECTIVE FLUXES -------------------------------------------
      DO 299 I=1,IM
 299  TPS(I)=0.D0
      DO 300 I=2,IMM1
 300  FLUXUA(I)=.125D0*((D(I+1)+D(I))*UA(I+1)
     1      +(D(I)+D(I-1))*UA(I))
     2           *(UA(I+1)+UA(I))
C----------- ADD VISCOUS FLUXES ---------------------------------
      DO  460 I=1,IMM1
 460  FLUXUA(I)=FLUXUA(I)
     1         -D(I)*2.D0*AAM2D(I)*(UAB(I+1)-UAB(I))/DX(I)
      DO  470 I=2,IM
      TPS(I)=.5*(D(I)+D(I-1))*(AAM2D(I)+AAM2D(I-1))
     4     *(VAB(I)-VAB(I-1))
     5        /(DX(I)+DX(I-1))
 470  CONTINUE
C----------------------------------------------------------------
      DO  480 I=2,IM
 480  ADVUA(I)=FLUXUA(I)-FLUXUA(I-1)
C----------------------------------------------------------------
C       CALCULATE V-ADVECTION & DIFFUSION
C----------------------------------------------------------------
      DO 481 I=1,IM
 481  ADVVA(I)=0.0D0
C---------ADVECTIVE FLUXES ----------------------------
      DO 700 I=2,IM
 700  FLUXUA(I)=.25D0*((D(I)+D(I-1))*UA(I))
     2          *(VA(I-1)+VA(I))
C------- ADD VISCOUS FLUXES -----------------------------------
      DO  870 I=1,IM
 870  FLUXUA(I)=FLUXUA(I)-TPS(I)
C---------------------------------------------------------------
      DO  880 I=1,IMM1
 880  ADVVA(I)=FLUXUA(I+1)-FLUXUA(I)
C---------------------------------------------------------------
C     COMPUTE BOTTOM FRICTION IF RUN IS EXTERNAL MODE ONLY
C---------------------------------------------------------------
      IF(MODE.NE.2) GO TO 5000
      DO 100 I=2,IM
      WUBOT(I)=-0.5D0*(CBC(I)+CBC(I-1))
     1     *SQRT(UAB(I)**2+(.5D0*(VAB(I)
     2     +VAB(I-1)))**2)*UAB(I)
  100 CONTINUE
      DO 102 I=1,IMM1
      WVBOT(I)=-1.D0*(CBC(I))
     1     *SQRT((.5D0*(UAB(I)+UAB(I+1)))**2
     2     +VAB(I)**2)*VAB(I)
  102 CONTINUE
 5000 CONTINUE
      RETURN
      END
*DECK ADVQ
      SUBROUTINE ADVQ(QB,Q,DTI2,QF)
C
      include 'comblk2d.h'
c
      DIMENSION QB(IM,KB),Q(IM,KB),QF(IM,KB)
      DIMENSION XFLUX(IM,KB),YFLUX(IM,KB)
      EQUIVALENCE (XFLUX,A),(YFLUX,C)
C
C******* HORIZONTAL ADVECTION ************************************
      DO 110 K=2,KBM1
      DO 110 I=2,IM
 110  XFLUX(I,K)=.125D0*(Q(I,K)+Q(I-1,K))
     1     *(DT(I)+DT(I-1))*(U(I,K)+U(I,K-1))
C******* HORIZONTAL DIFFUSION ************************************
      DO 315 K=2,KBM1
      DO 315 I=2,IM
      XFLUX(I,K)=XFLUX(I,K)
     1          -.5D0*(AAM(I,K)+AAM(I-1,K))*(H(I)+H(I-1))
     2           *(QB(I,K)-QB(I-1,K))*DUM(I)
     3           /(DX(I)+DX(I-1))
  315 CONTINUE
C****** VERTICAL ADVECTION; ADD FLUX TERMS ;THEN STEP FORWARD IN TIME **
      DO 230 K=2,KBM1
      DO 230 I=1,IMM1
      QF(I,K)=(W(I,K-1)*Q(I,K-1)-W(I,K+1)*Q(I,K+1))
     1                     /(DZ(K)+DZ(K-1))*ART(I)
     2                      +XFLUX(I+1,K)-XFLUX(I,K)
 230  QF(I,K)=((H(I)+ETB(I))*ART(I)*QB(I,K)-DTI2*QF(I,K))
     1             /((H(I)+ETF(I))*ART(I))
      RETURN
      END
*DECK ADVT
      SUBROUTINE ADVT(FB,F,FMEAN,DTI2,FF)
C
C     THIS SUBROUTINE INTEGRATES CONSERVATIVE CONSTITUENT EQUATIONS
C        PASSIVE AND ACTIVE......
C
c     PS: Patrick installed a relaxing term extending from the
c         middle of the domain (IM/2) to its end.
C
      include 'comblk2d.h'
c
      DIMENSION FB(IM,KB),F(IM,KB),FF(IM,KB),FMEAN(IM,KB)
      DIMENSION XFLUX(IM,KB),YFLUX(IM,KB)
      EQUIVALENCE (XFLUX,A),(YFLUX,C)
      DO 529 I=1,IM
      F(I,KB)=F(I,KBM1)
 529  FB(I,KB)=FB(I,KBM1)
C******* DO ADVECTION FLUXES **************************************
      DO 530 K=1,KBM1
      DO 530 I=2,IM
 530  XFLUX(I,K)=.25D0*((DT(I)+DT(I-1))
     2            *(F(I,K)+F(I-1,K))*U(I,K))
C******  ADD DIFFUSIVE FLUXES *************************************
c      IF(2..GT.1.) GOTO 300
      DO 99 K=1,KB
      DO 99 I=1,IM
  99  FB(I,K)=FB(I,K)-FMEAN(I,K)
      K=1
      XFLUX(I,K)=XFLUX(I,K)
     1   -.5D0*(AAM(I,K)+AAM(I-1,K))*(
     2   (H(I)+H(I-1))*(FB(I,K)-FB(I-1,K))
     3    )*DUM(I)/(TPRNU*(DX(I)+DX(I-1)))
      DO 100 K=2,KB-2
      DO 100 I=2,IM
      XFLUX(I,K)=XFLUX(I,K)
     1   -.5D0*(AAM(I,K)+AAM(I-1,K))*(
     2   (H(I)+H(I-1))*(FB(I,K)-FB(I-1,K))
c    3   -ZZ(K)*(FB(I,K-1)-FB(I,K+1)+FB(I-1,K-1)-FB(I-1,K+1))
c    4    /(ZZ(K-1)-ZZ(K+1))*(H(I)-H(I-1))
     5    )*DUM(I)/(TPRNU*(DX(I)+DX(I-1)))
  100 CONTINUE
      K=KB-1
      DO 101 I=2,IM
      XFLUX(I,K)=XFLUX(I,K)
     1   -.5D0*(AAM(I,K)+AAM(I-1,K))*(
     2   (H(I)+H(I-1))*(FB(I,K)-FB(I-1,K))
c    3   -ZZ(K)*(FB(I,K-1)-FB(I,K)+FB(I-1,K-1)-FB(I-1,K))
c    4    /(ZZ(K-1)-ZZ(K))*(H(I)-H(I-1))
     5    )*DUM(I)/(TPRNU*(DX(I)+DX(I-1)))
  101 CONTINUE
      DO 102 K=1,KB
      DO 102 I=1,IM
 102  FB(I,K)=FB(I,K)+FMEAN(I,K)
c 300  CONTINUE
C****** DO VERTICAL ADVECTION *************************************
      DO 505 I=1,IM
 505  FF(I,1)=-.5D0*DZR(1)*(F(I,1)+F(I,2))*W(I,2)*ART(I)
      DO 520 K=2,KBM1
      DO 520 I=1,IM
 520  FF(I,K)=.5D0*DZR(K)*((F(I,K-1)+F(I,K))*W(I,K)
     1                  -(F(I,K)+F(I,K+1))*W(I,K+1))*ART(I)
C****** ADD NET HORIZONTAL FLUXES; THEN STEP FORWARD IN TIME **********
      DO 120 K=1,KBM1
      DO 120 I=1,IM
      FF(I,K)=FF(I,K)
     1              +XFLUX(I+1,K)-XFLUX(I,K)
 120  FF(I,K)=(FB(I,K)*(H(I)+ETB(I))*ART(I)-DTI2*FF(I,K))
     1                    /((H(I)+ETF(I))*DX(I))
cpm  Relaxation term for damping at west boundary (add P.M.)
c      TAU0 = 1./(6.*3600.)
c      DIV = 2000.
c      do k=2,KB
c      do i=IM/2,IM
c        TAU = TAU0 * DIV**(-2.*dfloat(IM-i)/dfloat(IM))
c	   write(6,'(2i5,3g12.4)') k,i,tau,FF(i,k),
c     1               DTI*2*TAU*(F(i,k)-FMEAN(i,k))
c        FF(i,k) = FF(i,k) - DTI*2*TAU*(F(i,k)-FMEAN(i,k))
c      enddo
c      enddo
c     TAUSURF=1./(10.*86400.)
c     do i=1,IM
c        FF(i,1) = FF(i,1) - DTI2*TAUSURF*(F(i,1)-FMEAN(i,1))
c     enddo
      RETURN
      END
*DECK ADVU
      SUBROUTINE ADVU(DRHOX,ADVUA,DTI2)
C
      include 'comblk2d.h'
c
      DIMENSION DRHOX(IM,KB),XFLUX(IM,KB),YFLUX(IM,KB),
     1           ADVUA(IM),CURV4(IM,KB)
      EQUIVALENCE (A,XFLUX),(C,YFLUX),(VH,CURV4)
C
      DO 59 K=1,KB
      DO 59 I=1,IM
      UF(I,K)=0.D0
  59  XFLUX(I,K)=0.D0
      DO 60 K=1,KBM1
      DO 60 I=1,IM
  60  CURV4(I,K)=COR4(I)
      DO 61 I=1,IM
  61  CURV42D(I)=0.D0
      DO 70 K=1,KBM1
      DO 70 I=1,IM
   70 CURV42D(I)=CURV42D(I)+CURV4(I,K)*DZ(K)
C******** HORIZONTAL ADVECTION ************************************
      DO 100 K=1,KBM1
      DO 100 I=2,IMM1
 100  XFLUX(I,K)=.125D0*((DT(I+1)+DT(I))*
     1              U(I+1,K)+(DT(I)+DT(I-1))
     2             *U(I,K))*(U(I+1,K)+U(I,K))
C******  HORIZONTAL DIFFUSION *************************************
      DO  700 K=1,KBM1
      DO  700 I=2,IMM1
      XFLUX(I,K)=XFLUX(I,K)
     1        -DT(I)*AAM(I,K)*2.D0*(UB(I+1,K)-UB(I,K))/DX(I)
  700  CONTINUE
C******** DO VERT. ADVECTION; ADD HORIZ. ADVECTION *******
CSB**
      DO 139 I=1,IM
      UF(I,1)=0.D0
 139  UF(I,KB)=0.D0
      DO 140 K=2,KBM1
      DO 140 I=2,IM
 140  UF(I,K)=.25D0*(W(I,K)+W(I-1,K))*(U(I,K)+U(I,K-1))
      DO 145 K=1,KBM1
      DO 145 I=1,IM
 145  UF(I,K)=DZR(K)*(UF(I,K)-UF(I,K+1))*ARU(I)
      DO 146 K=1,KBM1
      DO 146 I=2,IM
 146  UF(I,K)=UF(I,K)
     1            +XFLUX(I,K)-XFLUX(I-1,K)
C--------------------------------------------------------------------
C        SAVE HORIZONTAL ADVECT. AND DIFF. FLUXES FOR EXT. MODE
C--------------------------------------------------------------------
      DO 799 I=1,IM
 799  ADVUA(I)=0.D0
      DO 800 K=1,KBM1
      DO 800 I=1,IM
  800 ADVUA(I)=ADVUA(I)+DZ(K)*UF(I,K)
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C********* -FVD + GDEG/DX + BAROCIMNIC TERM **********************
      DO 150 K=1,KBM1
      DO 150 I=2,IM
 150  UF(I,K)=UF(I,K)
     1   -ARU(I)*(CURV4(I,K)*DT(I)*2.*V(I,K)
     2             +CURV4(I-1,K)*DT(I-1)*2.*V(I-1,K))
     3        +GRAV*.25D0*(DT(I)+DT(I-1))
     4        *(EGF(I)-EGF(I-1)+EGB(I)-EGB(I-1))
     6        +DRHOX(I,K)
C******* STEP FORWARD IN TIME ***********************************
      DO 190 K=1,KBM1
      DO 190 I=2,IM
 190  UF(I,K)=
     1      ((H(I)+ETB(I)+H(I-1)+ETB(I-1))*ARU(I)*UB(I,K)
     2         -2.D0*DTI2*UF(I,K))
     3     /((H(I)+ETF(I)+H(I-1)+ETF(I-1))*ARU(I))
      RETURN
      END
*DECK ADVV
      SUBROUTINE ADVV(ADVVA,DTI2)
C
      include 'comblk2d.h'
c
      DIMENSION XFLUX(IM,KB),YFLUX(IM,KB),
     1                ADVVA(IM),CURV4(IM,KB)
      EQUIVALENCE (A,XFLUX),(C,YFLUX),(VH,CURV4)
C
      DO 299 K=1,KB
      DO 299 I=1,IM
      VF(I,K)=0.D0
 299  XFLUX(I,K)=0.D0
C
C********** HORIZONTAL ADVECTION *********************************
      DO 300 K=1,KBM1
      DO 300 I=2,IM
 300  XFLUX(I,K)=.25D0*((DT(I)+DT(I-1))*U(I,K))
     2                        *(V(I,K)+V(I-1,K))
C******* HORIZONTAL DIFFUSION *************************************
      DO  700 K=1,KBM1
      DO  700 I=2,IM
      DTAAM=(DT(I)+DT(I-1))
     1           *(AAM(I,K)+AAM(I-1,K))
      XFLUX(I,K)=XFLUX(I,K)
     1     -.5D0*DTAAM*
     3               (VB(I,K)-VB(I-1,K))
     4         /(DX(I)+DX(I-1))
  700 CONTINUE
C
C********** DO VERT. ADVECTION; ADD HORIZ. ADVECTION ************
      DO 359 I=1,IM
      VF(I,1)=0.
 359  VF(I,KB)=0.D0
      DO 360 K=2,KBM1
      DO 360 I=1,IM
 360  VF(I,K)=.5D0*W(I,K)*(V(I,K)+V(I,K-1))
      DO 400 K=1,KBM1
      DO 400 I=1,IMM1
 400  VF(I,K)=DZR(K)*(VF(I,K)-VF(I,K+1))*ARV(I)
     1            +XFLUX(I+1,K)-XFLUX(I,K)
C--------------------------------------------------------------------
C        SAVE HORIZONTAL ADVECT. AND DIFF. FLUXES FOR EXT. MODE
C--------------------------------------------------------------------
      DO 799 I=1,IM
 799  ADVVA(I)=0.D0
      DO 800 K=1,KBM1
      DO 800 I=1,IM
  800 ADVVA(I)=ADVVA(I)+DZ(K)*VF(I,K)
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C********* +FUD + GDEG/DX + BAROCIMNIC TERM *************************
      PGRAD=-.5D-7  !pm dEG/dy
      DO 340 K=1,KBM1
      DO 340 I=1,IMM1
 340  VF(I,K)=VF(I,K)
     1   +ARV(I)*(CURV4(I,K)*DT(I)*(U(I+1,K)+U(I,K))
     2             +CURV4(I,K)*DT(I)*(U(I+1,K)+U(I,K)))
c    4    +GRAV*ARV(I)*DT(I)*PGRAD
c     SUM1=0.
c     SUM2=0.
c     DO I=2,IM
c     SUM1=SUM1+VF(I,1)/(GRAV*ARV(I)*DT(I))*DX(I)
c     SUM2=SUM2+DX(I)
c     ENDDO
c     PGRAD=-SUM1/SUM2
c     IF(PGRAD.GT.0.)PGRAD=0.
c     PGRAD=-VF(10,1)/(GRAV*ARV(10)*DT(10))
C******* STEP FORWARD IN TIME ***************************************
      DO 390 K=1,KBM1
      DO 390 I=1,IM
  390 VF(I,K)=
     1     ((H(I)+ETB(I))*ARV(I)*VB(I,K)
     2          -DTI2*VF(I,K))
     3    /((H(I)+ETF(I))*ARV(I))
      RETURN
      END
*DECK BAROPG
      SUBROUTINE BAROPG(DRHOX,TRNU)
C
      include 'comblk2d.h'
c
      DIMENSION DRHOX(IM,KB),TRNU(IM),D1(IM),D2(IM)
C
C               X COMPONENT OF BAROCLINIC PRESSURE GRADIENT
C
C     RHO=RHO-RMEAN
      do i=1,IM
        DRHOX(i,1)=0.
      enddo
      DO 300 I=2,IM
 300  DRHOX(I,1)=.5*GRAV*(0.-ZZ(1))*(DT(I)+DT(I-1))
     2     *(RHO(I,1)-RHO(I-1,1))
     3   -.5*GRAV*ZZ(1)*(DT(I)-DT(I-1))
     4    *(RHO(I,2)+RHO(I-1,2)-RHO(I,1)-RHO(I-1,1))
     5   *(0.-ZZ(1))/(ZZ(2)-ZZ(1))
      DO 315 K=2,KBM1
      DO 310 I=2,IM
      D1(I)= +GRAV*.25*(ZZ(K-1)-ZZ(K))*(DT(I)+DT(I-1))
     2      *(RHO(I,K)-RHO(I-1,K)+RHO(I,K-1)-RHO(I-1,K-1))
      D2(I)=+GRAV*.25*(ZZ(K-1)+ZZ(K))*(DT(I)-DT(I-1))
     4      *(RHO(I,K)+RHO(I-1,K)-RHO(I,K-1)-RHO(I-1,K-1))
 310  DRHOX(I,K)=DRHOX(I,K-1)+D1(I)+D2(I)
c     WRITE(6,'('' K,ZZ,RHO ='',I5,9E11.2)')
c    1    K,ZZ(K),(RHO(I,K)     ,I=10,13)
c     WRITE(6,'('' K,ZZ,D1,D2 ='',I5,9E11.2)')
c    1    K,ZZ(K),(D1(I),D2(I),I=10,13)
 315  CONTINUE
      DO 320 K=1,KBM1
      DO 320 I=2,IM
 320  DRHOX(I,K)=DRHOX(I,K)*.5*(DT(I)+DT(I-1))*DUM(I)
     1           *RAMP
c     CALL PRXZ('  DRHOX     ',0.,DRHOX,IM,1,KB,0.,DT,ZZ)
C     RHO=RHO+RMEAN
C
C         VERTICALLY INTEGRATE PRESSURE GRADIENT
C
      DO 569 I=1,IM
 569  TRNU(I)=0.0D0
C
      DO 570 K=1,KBM1
      DO 570 I=1,IM
 570  TRNU(I)=TRNU(I)+DRHOX(I,K)*DZ(K)
C
      RETURN
      END
*DECK BCOND
      SUBROUTINE BCOND(IDX)
C
cJL   This routine was modified to open the seaward boundary
c     conditions as described in the README on the beginning
c     of the model
C
      include 'comblk2d.h'
C
      REAL*8 CE(KB)
      DATA PI/3.14167D0/,GEE/9.807D0/,XPRD/8.D0/
C
c
cJL
C     Including gradient and radiative boundary conditions
C
      GOTO (10,20,30,40,50,60), IDX

 10   CONTINUE

C-----------------------------------------------------------------------
C                   EXTERNAL ELEV. B.C.'S
C-----------------------------------------------------------------------

c#ifdef sponge
       DO 3001 I=IM-12,IMM1
       ELF(I)=ELF(I)-2.0*DTE*.29D-6*ELB(I)*1.6**(I-IM+12)
 3001  CONTINUE
c#endif

C                  ***** AT SEAWARD B'DRY I=IM ********

c Apply zero-gradient condition:
         ELF(IM)= ELF(IMM1)

C
cJL                ****** At landward B'dry I=1 *******
c Apply zero-gradient as part of experiment 34
c      ELF(1)=ELF(2)

c Apply mask:

      DO 111 I=1,IM
 111  ELF(I)=ELF(I)*FSM(I)
      RETURN
C
 20   CONTINUE

C-----------------------------------------------------------------------
C                   EXTERNAL VEL B.C.'S
C-----------------------------------------------------------------------

c#ifdef sponge

c COMMENTS:
c   The sponge is set up as a correction to the forward velocity UAF,
c via dU/dt = -kU, discretised as an Euler forward step (from UAB to UAF).
c The time scale of the damping at the weak end of the sponge is about
c 1/(40 days) = .29D-6, and the factor 1.5 represents the rate of decreasing
c damping timescale, down to around 5.5 hours at the boundary.
c NOTE: You have to damp V at the same rate as U (and ETA), remembering
c that V is offset in the Y direction. Failure to do this will cause POM
c to start building spurious velocities in an effort to satisfy continuity
c within the sponge. This is done in a pretty rough way below by just changing
c the offset on the V loop.

cc Do additional sponge on UA at TAS end.
       DO 2001 I=IM-11,IMM1
       UAF(I)=UAF(I)-2.0*DTE*.29D-6*UAB(I)*1.6**(I-IM+11)
 2001  CONTINUE
c
cc Do additional sponge on VA at TAS end.
       DO 2004 I=IM-12,IMM1
       VAF(I)=VAF(I)-2.0*DTE*.29D-6*VAB(I)*1.6**(I-IM+12)
 2004  CONTINUE
c#endif

C                  ***** AT SEAWARD B'DRY I=IM ********

c Apply zero-gradient condition:
         UAF(IM)= UAF(IMM1)
         VAF(IM)= VAF(IMM1)
c
cJL        *** ***** At Landward B'dry *************
C Apply zero-gradient as a part of experiments 34 and 35
c      VAF(1)=VAF(2)

c Apply masks:
      DO 133 I=1,IM
      UAF(I)=UAF(I)*DUM(I)
 133  VAF(I)=VAF(I)*DVM(I)
      RETURN

 30   CONTINUE

C-----------------------------------------------------------------------
C                   INTERNAL VEL B.C.'S
C-----------------------------------------------------------------------

c           ********** U *************

c#ifdef sponge
cc Do additional sponge on U:
       DO 2011 I=IM-11,IMM1
       DO 2011 K=1,KBM1
       UF(I,K)=UF(I,K)-2.0*DTI*.29D-6*UB(I,K)*1.6**(I-IM+11)
 2011  CONTINUE
c
cc Do additional sponge on V:
       DO 2014 I=IM-12,IMM1
       DO 2014 K=1,KBM1
       VF(I,K)=VF(I,K)-2.0*DTI*.29D-6*VB(I,K)*1.6**(I-IM+12)
 2014  CONTINUE
c#endif

C                  ***** AT SEAWARD B'DRY I=IM ********

c Apply Orlanski radiation condition to the baroclinic part:

       DO 453 K=1,KBM1

c Compute the U-UA (baroclinic) wave speed:
         CE(K)=UF(IMM1,K)+UB(IMM1,K) -2.0*U(IMM2,K)
     1          -(UAF(IMM1)+UAB(IMM1) -2.0*UA(IMM2) )
         IF ( CE(K).NE. 0.0) THEN
          CE(K)= -(UF(IMM1,K)-UB(IMM1,K)-UAF(IMM1)
     1    +UAB(IMM1))*DX(IMM1)/(DX(IM)*CE(K))
         ENDIF
         IF(CE(K).GE.1.0) CE(K)=1.0
         IF(CE(K).LE.0.0) CE(K)=0.0

c Apply radiating wave condition on U-UA:
       UF(IM,K)= UAF(IM)
     1     +(1.0-CE(K))*(UB(IM,K)-UAB(IM))/(1.0+CE(K))
     2     + 2.0*CE(K)*(U(IMM1,K)-UA(IMM1))/(1.0+CE(K))

 453   CONTINUE

       DO 553 K=1,KBM1

c Compute the V-VA (baroclinic) wave speed:
         CE(K)=VF(IMM1,K)+VB(IMM1,K)-2.0*V(IMM2,K)
     1      -(VAF(IMM1)+VAB(IMM1)-2.0*VA(IMM2))
         IF ( CE(K).NE.0.0) THEN
          CE(K)=-(VF(IMM1,K)-VB(IMM1,K)-VAF(IMM1)
     1     +VAB(IMM1) ) *DX(IMM1) /(DX(IM)*CE(K))
         ENDIF
         IF(CE(K).GE.1.0) CE(K)=1.0
         IF(CE(K).LE.0.0) CE(K)=0.0

c Apply radiating wave condition on V-VA:
         VF(IM,K)= VAF(IM) +
     1         (1.0-CE(K))*(VB(IM,K)-VAB(IM))/(1.0+CE(K))
     2      + 2.0*CE(K)*(V(IMM1,K)-VA(IMM1))/(1.0+CE(K))
 553   CONTINUE

c
cJL         ********* Landward B'dry *********
C Apply zero-gradient as part of experiments 34 and 35
c      do k=1,kbm1
c         VF(1,k)=VF(2,k)
c      enddo

c Apply masks:
      DO 160 K=1,KBM1
      DO 160 I=1,IM
      UF(I,K)=UF(I,K)*DUM(I)
      VF(I,K)=VF(I,K)*DVM(I)
  160 CONTINUE

C  THE  FOLLOWING  ONLY  PREVENTS  LARGE  NUMBERS IN  PRINTOUTS
      WVBOT(IM)=WVBOT(IMM1)
      WUBOT(IM)=WVBOT(IMM1)
      RETURN

 40   CONTINUE

C-----------------------------------------------------------------------
C                   TEMP & SAL B.C.'S
C-----------------------------------------------------------------------

C                  ***** AT SEAWARD B'DRY I=IM ********

c Apply Orlanski radiation condition on T&S:
c NOTE: TF held in UF, SF held in VF to save space.

       DO 653 K=1,KBM1

c Compute the T wave speed:
         CE(K)=UF(IMM1,K)+TB(IMM1,K) -2.0*T(IMM2,K)
         IF ( CE(K).NE. 0.0) THEN
          CE(K)= -( UF(IMM1,K)-TB(IMM1,K) )*DX(IMM1)
     1            /(DX(IM)*CE(K))
         ENDIF
         IF(CE(K).GE.1.0) CE(K)=1.0
         IF(CE(K).LE.0.0) CE(K)=0.0

c Apply radiating wave condition on T:
         UF(IM,K)= (1.0-CE(K))*TB(IM,K)/(1.0+CE(K))
     1      + 2.0*CE(K)*T(IMM1,K)/(1.0+CE(K))

 653   CONTINUE

       DO 753 K=1,KBM1

c Compute the S wave speed:
         CE(K)=VF(IMM1,K)+SB(IMM1,K) -2.0*S(IMM2,K)
          IF ( CE(K).NE. 0.0) THEN
           CE(K)= -( VF(IMM1,K)-SB(IMM1,K) )*DX(IMM1)
     1            /(DX(IM)*CE(K))
          ENDIF
          IF(CE(K).GE.1.0) CE(K)=1.0
          IF(CE(K).LE.0.0) CE(K)=0.0

c Apply radiating wave condition on S :
          VF(IM,K)= (1.0-CE(K))*SB(IM,K)/(1.0+CE(K))
     1      + 2.0*CE(K)*S(IMM1,K)/(1.0+CE(K))
 753   CONTINUE


C***************************************************


c
cJL         ********* Landward B'dry *********
C Apply zero-gradient as part of experiment 34
c      do k=1,kbm1
c         UF(1,k)=UF(2,k)
c         VF(1,k)=VF(2,k)
c      enddo

c Apply masks:
      DO 240 K=1,KBM1
      DO 240 I=1,IM
      UF(I,K)=UF(I,K)*FSM(I)
      VF(I,K)=VF(I,K)*FSM(I)

 240  CONTINUE

      RETURN


 50   CONTINUE

C---------------VERTICAL VEL. B. C.'S --------------------------------
      DO 250 K=1,KBM1
      DO 250 I=1,IM
      W(I,K)=W(I,K)*FSM(I)
 250  CONTINUE
      DO 251 K=1,KB
c ?????????????????????????????????????
c Check this bc, because Simon have not used W(1,K)=0
 251  W(IM,K)=0.D0

      RETURN

 60   CONTINUE
C---------------- Q2 AND Q2L B.C.'S -----------------------------------
      DO 300 K=1,KB

         UF(IM,K)=1.D-10
         VF(IM,K)=1.D-10

c
cJL         ********* Landward B'dry *********
C Apply zero-gradient as part of experiment 34
c         UF(1,k)=UF(2,k)
c         VF(1,k)=VF(2,k)

         DO 297 I=1,IM
            UF(I,K)=UF(I,K)*FSM(I)
 297     VF(I,K)=VF(I,K)*FSM(I)

 300  CONTINUE
      RETURN
      END
c
      SUBROUTINE BCONDOLD(IDX)
C
cJL   This was the original BCOND routine from pom2d.f that
c     was being used by Patrick in the NSW model. The boundary
c     is clamped at seaward, because FSM(IM),DUM(I),DVM(IM)
c     were set to zero in the main routine.
c
      include 'comblk2d.h'
C
      DATA PI/3.14167D0/,GEE/9.807D0/
C
      GO TO (10,20,30,40,50,60), IDX
C
 10   CONTINUE
C-----------------------------------------------------------------------
C                   EXTERNAL ELEV. B.C.'S
C-----------------------------------------------------------------------
C*SB  ELF(IM)=ELF(IMM1)
C*100 CONTINUE
C*SB  DO 110 I=1,IM
C*SB  ELF(I,M)=ELF(I,MM1)
C*SB  ELF(I,1)=ELF(I,2)
C*110 CONTINUE
CSB**
      DO 111 I=1,IM
 111  ELF(I)=ELF(I)*FSM(I)
      RETURN
C
 20   CONTINUE
C-----------------------------------------------------------------------
C                   EXTERNAL VEL B.C.'S
C-----------------------------------------------------------------------
C                   ***** SEAWARD ********
C*SB  UAF(IM)=RAMP+EL(IMM1)
C*SB  UMID=UA(IM)
C*SB  VAF(IM)=VA(IM)-DTI/(DX(IM)+DX(IMM1))
C*SB 1     *(  (UMID+ABS(UMID))*(VA(IM)-VA(IMM1))
C*SB 2        +(UMID-ABS(UMID))*(0.D0 -VA(IM))  )
C*120 VAF(IM)=0.D0
C                   **** NORTH AND SOUTH ****
C--------- SPONGE B.C. ----------------------------------------------
C     DO 124 I=1,IMM1
C124  VAF(I,MM1)=RAMP*VABN(I)
C    1        *(H(I,MM1)+H(I,MM2))
C    2        /(H(I,MM1)+ELF(I,MM1)+H(I,MM2)+ELF(I,MM2))
C     SPG=MM2-4
C     DO 125 I=1,IMM1
C     ALPH2DT=.0001*2.*DTE
C     UAF(I)=(1.-ALPH2DT)*UAF(I)
C 125 VAF(I)=(1.-ALPH2DT)*VAF(I)+ALPH2DT*VAF(I,MM1)
C--------------------------------------------------------------------
C*SB  DO 124 I=1,IMM1
C*124 VAF(I,M)=RAMP*VABN(I)+COVRHN(I)*(EL(I,MM1)-ELN(I))
C*SB  DO 130 I=1,IMM1
C*SB  VAF(I,2)=RAMP*VABS(I)
C*SB 1        *(H(I,2)+H(I,1))/(H(I,2)+ELF(I,2)+H(I,1)+ELF(I,1))
C*SB  VAF(I,1)=VAF(I,2)
C*SB  VMID=.5D0*(VA(I,M)+VA(I-1,M))
C*SB  UAF(I,M)=UA(I,M)-DTI/(DY(I,M)+DY(I,MM1))
C*SB 1    *(   (VMID+ABS(VMID))*(UA(I,M)-UA(I,MM1))
C*SB 2        +(VMID-ABS(VMID))*(0.D0-UA(I,M))  )
C*SB  VMID=.5D0*(VA(I,2)+VA(I-1,2))
C*SB  UAF(I,1)=UA(I,1)-DTI/(DY(I,1)+DY(I,2))
C*SB 1    *(   (VMID+ABS(VMID))*(UA(I,1)-0.D0)
C*SB 2        +(VMID-ABS(VMID))*(UA(I,2)-UA(I,1)) )
C*130 CONTINUE
CSB**
      DO 131 I=1,IM
      UAF(I)=UAF(I)*DUM(I)
 131  VAF(I)=VAF(I)*DVM(I)
      RETURN
C
 30   CONTINUE
C-----------------------------------------------------------------------
C                   INTERNAL VEL B.C.'S
C-----------------------------------------------------------------------
C                         **** SEAWARD *******
C*SB  DO 142 K=1,KBM1
C*SB  DO 140 =2,MM1
C*SB  GA=SQRT(H(IM)/2000.D0)
C*140 UF(IM,K)
C*SB 1  =GA*(.25D0*U(IMM1,K)+.5D0*U(IMM1,K)+.25D0*U(IMM1,K))
C*SB 2   +(1.D0-GA)*(.25D0*U(IM,K)+.5D0*U(IM,K)+.25D0*U(IM,K))
C*SB  UF(IM,M,K)=0.D0
C*SB  UF(IM,1,K)=0.D0
C*SB  DO 141 =1,M
C*SB  UMID=.5D0*(U(IM,K)+U(IM,K))
C*SB  VF(IM,K)=V(IM,K)-DTI/(DX(IM)+DX(IMM1))
C*SB 1     *(  (UMID+ABS(UMID))*(V(IM,K)-V(IMM1,K))
C*SB 2        +(UMID-ABS(UMID))*(0.D0 -V(IM,K))  )
C*141 CONTINUE
C*142 CONTINUE
C                       ***** NORTH AND SOUTH ***
C*SB  DO 158 K=1,KBM1
C*SB  DO 150 I=1,IMM1
C*SB  GA=SQRT(H(I,MM1)/2000.D0)
C*SB  VF(I,K)=
C*SB 1   GA*(.25D0*V(I-1,MM1,K)+.5D0*V(I,MM1,K)+.25D0*V(I+1,MM1,K))
C*SB 2 + (1.D0-GA)
C*SB 3 *(.25D0*V(I-1,M,K)+.5D0*V(I,K)+.25D0*V(I+1,M,K))
C*150 CONTINUE
C*SB  DO 152 I=1,IM
C*SB  GA=SQRT(H(I,2)/2000.D0)
C*SB  VF(I,2,K)=GA*(.25D0*V(I-1,3,K)+.5D0*V(I,3,K)+.25D0*V(I+1,3,K))
C*SB 1    +(1.D0-GA)*(.25D0*V(I-1,2,K)+.5D0*V(I,2,K)+.25D0*V(I+1,2,K))
C*152 VF(I,K)=VF(I,2,K)
C*SB  DO 156 I=1,IMM1
C*SB  VMID=.5D0*(V(I,K)+V(I-1,M,K))
C*SB  UF(I,K)=U(I,K)-DTI/(DY(I,M)+DY(I,MM1))
C*SB 1    *(   (VMID+ABS(VMID))*(U(I,K)-U(I,MM1,K))
C*SB 2        +(VMID-ABS(VMID))*(0.D0-U(I,K))  )
C*SB  VMID=.5D0*(V(I,2,K)+V(I-1,2,K))
C*SB  UF(I,K)=U(I,K)-DTI/(DY(I,1)+DY(I,2))
C*SB 1    *(   (VMID+ABS(VMID))*(U(I,K)-0.D0)
C*SB 2        +(VMID-ABS(VMID))*(U(I,2,K)-U(I,K)) )
C*156 CONTINUE
C*158 CONTINUE
C                    **********************
      DO 160 K=1,KBM1
CSB**
      DO 160 I=1,IM
      UF(I,K)=UF(I,K)*DUM(I)
      VF(I,K)=VF(I,K)*DVM(I)
  160 CONTINUE
C  THE  FOLLOWING  ONLY  PREVENTS  LARGE  NUMBERS IN  PRINTOUTS
CSB**
      RETURN
C
 40   CONTINUE
C-----------------------------------------------------------------------
C                   TEMP & SAL B.C.'S
C-----------------------------------------------------------------------
C                           *** SEAWARD *****
C*SB  DO 220 K=1,KBM1
C*SB  DO 220 =1,M
C*SB  UF(IM,K)=T(IM,K)-DTI/(DX(IM)+DX(IMM1))
C*SB 1     *(  (U(IM,K)+ABS(U(IM,K)))*(T(IM,K)-T(IMM1,K))
C*SB 2        +(U(IM,K)-ABS(U(IM,K)))*(TBE(,K) -T(IM,K))  )
C*SB  VF(IM,K)=S(IM,K)-DTI/(DX(IM)+DX(IMM1))
C*SB 1     *(  (U(IM,K)+ABS(U(IM,K)))*(S(IM,K)-S(IMM1,K))
C*SB 2        +(U(IM,K)-ABS(U(IM,K)))*(SBE(,K) -S(IM,K))  )
C
C                        **** NORTH AND SOUTH *****
C*220 CONTINUE
C*SB  DO 230 K=1,KBM1
C*SB  DO 230 I=1,IM
C*SB  UF(I,K)=T(I,K)-DTI/(DY(I,M)+DY(I,MM1))
C*SB 1    *(   (V(I,K)+ABS(V(I,K)))*(T(I,K)-T(I,MM1,K))
C*SB 2        +(V(I,K)-ABS(V(I,K)))*(TBN(I,K)-T(I,K))  )
C*SB  VF(I,K)=S(I,K)-DTI/(DY(I,M)+DY(I,MM1))
C*SB 1    *(   (V(I,K)+ABS(V(I,K)))*(S(I,K)-S(I,MM1,K))
C*SB 2        +(V(I,K)-ABS(V(I,K)))*(SBN(I,K)-S(I,K))  )
C*SB  UF(I,K)=T(I,K)-DTI/(DY(I,1)+DY(I,2))
C*SB 1    *(   (V(I,2,K)+ABS(V(I,2,K)))*(T(I,K)-TBS(I,K))
C*SB 2        +(V(I,2,K)-ABS(V(I,2,K)))*(T(I,2,K)-T(I,K)) )
C*SB  VF(I,K)=S(I,K)-DTI/(DY(I,1)+DY(I,2))
C*SB 1    *(   (V(I,2,K)+ABS(V(I,2,K)))*(S(I,K)-SBS(I,K))
C*SB 2        +(V(I,2,K)-ABS(V(I,2,K)))*(S(I,2,K)-S(I,K)) )
C*230 CONTINUE
      DO 240 K=1,KBM1
CSB**
      DO 240 I=1,IM
      UF(I,K)=UF(I,K)*FSM(I)
      VF(I,K)=VF(I,K)*FSM(I)
 240  CONTINUE
      RETURN
C
C
 50   CONTINUE
C---------------VERTICAL VEL. B. C.'S --------------------------------
      DO 250 K=1,KBM1
CSB**
      DO 250 I=1,IM
      W(I,K)=W(I,K)*FSM(I)
 250  CONTINUE
CSB**
      DO 251 K=1,KB
       W(1,K)=0
 251  W(IM,K)=0.
CSB**
      RETURN
C
 60   CONTINUE
C---------------- Q2 AND Q2L B.C.'S -----------------------------------
      DO 297 K=1,KB
      DO 297 I=1,IM
      UF(1,K)=1.D-10
      VF(1,K)=1.D-10
      UF(IM,K)=1.D-10
      VF(IM,K)=1.D-10
      UF(I,K)=UF(I,K)*FSM(I)
      VF(I,K)=VF(I,K)*FSM(I)
 297  CONTINUE
      RETURN
      END
c
      SUBROUTINE CNVRT(ZLEV,TLEV,IM,KS,ZZ,H,T,KB)
cvc
        implicit real*8 (a-h,o-z)
	implicit integer*4 (i-n)
      PARAMETER (IIM=192,KKS=20,KKB=74)
      DIMENSION ZLEV(KS),TLEV(IM,KS)
      DIMENSION TXLEV(IIM,KKS)
      DIMENSION ZZ(KB),T(IM,KB),TX(IIM,KKB),H(IM)
      DIMENSION IND(IIM),ZZD(KKB),VLEV(KKS),TZZ(KKB)
      do i=1,im
         do k=1,kb
            TX(i,k)=0.
         enddo
         do k=1,ks
            TXLEV(i,k)=0.
         enddo
         IND(i)=0
      enddo
c      CALL PRXZ('   TLEV',0.,TLEV,IM,1,KS,0.00001,H,ZLEV)
      WRITE(6,'(1x,2I5)') KB,KS
      DO 10 I=1,IM
      DO 5 K=1,KB
    5 ZZD(K)=-H(I)*ZZ(K)
      DO 6 K=1,KS
    6 VLEV(K)=TLEV(I,K)
      CALL SPLINC(ZLEV,VLEV,KS  ,2.D30,2.D30,ZZD,TZZ,KB)
C     WRITE(6,'(1x,2F14.6)') (ZZD(K),TZZ(K),K=1,KB)
      DO 7 K=1,KB
    7 T(I,K)=TZZ(K)
   10 CONTINUE
c      CALL PRXZ('   T   ',0.,T,IM,1,KB,0.00001,H,ZZ)
c      CALL PRXZ('   TLEV',0.,TLEV,IM,1,KS,0.,H,ZLEV)
      DO 20 I=2,IM-1
      DO 20 K=1,KS
      TXLEV(I,K)=2.*(TLEV(I+1,K)-TLEV(I,K))
   20 CONTINUE
c      CALL PRXZ('   TXLEV',0.,TXLEV,IM,1,KS,0.,H,ZLEV)
      DO 30 I=2,IM
      DO 25 K=1,KB-2
   25 ZZD(K)=-H(I)*.5*(ZZ(K)+ZZ(K+1))
      DO 26 K=1,KS
   26 VLEV(K)=TXLEV(I,K)
      CALL SPLINC(ZLEV,VLEV,KS,2.D30,2.D30,ZZD,TZZ,KB-2)
      DO 27 K=1,KB-2
      TX(I,K)=TZZ(K)
   27 CONTINUE
C     WRITE(6,'(1x,2I5,2E12.3)') (I,K,TZZ(K),TX(I,K),K=1,KB)
   30 CONTINUE
c      CALL PRXZ('   TX  ',0.,TX,IM,1,KB,0.,H,ZZ)
C  Set up list for search, solve and delete.
      N=0
      DO 50 I=2,IM-1
      N=N+1
   50 IND(N)=I
      NND=N
C   Find max H centers and exclude from list.
      DO 100 N=1,NND
      I=IND(N)
      IF(H(I).GT.H(I-1).AND.H(I).GT.H(I+1)) THEN
	CALL SHIFT(N,IND,NND,IM)
      ELSE
        DO 60 K=2,KB-1
   60   T(I,K)=-10.
      ENDIF
  100 CONTINUE
  150 CONTINUE
c      CALL PRXZ('   T  ',0.,T,IM,1,KB,0.,H,ZZ)
C
      LOOP=0
C
  190 CONTINUE
      N=0
  200 N=N+1
      LOOP=LOOP+1
      IF(LOOP.EQ.100) THEN
c      CALL PRXZ('   T  ',0.,T,IM,1,KB,0.,H,ZZ)
      STOP
      ENDIF
      IF(NND.EQ.0) GOTO 300
      I=IND(N)
      WRITE(6,'(''FIRST N,NND,I ='',3I5)') N,NND,I
      WRITE(6,'(''IND ='',10I5)') (IND(kkk),kkk=1,NND)
      IF(I.GT.1.AND.H(I).LT.H(I-1).AND.T(I-1,2).NE.-10.) THEN
      DO 205 K=1,KB-2
      R=(H(I)-H(I-1))*(ZZ(K+1)+ZZ(K))/((H(I)+H(I-1))*(ZZ(K)-ZZ(K+1)))
      T(I,K+1)=TX(I-1,K)
     1     -T(I,K)*(1.-R)+T(I-1,K)*(1.+R)+T(I-1,K+1)*(1.-R)
      T(I,K+1)=T(I,K+1)/(1+R)
  205 CONTINUE
	CALL SHIFT(N,IND,NND,IM)
        N=N-1
      ENDIF
      IF(N.LT.NND) GOTO 200
      N=NND+1
  210 N=N-1
      IF(NND.EQ.0) GOTO 300
      I=IND(N)
      WRITE(6,'(''SCOND N,NND,I ='',3I5)') N,NND,I
      WRITE(6,'(''IND ='',10I5)') (IND(kkk),kkk=1,NND)
      IF(I.LT.IM.AND.H(I).LT.H(I+1).AND.T(I+1,2).NE.-10.) THEN
      DO 215 K=1,KB-2
      R=(H(I+1)-H(I))*(ZZ(K+1)+ZZ(K))/((H(I+1)+H(I))*(ZZ(K)-ZZ(K+1)))
      T(I,K+1)=-TX(I,K)
     1    +T(I+1,K)*(1.-R)+T(I+1,K+1)*(1.+R)-T(I,K)*(1.+R)
      T(I,K+1)=T(I,K+1)/(1-R)
  215 CONTINUE
	CALL SHIFT(N,IND,NND,IM)
      ENDIF
      IF(N.GT.1) GOTO 210
      GOTO 190
  300 CONTINUE
c      CALL PRXZ('   T  ',0.,T,IM,1,KB,0.,H,ZZ)
      RETURN
      END
      SUBROUTINE SHIFT(N,IND,NND,IM)
      DIMENSION IND(IM)
	NND=NND-1
	DO 200 NNN=N,NND
  200 IND(NNN)=IND(NNN+1)
      RETURN
      END
*DECK DENS
      SUBROUTINE DENS
C
      include 'comblk2d.h'
c
      REAL*8 RHOF(IM,KB),TF(IM,KB),SF(IM,KB)
      EQUIVALENCE (A,RHOF),(VH,TF),(UF,SF)
C
C         THIS FUNCTION COMPUTES DENS - 1.025
C
      DO 1 K=1,KBM1
      DO 1 I=1,IM
       TR=T(I,K)+10.D0
       SR=S(I,K)+35.D0
c
C            Approximate pressure in units of bars
       P=-GRAV*1.025*ZZ(K)*DT(I)*0.01
C
       RHOR = 999.842594 + 6.793952D-2*TR
     $        - 9.095290D-3*TR**2 + 1.001685D-4*TR**3
     $        - 1.120083D-6*TR**4 + 6.536332D-9*TR**5
C
       RHOR = RHOR + (0.824493 - 4.0899D-3*TR
     $       + 7.6438D-5*TR**2 - 8.2467D-7*TR**3
     $       + 5.3875D-9*TR**4) * SR
     $       + (-5.72466D-3 + 1.0227D-4*TR
     $       - 1.6546D-6*TR**2) * ABS(SR)**1.5
     $       + 4.8314D-4 * SR**2
C
       CR=1449.1+.0821*P+4.55*TR-.045*TR**2
     $                              +1.34*(SR-35.)
       RHOR=RHOR + 1.D5*P/CR**2
     $     *(1.-2.*P/CR**2)
C
       RHO(I,K)=(RHOR-1025.)*1.D-3*FSM(I)

    1 CONTINUE
c
      DO 3 I=1,IM
    3  RHO(I,KB)=0.D0
      RETURN
      END
c
      SUBROUTINE SIGMAT(sigtheta)
C
c     Computes sigma_theta
c
c     Jose A. Lima 21/11/98
c
      include 'comblk2d.h'
c
      REAL*8 RHOF(IM,KB),TF(IM,KB),SF(IM,KB)
      EQUIVALENCE (A,RHOF),(VH,TF),(UF,SF)
cjl
      dimension sigtheta(im,kb)
C
C         THIS FUNCTION COMPUTES sigtheta=(DENS,p=0) - 1000
C
      DO 1 K=1,KB
      DO 1 I=1,IM
       TR=T(I,K)+10.D0
       SR=S(I,K)+35.D0
c
C      Approximate pressure in units of bars
cjl    The pressure level will be set to zero in order to
c      generate SIGMA-Theta
       P=0.
C
       RHOR = 999.842594 + 6.793952D-2*TR
     $        - 9.095290D-3*TR**2 + 1.001685D-4*TR**3
     $        - 1.120083D-6*TR**4 + 6.536332D-9*TR**5
C
       RHOR = RHOR + (0.824493 - 4.0899D-3*TR
     $       + 7.6438D-5*TR**2 - 8.2467D-7*TR**3
     $       + 5.3875D-9*TR**4) * SR
     $       + (-5.72466D-3 + 1.0227D-4*TR
     $       - 1.6546D-6*TR**2) * ABS(SR)**1.5
     $       + 4.8314D-4 * SR**2
C
       CR=1449.1+.0821*P+4.55*TR-.045*TR**2
     $                              +1.34*(SR-35.)
       RHOR=RHOR + 1.D5*P/CR**2
     $     *(1.-2.*P/CR**2)
C
       SIGTHETA(I,K)=(RHOR-1000)*FSM(I)

    1 CONTINUE
c
      RETURN
      END
*DECK DEPTH
      SUBROUTINE DEPTH(Z,ZZ,DZ,DZZ,DZR,KB,KBM1)
CSB   IMPIMCIT HALF PRECISION (A-H,O-Z)
C >>>
      DIMENSION Z(KB),ZZ(KB),DZ(KB),DZZ(KB),DZR(KBM1)
      KL1=6
      KL2=KB-6
C ***
C    THIS SUBROUTINE ESTABIMSHES THE VERTICAL RESOLUTION WITH LOG
C    DISTRIBUTIONS  AT THE TOP AND BOTTOM AND A IMNEAR DISTRIBUTION
C    BETWEEN. CHOOSE KL1 AND KL2. THE DEFAULT KL1 = .3*KB AND KL2 = KB-2
C    YIELDS A LOG DISTRIBUTION AT THE TOP AND NONE AT THE BOTTOM.
C ***
      BB=dfloat(KL2-KL1)+4.
      CC=dfloat(KL1)-2.
      DEL1=2./BB/EXP(.693147*dfloat(KL1-2))
      DEL2=2./BB/EXP(.693147*dfloat(KB-KL2-1))
      Z(1)=0.
      ZZ(1)=-DEL1/2.
      DO 30 K=2,KL1
      Z(K)=-DEL1*EXP(.693147*dfloat(K-2))
   30 ZZ(K)=-DEL1*EXP(.693147*(dfloat(K)-1.5))
      DO 40 K=KL1,KL2
      Z(K)=-(dfloat(K)-CC)/BB
   40 ZZ(K)=-(dfloat(K)-CC+0.5)/BB
      DO 50 K=KL2,KBM1
      Z(K)=(1.0-DEL2*EXP(.693147*dfloat(KB-K-1)))*(-1.)
   50 ZZ(K)=(1.0-DEL2*EXP(.693147*(dfloat(KB-K)-1.5)))*(-1.)
      Z(KB)=-1.0
      ZZ(KBM1)=-1.*(1.-DEL2/2.)
      ZZ(KB)=-1.*(1.+DEL2/2.)
C **
C     Used for a constant sigma level spacing.
C     Put in comments if you want to have the log layers.
c      DO 55 K=1,KB
c         DENOM=dfloat(KB-1)
c         Z(K)=-dfloat(K-1)/DENOM
c         ZZ(K)=Z(K)-0.5/DENOM
c  55  CONTINUE
C ***
      DO 60 K=1,KBM1
      DZ(K)=Z(K)-Z(K+1)
      DZR(K)=1./DZ(K)
   60 DZZ(K)=ZZ(K)-ZZ(K+1)
      WRITE(6,70)
   70 FORMAT(/2X,'K',7X,'Z',9X,'ZZ',9X,'DZ',9X,'DZZ',/)
      DO 90 K=1,KB
      WRITE(6,80) K,Z(K),ZZ(K),DZ(K),DZZ(K)
   80 FORMAT(' ',I5,4F10.4)
   90 CONTINUE
      RETURN
      END
*DECK PROFQ
      SUBROUTINE PROFQ(DT2)
C
      include 'comblk2d.h'
c

      REAL*8 KAPPA,KN
C
      DIMENSION PROD(IM,KB),KN(IM,KB),BOYGR(IM,KB)
      DIMENSION DH(IM),CC(IM,KB)
      DIMENSION GH(IM,KB),SM(IM,KB),SH(IM,KB)
C     EQUIVALENCE (A,SM),(C,SH),(PROD,KN),(TPS,DH)
C     EQUIVALENCE (DTEF,CC,GH)

      DATA PROD/IMK*0./,BOYGR/IMK*0./
      DATA GH/IMK*0./,SM/IMK*0./,SH/IMK*0./
      DATA A1,B1,A2,B2,C1/0.92,16.6,0.74,10.1,0.08/
      DATA E1/1.8/,E2/1.33/,E3/1.0/
      DATA KAPPA/0.40/,SQ/0.20/,SEF/1./
      DATA GEE/9.806/
      DATA SMALL/1.D-8/
C
c     Setting variables to null values
      do i=1,im
	  do k=1,kb
	    km(i,k)=0.
	    kh(i,k)=0.
	  enddo
	enddo
C
      DO 50 I=1,IM
   50 DH(I)=H(I)+ETF(I)
      DO 100 K=2,KBM1
      DO 100 I=1,IM
      A(I,K)=-DT2*(KQ(I,K+1)+KQ(I,K)+2.*UMOL)*.5
     1    /(DZZ(K-1)*DZ(K)*DH(I)*DH(I))
      C(I,K)=-DT2*(KQ(I,K-1)+KQ(I,K)+2.*UMOL)*.5
     1    /(DZZ(K-1)*DZ(K-1)*DH(I)*DH(I))
  100 CONTINUE
C***********************************************************************
C                                                                      *
C        THE FOLLOWING SECTION SOLVES THE EQUATION                     *
C        DT2*(KQ*Q2')' - Q2*(2.*DT2*DTEF+1.) = -Q2B                    *
C                                                                      *
C***********************************************************************
C------  SURFACE AND BOTTOM B.C.S ------------
      CONST1=16.6**.6666667*SEF
      DO 90 I=1,IM
      VH(I,1)=0.
      VHP(I,1)=SQRT( (.5*(WUSURF(I)+WUSURF(I+1)))**2
     1                +(.5*(WVSURF(I)+WVSURF(I)))**2 )*CONST1
      UF(I,KB)=SQRT( (.5*(WUBOT(I)+WUBOT(I+1)))**2
     1                +(.5*(WVBOT(I)+WVBOT(I)))**2 )*CONST1
   90 CONTINUE
C---------------------------------------------------------------------
C   RESTORE 10 DEG, 35 PPT AND 1.025 BIAS IN T, S AND RHO
C---------------------------------------------------------------------
c     DO 101 K=1,KBM1
c     DO 101 I=1,IM
c     TP=T(I,K)+10.
C      Calculate pressure in units of decibars
c     P=-GEE*1.025*ZZ(K)*DH(I)*.1
c     CC(I,K)=1449.1+.00821*P+4.55*TP -.045*TP**2
c    $                              +1.34*(S(I,K)- 0.)
c     CC(I,K)=CC(I,K)/SQRT((1.-.01642*P/CC(I,K))
c    $       *(1.-0.40*P/CC(I,K)**2))
c 101 CONTINUE
      DO 102 K=2,KBM1
      DO 102 I=2,IM
      Q2B(I,K)=ABS(Q2B(I,K))
      Q2LB(I,K)=ABS(Q2LB(I,K))
      BOYGR(I,K)=GEE*((RHO(I,K-1)-RHO(I,K))/(DZZ(K-1)*DH(I)))
C    1 +GEE**2*2.*1.025/(CC(I,K-1)**2+CC(I,K)**2)
  102 CONTINUE
C------ CALC. T.K.D. PRODUCTION -----------------------------------
      DO 120 K=2,KBM1
      DO 120 I=2,IM
      PROD(I,K)=KM(I,K)*.25*SEF
     1       *( (U(I,K)-U(I,K-1)+U(I+1,K)-U(I+1,K-1))**2
     2         +(V(I,K)-V(I,K-1)+V(I,K)-V(I,K-1))**2 )
     3              /(DZZ(K-1)*DH(I))**2
  120 PROD(I,K)=PROD(I,K)+KH(I,K)*BOYGR(I,K)
      DO 110 K=2,KBM1
      DO 110 I=1,IM
  110 DTEF(I,K)=Q2B(I,K)*SQRT(Q2B(I,K))/(B1*Q2LB(I,K)+SMALL)
      DO 140 K=2,KBM1
      DO 140 I=1,IM
      VHP(I,K)=1./(A(I,K)+C(I,K)*(1.-VH(I,K-1))
     1    -(2.*DT2*DTEF(I,K)+1.) )
      VH(I,K)=A(I,K)*VHP(I,K)
      VHP(I,K)=(-2.*DT2*PROD(I,K)
     1  +C(I,K)*VHP(I,K-1)-UF(I,K))*VHP(I,K)
  140 CONTINUE
      DO 150 K=1,KBM1
      KI=KB-K
      DO 150 I=1,IM
      UF(I,KI)=VH(I,KI)*UF(I,KI+1)+VHP(I,KI)
  150 CONTINUE
C***********************************************************************
C                                                                      *
C        THE FOLLOWING SECTION SOLVES THE EQUATION                     *
C        DT2(KQ*Q2L')' - Q2L*(DT2*DTEF+1.) = -Q2LB                     *
C                                                                      *
C***********************************************************************
      DO 155 I=1,IM
      VH(I,1)=0.0
  155 VHP(I,1)=0.0
      DO 160 K=2,KBM1
      DO 160 I=1,IM
      DTEF(I,K) =DTEF(I,K)*(1.+E2*((1./ABS(Z(K)-Z(1))+
     1    1./ABS(Z(K)-Z(KB))) *L(I,K)/(DH(I)*KAPPA))**2)
      VHP(I,K)=1./(A(I,K)+C(I,K)*(1.-VH(I,K-1))
     1    -(DT2*DTEF(I,K)+1.))
      VH(I,K)=A(I,K)*VHP(I,K)
      VHP(I,K)=(DT2*(-PROD(I,K)
     1   *L(I,K)*E1)+C(I,K)*VHP(I,K-1)-VF(I,K))*VHP(I,K)
  160 CONTINUE
      DO 170 K=1,KBM1
      KI=KB-K
      DO 170 I=1,IM
      VF(I,KI)=VH(I,KI)*VF(I,KI+1)+VHP(I,KI)
  170 CONTINUE
      DO 180 K=2,KBM1
      DO 180 I=1,IM
      IF(UF(I,K).LE.SMALL. OR.VF(I,K).LE.SMALL) THEN
        UF(I,K)=SMALL
        VF(I,K)=SMALL
      ENDIF
  180 CONTINUE
C***********************************************************************
C                                                                      *
C               THE FOLLOWING SECTION SOLVES FOR KM AND KH             *
C                                                                      *
C***********************************************************************
      COEF1=A2*(1.-6.*A1/B1)
      COEF2=3.*A2*B2+18.*A1*A2
      COEF3=A1*(1.-3.*C1-6.*A1/B1)
      COEF4=18.*A1*A1+9.*A1*A2
      COEF5=9.*A1*A2
C NOTE THAT SM,SH LIMIT TO INFINITY WHEN GH APPROACHES 0.0288
      DO 220 K=2,KBM1
      DO 220 I=2,IM
      L(I,K)=VF(I,K)/UF(I,K)
 220  GH(I,K)=L(I,K)**2/UF(I,K)*BOYGR(I,K)
      DO 230 K=2,KBM1
      DO 230 I=2,IM
      GH(I,K)=MIN(GH(I,K),.028)
      SH(I,K)=COEF1/(1.-COEF2*GH(I,K))
      SM(I,K)=COEF3+SH(I,K)*COEF4*GH(I,K)
      SM(I,K)=SM(I,K)/(1.-COEF5*GH(I,K))
  230 CONTINUE
C
      DO 280 K=2,KBM1
      DO 280 I=2,IM
      KN(I,K)=L(I,K)*SQRT(ABS(Q2(I,K)))
      KQ(I,K)=(KN(I,K)*.41*SM(I,K)+KQ(I,K))*.5
      KM(I,K)=(KN(I,K)*SM(I,K)+KM(I,K))*.5
      KH(I,K)=(KN(I,K)*SH(I,K)+KH(I,K))*.5
  280 CONTINUE
c      IF(IINT.EQ.2592.OR.IINT.EQ.15552) THEN
c        CALL PRXZ('    Q2   ',TIME,Q2,IM,1,KB,0.,DT,Z)
c        CALL PRXZ('     L   ',TIME,L ,IM,1,KB,0.,DT,Z)
c        CALL PRXZ(' BOYGR   ',TIME,BOYGR ,IM,1,KB,0.,DT,Z)
c        CALL PRXZ('   GH    ',TIME, GH   ,IM,1,KB,10.,DT,Z)
c        CALL PRXZ('   SM    ',TIME, SM   ,IM,1,KB,0.,DT,Z)
c        CALL PRXZ('   KM    ',TIME, KM   ,IM,1,KB,0.,DT,Z)
c      ENDIF
      RETURN
      END
*DECK PROFT
      SUBROUTINE PROFT(F,WFSURF,FSURF,NBC,DT2)
C
      include 'comblk2d.h'
c
      REAL*8 AF(IM,KB),CF(IM,KB),VHF(IM,KB),VHPF(IM,KB)
      REAL*8 FF(IM,KB)
      DIMENSION F(IM,KB),WFSURF(IM),FSURF(IM),DH(IM)
      EQUIVALENCE (A,AF),(RHO,CF),(VH,VHF),(AF,FF)
      EQUIVALENCE (TPS,DH)
      UMOLPR=UMOL
C**********************************************************************
C                                                                      *
C        THE FOLLOWING SECTION SOLVES THE EQUATION                     *
C         DT2*(KH*F')'-F=-FB                                           *
C                                                                      *
C**********************************************************************
CSB
      DO 90 I=1,IM
      DH(I)=H(I)+ETF(I)
   90 CONTINUE
      DO 100 K=2,KBM1
CSB
      DO 100 I=1,IM
      AF(I,K-1)=-DT2*(KH(I,K)+UMOLPR)/(DZ(K-1)*DZZ(K-1)*DH(I)
     1     *DH(I))
      CF(I,K)=-DT2*(KH(I,K)+UMOLPR)/(DZ(K)*DZZ(K-1)*DH(I)
     1     *DH(I))
  100 CONTINUE
C----- SURFACE BC'S; WFSURF OR FSURF -----------------------------------
      GO TO (50,51), NBC
   50 CONTINUE
CSB
      DO 501 I=1,IM
      VHF(I,1)=AF(I,1)/(AF(I,1)-1.)
      VHPF(I,1)=-DT2*WFSURF(I)/(-DZ(1)*DH(I))-F(I,1)
501   VHPF(I,1)=VHPF(I,1)/(AF(I,1)-1.)
      GO TO 52
   51 CONTINUE
CSB
      DO 511 I=1,IM
      VHF(I,1)=0.
511   VHPF(I,1)=FSURF(I)
   52 CONTINUE
C----------------------------------------------------------------------
      DO 101 K=2,KBM2
CSB
      DO 101 I=1,IM
      VHPF(I,K)=1./(AF(I,K)+CF(I,K)*(1.-VHF(I,K-1))-1.)
      VHF(I,K)=AF(I,K)*VHPF(I,K)
      VHPF(I,K)=(CF(I,K)*VHPF(I,K-1)-REAL(F(I,K)))*VHPF(I,K)
  101 CONTINUE
CSB
      DO 1011 K=1,KB
      DO 1011 I=1,IM
1011  FF(I,K)=F(I,K)
      DO 102 I=1,IM
  102 FF(I,KBM1)=((CF(I,KBM1)*VHPF(I,KBM2)-FF(I,KBM1))
     1   /(CF(I,KBM1)*(1.-VHF(I,KBM2))-1.))
      DO 105 K=2,KBM1
      KI=KB-K
      DO 105 I=1,IM
      FF(I,KI)=(VHF(I,KI)*FF(I,KI+1)+VHPF(I,KI))
  105 CONTINUE
CSB
      DO 106 K=1,KB
      DO 106 I=1,IM
106   F(I,K)=FF(I,K)
      RETURN
      END
*DECK PROFU
      SUBROUTINE PROFU(DT2)
C
      include 'comblk2d.h'
c
      DIMENSION DH(IM)
      DATA UMOLB/1.36D-6 /
C***********************************************************************
C                                                                      *
C        THE FOLLOWING SECTION SOLVES THE EQUATION                     *
C         DT2*(KM*U')'-U=-UB                                           *
C                                                                      *
C***********************************************************************
CSB
      DO 84 I=1,IM
84    DH(I)=1.0
      DO 85 I=2,IM
   85 DH(I)=.5D0*(H(I)+ETF(I)+H(I-1)+ETF(I-1))
      DO 90 K=1,KB
      DO 90 I=2,IM
   90 C(I,K)=(KM(I,K)+KM(I-1,K))*.5D0
      DO 100 K=2,KBM1
CSB
      DO 100 I=1,IM
      A(I,K-1)=-DT2*(C(I,K)+UMOL  )/(DZ(K-1)*DZZ(K-1)*DH(I)
     1     *DH(I))
      C(I,K)=-DT2*(C(I,K)+UMOL  )/(DZ(K)*DZZ(K-1)*DH(I)
     1     *DH(I))
  100 CONTINUE
CSB
      DO 1011 I=1,IM
      VH(I,1)=A(I,1)/(A(I,1)-1.D0)
      VHP(I,1)=(-DT2*WUSURF(I)/(-DZ(1)*DH(I))-UF(I,1))
     1   /(A(I,1)-1.D0)
1011  CONTINUE
      DO 101 K=2,KBM2
      DO 101 I=2,IM
      VHP(I,K)=1.D0/(A(I,K)+C(I,K)*(1.-VH(I,K-1))-1.)
      VH(I,K)=A(I,K)*VHP(I,K)
      VHP(I,K)=(C(I,K)*VHP(I,K-1)-UF(I,K))*VHP(I,K)
  101 CONTINUE
      DO 102 I=2,IM
      TPS(I)=0.5D0*(CBC(I)+CBC(I-1))
     1     *SQRT(UB(I,KBM1)**2+(.25D0*(VB(I,KBM1)
     2     +VB(I,KBM1)+VB(I-1,KBM1)+VB(I-1,KBM1)))**2)
      UF(I,KBM1)=(C(I,KBM1)*VHP(I,KBM2)-UF(I,KBM1))/(TPS(I)
     1 *DT2/(-DZ(KBM1)*DH(I))-1.D0-(VH(I,KBM2)-1.D0)*C(I,KBM1))
  102 UF(I,KBM1)=UF(I,KBM1)*DUM(I)
      DO 103 K=2,KBM1
      KI=KB-K
      DO 103 I=1,IM
      UF(I,KI)=(VH(I,KI)*UF(I,KI+1)+VHP(I,KI))*DUM(I)
  103 CONTINUE
CSB
      DO 104 I=1,IM
104   WUBOT(I)=-TPS(I)*UF(I,KBM1)
      RETURN
      END
*DECK PROFV
      SUBROUTINE PROFV(DT2)
C
      include 'comblk2d.h'
c
      DIMENSION DH(IM)
      DATA UMOLB/1.36D-6/
C***********************************************************************
C                                                                      *
C        THE FOLLOWING SECTION SOLVES THE EQUATION                     *
C         DT2*(KM*U')'-U=-UB                                           *
C                                                                      *
C***********************************************************************
CSB
      DO 84 I=1,IM
84    DH(I)=1.
      DO 85 I=1,IM
   85 DH(I)=H(I)+ETF(I)
      DO 90 K=1,KB
      DO 90 I=1,IM
   90 C(I,K)=KM(I,K)
      DO 100 K=2,KBM1
CSB
      DO 100 I=1,IM
      A(I,K-1)=-DT2*(C(I,K)+UMOL  )/(DZ(K-1)*DZZ(K-1)*DH(I)
     1     *DH(I))
      C(I,K)=-DT2*(C(I,K)+UMOL  )/(DZ(K)*DZZ(K-1)*DH(I)
     1     *DH(I))
  100 CONTINUE
CSB
      DO 1001 I=1,IM
      VH(I,1)=A(I,1)/(A(I,1)-1.D0)
      VHP(I,1)=(-DT2*WVSURF(I)/(-DZ(1)*DH(I))-VF(I,1))
     1   /(A(I,1)-1.D0)
1001  CONTINUE
      DO 101 K=2,KBM2
CSB
      DO 101 I=1,IM
      VHP(I,K)=1.D0/(A(I,K)+C(I,K)*(1.D0-VH(I,K-1))-1.D0)
      VH(I,K)=A(I,K)*VHP(I,K)
      VHP(I,K)=(C(I,K)*VHP(I,K-1)-VF(I,K))*VHP(I,K)
  101 CONTINUE
      DO 102 I=1,IMM1
      TPS(I)=CBC(I)
     1     *SQRT((.25D0*(UB(I,KBM1)+UB(I+1,KBM1)
     2     +UB(I,KBM1)+UB(I+1,KBM1)))**2+VB(I,KBM1)**2)
      VF(I,KBM1)=(C(I,KBM1)*VHP(I,KBM2)-VF(I,KBM1))/(TPS(I)
     1  *DT2/(-DZ(KBM1)*DH(I))-1.D0-(VH(I,KBM2)-1.D0)*C(I,KBM1))
  102 VF(I,KBM1)=VF(I,KBM1)*DVM(I)
      DO 103 K=2,KBM1
      KI=KB-K
      DO 103 I=1,IM
      VF(I,KI)=(VH(I,KI)*VF(I,KI+1)+VHP(I,KI))*DVM(I)
  103 CONTINUE
CSB
      DO 104 I=1,IM
104   WVBOT(I)=-TPS(I)*VF(I,KBM1)
      RETURN
      END
*DECK PRXZ
      SUBROUTINE PRXZ(LABEL,TIME,A,IM,ISKP,KB,SCALA,DT,ZZ)
      implicit real*8 (a-h,o-z)
	implicit integer*4 (i-n)
      DIMENSION A(IM,KB),NUM(IM),LINE(IM),IDT(IM),
     1        ZZ(KB),DT(IM)
      CHARACTER LABEL*(*)
c
c        THIS SUBROUTINE WRITES VERTICAL SECTIONS OF A 3-D FIELD
c
c      TIME=TIME IN DAYS
c      A= ARRAY(IM,KB) TO BE PRINTED
c      ISPL=PRINT SKIP FOR I
C      SCALE=DIVISOR FOR VALUES OF A
c
c      PS: This routine was modified by Jose Lima in order
C          to print the maximun and minimum values and their
C          respective positions in the array.  J.Lima 20/Mar/96
c
      DATA ZERO /1.D-10/
c
      AMAX=1.D-20
      AMIN=1.D20
      AMX=ZERO
      DO 150 K=1,KB
         DO 150 I=1,IM
	       AMX=DMAX1(ABS(A(I,K)),AMX)
             AMAX=dmax1(A(i,k),AMAX)
             AMIN=dmin1(A(i,K),AMIN)
 150  CONTINUE
      SCALE=SCALA
      IF(SCALE.GT.ZERO) GOTO 160
         SCALE=10.**(dINT(dLOG10(AMX)+1.D2)-103)
 160  CONTINUE
      SCALEI=1./SCALE
      WRITE(6,9980) LABEL
 9980 FORMAT(' ',A30)
      write(6,9970) AMAX
 9970 format(1x,'Maximum value :',G12.3)
      write(6,9971) AMIN
 9971 format(1x,'Minimum value :',G12.3)
      WRITE(6,9981) TIME,SCALE
 9981 FORMAT(' TIME = ',F9.3,' DAYS  MULTIPLY ALL VALUES BY',1PE10.3)
      IB=1
      IE=int(IB+23*ISKP)
  50  CONTINUE
         IF(IE.GT.IM) IE=IM
         DO 100 I=IB,IE,ISKP
            IDT(I)=dint(DT(I))
 100        NUM(I)=I
         WRITE(6,9990) (NUM(I),I=IB,IE,ISKP)
 9990    FORMAT(/,'    I =  ',24I5,/)
         WRITE(6,9991) (IDT(I),I=IB,IE,ISKP)
 9991    FORMAT(8X,'D =',24I5.0,/,'       ZZ ')
         DO 200 K=1,KB
            DO 190 I=IB,IE,ISKP
               LINE(I)=dINT(SCALEI*A(I,K))
               if (dabs(dfloat(line(i))).gt.9999) then
                  line(i)=9999
               endif
 190        CONTINUE
            WRITE(6,9966) K,ZZ(K),(LINE(I),I=IB,IE,ISKP)
 9966       FORMAT(1X,I2,2X,F6.3,24I5)
 200     CONTINUE
         WRITE(6,9984)
         IF((IE+ISKP).GE.IM) GO TO 10
         IB=dint(dfloat(IB+24*ISKP))
         IE=dint(dfloat(IB+23*ISKP))
      GO TO 50
 9984 FORMAT(//)
 10   CONTINUE
      RETURN
      END
*DECK SHPFIL
      SUBROUTINE SHPFIL(UR,UI,IM,KB,NORD)
C
C    THIS IS A SHAPIRO FILTER WHICH REMOVES SMALL SCALE
C    NOISE (TWO-DELTA-X NOISE IS COMPLETELY ELIMINATED)
C    WHILE REDUCING FILTER EFFECT ON LARGER SCALES.
C
      PARAMETER(II=41)
      DIMENSION UI(IM,KB),UR(IM,KB)
      COMPLEX FLUX(II),VCM(II),FLCON(8)
      DATA FLCON/ (.5,0.),  (-.5,0.),  (0.,.5),  (0.,-.5),
     1    (.354,.354), (-.354,-.354), (-.354,.354), (.354,-.354)/
C
      DO 1000 K=1,KB
C
      DO 70 I=1,IM
   70 VCM(I)=CMPLX(UR(I,K),UI(I,K))
C
      DO 500 LOOP=1,NORD
C
C     TIME=dfloat(LOOP)+.0001
C     WRITE(6,'(1X,I5,2F10.3)') LOOP,FLCON(LOOP)
C
      DO  100 I=1,IM-1
  100 FLUX(I)= FLCON(LOOP)*(VCM(I+1)-VCM(I))
      DO 120 I=2,IM-1
 120  VCM(I)=VCM(I)
     1     +0.5*(FLUX(I)-FLUX(I-1))
 500  CONTINUE
C
      DO 10  I=1,IM
      UR(I,K)=REAL(VCM(I))
  10  UI(I,K)=AIMAG(VCM(I))
 1000 CONTINUE
      RETURN
      END
C
*DECK VERTVL
      SUBROUTINE VERTVL(DTI2)
C
      include 'comblk2d.h'
c
      DIMENSION XFLUX(IM,KB),YFLUX(IM,KB)
      EQUIVALENCE (XFLUX,A),(YFLUX,C)
C
C
C CALCULATE NEW VERTICAL VELOCITY
C
C REESTABIMSH BOUNDARY CONDITIONS
      DO 100 K=1,KBM1
      DO 100 I=2,IM
 100  XFLUX(I,K)
     1 =.5D0*(DT(I)+DT(I-1))*U(I,K)
      DO 120 K=1,KBM1
      DO 120 I=1,IM
  120 YFLUX(I,K)
     1  =DX(I)*DT(I)*V(I,K)
CSB
      DO 125 I=1,IM
125   W(I,1)=0.D0
      DO 710 K=1,KBM1
      DO 710 I=1,IMM1
 710  W(I,K+1)=W(I,K)
     1    +DZ(K)*((XFLUX(I+1,K)-XFLUX(I,K))
     2            /DX(I)
     3                        +(ETF(I)-ETB(I))/DTI2 )
      RETURN
      END
      SUBROUTINE ZTOSIG(ZS,TB,ZZ,H,T,IM,KS,KB)
cvc
        implicit real*8 (a-h,o-z)
	implicit integer*4 (i-n)
      DIMENSION ZS(KS),TB(IM,KS),ZZ(KB),H(IM),T(IM,KB),
     1          TIN(KS),TOUT(KB),ZZH(KB)
      DO 40 I=1,IM
        IF(H(I).LT.1.00001)GO TO 40
      DO 45 K=1,KS
      TIN(K)=TB(I,K)
      IF(TIN(K).LT.0.00001.AND.K.NE.1)TIN(K)=TIN(K-1)
      IF(ZS(K).LT.H(I)) KBOT=K
C     IF(ZND(K).LT.-H(I).AND.K.NE.1)TIN(K)=TIN(K-1)
   45 CONTINUE
C
      DO 50 K=1,KB
   50 ZZH(K)=-ZZ(K)*H(I)
C
C        VERTICAL LINEAR INTERP.
      CALL SPLINC(ZS,TIN,KS  ,2.D30,2.D30,ZZH,TOUT,KB)
C
      IPR=IM/2
      IF(I.EQ.IPR) THEN
      WRITE(6,'(//46H Data interpolated from z to sigma grid at I =,I5)'
     1) IPR
      WRITE(6,'(''    H ='',F10.1)') H(IPR)
      WRITE(6,'(1X,I5,4F10.4)') (K,ZS(K),TIN(K),ZZH(K),TOUT(K),K=1,KB)
      WRITE(6,'(1X,I5,2F10.4)') (K,ZS(K),TIN(K),K=KB+1,KS)
      ENDIF
C
      do k=1,KB
        T(I,k)=TOUT(k)
      enddo
   40 CONTINUE
C
      RETURN
      END
C
C
      SUBROUTINE SPLINC(X,Y,N,YP1,YPN,XNEW,YNEW,M)
cvc
        implicit real*8 (a-h,o-z)
	implicit integer*4 (i-n)
      PARAMETER (NMAX=100)
      DIMENSION X(N),Y(N),Y2(NMAX),U(NMAX),XNEW(M),YNEW(M)
      IF (YP1.GT..99E30) THEN
        Y2(1)=0.
        U(1)=0.
      ELSE
        Y2(1)=-0.5
        U(1)=(3./(X(2)-X(1)))*((Y(2)-Y(1))/(X(2)-X(1))-YP1)
      ENDIF
      DO 11 I=2,N-1
        SIG=(X(I)-X(I-1))/(X(I+1)-X(I-1))
        P=SIG*Y2(I-1)+2.
        Y2(I)=(SIG-1.)/P
        U(I)=(6.*((Y(I+1)-Y(I))/(X(I+1)-X(I))-(Y(I)-Y(I-1))
     *      /(X(I)-X(I-1)))/(X(I+1)-X(I-1))-SIG*U(I-1))/P
11    CONTINUE
      IF (YPN.GT..99E30) THEN
        QN=0.
        UN=0.
      ELSE
        QN=0.5
        UN=(3./(X(N)-X(N-1)))*(YPN-(Y(N)-Y(N-1))/(X(N)-X(N-1)))
      ENDIF
      Y2(N)=(UN-QN*U(N-1))/(QN*Y2(N-1)+1.)
      DO 12 K=N-1,1,-1
        Y2(K)=Y2(K)*Y2(K+1)+U(K)
12    CONTINUE
C
      DO 20 I =1,M
      CALL SPLINT(X,Y,Y2,N,XNEW(I),YNEW(I))
  20  CONTINUE
      RETURN
      END
      SUBROUTINE SPLINT(XA,YA,Y2A,N,X,Y)
cvc
      implicit real*8 (a-h,o-z)
	implicit integer*4 (i-n)
      DIMENSION XA(N),YA(N),Y2A(N)
      KLO=1
      KHI=N
1     IF (KHI-KLO.GT.1) THEN
        K=(KHI+KLO)/2
        IF(XA(K).GT.X)THEN
          KHI=K
        ELSE
          KLO=K
        ENDIF
      GOTO 1
      ENDIF
      H=XA(KHI)-XA(KLO)
      A=(XA(KHI)-X)/H
      B=(X-XA(KLO))/H
      Y=A*YA(KLO)+B*YA(KHI)+
     *      ((A**3-A)*Y2A(KLO)+(B**3-B)*Y2A(KHI))*(H**2)/6.
      RETURN
      END
      SUBROUTINE SLPMIN(H,IM,FSM)
      DIMENSION H(IM),FSM(IM)
      DIMENSION SL(IM)
      DO 3 LOOP=1,10
cvc
C       SLMIN=0.5
       SLMIN=0.2
cvc
      DO 1 I=2,IM-1
      IF(FSM(I).EQ.0..OR.FSM(I+1).EQ.0.) GOTO 1
      SL(I)=ABS(H(I+1)-H(I))/(H(I)+H(I+1))
      IF(SL(I).LT.SLMIN) GOTO 1
      DH=0.5*(SL(I)-SLMIN)*(H(I)+H(I+1))
      SN=-1.
      IF(H(I+1).GT.H(I)) SN=1.
      H(I+1)=H(I+1)-SN*DH
      H(I)=H(I)+SN*DH
   1  CONTINUE
      DO 2 I=IM-1,2,-1
      IF(FSM(I).EQ.0..OR.FSM(I+1).EQ.0.) GOTO 2
      SL(I)=ABS(H(I+1)-H(I))/(H(I)+H(I+1))
      IF(SL(I).LT.SLMIN) GOTO 2
      DH=0.5*(SL(I)-SLMIN)*(H(I)+H(I+1))
      SN=-1.
      IF(H(I+1).GT.H(I)) SN=1.
      H(I+1)=H(I+1)-SN*DH
      H(I)=H(I)+SN*DH
   2  CONTINUE
   3  CONTINUE
c      CALL PRXZ('  SL        ',0.,SL ,IM,1,1 ,0., H,ZZ)
      RETURN
      END

c*********************************************************************
c  THIS GROUP OF SUBROUTINES ARE ADDED TO ORIGINAL POM2D
c*********************************************************************
      function THETA(SST,T250,zn)
      real*8  SST,T250,zn,Z(9),LEVMIN(9),LEVMAX(9),LD(9)
      real*8  alpha,thetan(9),hmix
      real*8  XI(50),C(4,50)
      data hmix/1.0/
      data Z/250.,500.,750.,1000.,1500.,2000.,3000.,4000.,5000./
      data LEVMIN/10.0,8.5 ,6.2,4.9,3.0,2.2,1.5,0.8,0.4/
      data LEVMAX/20.0,11.6,7.4,5.6,3.2,2.2,1.5,0.8,0.4/
      data LD/    10. , 3.1,1.2,.7 ,0.2,0.,0.,0.,0./

c     data LEVMIN/10.0,8.6 ,6.3,5.0,3.0,2.2,1.5,0.8,0.4/
c     data LEVMAX/20.0,11.6,7.4,5.6,3.2,2.2,1.5,0.8,0.4/
c     data LD/    10. , 3.0,1.1,.6 ,0.2,0.,0.,0.,0./

      if(zn.le.hmix)then
      THETA=SST
      return
      endif


      alpha = (T250-LEVMIN(1))/LD(1)
      C(1,1)= SST
      XI(1)= hmix
      do 101 k=1,9
      thetan(k)= LEVMIN(k)+alpha*LD(k)
      XI(k+1) = Z(k)
      C(1,k+1)=thetan(k)
 101  continue

      C(2,1)=0.0
      C(2,10) = (C(1,9)-C(1,10))/(XI(9)-XI(10))
      N=9

      CALL SPLINE(N,XI,C)
      THETA =PCUBIC(zn,N,XI,C)
      return
      end

c****************************************************************************
      function SALIN(tn,zn)
      real*8 tn,zn
C   Salinity is computed from a polynomial form
C   ref:  A.F.Pearce AJMFR 1983,pp 115-19

C      Salinity for 25S-45S (T=2.5 -- 28.0 )  IMPORTANTE: O NUMERO DE DIAS DE DADOS DE VENTO FORNECIDOS DEVE SER IGUAL C
C     AO NUMERO DE DIAS DA SIMULACAO NO MODO PROGNOSTICO                       C
      temp = tn +zn*.12D-3
      if(temp.lt.2.0) then
      SALIN =34.730 + .01*(2.0-temp)
      return
      endif


      SALIN= 35.2670-.361225*temp +.0503699*temp**2
     $  -.00215048*temp**3  +.0000291048*temp**4
      return
      end
c***************************************************************************
      subroutine SPLINE(N,XI,C)
cvc
        implicit real*8 (a-h,o-z)
	implicit integer*4 (i-n)
c   cubic spline matrix initialiser routine
c      N+1 point (N intevals)
c      XI(N+1) positions
c      Cubic polynomial evaluated using PCUBIC

       dimension XI(50),C(4,50),D(50),DIAG(50)
       data DIAG(1),D(1)/1.,0./
       NP1 = N+1
       DO 10 M= 2,NP1
       D(M) = XI(M) - XI(M-1)
 10    DIAG(M) = (C(1,M) - C(1,M-1))/D(M)
       DO 20 M=2,N
       C(2,M)= 3.*(D(M)*DIAG(M+1) + D(M+1)*DIAG(M))
 20    DIAG(M) = 2.*(D(M) + D(M+1) )
       DO 30 M=2,N
       G = -D(M+1)/DIAG(M-1)
 30    C(2,M) = C(2,M)+G*C(2,M-1)
       NJ = NP1
       DO 40 M=2,N
       NJ = NJ -1
 40    C(2,NJ) = (C(2,NJ) - D(NJ)*C(2,NJ+1))/DIAG(NJ)
       DO 50 M=2,NP1
       DIVDF1 = (C(1,M)-C(1,M-1) )/D(M)
       DIVDF3 = C(2,M-1)+C(2,M) - 2.*DIVDF1
       C(3,M-1) =(DIVDF1 -C(2,M-1) - DIVDF3)/D(M)
 50    C(4,M-1) = DIVDF3/D(M)**2

       RETURN
       END
c***************************************************************************
       function PCUBIC(XBAR,N,XI,C)
cvc
       implicit real*4 (a-h,o-z)
	implicit integer*4 (i-n)
       DIMENSION XI(50),C(4,50)

       DO 50  I=N,1,-1
       IF (XI(I).LT.XBAR) THEN
       DX = XBAR -XI(I)
       PCUBIC = C(1,I) + DX*(C(2,I) + DX*(C(3,I)+DX*C(4,I)))
       RETURN
       ENDIF
 50    CONTINUE
       print *, 'evaluation error ',XBAR
       PCUBIC=0.0
       return
       end
c***************************************************************************
c***************************************************************************
c***************************************************************************
c***************************************************************************
c***************************************************************************
c***************************************************************************

      subroutine update_vento(DIA,IEND)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CAPF-TB subrotina UPDATE_VENTO para uso com o POMxz adaptada da subrotina de mesmo nome elaborada por Edmo Campos        C
C       André Palóczy Filho e Tiago Carrilho Biló 17/01/2011                                                             C
C------------------------------------------------------------------------------------------------------------------------C
C       esta subrotina le um arquivo com dados de vento (dependentes do tempo) correspondentes aos dias da simulacao,    C
C       interpolados para o numero de pontos de grade horizontais, interpola as componentes u e v para cada passo de     C
C       tempo do modelo, e calcula as tensões de cisalhamento correspondentes, atualizando as variaveis WUSURF e WVSURF  C
C       --> Formato do arquivo de dados de vento:                                                                        C
C                                                                                                                        C
C                                                 0         | uvento_forc(1,:) | vvento_forc(1,:)                        C
C                                             TIME_VENTO(2) | uvento_forc(2,:) | vvento_forc(2,:)                        C
C                                             TIME_VENTO(3) | uvento_forc(3,:) | vvento_forc(3,:)                        C
C                                                 .         | .                | .                                       C
C                                                 .         | .                | .                                       C
C                                                 .         | .                | .                                       C
C                                             TIME_VENTO(n) | uvento_forc(n,:) | vvento_forc(n,:)                        C
C                                                                                                                        C
C IMPORTANTE: O NUMERO DE DIAS DE DADOS DE VENTO FORNECIDOS DEVE SER IGUAL                                               C
C AO NUMERO DE DIAS DA SIMULACAO NO MODO PROGNOSTICO                                                                     C
C                                                                                                                        C
C     Onde n é o numero de de dias nos quais há dados de vento disponíveis. As dimensões das variáveis são as seguintes: C
C     TIME_VENTO(2), uvento_forc(n,IM), vvento_forc(n,IM)                                                                C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     IMPORTANTE: O NUMERO DE DIAS DE DADOS DE VENTO FORNECIDOS DEVE SER IGUAL C
C     AO NUMERO DE DIAS DA SIMULACAO NO MODO PROGNOSTICO                       C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      include 'comblk2d.h'
cccccccccccccccccccccccccccccccc
c     DECLARACAO DAS VARIAVEIS c
cccccccccccccccccccccccccccccccc
      integer j
      real*8 TIME_CONT
      real*8 TIME_VENTO(2),uvento_forc(2,IM),vvento_forc(2,IM)
      real*8 uwind(IM),vwind(IM)
      real*8 dtinterp,dventou,dventov
      real*8 beta,rwa,spd,cd

C     TEMPO DESDE A TROCA PARA O MODO PROGNOSTICO (TEMPO DO MODELO - TEMPO NO QUAL TROCOU DO DIAGNOSTICO PARA O PROGNOSTICO)
      TIME_CONT=TIME-DIA
C     ITERACAO DO MODELO NA QUAL COMECA O MODO PROGNOSTICO
      CONT0=int(DIA/(DTI/86400))+1

c     LACO PARA A PRIMEIRA ITERACAO DO MODELO NO MODO PROGNOSTICO
      IF (IINT.EQ.CONT0) THEN

      TIME_CONT=0
c     le as 2 primeiras linhas do arquivo de vento --> TIME_VENTO em DIAS <--
        open(373,FILE='vento_simu_v2001.dat')
        read(373,*) TIME_VENTO(1),uvento_forc(1,:),vvento_forc(1,:)
        read(373,*) TIME_VENTO(2),uvento_forc(2,:),vvento_forc(2,:)

c     armazena o PRIMEIRO instante de vento nas variaveis uwind e vwind
        do j=1,IM
          uwind(j)=uvento_forc(1,j)
          vwind(j)=vvento_forc(1,j)
        end do

       print*,"--PRIMEIRO PASSO DE TEMPO DO MODO PROGNOSTICO--"
       print*,TIME_VENTO(:),'troca'
       print*,uvento_forc(1,2),'-->',uvento_forc(2,2),uwind(2)

C     PARA DE LER O ARQUIVO QUANDO CHEGA NA ULTIMA ITERACAO DO MODELO
      ELSEIF (IINT.EQ.IEND) THEN

c     armazena o ULTIMO instante de vento nas variaveis uwind e vwind
        do j=1,IM
          uwind(j)=uvento_forc(2,j)
          vwind(j)=vvento_forc(2,j)
        end do

        print*,"--ULTIMO PASSO DE TEMPO--"
        print*,uvento_forc(1,2),'-->',uvento_forc(2,2),uwind(2)
        print*,IINT,IEND,TIME

c     LACO PARA CARREGAR O PROXIMO PAR DE DADOS DE VENTO
c     so' se nao estiver na ultima iteracao
      ELSEIF (TIME_CONT.GT.TIME_VENTO(2)) THEN

c     ATUALIZA OS VETORES uvento_forc e vvento_forc (substitui o dado do tempo superior para o tempo inferior)
        do j=1,IM
          uvento_forc(1,j)=uvento_forc(2,j)
          vvento_forc(1,j)=vvento_forc(2,j)
          TIME_VENTO(1)=TIME_VENTO(2)
        end do
c     LE O PROXIMO DADO DE VENTO
        read(373,*) TIME_VENTO(2),uvento_forc(2,:),vvento_forc(2,:)

      dtinterp=(TIME_CONT-TIME_VENTO(1))/(TIME_VENTO(2)-TIME_VENTO(1))
      do j=1,IM
        dventou=(uvento_forc(2,j)-uvento_forc(1,j))*dtinterp
        dventov=(vvento_forc(2,j)-vvento_forc(1,j))*dtinterp
        uwind(j)=uvento_forc(1,j)+dventou
        vwind(j)=vvento_forc(1,j)+dventov
      end do

      print*,TIME_VENTO(:),'troca'
      print*,uvento_forc(1,2),'-->',uvento_forc(2,2),uwind(2)
c     INTERPOLACAO DOS DADOS DE VENTO PARA OS TEMPOS 'TIME' DO MODELO QUE NAO BATEM COM OS TEMPOS DOS DADOS DE VENTO
c     funciona ate' a iteracao na qual TIME==TIME_VENTO(2)
      ELSE

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
c     --> (INTERPOLACAO NO TEMPO PROPRIAMENTE DITA - teorema de Tales) <--
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      dtinterp=(TIME_CONT-TIME_VENTO(1))/(TIME_VENTO(2)-TIME_VENTO(1))
      do j=1,IM
        dventou=(uvento_forc(2,j)-uvento_forc(1,j))*dtinterp
        dventov=(vvento_forc(2,j)-vvento_forc(1,j))*dtinterp
        uwind(j)=uvento_forc(1,j)+dventou
        vwind(j)=vvento_forc(1,j)+dventov
      end do

      END IF

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     CALCULANDO TENSAO DE CISALHAMENTO DO VENTO (ATUALIZANDO WUSURF E WVSURF)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     --- RHOair/RHOwater
        rwa=1.226e-3/1.025
        do j=1,IM
C         --- transfer coordinates to model grid direction
c         O BETA refere-se ao angulo entre o referencial local do grid
c         curvilineo para o eixo E verdadeiro, no sentido anti-horario.
c         para este caso, o angulo beta e' um so', pois estamos trabalhando com uma grade bidimensional, ao longo dos eixos x-z (secao vertical).
c         no caso original, beta=beta(i,j) porque a grade era curvilinear, ao longo dos eixos x-y, e cada vetor em cada posicao
c         precisa ser rotacionado de um angulo diferente
c         beta foi calculado com a rotina sw_dist do matlab
c         beta esta' em graus, converter para radianos (beta*pi/180)
          beta=-67.3873*3.14159265358979/180
C         IMPORTANTE: NAS 2 LINHAS ABAIXO, USA-SE AS VARIAVEIS WUSURF E WVSURF POR CONVENIENCIA, MAS NA REALIDADE ELAS DENOTAM
C         AS COMPONENTES u E v DO VENTO ROTACIONADAS PARA A SECAO MODELADA. NAS LINHAS SEGUINTES, WUSURF E WVSURF SERAO CONVERTIDAS
C         EFETIVAMENTE PARA AS COMPONENTES DA TENSAO DE CISALHAMENTO ROTACIONADAS PARA O SISTEMA DE COORDENADAS DO MODELO.
          WUSURF(j)=uwind(j)*cos(beta)-vwind(j)*sin(beta)
          WVSURF(j)=uwind(j)*sin(beta)+vwind(j)*cos(beta)
C         --- calculate wind stress in model units from wind velocity
C         --- note: sign opposite to wind vectors
          spd=sqrt(uwind(j)**2+vwind(j)**2)
          cd=(7.5E-4+6.7E-5*spd)
          WUSURF(j)=-1.*rwa*cd*spd*WUSURF(j)
          WVSURF(j)=-1.*rwa*cd*spd*WUSURF(j)
        end do

C
C SALVANDO DADOS INTERPOLADOS PARA PLOTAR NO MATLAB
C
      print*,'salvando vento DIA: ',TIME_CONT
      do j=1,IM
        open(668,FILE='../vento_interp_v2001.dat',status='unknown')
        write(668,*)TIME_CONT,uwind(j),vwind(j)
      end do

      end
