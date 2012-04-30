      SUBROUTINE CALC
C***********************************************************************
C             CALC Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Controls Flow and Processing of CALCulation Modules
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED: Added check for runtime error (RUNERR) before
C                  continuing with source loop.
C                  R. W. Brode, U.S. EPA, OAQPS, AQMG, 10/19/2009
C
C                  Moved METHDR assignment statement from SUB. PCALC
C                  to beginning of source loop.
C                  R. W. Brode, MACTEC (f/k/a PES), Inc., 12/28/05
C
C        INPUTS:  Arrays of Source Parameters
C                 Arrays of Receptor Locations
C                 Meteorological Variables for One Hour
C
C        OUTPUTS: Array of 1-hr CONC or DEPOS Values for Each
C                 Source/Receptor
C
C        CALLED FROM:   HRLOOP
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'CALC'
      PATH = 'CN'

C     Assing METHDR = .TRUE. to print source&receptor-independent
C     meteorology debug information to METEOR debug output file.
      METHDR = .TRUE.

C     Begin Source LOOP
      SOURCE_LOOP: DO ISRC = 1, NUMSRC
         IF (SRCTYP(ISRC)(1:5) .EQ. 'POINT') THEN
C           Calculate Point Source Values                   ---   CALL PCALC
            CALL PCALC

         ELSE IF (SRCTYP(ISRC) .EQ. 'VOLUME') THEN
C           Calculate Volume Source Values                  ---   CALL VCALC
            CALL VCALC

         ELSE IF (SRCTYP(ISRC)(1:4) .EQ. 'AREA') THEN
C           Calculate AREA/AREAPOLY/AREACIRC Source Values  ---   CALL ACALC
            CALL ACALC

         ELSE IF (SRCTYP(ISRC) .EQ. 'OPENPIT') THEN
C           Calculate OpenPit Source Values                 ---   CALL OCALC
            CALL OCALC

         END IF

C        Check for runtime error (RUNERR) before continuing loop
         IF (RUNERR) EXIT

      END DO SOURCE_LOOP
C     End Source LOOP

      IF (L_BACKGRND) THEN
C ---    User-specified background concentrations are included; 
C        add to modeled concentrations by source group
         CALL SUMBACK
      END IF
      
      RETURN
      END

      SUBROUTINE PCALC
C***********************************************************************
C             PCALC Module of the AMS/EPA Regulatory Model - AERMOD
c ----------------------------------------------------------------------
c ---    ISC-PRIME     Version 1.0    Level 970812              Modified
c ---        D. Strimaitis, J. Scire
c ---        Earth Tech, Inc.
c            Prepared for EPRI under contract WO3527-01
c ----------------------------------------------------------------------
C
C        PURPOSE: Calculates concentration or deposition values
C                 for POINT sources
C
C        PROGRAMMER: Roger Brode and Jim Paumier, PES, Inc.
C
C        DATE:    September 30, 1993
C
C        CHANGES:
C                  Added arrays to save WDSIN and WDCOS by source for use
C                  with PVMRM option.  Corrects potential problem for 
C                  PVMRM applications with multi-level wind inputs.
C                  R.W. Brode, U.S. EPA/OAQPS/AQMG, 01/26/2007
C
C                  Added call to subroutine HEFF for calculation
C                  of ZSUBP for deposition applications.
C                  R. W. Brode, U.S. EPA, OAQPS, AQMG, 01/24/2007
C
C                  Moved METHDR assignment statement from SUB. PCALC
C                  to beginning of source loop in SUB. CALC.
C                  R. W. Brode, MACTEC (f/k/a PES), Inc., 12/28/05
C
C                  Modified for capped stack option (POINTCAP) and
C                  for multiple urban area option.
C                  R. W. Brode, MACTEC (f/k/a PES), Inc., 09/30/05
C
C                  Removed code that adjusts DHCRIT based on distance
C                  to well-mixed layer for stable conditions for
C                  consistency with Model Formulation Document.
C                  R. W. Brode, MACTEC (f/k/a PES), Inc., 09/30/05
C
C                  Modified to include initialization of __VAL arrays
C                  at end of receptor loop.
C                  R. W. Brode, MACTEC (f/k/a PES), Inc., 10/26/04
C
C                  Modified to include the PVMRM and OLM options for
C                  modeling conversion of NOx to NO2.
C                  Added debug statement based on ENSR code.
C                  R. W. Brode, MACTEC (f/k/a PES), Inc., 07/27/04
C
CRWB               Modified to call DHPSS to obtain plume centroid height
CRWB               (CENTER) for Schulman-Scire downwash cases.  Modified
CRWB               to compare XFINAL to XMIXED only for unstable cases with
CRWB               HS < ZI.  Added initialization of TGEFF and TGEFF3 as TGS.
CRWB               Additional modifications made to improve consistency with
CRWB               the implementation of Schulman-Scire downwash algorithm
CRWB               in the ISCST3 model.
CRWB               R. Brode, PES - 12/6/99
C
CRWB               Modified to use wind direction at midpoint between
CRWB               stack height and "final" plume height for transport.
CRWB               R. Brode, PES - 1/22/98
C
CRWB               Use effective parameters evaluated at stack height
CRWB               for the indirect plume, as for direct plume.  Also
CRWB               commented out calls to LOCATE and GINTRP with ZIO2.
CRWB               This change is made for the Base Case model.
CRWB               R. Brode, PES - 12/8/94
C
CRWB               Commented out calls to LOCATE and GINTRP with HTEFF in
CRWB               order to use effective parameters evaluated at stack height
CRWB               instead of HTEFF for the direct plume and the stable
CRWB               plume.  This change is made for the Base Case model.
CRWB               R. Brode, PES - 12/7/94
C
C                  Moved calculation of penetration factor from outside
C                  to inside receptor loop, and deleted code related 
C                  to indirect and penetrated plumes which is no
C                  longer needed.  (R.F. Lee, 7/13/94)
C
C                  Added true centerline concentration calculations
C                  for EVALFL output.  (R.F. Lee, 7/25/94)
C
CRJP               Changes made to calculations of effective parameters
CRJP               in conjunction with new treatment of inhomogeneity.
CRJP               (Bob Paine, 10/4/94)
C
C
C        INPUTS:  Source Parameters for Specific Source
C                 Arrays of Receptor Locations
C                 Meteorological Variables for One Hour
C
C        OUTPUTS: 1-hr CONC or DEPOS Values for Each Receptor for
C                 Particular Source
C
C        CALLED FROM:   CALC
C
C        Assumptions:
C
C        References:  "A Dispersion Model for the Convective Boundary
C                      Layer", J. Weil, 8/17/93
C                     "Inhomogeneous Boundary Layer", A. Venkatram,
C                      6/25/93
C
C***********************************************************************
C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      INTEGER :: I, KITER, NDXZMID, NDXZPL
      INTEGER :: NDXBH
      DOUBLE PRECISION :: HSPRIM, ZPLM, DHFOLD, SVPM, UPM, TGPM, 
     &                    PTPM, PTP, ZMID
      DOUBLE PRECISION :: VALABV, VBELOW
      DOUBLE PRECISION :: USTACK, UBLDG, XBREC, YBREC
      DOUBLE PRECISION :: AERPLM(NUMTYP), AERPAN(NUMTYP), FRAN, FRAN3
      DOUBLE PRECISION :: VSEQ
      LOGICAL :: L_PLUME

      logical :: ldbhr
      
C     Variable Initializations
      MODNAM = 'PCALC'

C     Initialize __VAL arrays (1:NUMTYP)
      HRVAL   = 0.0D0
      AERVAL  = 0.0D0
      AERPLM  = 0.0D0
      AERPAN  = 0.0D0
      PRMVAL  = 0.0D0

C     Set the Source Variables for This Source              ---   CALL SETSRC
      CALL SETSRC

C     Apply Variable Emission Rate Factors                  ---   CALL EMFACT
      CALL EMFACT(QS)

      IF (QTK .NE. 0.0D0) THEN

C        Set Mixing Height and Profiles for Urban Option if Needed
         IF (URBSRC(ISRC) .EQ. 'Y') THEN
C           Find Urban Area Index for This Source
            DO I = 1, NUMURB
               IF (IURBGRP(ISRC,I) .EQ. 1) THEN
                  IURB = I
                  EXIT
               END IF
            END DO
            IF (STABLE .OR. L_MorningTrans(IURB)) THEN
               URBSTAB = .TRUE.
               ZI = MAX( ZIURB(IURB), ZIMECH )
               GRIDSV = GRDSVU(1:MXGLVL,IURB)
               GRIDSW = GRDSWU(1:MXGLVL,IURB)
               GRIDTG = GRDTGU(1:MXGLVL,IURB)
               GRIDPT = GRDPTU(1:MXGLVL,IURB)
               OBULEN = DABS( URBOBULEN(IURB) )
               USTAR  = URBUSTR(IURB)
            ELSE
               URBSTAB = .FALSE.
               ZI = ZIRUR
               GRIDSV = GRDSVR
               GRIDSW = GRDSWR
               GRIDTG = GRDTGR
               GRIDPT = GRDPTR
               OBULEN = RUROBULEN
               USTAR  = RURUSTR
            END IF
         ELSE IF (URBAN .AND. URBSRC(ISRC) .EQ. 'N') THEN
            URBSTAB = .FALSE.
            ZI = ZIRUR
            GRIDSV = GRDSVR
            GRIDSW = GRDSWR
            GRIDTG = GRDTGR
            GRIDPT = GRDPTR
            OBULEN = RUROBULEN
            USTAR  = RURUSTR
         ELSE
            URBSTAB = .FALSE.
         END IF

C        Calculate the initial meteorological variables     ---   CALL METINI
         CALL METINI

C        Calculate Buoyancy and Momentum Fluxes             ---   CALL FLUXES
         CALL FLUXES(VSEQ)

C        Set Wake and Building Type Switches                ---   CALL WAKFLG
C ---    NOTE:  WAKFLG sets building dimensions based on wind
C        direction at stack top.
         CALL WAKFLG

C        Define temporary values of CENTER and SURFAC based on HS
         CENTER = HS
         IF( CENTER .LT. 0.1D0*ZI )THEN
            SURFAC = .TRUE.
         ELSE
            SURFAC = .FALSE.
         END IF

C        Check for stack-tip downwash option and adjust if necessary
         IF( NOSTD )THEN
C           No stack-tip downwash, no adjustments necessary
            HSP = HS
         ELSE IF (SRCTYP(ISRC) .EQ. 'POINTCAP') THEN
C           Apply stack-tip downwash for capped stacks with VS = 0.001m/s
            HSP = HSPRIM ( US, VSEQ, HS, DS )
         ELSE IF (SRCTYP(ISRC) .EQ. 'POINTHOR') THEN
C           Do not apply stack-tip downwash for horizontal releases
            HSP = HS
         ELSE
C           Make adjustments for stack-tip downwash
            HSP = HSPRIM ( US, VS, HS, DS )
         END IF

C        Calculate Distance to Final Rise                   ---   CALL DISTF
         CALL DISTF

C        Calculate the plume penetration factor             ---   CALL PENFCT
         CALL PENFCT


         IF(DEBUG) THEN
            WRITE(DBGUNT,6000) DHFAER, UP, TGS
6000        FORMAT(/,5X,'INITIAL PLUME RISE ESTIMATE:  DELH = ',
     &          F6.1,' M; Uplume = ',F5.2,' M/S; DTHDZ = ',
     &          F7.4,' DEG K/M')
         END IF

         IF (STABLE.OR.(UNSTAB.AND.(HS.GE.ZI))) THEN
C           Use iterative approach to stable plume rise calculations
            KITER = 0
50          ZPLM = HSP + 0.5D0 * DHFAER
            DHFOLD = DHFAER

C----       Locate index below ZPLM

            CALL LOCATE(GRIDHT, 1, MXGLVL, ZPLM, NDXZPL)

C----       Get Wind speed at ZPLM; replace UP.  Also, replace TGP,
C           vertical potential temperature gradient, if stable.

            CALL GINTRP( GRIDHT(NDXZPL), GRIDSV(NDXZPL),
     &           GRIDHT(NDXZPL+1), GRIDSV(NDXZPL+1), ZPLM, SVPM )
            CALL GINTRP( GRIDHT(NDXZPL), GRIDWS(NDXZPL),
     &           GRIDHT(NDXZPL+1), GRIDWS(NDXZPL+1), ZPLM, UPM )
            SVPM = MAX( SVPM, SVMIN, 0.05D0*UPM )
            UPM  = DSQRT( UPM*UPM + 2.0D0*SVPM*SVPM )
CRWB        Use average of stack top and midpoint wind speeds.
            UP = 0.5D0 * (US + UPM)

            CALL GINTRP( GRIDHT(NDXZPL), GRIDTG(NDXZPL),
     &           GRIDHT(NDXZPL+1), GRIDTG(NDXZPL+1), ZPLM, TGPM )
            CALL GINTRP( GRIDHT(NDXZPL), GRIDPT(NDXZPL),
     &           GRIDHT(NDXZPL+1), GRIDPT(NDXZPL+1), ZPLM, PTPM )
CRWB        Use average of stack top and midpoint temperature gradients.
            TGP = 0.5D0 * (TGS + TGPM)
            PTP = 0.5D0 * (PTS + PTPM)
            BVF = DSQRT( G * TGP / PTP )
            IF(BVF .LT. 1.0D-10) BVF = 1.0D-10
            BVPRIM  = 0.7D0 * BVF

            CALL DISTF

            KITER = KITER + 1

CRJP        Add temporary debugging statements

            IF(DEBUG) THEN
               WRITE(DBGUNT,6001) KITER,DHFOLD, DHFAER, ZPLM, UP,TGP
6001           FORMAT(/,5X,'OPTH2 ITER. #',I1,': OLD DELH = ',
     &          F6.1,' M; NEW DELH = ',F6.1,' M; MET LEVEL = ',
     &          F6.1,' M; NEW Upl = ',F5.2,' M/S; NEW DTHDZ = ',
     &          F7.4,' K/M')
            END IF
            
C           Check for convergence            
            IF(DABS((DHFOLD - DHFAER)/DHFAER) .LT. 0.01D0) GO TO 60
            
            IF(KITER .GE. 5) THEN
               DHFAER = 0.5D0 * (DHFAER + DHFOLD)
               IF(DEBUG) WRITE(DBGUNT,6002) DHFAER
6002          FORMAT(/,5X,'OPTH2 ITERATION FAILED TO CONVERGE; PLUME',
     &         ' RISE SET AT ',F6.1,' METERS.',/)
               GO TO 60
            ELSE
               GO TO 50
            END IF

60          CONTINUE

CRWB        After completing iteration, reset UP and TGP to stack top
CRWB        values for subsequent distance-dependent plume rise calcs.
            UP = US
            TGP = TGS
            PTP = PTS
            BVF = DSQRT( G * TGP / PTP )
            IF(BVF .LT. 1.0D-10) BVF = 1.0D-10
            BVPRIM  = 0.7D0 * BVF
         END IF

C        Initialize PRM_FSTREC Logical Switch for First Receptor of Loop;
         PRM_FSTREC = .TRUE.

C        Initialize 'ARC' Arrays for EVALFILE Output        ---   CALL EVLINI
         IF (EVAL(ISRC)) THEN
            CALL EVLINI
         END IF

         ZMIDMX = 0.5D0 * ZI

CRJP     Add temporary debugging statement.

         IF( DEBUG) THEN
             WRITE(DBGUNT,6010) KURDAT, ZMIDMX
6010         FORMAT(/,72('*'),//,5X,'YYMMDDHH: ',I8,//,
     &                5X,'Height assigned to midpoint of ',
     &         'well-mixed layer for effective parameters = ',
     &         F6.1,' meters.',/)
         END IF
CRJP
CRJP     Calculate distance to uniformly mixed plume within the
CRJP     boundary layer (XMIXED) after Turner's Workbook (1970), page 7:
CRJP     distance is approximately (Zi * UAVG)/SWAVG, where UAVG
CRJP     and SWAVG are wind speed and sigma-w averaged over the depth
CRJP     between the ground and Zi (or the plume height, if higher in
CRJP     stable conditions); this height is denoted as 2 * ZMIDMX.
CRJP
CRJP     First, get refined estimate of final rise and distance to final
CRJP     rise if downwash conditions prevail.
CRJP
         XFINAL = XMAX
         DHCRIT = DHFAER
         XMIXED = ZI * UAVG / SWAVG
         IF (UNSTAB .AND. HS.LT.ZI) THEN
C           Check for XMIXED smaller than 1.25*XFINAL
            IF (XMIXED .LT. 1.25D0*XFINAL) THEN
               XFINAL = 0.8D0 * XMIXED
               CALL CBLPRD (XFINAL)
               DHCRIT = DHP1
            END IF
         END IF


C ---    Initialize PDF parameters for use in calculating ZSUBP
         IF( UNSTAB  .AND.  (HS .LT. ZI) ) THEN
            CALL PDF
         END IF
C        Set Dry Deposition Variables for this Source
         IF (LUSERVD .AND. LDGAS .AND. NPD.EQ.0) THEN
C           Assign user-specified gas dry deposition velocity (GASDEPVD option)
            VDEPG = USERVD
         ELSE IF (LDPART .OR. (.NOT.LUSERVD .AND. LDGAS .AND. 
     &                                                   NPD.EQ.0)) THEN
C           Calculate Deposition Velocities for this Source    ---   CALL VDP
            CALL VDP
         END IF
         IF (LWPART .OR. LWGAS) THEN
CPES        Set value of ZSUBP = MAX( ZI, TOP OF PLUME ), where
CPES        TOP OF PLUME is defined as plume height (HE) plus 2.15*SZ,
CPES        evaluated at a distance of 20 kilometers downwind.
CPES        Apply minimum value of 500m and maximum value of 10,000m.
            CALL HEFF (20000.0D0)
            IF( STABLE .OR. (UNSTAB .AND. HS .GE. ZI) )THEN
               HE = HSP + DHCRIT
               CALL SIGZ(20000.0D0)
               ZSUBP = MAX( 500.0D0, ZI, HE + SZCOEF*SZAS )
            ELSE IF (UNSTAB) THEN
               HED1 = HSP + DHCRIT
               IF (PPF .GT. 0.0D0) THEN
                  CALL CBLPR3
               END IF
               CALL SIGZ(20000.0D0)

               IF (PPF .EQ. 0.0D0) THEN
                  ZSUBP=MAX( 500.0D0, ZI, HED1 + SZCOEF*(SZAD1+SZAD2)/
     &                  2.0D0 )
               ELSE IF (PPF .EQ. 1.0D0) THEN
                  ZSUBP=MAX( 500.0D0, ZI, HE3 + SZCOEF*SZA3)
               ELSE
                  ZSUBP=MAX( 500.0D0, ZI, PPF*(HE3+SZCOEF*SZA3) + 
     &                (1.0D0-PPF)*(HED1 + SZCOEF*(SZAD1+SZAD2)/2.0D0) )
               END IF
            END IF
            ZSUBP = MIN( 10000.0D0, ZSUBP )
C           Calculate Scavenging Ratios for this Source           ---   CALL SCAVRAT
            CALL SCAVRAT
         END IF

CRWB     Determine transport wind direction using midpoint between
CRWB     stack height and "final" plume height.  R. Brode, PES, 1/22/98
C----    Define ZMID=midpoint between stack height and "final" plume height
         ZMID = MIN( 4000.0D0, (HS + 0.5D0*DHFAER) )

C----    Locate index below ZMID
         CALL LOCATE(GRIDHT, 1, MXGLVL, ZMID, NDXZMID)

C----    Extract WD for grid levels above and below ZMID
         VALABV = GRIDWD(NDXZMID+1)
         VBELOW = GRIDWD(NDXZMID)

C----    Check for 360 crossover and adjust if necessary
         IF( (VALABV-VBELOW) .LT. -180.0D0) THEN
            VALABV = VALABV + 360.0D0
         ELSE IF( (VALABV-VBELOW) .GT. 180.0D0) THEN
            VALABV = VALABV - 360.0D0
         END IF

C----    Assign Wind direction
         IF (VBELOW .EQ. VALABV) THEN
            WDIR = VBELOW
         ELSE
C----       Interpolate to ZMID
            CALL GINTRP( GRIDHT(NDXZMID), VBELOW,
     &                   GRIDHT(NDXZMID+1), VALABV,
     &                   ZMID, WDIR )
         END IF

C        Check for WDIR > 360 or < 0
         IF (WDIR .GT. 360.0D0) THEN
            WDIR = WDIR - 360.0D0
         ELSE IF (WDIR .LE. 0.0D0) THEN
            WDIR = WDIR + 360.0D0
         END IF
C
C----    Convert direction to radians, compute sine and cosine of direction,
C        and determine nearest 10-degree sector.
C
C---->   wind direction = wind direction in degrees * DTORAD

         WDSIN = DSIN(WDIR * DTORAD)
         WDCOS = DCOS(WDIR * DTORAD)

C ---    Save WDSIN and WSCOS for later use by PVMRM option
         AWDSIN(ISRC) = WDSIN
         AWDCOS(ISRC) = WDCOS

         AFV = WDIR - 180.0D0
         IF (AFV .LT. 0.0D0) THEN
            AFV = AFV + 360.0D0
         END IF
         IFVSEC = IDINT (AFV*0.10D0 + 0.4999D0)
         IF (IFVSEC .EQ. 0) IFVSEC = 36

c
c --- PRIME ---------------------------------------------------------
c ---    Setup computations for numerical plume rise algorithm
c ---    and building wake analysis
         if(WAKE) then
c ---       Store selected data in new variables for future reference
            ustack=us

c ---       Compute wind speed at top of building           ---   CALL WSADJ
c ---       Locate index below building height
            CALL LOCATE(GRIDHT, 1, MXGLVL, DSBH, NDXBH)

            CALL GINTRP( GRIDHT(NDXBH), GRIDWS(NDXBH),
     &           GRIDHT(NDXBH+1), GRIDWS(NDXBH+1), DSBH, UBLDG )

c ---       Refresh /WAKEDAT/ variables                     ---   CALL WAKE_INI
            ldbhr=DEBUG
c ---       Note that logical RURAL has no impact on calculations
            rural = .true.
            call WAKE_INI(ldbhr,rural,dsbh,dsbw,dsbl,
     &                    xadj,yadj,ubldg,ustack)
         end if
c ------------------------------------------------------------

CRJP     Add temporary debugging statement.

         IF(DEBUG) THEN
            WRITE(DBGUNT, 6011) DHCRIT, XFINAL, XMIXED
6011        FORMAT(5X,'For effective parameter calculations: ',
     &        '"Final" plume rise = ',G14.6, ' m; Distance to final ',
     &        'rise = ',G14.6,' m',/,5x,'Distance to well-mixed ',
     &        'state = ',G14.6,' m.',/)
CRJP
CRJP        Make call to PSRDEB
CRJP
            CALL PSRDEB

         END IF
C
C        Begin Receptor LOOP *******************************************
CCRFL
CCRFL    Add logical variable METHDR, which is set to TRUE at the start
CCRFL    of the receptor loop, and reset to false after the headers and
CCRFL    non-receptor dependent meteorological variables in the
CCRFL    meteorological debug file are printed.  METHDR is also added
CCRFL    to MAIN.INC and METEXT.FOR (Subroutine METDEB).  9/27/94, R.F. Lee.
CCRFL
CCRWB    METHDR assignment statement moved to SUB. CALC.  12/28/05, R.W. Brode.

         RECEPTOR_LOOP: DO IREC = 1, NUMREC
C           Calculate Down and Crosswind Distances          ---   CALL XYDIST
            IF (EVONLY) THEN
               CALL XYDIST(IEVENT)
            ELSE
               CALL XYDIST(IREC)
            END IF

C ---       Calculate AERMOD Concentration Without Downwash, AERVAL
C ---       First calculate coherent plume component using downwind distance
            L_PLUME = .TRUE.
C ---       Assign XDIST for use in dry depletion (FUNCTION F2INT)
            XDIST = X
            CALL AERCALC( X, L_PLUME, AERPLM )

            IF (L_EFFSIGY) THEN
C ---          No "pancake" calculation for non-DFAULT FASTALL option (EFFSIGY)
               AERVAL = AERPLM
               AERPAN = 0.0D0
               
            ELSE

C ---          Next calculate random "pancake" component using radial distance
               L_PLUME = .FALSE.
C ---          Assign XDIST for use in dry depletion (FUNCTION F2INT)
               XDIST = DISTR
               CALL AERCALC( DISTR, L_PLUME, AERPAN )

C ---          Calculate fraction of random kinetic energy to total kinetic energy.
C              Note that these effective parameters are based on the radial dist.
               IF (STABLE .OR. (UNSTAB.AND.(HS.GE.ZI))) THEN
                  CALL MEANDR( UEFF, SVEFF, FRAN )
               ELSE IF (UNSTAB) THEN
                  CALL MEANDR( UEFFD, SVEFFD, FRAN )
                  IF (PPF .GT. 0.0D0) THEN
C                    For penetrated source calculate weighted average of
C                    direct/indirect plume component and penetrated component
                     CALL MEANDR( UEFF3, SVEFF3, FRAN3 )
                     FRAN = PPF*FRAN3 + (1.0D0-PPF)*FRAN
                  END IF
               END IF

C ---          Combine coherent plume and random "pancake" components (1:NUMTYP)
               AERVAL = FRAN*AERPAN + (1.0D0-FRAN)*AERPLM

            END IF
            
C           ENSR STATEMENT
            IF(DEBUG) THEN
               DO ITYP = 1, NUMTYP
                  WRITE(DBGUNT,10) AERPAN(ITYP), AERPLM(ITYP), FRAN,
     &                             AERVAL(ITYP)
10                FORMAT(/,'AERVAL(ITYP) = FRAN*AERPAN(ITYP) + (1.-FRAN)
     &                                               *AERPLM(ITYP)',//,
     &                    'PANCAKE/MEANDER COMPONENT, AERPAN(ITYP) = ',
     &            G16.8,/,'COHERENT PLUME COMPONENT,  AERPLM(ITYP) = ',
     &            G16.8,/,'MEANDER FACTOR, FRAN = ',
     &            G16.8,/,'RESULTANT CONC, AERVAL(ITYP) = ',G16.8,//)
               END DO
            END IF

            IF (WAKE .AND. (STABLE .OR. HS.LE.ZI)) THEN
c ---          Calculate receptor coordinates relative to upwind face of bldg.:
c              xbrec is downwind dist. of receptor from upwind
c              bldg face; ybrec is crosswind dist. of receptor from
c              center of upwind bldg. face
               xbrec=x-xadj
               ybrec=y-yadj

               XDIST = X
C ---          Calculate PRIME Downwash Concentration, PRMVAL
               CALL PRMCALC ( XBREC, YBREC )
C ---          Check for runtime error (RUNERR) before continuing
               IF (RUNERR) RETURN

C ---          Calculate Gamma weighting factor, GAMFACT
               CALL GAMCALC ( XBREC, YBREC )

C ---          Calculate hourly concentration from PRIME and AERMOD values (1:NUMTYP)
               HRVAL =  GAMFACT  * PRMVAL + (1.0D0-GAMFACT) * AERVAL

               if (debug) then
                  DO ITYP = 1, NUMTYP
                    write(iounit,*) 'YYMMDDHH:  ',kurdat,' ISRC: ',isrc,
     &                              ' IREC: ',irec
                    write(iounit,*) ' GAMFACT = ',GAMFACT
                    write(iounit,*) ' AERVAL  = ',AERVAL(ITYP)
                    write(iounit,*) ' PRMVAL  = ',PRMVAL(ITYP)
                    write(iounit,*) ' HRVAL   = ',HRVAL(ITYP)
                    write(iounit,*) ' '
                  END DO
               end if

            ELSE
C ---          No WAKE effects or HS > ZI, set GAMFACT to 0.0 and use AERVAL only.
               GAMFACT = 0.0D0
C ---          Calculate hourly concentration from PRIME and AERMOD values (1:NUMTYP)
               HRVAL  = AERVAL
               PRMVAL = 0.0D0

            END IF

            IF (PVMRM .AND. .NOT. O3MISS) THEN
C ---          Store data by source and receptor for PVMRM option
               DO ITYP = 1, NUMTYP
                  CHI(IREC,ISRC,ITYP) = HRVAL(ITYP)
               END DO
               IF (STABLE .OR. (UNSTAB.AND.(HS.GE.ZI))) THEN
                  HECNTR(IREC,ISRC) = HE
                  UEFFS(IREC,ISRC)  = UEFF
               ELSE
                  HECNTR(IREC,ISRC) = CENTER
                  UEFFS(IREC,ISRC)  = UEFFD
               END IF
               IF (PPF .GT. 0.0D0) THEN
                  HECNTR3(IREC,ISRC) = HE3
                  PPFACT(ISRC)       = PPF
                  UEFF3S(IREC,ISRC)  = UEFF3
               ELSE
                  PPFACT(ISRC) = 0.0D0
               END IF
               FOPTS(IREC,ISRC) = FOPT

C              Initialize __VAL arrays (1:NUMTYP)
               HRVAL   = 0.0D0
               AERVAL  = 0.0D0
               AERPLM  = 0.0D0
               AERPAN  = 0.0D0
               PRMVAL  = 0.0D0

C              Cycle to next receptor & skip call to SUMVAL (will be done later)
               CYCLE RECEPTOR_LOOP
            ELSE IF (OLM .AND. .NOT. O3MISS) THEN
C ---          Store data by source and receptor for OLM option
               DO ITYP = 1, NUMTYP
                  CHI(IREC,ISRC,ITYP) = HRVAL(ITYP)
               END DO

C              Initialize __VAL arrays (1:NUMTYP)
               HRVAL   = 0.0D0
               AERVAL  = 0.0D0
               AERPLM  = 0.0D0
               AERPAN  = 0.0D0
               PRMVAL  = 0.0D0

C              Cycle to next receptor & skip call to SUMVAL (will be done later)
               CYCLE RECEPTOR_LOOP
            END IF

C           Sum HRVAL to AVEVAL and ANNVAL Arrays  ---   CALL SUMVAL
C           As noted just above, the IF block and initialization of
C           the _VAL arrays is skipped for a PVMRM or OLM run
C           The summing is performed in PVMRM_CALC and OLM_CALC
            IF (EVONLY) THEN
               CALL EV_SUMVAL
            ELSE
               CALL SUMVAL
            END IF
            IF (EVAL(ISRC)) THEN
C              Check ARC centerline values for EVALFILE
C              output                              ---   CALL EVALCK
               CALL EVALCK
            END IF

         END DO RECEPTOR_LOOP
C        End Receptor LOOP

C        Output 'ARC' Values for EVALFILE                   ---   CALL EVALFL
         IF (EVAL(ISRC)) THEN
            CALL EVALFL
         END IF

      END IF

      RETURN
      END


      SUBROUTINE AERCALC( XARG, L_PLUME, AEROUT )
C***********************************************************************
C             AERCALC Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Calculates the AERMOD concentration without downwash
C
C        PROGRAMMER: Roger Brode, PES, Inc.
C
C        DATE:     November 10, 2000
C
C        CHANGES:
C                  Added debug statement based on ENSR code.
C                  R. W. Brode, MACTEC (f/k/a PES), Inc., 07/27/04
C
C        INPUTS:   XARG         - Real - Distance (m), downwind for coherent
C                                        plume component and radial for
C                                        random component
C                  L_PLUME      - Log  - Specifies coherent plume calculation
C                                        if TRUE, otherwise random component
C
C        OUTPUTS:  AEROUT(NTYP) - Real - AERMOD component of concentration
C                                        without building downwash for either
C                                        coherent plume component or for
C                                        random component, depending on
C                                        L_PLUME.
C
C        CALLED FROM:   PCALC
C
C***********************************************************************
C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      INTEGER :: J
      DOUBLE PRECISION :: AEROUT(NUMTYP), AERTMP(NUMTYP), FYOUT, XARG, 
     &                    ADJ, FRAN, FRAN3, SYEFF
      LOGICAL :: L_PLUME

C     Variable Initializations
      MODNAM = 'AERCALC'

C     Initialize AEROUT(NUMTYP) and AERTMP(NUMTYP) arrays
      AEROUT = 0.0D0
      AERTMP = 0.0D0

      IF (XARG .LT. 1.0D0) THEN
C        Receptor Too Close to Source for Calculation
         AEROUT = 0.0D0

      ELSE IF (DISTR .GT. MAXDIST) THEN
C        Maximum distance from source; assigned
C        value of 80km for obsolescent TOXICS option
C        or new FASTALL option; otherwise "unlimited" (1.0D20)
         AEROUT = 0.0D0

      ELSE

C        Determine Deposition Correction Factors
         IF (NPD .EQ. 0 .AND. (LDGAS .OR. LWGAS)) THEN
            CALL PDEPG (XARG)
         ELSE
            DQCORG = 1.0D0
            WQCORG = 1.0D0
         END IF
         IF (NPD .GT. 0 .AND. (LDPART .OR. LWPART)) THEN
            CALL PDEP (XARG)
         ELSE IF (NPD .GT. 0) THEN
C           Set DQCOR(NPD) and WQCOR(NPD) arrays to 1.0                  
            DQCOR = 1.0D0
            WQCOR = 1.0D0
         END IF

C        Set initial effective parameters
         UEFF  = US
         SVEFF = SVS
         SWEFF = SWS
         TGEFF = TGS
         IF ( UNSTAB  .AND.  (HS .LT. ZI) ) THEN
            UEFFD  = US
            SVEFFD = SVS
            SWEFFD = SWS
            UEFFN  = US
            SVEFFN = SVS
            SWEFFN = SWS
            UEFF3  = US
            SVEFF3 = SVS
            SWEFF3 = SWS
            TGEFF3 = TGS
         END IF

CRJP     Add temporary debugging statement here.

C        ENSR ENHANCEMENT OF WRITE STATEMENT TO IDENTIFY COMPONENT CONCENTRATION
         IF(DEBUG) THEN
           IF(L_PLUME)THEN
              WRITE(DBGUNT, 6014) SRCID(ISRC)
6014          FORMAT(//,'SRCID: ', A8)
              WRITE(DBGUNT, 6015) UEFF, SVEFF, SWEFF
6015          FORMAT(//,'COHERENT PLUME COMPONENT',/,5X,
     &          'Initial effective parameters for ',
     &          'stable or direct convective ',
     &          'plume:',//,5x,'Ueff = ',F7.2,' m/s; ',
     &          'SVeff = ',F7.2,
     &          ' m/s; SWeff = ',F7.2,' m/s.',/)
           ELSE
              WRITE(DBGUNT, 6016) UEFF, SVEFF, SWEFF
6016          FORMAT(//,'MEANDER COMPONENT',/,5X,
     &          'Initial effective parameters for ',
     &          'stable or direct convective ',
     &          'plume:',//,5x,'Ueff = ',F7.2,' m/s; ',
     &          'SVeff = ',F7.2,
     &          ' m/s; SWeff = ',F7.2,' m/s.',/)
           END IF
         END IF

C        Define plume centroid height (CENTER) for use in
C        inhomogeneity calculations
         CALL CENTROID ( XARG )

C        Calculate the plume rise                     ---   CALL DELTAH
         CALL DELTAH ( XARG )

C        If the atmosphere is unstable and the stack
C        top is below the mixing height, calculate
C        the CBL PDF coefficients                     ---   CALL PDF
         IF( UNSTAB  .AND.  (HS .LT. ZI) ) THEN
            CALL PDF
         END IF

C        Determine Effective Plume Height             ---   CALL HEFF
         CALL HEFF ( XARG )

C        Compute effective parameters using an
C        average through plume layer
         CALL IBLVAL ( XARG )

C        Call PDF & HEFF again for final CBL plume heights
         IF (UNSTAB .AND. (HS.LT.ZI) ) THEN
            CALL PDF
            CALL HEFF ( XARG )
         END IF

C        Determine Dispersion Parameters              ---   CALL PDIS
         CALL PDIS ( XARG )

C        Calculate the 'y-term' contribution to
C        dispersion, FSUBY
         IF (L_PLUME) THEN
            IF (L_EFFSIGY) THEN
C ---          Calculate fraction of random kinetic energy to total kinetic energy
C              for FASTALL option to optimize meander using effective sigma-y.
C              Note that these effective parameters are based on the downwind distance,
C              rather than the radial distance used in the standard meander approach
               IF (STABLE .OR. (UNSTAB.AND.(HS.GE.ZI))) THEN
                  CALL MEANDR( UEFF, SVEFF, FRAN )
               ELSE IF (UNSTAB) THEN
                  CALL MEANDR( UEFFD, SVEFFD, FRAN )
                  IF (PPF .GT. 0.0D0) THEN
C                    For penetrated source calculate weighted average of
C                    direct/indirect plume component and penetrated component
                     CALL MEANDR( UEFF3, SVEFF3, FRAN3 )
                     FRAN = PPF*FRAN3 + (1.0D0-PPF)*FRAN
                  END IF
               END IF

C              Calculate effective sigma-y for non-DFAULT FASTALL option (EFFSIGY)
               SYEFF = 1.0D0/((FRAN/(SRT2PI*XARG)) + (1.0D0-FRAN)/SY)
               IF (DABS(Y) .GT. NUMSYEFF*SYEFF) THEN
C                 Plume is more than 6 sigmas off centerline, skip calculation
                  FYOUT = 0.0D0
               ELSE
C                 Calculate FSUBY for coherent plume        ---   CALL FYPLM
                  CALL FYPLM(SYEFF,FYOUT)
               END IF
            ELSE
C              Calculate FSUBY for coherent plume        ---   CALL FYPLM
               CALL FYPLM(SY,FYOUT)
            END IF
         ELSE
C           Calculate FSUBY for random component      ---   CALL FYPAN
            CALL FYPAN(FYOUT)
         END IF
         FSUBY  = FYOUT
         FSUBYD = FSUBY
         FSUBYN = FSUBYD

C        Calculate the 'y-term' contribution to dispersion
C        for the penetrated plume, FSUBY3
         IF( UNSTAB  .AND.  (HS .LT. ZI)  .AND.
     &       (PPF .GT. 0.0D0) )THEN
C           Compute meander fraction of horizontal distribution function
C           from Venky's memo of 6/24/98.
            IF (L_PLUME) THEN
               IF (L_EFFSIGY) THEN
C                 Calculate effective sigma-y for non-DFAULT FASTALL option (EFFSIGY)
                  SYEFF = 1.0D0/((FRAN/(SRT2PI*XARG))+ (1.0D0-FRAN)/SY3)
                  IF (DABS(Y) .GT. NUMSYEFF*SYEFF) THEN
C                    Plume is more than 6 sigmas off centerline, skip calculation
                     FYOUT = 0.0D0
                  ELSE
C                    Calculate FSUBY for coherent plume        ---   CALL FYPLM
                     CALL FYPLM(SYEFF,FYOUT)
                  END IF
               ELSE
C                 Calculate FSUBY for coherent plume        ---   CALL FYPLM
                  CALL FYPLM(SY3,FYOUT)
               END IF
            ELSE
C              Calculate FSUBY for random component   ---   CALL FYPAN
               CALL FYPAN(FYOUT)
            END IF
            FSUBY3 = FYOUT
         ELSE
            FSUBY3 = 0.0D0
         END IF

C        Check for zero "y-terms"; if zero then skip calculations
C        and go to next receptor.
         IF( FSUBY.EQ.0.0D0 .AND. FSUBY3.EQ.0.0D0 )THEN
C           Set AEROUT(NUMTYP) array to 0.0         
            AEROUT = 0.0D0

         ELSE

            IF (NPD .EQ. 0) THEN
C              Perform calculations for gases
C              Assign plume tilt, HV = 0.0
               HV = 0.0D0

               ADJ = DQCORG * WQCORG

               IF (STABLE .OR. (UNSTAB.AND.(HS.GE.ZI))) THEN
C                 Calculate height of the "effective reflecting surface"
                  CALL REFL_HT (HE, XARG, SZB, 0.0D0, HSBL)
               ELSEIF ( UNSTAB ) THEN
                  HSBL = 0.0D0
               END IF

               IF (UNSTAB .AND. (HS.LT.ZI) .AND. (PPF.GT.0.0D0)) THEN
C                 Calculate height of the "effective reflecting surface"
                  CALL REFL_HT (HE3, XARG, SZB3, 0.0D0, HPEN)
               ELSE
                  HPEN = 0.0D0
               END IF

C              Determine the CRITical Dividing Streamline---   CALL CRITDS
               CALL CRITDS (HE)

C              Calculate the fraction of plume below
C              HCRIT, PHEE                               ---   CALL PFRACT
               CALL PFRACT (HE)

C              Calculate FOPT = f(PHEE)                  ---   CALL FTERM
               CALL FTERM

C              Calculate AERMOD Concentration     ---   CALL AER_PCHI
               CALL AER_PCHI( XARG, ADJ, VDEPG, 0, AEROUT )

            ELSE
C              Perform calculations for particles, loop through particle sizes

C              Begin loop over particle sizes
               DO J = 1, NPD

C                 Calculate Plume Tilt Due to Settling, HV
                  HV = (XARG/US) * VGRAV(J)

C                 Adjust Jth contribution by mass fraction and source
C                 depletion
                  ADJ = PHI(J) * DQCOR(J) * WQCOR(J)

                  IF (STABLE .OR. (UNSTAB.AND.(HS.GE.ZI))) THEN
C                    Calculate height of the "effective reflecting surface"
C                    Calculate Settled Plume Height(s), HESETL
                     HESETL = MAX( 0.0D0, HE - HV )
                     CALL REFL_HT (HESETL, XARG, SZB, 0.0D0, HSBL)
                  ELSEIF ( UNSTAB ) THEN
                     HESETL = MAX( 0.0D0, 0.5D0*(HED1+HED2) - HV )
                     HSBL = 0.0D0
                  END IF

                  IF (UNSTAB .AND. (HS.LT.ZI) .AND. (PPF.GT.0.0D0)) THEN
C                    Calculate height of the "effective reflecting surface"
C                    Calculate Settled Plume Height(s), HE3SETL
                     HE3SETL = MAX( 0.0D0, HE3 - HV )
                     CALL REFL_HT (HE3SETL, XARG, SZB3, 0.0D0, HPEN)
                     HPEN = MAX( HPEN, ZI )
                  ELSE
                     HPEN = 0.0D0
                  END IF

C                 Determine the CRITical Dividing Streamline---   CALL CRITDS
                  CALL CRITDS (HESETL)

C                 Calculate the fraction of plume below
C                 HCRIT, PHEE                               ---   CALL PFRACT
                  CALL PFRACT (HESETL)

C                 Calculate FOPT = f(PHEE)                  ---   CALL FTERM
                  CALL FTERM

C                 Calculate AERMOD Concentration            ---   CALL AER_PCHI
                  CALL AER_PCHI( XARG, ADJ, VDEP(J), J, AERTMP )
                  AEROUT = AEROUT + AERTMP

               END DO
C              End loop over particle sizes

            END IF

         END IF

      END IF

      RETURN
      END


      SUBROUTINE PRMCALC (XBREC, YBREC)
C***********************************************************************
C             PRMCALC Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Calculates the PRIME downwash component of the
C                 concentration
C
C        PROGRAMMER: Roger Brode, PES, Inc.
C
C        DATE:     November 10, 2000
C
C        MODIFIED:
C                  Modified to place receptor on centerline of cavity
C                  plumes by setting Y2 = 0.0 for SCREEN option.
C                  R. W. Brode, MACTEC (f/k/a PES), Inc., 10/26/04
C
C        INPUTS:  XBREC - Real    - Downwind distance (m) of receptor
C                                   from upwind edge of building
C                 YBREC - Real    - Lateral distance (m) of receptor from
C                                   center of upwind edge of building
C
C        OUTPUTS: PRMVAL(NTYP) - Real - PRIME downwash component of
C                                       concentration
C
C        CALLED FROM:   PCALC
C
C***********************************************************************
C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      
      CHARACTER MODNAM*12
      INTEGER :: IPOSITN, N1, N2, IS, J
      DOUBLE PRECISION :: ADJ
      DOUBLE PRECISION :: DHPOUT, SYOUT, SZOUT, FYOUT
      DOUBLE PRECISION :: XBREC, YBREC, FQCAV, SYCAV, 
     &                    SZCAV
      DOUBLE PRECISION :: ZHI, ZLO
      INTEGER :: NDXBHI, NDXBLO, NDXALO
c --- Declare local PRIME arrays for "3-source" data
      DOUBLE PRECISION :: q2(3),y2(3),sy2(3),z2(3),h2(3),sz2(3),qc2(3),
     &                    qtksav,ppfsav

      logical :: L_INWAKE

C     Variable Initializations
      MODNAM = 'PRMCALC'

c --- PRIME ---------------------------------------------------------
c --- Calculate where receptor is relative to near-wake cavity
c     and building (IPOSITN=1 for within bldg; 2=within
c     near-wake, 3=within far wake; 4=outside)
c --- Note:  xbrec is downwind dist. of receptor from upwind
c     bldg face; ybrec is crosswind dist. of receptor from
c     center of upwind bldg. face                  ---  CALL POSITION
      call POSITION(xbrec,ybrec,zflag,ipositn)

      if(ipositn.EQ.4 .AND. X.LE.0.0D0) then
c ---    Receptor is upwind of sources and is not within
c ---    a building wake - use AERMOD calculation
c ---    Set PRMVAL(NUMTYP) array = AERVAL(NUMTYP) array
         PRMVAL = AERVAL

      elseif(ipositn.NE.2 .AND. distr .LT. 0.99D0) then
c ---    Receptor Too Close to Source for Calculation and is not
c ---    within a building near-wake (cavity) - use AERMOD calculation
c ---    Set PRMVAL(NUMTYP) array = AERVAL(NUMTYP) array
         PRMVAL = AERVAL
c -------------------------------------------------------------

      ELSE IF (DISTR .GT. MAXDIST) THEN
C        Maximum distance from source; assigned
C        value of 80km for obsolescent TOXICS option
C        or new FASTALL option; otherwise "unlimited" (1.0D20)
C ---    Receptor is beyond MAXDIST - use AERMOD calculation
c ---    Set PRMVAL(NUMTYP) array = AERVAL(NUMTYP) array
         PRMVAL = AERVAL

      ELSE IF (.NOT. WAKE) THEN
C ---    No wake effects for this source for this hour - use AERMOD calculation
c ---    Set PRMVAL(NUMTYP) array = AERVAL(NUMTYP) array
         PRMVAL = AERVAL

      ELSE
C ---    Calculate PRIME concentration with downwash

C ---    Calculate effective parameters to define ambient turbulence intensities,
C        as averages across layer from ground to top of wake (as calculated at
C        a downwind distance of 15R).
         ZHI = 1.2D0*RSCALE * (15.0D0 +
     &                  (DSBH/(1.2D0*RSCALE))**3)**THIRD
         IF (UNSTAB) THEN
            ZHI = MIN( ZHI, ZI )
         END IF
         ZLO = 0.0D0

         CALL LOCATE(GRIDHT, 1, MXGLVL, ZHI, NDXBHI)
         CALL LOCATE(GRIDHT, 1, MXGLVL, ZLO, NDXBLO)
         NDXALO = NDXBLO + 1
         CALL ANYAVG ( MXGLVL, GRIDHT, GRIDWS, ZLO,NDXALO,
     &      ZHI,NDXBHI,UEFF )
         CALL ANYAVG ( MXGLVL, GRIDHT, GRIDSV, ZLO,NDXALO,
     &      ZHI,NDXBHI,SVEFF )
         CALL ANYAVG ( MXGLVL, GRIDHT, GRIDSW, ZLO,NDXALO,
     &      ZHI,NDXBHI,SWEFF )
         CALL ANYAVG ( MXGLVL, GRIDHT, GRIDTG, ZLO,NDXALO,
     &      ZHI,NDXBHI,TGEFF )

CRWB     Modify treatment of low wind/low turbulence cases.
CRWB     R. Brode, PES, 8/15/96
         SWEFF = MAX( SWEFF, SWMIN )
         SVEFF = MAX( SVEFF, SVMIN, 0.05D0*UEFF )
         UEFF  = DSQRT( UEFF*UEFF + 2.0D0*SVEFF*SVEFF )

         if (debug) then
            write(iounit,*) 'PRIME Effective Parameters: '
            write(iounit,*) 'ZLO, ZHI     = ', zlo, zhi
            write(iounit,*) 'SWEFF, SVEFF = ', sweff, sveff
            write(iounit,*) 'UEFF,  TGEFF = ', ueff, tgeff
         end if

C        Calculate the plume rise                     ---   CALL PRMDELH
         CALL PRMDELH ( X, L_INWAKE )
         
C ---    Check for runtime error (RUNERR) before continuing
         IF (RUNERR) RETURN

         IF (.NOT. L_INWAKE) THEN
C           Plume is not affected by wake, set PRMVAL = AERVAL and return
            PRMVAL = AERVAL
            RETURN
         END IF

C        Determine Effective Plume Height             ---   CALL PRMHEFF
         CALL PRMHEFF

         IF (UNSTAB .AND. HE .GE. ZI) THEN
C           Plume is above ZI, set PRMVAL = AERVAL and return
            PRMVAL = AERVAL
            RETURN
         END IF

c ---    Calculate sigmas
         dhpout = dhp
         call WAKE_XSIG(x,hs,dhpout,nobid,szout,syout,
     &                  szcav,sycav)
         sy = syout
         sz = szout

c ---    PRIME ---------------------------------------------------
c ---    When there is a building wake, consider treatment of mass in
c ---    cavity as additional sources, or as only source
         qtksav = qtk
         ppfsav = ppf
c ---    Place selected plume data into transfer arrays (first element)
         q2(1)  = qtk
         y2(1)  = y
         sy2(1) = sy
         z2(1)  = zflag
         h2(1)  = he
         sz2(1) = sz
         n1 = 1
         n2 = 1
         if(WAKE) then
c ---       Define cavity source                              ---   CALL CAV_SRC
            call CAV_SRC(x,y,zflag,fqcav,qc2,h2,y2,z2,sz2,sy2,n1,n2)
            if (SCREEN) then
c ---          Force receptor to be on "centerline" for all plumes for SCREEN
               y2 = 0.0D0
            end if
            if(fqcav.GT.0.0D0) then
c ---          Set source strengths
               q2(1)=qtk*(1.0D0-fqcav)
               q2(2)=qtk*fqcav*qc2(2)
               q2(3)=qtk*fqcav*qc2(3)
            end if
         end if

c ---    Initialize PRMVAL(NUMTYP) output array values to zero, because contributions
c ---    due to more than one source are summed here (or do loop may
c ---    not execute if neither source contributes)
         PRMVAL = 0.0D0

c ---    Loop over 3 possible sources (is=1 for primary source,
c ---    is=2 for "outside" cavity source, and is=3 for "inside" cavity source)
         do is = n1, n2

c ---       Cycle to next source if emission rate is 0.0
            if (q2(is) .eq. 0.0D0) cycle

c ---       Transfer data for current source
            qtk = q2(is)
            y   = y2(is)
            sy  = sy2(is)
            sz  = sz2(is)
            he  = h2(is)
            zflag = z2(is)

c -------------------------------------------------------------
C           Calculate the 'y-term' contribution to
C           dispersion, FSUBY                              ---   CALL FYPLM
            CALL FYPLM(SY,FYOUT)
            FSUBY  = FYOUT

            IF( FSUBY.EQ.0.0D0 )THEN
C ---          Lateral term is 0.0, set PRMVAL array (1:NUMTYP) to 0.0.
               PRMVAL = 0.0D0

            ELSE

C ---          Set FOPT = 0.5 for PRIME calculation since wake is "near neutral"
               FOPT = 0.5D0

               IF (NPD .EQ. 0) THEN
C                 Determine Deposition Correction Factors
                  IF ((LDGAS.OR.LWGAS) .AND. IS.NE.3 .AND. 
     &                                       X.GT.1.0D0) THEN
C                    Do not apply depletion for "inside cavity source", IS=3
                     CALL PRM_PDEPG (X)

C                    Reassign plume height and sigmas, which may have changed
C                    during integration
                     sy  = sy2(is)
                     sz  = sz2(is)
                     he  = h2(is)
                  ELSE
                     DQCORG = 1.0D0
                     WQCORG = 1.0D0
                  END IF

                  ADJ = DQCORG * WQCORG

                  CALL PRM_PCHI( ADJ, VDEPG, 0 )

               ELSE
                  IF ((LDPART.OR.LWPART) .AND. IS.NE.3 .AND.
     &                                                 X.GT.1.0D0) THEN
C                    Do not apply depletion for "inside cavity source", IS=3
                     CALL PRM_PDEP (X)

C                    Reassign plume height and sigmas, which may have changed
C                    during integration
                     sy  = sy2(is)
                     sz  = sz2(is)
                     he  = h2(is)
                  ELSE                  
C                    Set DQCOR(NPD) and WQCOR(NPD) arrays to 1.0                  
                     DQCOR = 1.0D0
                     WQCOR = 1.0D0
                  END IF

                  DO J = 1, NPD

                     ADJ = PHI(J) * DQCOR(J) * WQCOR(J)
                     HV  = (X/US) * VGRAV(J)
                     HE  = MAX ( 0.0D0, HE - HV )

                     CALL PRM_PCHI( ADJ, VDEP(J), J )

                  END DO
               END IF

            END IF

         END DO

c ---    Restore original plume data
         QTK = QTKSAV
         PPF = PPFSAV
         y   = y2(1)
         sy  = sy2(1)
         sz  = sz2(1)
         he  = h2(1)
         zflag = z2(1)

      END IF

      RETURN
      END


      SUBROUTINE GAMCALC ( XARG, YARG )
C***********************************************************************
C             GAMCALC Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Calculates the Gamma weighting factor to combine
C                 AERMOD and PRIME concentrations
C
C        PROGRAMMER: Roger Brode, PES, Inc.
C
C        DATE:     July 19, 2001
C
C        INPUTS:   XARG - Real - Downwind distance (m) of receptor
C                                from upwind edge of building
C                  YARG - Real - Lateral distance (m) of receptor from
C                                center of upwind edge of building
C
C        OUTPUTS:  GAMFACT - Real - Gamma weighting factor to combine
C                                   AERMOD and PRIME concentrations
C
C        CALLED FROM:   PCALC
C
C***********************************************************************
C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      DOUBLE PRECISION :: XARG, YARG
      DOUBLE PRECISION :: WAKE_LEN, WAKE_WID, WAKE_HGT
      DOUBLE PRECISION :: XAY, XAZ, XAMX
      DOUBLE PRECISION :: SIGMA_XG,  SIGMA_YG,  SIGMA_ZG,
     &                    EXPARG_XG, EXPARG_YG, EXPARG_ZG

C --- Variable Initializations
      MODNAM = 'GAMCALC'

      IF (XARG .LE. 0.0D0 .OR. .NOT.WAKE) THEN
C ---    Receptor is upwind of building or no WAKE, set GAMFACT = 0.0 to
C ---    use AERVAL only.
         GAMFACT = 0.0D0

      ELSE

C ---    Calculate the height, half-width and "length" of the wake.
C ---    Length of wake is measured from upwind edge of building.
         WAKE_HGT = 1.2D0*RSCALE * (XARG/RSCALE +
     &              (DSBH/(1.2D0*RSCALE))**3)**THIRD
         WAKE_WID = 0.5D0*DSBW + (RSCALE/3.0D0)*
     &                           (XARG/RSCALE)**THIRD
C ---    Obtain distance to transition from wake to ambient turbulence,
C ---    without cap at 15R.
         call WAKE_XA2(DSBL,RSCALE,xaz,xay)
         XAMX = MAX(XAZ,XAY)
C ---    Set WAKE_LEN as maximum of 15R and transition distance
         WAKE_LEN = MAX(15.0D0 * RSCALE, XAMX)

C ---    Assign wake dimensions to SIGMA_?G terms
         SIGMA_XG = WAKE_LEN
         SIGMA_YG = WAKE_WID
         SIGMA_ZG = WAKE_HGT

C ---    Calculate exponential argument for alongwind dimension
         IF (XARG .LE. SIGMA_XG) THEN
            EXPARG_XG = 0.0D0
         ELSE
            EXPARG_XG = -((XARG-SIGMA_XG)**2 / (2.0D0 * SIGMA_XG**2))
         END IF

C ---    Calculate exponential argument for crosswind dimension
         IF (DABS(YARG) .LE. SIGMA_YG) THEN
            EXPARG_YG = 0.0D0
         ELSE
            EXPARG_YG = -((DABS(YARG)-SIGMA_YG)**2 / 
     &                      (2.0D0 * SIGMA_YG**2))
         END IF

C ---    Calculate exponential argument for vertical dimension, using ZRT,
C ---    height of receptor above stack base, including terrain and flagpole.
         IF (ZRT .LE. SIGMA_ZG) THEN
            EXPARG_ZG = 0.0D0
         ELSE
            EXPARG_ZG = -((ZRT-SIGMA_ZG)**2 / (2.0D0 * SIGMA_ZG**2))
         END IF

C        Calculate gamma weighting factor, GAMFACT
         IF (EXPARG_XG.GT.EXPLIM .AND. EXPARG_YG.GT.EXPLIM .AND.
     &                                 EXPARG_ZG.GT.EXPLIM) THEN
            GAMFACT = DEXP(EXPARG_XG)*DEXP(EXPARG_YG)*DEXP(EXPARG_ZG)
         ELSE
            GAMFACT = 0.0D0
         END IF

      END IF

      RETURN
      END


      SUBROUTINE CENTROID (XARG)
C***********************************************************************
C             CENTROID Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Calculates the plume centroid height, and sets the
C                 SURFAC logical variable
C
C        PROGRAMMER: Roger Brode, PES, Inc.
C
C        DATE:     November 10, 2000
C
C        INPUTS:  Downwind distance, XARG (m)
C
C        OUTPUTS: Plume centroid height, CENTER
C                 Surface logical variable, SURFAC
C
C        CALLED FROM:   PCALC, VCALC, ACALC, PLUMEF
C
C***********************************************************************
C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      DOUBLE PRECISION :: XARG, DELX, DELZ, FRAC, xtmp

C     Variable Initializations
      MODNAM = 'CENTROID'

      xtmp = max( xarg, 1.0D0 )

      IF( UNSTAB  .AND.  (HS .LT. ZI) ) THEN
C----    Calculate plume centerline height without PDF adjustments
         IF (SRCTYP(ISRC)(1:5) .EQ. 'POINT') THEN
            CALL CBLPRD ( xtmp )
            HTEFF = MIN (HSP+DHP1, ZI)
         ELSE
            HTEFF = HSP
         END IF
         IF( xtmp .LT. XFINAL) THEN
            CENTER = HTEFF
         ELSE IF( xtmp .GE. XMIXED) THEN
            CENTER = ZMIDMX
         ELSE
            DELX = XMIXED - XFINAL
            DELZ = ZMIDMX - MIN( HSP+DHCRIT, ZI)
            FRAC = (xtmp-XFINAL)/DELX
            CENTER = MIN(HSP+DHCRIT,ZI) + FRAC * DELZ
         END IF

C----    Determine if this is a surface layer release
         IF( CENTER .LT. 0.1D0*ZI )THEN
            SURFAC = .TRUE.

         ELSE
            SURFAC = .FALSE.
         END IF

      ELSE IF( UNSTAB .AND. (HS .GE. ZI) )THEN
         SURFAC = .FALSE.
         CENTER = HSP

      ELSE
C----    Assign centroid height to release height
         CENTER = HSP

      END IF

      RETURN
      END


      SUBROUTINE REFL_HT (HEARG, XARG, SZBARG, VSIGZARG, HEREFL)
C***********************************************************************
C             REFL_HT Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Calculates height of the "effective reflecting surface"
C                 for stable plumes, including penetrated source for
C                 point sources.
C
C        PROGRAMMER: Roger Brode, PES, Inc.
C
C        DATE:     November 10, 2000
C
C        INPUTS:  Height of plume (m),                HEARG
C                 Downwind distance (m),              XARG
C                 Bouyancy induced dispersion (m),    SZBARG
C                 Virtual source dispersion term (m), VSIGZARG
C
C        OUTPUTS: Effective height of the reflecting surface (m), HEREFL
C
C        CALLED FROM:   PCALC, VCALC, ACALC, PLUMEF
C
C***********************************************************************
C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      INTEGER :: NDXHE
      DOUBLE PRECISION :: HEARG, HEREFL, XARG, SZBARG, VSIGZARG,
     &           SVREFL, SWREFL, UREFL, TGREFL, PTREFL,
     &           TTRAVL, BVFRQ, ZTMP, SZREFL, SZRF, SIGF, SZHSBL

C     Variable Initializations
      MODNAM = 'REFL_HT'

C---- Compute the height of the "effective reflecting surface"
C---- for stable plumes, HEREFL
C---- First locate index below HEARG
      CALL LOCATE(GRIDHT, 1, MXGLVL, HEARG, NDXHE)

      IF (NDXHE .GE. 1) THEN

C---- Sigma_V at HEARG
         CALL GINTRP( GRIDHT(NDXHE), GRIDSV(NDXHE),
     &                GRIDHT(NDXHE+1), GRIDSV(NDXHE+1),
     &                HEARG, SVREFL )

C---- Sigma_W at HEARG
         CALL GINTRP( GRIDHT(NDXHE), GRIDSW(NDXHE),
     &                GRIDHT(NDXHE+1), GRIDSW(NDXHE+1),
     &                HEARG, SWREFL )

C---- Wind speed at HEARG
         CALL GINTRP( GRIDHT(NDXHE), GRIDWS(NDXHE),
     &                GRIDHT(NDXHE+1), GRIDWS(NDXHE+1),
     &                HEARG, UREFL )

C---- Temperature gradient at HEARG
         CALL GINTRP( GRIDHT(NDXHE), GRIDTG(NDXHE),
     &                GRIDHT(NDXHE+1), GRIDTG(NDXHE+1),
     &                HEARG, TGREFL )

C---- Potential temperature at HEARG
         CALL GINTRP( GRIDHT(NDXHE), GRIDPT(NDXHE),
     &                GRIDHT(NDXHE+1), GRIDPT(NDXHE+1),
     &                HEARG, PTREFL )

      ELSE
         SVREFL = GRIDSV(1)
         SWREFL = GRIDSW(1)
         UREFL  = GRIDWS(1)
         TGREFL = GRIDTG(1)
         PTREFL = GRIDPT(1)
      END IF

C---- Apply minimum wind speed and turbulence checks to values
C     at HEARG
C
      SWREFL = MAX( SWREFL, SWMIN )
      SVREFL = MAX( SVREFL, SVMIN, 0.05D0*UREFL )
      UREFL  = DSQRT( UREFL*UREFL + 2.0D0*SVREFL*SVREFL )

C     Compute surface sigma-z term for stable conditions
      IF (STABLE) THEN
         SZSURF = (RTOF2/RTOFPI) * USTAR * (XARG/UREFL) *
     &            (1.0D0 + 0.7D0*XARG/OBULEN)**(-1.0D0*THIRD)
      ELSE
         SZSURF = 0.0D0
      END IF

C     Compute ambient sigma-z term at HEARG
      TTRAVL = XARG / UREFL
C---- Apply Sigma-Z formulation from CTDMPLUS

      BVFRQ = DSQRT( G * TGREFL / PTREFL )
      IF(BVFRQ .LT. 1.0D-10) BVFRQ = 1.0D-10

C     Set height for sigma-z calculation, ZTMP
      ZTMP = MAX( HS, HEARG, 1.0D-4 )
      SZREFL = SWREFL * TTRAVL /
     &  DSQRT( 1.0D0 + SWREFL*TTRAVL * ( 1.0D0/(0.72D0*ZTMP) +
     &  BVFRQ/(0.54D0*SWREFL) ) )

      IF (HEARG .GE. ZI) THEN
         SZRF = SZREFL
      ELSE
         SIGF = MIN( HEARG/ZI, 1.0D0 )
         SZRF = (1.0D0 - SIGF) * SZSURF + SIGF * SZREFL
      END IF

C     Calculate sigma-z at plume height, SZHSBL
      SZHSBL = DSQRT( SZBARG*SZBARG + SZRF*SZRF + VSIGZARG*VSIGZARG)

C     Compute height of effective reflecting surface, HEREFL
      HEREFL = MAX( ZI, HEARG + 2.15D0*SZHSBL )

C     For urban/stable plumes below ZI, set HEREFL = ZI.
      IF (URBSTAB .AND. HEARG .LT. ZI) HEREFL = ZI

      RETURN
      END


      SUBROUTINE PSRDEB
C***********************************************************************
C             PSRDEB Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Outputs point source information for debugging
C                 purposes
C
C        PROGRAMMER: Bob Paine.  Implemented by Russ Lee.
C
C        DATE:    August 18, 1994
C
C        INPUTS:  Source Parameters for Specific Source, including
C                 those calculated in PCALC
c
CRJP              DHCRIT = "Final" plume rise (m)
C
C        OUTPUTS: Debugging information for a specific source.
C
C        CALLED FROM:   CALC
C
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'PSRDEB'

      WRITE (DBGUNT, 6130) KURDAT

      WRITE (DBGUNT, 6135) ISRC, QS, TS, VS, DS, FB, FM, HS, DHCRIT
      WRITE (DBGUNT, 6140) HS,WDIR, US, UP, SVS, SWS, TGS

 6130 FORMAT(20('===='),//,' yymmddhh:  ', I8,
     &  //,8X,'<----------------- SOURCE INFORMATION ---------',
     1  '------> FINAL PLUME',/,
     2  ' SOURCE   QS    TS      VS     DS   BUOY FLUX  MOM FLUX  ',
     3  ' HS      RISE',/,
     4  '   #    (G/S)   (K)    (M/S)   (M)   (M4/S3)    (M4/S2)  ',
     5  ' (M)      (M)',/)
 6135 FORMAT(I4,F9.1,F7.1,F7.2,F7.2,F10.1,2X,F9.1,F6.1,F9.1,
     1  //,2X,'VARIABLES AT ',T21,'HEIGHT   WDIR   ',
     2  'USCAL  URISE  SIGV   SIGW   DTHDZ ',
     3  /,2X,'STACK HEIGHT:',T21,
     3  '  (M)    (DEG)  (M/S)  (M/S)  (M/S)  (M/S) (DEG/M)',/)
 6140 FORMAT(19X,F7.1,F7.0,F8.2,2F7.2,F7.2,F8.4,/)

      RETURN
      END


      SUBROUTINE VCALC
C***********************************************************************
C        VCALC Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Calculates concentration or deposition values
C                 for VOLUME sources
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:
C                  Added arrays to save WDSIN and WDCOS by source for use
C                  with PVMRM option.  Corrects potential problem for 
C                  PVMRM applications with multi-level wind inputs.
C                  R.W. Brode, U.S. EPA/OAQPS/AQMG, 01/26/2007
C
C                  Added call to subroutine HEFF for calculation
C                  of ZSUBP for deposition applications.
C                  R. W. Brode, U.S. EPA, OAQPS, AQMG, 01/24/2007
C
C                  Modified to include initialization of __VAL arrays
C                  at end of receptor loop.
C                  R. W. Brode, MACTEC (f/k/a PES), Inc., 10/26/04
C
C                  Modified to include the PVMRM and OLM options for
C                  modeling conversion of NOx to NO2.
C                  Added debug statement based on ENSR code.
C                  R. W. Brode, MACTEC (f/k/a PES), Inc., 07/27/04
C
C                  To assign values to XDIST before calls to
C                  SUBROUTINE VOLCALC.
C                  R. W. Brode, MACTEC (f/k/a PES), Inc., 03/19/04
C
C        INPUTS:  Source Parameters for Specific Source
C                 Arrays of Receptor Locations
C                 Meteorological Variables for One Hour
C
C        OUTPUTS: 1-hr CONC or DEPOS Values for Each Receptor for
C                 Particular Source
C
C        CALLED FROM:   CALC
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      INTEGER :: I
      DOUBLE PRECISION :: AERPLM(NUMTYP), AERPAN(NUMTYP), FRAN
      LOGICAL :: L_PLUME

C     Variable Initializations
      MODNAM = 'VCALC'
      WAKE = .FALSE.

C     Initialize __VAL arrays (1:NUMTYP)
      HRVAL   = 0.0D0
      AERVAL  = 0.0D0
      AERPLM  = 0.0D0
      AERPAN  = 0.0D0

C     Set the Source Variables for This Source              ---   CALL SETSRC
      CALL SETSRC

C     Apply Variable Emission Rate Factors                  ---   CALL EMFACT
      CALL EMFACT(QS)

C     Initialize 'ARC' Arrays for EVALFILE Output           ---   CALL EVLINI
      IF (EVAL(ISRC)) THEN
         CALL EVLINI
      END IF

      IF (QTK .NE. 0.0D0) THEN

C        Set Mixing Height and Profiles for Urban Option if Needed
         IF (URBSRC(ISRC) .EQ. 'Y') THEN
C           Find Urban Area Index for This Source
            DO I = 1, NUMURB
               IF (IURBGRP(ISRC,I) .EQ. 1) THEN
                  IURB = I
                  EXIT
               END IF
            END DO
            IF (STABLE .OR. L_MorningTrans(IURB)) THEN
               URBSTAB = .TRUE.
               ZI = MAX( ZIURB(IURB), ZIMECH )
               GRIDSV = GRDSVU(1:MXGLVL,IURB)
               GRIDSW = GRDSWU(1:MXGLVL,IURB)
               GRIDTG = GRDTGU(1:MXGLVL,IURB)
               GRIDPT = GRDPTU(1:MXGLVL,IURB)
               OBULEN = DABS( URBOBULEN(IURB) )
               USTAR  = URBUSTR(IURB)
            ELSE
               URBSTAB = .FALSE.
               ZI = ZIRUR
               GRIDSV = GRDSVR
               GRIDSW = GRDSWR
               GRIDTG = GRDTGR
               GRIDPT = GRDPTR
               OBULEN = RUROBULEN
               USTAR  = RURUSTR
            END IF
         ELSE IF (URBAN .AND. URBSRC(ISRC) .EQ. 'N') THEN
            URBSTAB = .FALSE.
            ZI = ZIRUR
            GRIDSV = GRDSVR
            GRIDSW = GRDSWR
            GRIDTG = GRDTGR
            GRIDPT = GRDPTR
            OBULEN = RUROBULEN
            USTAR  = RURUSTR
         ELSE
            URBSTAB = .FALSE.
         END IF

C        Initialize meteorological variables                ---   CALL METINI
         CALL METINI

C        Save WDSIN and WSCOS for later use by PVMRM option
         AWDSIN(ISRC) = WDSIN
         AWDCOS(ISRC) = WDCOS

C        Initialize miscellaneous variables
         FB  = 0.0D0
         FM  = 0.0D0
         PPF = 0.0D0
         HSP = HS
         DHP  = 0.0D0
         DHP1 = 0.0D0
         DHP2 = 0.0D0
         DHP3 = 0.0D0
         DHCRIT = 0.0D0
         XFINAL = 0.0D0
         XMIXED = ZI * UAVG / SWAVG
         IF(XMIXED .LT. XFINAL) XMIXED = XFINAL
         ZMIDMX = 0.5D0 * ZI

C        Calculate Effective Radius
         XRAD = 2.15D0*SYINIT

C ---    Initialize PDF parameters for use in calculating ZSUBP
         IF( UNSTAB  .AND.  (HS .LT. ZI) ) THEN
            CALL PDF
         END IF
C        Set Dry Deposition Variables for this Source
         IF (LUSERVD .AND. LDGAS .AND. NPD.EQ.0) THEN
C           Assign user-specified gas dry deposition velocity (GASDEPVD option)
            VDEPG = USERVD
         ELSE IF (LDPART .OR. (.NOT.LUSERVD .AND. LDGAS .AND. 
     &                                                   NPD.EQ.0)) THEN
C           Calculate Deposition Velocities for this Source    ---   CALL VDP
            CALL VDP
         END IF
         IF (LWPART .OR. LWGAS) THEN
CPES        Set value of ZSUBP = MAX( ZI, TOP OF PLUME ), where
CPES        TOP OF PLUME is defined as plume height (HE) plus 2.15*SZ,
CPES        evaluated at a distance of 20 kilometers downwind.
CPES        Apply minimum value of 500m and maximum value of 10,000m.
            CALL HEFF (20000.0D0)
            IF( STABLE .OR. (UNSTAB .AND. HS .GE. ZI) )THEN
               CALL SIGZ(20000.0D0)
               ZSUBP = MAX( 500.0D0, ZI, HS + SZCOEF*SZAS )
            ELSE IF (UNSTAB) THEN
               CALL SIGZ(20000.0D0)
               ZSUBP = MAX( 500.0D0, ZI, HS + 
     &                                   SZCOEF*(SZAD1+SZAD2)/2.0D0 )
            END IF
            ZSUBP = MIN( 10000.0D0, ZSUBP )
C           Calculate Scavenging Ratios for this Source           ---   CALL SCAVRAT
            CALL SCAVRAT
         END IF

C        Begin Receptor LOOP
         RECEPTOR_LOOP: DO IREC = 1, NUMREC
C           Calculate Down and Crosswind Distances          ---   CALL XYDIST
            IF (EVONLY) THEN
               CALL XYDIST(IEVENT)
            ELSE
               CALL XYDIST(IREC)
            END IF

C ---       First calculate coherent plume component using downwind distance
            L_PLUME = .TRUE.
C ---       Assign XDIST for use in dry depletion (FUNCTION F2INT)
            XDIST = X
            CALL VOLCALC( X, L_PLUME, AERPLM )

            IF (L_EFFSIGY) THEN
C ---          No "pancake" calculation for non-DFAULT FASTALL option (EFFSIGY)
               HRVAL  = AERPLM
               AERPAN = 0.0D0
               
            ELSE

C ---          Next calculate random "pancake" component using radial distance
               L_PLUME = .FALSE.
C ---          Assign XDIST for use in dry depletion (FUNCTION F2INT)
               XDIST = DISTR
               CALL VOLCALC( DISTR, L_PLUME, AERPAN )

C ---          Calculate fraction of random kinetic energy to total kinetic energy.
C              Note that these effective parameters are based on the radial dist.
               IF (STABLE .OR. (UNSTAB.AND.(HS.GE.ZI))) THEN
                  CALL MEANDR( UEFF, SVEFF, FRAN )
               ELSE IF (UNSTAB) THEN
                  CALL MEANDR( UEFFD, SVEFFD, FRAN )
               END IF

C ---          Combine coherent plume and random "pancake" components (1:NUMTYP)
               HRVAL = FRAN*AERPAN + (1.0D0-FRAN)*AERPLM

            END IF

C           ENSR STATEMENT
            IF(DEBUG) THEN
               DO ITYP = 1, NUMTYP
                  WRITE(DBGUNT,10) AERPAN(ITYP), AERPLM(ITYP), FRAN,
     &                             HRVAL(ITYP)
10               FORMAT(/,'HRVAL(ITYP) = FRAN*AERPAN(ITYP) + (1.-FRAN)',
     &                                               '*AERPLM(ITYP)',//,
     &                    'PANCAKE/MEANDER COMPONENT, AERPAN(ITYP) = ',
     &            G16.8,/,'COHERENT PLUME COMPONENT,  AERPLM(ITYP) = ',
     &            G16.8,/,'MEANDER FACTOR, FRAN = ',
     &            G16.8,/,'RESULTANT CONC, HRVAL(ITYP) = ',G16.8,//)
               END DO
            END IF

            IF (PVMRM .AND. .NOT.O3MISS) THEN
C ---          Store data by source and receptor for PVMRM option
               DO ITYP = 1, NUMTYP
                  CHI(IREC,ISRC,ITYP) = HRVAL(ITYP)
               END DO
               IF (STABLE .OR. (UNSTAB.AND.(HS.GE.ZI))) THEN
                  HECNTR(IREC,ISRC) = HE
                  UEFFS(IREC,ISRC)  = UEFF
               ELSE
                  HECNTR(IREC,ISRC) = CENTER
                  UEFFS(IREC,ISRC)  = UEFFD
               END IF
               IF (PPF .GT. 0.0D0) THEN
                  HECNTR3(IREC,ISRC) = HE3
                  PPFACT(ISRC)       = PPF
                  UEFF3S(IREC,ISRC)  = UEFF3
               END IF
               FOPTS(IREC,ISRC) = FOPT
C              Cycle to next receptor & skip call to SUMVAL (will be done later)
               CYCLE RECEPTOR_LOOP
            ELSE IF (OLM .AND. .NOT.O3MISS) THEN
C ---          Store data by source and receptor for OLM option
               DO ITYP = 1, NUMTYP
                  CHI(IREC,ISRC,ITYP) = HRVAL(ITYP)
               END DO
C              Cycle to next receptor & skip call to SUMVAL (will be done later)
               CYCLE RECEPTOR_LOOP
            END IF

            IF (EVONLY) THEN
               CALL EV_SUMVAL
            ELSE
               CALL SUMVAL
            END IF
            IF (EVAL(ISRC)) THEN
C              Check ARC centerline values for EVALFILE
C              output                              ---   CALL EVALCK
               CALL EVALCK
            END IF

C           Initialize __VAL arrays (1:NUMTYP)
            HRVAL   = 0.0D0
            AERVAL  = 0.0D0
            AERPLM  = 0.0D0
            AERPAN  = 0.0D0

         END DO RECEPTOR_LOOP
C        End Receptor LOOP

C        Output 'ARC' Values for EVALFILE                   ---   CALL EVALFL
         IF (EVAL(ISRC)) THEN
            CALL EVALFL
         END IF

      END IF

      RETURN
      END


      SUBROUTINE VOLCALC( XARG, L_PLUME, AEROUT )
C***********************************************************************
C             VOLCALC Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Calculates the AERMOD concentration without downwash
C
C        PROGRAMMER: Roger Brode, PES, Inc.
C
C        DATE:     November 10, 2000
C
C        CHANGES:
C                  Added debug statement based on ENSR code.
C                  R. W. Brode, MACTEC (f/k/a PES), Inc., 07/27/04
C
C        INPUTS:   XARG         - Real - Distance (m), downwind for coherent
C                                        plume component and radial for
C                                        random component
C                  L_PLUME      - Log  - Specifies coherent plume calculation
C                                        if TRUE, otherwise random component
C
C        OUTPUTS:  AEROUT(NTYP) - Real - AERMOD component of concentration
C                                        without building downwash for either
C                                        coherent plume component or for
C                                        random component, depending on
C                                        L_PLUME.
C
C        CALLED FROM:   VCALC
C
C***********************************************************************
C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      DOUBLE PRECISION :: AEROUT(NUMTYP), AERTMP(NUMTYP), FYOUT, XARG, 
     &                    ADJ, FRAN, SYEFF
      INTEGER :: J
      LOGICAL :: L_PLUME

C     Variable Initializations
      MODNAM = 'VOLCALC'

C     Initialize AEROUT and AERTMP arrays (1:NUMTYP)
      AEROUT = 0.0D0
      AERTMP = 0.0D0

      IF (DISTR .LT. (XRAD+0.99D0)) THEN
C        Receptor Too Close to Source for Calculation
C        Receptor is beyond 80km from source.
         AEROUT = 0.0D0
      ELSE IF ((XARG-XRAD) .LT. 0.0D0) THEN
C        Receptor Upwind of Downwind Edge
         AEROUT = 0.0D0
      ELSE IF (DISTR .GT. MAXDIST) THEN
C        Maximum distance from source; assigned
C        value of 80km for obsolescent TOXICS option
C        or new FASTALL option; otherwise "unlimited" (1.0D20)
         AEROUT = 0.0D0
      ELSE

C        Determine Deposition Correction Factors
         IF (NPD .EQ. 0 .AND. (LDGAS .OR. LWGAS)) THEN
            CALL PDEPG (XARG)
         ELSE
            DQCORG = 1.0D0
            WQCORG = 1.0D0
         END IF
         IF (NPD .GT. 0 .AND. (LDPART .OR. LWPART)) THEN
            CALL PDEP (XARG)
         ELSE IF (NPD .GT. 0) THEN
C           Set DQCOR(NPD) and WQCOR(NPD) arrays to 1.0                  
            DQCOR = 1.0D0
            WQCOR = 1.0D0
         END IF

C        Set initial effective parameters
         UEFF  = US
         SVEFF = SVS
         SWEFF = SWS
         TGEFF = TGS
         IF ( UNSTAB  .AND.  (HS .LT. ZI) ) THEN
            UEFFD  = US
            SVEFFD = SVS
            SWEFFD = SWS
            UEFFN  = US
            SVEFFN = SVS
            SWEFFN = SWS
         END IF

CRJP     Add temporary debugging statement here.

C   ENSR ENHANCEMENT OF WRITE STATEMENT TO IDENTIFY COMPONENT CONCENTRATION
         IF(DEBUG) THEN
           IF(L_PLUME)THEN
              WRITE(DBGUNT, 6014) SRCID(ISRC)
6014          FORMAT(//,'SRCID: ', A8)
              WRITE(DBGUNT, 6015) UEFF, SVEFF, SWEFF
6015          FORMAT(//,'COHERENT PLUME COMPONENT',/,5X,
     &          'Initial effective parameters for ',
     &          'stable or direct convective ',
     &          'plume:',//,5x,'Ueff = ',F7.2,' m/s; ',
     &          'SVeff = ',F7.2,
     &          ' m/s; SWeff = ',F7.2,' m/s.',/)
           ELSE
              WRITE(DBGUNT, 6016) UEFF, SVEFF, SWEFF
6016          FORMAT(//,'MEANDER COMPONENT',/,5X,
     &          'Initial effective parameters for ',
     &          'stable or direct convective ',
     &          'plume:',//,5x,'Ueff = ',F7.2,' m/s; ',
     &          'SVeff = ',F7.2,
     &          ' m/s; SWeff = ',F7.2,' m/s.',/)
           END IF
         END IF

C        Define plume centroid height (CENTER) for use in
C        inhomogeniety calculations
         CALL CENTROID (XARG)

C        If the atmosphere is unstable and the stack
C        top is below the mixing height, calculate
C        the CBL PDF coefficients                     ---   CALL PDF
         IF( UNSTAB  .AND.  (HS .LT. ZI) ) THEN
            CALL PDF
         END IF

C        Determine Effective Plume Height             ---   CALL HEFF
         CALL HEFF ( XARG )

C        Compute effective parameters using an
C        iterative average through plume rise layer
         CALL IBLVAL ( XARG )

C        Call PDF & HEFF again for final CBL plume heights
         IF (UNSTAB .AND. (HS.LT.ZI) ) THEN
            CALL PDF
            CALL HEFF ( XARG )
         END IF

C        Determine Dispersion Parameters              ---   CALL VDIS
         CALL VDIS ( XARG )

C        Calculate the 'y-term' contribution to
C        dispersion, FSUBY
         IF (L_PLUME) THEN
            IF (L_EFFSIGY) THEN
C ---          Calculate fraction of random kinetic energy to total kinetic energy
C              for FASTALL option to optimize meander using effective sigma-y (EFFSIGY).
C              Note that these effective parameters are based on the downwind distance,
C              rather than the radial distance used in the standard meander approach
               IF (STABLE .OR. (UNSTAB.AND.(HS.GE.ZI))) THEN
                  CALL MEANDR( UEFF, SVEFF, FRAN )
               ELSE IF (UNSTAB) THEN
                  CALL MEANDR( UEFFD, SVEFFD, FRAN )
               END IF

C              Calculate effective sigma-y for non-DFAULT FASTALL option (EFFSIGY)
               SYEFF = 1.0D0/((FRAN/(SRT2PI*XARG)) + (1.0D0-FRAN)/SY)
               IF (DABS(Y) .GT. NUMSYEFF*SYEFF) THEN
C                 Plume is more than 6 sigmas off centerline, skip calculation
                  FYOUT = 0.0D0
               ELSE
C                 Calculate FSUBY for coherent plume        ---   CALL FYPLM
                  CALL FYPLM(SYEFF,FYOUT)
               END IF
            ELSE
C              Calculate FSUBY for coherent plume        ---   CALL FYPLM
               CALL FYPLM(SY,FYOUT)
            END IF
         ELSE
C           Calculate FSUBY for random component      ---   CALL FYPAN
            CALL FYPAN(FYOUT)
         END IF
         FSUBY  = FYOUT
         FSUBYD = FSUBY
         FSUBYN = FSUBYD

C        Set lateral term = 0.0 for penetrated source
         FSUBY3 = 0.0D0

C        Check for zero "y-terms"; if zero then skip calculations
C        and go to next receptor.
         IF( FSUBY.EQ.0.0D0 .AND. FSUBY3.EQ.0.0D0 )THEN
            AEROUT = 0.0D0

         ELSE

            IF (NPD .EQ. 0) THEN
C              Perform calculations for gases
C              Assign plume tilt, HV = 0.0

               ADJ = DQCORG * WQCORG

               IF (STABLE .OR. (UNSTAB.AND.(HS.GE.ZI))) THEN
C                 Calculate height of the "effective reflecting surface"
                  CALL REFL_HT (HE, XARG, 0.0D0, VSIGZ, HSBL)
               ELSE IF ( UNSTAB ) THEN
                  HSBL = 0.0D0
               END IF

C              Determine the CRITical Dividing Streamline---   CALL CRITDS
               CALL CRITDS (HE)

C              Calculate the fraction of plume below
C              HCRIT, PHEE                               ---   CALL PFRACT
               CALL PFRACT (HE)

C              Calculate FOPT = f(PHEE)                  ---   CALL FTERM
               CALL FTERM

C              Calculate Conc. for Virtual Point Source  ---   CALL AER_PCHI
               CALL AER_PCHI( XARG, ADJ, VDEPG, 0, AEROUT )

            ELSE
C              Perform calculations for particles, loop through particle sizes

C              Begin loop over particle sizes
               DO J = 1, NPD

C                 Calculate Plume Tilt Due to Settling, HV
                  HV = (XARG/US) * VGRAV(J)

C                 Adjust Jth contribution by mass fraction and source
C                 depletion
                  ADJ = PHI(J) * DQCOR(J) * WQCOR(J)

                  IF (STABLE .OR. (UNSTAB.AND.(HS.GE.ZI))) THEN
C                    Calculate height of the "effective reflecting surface"
                     HESETL = MAX( 0.0D0, HE - HV )
                     CALL REFL_HT (HESETL, XARG, 0.0D0, VSIGZ, HSBL)
                  ELSE IF ( UNSTAB ) THEN
                     HESETL = MAX( 0.0D0, 0.5D0*(HED1+HED2) - HV )
                     HSBL = 0.0D0
                  END IF

C                 Determine the CRITical Dividing Streamline---   CALL CRITDS
                  CALL CRITDS (HESETL)

C                 Calculate the fraction of plume below
C                 HCRIT, PHEE                               ---   CALL PFRACT
                  CALL PFRACT (HESETL)

C                 Calculate FOPT = f(PHEE)                  ---   CALL FTERM
                  CALL FTERM

C                 Calculate Conc. for Virtual Point Source  ---   CALL AER_PCHI
                  CALL AER_PCHI( XARG, ADJ, VDEP(J), J, AERTMP )
                  AEROUT = AEROUT + AERTMP

               END DO
C              End loop over particle sizes

            END IF
         END IF
      END IF

      RETURN
      END


      SUBROUTINE ACALC
C***********************************************************************
C            ACALC Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Calculates concentration or deposition values
C                 for AREA sources utilizing an integrated line source.
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   
C                    Added arrays to save WDSIN and WDCOS by source for use
C                    with PVMRM option.  Corrects potential problem for 
C                    PVMRM applications with multi-level wind inputs.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 01/26/2007
C
C                    Added call to subroutine HEFF for calculation
C                    of ZSUBP for deposition applications.
C                    R. W. Brode, U.S. EPA, OAQPS, AQMG, 01/24/2007
C
C        MODIFIED:   Modified to initialize HE = HS prior to first
C                    call to sub. ADISY.
C                    R. W. Brode, MACTEC (f/k/a PES), Inc., 07/05/05
C
C        MODIFIED:   Modified to include initialization of __VAL arrays
C                    at end of receptor loop.
C                    R. W. Brode, MACTEC (f/k/a PES), Inc., 10/26/04
C
C        MODIFIED:   To include the PVMRM and OLM options for
C                    modeling conversion of NOx to NO2.
C                    Added debug statement based on ENSR code.
C                    R. W. Brode, MACTEC (f/k/a PES), Inc., 07/27/04
C
C        MODIFIED:   To include tilted plume for point source
C                    approximation of particle emissions.
C                    R. W. Brode, MACTEC (f/k/a PES), Inc., 07/23/04
C
C        MODIFIED:   To allow TOXICS option for AREAPOLY sources.
C                    R. W. Brode, MACTEC (f/k/a PES), Inc., 05/12/04
C
C        MODIFIED:   To assign value to XDIST for use in dry depletion.
C                    R. W. Brode, MACTEC (f/k/a PES), Inc., 03/19/04
C
C        MODIFIED:   To avoid potential math errors for AREAPOLY sources
C                    R.W. Brode, PES, Inc. - 02/25/02
C
C        MODIFIED:   To incorporate numerical integration algorithm
C                    for AREA source - 7/7/93
C
C        INPUTS:  Source Parameters for Specific Source
C                 Arrays of Receptor Locations
C                 Meteorological Variables for One Hour
C
C        OUTPUTS: Array of 1-hr CONC or DEPOS Values for Each Source/Receptor
C
C        CALLED FROM:   CALC
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      INTEGER :: I, J
      DOUBLE PRECISION :: XDEP, WIDTH, LENGTH, XMAXR, QTKSAV, XPOINT, 
     &                    ADJ
      DOUBLE PRECISION :: AEROUT(NUMTYP), FYOUT

C     Variable Initializations
      MODNAM = 'ACALC'
      WAKE = .FALSE.

C     Initialize __VAL arrays (1:NUMTYP)
      HRVAL   = 0.0D0
      AERVAL  = 0.0D0
      AEROUT  = 0.0D0

C     Set the Source Variables for This Source              ---   CALL SETSRC
      CALL SETSRC

C     Apply Variable Emission Rate Factors                  ---   CALL EMFACT
      CALL EMFACT(QS)

C     Initialize 'ARC' Arrays for EVALFILE Output           ---   CALL EVLINI
      IF (EVAL(ISRC)) THEN
         CALL EVLINI
      END IF

      IF (QTK .NE. 0.0D0) THEN

C        Set Mixing Height and Profiles for Urban Option if Needed
         IF (URBSRC(ISRC) .EQ. 'Y') THEN
C           Find Urban Area Index for This Source
            DO I = 1, NUMURB
               IF (IURBGRP(ISRC,I) .EQ. 1) THEN
                  IURB = I
                  EXIT
               END IF
            END DO
            IF (STABLE .OR. L_MorningTrans(IURB)) THEN
               URBSTAB = .TRUE.
               ZI = MAX( ZIURB(IURB), ZIMECH )
               GRIDSV = GRDSVU(1:MXGLVL,IURB)
               GRIDSW = GRDSWU(1:MXGLVL,IURB)
               GRIDTG = GRDTGU(1:MXGLVL,IURB)
               GRIDPT = GRDPTU(1:MXGLVL,IURB)
               OBULEN = DABS( URBOBULEN(IURB) )
               USTAR  = URBUSTR(IURB)
            ELSE
               URBSTAB = .FALSE.
               ZI = ZIRUR
               GRIDSV = GRDSVR
               GRIDSW = GRDSWR
               GRIDTG = GRDTGR
               GRIDPT = GRDPTR
               OBULEN = RUROBULEN
               USTAR  = RURUSTR
            END IF
         ELSE IF (URBAN .AND. URBSRC(ISRC) .EQ. 'N') THEN
            URBSTAB = .FALSE.
            ZI = ZIRUR
            GRIDSV = GRDSVR
            GRIDSW = GRDSWR
            GRIDTG = GRDTGR
            GRIDPT = GRDPTR
            OBULEN = RUROBULEN
            USTAR  = RURUSTR
         ELSE
            URBSTAB = .FALSE.
         END IF

C        Initialize meteorological variables                ---   CALL METINI
         CALL METINI

C        Save WDSIN and WSCOS for later use by PVMRM option
         AWDSIN(ISRC) = WDSIN
         AWDCOS(ISRC) = WDCOS

C        Initialize miscellaneous variables
         FB  = 0.0D0
         FM  = 0.0D0
         PPF = 0.0D0
         HSP = HS
         HE  = HS
         DHP  = 0.0D0
         DHP1 = 0.0D0
         DHP2 = 0.0D0
         DHP3 = 0.0D0
         DHCRIT = 0.0D0
         XFINAL = 0.0D0
         XMIXED = ZI * UAVG / SWAVG
         IF(XMIXED .LT. XFINAL) XMIXED = XFINAL
         ZMIDMX = 0.5D0 * ZI

C ---    Initialize PDF parameters for use in calculating ZSUBP
         IF( UNSTAB  .AND.  (HS .LT. ZI) ) THEN
            CALL PDF
         END IF
C        Set Dry Deposition Variables for this Source
         IF (LUSERVD .AND. LDGAS .AND. NPD.EQ.0) THEN
C           Assign user-specified gas dry deposition velocity (GASDEPVD option)
            VDEPG = USERVD
         ELSE IF (LDPART .OR. (.NOT.LUSERVD .AND. LDGAS .AND. 
     &                                                   NPD.EQ.0)) THEN
C           Calculate Deposition Velocities for this Source    ---   CALL VDP
            CALL VDP
         END IF
         IF (LWPART .OR. LWGAS) THEN
CPES        Set value of ZSUBP = MAX( ZI, TOP OF PLUME ), where
CPES        TOP OF PLUME is defined as plume height (HE) plus 2.15*SZ,
CPES        evaluated at a distance of 20 kilometers downwind.
CPES        Apply minimum value of 500m and maximum value of 10,000m.
            CALL HEFF (20000.0D0)
            IF( STABLE .OR. (UNSTAB .AND. HS .GE. ZI) )THEN
               CALL SIGZ(20000.0D0)
               ZSUBP = MAX( 500.0D0, ZI, HS + SZCOEF*SZAS )
            ELSE IF (UNSTAB) THEN
               CALL SIGZ(20000.0D0)
               ZSUBP = MAX( 500.0D0, ZI, HS + 
     &                                   SZCOEF*(SZAD1+SZAD2)/2.0D0 )
            END IF
            ZSUBP = MIN( 10000.0D0, ZSUBP )
C           Calculate Scavenging Ratios for this Source           ---   CALL SCAVRAT
            CALL SCAVRAT
         END IF

C        Begin Receptor LOOP
         RECEPTOR_LOOP: DO IREC = 1, NUMREC
C           Calculate Down and Crosswind Distances          ---   CALL ARDIST
            IF (EVONLY) THEN
               CALL ARDIST(IEVENT,XDEP,WIDTH,LENGTH,XMAXR)
            ELSE
               CALL ARDIST(IREC,XDEP,WIDTH,LENGTH,XMAXR)
            END IF

C           Check to see if receptor is upwind of area source
            IF (XMAXR .LT. 1.0D0) CYCLE RECEPTOR_LOOP

C           Check to see if receptor is beyond the maximum
C           distance from source; assigned value of 80km 
C           for obsolescent TOXICS option or new FASTAREA or
C           FASTALL options; otherwise "unlimited" (1.0D20)
            IF (DISTR .GT. MAXDIST) CYCLE RECEPTOR_LOOP

C           Initialize HE for initial call to ADISY
            HE = HS
C           Set initial effective parameters
            UEFF  = US
            SVEFF = SVS
            SWEFF = SWS
            TGEFF = TGS
            IF ( UNSTAB  .AND.  (HS .LT. ZI) ) THEN
               UEFFD  = US
               SVEFFD = SVS
               SWEFFD = SWS
               UEFFN  = US
               SVEFFN = SVS
               SWEFFN = SWS
            END IF

C ---       Check to see if receptor is beyond edge of plume laterally.
            IF ( (DABS(Y)-0.5D0*WIDTH) .GT. 0.0D0) THEN
C ---          Receptor is outside "projected" width of effective area source
C              Calculate maximum sigma-y based on XMAXR and compare minimum 
C              lateral distance of receptor from source to 4*sigma-y
               CALL ADISY(XMAXR)
               IF ((DABS(Y)-0.5D0*WIDTH) .GE. 4.0D0*SY) 
     &                                       CYCLE RECEPTOR_LOOP
            END IF

            IF(DEBUG) THEN
              WRITE(DBGUNT, 6015) UEFF, SVEFF, SWEFF
C   ENSR ENHANCEMENT OF WRITE STATEMENT
6015            FORMAT(//,'AERMOD AREA SOURCE COMPONENT',/,5X,
     &          'Initial effective parameters for ',
     &          'stable or direct convective ',
     &          'plume:',//,5x,'Ueff = ',F7.2,' m/s; ',
     &          'SVeff = ',F7.2,
     &          ' m/s; SWeff = ',F7.2,' m/s.',/)
            END IF

C           Determine the CRITical Dividing Streamline      ---   CALL CRITDS
            CALL CRITDS (HE)

C           Set distance factor for point source approx. for FASTAREA option
C           based on "equivalent" PG stability class (KST)
            IF (URBSTAB) THEN
               VP_FACT = VIRTPNT_URB(KST)
            ELSE
               VP_FACT = VIRTPNT_RUR(KST)
            END IF

C           Calculate distance for switch to point source approx. for FASTAREA
            XPOINT = 1.5D0*LENGTH + VP_FACT*WIDTH

C ---       Assign XDIST for use in dry depletion (FUNCTION F2INT)
            XDIST = X

            IF (.NOT.FASTAREA .OR.
     &              (FASTAREA .AND. X .LT. XPOINT)) THEN
               IF (ARDPLETE) THEN

                  IF (NPD .EQ. 0 .AND. (LDGAS .OR. LWGAS)) THEN
                     CALL PDEPG (XDEP)
                  ELSE
                     DQCORG = 1.0D0
                     WQCORG = 1.0D0
                  END IF
                  IF (NPD .GT. 0 .AND. (LDPART .OR. LWPART)) THEN
                     CALL PDEP (XDEP)
                  ELSE IF (NPD .GT. 0) THEN
C                    Set DQCOR(NPD) and WQCOR(NPD) arrays to 1.0                  
                     DQCOR = 1.0D0
                     WQCOR = 1.0D0
                  END IF

               END IF

               DO ITYP = 1, NUMTYP
C                 Calculate Area Source Integral         ---   CALL AREAIN
                  CALL AREAIN
               END DO
            ELSE
C              Use point source approximation
C              Save emissions per unit area and calculate total emissions
               QTKSAV = QTK
               IF (SRCTYP(ISRC) .EQ. 'AREA' .OR.
     &             SRCTYP(ISRC) .EQ. 'AREAPOLY') THEN
C                 Note that XINIT and YINIT are equivalent values for AREAPOLY
                  QTK = QTK * XINIT * YINIT
               ELSE IF (SRCTYP(ISRC) .EQ. 'AREACIRC') THEN
                  QTK = QTK * PI * RADIUS(ISRC) * RADIUS(ISRC)
               END IF
               SYINIT = 0.0D0

C              Determine Deposition Correction Factors
               IF (NPD .EQ. 0 .AND. (LDGAS .OR. LWGAS)) THEN
                  CALL PDEPG (X)
               ELSE
                  DQCORG = 1.0D0
                  WQCORG = 1.0D0
               END IF
               IF (NPD .GT. 0 .AND. (LDPART .OR. LWPART)) THEN
                  CALL PDEP (X)
               ELSE IF (NPD .GT. 0) THEN
C                 Set DQCOR(NPD) and WQCOR(NPD) arrays to 1.0                  
                  DQCOR = 1.0D0
                  WQCOR = 1.0D0
               END IF

C              Define plume centroid height (CENTER) for use in
C              inhomogeniety calculations
               CALL CENTROID (X)

C              If the atmosphere is unstable and the stack
C              top is below the mixing height, calculate
C              the CBL PDF coefficients                     ---   CALL PDF
               IF( UNSTAB  .AND.  (HS .LT. ZI) ) THEN
                  CALL PDF
               END IF

C              Determine Effective Plume Height             ---   CALL HEFF
               CALL HEFF ( X )

C              Compute effective parameters using an
C              iterative average through plume rise layer
               CALL IBLVAL (X)

C              Call PDF & HEFF again for final CBL plume heights
               IF (UNSTAB .AND. (HS.LT.ZI) ) THEN
                  CALL PDF
                  CALL HEFF (X)
               END IF

C              Determine Dispersion Parameters              ---   CALL VDIS
               CALL VDIS (X)

C              Calculate the 'y-term' contribution to
C              dispersion, FSUBY, for coherent plume        ---   CALL FYPLM
               CALL FYPLM(SY,FYOUT)
               FSUBY  = FYOUT
               FSUBYD = FSUBY
               FSUBYN = FSUBYD

C              Set lateral term = 0.0 for penetrated source
               FSUBY3 = 0.0D0

C              Check for zero "y-terms"; if zero then skip calculations
C              and go to next receptor.
               IF( FSUBY.EQ.0.0D0 .AND. FSUBY3.EQ.0.0D0 )THEN
C                 Set HRVAL(NUMTYP) array to 0.0               
                  HRVAL = 0.0D0

               ELSE

                  IF (NPD .EQ. 0) THEN
C                    Perform calculations for gases
C                    Assign plume tilt, HV = 0.0

                     ADJ = DQCORG * WQCORG

                     IF (STABLE .OR. (UNSTAB.AND.(HS.GE.ZI))) THEN
C                       Calculate height of the "effective reflecting surface"
                        CALL REFL_HT (HE, X, 0.0D0, VSIGZ, HSBL)
                     ELSE IF ( UNSTAB ) THEN
                        HSBL = 0.0D0
                     END IF

C                    Determine the CRITical Dividing Streamline---   CALL CRITDS
                     CALL CRITDS (HE)

C                    Calculate the fraction of plume below
C                    HCRIT, PHEE                               ---   CALL PFRACT
                     CALL PFRACT (HE)

C                    Calculate FOPT = f(PHEE)                  ---   CALL FTERM
                     CALL FTERM

C                    Calculate Conc. for Virtual Point Source  ---   CALL AER_PCHI
                     CALL AER_PCHI( X, ADJ, VDEPG, 0, AEROUT )
                     HRVAL = AEROUT

                  ELSE
C                    Perform calculations for particles, loop through particle sizes

C                    Begin loop over particle sizes
                     DO J = 1, NPD

C                       Calculate Plume Tilt Due to Settling, HV
                        HV = (X/US) * VGRAV(J)

C                       Adjust Jth contribution by mass fraction and source
C                       depletion
                        ADJ = PHI(J) * DQCOR(J) * WQCOR(J)

                        IF (STABLE .OR. (UNSTAB.AND.(HS.GE.ZI))) THEN
C                          Calculate height of the "effective reflecting surface"
C                          Calculate Settled Plume Height(s), HESETL
                           HESETL = MAX( 0.0D0, HE - HV )
                           CALL REFL_HT (HESETL, X, 0.0D0, VSIGZ, HSBL)
                        ELSE IF ( UNSTAB ) THEN
C                          Calculate Settled Plume Height(s), HESETL
                           HESETL = MAX( 0.0D0, 0.5D0*(HED1+HED2) - HV )
                           HSBL = 0.0D0
                        END IF

C                       Determine the CRITical Dividing Streamline---   CALL CRITDS
                        CALL CRITDS (HESETL)

C                       Calculate the fraction of plume below
C                       HCRIT, PHEE                               ---   CALL PFRACT
                        CALL PFRACT (HESETL)

C                       Calculate FOPT = f(PHEE)                  ---   CALL FTERM
                        CALL FTERM

C                       Calculate Conc. for Virtual Point Source  ---   CALL AER_PCHI
                        CALL AER_PCHI( X, ADJ, VDEP(J), J, AEROUT )
                        HRVAL = HRVAL + AEROUT

                     END DO
C                    End loop over particle sizes

                  END IF
               END IF
               QTK = QTKSAV
            END IF

            IF (PVMRM .AND. .NOT.O3MISS) THEN
C ---          Store data by source and receptor for PVMRM option
               DO ITYP = 1, NUMTYP
                  CHI(IREC,ISRC,ITYP) = HRVAL(ITYP)
               END DO
               IF (STABLE .OR. (UNSTAB.AND.(HS.GE.ZI))) THEN
                  HECNTR(IREC,ISRC) = HE
                  UEFFS(IREC,ISRC)  = UEFF
               ELSE
                  HECNTR(IREC,ISRC) = CENTER
                  UEFFS(IREC,ISRC)  = UEFFD
               END IF
               IF (PPF .GT. 0.0D0) THEN
                  HECNTR3(IREC,ISRC) = HE3
                  PPFACT(ISRC)       = PPF
                  UEFF3S(IREC,ISRC)  = UEFF3
               ELSE
                  PPFACT(ISRC) = 0.0D0
               END IF
               FOPTS(IREC,ISRC) = FOPT
C              Cycle to next receptor & skip call to SUMVAL (will be done later)
               CYCLE RECEPTOR_LOOP
            ELSE IF (OLM .AND. .NOT.O3MISS) THEN
C ---          Store data by source and receptor for OLM option
               DO ITYP = 1, NUMTYP
                  CHI(IREC,ISRC,ITYP) = HRVAL(ITYP)
               END DO
C              Cycle to next receptor & skip call to SUMVAL (will be done later)
               CYCLE RECEPTOR_LOOP
            END IF

C           Sum HRVAL to AVEVAL and ANNVAL Arrays           ---   CALL SUMVAL
            IF (EVONLY) THEN
               CALL EV_SUMVAL
            ELSE
               CALL SUMVAL
            END IF

C           Initialize __VAL arrays (1:NUMTYP)
            HRVAL   = 0.0D0
            AERVAL  = 0.0D0
            AEROUT  = 0.0D0

         END DO RECEPTOR_LOOP
C        End Receptor LOOP

C        Output 'ARC' Values for EVALFILE                   ---   CALL EVALFL
         IF (EVAL(ISRC)) THEN
            CALL EVALFL
         END IF

      END IF

      RETURN
      END

      SUBROUTINE ARDIST(INDX,XDEP,WIDTH,LENGTH,XMAXREC)
C***********************************************************************
C                 ARDIST Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Sets Receptor Variables and Calculates Downwind (X)
C                 and Crosswind (Y) Distances, Crosswind Width (WIDTH),
C                 Distance used for AREADPLT Option (XDEP), Maximum
C                 Downwind Distance by Vertex (XMAXREC), and
C                 Radial Distance from Source to Receptor (DISTR)
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   Recalculate the center of effective area source
C                    for OPENPIT sources when INDX = 1.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
C
C        INPUTS:  Receptor Index, INDX
C                 Source Location
C                 Arrays of Receptor Locations
C                 SIN and COS of Wind Direction FROM Which Wind
C                 is Blowing, WDSIN and WDCOS
C
C        OUTPUTS: Values of X, Y, and DISTR (m) [in MAIN1]
C                 XDEP (m)
C                 WIDTH (m)
C                 LENGTH (m)
C                 XMAXREC (m)
C
C        CALLED FROM:   ACALC
C                       OCALC
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, INDX
      DOUBLE PRECISION :: XSRC, YSRC, XMINREC, XMAXREC, YMINREC, 
     &           YMAXREC, XDEP, WIDTH, LENGTH

C     Variable Initializations
      MODNAM = 'ARDIST'

C     Set Receptor Coordinates, Terrain Elevation and Flagpole Heights
      XR = AXR(INDX)
      YR = AYR(INDX)
      ZELEV = AZELEV(INDX)
      ZHILL = AZHILL(INDX)
      ZFLAG = AZFLAG(INDX)

      XMINREC =  9999999.0D0
      XMAXREC = -9999999.0D0
      YMINREC =  9999999.0D0
      YMAXREC = -9999999.0D0

C     Calculate Downwind (X) and Crosswind (Y) Distances for Each Vertex
      DO I = 1, NVERT+1
         XSRC = XVERT(I)
         YSRC = YVERT(I)
         SPA(I,1) = -((XR-XSRC)*WDSIN + (YR-YSRC)*WDCOS)
         SPA(I,2) =   (XR-XSRC)*WDCOS - (YR-YSRC)*WDSIN
         XMINREC = MIN(XMINREC, SPA(I,1))
         XMAXREC = MAX(XMAXREC, SPA(I,1))
         YMINREC = MIN(YMINREC, SPA(I,2))
         YMAXREC = MAX(YMAXREC, SPA(I,2))
      END DO

C     Calculate crosswind width, WIDTH, and alongwind length, LENGTH
      WIDTH  = YMAXREC - YMINREC
      LENGTH = XMAXREC - XMINREC

C     Determine downwind distance to use for AREADPLT option, XDEP
      IF (XMINREC .GE. 0.0D0) THEN
         XDEP = XMINREC + LENGTH/3.0D0
      ELSE
         XDEP = XMAXREC/3.0D0
      END IF

      XDEP = MAX( 1.0D0, XDEP )

C --- Recalculate center of effective area for OPENPIT sources if INDX=1
      IF (INDX .EQ. 1 .AND. SRCTYP(ISRC) .EQ. 'OPENPIT') THEN
         XCNTR = 0.0D0
         YCNTR = 0.0D0
         DO I = 1, NVERT
            XCNTR = XCNTR + XVERT(I)
            YCNTR = YCNTR + YVERT(I)
         END DO
         XCNTR = XCNTR/DBLE(NVERT)
         YCNTR = YCNTR/DBLE(NVERT)
      END IF

C     Calculate Downwind (X) and Crosswind (Y) Distances from Center of Source
      X = -((XR-XCNTR)*WDSIN + (YR-YCNTR)*WDCOS)
      Y =   (XR-XCNTR)*WDCOS - (YR-YCNTR)*WDSIN

C     Calculate Radial Distance from Center of Source
      DISTR = DSQRT(X*X + Y*Y)

C     Calculate height of receptor above stack base, ZRT
      IF (L_FLATSRC(ISRC)) THEN
         ZRT = ZFLAG
      ELSE
         ZRT = ZELEV - ZS + ZFLAG
      END IF

      RETURN
      END

      SUBROUTINE OCALC
C***********************************************************************
C                 OCALC Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Calculates concentration or deposition values
C                 for OPENPIT sources
C
C        PROGRAMMER: Jayant Hardikar, Roger Brode
C        ADAPTED FROM:  SUBROUTINE ACALC
C
C        DATE:    July 19, 1994
C
C        MODIFIED:   
C                    Corrected calculation of adjusted emission for the
C                    point source approximation under FASTAREA option
C                    to use length and width of effective area source
C                    rather than length and width of the openpit source.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
C
C                    Added arrays to save WDSIN and WDCOS by source for use
C                    with PVMRM option.  Corrects potential problem for 
C                    PVMRM applications with multi-level wind inputs.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 01/26/2007
C
C                    Added call to subroutine HEFF for calculation
C                    of ZSUBP for deposition applications.
C                    R. W. Brode, U.S. EPA, OAQPS, AQMG, 01/24/2007
C
C        MODIFIED:   Modified to initialize HE = HS prior to first
C                    call to sub. ADISY.
C                    R. W. Brode, MACTEC (f/k/a PES), Inc., 07/05/05
C
C        MODIFIED:   Modified to include initialization of __VAL arrays
C                    at end of receptor loop.
C                    R. W. Brode, MACTEC (f/k/a PES), Inc., 10/26/04
C
C        MODIFIED:   To include tilted plume for point source
C                    approximation of particle emissions.
C                    R. W. Brode, MACTEC (f/k/a PES), Inc., 07/23/04
C
C        MODIFIED:   To move call to METINI up since AFV is needed for
C                    SUBROUTINE LWIND.
C                    R. W. Brode, PES Inc., - 1/22/98
C
C        MODIFIED:   To skip calculations if QPTOT = 0.0, avoiding
C                    zero divide error in SUB. AMFRAC.
C                    R. W. Brode, PES Inc., - 4/14/95
C
C        INPUTS:  Source Parameters for Specific Source
C                 Arrays of Receptor Locations
C                 Meteorological Variables for One Hour
C
C        OUTPUTS: Array of 1-hr CONC or DEPOS Values for Each Source/Receptor
C
C        CALLED FROM:   CALC
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      INTEGER :: I, II, J, ICAT, INOUT, NDXR
      DOUBLE PRECISION :: QPTOT, XVM(5), YVM(5), XDEP, WIDTH, LENGTH, 
     &                    XMAXR, QTKSAV, XPOINT, ADJ, FYOUT
      DOUBLE PRECISION :: AEROUT(NUMTYP)

C     Variable Initializations
      MODNAM = 'OCALC'
      WAKE = .FALSE.

C     Initialize __VAL arrays (1:NUMTYP)
      HRVAL   = 0.0D0
      AERVAL  = 0.0D0
      AEROUT  = 0.0D0

C     Obtain reference wind speed at 10-meters
C---- First locate index below 10.
      CALL LOCATE(GRIDHT, 1, MXGLVL, 10.0D0, NDXR)
      CALL GINTRP( GRIDHT(NDXR), GRIDWS(NDXR),
     &             GRIDHT(NDXR+1), GRIDWS(NDXR+1),
     &             10.0D0, UREF10 )

C     Set Mixing Height and Profiles for Urban Option if Needed
      IF (URBSRC(ISRC) .EQ. 'Y') THEN
C        Find Urban Area Index for This Source
         DO I = 1, NUMURB
            IF (IURBGRP(ISRC,I) .EQ. 1) THEN
               IURB = I
               EXIT
            END IF
         END DO
         IF (STABLE .OR. L_MorningTrans(IURB)) THEN
            URBSTAB = .TRUE.
            ZI = MAX( ZIURB(IURB), ZIMECH )
            GRIDSV = GRDSVU(1:MXGLVL,IURB)
            GRIDSW = GRDSWU(1:MXGLVL,IURB)
            GRIDTG = GRDTGU(1:MXGLVL,IURB)
            GRIDPT = GRDPTU(1:MXGLVL,IURB)
            OBULEN = DABS( URBOBULEN(IURB) )
            USTAR  = URBUSTR(IURB)
         ELSE
            URBSTAB = .FALSE.
            ZI = ZIRUR
            GRIDSV = GRDSVR
            GRIDSW = GRDSWR
            GRIDTG = GRDTGR
            GRIDPT = GRDPTR
            OBULEN = RUROBULEN
            USTAR  = RURUSTR
         END IF
      ELSE IF (URBAN .AND. URBSRC(ISRC) .EQ. 'N') THEN
         URBSTAB = .FALSE.
         ZI = ZIRUR
         GRIDSV = GRDSVR
         GRIDSW = GRDSWR
         GRIDTG = GRDTGR
         GRIDPT = GRDPTR
         OBULEN = RUROBULEN
         USTAR  = RURUSTR
      ELSE
         URBSTAB = .FALSE.
      END IF

C     Set the Source Variables for This Source              ---   CALL SETSRC
      CALL SETSRC

C     Initialize meteorological variables                   ---   CALL METINI
      CALL METINI

C     Save WDSIN and WSCOS for later use by PVMRM option
      AWDSIN(ISRC) = WDSIN
      AWDCOS(ISRC) = WDCOS

C*    Initialize the Total Adjusted Emission Rate from
C*    All Particles
      QPTOT = 0.0D0

      IF (NPD .EQ. 0) THEN
C*       Assign input emission to QPTOT variable for gas emissions
         QPTOT = QS

      ELSE

C*       Loop over Particle Size Categories
         DO ICAT = 1,NPD
C*          Calculate the Escape Fraction for Each Category    ---   CALL ESCAPE
            CALL ESCAPE(ICAT)

C*          Adjust the Emission Rate for Each Category         ---   CALL ADJEMI
            CALL ADJEMI(ICAT,QPTOT)

C*       End Loop Over Particle Size Categories
         END DO

      END IF
      
C*    Skip Calculations if QPTOT = 0.0
      IF (QPTOT .EQ. 0.0D0)  GO TO 999

      IF (NPD .GT. 0) THEN
C*       Adjust the Mass Fractions for All the Particle
C*       Size Categories                                    ---   CALL AMFRAC
         CALL AMFRAC(QPTOT)
      END IF
      
C*    Determine the AlongWind Length of the OPENPIT Source  ---   CALL LWIND
      CALL LWIND

C*    Calculate the Relative Depth of the OPENPIT Source    ---   CALL PDEPTH
      CALL PDEPTH

C*    Calculate the Fractional Size of the
C*    Effective Pit Area (PITFRA)                           ---   CALL PTFRAC
      CALL PTFRAC

C*    WRITE DEBUG INFORMATION
      IF (DEBUG) THEN
         WRITE (IOUNIT,*)
         WRITE (IOUNIT,*)
         WRITE (IOUNIT,*) 'DETAIL INFORMATION ON THE OPENPIT SOURCE:'
         WRITE (IOUNIT,*)
         WRITE (IOUNIT,*)
      END IF

C*    Determine the Coordinates of the Effective Pit Area
C*    in Wind Direction Coordinate System                   ---   CALL PITEFF
      CALL PITEFF

C*    Calculate the adusted Emission Rate per unit area (QEFF) for the 
C*    Effective Pit Area (PITFRA)                           ---   CALL PITEMI
      CALL PITEMI(QPTOT)

C*    WRITE DEBUG INFORMATION
      IF (DEBUG) THEN
         IF (NPD .GT. 0) THEN
            WRITE (IOUNIT,*) 'OPENPIT PARTICLE CHARACTERISTICS:'
            WRITE (IOUNIT,*) '-------------------------------'
            WRITE (IOUNIT,*)
            WRITE (IOUNIT,8000) (EFRAC(II),II = 1, NPD)
8000        FORMAT (1X,'ESCAPE FRACTIONS= ',10(F8.3,2X))
            WRITE (IOUNIT,8200) (QPART(II),II = 1, NPD)
8200        FORMAT (1X,'ADJUSTED EMISSION RATES= ',10(F8.3,2X))
            WRITE (IOUNIT,8400) (PHI(II),II = 1, NPD)
8400        FORMAT (1X,'ADJUSTED MASS FRACTIONS= ',10(F8.3,2X))
         END IF
         WRITE (IOUNIT,*) 'EMISSION RATE OF EFFECTIVE PIT= ',QEFF
         WRITE (IOUNIT,*)
      END IF

C     Apply Variable Emission Rate Factors                  ---   CALL EMFACT
      CALL EMFACT(QEFF)

C     Initialize 'ARC' Arrays for EVALFILE Output           ---   CALL EVLINI
      IF (EVAL(ISRC)) THEN
         CALL EVLINI
      END IF

      IF ((QTK.NE.0.0D0) .AND. (STABLE .OR. (HS.LE.ZI))) THEN
C        Initialize miscellaneous variables
         FB  = 0.0D0
         FM  = 0.0D0
         PPF = 0.0D0
         HSP = HS
         DHP  = 0.0D0
         DHP1 = 0.0D0
         DHP2 = 0.0D0
         DHP3 = 0.0D0
         DHCRIT = 0.0D0
         XFINAL = 0.0D0
         XMIXED = ZI * UAVG / SWAVG
         IF(XMIXED .LT. XFINAL) XMIXED = XFINAL
         ZMIDMX = 0.5D0 * ZI

C ---    Initialize PDF parameters for use in calculating ZSUBP
         IF( UNSTAB  .AND.  (HS .LT. ZI) ) THEN
            CALL PDF
         END IF
C        Set Dry Deposition Variables for this Source
         IF (LUSERVD .AND. LDGAS .AND. NPD.EQ.0) THEN
C           Assign user-specified gas dry deposition velocity (GASDEPVD option)
            VDEPG = USERVD
         ELSE IF (LDPART .OR. (.NOT.LUSERVD .AND. LDGAS .AND. 
     &                                                   NPD.EQ.0)) THEN
C           Calculate Deposition Velocities for this Source    ---   CALL VDP
            CALL VDP
         END IF
         IF (LWPART .OR. LWGAS) THEN
CPES        Set value of ZSUBP = MAX( ZI, TOP OF PLUME ), where
CPES        TOP OF PLUME is defined as plume height (HE) plus 2.15*SZ,
CPES        evaluated at a distance of 20 kilometers downwind.
CPES        Apply minimum value of 500m and maximum value of 10,000m.
            CALL HEFF (20000.0D0)
            IF( STABLE .OR. (UNSTAB .AND. HS .GE. ZI) )THEN
               CALL SIGZ(20000.0D0)
               ZSUBP = MAX( 500.0D0, ZI, HS + SZCOEF*SZAS )
            ELSE IF (UNSTAB) THEN
               CALL SIGZ(20000.0D0)
               ZSUBP = MAX( 500.0D0, ZI, HS + 
     &                                   SZCOEF*(SZAD1+SZAD2)/2.0D0 )
            END IF
            ZSUBP = MIN( 10000.0D0, ZSUBP )
C           Calculate Scavenging Ratios for this Source           ---   CALL SCAVRAT
            CALL SCAVRAT
         END IF

C        Begin Receptor LOOP
         RECEPTOR_LOOP: DO IREC = 1, NUMREC
C           Check for receptor located inside boundary of open pit source
            DO I = 1, NVERT+1
               XVM(I) = AXVERT(I,ISRC)
               YVM(I) = AYVERT(I,ISRC)
            END DO
            XR = AXR(IREC)
            YR = AYR(IREC)
            CALL PNPOLY(XR,YR,XVM,YVM,5,INOUT)
            IF (INOUT .GT. 0) THEN
C              Receptor is within boundary - skip to next receptor
               CYCLE RECEPTOR_LOOP
            END IF

C           Calculate Down and Crosswind Distances          ---   CALL ARDIST
            IF (EVONLY) THEN
               CALL ARDIST(IEVENT,XDEP,WIDTH,LENGTH,XMAXR)
            ELSE
               CALL ARDIST(IREC,XDEP,WIDTH,LENGTH,XMAXR)
            END IF

C           Check to see if receptor is upwind of area source
            IF (XMAXR .LT. 1.0D0) CYCLE RECEPTOR_LOOP

C           Check to see if receptor is beyond the maximum
C           distance from source; assigned value of 80km 
C           for obsolescent TOXICS option or new FASTALL option; 
C           otherwise "unlimited" (1.0D20)
            IF (DISTR .GT. MAXDIST) CYCLE RECEPTOR_LOOP

C           Initialize HE for initial call to ADISY
            HE = HS
C           Set initial effective parameters
            UEFF  = US
            SVEFF = SVS
            SWEFF = SWS
            TGEFF = TGS
            IF ( UNSTAB  .AND.  (HS .LT. ZI) ) THEN
               UEFFD  = US
               SVEFFD = SVS
               SWEFFD = SWS
               UEFFN  = US
               SVEFFN = SVS
               SWEFFN = SWS
            END IF

C ---       Check to see if receptor is beyond edge of plume laterally.
            IF ( (DABS(Y)-0.5D0*WIDTH) .GT. 0.0D0) THEN
C ---          Receptor is outside "projected" width of effective area source
C              Calculate maximum sigma-y based on XMAXR and compare minimum 
C              lateral distance of receptor from source to 4*sigma-y
               CALL ADISY(XMAXR)
               IF ((DABS(Y)-0.5D0*WIDTH) .GE. 4.0D0*SY)
     &                                       CYCLE RECEPTOR_LOOP
            END IF

            IF(DEBUG) THEN
              WRITE(DBGUNT, 6015) UEFF, SVEFF, SWEFF
6015          FORMAT(//,5X,'Initial effective parameters for ',
     &          'stable or direct convective ',
     &          'plume:',//,5x,'Ueff = ',F7.2,' m/s; ',
     &          'SVeff = ',F7.2,
     &          ' m/s; SWeff = ',F7.2,' m/s.',/)
            END IF

C           Determine the CRITical Dividing Streamline---   CALL CRITDS
            CALL CRITDS (HE)

C           Set distance factor for point source approx. for FASTAREA option
C           based on "equivalent" PG stability class (KST)
            IF (URBSTAB) THEN
               VP_FACT = VIRTPNT_URB(KST)
            ELSE
               VP_FACT = VIRTPNT_RUR(KST)
            END IF

C           Calculate distance for switch to point source approx. for FASTAREA
            XPOINT = 1.5D0*LENGTH + VP_FACT*WIDTH
            IF (.NOT.FASTAREA .OR.
     &              (FASTAREA .AND. X .LT. XPOINT)) THEN
               IF (ARDPLETE) THEN

                  IF (NPD .EQ. 0 .AND. (LDGAS .OR. LWGAS)) THEN
                     CALL PDEPG (XDEP)
                  ELSE
                     DQCORG = 1.0D0
                     WQCORG = 1.0D0
                  END IF
                  IF (NPD .GT. 0 .AND. (LDPART .OR. LWPART)) THEN
                     CALL PDEP (XDEP)
                  ELSE IF (NPD .GT. 0) THEN
C                    Set DQCOR(NPD) and WQCOR(NPD) arrays to 1.0
                     DQCOR = 1.0D0
                     WQCOR = 1.0D0
                  END IF

               END IF

               DO ITYP = 1, NUMTYP
C                 Calculate Area Source Integral         ---   CALL AREAIN
                  CALL AREAIN
               END DO
            ELSE
C ---          Use point source approximation
C              Save emissions per unit area (QTK) and calculate total emissions
C              based on effective area (= XEFF*YEFF)
               QTKSAV = QTK
               QTK = QTK * XEFF * YEFF
               SYINIT = 0.0D0

C              Determine Deposition Correction Factors
               IF (NPD .EQ. 0 .AND. (LDGAS .OR. LWGAS)) THEN
                  CALL PDEPG (X)
               ELSE
                  DQCORG = 1.0D0
                  WQCORG = 1.0D0
               END IF
               IF (NPD .GT. 0 .AND. (LDPART .OR. LWPART)) THEN
                  CALL PDEP (X)
               ELSE IF (NPD .GT. 0) THEN
C                 Set DQCOR(NPD) and WQCOR(NPD) arrays to 1.0
                  DQCOR = 1.0D0
                  WQCOR = 1.0D0
               END IF

C              Define plume centroid height (CENTER) for use in
C              inhomogeniety calculations
               CALL CENTROID (X)

C              If the atmosphere is unstable and the stack
C              top is below the mixing height, calculate
C              the CBL PDF coefficients                     ---   CALL PDF
               IF( UNSTAB  .AND.  (HS .LT. ZI) ) THEN
                  CALL PDF
               END IF

C              Determine Effective Plume Height             ---   CALL HEFF
               CALL HEFF ( X )

C              Compute effective parameters using an
C              iterative average through plume rise layer
               CALL IBLVAL (X)

C              Call PDF & HEFF again for final CBL plume heights
               IF (UNSTAB .AND. (HS.LT.ZI) ) THEN
                  CALL PDF
                  CALL HEFF (X)
               END IF

C              Determine Dispersion Parameters              ---   CALL VDIS
               CALL VDIS (X)

C              Calculate the 'y-term' contribution to
C              dispersion, FSUBY, for coherent plume        ---   CALL FYPLM
               CALL FYPLM(SY,FYOUT)
               FSUBY  = FYOUT
               FSUBYD = FSUBY
               FSUBYN = FSUBYD

C              Set lateral term = 0.0 for penetrated source
               FSUBY3 = 0.0D0

C              Check for zero "y-terms"; if zero then skip calculations
C              and go to next receptor.
               IF( FSUBY.EQ.0.0D0 .AND. FSUBY3.EQ.0.0D0 )THEN
C                 Set HRVAL(NUMTYP) array to 0.0               
                  HRVAL = 0.0D0

               ELSE

                  IF (NPD .EQ. 0) THEN
C                    Perform calculations for gases
C                    Assign plume tilt, HV = 0.0

                     ADJ = DQCORG * WQCORG

                     IF (STABLE .OR. (UNSTAB.AND.(HS.GE.ZI))) THEN
C                       Calculate height of the "effective reflecting surface"
                        CALL REFL_HT (HE, X, 0.0D0, VSIGZ, HSBL)
                     ELSE IF ( UNSTAB ) THEN
                        HSBL = 0.0D0
                     END IF

C                    Determine the CRITical Dividing Streamline---   CALL CRITDS
                     CALL CRITDS (HE)

C                    Calculate the fraction of plume below
C                    HCRIT, PHEE                               ---   CALL PFRACT
                     CALL PFRACT (HE)

C                    Calculate FOPT = f(PHEE)                  ---   CALL FTERM
                     CALL FTERM

C                    Calculate Conc. for Virtual Point Source  ---   CALL AER_PCHI
                     CALL AER_PCHI( X, ADJ, VDEPG, 0, AEROUT )
                     HRVAL = AEROUT

                  ELSE
C                    Perform calculations for particles, loop through particle sizes

C                    Begin loop over particle sizes
                     DO J = 1, NPD

C                       Calculate Plume Tilt Due to Settling, HV
                        HV = (X/US) * VGRAV(J)

C                       Adjust Jth contribution by mass fraction and source
C                       depletion
                        ADJ = PHI(J) * DQCOR(J) * WQCOR(J)

                        IF (STABLE .OR. (UNSTAB.AND.(HS.GE.ZI))) THEN
C                          Calculate height of the "effective reflecting surface"
C                          Calculate Settled Plume Height(s), HESETL
                           HESETL = MAX( 0.0D0, HE - HV )
                           CALL REFL_HT (HESETL, X, 0.0D0, VSIGZ, HSBL)
                        ELSE IF ( UNSTAB ) THEN
C                          Calculate Settled Plume Height(s), HESETL
                           HESETL = MAX( 0.0D0, 0.5D0*(HED1+HED2) - HV )
                           HSBL = 0.0D0
                        END IF

C                       Determine the CRITical Dividing Streamline---   CALL CRITDS
                        CALL CRITDS (HESETL)

C                       Calculate the fraction of plume below
C                       HCRIT, PHEE                               ---   CALL PFRACT
                        CALL PFRACT (HESETL)

C                       Calculate FOPT = f(PHEE)                  ---   CALL FTERM
                        CALL FTERM

C                       Calculate Conc. for Virtual Point Source  ---   CALL AER_PCHI
                        CALL AER_PCHI( X, ADJ, VDEP(J), J, AEROUT )
                        HRVAL = HRVAL + AEROUT

                     END DO
C                    End loop over particle sizes

                  END IF
               END IF
               QTK = QTKSAV
            END IF

            IF (PVMRM .AND. .NOT.O3MISS) THEN
C ---          Store data by source and receptor for PVMRM option
               DO ITYP = 1, NUMTYP
                  CHI(IREC,ISRC,ITYP) = HRVAL(ITYP)
               END DO
               IF (STABLE .OR. (UNSTAB.AND.(HS.GE.ZI))) THEN
                  HECNTR(IREC,ISRC) = HE
                  UEFFS(IREC,ISRC)  = UEFF
               ELSE
                  HECNTR(IREC,ISRC) = CENTER
                  UEFFS(IREC,ISRC)  = UEFFD
               END IF
               IF (PPF .GT. 0.0D0) THEN
                  HECNTR3(IREC,ISRC) = HE3
                  PPFACT(ISRC)       = PPF
                  UEFF3S(IREC,ISRC)  = UEFF3
               ELSE
                  PPFACT(ISRC) = 0.0D0
               END IF
               FOPTS(IREC,ISRC) = FOPT
C              Cycle to next receptor & skip call to SUMVAL (will be done later)
               CYCLE RECEPTOR_LOOP
            ELSE IF (OLM .AND. .NOT.O3MISS) THEN
C ---          Store data by source and receptor for OLM option
               DO ITYP = 1, NUMTYP
                  CHI(IREC,ISRC,ITYP) = HRVAL(ITYP)
               END DO
C              Cycle to next receptor & skip call to SUMVAL (will be done later)
               CYCLE RECEPTOR_LOOP
            END IF

C           Sum HRVAL to AVEVAL and ANNVAL Arrays           ---   CALL SUMVAL
            IF (EVONLY) THEN
               CALL EV_SUMVAL
            ELSE
               CALL SUMVAL
            END IF

C           Initialize __VAL arrays (1:NUMTYP)
            HRVAL   = 0.0D0
            AERVAL  = 0.0D0
            AEROUT  = 0.0D0

         END DO RECEPTOR_LOOP
C        End Receptor LOOP

C        Output 'ARC' Values for EVALFILE                   ---   CALL EVALFL
         IF (EVAL(ISRC)) THEN
            CALL EVALFL
         END IF

      END IF

 999  RETURN
      END


      SUBROUTINE SETSRC
C***********************************************************************
C             SETSRC Module of the AMS/EPA Regulatory Model - AERMOD
c ----------------------------------------------------------------------
c ---    ISC-PRIME     Version 1.0    Level 970812              Modified
c ---        D. Strimaitis
c ---        Earth Tech, Inc.
c            Prepared for EPRI under contract WO3527-01
c ----------------------------------------------------------------------
C
C        PURPOSE: Sets the Source Parameters for a Particular Source
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To initialize AREA source dimensions and initial 
C                    sigmas to correct initialization problems with PVMRM.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/28/2011
C
C                    To incorporate factor to adjust initial diameter of
C                    plume for capped stacks for use in the PRIME
C                    algorithm, for BETA-test draft option.
C                    R. W. Brode, U.S. EPA/OAQPS/AQMG, 12/07/06
C
C        MODIFIED:   To incorporate inputs for numerical integration
C                    algorithm for AREA source - 7/7/93
C
C        INPUTS:  Source Parameters Arrays
C                 Source Index
C
C        OUTPUTS: Source Parameters for a Particular Source
C
C        CALLED FROM:   PCALC
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: J

C     Variable Initializations
      MODNAM = 'SETSRC'

C --- Initialize AREA source dimensions and initial sigmas
C     to avoid initialization problems with PVMRM
      XINIT  = 0.0D0
      YINIT  = 0.0D0
      SYINIT = 0.0D0
      SZINIT = 0.0D0
      ANGLE  = 0.0D0
      PALPHA = 0.0D0
      PDEFF  = 0.0D0
      PITLEN = 0.0D0
      PITWID = 0.0D0
      
C     Assign The Values From Array Elements To Variables
      IF (SRCTYP(ISRC)(1:5) .EQ. 'POINT') THEN
         XS = AXS(ISRC)
         YS = AYS(ISRC)
         ZS = AZS(ISRC)
         QS = AQS(ISRC)
         HS = AHS(ISRC)

         DS = ADS(ISRC)
         VS = AVS(ISRC)
         TS = ATS(ISRC)

         IF (SRCTYP(ISRC) .EQ. 'POINTCAP') THEN
C           Assign factor to adjust initial diameter of plume for
C           capped stacks for use in PRIME algorithm.
            DSFACT = ADSFACT(ISRC)
         ELSE
            DSFACT = 1.0D0
         END IF

      ELSE IF (SRCTYP(ISRC) .EQ. 'VOLUME') THEN
         XS = AXS(ISRC)
         YS = AYS(ISRC)
         ZS = AZS(ISRC)
         QS = AQS(ISRC)
         HS = AHS(ISRC)

         SYINIT = ASYINI(ISRC)
         SZINIT = ASZINI(ISRC)

      ELSE IF (SRCTYP(ISRC) .EQ. 'AREA') THEN
         XS = AXS(ISRC)
         YS = AYS(ISRC)
         ZS = AZS(ISRC)
         QS = AQS(ISRC)
         HS = AHS(ISRC)

         XINIT = AXINIT(ISRC)
         YINIT = AYINIT(ISRC)
         ANGLE = AANGLE(ISRC)

         SZINIT = ASZINI(ISRC)
         NVERT  = 4

C        Store Vertices in Temporary Arrays
         DO IVERT = 1, NVERT+1
            XVERT(IVERT) = AXVERT(IVERT,ISRC)
            YVERT(IVERT) = AYVERT(IVERT,ISRC)
         END DO

         XCNTR = AXCNTR(ISRC)
         YCNTR = AYCNTR(ISRC)

      ELSE IF (SRCTYP(ISRC) .EQ. 'AREAPOLY') THEN
         XS = AXS(ISRC)
         YS = AYS(ISRC)
         ZS = AZS(ISRC)
         QS = AQS(ISRC)
         HS = AHS(ISRC)

         SZINIT = ASZINI(ISRC)
         NVERT  = NVERTS(ISRC)

C        Store Vertices in Temporary Arrays
         DO IVERT = 1, NVERT+1
            XVERT(IVERT) = AXVERT(IVERT,ISRC)
            YVERT(IVERT) = AYVERT(IVERT,ISRC)
         END DO

C        Assign equivalent values of XINIT and YINIT for calculating area
         XINIT = AXINIT(ISRC)
         YINIT = AYINIT(ISRC)

C        Assign centroid of polygon
         XCNTR = AXCNTR(ISRC)
         YCNTR = AYCNTR(ISRC)

      ELSE IF (SRCTYP(ISRC) .EQ. 'AREACIRC') THEN
         XS = AXS(ISRC)
         YS = AYS(ISRC)
         ZS = AZS(ISRC)
         QS = AQS(ISRC)
         HS = AHS(ISRC)

         SZINIT = ASZINI(ISRC)
         NVERT  = NVERTS(ISRC)

C        Store Vertices in Temporary Arrays
         DO IVERT = 1, NVERT+1
            XVERT(IVERT) = AXVERT(IVERT,ISRC)
            YVERT(IVERT) = AYVERT(IVERT,ISRC)
         END DO

C        Assign equivalent values of XINIT and YINIT for calculating area
         XINIT = AXINIT(ISRC)
         YINIT = AYINIT(ISRC)

         XCNTR = AXCNTR(ISRC)
         YCNTR = AYCNTR(ISRC)

      ELSE IF (SRCTYP(ISRC) .EQ. 'OPENPIT') THEN
         XS = AXS(ISRC)
         YS = AYS(ISRC)
         ZS = AZS(ISRC)
         QS = AQS(ISRC)
C        Set Emission Height of Effective Area, HS = 0.0
         HS = 0.0D0
C        Set Height of Emissions Above Base of Pit, EMIHGT
         EMIHGT = AHS(ISRC)
         NVERT  = 4

         XINIT = AXINIT(ISRC)
         YINIT = AYINIT(ISRC)
         ANGLE = AANGLE(ISRC)
         PALPHA = AALPHA(ISRC)
         PDEFF  = APDEFF(ISRC)
         SZINIT = ASZINI(ISRC)
         PITLEN = MAX(XINIT,YINIT)
         PITWID = MIN(XINIT,YINIT)

C        Store Vertices in Temporary Arrays
         DO IVERT = 1, NVERT+1
            XVERT(IVERT) = AXVERT(IVERT,ISRC)
            YVERT(IVERT) = AYVERT(IVERT,ISRC)
         END DO

         XCNTR = AXCNTR(ISRC)
         YCNTR = AYCNTR(ISRC)

      END IF

      NPD = INPD(ISRC)
      IF (NPD .GT. 0) THEN
         DO J = 1, NPD
            PDIAM(J) = APDIAM(J,ISRC)
            PHI(J)   = APHI(J,ISRC)
            PDENS(J) = APDENS(J,ISRC)
            VGRAV(J) = AVGRAV(J,ISRC)
            TSTOP(J) = ATSTOP(J,ISRC)
         END DO
      END IF

C     Initialize SURFAC variable
      SURFAC = .FALSE.

      RETURN
      END

      SUBROUTINE FLUXES(VSEQ)
C***********************************************************************
C             FLUXES Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Calculates the source momentum and buoyancy fluxes
C
C        PROGRAMMER: Roger Brode and Jim Paumier, PES, Inc.
C
C        DATE:    September 30, 1993
C
C        MODIFIED:
C                  To incorporate capped stack (POINTCAP) and
C                  horizontal release (POINTHOR) options.
C                  R. W. Brode, MACTEC (f/k/a PES), Inc., 09/30/05
C                  R. W. Brode, U.S. EPA/OAQPS/AQMG, 12/07/06
C
C        INPUTS:  Ambient temperature at source height, TA
C                 Source gas exit temperature, TS
C                 Source gas exit velocity, VS
C                 Source diameter, DS
C
C        OUTPUTS: Momentum flux, FM, and buoyancy flux, FB
C
C        CALLED FROM:   PCALC
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      DOUBLE PRECISION  TSEQ, DSEQ, VSEQ

C     Variable Initializations
      MODNAM = 'FLUXES'

C     Note:  TA is now ambient temperature AT STACK HEIGHT and
C            was computed in METINI

C     Check for Negative Stack Temperature, Used to
C     Indicate Constant TS-TA
      IF (TS .LT. 0.0D0) THEN
         TS = TA + DABS(TS)
      END IF

      IF (TS .LT. TA)  TS = TA
      FB = (0.25D0 / TS) * (VS * DS * DS) * G * (TS - TA)
      FM = (0.25D0 / TS) * (VS * DS * DS) * VS * TA

C     Set effective exit velocity, diameter and momentum flux for
C     capped stack (POINTCAP) or horizontal release (POINTHOR).
      IF (SRCTYP(ISRC) .EQ. 'POINTCAP') THEN
         VSEQ = 0.001D0
         DSEQ = DS * DSQRT( VS/VSEQ )
         TSEQ = TS
         FM = (0.25D0 / TSEQ) * (VSEQ * DSEQ * DSEQ) * VSEQ * TA
      ELSE IF (SRCTYP(ISRC) .EQ. 'POINTHOR') THEN
         VSEQ = 0.001D0
         DSEQ = DS * DSQRT( VS/VSEQ )
         TSEQ = TS
         FM = (0.25D0 / TSEQ) * (VSEQ * DSEQ * DSEQ) * VSEQ * TA
      ELSE
         VSEQ = VS
         DSEQ = DS
         TSEQ = TS
      END IF

C     To avoid divide by zero or underflow, set FB and FM to a minimum value
      IF( FB .LT. 1.0D-10 ) FB = 1.0D-10
      IF( FM .LT. 1.0D-10 ) FM = 1.0D-10

      RETURN
      END

      SUBROUTINE HEFF ( XARG )
C***********************************************************************
C             HEFF Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Calculates Effective Plume Height (m)
C
C        PROGRAMMER: Roger Brode and Jim Paumier, PES, Inc.
C
C        DATE:    September 30, 1993
C
C        REVISIONS:  Corrected formulations for HEN1 & HEN2 per
C                    Model Formulation Document and conversation
C                    with Russ Lee and Jeff Weil.
C                    Roger Brode, PES, Inc. - 12/7/94
C
C        INPUTS:  Arrays of Source Parameters
C                 Logical Wake Flags
C                 Meteorological Variables for One Hour
C                 Wind Speed Adjusted to Stack Height
C                 Downwind Distance
C                 Terrain Elevation of Receptor
C
C        OUTPUTS: Effective Plume Height (HE)
C
C        CALLED FROM:   PCALC, VCALC, ACALC, PLUMEF
C
C   References:   "A Dispersion Model for the Convective Boundary
C                  Layer", J. Weil, 8/17/93
C                 "Plume Penetration of the CBL and Source 3: Source
C                  Strength and Plume Rise", J. Weil, 9/1/93
C
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      DOUBLE PRECISION :: XARG

C     Variable Initializations
      MODNAM = 'HEFF'

C     Compute the effective plume height
      IF( STABLE  .OR.  (UNSTAB .AND. (HS .GE. ZI) ) )THEN
C        The atmosphere is stable or the release is above the CBL
C        mixing ht.
         HE = HSP + DHP
C        Don't Allow Effective Plume Height to be < 0.0
         HE = MAX( 0.0D0, HE)

      ELSEIF( UNSTAB )THEN
C        The atmosphere is unstable and the release is below the
C        mixing ht.

C        Compute the effective direct plume height, for both the
C        Plume 1 (HED1) and Plume 2 (HED2)
         HED1 = HSP + DHP1 + (ASUB1 * WSTAR * XARG / UEFFD)
         HED2 = HSP + DHP1 + (ASUB2 * WSTAR * XARG / UEFFD)

C        Compute the effective indirect plume height, for both the
C        updraft (HEN1) and downdraft (HEN2)

         HEN1 = HSP + DHP1 - DHP2 + (ASUB1 * WSTAR * XARG / UEFFN)
         HEN2 = HSP + DHP1 - DHP2 + (ASUB2 * WSTAR * XARG / UEFFN)

C        Compute the plume height for the penetrated source
C        (See Eq. 8 in the reference for Source 3)
         HE3 = HSP + DHP3

      END IF

      RETURN
      END

      SUBROUTINE PRMHEFF
C***********************************************************************
C             PRMHEFF Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Calculates Effective Plume Height (m)
C
C        PROGRAMMER: Roger Brode and Jim Paumier, PES, Inc.
C
C        DATE:    September 30, 1993
C
C        REVISIONS:  Corrected formulations for HEN1 & HEN2 per
C                    Model Formulation Document and conversation
C                    with Russ Lee and Jeff Weil.
C                    Roger Brode, PES, Inc. - 12/7/94
C
C        INPUTS:  Arrays of Source Parameters
C                 Logical Wake Flags
C                 Meteorological Variables for One Hour
C                 Wind Speed Adjusted to Stack Height
C                 Downwind Distance
C                 Terrain Elevation of Receptor
C
C        OUTPUTS: Effective Plume Height (HE)
C
C        CALLED FROM:   PCALC, VCALC, ACALC, PLUMEF
C
C   References:   "A Dispersion Model for the Convective Boundary
C                  Layer", J. Weil, 8/17/93
C                 "Plume Penetration of the CBL and Source 3: Source
C                  Strength and Plume Rise", J. Weil, 9/1/93
C
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'PRMHEFF'

C     Compute the effective plume height
      IF( STABLE )THEN
C        The atmosphere is stable or the release is above the CBL
C        mixing ht.

         HE = HS + DHP
C        Don't Allow Effective Plume Height to be < 0.0
         HE = MAX( 0.0D0, HE)

      ELSE IF( UNSTAB )THEN
C        The atmosphere is unstable and the release is below the
C        mixing ht.

         HE = HS + DHP
C        Don't Allow Effective Plume Height to be < 0.0
         HE = MAX( 0.0D0, HE)
      END IF

      RETURN
      END

      SUBROUTINE PDIS ( XARG )
C***********************************************************************
C             PDIS module of the AMS/EPA Regulatory Model - AERMOD
c ----------------------------------------------------------------------
c ---    ISC-PRIME     Version 1.0    Level 970812              Modified
c ---        D. Strimaitis
c ---        Earth Tech, Inc.
c            Prepared for EPRI under contract WO3527-01
c ----------------------------------------------------------------------
C
C        PURPOSE: Calculates Dispersion Parameters for POINT Sources
C
C        PROGRAMMER: Roger Brode and Jim Paumier, PES, Inc.
C
C        DATE:    Spetember 30, 1993
C
C        REVISIONS:  SZSURF calculation reinstated 7/13/94, R.F. Lee
C
C        INPUTS:  Arrays of Source Parameters
C                 Logical Wake Flags
C                 Wake Plume Height, HEMWAK
C                 Meteorological Variables for One Hour
C                 Distance, XARG
C
C        OUTPUTS: Lateral and Vertical Dispersion Coefficients, SY and SZ
C
C        CALLED FROM:   PCALC
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      DOUBLE PRECISION :: XARG

C     Variable Initializations
      MODNAM = 'PDIS'

C     Calculate Sigma-y from formulae                 --- CALL SIGY
      CALL SIGY ( XARG )
C     Calculate Sigma-z from formuale                 --- CALL SIGZ
      CALL SIGZ ( XARG )

C     Set all virtual source terms to 0 for non-downwashing sources
      VSIGY = 0.0D0
      VSYN  = 0.0D0
      VSIGZ = 0.0D0
      VSZD1 = 0.0D0
      VSZD2 = 0.0D0
      VSZN1 = 0.0D0
      VSZN2 = 0.0D0
      VSZ3  = 0.0D0
      VSY3  = 0.0D0

C     Calculate the buoyancy-induced dispersion parameters
      IF( NOBID )THEN
C        Set BID Terms to 0.0
         IF( STABLE  .OR.  (UNSTAB .AND. (HS .GE. ZI) ) )THEN
            SYB = 0.0D0
            SZB = 0.0D0

         ELSE IF( UNSTAB )THEN
            SYB  = 0.0D0
            SZBD = 0.0D0
            SZBN = 0.0D0
            SYB3 = 0.0D0
            SZB3 = 0.0D0

         END IF

      ELSE
C        Specify BID Terms                                 --- CALL BID
         CALL BID

      END IF

C---- Calculate the root-mean-square sigma_Y and sigma_Z   --- CALL RMSSIG
      CALL RMSSIG

      RETURN
      END

      SUBROUTINE VDIS (XARG)
C***********************************************************************
C             VDIS Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Calculates Dispersion Parameters for VOLUME Sources
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Arrays of Source Parameters
C                 Meteorological Variables for One Hour
C                 Downwind Distance
C
C        OUTPUTS: Lateral and Vertical Dispersion Coefficients
C
C        CALLED FROM:   VCALC
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      DOUBLE PRECISION :: XARG

C     Variable Initializations
      MODNAM = 'VDIS'

C     Calculate Sigma-y from formulae                 --- CALL SIGY
      CALL SIGY ( XARG )
C     Calculate Sigma-z from formulae                 --- CALL SIGZ
      CALL SIGZ ( XARG )

C     Set virtual source terms based on initial sigmas input by user
      VSIGY = SYINIT
      VSYN  = SYINIT
      VSIGZ = SZINIT
      VSZD1 = SZINIT
      VSZD2 = SZINIT
      VSZN1 = SZINIT
      VSZN2 = SZINIT
      VSZ3  = 0.0D0
      VSY3  = 0.0D0

C     Set BID terms to zero
      SYB  = 0.0D0
      SZB  = 0.0D0
      SZBD = 0.0D0
      SZBN = 0.0D0
      SYB3 = 0.0D0
      SZB3 = 0.0D0

C---- Calculate the root-mean-square sigma_Y and sigma_Z   --- CALL RMSSIG
      CALL RMSSIG

      RETURN
      END

      SUBROUTINE ADISY(XARG)
C***********************************************************************
C                 ADISY Module of the AERMOD Model
C
C        PURPOSE: Calculates Lateral Dispersion Parameters for AREA Sources
C
C        PROGRAMMER: Roger Brode, PES, Inc.
C
C        DATE:    July 21, 1994
C
C        MODIFIED:   To calculate sigma-y and sigma-z separately
C                    for AREA source - R.Brode, PES, 12/9/98
C
C        INPUTS:  Arrays of Source Parameters
C                 Meteorological Variables for One Hour
C                 Downwind Distance
C
C        OUTPUTS: Lateral and Vertical Dispersion Coefficients
C
C        CALLED FROM:   VCALC
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      DOUBLE PRECISION :: XARG

C     Variable Initializations
      MODNAM = 'ADISY'

C     Calculate Sigma-y from formulae                 --- CALL SIGY
      CALL SIGY ( XARG )

C     Set virtual source terms based on initial sigmas input by user
      VSIGY = 0.0D0
      VSY3  = 0.0D0

C     Set BID terms to zero
      SYB  = 0.0D0
      SYB3 = 0.0D0

      SY = SYAMB

      RETURN
      END

      SUBROUTINE ADISZ(XARG)
C***********************************************************************
C                 ADISZ Module of the AERMOD Model
C
C        PURPOSE: Calculates Vertical Dispersion Parameters for AREA Sources
C
C        PROGRAMMER: Roger Brode, PES, Inc.
C
C        DATE:    July 21, 1994
C
C        MODIFIED:   To calculate sigma-y and sigma-z separately
C                    for AREA source - R.Brode, PES, 12/9/98
C
C        INPUTS:  Arrays of Source Parameters
C                 Meteorological Variables for One Hour
C                 Downwind Distance
C
C        OUTPUTS: Lateral and Vertical Dispersion Coefficients
C
C        CALLED FROM:   VCALC
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      DOUBLE PRECISION :: XARG

C     Variable Initializations
      MODNAM = 'ADISZ'

C     Calculate Sigma-z from formulae                 --- CALL SIGZ
      CALL SIGZ ( XARG )

C     Set virtual source terms based on initial sigmas input by user
      VSIGZ = SZINIT
      VSZD1 = SZINIT
      VSZD2 = SZINIT
      VSZN1 = SZINIT
      VSZN2 = SZINIT
      VSZ3  = 0.0D0

C     Set BID terms to zero
      SZB  = 0.0D0
      SZBD = 0.0D0
      SZBN = 0.0D0
      SZB3 = 0.0D0

C---- Calculate the root-mean-square sigma_Y and sigma_Z   --- CALL RMSSIG
      CALL RMSSIG

      RETURN
      END

      SUBROUTINE AER_PCHI( XARG, ADJ, VDINP, JIN, AEROUT )
C***********************************************************************
C        AER_PCHI Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Calculates Hourly Concentration for POINT Sources
C                 Using Gaussian Plume Equation
C
C        PROGRAMMER: Roger Brode, PES, Inc.
C
C        DATE:    November 10, 2000
C
C        MODIFIED:   To include lateral term (FSUBY) in weighting of
C                    direct and penetrated contributions for wet dep.
C                    Added debug statement for CONC based on ENSR.
C                    - R.Brode, MACTEC, 7/27/2004
C
C        MODIFIED:   To correct WETFLUX values for conversion from
C                    seconds to hours and to include SQRT(2PI) in
C                    denominator of integrated vertical term.
C                    - R.Brode, MACTEC, 3/9/2004
C
C        INPUTS:  Distance, XARG (downwind for plume; radial for pancake)
C                 Crosswind Distance
C                 Plume Height
C                 Stack Top Wind Speed
C                 Lateral Dispersion Parameter
C                 Vertical Dispersion Parameter
C                 Stability Class
C                 Mixing Height
C                 Receptor Height Above Ground
C                 Emission Rate and Units Scaling Factor
C                 Source Parameter Arrays
C
C        OUTPUTS: AEROUT, AERMOD Concentration for Particular
C                 Source/Receptor Combination
C
C        CALLED FROM:   AERCALC, VOLCALC, ACALC
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      INTEGER :: JIN
      DOUBLE PRECISION :: AEROUT(NUMTYP), XARG, ADJ, VDINP, DRYFLUX,
     &                    WETFLUX
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'AER_PCHI'
      DRYFLUX = 0.0D0
      WETFLUX = 0.0D0

C---- Calculate the contribution due to horizontal plume, CWRAP
      IF (FOPT .EQ. 0.0D0) THEN
         CWRAP = 0.0D0
      ELSE
         CALL CPLUME (ZRT, CWRAP)
      END IF

C---- Calculate the contribution due to terrain-following plume, CLIFT
      IF (ZRT .EQ. ZFLAG) THEN
C----    Effective receptor heights are equal, therefore CLIFT = CWRAP
         CLIFT = CWRAP
      ELSE IF (FOPT .EQ. 1.0D0) THEN
         CLIFT = 0.0D0
      ELSE
         CALL CPLUME (ZFLAG, CLIFT)
      END IF

C---- Calculate the exponential decay term, D               ---   CALL DECAY
      Call DECAY (XARG)

C---- Calculate the hourly concentration and deposition values
      ITYP = 0
      IF (CONC) THEN
         ITYP = 1
         AEROUT(ITYP) = ADJ * EMIFAC(ITYP) *
     &                 (FOPT * CWRAP + (1.0D0 - FOPT) * CLIFT) * D

C   ENHANCEMENT TO DEBUG OUTPUT BASED ON ENSR
         IF (DEBUG) THEN
            WRITE(DBGUNT,10) ITYP, ADJ, FOPT, CWRAP, CLIFT, D,
     &                       AEROUT(ITYP)
10          FORMAT(/,'ITYP = ',I2,' - CONC:',
     &             /,'AEROUT(ITYP) = ADJ * EMIFAC(ITYP) * (FOPT * ',
     &               'CWRAP + (1.0 -FOPT) * CLIFT) * D',
     &             /,' ADJ   = ',G16.8,
     &             /,' FOPT  = ',G16.8,
     &             /,' CWRAP = ',G16.8,
     &             /,' CLIFT = ',G16.8,
     &             /,' D     = ',G16.8,
     &             /,' AEROUT(ITYP) = ',G16.8,/)
         END IF

      END IF

      IF (DEPOS .OR. DDEP) THEN
C        Calculate DRYFLUX, vertical term for wet deposition
C----    Calculate the contribution due to horizontal plume, CWRAP
         IF (FOPT .EQ. 0.0D0) THEN
            CWRAP = 0.0D0
         ELSE
            CALL CPLUME (ZRT-ZFLAG+ZRDEP, CWRAP)
         END IF

C----    Calculate the contribution due to terrain-following plume, CLIFT
         IF (ZRT .EQ. ZFLAG) THEN
C----       Effective receptor heights are equal, therefore CLIFT = CWRAP
            CLIFT = CWRAP
         ELSE IF (FOPT .EQ. 1.0D0) THEN
            CLIFT = 0.0D0
         ELSE
            CALL CPLUME (ZRDEP, CLIFT)
         END IF

         DRYFLUX = (FOPT * CWRAP + (1.0D0 - FOPT) * CLIFT) * D
     &
      END IF
      
      IF (DEPOS .OR. WDEP) THEN
C        Calculate WETFLUX, vertical term for wet deposition.
C        Note that the SRT2PI for the integrated vertical term
C        has been removed since it should be divided by SRT2PI.
C        Additional factor of 3600. has been added to denominator
C        to account for conversion from seconds to hours when
C        divided by wind speed below.
         IF (PRATE .GT. 0.0D0) THEN
            IF (NPD .EQ. 0) THEN
               WETFLUX = (ADJ*FRACSAT*PRATE*1.0D6*RGAS*TA)/
     &                   (ZSUBP*HENRY(ISRC)*1.0D9*DENOM*3600.0D0)
            ELSE
               WETFLUX = 1.0D-3*ADJ*WASHOUT(JIN)*PRATE/
     &                   (ZSUBP*3600.0D0)
            END IF
         ELSE
            WETFLUX = 0.0D0
         END IF
      END IF

      IF (DEPOS) THEN
         ITYP = ITYP + 1
         IF( STABLE  .OR.  (UNSTAB .AND. (HS .GE. ZI) ) ) THEN
            AEROUT(ITYP) = ADJ * VDINP * EMIFAC(ITYP) * DRYFLUX +
     &                     QTK * WETFLUX * EMIFAC(ITYP) * FSUBY/UEFF
         ELSE IF (UNSTAB) THEN
            AEROUT(ITYP) = ADJ * VDINP * EMIFAC(ITYP) * DRYFLUX +
     &                     QTK * WETFLUX * EMIFAC(ITYP) *
     &                     (PPF*FSUBY3/UEFF3+(1.0D0-PPF)*FSUBY/UEFFD)
         END IF

         IF (DEBUG) THEN
            WRITE(DBGUNT,11) ITYP, ADJ, VDINP, DRYFLUX, WETFLUX,
     &                       AEROUT(ITYP)
11          FORMAT(/,'ITYP = ',I2,' - DEPOS:',
     &             /,' ADJ     = ',G16.8,
     &             /,' VDINP   = ',G16.8,
     &             /,' DRYFLUX = ',G16.8,
     &             /,' WETFLUX = ',G16.8,
     &             /,' AEROUT(ITYP) = ',G16.8,/)
         END IF

      END IF

      IF (DDEP) THEN
         ITYP = ITYP + 1
         AEROUT(ITYP) = ADJ * VDINP * EMIFAC(ITYP) * DRYFLUX

         IF (DEBUG) THEN
            WRITE(DBGUNT,12) ITYP, ADJ, VDINP, DRYFLUX,
     &                       AEROUT(ITYP)
12          FORMAT(/,'ITYP = ',I2,' - DDEP:',
     &             /,' ADJ     = ',G16.8,
     &             /,' VDINP   = ',G16.8,
     &             /,' DRYFLUX = ',G16.8,
     &             /,' AEROUT(ITYP) = ',G16.8,/)
         END IF

      END IF

      IF (WDEP) THEN
         ITYP = ITYP + 1
         IF( STABLE  .OR.  (UNSTAB .AND. (HS .GE. ZI) ) ) THEN
            AEROUT(ITYP) = QTK * WETFLUX * EMIFAC(ITYP) * FSUBY/UEFF
         ELSE IF (UNSTAB) THEN
            AEROUT(ITYP) = QTK * WETFLUX * EMIFAC(ITYP) *
     &                     (PPF*FSUBY3/UEFF3+(1.0D0-PPF)*FSUBY/UEFFD)
         END IF

         IF (DEBUG) THEN
            WRITE(DBGUNT,13) ITYP, ADJ, ZSUBP, PRATE, WETFLUX,
     &                       AEROUT(ITYP)
13         FORMAT(/,'ITYP = ',I2,' - WDEP:',
     &             /,' ADJ     = ',G16.8,
     &             /,' ZSUBP   = ',G16.8,
     &             /,' PRATE   = ',G16.8,
     &             /,' WETFLUX = ',G16.8,
     &             /,' AEROUT(ITYP) = ',G16.8,/)
         END IF

      END IF


CCRFL Call to METDEB was moved here from METEXT on 9/26/94, R.F. Lee.
CCRFL Print meteorological debug output.                   ---   CALL METDEB
      IF (METEOR) CALL METDEB

      IF ( DEBUG ) THEN
C        Print Out Debugging Information                    ---   CALL DEBOUT
         CALL DEBOUT
      END IF

      RETURN
      END

      SUBROUTINE PRM_PCHI(ADJ, VDINP, JIN)
C***********************************************************************
C        PRM_PCHI Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Calculates Hourly Concentration for POINT Sources
C                 with PRIME Downwash Algorithm
C
C        PROGRAMMER: Roger Brode, PES, Inc.
C
C        DATE:     November 10, 2000
C
C        MODIFIED:   To correct WETFLUX values for conversion from
C                    seconds to hours and to include SQRT(2PI) in
C                    denominator of integrated vertical term.
C                    - R.Brode, MACTEC, 3/9/2004
C
C        INPUTS:  Downwind Distance
C                 Crosswind Distance
C                 Plume Height
C                 Stack Top Wind Speed
C                 Lateral Dispersion Parameter
C                 Vertical Dispersion Parameter
C                 Stability Class
C                 Mixing Height
C                 Receptor Height Above Ground
C                 Emission Rate and Units Scaling Factor
C                 Source Parameter Arrays
C
C        OUTPUTS: PRMVAL, PRIME Concentration for Particular
C                 Source/Receptor Combination, summed across
C                 three PRIME "sources", i.e., primary source,
C                 inside cavity source and outside cavity source
C
C        CALLED FROM:   PRMCALC
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: JIN
      DOUBLE PRECISION :: ADJ, VDINP, DRYFLUX, WETFLUX

C     Variable Initializations
      MODNAM = 'PRM_PCHI'
      DRYFLUX = 0.0D0
      WETFLUX = 0.0D0

C---- Calculate the exponential decay term, D               ---   CALL DECAY
      Call DECAY (X)

C---- Calculate the hourly concentration value
      ITYP = 0
      IF (CONC) THEN
         ITYP = 1
C----    Calculate the contribution due to horizontal plume, CWRAP
         IF (FOPT .EQ. 0.0D0) THEN
            CWRAP = 0.0D0
         ELSE
            CALL PRM_PLUME (ZRT, CWRAP)
         END IF

C----    Calculate the contribution due to terrain-following plume, CLIFT
         IF (ZRT .EQ. ZFLAG) THEN
C----       Effective receptor heights are equal, therefore CLIFT = CWRAP
            CLIFT = CWRAP
         ELSE IF (FOPT .EQ. 1.0D0) THEN
            CLIFT = 0.0D0
         ELSE
            CALL PRM_PLUME (ZFLAG, CLIFT)
         END IF

         PRMVAL(ITYP) = PRMVAL(ITYP) + ADJ * EMIFAC(ITYP) *
     &                 (FOPT * CWRAP + (1.0D0 - FOPT) * CLIFT) * D
      END IF

      IF (DEPOS .OR. DDEP) THEN
C        Calculate DRYFLUX, vertical term for wet deposition
C----    Calculate the contribution due to horizontal plume, CWRAP
         IF (FOPT .EQ. 0.0D0) THEN
            CWRAP = 0.0D0
         ELSE
            CALL PRM_PLUME (ZRT-ZFLAG+ZRDEP, CWRAP)
         END IF

C----    Calculate the contribution due to terrain-following plume, CLIFT
         IF (ZRT .EQ. ZFLAG) THEN
C----       Effective receptor heights are equal, therefore CLIFT = CWRAP
            CLIFT = CWRAP
         ELSE IF (FOPT .EQ. 1.0D0) THEN
            CLIFT = 0.0D0
         ELSE
            CALL PRM_PLUME (ZRDEP, CLIFT)
         END IF

         DRYFLUX = (FOPT * CWRAP + (1.0D0 - FOPT) * CLIFT) * D
      END IF

      IF (DEPOS .OR. WDEP) THEN
C        Calculate WETFLUX, vertical term for wet deposition
C        Note that the SRT2PI for the integrated vertical term
C        has been removed since it should be divided by SRT2PI.
C        Additional factor of 3600. has been added to denominator
C        to account for conversion from seconds to hours when
C        divided by wind speed below.
         IF (PRATE .GT. 0.0D0) THEN
            IF (NPD .EQ. 0) THEN
               WETFLUX = (ADJ*FRACSAT*PRATE*1.0D6*RGAS*TA)/
     &                   (ZSUBP*HENRY(ISRC)*1.0D9*DENOM*3600.0D0)
            ELSE
               WETFLUX = 1.0D-3*ADJ*WASHOUT(JIN)*PRATE/
     &                   (ZSUBP*3600.0D0)
            END IF
         ELSE
            WETFLUX = 0.0D0
         END IF
      END IF

      IF (DEPOS) THEN
         ITYP = ITYP + 1
         PRMVAL(ITYP) = PRMVAL(ITYP) + ADJ*VDINP*EMIFAC(ITYP)*DRYFLUX +
     &                  QTK * WETFLUX * EMIFAC(ITYP) * FSUBY/UEFF
      END IF

      IF (DDEP) THEN
         ITYP = ITYP + 1
         PRMVAL(ITYP) = PRMVAL(ITYP) + ADJ*VDINP*EMIFAC(ITYP)*DRYFLUX
      END IF

      IF (WDEP) THEN
         ITYP = ITYP + 1
         PRMVAL(ITYP) = PRMVAL(ITYP)+QTK*WETFLUX*EMIFAC(ITYP)*FSUBY/UEFF
      END IF

      IF ( DEBUG ) THEN
C        Print Out Debugging Information                    ---   CALL DEBOUT
         CALL DEBOUT
      END IF

      RETURN
      END

      SUBROUTINE AER_ACHI( XARG, ADJ, VDINP, JIN, FYARG, POUT )
C***********************************************************************
C        AER_ACHI Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Calculates Hourly Concentration for AREA Sources
C                 Using Gaussian Plume Equation
C
C        PROGRAMMER: Roger Brode, PES, Inc.
C
C        DATE:    November 10, 2000
C
C        MODIFIED:   To correct WETFLUX values for conversion from
C                    seconds to hours and to include SQRT(2PI) in
C                    denominator of integrated vertical term.
C                    - R.Brode, MACTEC, 3/9/2004
C
C        INPUTS:  Distance, XARG (downwind for plume; radial for pancake)
C                 Crosswind Distance
C                 Plume Height
C                 Stack Top Wind Speed
C                 Lateral Dispersion Parameter
C                 Vertical Dispersion Parameter
C                 Stability Class
C                 Mixing Height
C                 Receptor Height Above Ground
C                 Emission Rate and Units Scaling Factor
C                 Source Parameter Arrays
C
C        OUTPUTS: AEROUT, AERMOD Concentration for Particular
C                 Source/Receptor Combination
C
C        CALLED FROM:   AERCALC, VOLCALC, ACALC
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      INTEGER :: JIN
      DOUBLE PRECISION :: POUT, XARG, ADJ, VDINP, FYARG, DRYFLUX, 
     &                    WETFLUX
      LOGICAL :: SCONC, SDEPOS, SDDEP, SWDEP
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'AER_ACHI'
      DRYFLUX = 0.0D0
      WETFLUX = 0.0D0

C     Determine appropriate output type for this ITYP, assign output type
C     logicals to local variables, and set others to .FALSE.
      IF (OUTTYP(ITYP) .EQ. 'CONC') THEN
         SCONC  = .TRUE.
         SDEPOS = .FALSE.
         SDDEP  = .FALSE.
         SWDEP  = .FALSE.
      ELSE IF (OUTTYP(ITYP) .EQ. 'DEPOS') THEN
         SCONC  = .FALSE.
         SDEPOS = .TRUE.
         SDDEP  = .FALSE.
         SWDEP  = .FALSE.
      ELSE IF (OUTTYP(ITYP) .EQ. 'DDEP') THEN
         SCONC  = .FALSE.
         SDEPOS = .FALSE.
         SDDEP  = .TRUE.
         SWDEP  = .FALSE.
      ELSE IF (OUTTYP(ITYP) .EQ. 'WDEP') THEN
         SCONC  = .FALSE.
         SDEPOS = .FALSE.
         SDDEP  = .FALSE.
         SWDEP  = .TRUE.
      ELSE   ! this condition should never happen, set all F
         SCONC  = .FALSE.
         SDEPOS = .FALSE.
         SDDEP  = .FALSE.
         SWDEP  = .FALSE.
         RETURN
      END IF

      POUT = 0.0D0

C---- Calculate the exponential decay term, D               ---   CALL DECAY
      Call DECAY (XARG)

      IF (SCONC) THEN
C----    Get Concentration or Deposition due to horizontal plume, CWRAP
         IF (FOPT .EQ. 0.0D0) THEN
            CWRAP = 0.0D0
         ELSE
            CALL ACPLUME (ZRT, FYARG, CWRAP)
         END IF

C----    Calculate the contribution due to terrain-following plume, CLIFT
         IF (ZRT .EQ. ZFLAG) THEN
C----       Effective receptor heights are equal, therefore CLIFT = CWRAP
            CLIFT = CWRAP
         ELSE IF (FOPT .EQ. 1.0D0) THEN
            CLIFT = 0.0D0
         ELSE
C           Get Concentration or Deposition due to LIFT algorithm
            CALL ACPLUME (ZFLAG, FYARG, CLIFT)
         END IF

C----    Calculate the hourly concentration value
C        Now compute the function
         POUT = ADJ * (FOPT * CWRAP + (1.0D0 - FOPT) * CLIFT) * D

      END IF
      IF (SDEPOS .OR. SDDEP) THEN
C----    Get Concentration or Deposition due to horizontal plume, CWRAP
         IF (FOPT .EQ. 0.0D0) THEN
            CWRAP = 0.0D0
         ELSE
            CALL ACPLUME (ZRT-ZFLAG+ZRDEP, FYARG, CWRAP)
         END IF

C----    Calculate the contribution due to terrain-following plume, CLIFT
         IF (ZRT .EQ. ZFLAG) THEN
C----       Effective receptor heights are equal, therefore CLIFT = CWRAP
            CLIFT = CWRAP
         ELSE IF (FOPT .EQ. 1.0D0) THEN
            CLIFT = 0.0D0
         ELSE
C           Get Concentration or Deposition due to LIFT algorithm
            CALL ACPLUME (ZRDEP, FYARG, CLIFT)
         END IF

C----    Calculate the hourly concentration value
C        Now compute the function
         DRYFLUX = ADJ * (FOPT * CWRAP + (1.0D0 - FOPT) * CLIFT) * D

      END IF
      IF (SDEPOS .OR. SWDEP) THEN
C        Calculate WETFLUX, vertical term for wet deposition
C        Note that the SRT2PI for the integrated vertical term
C        has been removed since it should be divided by SRT2PI.
C        Additional factor of 3600. has been added to denominator
C        to account for conversion from seconds to hours when
C        divided by wind speed below.
         IF (PRATE .GT. 0.0D0) THEN
            IF (NPD .EQ. 0) THEN
               WETFLUX = (ADJ*FRACSAT*PRATE*1.0D6*RGAS*TA)/
     &                   (ZSUBP*HENRY(ISRC)*1.0D9*DENOM*3600.0D0)
            ELSE
               WETFLUX = 1.0D-3*ADJ*WASHOUT(JIN)*PRATE/
     &                   (ZSUBP*3600.0D0)
            END IF
         ELSE
            WETFLUX = 0.0D0
         END IF
      END IF
      IF (SDEPOS) THEN
         IF( STABLE  .OR.  (UNSTAB .AND. (HS .GE. ZI) ) ) THEN
            POUT = VDINP*DRYFLUX + WETFLUX*FYARG/UEFF
         ELSE IF (UNSTAB) THEN
            POUT = VDINP*DRYFLUX + WETFLUX*FYARG/UEFFD
         END IF
      END IF
      IF (SDDEP) THEN
         POUT = VDINP*DRYFLUX
      END IF
      IF (SWDEP) THEN
         IF( STABLE  .OR.  (UNSTAB .AND. (HS .GE. ZI) ) ) THEN
            POUT = WETFLUX*FYARG/UEFF
         ELSE IF (UNSTAB) THEN
            POUT = WETFLUX*FYARG/UEFFD
         END IF
      END IF


CCRFL Call to METDEB was moved here from METEXT on 9/26/94, R.F. Lee.
CCRFL Print meteorological debug output.                   ---   CALL METDEB
      IF (METEOR) CALL METDEB

      IF ( DEBUG ) THEN
C        Print Out Debugging Information                    ---   CALL DEBOUT
         CALL DEBOUT
      END IF

      RETURN
      END

      SUBROUTINE DEBOUT
C***********************************************************************
C             DEBOUT Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Outputs Debugging Information: Sigmas, Plume Heights,
C                 etc., for Each Calculation
C
C        PROGRAMMER: Roger Brode and Jim Paumier, PES, Inc.
C
C        DATE:    October 8, 1993
C
C        REVISIONS:  Revised emission rate terms:  for CHID & CHIN,
C                    to QTK*(1-PPF), and for CHI3 to QTK*PPF.
C                    Ref:  P.D.F. Model for Dispersion in the
C                    Convective Boundary Layer, J.C. Weil, 6/27/94.
C                    Changed 7/19/94, R.F. Lee.
C
C                    Revised by Bob Paine to improve readability
C                    of debugging output.  Changed 8/18/94, R.F. Lee
C                    & R. Paine.
C
C
C        INPUTS:  Downwind Distance
C                 Crosswind Distance
C                 Plume Height
C                 Stack Top Wind Speed
C                 Lateral Dispersion Parameter
C                 Vertical Dispersion Parameter
C                 Stability Class
C                 Mixing Height
C                 Receptor Height Above Ground
C                 Emission Rate and Units Scaling Factor
C                 Source Parameter Arrays
C
C        OUTPUTS: Debug Outputs
C
C        CALLED FROM:   PCHI, PDEP, AREAIN
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      DOUBLE PRECISION :: CHID, CHIN, CHI3

C     Variable Initializations
      MODNAM = 'DEBOUT'

C     Calculate contributions from each "plume"
      IF (STABLE .OR. (UNSTAB .AND. HS.GE.ZI)) THEN
         CHID = HRVAL(1)
      ELSE IF (UNSTAB) THEN
CCRFL
CCRFL    Revised emission rate terms:  for CHID & CHIN, to QTK*(1-PPF),
CCRFL    and for CHI3 to QTK*PPF.  Ref:  P.D.F. Model for Dispersion in
CCRFL    the Convective Boundary Layer, J.C. Weil, 6/27/94.  Changed
CCRFL    7/19/94, R.F. Lee.
CCRFL
         CHID = (QTK*EMIFAC(1) * (1.0D0-PPF) / UEFFD) * (FSUBYD*FSUBZD)
         CHIN = (QTK*EMIFAC(1) * (1.0D0-PPF) / UEFFN) * (FSUBYN*FSUBZN)
         IF (PPF.GT.0.0D0 .AND. UEFF3.GT.0.0D0) THEN
            CHI3 = (QTK * EMIFAC(1) * PPF / UEFF3) * (FSUBY3*FSUBZ3)
         ELSE
            CHI3 = 0.0D0
         END IF
      END IF

C     Write a blank line to separate the groupings
      WRITE ( DBGUNT , 101 ) 

C     Write the debug output for the receptor data
      WRITE ( DBGUNT , 320 ) IREC, XR, YR, ZELEV, ZHILL, ZFLAG,
     &                       X,  Y, ZELEV-ZS, HCRIT, PHEE, FOPT,
     &                       D, CWRAP*EMIFAC(1), CLIFT*EMIFAC(1),
     &                       AERVAL(1),PRMVAL(1)
C
C     Write header for plume sigma information
C
      WRITE ( DBGUNT, 330 )
c
C     Write the data that was used in the plume computations,
C      which is stability-dependent.
C
      IF( STABLE  .OR.  (UNSTAB .AND. (HS .GE. ZI) ) )THEN
         WRITE ( DBGUNT, 400 ) PPF,QTK,HE,SYAMB,VSIGY,SYB,SY,
     &      FSUBY,SZAMB,VSIGZ,SZB,SZSURF,SZ,FSUBZ,CHID

      ELSEIF ( UNSTAB ) THEN
         IF((1.0D0 - PPF) * QTK .GT. 0.0D0) THEN
            WRITE ( DBGUNT, 410 ) PPF,(1.0D0-PPF)*QTK,HED1,SYAMB,
     &         VSIGY,SYB,SY,SZAD1,VSZD1,SZBD,SZSURF,SZD1
            WRITE ( DBGUNT, 420 ) (1.0D0-PPF)*QTK,HED2,SYAMB,VSIGY,
     &         SYB,SY,FSUBYD,SZAD2,VSZD2,SZBD,SZSURF,SZD2,FSUBZD,CHID
CCRFL
CCRFL  SZSURF has been added to the indirect plume sigma z calculations--
CCRFL  add it also to the debug output for the indirect plume.
CCRFL  Changed 9/12/94.  R.F. Lee.  (Format statements 430 and 440 were
CCRFL  changed also.)
            WRITE ( DBGUNT, 430 ) PPF,(1.0D0-PPF)*QTK,HEN1,SYAMB,
     &         VSIGY,SYB,SY,SZAN1,VSZN1,SZBN,SZSURF,SZN1
            WRITE ( DBGUNT, 440 ) (1.0D0-PPF)*QTK,HEN2,SYAMB,
     &         VSIGY,SYB,SY,FSUBYN,SZAN2,VSZN2,SZBN,SZSURF,SZN2,FSUBZN,
     &         CHIN
       END IF
       IF ((PPF*QTK) .GT. 0.0D0) THEN
             WRITE ( DBGUNT, 450 ) PPF, PPF*QTK, HE3,SYA3,VSY3,
     &          SYB3,SY3,FSUBY3,SZA3,VSZ3,SZB3,SZ3,FSUBZ3,CHI3
         END IF
      END IF
C
C     FORMAT STATEMENTS
C
  101 FORMAT ( 1X )
  320 FORMAT('  REC  REC-X    REC-Y   REC-Z    HILLHT  FLAGPL    ',
     & 'DEL-X   DEL-Y  DEL-Z   HCRIT   PHEE  FOPT  DECAY   CWRAP ',
     & '     CLIFT      AERVAL    PRMVAL',/,
     & '    #   (M)      (M)     (M)      (M)     (M)       (M)   ',
     & '  (M)    (M)     (M)                       (UG/M3)    ',
     & '(UG/M3)    (UG/M3)',//,
     & I5,F8.1,F9.1,F8.1,F9.1,F8.1,F10.1,F8.1,F7.1,F7.1,F7.3,F6.3,
     & F7.3,4E11.4,/)
  330 FORMAT('   PLUME   PART.  SOURCE  PLUME  <----- SIGMA-Y TERMS --',
     &  '--->   GAUSS.     <--------- SIGMA-Z TERMS -------->   GAUSS.',
     &  /,
     &      ' COMPONENT PEN.     Q     HEIGHT  AMB.  DOWNW.  BUOY.  TO',
     &  'TAL   HORIZ.      AMB.  DOWNW.  BUOY.  SURF.  TOTAL   VERT.  ',
     &  '       CHI ',/,
     &       '   TYPE    FRAC.   (G/S)   (M)    (M)    (M)     (M)    ',
     &  '(M)    TERM        (M)    (M)     (M)    (M)    (M)    TERM ',
     &  '       (UG/M3)',/)
  400    FORMAT(' GAUSSIAN ',F6.3,F9.2,F7.1,4F7.1,E11.4,2X,5F7.1,E11.4,
     &      E12.4)
  410    FORMAT(' DIRECT #1',F6.3,F9.2,F7.1,4F7.1,13X,5F7.1)
  420    FORMAT(' DIRECT #2',6X,F9.2,F7.1,4F7.1,E11.4,2X,5F7.1,E11.4,
     &      E12.4)
  430    FORMAT(' INDIRECT1',F6.3,F9.2,F7.1,4F7.1,13X,4F7.1,
     &      F7.1)
  440    FORMAT(' INDIRECT2',6X,F9.2,F7.1,4F7.1,E11.4,2X,5F7.1,
     &      E11.4,E12.4)
  450    FORMAT(' PENETRATE',F6.3,F9.2,F7.1,4F7.1,E11.4,2X,3F7.1,7X,
     &      F7.1,E11.4,E12.4,/)
C
      RETURN
      END


      SUBROUTINE PENFCT
C***********************************************************************
C             PENFCT Module of the AMS/EPA Regulatory Model - AERMOD
C
C   PURPOSE: Calculate the plume penetration factor
C
C   PROGRAMMER: Roger Brode and Jim Paumier, PES, Inc.
C
C   DATE:    September 30, 1993
C
C   REVISED: To use VPTGZI = 0.01 for Base Case model. R.Brode, PES - 12/7/94
C
C   INPUTS:  Stability, STABLE/UNSTAB
C            Buoyancy flux, FB
C            Wind speed at release height, UP (computed in METINI)
C            Potential temperature at ZI
C            Potential temperature gradient above ZI, VPTGZI (from
C            AERMET)
C
C   OUTPUTS: Plume penetration factor, PPF
C
C   CALLED FROM:   PCALC
C
C   Assumptions:
C
C   References:   "Plume Penetration of the CBL and Source 3: Source
C                 Strength and Plume Rise", J. Weil, 9/1/93
C                 "A Dispersion Model for the Convective Boundary Layer",
C                 J. Weil, 8/17/93
C
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      DOUBLE PRECISION    BVZI2

      MODNAM = 'PENFCT'

      IF( STABLE ) THEN
         PPF = 0.0D0

      ELSE IF (UNSTAB .AND. (HS .GE. ZI) )THEN
         PPF = 1.0D0

      ELSE
C        Compute the square of the Brunt-Vaisala frequency at ZI, BVZI2

         BVZI2 = (G / PTATZI) * VPTGZI

C        Compute the value of PsubS, Eq. 26b in the 2nd reference
         PSUBS = FB / ( UP * BVZI2 * (ZI-HSP)*(ZI-HSP)*(ZI-HSP) )

C        Compute the ratio of delta(Hsub_e)/delta(Hsub_h), HEDHH
C        (Eq. 25 in the 2nd ref.
C        NOTE: 17.576 = (2.6)**3 and 0.296296 is (2/3)**3
         HEDHH = (17.576D0 * PSUBS + 0.296296D0) ** THIRD

C        Check the value of HEDHH and compute the plume penetration, P
         IF( HEDHH .LT. (2.0D0*THIRD) )THEN
            PPF = 0.0D0

         ELSE IF( HEDHH .GT. 2.0D0 )THEN
            PPF = 1.0D0

         ELSE
            PPF = 1.5D0 - (1.0D0 / HEDHH)

         END IF

      END IF

      RETURN
      END

      SUBROUTINE CPLUME (ZARG, COUT)
C***********************************************************************
C             CPLUME Module of the AMS/EPA Regulatory Model - AERMOD
C
C   PURPOSE: Calculate the contribution to the concentration due to
C            plume component, either horizontal or terrain-following,
C            depending on the input receptor height, ZARG
C
C   PROGRAMMER: Roger Brode, PES, Inc.
C
C   DATE:    September 30, 1993
C
C   REVISIONS:
C               Make stable plume reflections dependent on the
C               developmental option switch, OPTG1 & OPTG2,
C               R. Brode, PES, 1/6/95
C
C               Remove stable plume reflections off of ZI for
C               Base Case model.  R. Brode, PES - 12/7/94
C
C               Revised emission rates for each plume to QTK*(1.-PPF)
C               for the direct and indirect plumes, and to QTK*PPF
C               for the penetrated plume.  Ref:  P.D.F. Model for
C               Dispersion in the Convective Boundary Layer,
C               J.C. Weil, 6/27/94. Changes made 7/19/94, R.F. Lee.
C
C               Added true centerline concentration calculations.
C               Changes made 7/25/94, R.F. Lee.
C
C   INPUTS:  Stability, STABLE/UNSTAB
C            Fraction of plume vertical flux remaining in the CBL, FOPT
C            Mixing height, ZI
C            Plume heights, HE/HED1/HED2/HEN1/HEN2
C            sigma_Z's: SZ, SZD1, SZD2, SZN1, SZN2, SZ3
C            Receptor height, ZARG
C
C   OUTPUTS: Contribution due to WRAP, CWRAP
C
C   CALLED FROM:   PCHI
C
C   Assumptions:  For receptor height (ZR) above the mixing height (ZI)
C                 for unstable conditions, the direct and indirect plume
C                 impacts are set to zero.
C
C   References:   "A Dispersion Model for the Convective Boundary
C                 Layer", J. Weil, 8/17/93
C
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      DOUBLE PRECISION :: ZARG, COUT

      MODNAM = 'CPLUME'

C     Assign receptor height for vertical term calculations
      ZR = ZARG

      IF( STABLE  .OR.  (UNSTAB .AND. (HS .GE. ZI) ) ) THEN
C        Calculate the vertical term, FSUBZ              ---   CALL VRTSBL
C        With stable plume reflections and effective Zi
         IF (ZR .LE. HSBL) THEN
            CALL VRTSBL (SZ, MAX( 0.0D0, HE-HV ), HSBL)
         ELSE
            CALL VRTSBN (SZ, MAX( 0.0D0, HE-HV ))
         END IF

C        Calculate the concentration for a stable atmosphere
         COUT = (QTK / UEFF) * ( FSUBY * FSUBZ )

      ELSEIF( UNSTAB )THEN
         IF (PPF .LT. 1.0D0) THEN
C           Calculate the vertical term for the direct plume, FSUBZD
            IF (ZR .LE. ZI) THEN
C              Calculation for Receptor below Zi      ---   CALL VRTCBL
               CALL VRTCBL ( HED1-HV, HED2-HV, SZD1, SZD2, 1.0D0)
               FSUBZD = FSUBZ
            ELSE
C              Set FSUBZ = 0.0 for "receptor height" (ZR) > ZI
               FSUBZD = 0.0D0
            END IF

C           Calculate the vertical term for the indirect plume, FSUBZN
            IF (ZR .LE. ZI) THEN
C              Calculation for Receptor below Zi      ---   CALL VRTCBL
               CALL VRTCBL ( HEN1-HV, HEN2-HV, SZN1, SZN2, -1.0D0 )
               FSUBZN = FSUBZ
            ELSE
C              Set FSUBZ = 0.0 for "receptor height" (ZR) > ZI
               FSUBZN = 0.0D0
            END IF
         ELSE
            FSUBZD = 0.0D0
            FSUBZN = 0.0D0

         END IF

C        Note that UEFF and UEFF3 can never be zero, since they get
C        set to a minimum value earlier on.

         IF( PPF .GT. 0.0D0 )THEN
C           Calculate the vertical term for the penetrated
C           plume, FSUBZ3                                ---   CALL VRTSBL
            IF (ZR .LE. HPEN) THEN
               CALL VRTSBL (SZ3, MAX(0.0D0,HE3-HV), HPEN)
            ELSE
               CALL VRTSBN (SZ3, MAX(0.0D0,HE3-HV))
            END IF
            FSUBZ3 = FSUBZ

            IF (PPF .LT. 1.0D0) THEN
               COUT = (QTK * (1.0D0-PPF) / UEFFD) * ( FSUBYD*FSUBZD ) +
     &                (QTK * (1.0D0-PPF) / UEFFN) * ( FSUBYN*FSUBZN ) +
     &                (QTK * PPF / UEFF3) * ( FSUBY3*FSUBZ3 )

            ELSE
               COUT = (QTK * PPF / UEFF3) * ( FSUBY3*FSUBZ3 )

            END IF

         ELSE
            FSUBZ3 = 0.0D0
            HPEN   = 0.0D0
            COUT = (QTK / UEFFD) * ( FSUBYD * FSUBZD ) +
     &             (QTK / UEFFN) * ( FSUBYN * FSUBZN )

         END IF

      END IF

      RETURN
      END

      SUBROUTINE PRM_PLUME (ZARG, COUT)
C***********************************************************************
C             PRM_PLUME Module of the AMS/EPA Regulatory Model - AERMOD
C
C   PURPOSE: Calculate the contribution to the concentration due to
C            PRIME downwash component
C
C   PROGRAMMER: Roger Brode, PES, Inc.
C
C   DATE:    July 5, 2001
C
C   INPUTS:  Receptor height, ZARG
C
C   OUTPUTS: Contribution due to PRIME, COUT
C
C   CALLED FROM:   PRM_PCHI
C
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      DOUBLE PRECISION :: ZARG, COUT

      MODNAM = 'PRM_PLUME'

C     Assign receptor height for vertical term calculations
      ZR = ZARG

      IF (STABLE) THEN
         CALL VRTSBN (SZ, HE)
      ELSE IF (UNSTAB .AND. HE.LE.ZI) THEN
         CALL VRTSBL (SZ, HE, ZI)
      ELSE
         FSUBZ = 0.0D0
      END IF

C     Calculate the WRAP term for a stable atmosphere
      COUT = (QTK / US) * ( FSUBY * FSUBZ )

      RETURN
      END

      SUBROUTINE ACPLUME (ZARG, FYARG, COUT)
C***********************************************************************
C             ACPLUME Module of the AMS/EPA Regulatory Model - AERMOD
C
C   PURPOSE: Calculate the contribution to the concentration due to
C            plume component, either horizontal or terrain-following,
C            for AREA sources
C
C   PROGRAMMER: Roger Brode, PES, Inc.
C
C   DATE:    September 30, 1993
C
C   REVISIONS:
C               Make stable plume reflections dependent on the
C               developmental option switch, OPTG1 & OPTG2,
C               R. Brode, PES, 1/6/95
C
C               Remove stable plume reflections off of ZI for
C               Base Case model.  R. Brode, PES - 12/7/94
C
C               Revised emission rates for each plume to QTK*(1.-PPF)
C               for the direct and indirect plumes, and to QTK*PPF
C               for the penetrated plume.  Ref:  P.D.F. Model for
C               Dispersion in the Convective Boundary Layer,
C               J.C. Weil, 6/27/94. Changes made 7/19/94, R.F. Lee.
C
C               Added true centerline concentration calculations.
C               Changes made 7/25/94, R.F. Lee.
C
C   INPUTS:  Stability, STABLE/UNSTAB
C            Fraction of plume vertical flux remaining in the CBL, FOPT
C            Mixing height, ZI
C            Plume heights, HE/HED1/HED2/HEN1/HEN2
C            sigma_Z's: SZ, SZD1, SZD2, SZN1, SZN2, SZ3
C
C   OUTPUTS: Contribution due to WRAP, CWRAP
C
C   CALLED FROM:   ACHI
C
C   Assumptions:  For receptor height (ZR) above the mixing height (ZI)
C                 for unstable conditions, the direct and indirect plume
C                 impacts are set to zero.
C
C   References:   "A Dispersion Model for the Convective Boundary
C                 Layer", J. Weil, 8/17/93
C
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      DOUBLE PRECISION :: ZARG, FYARG, COUT

      MODNAM = 'ACPLUME'

C     Assign receptor height for vertical term calculations
      ZR = ZARG

C     Assign lateral term
      FSUBY = FYARG

      IF( STABLE  .OR.  (UNSTAB .AND. (HS .GE. ZI) ) ) THEN
C        Calculate the vertical term, FSUBZ              ---   CALL VRTSBL
C        With stable plume reflections
         IF (ZR .LE. HSBL) THEN
            CALL VRTSBL (SZ, MAX( 0.0D0, HE-HV ), HSBL)
         ELSE
            CALL VRTSBN (SZ, MAX( 0.0D0, HE-HV ))
         END IF

C        Calculate the WRAP term for a stable atmosphere
         COUT = (1.0D0 / UEFF) * ( FSUBY * FSUBZ )

      ELSEIF( UNSTAB )THEN
C        Calculate the vertical term for the direct plume, FSUBZD
         IF (ZR .LE. ZI) THEN
C           Calculation for Receptor below Zi         ---   CALL VRTCBL
            CALL VRTCBL ( HED1-HV, HED2-HV, SZD1, SZD2, 1.0D0 )
            FSUBZD = FSUBZ
         ELSE
C           Set FSUBZ = 0.0 for "receptor height" (ZR) > ZI
            FSUBZD = 0.0D0
         END IF

C        Calculate the vertical term for the indirect plume, FSUBZN
         IF (ZR .LE. ZI) THEN
C           Calculation for Receptor below Zi         ---   CALL VRTCBL
            CALL VRTCBL ( HEN1-HV, HEN2-HV, SZN1, SZN2, -1.0D0 )
            FSUBZN = FSUBZ
         ELSE
C           Set FSUBZ = 0.0 for "receptor height" (ZR) > ZI
            FSUBZN = 0.0D0
         END IF

C        Note that UEFF and UEFF3 can never be zero, since they get
C        set to a minimum value earlier on.

         FSUBZ3 = 0.0D0
         HPEN   = 0.0D0
         COUT = (1.0D0 / UEFFD) * ( FSUBY * FSUBZD ) +
     &          (1.0D0 / UEFFN) * ( FSUBY * FSUBZN )

      END IF

      RETURN
      END

      SUBROUTINE LTOPG(LSTAB)
C-----------------------------------------------------------------------
C                LTOPG Module of AERMOD Model
C
C        PURPOSE:  Converts Monin-Obukhov length to PG stability class
C                  for use with FASTAREA option, based on Golder (1972)
C
C        MODIFIED: The original LTOPG routine in AERMOD was replaced 
C                  with the LSTAB function from the CTDMPLUS model, which
C                  more closely matches the PG-class curves in Figure 4 
C                  of the Golder (1972) paper.
C                  R. W. Brode, U.S. EPA, OAQPS, AQMG, 02/29/2012
C
C LTOPG is based on the LSTAB function in the CTDMPLUS model:
C
C FUNCTION: LSTAB
C
C PURPOSE: THIS FUNCTION CALCULATES A P-G STABILITY CLASS GIVEN THE
C               MONIN-OBUKHOV LENGTH (L) AND THE SURFACE ROUGHNESS
C               LENGTH (Z0).
C
C ASSUMPTIONS: THE DIVIDING LINES BETWEEN CATEGORIES ARE ASSUMED TO BE
C               LINEAR.
C
C LIMITATIONS: THIS FUNCTION IS ONLY VALID FOR 0.01 <= Z0 <= 0.5(M).
C              HOWEVER, RESULTS ARE EXTENDED TO OTHER VALUES OF Z0 BY
C              USING Z0 = 0.01 IF Z0 < 0.01 M, AND BY USING Z0 = 0.5
C              IF Z0 > 0.5 M.
C
C ARGUMENTS
C  PASSED:
C       EL      REAL    MONIN-OBUKHOV LENGHT (M)
C       ZR0     REAL    SURFACE ROUGHNESS LENGTH (M)
C  RETURNED FUNCTION VALUE:
C       LSTAB   INT     P-G STABILITY CATEGORY 1=A, 2=B, ETC.
C
C CALLING ROUTINES: SEQMOD
C
C EXTERNAL ROUTINES: NONE
C
C INTERNAL FUNCTIONS:
C       XL - EQUATION OF DIVIDING LINE BETWEEN P-G STABILITY CLASSES
C
C INTRINSIC FUNCTIONS: ALOG
C
C REFERENCES:
C       GOLDER, D. (1972): RELATIONS AMONG STABILITY PARAMETERS IN THE
C                       SURFACE LAYER, BOUNDARY-LAYER METEOROLOGY, 3:56.
C
C-----------------------------------------------------------------------
C
C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      DOUBLE PRECISION :: EL, XEL, XL, Z0, ZR0, YY, XM, B
      
      INTEGER :: LSTAB

      XL(YY,XM,B)=XM/(DLOG(YY)-B)
C
C
      EL  = OBULEN
      ZR0 = SFCZ0
C
      Z0 = ZR0
      IF(Z0 .GT. 0.5D0) Z0 = 0.5D0
      IF(Z0 .LT. 0.01D0) Z0 = 0.01D0
      IF(EL .LT. 0.0D0) THEN
          XEL = -EL
          IF(XEL .LE. XL(Z0,-70.0D0,4.35D0)) THEN
C             STABILITY A
              LSTAB=1
            ELSE IF(XEL .LE. XL(Z0,-85.2D0,0.502D0)) THEN
C             STABILITY B
              LSTAB=2
            ELSE IF(XEL .LE. XL(Z0,-245.0D0,0.050D0)) THEN
C             STABILITY C
              LSTAB=3
            ELSE
C             STABILITY D
              LSTAB=4
          ENDIF
        ELSE
          IF(EL .GE. XL(Z0,-327.0D0,0.627D0)) THEN
C             STABILITY D
              LSTAB=4
            ELSE IF(EL .GE. XL(Z0,-70.0D0,0.295D0)) THEN
C             STABILITY E
              LSTAB=5
            ELSE
C             STABILITY F
              LSTAB=6
          ENDIF
      ENDIF
C
      RETURN
      END

c----------------------------------------------------------------------
      subroutine VDP
c----------------------------------------------------------------------
c
c --- ISCST3     R.W. Brode, PES, Inc.
c
c --- PURPOSE:  Compute particle and gas dry deposition velocities
c               based on ANL report,
c               Wesely, et. al. (2001)
c
c --- MODIFIED: To add lower limit to dq to avoid zero-divide.
c               R. W. Brode, U.S. EPA/OAQPS/AQMG, 01/24/2007
c
c --- MODIFIED: To add SCHMIDT number as global array.
c               R. W. Brode, MACTEC (f/k/a PES), Inc., 03/19/04
c
c --- INPUTS (and other variables):
c
c      DEFINITIONS OF DRY DEPOSITION VARIABLES AND CONSTANTS
c      C0-C6 = coefficients used in computing saturation vapor pressure
c      de = water vapor deficit computed from Ta and ambient RH (kPa)
c      D_suba = diffusivity in air of gas of interest (m**2/s) (User Input)
c      D_sub_b = diffusivity in air of particle (m**2/s)
c      dq = specific humidity deficit (g/kg)
c      Dv = diffusivity of water vapor in air (0.219e-04m**2/s)
c      el = Monin-Obukhov stability length scale (m)
c      EsTa = saturation vapor pressure at the ambient temperature (kPa)
c             (calculated outside source loop and passed through MODULE MAIN1)
c      F =  factor used in specifying LAIr
c      fo = measure of reactivity
c      f1 = factor for the variation of Rs with solar irradiance
c      f2 = factor for the variation of Rs with available soil moisture (global)
c      f3 = factor for the variation of Rs with water vapor deficit
c      f4 = factor for the variation of Rs with temperature
c      QSW = solar irradiance (W/m**2) (Calculated from AERMET outputs)
c      Gr = reference solar irradiance (30 W/m**2 for forests, otherwise
c           100 W/m**2)
c      Gust_Adj = unstable gusty wind adjustment for Rd
c      HENRY = Henry's Law coefficient for gas of interest (Pa*m**3/mol)
c              (User Input)
c      ISEA5 = Wesely season category (1-5) (Based on User Input)
c      iseas = assign Wesely season category by month for the locale of the
c              meteorological data
c      LANUSE = land use category (1-9) (User Input)
c      P = ambient pressure (kPa)  (Provided by AERMET)
c      Po = reference pressure (101.3 kPa)
c      Prate = precipitation total for the current hour (mm) (Provided by AERMET)
c      prec1 = precipitation one hour back
c      prec2 = precipitation two hours back
c      q = ambient specific humidity (g/kg)
c      qsat = specific humidity at saturation (g/kg)
c      rLAI = relative leaf area index factor
c      Ra = aerodynamic resistance (s/m)
c      Rac = gas-phase aerodymanic resistance within the canopy (s/m)
c      Raci = in-canopy aerodynamic resistance appropriate for Ustar=0.3 (s/m)
c      Rb = quasiliminar resistance for bulk surface (s/m)
c      Rc = bulk surface resistance (s/m)
c      Rcl = bulk cuticle resistance to uptake associated with lipid
c            solubility (s/m)
c      Rcli = resistance to uptake by lipids in cuticles for individual
c             leaves (s/m) (User Input)
c      Rcox = cuticle resistance for ozone, wetted (s/m)
c      Rcs = bulk surface resistance for sulfur dioxide, wetted (s/m)
c      Rcut = cuticle resistance (s/m)
c      Restab = table of resistances that vary with land use and season
c               categories only.
c      Rg = ground resistance (s/m)
c      Rgo = ground resistance for ozone, wetted (s/m)
c      Rgs = ground resistance for sulfur dioxide, wetted (s/m)
c      Ri = surface resistance component (from table) (s/m)
c      RH = relative humidity (%)  (Provided by AERMET)
c      rLAI = relative leaf area index
c      Rm = mesophyll resistance (s/m)
c      Rp = resistance component for particles (s/m)
c      Rs = bulk canopy stomatal resistance (s/m)
cpes   Rx = term used to adjust components of cuticular and ground resistances
c           in the event of a hard freeze
c      S = scaling factor used to estimate cuticle resistance by land use category
c      Stab = table of S by land use category
c      Ta = ambient temperature (deg K) (Provided by AERMET)
c      Tcel = ambient temperature in celsius
c      To = reference temperature (273.16 K)
c      ustar  = friction velocity at the meteorological site (Provided by AERMET)
c      uref = wind speed at anemometer height from the meteorological site
c             (Provided by AERMET)
c      Vdepg = gaseous deposition velocity (m/s)
c      Vdep(i) = particle deposition velocity for i-th particle size category (m/s)
c      vd1 = submicron particle deposition velocity (cm/s)
c      vd2 = coarse particle deposition velocity (cm/s)
c      VONKAR = von Karman constant (0.4)
c      vp = vapor pressure (kPa)
c      Wnew = available rootzone water for current hour (mm) (calculated
c             outside source loop and passed through MODULE MAIN1)
c      Wold = available rootzone water for previous hour (mm) (global)
c      Xnu = kinematic viscosity of air (0.1505 x 10-4 m**2/s, before
c            correction for ambient temp. and pressure)
c      Zrdep = reference height (m)

c
c --- OUTPUT:  Deposition velocity for gases, Vdepg (m/s), or
c              Deposition velocity for particles, Vdep(i) (m/s) by
c                 particle size for Method 1 or for single category
c                 for Method 2
c
c --- VDP_TOX called by:  VDP
c --- VDP_TOX calls:      none
c----------------------------------------------------------------------
c
      USE MAIN1
      IMPLICIT NONE

      DOUBLE PRECISION, PARAMETER :: a1=1.257D0, a2=0.4D0, a3=0.55D0, 
     &                               xmfp=6.5D-6
      INTEGER LANUSE, ILAND_NDX
      DOUBLE PRECISION Tcel, Ri, Rcs, Rco, Rx
      DOUBLE PRECISION Raci, Rgs, Rgo, F, rLAI, Gr
      DOUBLE PRECISION de, Rp, f1, alfa, f3, f4, ppp, Rcox, Rac
      DOUBLE PRECISION D_suba, Rb, Rcl, vd1, vd2, Sfact, D_sub_b
      DOUBLE PRECISION Stab(9), Restab(9,6,5), Dv
      DOUBLE PRECISION Po, To, Pressure, a, q, dq, qsat, vp
      INTEGER j, k, isea5

      INTEGER :: I, N
      DOUBLE PRECISION :: RA, T1, ST, XINERT,
     &        rd(npdmax), RG, RS, RC, GUST_ADJ
c
C     Initialize stability factor array by land use category
      data Stab/1.0D-5,6.0D0,5.0D0,7.0D0,3.0D0,4.0D0,1.0D-5,
     &          1.0D-5,3.0D0/

C     Initialize resistance table by land use category and season
C     Split DATA statement into 2 parts to avoid continuation line limit
      data (((Restab(i,j,k),i=1,9),j=1,6),k=1,3)/
     &1.D07,   60.D0, 120.D0, 100.D0, 200.D0, 150.D0,1.D07,1.D07,80.D0,
     &1.D07, 2000.D0,2000.D0,2000.D0,2000.D0,2000.D0,1.D07,1.D07,2.5D3,
     &1.D07, 1000.D0,1000.D0,1000.D0,2000.D0,2000.D0,1.D07,1.D07, 1.D3,
     &100.D0, 200.D0, 100.D0,2000.D0, 100.D0,1500.D0,0.D0, 0.D0, 300.D0,
     &400.D0, 150.D0, 350.D0, 300.D0, 500.D0, 450.D0,0.D0,1000.D0, 0.D0,
     &300.D0, 150.D0, 200.D0, 200.D0, 300.D0, 300.D0,2.D3,400.D0, 1.D3,
     &1.D07,   1.D07,  1.D07, 350.D0,  1.D07, 700.D0,1.D07,1.D07, 1.D07,
     &1.D07, 6500.D0,6500.D0,3000.D0,2000.D0,2000.D0,1.D07,1.D07, 6.5D3,
     &1.D07,  400.D0, 300.D0, 500.D0, 600.D0,1000.D0,1.D07,1.D07,300.D0,
     &100.D0, 150.D0, 100.D0,1700.D0, 100.D0,1200.D0, 0.D0, 0.D0,200.D0,
     &400.D0, 200.D0, 350.D0, 300.D0, 500.D0, 450.D0, 0.D0,1.D3,  0.D0,
     &300.D0, 150.D0, 200.D0, 200.D0, 300.D0, 300.D0,2.0D3,400.D0,8.D2,
     &1.D07,   1.D07,  1.D07, 500.D0,  1.D07,1000.D0,1.D07, 1.D07,1.D07,
     &1.D07,   1.D07,9000.D0,6000.D0,2000.D0,2000.D0,1.D07, 1.D07, 9.D3,
     &1.D07,   1.D07, 400.D0, 600.D0, 800.D0,1600.D0,1.D07, 1.D07,8.D2,
     &100.D0,   0.D0, 100.D0,1500.D0, 100.D0,1000.D0, 0.D0,  0.D0,1.D2,
     &400.D0, 150.D0, 350.D0, 300.D0, 500.D0, 450.D0, 0.D0,  0.D0,1.D3,
     &300.D0, 150.D0, 200.D0, 200.D0, 300.D0, 300.D0,2.D3, 400.D0,1.D3/
      data (((Restab(i,j,k),i=1,9),j=1,6),k=4,5)/     
     &1.D07,   1.D07,  1.D07, 800.D0,  1.D07,1600.D0,1.D07, 1.D07,1.D07,
     &1.D07,   1.D07,  1.D07, 400.D0,  1.D07, 800.D0,1.D07, 1.D07, 9.D3,
     &1.D07, 2000.D0,1000.D0, 600.D0,2000.D0,1200.D0,1.D07, 1.D07,8.D2,
     &100.D0,   0.D0,  10.D0,1500.D0, 100.D0,1000.D0, 0.D0, 0.D0, 50.D0,
     &100.D0, 100.D0, 100.D0, 100.D0, 200.D0, 200.D0, 0.D0, 1.D3,100.D0,
     &600.D0,3500.D0,3500.D0,3500.D0, 500.D0, 500.D0,2.D3,400.D0,3.5D3,
     &1.D07,  100.D0, 120.D0, 100.D0, 200.D0, 150.D0,1.D07, 1.D07,80.D0,
     &1.D07, 2000.D0,2000.D0,1500.D0,2000.D0,2000.D0,1.D07, 1.D07, 2.D3,
     &1.D07, 1000.D0, 250.D0, 350.D0, 500.D0, 700.D0,1.D07,1.D07,300.D0,
     &100.D0,  50.D0,  80.D0,1500.D0, 100.D0,1000.D0, 0.D0, 0.D0,200.D0,
     &500.D0, 150.D0, 350.D0, 300.D0, 500.D0, 450.D0, 0.D0,1.D3,  0.D0,
     &300.D0, 150.D0, 200.D0, 200.D0, 300.D0, 300.D0,2.D3, 400.D0, 1.D3/

      Dv = 0.219D-04
      Po = 101.3D0
      To = 273.16D0
CPES  Define alfa based on Eqn. 10
      alfa = 0.1D0


c ... Convert surface pressure and temperature to proper units.
      Pressure = SFCP/10.0D0
      if (Pressure .lt. 10.0D0) pressure = 100.0D0        ! Assume 100 kPa is missing
      Tcel = Ta-To

CPESc ... check to catch errors in temperature input
CPESc     This code is not used since currently no dew point data (Td)
CPESc     provided from AERMET.
CPES      if (Tcel.lt.Td .and. Td.lt.50.) then
CPES       Tcel = Tdry
CPES       Ta = Tcel+273.2
CPES      end if

      if (npd .eq. 0 .AND. LDGAS .AND. .NOT.LUSERVD) then
c ...    Assign parameters for gas deposition

c ...    Assign land use category for this direction based on user input.
c        Use method consistent with specification of IFVSEC
         ILAND_NDX = IDINT (AFV*0.10D0 + 0.4999D0)
         IF (ILAND_NDX .EQ. 0) ILAND_NDX = 36
         LANUSE = ILAND_GD(ILAND_NDX)

c ...    Assign Wesely "seasonal" category (1-5) based on calendar month
         ISEA5 = iseas_gd(imonth)

c ...    Assign surface roughness, stability factor and resistance terms
         Sfact   = Stab(LANUSE)
         Ri      = Restab(LANUSE,1,ISEA5)
         Rcs     = Restab(LANUSE,2,ISEA5)
         Rco     = Restab(LANUSE,3,ISEA5)
         Raci    = Restab(LANUSE,4,ISEA5)
         Rgs     = Restab(LANUSE,5,ISEA5)
         Rgo     = Restab(LANUSE,6,ISEA5)
c ...    Compute rLAI and reference solar irradiance as a function of
c        land use category and season.
         if (ISEA5.EQ.1 .OR. ISEA5.EQ.3 .OR. ISEA5.EQ.4) THEN
            F = 1.0D0
         else if (ISEA5 .EQ. 2) then
c           Assign user-supplied value for season 2, default it 0.50
            F = FSEAS2
         else if (ISEA5 .EQ. 5) then
c           Assign user-supplied value for season 5, default it 0.25
            F = FSEAS5
         end if
         if (LANUSE.eq.4 .or. LANUSE.eq.6) then
            rLAI = F
            Gr = 30.0D0
         else
            rLAI = DSQRT(F)
            Gr = 100.0D0
         end if

      else if (npd .eq. 0 .AND. LDGAS .AND. LUSERVD) then
         return

      end if

c ... Use Zrdep of SFCZ0 plus 1.0 meter for deposition option
      Zrdep = SFCZ0 + 1.0D0

c ... Check to avoid corruption by bad humidity input data
      if (RH.gt.100.0D0) RH = 100.0D0
      if (RH.lt.5.0D0) RH = 5.0D0

c ... Compute vapor pressure deficit
      de = ((100.0D0-RH)/100.0D0)*EsTa
      if (de.lt.0.0D0) de = 0.0D0

c ... Compute specific humidity at saturation (g/kg)
      qsat = 1.0D03*0.622D0*EsTa/(Pressure-0.378D0*EsTa)

c ... Compute ambient specific humidity (g/kg)
      vp = (RH/100.0D0)*EsTa
      q = 1.0D03*0.622D0*vp/(Pressure-0.378D0*vp)

c ... Compute specific humidity deficit (g/kg)
      dq = qsat-q
c ... For negative or zero humidity deficit, set dq=0.001 to avoid zero-divide
      if (dq.le.0.0D0) dq = 0.001D0

c ... Compute atmospheric resistance Ra

      if (obulen.ge.0.0D0) then
         Ra = (1.0D0/(VONKAR*ustar))*(DLOG(Zrdep/SFCZ0) + 
     &                                 5.0D0*Zrdep/obulen)

      else
c        Ra = (1.0D0/(VONKAR*ustar))*(log(Zrdep/SFCZ0)-
c    1   2.*log(0.5D0*(1.+sqrt(1.0D0-16.0D0*(Zrdep/obulen)))))
c ...   The following is the expanded form of the unstable Ra equation (2c)
        Ra = (1.0D0/(VONKAR*ustar))*DLOG(((DSQRT(1.0D0-
     &       16.0D0*Zrdep/obulen)-1.0D0)*
     &   (DSQRT(1.0D0-16.0D0*SFCZ0/obulen)+1.0D0))/
     &  ((DSQRT(1.0D0-16.0D0*Zrdep/obulen)+1.0D0)*(DSQRT(1.0D0-
     &       16.0D0*SFCZ0/obulen)-1.0D0)))
      end if

c ... Compute kinematic viscosity of air (m**2/s), with temp and presssure corrections
      Xnu = 0.1505D0*1.0D-4*((Ta/To)**1.772D0)*(Pressure/Po)*
     &     (1.0D0+0.0132D0*(Pressure-Po))

C***
      if (npd .eq. 0 .AND. LDGAS) then

c ...    Compute gas deposition velocity, vdepg

c ...    compute parameters necessary for bulk stomatal resistance
         f1 = ((QSW/Gr)+0.01D0)/((QSW/Gr)+1.0D0)
         if (f1 .le. 0.01D0) f1 = 0.01D0
         if (f1 .gt. 1.0D0)  f1 = 1.0D0
         if (Ri .eq. 1.0D07) f1 = 0.01D0
CPES     Calculation of Wnew and f2 moved outside the source loop

         f3 = 1.0D0/(1.0D0+alfa*de)
         if (f3.le.0.01D0) f3 = 0.01D0

         f4 = 1.0D0-0.0016D0*(298.2D0-Ta)**2
         if (f4.le.0.01D0) f4 = 0.01D0


c ...    Modify certain resistances if the surface is wetted (high humidity/
c        weak mixing or rain during current or previous two hours)
         ppp = Prate+prec1+prec2

CPES     Adjust Rcs and Rco based on cloud cover from AERMET
c ...    Determine factor "a" as a function of cloud cover
         a = 0.30D0
         if (NCLOUD.le.2) a = 0.45D0
         if (NCLOUD.ge.8) a = 0.15D0
         if (ISEA5 .EQ. 5 .and. IPCODE .gt.18 .and. Ta .LT. To) then
c ...       Skip adjustments for wetted surface if surface is snow covered
            Rcox = Rco
            continue
         else if ((ustar.lt.(a/dq).and.((ihour.lt.8).or.(ihour.gt.19)))
     &       .or. ppp.gt.0.0D0) then
            Rcs  = 50.0D0
            Rcox = 0.75D0*Rco
            Rgs  = 50.0D0
            if ((ustar.lt.(a/dq)).and.
     &         ((ihour.lt.8).or.(ihour.gt.19)))then
c ...          Limit Ra for gases if surface is wetted by dew
               if (Ra.lt.1000.0D0) Ra = 1000.0D0
            end if
         else
            Rcox = Rco
         end if

c ...    Calculate Rx term, used to adjust cuticular and ground terms for
c        hard freeze
         Rx = 1.0D03*DEXP(-(Ta-269.2D0))

c ...    drive some resistances to high values if there is a hard freeze
         Rcs  = Rcs + Rx
         Rgs  = Rgs + Rx
         Rgo  = Rgo + Rx
         Rcox = Rcox + Rx

c ...    then compute in-canopy aerodynamic resistance, Racx
         Rac = 0.3D0*Raci/ustar

c ...    Assign diffusivity for this source. Note that conversion of
c        diffusivity to m2/s is made in VDP1
         D_suba = pdiff(ISRC)

c ...    Compute quasiliminar resistance for bulk surface, Rb

         Rb = 2.2D0*((Xnu/D_suba)**(2.0D0*THIRD))/(VONKAR*ustar)

c ...    Compute bulk surface resistance, Rc

c ...    First compute the bulk canopy stomatal resistance, Rs

         Rs = Ri*(Dv/D_suba)/(f1*f2*f3*f4)
         if (Rs.gt.1.0D07) Rs = 1.0D07

c ...    Next compute bulk canopy leaf mesophyll resistance, Rm
c ...    The fo factor applies to ozone (fo=1.0) and nitrogen oxide (fo=0.1).
c        The fo factor should also be set to 1.0 for titanium tetrachloride
c        and divalent mercury, otherwise fo is 0.0

         Rm = 1.0D0/((0.034D0/HENRY(ISRC))+100.0D0*fo)
         if (Rm.gt.1.0D07) Rm = 1.0D07

c ...    Then compute cuticular resistance, Rcut
c        Note that Rcli is converted from s/cm to s/m in SUB. VDP1

         Rcl = Rcli(ISRC)/(rLAI*Sfact)
c ...    Adjust Rcl for hard freeze
         Rcl = Rcl + Rx
         if (Rcl.lt.100.0D0) Rcl = 100.0D0

         Rcut = 1.0D0/((1.0D-3/(HENRY(ISRC)*Rcs))+
     &        ((fo+fo*fo/HENRY(ISRC))/Rcox)+(1.0D0/Rcl))

c ...    Next compute ground resistance Rg

         Rg=1.0D0/((1.0D-3/(HENRY(ISRC)*Rgs))+((fo+(0.1D0*fo*fo))/Rgo))
         if (Rg.gt.1.0D07) Rg = 1.0D07

c ...    Finally, combine to compute Rc

         Rc = 1.0D0/((rLAI/(Rs+Rm))+(rLAI/Rcut)+(1.0D0/(Rac+Rg)))

c ...    Add the parallel resistances and take the inverse to compute the
c        deposition velocity for gases, Vdepg.

         Vdepg = 1.0D0/(Ra+Rb+Rc)

C***
      else if (npd .gt. 0) then
c ...    Compute particle deposition velocity, vdep

         if (.NOT. L_METHOD2(ISRC)) then
c ...       Calculate using existing ISCST3 method with modified Rb (Eq. 21)

c ---       Calculate t1 using Xnu, adjusted for ambient temp and pressure
            t1 = ustar*ustar/Xnu
c
c ---       LOOP OVER SIZE INTERVALS
            do i=1,npd
c
               st=tstop(i)*t1
c
c ---          Compute inertial impaction term
               xinert=10.0D0**(-3.0D0/st)
c
c ---          Calculate Schmidt number based on ambient temp and pressure
               D_sub_b = 8.09D-14 * (TA*SCF(I)/PDIAM(I))
               Schmidt(i) = Xnu/D_sub_b

c ---          Calculate unstable gusty wind adjustment for rd,
c              set factor to 1.0 for DFAULT option
               if (wstar .le. 0.0D0) then
                  gust_adj = 1.0D0
               else
                  gust_adj = 1.0D0 + 0.24D0*wstar*wstar/(ustar*ustar)
               end if
c
c ---          Compute the deposition layer resistance (s/m)
               rd(i) = 1.0D0 / (gust_adj * ustar *
     &                       (Schmidt(i)**(-2.0D0*THIRD) + xinert))
c
c ---          Deposition velocity for this particle size category
               vdep(i) =1.0D0/(ra+rd(i)+ra*rd(i)*vgrav(i))+vgrav(i)+
     &                                                     vdphor

            end do
c ***
            if(DEBUG .OR. DEPOSDBG)then
               write(iounit,*)
               write(iounit,*)'RA (s/m)    = ',ra
               write(iounit,*)'RD (s/m)    = ',(rd(n),n=1,npd)
               write(iounit,*)'VDEP (m/s)  = ',(vdep(n),n=1,npd)
            end if

         else if (L_METHOD2(ISRC)) then
c ...       Calculate deposition velocities for fine (vd1) and
c           coarse (vd2) particles for METHOD 2
            if (obulen .ge. 0.0D0) then
               Rp = 500.0D0/ustar
            else
               Rp = 500.0D0/(ustar*(1.0D0-(300.0D0/obulen)))
            end if

c
c ---       Calculate Schmidt number based on ambient temp and pressure
            D_sub_b = 8.09D-14 * (TA*SCF(1)/PDIAM(1))
            Schmidt(1) = Xnu/D_sub_b

            vd1 = 1.0D0/(Ra+Rp)
c ...       Assign value of 0.002 m/s for Vgrav for coarse mode of METHOD 2
            vd2 = 0.002D0+(1.0D0/(Ra+Rp+0.002D0*Ra*Rp))

c ...       Combine fine and coarse terms to get total deposition velocity.
c           Note that subscript 1 for vdep is used since NPD = 1 for METHOD 2.
            vdep(1) = finemass(isrc)*vd1 + (1.0D0-finemass(isrc))*vd2
         end if

      end if

c ... Write resistances and deposition velocities to separate files:
c     GDEP.DAT for gas deposition and PDEP.DAT for particle deposition.

      IF (DEBUG .OR. DEPOSDBG) THEN
         if (LDGAS .AND. npd .eq. 0) then
            write(GDEPDBG,1001) kurdat, isrc, ra, rb, rc, vdepg
1001        format(1x,i8,1x,i6,4(2x,e12.6))
         end if
         if (LDPART .AND. .NOT. L_METHOD2(ISRC)) then
            do i = 1, npd
               write(PDEPDBG,1002) kurdat, isrc, i, ra, rd(i), vgrav(i),
     &                         vdep(i)
1002           format(1x,i8,1x,i6,2x,i3,'  METHOD_1 ',4(2x,e12.6))
            end do
         else if (LDPART .AND. L_METHOD2(ISRC)) then
            write(PDEPDBG,1003) kurdat, isrc, ra, rp, vgrav(1), vdep(1)
1003        format(1x,i8,1x,i6,4x,'-  METHOD_2 ',4(2x,e12.6))
         end if
      END IF

      return
      end


c----------------------------------------------------------------------
      subroutine scavrat
c----------------------------------------------------------------------
c
c --- AERMOD     R.W. Brode, PES
c
c --- PURPOSE:  Compute the wet SCAVenging RATio for particles, as a
c               function of particle size, and for gases, based on
c               new algorithms developed by Chris Walcek
c
c --- MODIFIED: To add calculation of collision efficiency as a
c               function of particle size and raindrop size for
c               particulate emissions, based on Slinn (1984) and
c               Seinfeld and Pandis (1998).
c               R. W. Brode, MACTEC (f/k/a PES), Inc., 03/19/04
c
c --- INPUTS:
c     Global variables:
c            IPCODE - integer    - Precip. code (00-45)
c             PRATE - real       - Precip. rate (mm/hr)
c                TA - real       - Ambient Temperature (deg K)
c               NPD - integer    - Number of particle size categories
c
c --- OUTPUT:
c     Global variables:
c            WASHOUT- real array - Washout coefficient for particles
c            PSCVRT - real array - Scavenging ratio for particles (1/s)
c            GSCVRT - real       - Scavenging ratio for gases (1/s)
c            ECOLL  - real array - Collision efficiency for particles
c
c     Local variables:
c
c --- SCAVRAT called by:  PCALC, VCALC, ACALC
c --- SCAVRAT calls:      none
c----------------------------------------------------------------------
c
c --- Include common blocks
      USE MAIN1
      IMPLICIT NONE

C --- Assign RHOW, density of water (g/m^3), as a parameter = 1.0E6
      DOUBLE PRECISION, PARAMETER :: RHOW = 1.0D6
      INTEGER :: I, N
      DOUBLE PRECISION :: VFALL, RDROP, FSUBG, FSUBL,
     &           TABS, TRES, REYNOLD, STOKE, SSTAR, KAPPA,
     &           TERM1, TERM2, TERM3

      if(DEBUG .OR. DEPOSDBG)then
         write(iounit,*)
         write(iounit,*)'SUBR. SCAVRAT -- Inputs'
         write(iounit,*)'IPCODE               = ',ipcode
         write(iounit,*)'PRATE (mm/hr)        = ',prate
         write(iounit,*)'TA (deg K)           = ',ta
         write(iounit,*)'NPD                  = ',npd
         write(iounit,*)
      end if

c --- If no precipitation, no wet removal
      if(prate .EQ. 0.0D0) then
         if (npd .gt. 0) then
c ---       Set pscvrt(npd), washout(npd), and ecoll(npd) arrays to 0      
            pscvrt = 0.0D0
            washout= 0.0D0
            ecoll  = 0.0D0
         else
            gscvrt = 0.0D0
         end if
          
      else if (npd .gt. 0) then
CPES --- Apply deposition option based on Wesely, et. al. (2001), with
CPES     with modifications based on Chris Walcek, for particles.
CPES     ZSUBP is calculated in PCALC as the top of the plume or the PBL
CPES     height (ZI), whichever is greater.  The top of the plume is defined
CPES     as plume centerline height plus 2.15 sigma-z, evaluated at a downwind
CPES     distance of 20 kilometers.  Since STABLE hours are modeled as
CPES     unlimited mixing, ZSUBP is simply the top of the plume for those
CPES     hours.

C ---    Calculate the precipitaion fall speed, VFALL (m/s) based on
C        precipitation rate in mm/hr.
         VFALL = 3.75D0 * PRATE**0.111D0

C ---    Calculate rainfall droplet radius, RDROP (cm), based on precipitation
C        rate in mm/hr.
         RDROP = (PRATE**0.232D0) / 18.11D0

         DO I = 1, NPD
C ---       Calculate collision efficiency, ECOLL, as function of particle
C           size and raindrop size based on Slinn (1984) and Seinfeld and
C           Pandis (1998).

C ---       Calculate Reynolds number for raindrop
            REYNOLD = RDROP*0.01D0*VFALL/XNU
C           Calculate diffusion term, TERM1
            TERM1 = (4.0D0/(REYNOLD*SCHMIDT(I)))*(1.0D0+0.4D0*
     &               DSQRT(REYNOLD)*SCHMIDT(I)**THIRD+
     &               0.16D0*DSQRT(REYNOLD*SCHMIDT(I)))

C ---       Calculate ratio of particle diameter and raindrop diameter,
C           KAPPA, with adjustments for units
            KAPPA = (PDIAM(I)*1.0D-6)/(RDROP*0.02D0)
C           Calculate interception term, TERM2
C           The constant term 1.81E-2 is ratio of viscosity or air to water
            TERM2 = 4.0D0*KAPPA*(1.81D-2+KAPPA*
     &              (1.0D0+2.0D0*DSQRT(REYNOLD)))

C ---       Calculate Stokes number for raindrop
            STOKE = TSTOP(I)*(VFALL-VGRAV(I))/(RDROP*0.01D0)
C           Calculate critical Stokes number
            SSTAR = (1.2D0+DLOG(1.0D0+REYNOLD)/12.0D0)/
     &              (1.0D0+DLOG(1.0D0+REYNOLD))
            SSTAR = MIN( SSTAR, STOKE )
C           Calculate inertial impaction term, TERM3
            TERM3 = ((STOKE-SSTAR)/
     &               (STOKE-SSTAR+2.0D0*THIRD))**(1.5D0)
C           Scale TERM3, inertial impaction term,by ratio of water
C           density (1 g/cm**3) to particle density
            TERM3 = TERM3 * DSQRT(1.0D0/PDENS(I))

            ECOLL(I) = MIN( 1.0D0, TERM1 + TERM2 + TERM3 )

C ---       Calculate washout coefficient from Equation 29; factor of 0.01
C           converts drop radius from cm to m.
            WASHOUT(I) = 1.5D0 * (ZSUBP * ECOLL(I)) / 
     &                         (2.0D0*RDROP*0.01D0)

            IF (PRATE .GT. 0.0D0) THEN
C ---          Calculate scavenging rate (1/s); factor of 3.6E4 converts drop
C              radius from cm to mm, and converts hours to seconds.
               PSCVRT(I) = 1.5D0 * ECOLL(I) * PRATE / 
     &                          (2.0D0*RDROP * 3.6D4)
            ELSE
               PSCVRT(I) = 0.0D0
            END IF
         END DO
      else
CPES --- Apply deposition option based on Wesely, et. al. (2001),
CPES     with modifications based on Chris Walcek, for gases.
CPES     ZSUBP is calculated in PCALC as the top of the plume or the PBL
CPES     height (ZI), whichever is greater.  The top of the plume is defined
CPES     as plume centerline height plus 2.15 sigma-z, evaluated at a downwind
CPES     distance of 20 kilometers.

C ---    Calculate the precipitaion fall speed, VFALL (m/s) based on
C        precipitation rate in mm/hr.
         VFALL = 3.75D0 * PRATE**0.111D0

C ---    Calculate rainfall droplet radius, RDROP (cm), based on precipitation
C        rate in mm/hr.
         RDROP = (PRATE**0.232D0) / 18.11D0

C ---    Calculate liquid content of falling rain, LIQCONT (g/m^3), based on
C        precipitation rate in mm/hr.
         LIQCONT = (PRATE**0.889D0) / 13.28D0

C ---    Calculate gas-side diffusion enhancement factor, FSUBG (unitless),
C        based on droplet radius (cm).  Linear approximation based on
C        Figure 13-20 of "Microphysics of Clouds and Precipitation" by
C        Hans Pruppacher and James Klett.
         FSUBG = 80.0D0*RDROP + 1.0D0

C ---    Set the liquid-side diffusion enhancement factor, FSUBL (unitless),
C        based on droplet radius.
         IF (RDROP .LT. 0.01D0) THEN
            FSUBL = 1.0D0
         ELSE IF (RDROP .LE. 0.05D0) THEN
            FSUBL = 2.6D0
         ELSE
            FSUBL = 20.0D0
         END IF

C ---    Calculate the absorption time scale, TABS (s); first calculate
C        term in the denominator.
         DENOM = (1.0D0 + (LIQCONT*RGAS*TA)/(HENRY(ISRC)*RHOW))
         TABS=(RDROP**2*RGAS*TA/(HENRY(ISRC)*3.0D0*PDIFF(ISRC)*
     &        1.0D4*FSUBG)+(4.0D0*RDROP*RGAS*TA)/(HENRY(ISRC)*3.0D0*
     &        50000.0D0*0.01D0)+(RDROP**2*0.17D0)/(3.0D0*PDIFFW(ISRC)*
     &        1.0D4*FSUBL))/DENOM

C ---    Calculate the residence time of drops in the plume, TRES (s);
C        ZSUBP is defined in PCALC.
         TRES = ZSUBP/VFALL

C ---    Calculate the fraction of saturation, FRACSAT, based on time scales.
         FRACSAT = MIN( 1.0D0, TRES/TABS )

C ---    Calculate equivalent scavenging rate (1/s)
         GSCVRT = (FRACSAT*RGAS*TA*PRATE)/
     &            (3600.0D0*ZSUBP*HENRY(ISRC)*1.0D3*DENOM)

      end if

      if(DEBUG .OR. DEPOSDBG)then
         write(iounit,*)'SUBR. SCAVRAT -- Results'
         if (npd .eq. 0) then
            write(iounit,*)'GSCVRT (1/s)= ',gscvrt
         else if (npd .gt. 0) then
            write(iounit,*)'PSCVRT (1/s)= ',(pscvrt(n),n=1,npd)
            write(iounit,*)'COLL. EFF.  = ',(ecoll(n),n=1,npd)
            write(iounit,*)'WASHOUT COEF= ',(washout(n),n=1,npd)
         end if
         write(iounit,*)
      end if

      return
      end

      SUBROUTINE PDEP (XARG)
C***********************************************************************
C               PDEP Module of AERMOD Model
C
C        PURPOSE: Calculates Deposition Adjustment Factors from DEPLETE
C
C        PROGRAMMER: R. W. Brode, MACTEC/PES, Inc.
C
C        DATE:       September 20, 2003
C
C
C        INPUTS:
C
C
C        OUTPUTS:
C
C
C        CALLED FROM:
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I
      DOUBLE PRECISION :: XARG

C     Variable Initializations
      MODNAM = 'PDEP'

C     Loop over particle sizes
      DO I = 1, NPD
         DQCOR(I) = 1.0D0
         WQCOR(I) = 1.0D0
         IF (DDPLETE) THEN
C           Determine factor for dry depletion
            VSETL = VGRAV(I)
            CALL DEPLETE(VDEP(I),XARG,ROMBERG,DQCOR(I))
         END IF
         IF (WDPLETE .AND. PSCVRT(I).GT.0.0D0) THEN
C           Determine source depletion factor from wet removal
C           Simple Terrain Model
            WQCOR(I) = DEXP(-PSCVRT(I)*XARG/US)
         END IF
      END DO

      RETURN
      END


      SUBROUTINE PDEPG (XARG)
C***********************************************************************
C               PDEPG Module of AERMOD Model
C
C        PURPOSE: Calculates Deposition Adjustment Factors from DEPLETE
C                 for Gases
C
C        PROGRAMMER: R. W. Brode, MACTEC/PES, Inc.
C
C        DATE:       September 29, 2003
C
C        INPUTS:
C
C
C        OUTPUTS:
C
C
C        CALLED FROM:
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      DOUBLE PRECISION :: XARG

C     Variable Initializations
      MODNAM = 'PDEPG'

C     Initialize source depletion factors to unity.
      DQCORG  = 1.0D0
      WQCORG  = 1.0D0
      IF (DDPLETE) THEN
C        Determine factor for dry depletion
         CALL DEPLETE( VDEPG, XARG, ROMBERG, DQCORG)
      END IF
      IF (WDPLETE .AND. GSCVRT.GT.0.0D0) THEN
C        Determine source depletion factor
C        from wet removal (GASES)
C        Simple Terrain Model
         WQCORG = DEXP(-GSCVRT*XARG/US)
      END IF

      RETURN
      END

c-----------------------------------------------------------------------
      subroutine deplete(vdi, xri, lromb, qcor)
c-----------------------------------------------------------------------
c
c --- DEPLETE Module of AERMOD
c              R.W. Brode, MACTEC/PES
c
c PURPOSE:     Subroutine DEPLETE provides the value of the integral of
c              the vertical distribution function over the travel of the
c              plume from the source to the receptor.  Integration is
c              performed by 2-point gaussian quadrature or Romberg
c              integration method, depending on logical argument, lromb.
c
c ARGUMENTS:
c    PASSED:   vdi      deposition velocity (m/s)              [r]
c              vsi      gravitational settling velocity (m/s)  [r]
c              xri      distance from source to receptor (m)   [r]
c              hmixi    mixing height (m)                      [r]
c              lromb    logical for use of Romberg integration [l]
c
c  RETURNED:   qcor     ratio of depleted emission rate to original  [r]
c
c CALLING ROUTINES:   PDEP, PDEPG
c
c EXTERNAL ROUTINES:  F2INT, QATR2, QG2D2
c-----------------------------------------------------------------------

c     Set up call to QATR2(xl,xu,eps,ndim2,fct,y,ier,num,aux2)
c     Declare parameter to fix the size of the aux2 array
      IMPLICIT NONE

      LOGICAL LROMB
      DOUBLE PRECISION :: VDI, XRI, QCOR, EPS, VALUE
      DOUBLE PRECISION, external :: F2INT
      INTEGER NUM, IER
      INTEGER, parameter :: ndim2=12
      DOUBLE PRECISION aux2(ndim2)

C     Evaluate integral, Use Romberg if LROMB=.T., otherwise use
C     two-point Gaussian Quadrature:
      IF (LROMB) THEN
C        Use ROMBERG Integration
         eps = 0.050D0
         call QATR2(1.0D0,xri,eps,ndim2,F2INT,value,ier,num,aux2)
      ELSE
C        Use 2-point Gaussian Quadrature
         CALL QG2D2(1.0D0,XRI,F2INT,VALUE)
      END IF

      if (vdi*value .gt. 50.0D0) then
c        Potential underflow, limit product to 50.0
         value = 50.0D0/vdi
      else if (vdi*value .lt. -50.0D0) then
c        Potential overflow, limit product to 50.0
         value = -50.0D0/vdi
      end if

      qcor=DEXP(-vdi*value)

      return
      end

c-----------------------------------------------------------------------
      function f2int(xi)
c-----------------------------------------------------------------------
c
c --- F2INT Module of AERMOD
c              R.W. Brode, MACTEC/PES
c
c PURPOSE:     Function is the integrand of integral over the travel
c              distance to obtain the fraction of material removed from
c              the plume. Module MAIN1 is used to pass data that are
c              constant during the integration, so QATR (the integrator)
c              only needs to pass values of distance.
c
c ARGUMENTS:
c    PASSED:  xi        distance from source                         [r]
c
c  RETURNED:  f2int     value of integrand                           [r]
c
c CALLING ROUTINES:   QATR2, QG2D2
c
c EXTERNAL ROUTINES:
c
c-----------------------------------------------------------------------
c
      USE MAIN1
      IMPLICIT NONE

      DOUBLE PRECISION XI, VWRAP, VLIFT, F2INT, ZETMP, ZHTMP

C     Initialize VWRAP and VLIFT
      VWRAP = 0.0D0
      VLIFT = 0.0D0

C     Set initial effective parameters
      UEFF  = US
      SVEFF = SVS
      SWEFF = SWS
      TGEFF = TGS
      IF ( UNSTAB  .AND.  (HS .LT. ZI) ) THEN
         UEFFD  = US
         SVEFFD = SVS
         SWEFFD = SWS
         UEFFN  = US
         SVEFFN = SVS
         SWEFFN = SWS
         UEFF3  = US
         SVEFF3 = SVS
         SWEFF3 = SWS
         TGEFF3 = TGS
      END IF

C     Set temporary receptor elevation and height scale
      ZETMP = ZELEV
      ZELEV = ZS + (ZETMP-ZS)*XI/XDIST

      ZHTMP = ZHILL
      ZHILL = HS + (ZHTMP-HS)*XI/XDIST

C     Define plume centroid height (CENTER) for use in
C     inhomogeneity calculations
      CALL CENTROID ( XI )

      IF (SRCTYP(ISRC)(1:5) .EQ. 'POINT') THEN
C        Calculate the plume rise                     ---   CALL DELTAH
         CALL DELTAH ( XI )
      END IF

C     If the atmosphere is unstable and the stack
C     top is below the mixing height, calculate
C     the CBL PDF coefficients                     ---   CALL PDF
      IF( UNSTAB  .AND.  (HS .LT. ZI) ) THEN
         CALL PDF
      END IF

C     Determine Effective Plume Height             ---   CALL HEFF
      CALL HEFF ( XI )

C     Compute effective parameters using an
C     average through plume layer
      CALL IBLVAL ( XI )

C     Call PDF & HEFF again for final CBL plume heights
      IF (UNSTAB .AND. (HS.LT.ZI) ) THEN
         CALL PDF
         CALL HEFF ( XI )
      END IF

C     Determine Dispersion Parameters              ---   CALL PDIS
      CALL PDIS ( XI )

C     Calculate Plume Tilt Due to Settling, HV
      HV = (XI/US) * VSETL

      IF (STABLE .OR. (UNSTAB.AND.(HS.GE.ZI))) THEN
C        Calculate height of the "effective reflecting surface"
C        Calculate Settled Plume Height(s), HESETL
         HESETL = MAX( 0.0D0, HE - HV )
         CALL REFL_HT (HESETL, XI, SZB, 0.0D0, HSBL)
      ELSE IF ( UNSTAB ) THEN
         HESETL = MAX( 0.0D0, 0.5D0*(HED1+HED2) - HV )
         HSBL = 0.0D0
      END IF

      IF (UNSTAB .AND. (HS.LT.ZI) .AND. (PPF.GT.0.0D0)) THEN
C        Calculate height of the "effective reflecting surface"
C        Calculate Settled Plume Height(s), HE3SETL
         HE3SETL = MAX( 0.0D0, HE3 - HV )
         CALL REFL_HT (HE3SETL, XI, SZB3, 0.0D0, HPEN)
         HPEN = MAX( HPEN, ZI )
      ELSE
         HPEN = 0.0D0
      END IF

C     Determine the CRITical Dividing Streamline---   CALL CRITDS
      CALL CRITDS (HESETL)

C     Calculate the fraction of plume below
C     HCRIT, PHEE                               ---   CALL PFRACT
      CALL PFRACT (HESETL)

C     Calculate FOPT = f(PHEE)                  ---   CALL FTERM
      CALL FTERM

      IF (FOPT .EQ. 0.0D0) THEN
         VWRAP = 0.0D0
      ELSE
C        Assign receptor height for vertical term calculations
         ZR = ZRT + ZRDEP

         IF( STABLE  .OR.  (UNSTAB .AND. (HS .GE. ZI) ) ) THEN
C           Calculate the vertical term, FSUBZ              ---   CALL VRTSBL
C           With stable plume reflections and effective Zi
            IF (ZR .LE. HSBL) THEN
               CALL VRTSBL (SZ, MAX( 0.0D0, HE-HV ), HSBL)
            ELSE
               CALL VRTSBN (SZ, MAX( 0.0D0, HE-HV ))
            END IF

C           Calculate value of integral, VWRAP
            VWRAP = FSUBZ/UEFF

         ELSE IF( UNSTAB )THEN
            IF (PPF .LT. 1.0D0) THEN
C              Calculate the vertical term for the direct plume, FSUBZD
               IF (ZR .LE. ZI) THEN
C                 Calculation for Receptor below Zi      ---   CALL VRTCBL
                  CALL VRTCBL ( HED1-HV, HED2-HV, SZD1, SZD2, 1.0D0 )
                  FSUBZD = FSUBZ
               ELSE
C                 Set FSUBZ = 0.0 for "receptor height" (ZR) > ZI
                  FSUBZD = 0.0D0
               END IF

C              Calculate the vertical term for the indirect plume, FSUBZN
               IF (ZR .LE. ZI) THEN
C                 Calculation for Receptor below Zi      ---   CALL VRTCBL
                  CALL VRTCBL ( HEN1-HV, HEN2-HV, SZN1, SZN2, -1.0D0 )
                  FSUBZN = FSUBZ
               ELSE
C                 Set FSUBZ = 0.0 for "receptor height" (ZR) > ZI
                  FSUBZN = 0.0D0
               END IF
            ELSE
               FSUBZD = 0.0D0
               FSUBZN = 0.0D0

            END IF

C           Note that UEFF and UEFF3 can never be zero, since they get
C           set to a minimum value earlier on.

            IF( PPF .GT. 0.0D0 )THEN
C              Calculate the vertical term for the penetrated
C              plume, FSUBZ3                                ---   CALL VRTSBL
               IF (ZR .LE. HPEN) THEN
                  CALL VRTSBL (SZ3, MAX(0.0D0,HE3-HV), HPEN)
               ELSE
                  CALL VRTSBN (SZ3, MAX(0.0D0,HE3-HV))
               END IF
               FSUBZ3 = FSUBZ

C              Calculate value of integral, VWRAP
               IF (PPF .LT. 1.0D0) THEN
                  VWRAP = (1.0D0-PPF)*FSUBZD/UEFFD +
     &                    (1.0D0-PPF)*FSUBZN/UEFFN +
     &                    PPF*FSUBZ3/UEFF3
               ELSE
                  VWRAP = PPF*FSUBZ3/UEFF3
               END IF

            ELSE
               FSUBZ3 = 0.0D0
               HPEN   = 0.0D0

C              Calculate value of integral, VWRAP
               VWRAP = FSUBZD/UEFFD +
     &                 FSUBZN/UEFFN

            END IF

         END IF
      END IF

C---- Calculate the contribution due to terrain-following plume, VLIFT
      IF (ZRT .EQ. 0.0D0) THEN
C----    Effective receptor heights are equal, therefore VLIFT = VWRAP
         VLIFT = VWRAP
      ELSE IF (FOPT .EQ. 1.0D0) THEN
         VLIFT = 0.0D0
      ELSE
C        Assign receptor height for vertical term calculations
         ZR = ZRDEP

         IF( STABLE  .OR.  (UNSTAB .AND. (HS .GE. ZI) ) ) THEN
C           Calculate the vertical term, FSUBZ              ---   CALL VRTSBL
C           With stable plume reflections and effective Zi
            IF (ZR .LE. HSBL) THEN
               CALL VRTSBL (SZ, MAX( 0.0D0, HE-HV ), HSBL)
            ELSE
               CALL VRTSBN (SZ, MAX( 0.0D0, HE-HV ))
            END IF

C           Calculate value of integral, VLIFT
            VLIFT = FSUBZ/UEFF

         ELSE IF( UNSTAB )THEN
            IF (PPF .LT. 1.0D0) THEN
C              Calculate the vertical term for the direct plume, FSUBZD
               IF (ZR .LE. ZI) THEN
C                 Calculation for Receptor below Zi      ---   CALL VRTCBL
                  CALL VRTCBL ( HED1-HV, HED2-HV, SZD1, SZD2, 1.0D0 )
                  FSUBZD = FSUBZ
               ELSE
C                 Set FSUBZ = 0.0 for "receptor height" (ZR) > ZI
                  FSUBZD = 0.0D0
               END IF

C              Calculate the vertical term for the indirect plume, FSUBZN
               IF (ZR .LE. ZI) THEN
C                 Calculation for Receptor below Zi      ---   CALL VRTCBL
                  CALL VRTCBL ( HEN1-HV, HEN2-HV, SZN1, SZN2, -1.0D0 )
                  FSUBZN = FSUBZ
               ELSE
C                 Set FSUBZ = 0.0 for "receptor height" (ZR) > ZI
                  FSUBZN = 0.0D0
               END IF
            ELSE
               FSUBZD = 0.0D0
               FSUBZN = 0.0D0

            END IF

C           Note that UEFF and UEFF3 can never be zero, since they get
C           set to a minimum value earlier on.

            IF( PPF .GT. 0.0D0 )THEN
C              Calculate the vertical term for the penetrated
C              plume, FSUBZ3                                ---   CALL VRTSBL
               IF (ZR .LE. HPEN) THEN
                  CALL VRTSBL (SZ3, MAX(0.0D0,HE3-HV), HPEN)
               ELSE
                  CALL VRTSBN (SZ3, MAX(0.0D0,HE3-HV))
               END IF
               FSUBZ3 = FSUBZ

C              Calculate value of integral, VLIFT
               IF (PPF .LT. 1.0D0) THEN
                  VLIFT = (1.0D0-PPF)*FSUBZD/UEFFD +
     &                    (1.0D0-PPF)*FSUBZN/UEFFN +
     &                    PPF*FSUBZ3/UEFF3
               ELSE
                  VLIFT = PPF*FSUBZ3/UEFF3
               END IF

            ELSE
               FSUBZ3 = 0.0D0
               HPEN   = 0.0D0

C              Calculate value of integral, VLIFT
               VLIFT = FSUBZD/UEFFD +
     &                 FSUBZN/UEFFN

            END IF

         END IF
      END IF

C     Blend horizontal and terrain-responding components of integral
      F2INT = FOPT * VWRAP + (1.0D0 - FOPT) * VLIFT

C     Reassign receptor elevation and height scales
      ZELEV = ZETMP
      ZHILL = ZHTMP

      RETURN
      END

      SUBROUTINE PRM_PDEP (XARG)
C***********************************************************************
C               PRM_PDEP Module of AERMOD Model
C
C        PURPOSE: Calculates Deposition Adjustment Factors from
C                 PRM_DEPLETE for PRIME component
C
C        PROGRAMMER: R. W. Brode, PES, Inc.
C
C        DATE:       September 29, 1994
C
C
C        INPUTS:
C
C
C        OUTPUTS:
C
C
C        CALLED FROM:
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I
      DOUBLE PRECISION :: XARG

C     Variable Initializations
      MODNAM = 'PRM_PDEP'

C     Loop over particle sizes
      DO I = 1, NPD
         DQCOR(I) = 1.0D0
         WQCOR(I) = 1.0D0
         IF (DDPLETE) THEN
C           Determine factor for dry depletion
            VSETL = VGRAV(I)
            CALL PRM_DEPLETE(VDEP(I),XARG,ROMBERG,DQCOR(I))
         END IF
         IF (WDPLETE .AND. PSCVRT(I).GT.0.0D0) THEN
C           Determine source depletion factor from wet removal
C           Simple Terrain Model
            WQCOR(I) = DEXP(-PSCVRT(I)*XARG/US)
         END IF
      END DO

      RETURN
      END


      SUBROUTINE PRM_PDEPG (XARG)
C***********************************************************************
C               PRM_PDEPG Module of AERMOD Model
C
C        PURPOSE: Calculates Deposition Adjustment Factors from
C                 PRM_DEPLETE for PRIME component for gases
C
C        PROGRAMMER: R. W. Brode, MACTEC/PES, Inc.
C
C        DATE:       September 29, 2003
C
C        INPUTS:
C
C
C        OUTPUTS:
C
C
C        CALLED FROM:
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      DOUBLE PRECISION :: XARG

C     Variable Initializations
      MODNAM = 'PRM_PDEPG'

C     Initialize source depletion factors to unity.
      DQCORG  = 1.0D0
      WQCORG  = 1.0D0
      IF (DDPLETE) THEN
C        Determine factor for dry depletion
         CALL PRM_DEPLETE( VDEPG, XARG, ROMBERG, DQCORG)
      END IF
      IF (WDPLETE .AND. GSCVRT.GT.0.0D0) THEN
C        Determine source depletion factor
C        from wet removal (GASES)
C        Simple Terrain Model
         WQCORG = DEXP(-GSCVRT*XARG/US)
      END IF

      RETURN
      END

c-----------------------------------------------------------------------
      subroutine PRM_deplete(vdi, xri, lromb, qcor)
c-----------------------------------------------------------------------
c
c --- PRM_DEPLETE Module of AERMOD
c
c
c PURPOSE:     Subroutine PRM_DEPLETE provides the value of the integral of
c              the vertical distribution function over the travel of the
c              plume from the source to the receptor for the PRIME downwash
c              component.  Integration is performed by 2-point gaussian
c              quadrature or Romberg integration method, depending on
c              logical argument, lromb.
c
c ARGUMENTS:
c    PASSED:   vdi      deposition velocity (m/s)              [r]
c              vsi      gravitational settling velocity (m/s)  [r]
c              xri      distance from source to receptor (m)   [r]
c              hmixi    mixing height (m)                      [r]
c              lromb    logical for use of Romberg integration [l]
c
c  RETURNED:   qcor     ratio of depleted emission rate to original  [r]
c
c CALLING ROUTINES:   PRM_PDEP, PRM_PDEPG
c
c EXTERNAL ROUTINES:  PRM_F2INT, QATR2, QG2D2
c-----------------------------------------------------------------------

c     Set up call to QATR2(xl,xu,eps,ndim2,fct,y,ier,num,aux2)
c     Declare parameter to fix the size of the aux2 array
      IMPLICIT NONE

      LOGICAL LROMB
      DOUBLE PRECISION :: VDI, XRI, QCOR, EPS, VALUE
      DOUBLE PRECISION, external :: PRM_F2INT
      INTEGER NUM, IER
      INTEGER, parameter :: ndim2=12
      DOUBLE PRECISION aux2(ndim2)

C     Evaluate integral, Use Romberg if LROMB=.T., otherwise use
C     two-point Gaussian Quadrature:
      IF (LROMB) THEN
C        Use ROMBERG Integration
         eps = 0.05D0
         call QATR2(1.0D0,xri,eps,ndim2,PRM_F2INT,value,ier,num,aux2)
      ELSE
C        Use 2-point Gaussian Quadrature
         CALL QG2D2(1.0D0,XRI,PRM_F2INT,VALUE)
      END IF

      if (vdi*value .gt. 50.0D00) then
c        Potential underflow, limit product to 50.0
         value = 50.0D0/vdi
      else if (vdi*value .lt. -50.0D0) then
c        Potential overflow, limit product to 50.0
         value = -50.0D0/vdi
      end if

      qcor=DEXP(-vdi*value)

      return
      end

c-----------------------------------------------------------------------
      function PRM_f2int(xi)
c-----------------------------------------------------------------------
c
c --- PRM_F2INT Module of AERMOD
c              R.W. Brode, MACTEC/PES
c
c PURPOSE:     Function is the integrand of integral over the travel
c              distance to obtain the fraction of material removed from
c              the plume for the PRIME downwash component. Module MAIN1
c              is used to pass data that are constant during the
c              integration, so QATR (the integrator) only needs to
c              pass values of distance.
c
c ARGUMENTS:
c    PASSED:  xi         distance from source                        [r]
c
c  RETURNED:  PRM_f2int  value of integrand                          [r]
c
c CALLING ROUTINES:   QATR2, QG2D2
c
c EXTERNAL ROUTINES:
c
c-----------------------------------------------------------------------
      USE MAIN1
      IMPLICIT NONE

      DOUBLE PRECISION :: XI, VWRAP, VLIFT, PRM_F2INT, ZETMP, ZHTMP
      DOUBLE PRECISION :: DHPOUT, SYOUT, SZOUT, SYCAV, SZCAV
      LOGICAL L_INWAKE

C     Set temporary receptor elevation and height scale
      ZETMP = ZELEV
      ZELEV = ZS + (ZETMP-ZS)*XI/X

      ZHTMP = ZHILL
      ZHILL = HS + (ZHTMP-HS)*XI/X

C     Calculate the plume rise                     ---   CALL PRMDELH
      CALL PRMDELH ( XI, L_INWAKE )

C     Determine Effective Plume Height             ---   CALL PRMHEFF
      CALL PRMHEFF

c --- Calculate sigmas
      dhpout = dhp
      call WAKE_XSIG(XI,hs,dhpout,nobid,szout,syout,
     &               szcav,sycav)
      sy = syout
      sz = szout

C     Calculate Plume Tilt Due to Settling, HV
      HV = (XI/US) * VSETL
      HE = MAX( 0.0D0, HE - HV )

C     Calculate FOPT = f(PHEE)                  ---   CALL FTERM
      FOPT = 0.5D0

C     Assign receptor height for vertical term calculations
      ZR = ZRT + ZRDEP

      IF (STABLE) THEN
         CALL VRTSBN (SZ, HE)
      ELSE IF (UNSTAB .AND. HE.LE.ZI) THEN
         CALL VRTSBL (SZ, HE, ZI)
      ELSE
         FSUBZ = 0.0D0
      END IF

      VWRAP = FSUBZ/UEFF

C---- Calculate the contribution due to terrain-following plume, VLIFT
      IF (ZRT .EQ. 0.0D0) THEN
C----    Effective receptor heights are equal, therefore VLIFT = VWRAP
         VLIFT = VWRAP
      ELSE IF (FOPT .EQ. 1.0D0) THEN
         VLIFT = 0.0D0
      ELSE
C        Assign receptor height for vertical term calculations
         ZR = ZRDEP
         IF (STABLE) THEN
            CALL VRTSBN (SZ, HE)
         ELSE IF (UNSTAB .AND. HE.LE.ZI) THEN
            CALL VRTSBL (SZ, HE, ZI)
         ELSE
            FSUBZ = 0.0D0
         END IF
         VLIFT = FSUBZ/UEFF
      END IF

C     Blend horizontal and terrain-responding components of integral
      PRM_F2INT = FOPT * VWRAP + (1.0D0 - FOPT) * VLIFT

C     Reassign receptor elevation and height scales
      ZELEV = ZETMP
      ZHILL = ZHTMP

      RETURN
      END

c-----------------------------------------------------------------------
      subroutine qatr2(xl,xu,eps,ndim,fct,y,ier,i,aux)
c-----------------------------------------------------------------------
c
c --- ISCST2    Version: 1.0            Level: 930930           QATR2
c
c PURPOSE:      Integration routine adapted from the IBM SSP program
c               DQATR.  Modified for single precision.  This is a COPY
c               of QATR for use in double integrations.
c
c MODIFIED:     To use new convergence criteria, including a lower
c               threshold in the value of the integral (1.0E-10), and
c               to check for "delta-x" < 1.0 meters (delta-x = hh).
c               R. W. Brode, PES, Inc. - 9/30/94
c
c ARGUMENTS:
c    PASSED:    xl,xu   lower and upper limits of integration        [r]
c               eps     fractional error used to define convergence  [r]
c               ndim    dimension of array aux (parameter)           [p]
c               fct     external function (integrand)
c               aux     working array, passed to allow variable dim. [r]
c  RETURNED:    y       value of integral                            [r]
c               ier     status flag at terminatio                    [i]
c               i       number of subdivision steps                  [i]
c
c CALLING ROUTINES:     DEPLETE
c
c EXTERNAL ROUTINES:    none
c-----------------------------------------------------------------------

c  NOTES: status flags denote the following --
c               ier=0   value of integral converged to within eps
c               ier=1   value of integral is diverging
c               ier=2   value of integral did not converge to within
c                       eps before ndim limit was reached

c  NDIM Note:  The aux(ndim) array keeps track of the average value of
c              the integrand for each of the steps in subdividing the
c              interval.  For example, when i=4 in the "do 7 i=2,ndim"
c              loop, aux(4) contains the mean value as obtained from
c              the trapezoidal rule, while aux(1 through 3) contain
c              a set of current Romberg extrapolations.  At each new
c              value of i, the interval is subdivided again, and the
c              integrand is evaluated at jj=2**(i-2) new points.
c              Therefore, at i=5, there will be jj=8 new points added
c              to the 9 points already used in the interval.  When i=17
c              there will be jj=32,768 new points added to the 32,769
c              already used.  This is the maximum number of new points
c              that are allowed as jj is an INTEGER*2 variable, with
c              a maximum value of 2**15.  Therefore, i should not exceed
c              17, and probably should be no larger than 16.  This means
c              that NDIM should be set at 16.  Larger values of NDIM
c              could be accepted if the INTEGER*2 variables were changed
c              to INTEGER*4, but for most applications, 30000 to 60000
c              points ought to be sufficient for evaluating an integral.

      IMPLICIT NONE

      INTEGER :: NDIM, IER
      DOUBLE PRECISION :: Y, EPS, XU, XL, AUX(NDIM), HALF, FCT, H, HH,
     &                    DELT2, P, DELT1, HD, X, SM, Q
      EXTERNAL fct
      integer i,ii,ji,j,jj
      half=0.5D0

c     Preparations for Romberg loop
      aux(1)=half*(fct(xl)+fct(xu))
      h=xu-xl

      if(h .EQ. 0.0D0 .OR. aux(1) .EQ. 0.0D0) then
         ier=0
         y = 0.0D0
         return
      end if

      hh=h
      delt2=0.0D0
      p=1.0D0
      jj=1

      do i=2,ndim
         y=aux(1)
         delt1=delt2
         hd=hh
         hh=half*hh
         p=half*p
         x=xl+hh
         sm=0.0D0

         do j=1,jj
            sm=sm+fct(x)
            x=x+hd
         end do

c  A new approximation to the integral is computed by means
c  of the trapezoidal rule
         aux(i)=half*aux(i-1)+p*sm

c  Start of Rombergs extrapolation method

         q=1.0D0
         ji=i-1
         do j=1,ji
            ii=i-j
            q=q+q
            q=q+q
            aux(ii)=aux(ii+1)+(aux(ii+1)-aux(ii))/(q-1.0D0)
         end do

c  End of Romberg step

         delt2=DABS(y-aux(1))

         if (i .GE. 3) then
c  Modification for cases in which function = 0 over interval
crwb        add lower threshold convergence test
            if (aux(1) .LT. 1.0D-10) then
               ier=0
               y=h*aux(1)
               return
            elseif (delt2 .LE. eps*DABS(aux(1)) ) then
               ier=0
               y=h*aux(1)
               return
crwb        add lower limit on "delta-x" of 1.0m
            elseif (hh .LT. 1.0D0) then
               ier=0
               y=h*aux(1)
               return
c           elseif (delt2 .GE. delt1)then
c              ier=1
c              y=h*y
c              return
            endif
         endif
        jj=jj+jj
      end do

      ier=2
      y=h*aux(1)

      return
      end



      SUBROUTINE QG2D2(XL,XU,FCT,Y)
C     ..................................................................
C
C        SUBROUTINE QG2D2
C
C        PURPOSE
C           TO COMPUTE INTEGRAL(FCT(X), SUMMED OVER X FROM XL TO XU)
C
C        USAGE
C           CALL QG2 (XL,XU,FCT,Y)
C           PARAMETER FCT REQUIRES AN EXTERNAL STATEMENT
C
C        DESCRIPTION OF PARAMETERS
C           XL     - THE LOWER BOUND OF THE INTERVAL.
C           XU     - THE UPPER BOUND OF THE INTERVAL.
C           FCT    - THE NAME OF AN EXTERNAL FUNCTION SUBPROGRAM USED.
C           Y      - THE RESULTING INTEGRAL VALUE.
C
C        REMARKS
C           NONE
C
C        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C           THE EXTERNAL FUNCTION SUBPROGRAM FCT(X) MUST BE FURNISHED
C           BY THE USER.
C
C        METHOD
C           EVALUATION IS DONE BY MEANS OF 2-POINT GAUSS QUADRATURE
C           FORMULA, WHICH INTEGRATES POLYNOMIALS UP TO DEGREE 3
C           EXACTLY.
C           FOR REFERENCE, SEE
C           V.I.KRYLOV, APPROXIMATE CALCULATION OF INTEGRALS,
C           MACMILLAN, NEW YORK/LONDON, 1962, PP.100-111 AND 337-338.
C
C     ..................................................................
C
C
      IMPLICIT NONE

      DOUBLE PRECISION :: A, B, Y, XL, XU, FCT
      EXTERNAL FCT

      A = 0.5D0*(XU+XL)
      B = XU-XL
      Y = 0.2886751D0*B
      Y = 0.5D0*B*(FCT(A+Y)+FCT(A-Y))

      RETURN
      END

      SUBROUTINE OLM_CALC
C***********************************************************************
C             OLM_CALC Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Processes Hourly Results for OLM Option
C
C        PROGRAMMER: Roger W. Brode, PES, Inc.
C
C        DATE:    May 6, 2002
C
C        MODIFIED: Incorporated equilibrium ratio for OLM option,
C                  with default of 0.90, as with PVMRM option.
C                  R. W. Brode, U.S. EPA, OAQPS, AQMG, 02/28/2011
C
C        MODIFIED: Corrected initialization problem with OLMGROUP
C                  keyword option.  NO2VAL and NO_VAL arrays need
C                  to be reinitialized for each receptor.
C                  R. W. Brode, U.S. EPA, OAQPS, AQMG, 10/19/2009
C
C        INPUTS:
C
C
C        OUTPUTS:
C
C
C        CALLED FROM:   HRLOOP
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      DOUBLE PRECISION :: OLMVAL(NUMOLM,NUMTYP),
     &                    NO2VAL(NUMOLM), NO_VAL(NUMOLM)
      DOUBLE PRECISION :: NO2, NO, OLMTEMP, PercentNO2

C     Variable Initializations
      MODNAM = 'OLM_CALC'
      NO2 = 0.0D0
      NO  = 0.0D0
      OLMTEMP = 0.0D0
      PercentNO2 = 0.0D0
C     Initialize NO2VAL(NUMOLM) and NO_VAL(NUMOLM) arrays      
      NO2VAL = 0.0D0
      NO_VAL = 0.0D0
C     Initialize OLMVAL(NUMOLM,NUMTYP) array
      OLMVAL = 0.0D0

C     Begin Receptor LOOP
      RECEPTOR_LOOP: DO IREC = 1, NUMREC
C        Reinitialize the NO2VAL, NO_VAL, and OLMVAL arrays for this rec      
         NO2VAL = 0.0D0
         NO_VAL = 0.0D0
         OLMVAL = 0.0D0
         SOURCE_LOOP: DO ISRC = 1, NUMSRC
            IF (.NOT. L_OLMGRP(ISRC)) THEN
C              Source is not in an OLMGROUP; apply NO2_RATIO to CONC (ITYP=1)
               NO2 = ANO2_RATIO(ISRC) * CHI(IREC,ISRC,1)
               NO  = (1.0D0-ANO2_RATIO(ISRC))*CHI(IREC,ISRC,1)*
     &                                         (30.0D0/46.0D0)

C              Determine if O3 limited. If not, then no conversion needed.
               IF ((O3CONC/48.0D0) .LT. (NO/30.0D0)) THEN
                  HRVAL(1) = NO2 + (O3CONC*(46.0D0/48.0D0))
               ELSE
                  HRVAL(1) = CHI(IREC,ISRC,1)
               END IF

C              Calculate an equivalent Percent NO2 for CONC
               PercentNO2 = HRVAL(1)/CHI(IREC,ISRC,1)

C              Limit to equilibrium concentration of NO2 (default set at 90 percent)
               IF (PercentNO2 .GT. NO2Equil) PercentNO2 = NO2Equil

C              Apply equivalent PercentNO2 to other ITYPs (DDEP, WDEP or DEPOS)
               DO ITYP = 2, NUMTYP
                  HRVAL(ITYP) = PercentNO2 * CHI(IREC,ISRC,ITYP)
               END DO

C              Sum HRVAL to AVEVAL and ANNVAL Arrays  ---   CALL SUMVAL
               IF (EVONLY) THEN
                  CALL EV_SUMVAL
               ELSE
                  CALL SUMVAL
               END IF
               IF (EVAL(ISRC)) THEN
C                 Check ARC centerline values for EVALFILE
C                 output                              ---   CALL EVALCK
                  CALL EVALCK
               END IF
               CYCLE SOURCE_LOOP
            ELSE
               DO IOLM = 1, NUMOLM
                  IF (IGRP_OLM(ISRC,IOLM) .EQ. 1) THEN
C                    Calculate NO2 and NO CONV Values for OLMGROUP
                     NO2VAL(IOLM) = NO2VAL(IOLM) +
     &                      ANO2_RATIO(ISRC)* CHI(IREC,ISRC,1)
                     NO_VAL(IOLM) = NO_VAL(IOLM) +
     &                  (1.0D0-ANO2_RATIO(ISRC))*CHI(IREC,ISRC,1)*
     &                                            (30.0D0/46.0D0)

                     DO ITYP = 1, NUMTYP
C                       Calculate Hourly Values (Full Conversion) for OLMGROUP
                        OLMVAL(IOLM,ITYP) = OLMVAL(IOLM,ITYP) +
     &                                      CHI(IREC,ISRC,ITYP)
                     END DO
C                    Exit the OLMGROUP Loop
                     EXIT
                  END IF
               END DO
            END IF
         END DO SOURCE_LOOP

         DO IOLM = 1, NUMOLM

C           Determine if combined plume is O3 limited, and assign
C           PercentNO2 to OLMGroup for CONC (ITYP=1)
            IF ((O3CONC/48.0D0) .LT. ((NO_VAL(IOLM))/30.0D0)) THEN
               OLMTEMP  = NO2VAL(IOLM)+(O3CONC*(46.0D0/48.0D0))
               PercentNO2 = OLMTEMP/OLMVAL(IOLM,1)
            ELSE
               PercentNO2 = 1.0D0
            END IF

C           Limit to equilibrium concentration of NO2 (default set at 90 percent)
            IF (PercentNO2 .GT. NO2Equil) PercentNO2 = NO2Equil

C           Apply PercentNO2 of OLMGroup to Individual Sources
            DO ISRC = 1, NUMSRC
               IF (IGRP_OLM(ISRC,IOLM) .EQ. 1) THEN
                  DO ITYP = 1, NUMTYP
                     HRVAL(ITYP) = PercentNO2 * CHI(IREC,ISRC,ITYP)
                  END DO
                     
C                 Sum HRVAL to AVEVAL and ANNVAL Arrays  ---   CALL SUMVAL
                  IF (EVONLY) THEN
                     CALL EV_SUMVAL
                  ELSE
                     CALL SUMVAL
                  END IF
                  IF (EVAL(ISRC)) THEN
C                    Check ARC centerline values for EVALFILE
C                    output                              ---   CALL EVALCK
                     CALL EVALCK
                  END IF
               END IF
            END DO

         END DO

C        Initialize __VAL arrays (1:NUMTYP)
         HRVAL   = 0.0D0
         AERVAL  = 0.0D0

      END DO RECEPTOR_LOOP
C     End Receptor LOOP

      RETURN
      END

      SUBROUTINE PVMRM_CALC(SRCS2USE)
C***********************************************************************
C             PVMRM_CALC Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Processes Hourly Results for PVMRM Option
C
C        PROGRAMMER: Roger W. Brode, MACTEC, Inc. (f/k/a PES, Inc.)
C
C        DATE:    May 12, 2004
C
C        MODIFIED:   Include calls to SETSRC for all sources types 
C                    to correct initialization problems with PVMRM.
C                    Also include call to subroutine EMFACT to apply 
C                    emission factors, if appropriate, in order to 
C                    use the EMISFACT-adjusted emission rates in 
C                    the calculation of the moles of NOx.
C                    Also assigned file unit 9 to variable PVMDBG 
C                    for the PVMRM debug file, and adjusted the 
C                    PVMRM debug output to report total PercentNO2, 
C                    including in-stack contribution.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/28/2011
C
C                    Retrieve WDSIN and WDCOS for dominant source from 
C                    new arrays that save values by source. Corrects 
C                    potential problem for PVMRM applications with 
C                    multi-level wind inputs.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 01/26/2007
C
C                    Removed code related to EVENT processing and
C                    EVALCART options that are not supported for
C                    PSDCREDIT option.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 01/24/2007
C
C        INPUTS:  SRCS2USE - 'ALLSRCS' = all three PSDGROUPs
C                            'NAAQSRC' = existing baseline and
C                                        increment consuming PSDGROUPs
C                            'ALLBASE' = existing and retired baseline
C                                        PSDGROUPs
C
C        OUTPUTS:
C
C
C        CALLED FROM:   HRLOOP
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER SRCS2USE*7
      CHARACTER MODNAM*12
      
      DOUBLE PRECISION, PARAMETER :: EPS = 0.01D0
      DOUBLE PRECISION :: MAXCHI(NUMREC,NUMTYP), MAXCONC 
      DOUBLE PRECISION :: CWDELT, DWDELT
      DOUBLE PRECISION :: BVERT, BVERT3, BHORIZ, VOLDOM, VOLSUM,
     &                    XDOM, YDOM, DISTDOM, UDOM, FDOM
      DOUBLE PRECISION :: domO3moles, sumO3moles, domNOxmoles, 
     &                    sumNOxmoles, domConverted, sumConverted, 
     &                    PercentNO2
      DOUBLE PRECISION :: AVE_NO2RATIO
      DOUBLE PRECISION :: ZCORR, PCORR, TCORR, PTZ, TAP
      DOUBLE PRECISION :: XDEP, WIDTH, LENGTH, XMAXR
      DOUBLE PRECISION :: QPTOT
      INTEGER :: DOMIDX(NUMREC,NUMTYP), IDOM, NDXBLZ

C     Variable Initializations
      MODNAM = 'PVMRM_CALC'


C     Determine maximum values and corresponding index by receptor
      IF (TRIM(SRCS2USE) .EQ. 'ALLSRCS') THEN
C        Use the global array operators MAXVAL and MAXLOC
         MAXCHI = MAXVAL(CHI,DIM=2)
         DOMIDX = MAXLOC(CHI,DIM=2)
      ELSE
C        PSDCREDIT run; use the routine AERMAX to limit which sources 
C        to use in determining the dominant and major contributing sources
         CALL AERMAX(SRCS2USE,MAXCHI,DOMIDX)

      END IF

C     Begin Receptor LOOP
      RECEPTOR_LOOP: DO IREC = 1, NUMREC

C        Get max concentration and index for dominant source for 
C        this receptor
         MAXCONC = MAXCHI(IREC,1)
         IF (MAXCONC .EQ. 0.0D0) THEN
C           No impacts, cycle to next receptor
            CYCLE
         END IF

C        Set index for dominant source, IDOM
         IDOM = DOMIDX(IREC,1)
         UDOM = UEFFS(IREC,IDOM)
         FDOM = FOPTS(IREC,IDOM)

C        Assign wind direction based on dominant source
         WDSIN = AWDSIN(IDOM)
         WDCOS = AWDCOS(IDOM)

C        Determine extent of major contributing sources
         HMNT  =  1.0D10
         HMXT  =  0.0D0
         HMNH  =  1.0D10
         HMXH  =  0.0D0
         HMNT3 =  1.0D10
         HMXT3 =  0.0D0
         HMNH3 =  1.0D10
         HMXH3 =  0.0D0
         CWMIN =  1.0D20
         CWMAX = -1.0D20
         DWMIN =  1.0D20
         DWMAX = -1.0D20

         NUMCONT = 0
C ---    Loop through sources to obtain major contributing sources
C        Consideration given as to whether this is an emission credit run
C         'ALLSRCS' = full PVMRM run
C         'NAAQSRC' = increment-consuming and non-retired baseline
C         'ALLBASE' = non-retired baseline and retired baseline
C
C        MAJOR_CONT subroutine also defines the lateral (crosswind) 
C        and longitudinal (alongwind) extent of major contributing 
C        sources, through CWMIN/CWMAX and DWMIN/DWMAX
         DO ISRC = 1, NUMSRC

            IF (.NOT. PSDCREDIT) THEN
C              Full PVMRM run
               CALL MAJOR_CONT(MAXCONC)

            ELSE IF (PSDCREDIT .AND. TRIM(SRCS2USE) .EQ. 'NAAQSRC') THEN
C              Emission credit run: Increment Consumption+Nonretired Baseline=NAAQS
               IF (PSDSRCTYP(ISRC) .EQ. 'IC' .OR.
     &             PSDSRCTYP(ISRC) .EQ. 'NB') THEN
                  CALL MAJOR_CONT(MAXCONC)
               ELSE
                  CYCLE
               END IF

            ELSE IF (PSDCREDIT .AND. TRIM(SRCS2USE) .EQ. 'ALLBASE') THEN
C              Emission credit run: Nonretired Baseline and Retired Baseline
               IF (PSDSRCTYP(ISRC) .EQ. 'NB' .OR.
     &             PSDSRCTYP(ISRC) .EQ. 'RB') THEN
                  CALL MAJOR_CONT(MAXCONC)
               ELSE
                  CYCLE
               END IF
            END IF
         END DO

C        Set vertical dimensions of "box" for major cont. sources.
C        Use terrain weighting factor for dominant source (FDOM) to
C        combine heights for horizontal and terrain responding plumes.
         BVERT = FDOM*(HMXH - HMNH) + (1.0D0-FDOM)*(HMXT - HMNT)
         IF (UNSTAB .AND. PPFACT(IDOM) .GT. 0.0D0) THEN
            BVERT3 = FDOM*(HMXH3 - HMNH3) + (1.0D0-FDOM)*(HMXT3 - HMNT3)
         ELSE
            BVERT3 = 0.0D0
         END IF

C        Set horizontal dimensions of "box" for major cont. sources.
         IF (CWMAX .GT. CWMIN) THEN
            BHORIZ = CWMAX - CWMIN
         ELSE
            BHORIZ = 0.0D0
         END IF

C ---    Assign temporary global source index based on dominant source
         ISRC = IDOM
         
C        Calculate Distance from Dominant Source
         IF (SRCTYP(IDOM)(1:4) .EQ. 'AREA') THEN
C ---       Calculations for AREA source types
            CALL SETSRC
            CALL EMFACT(QS)

            IF (EVONLY) THEN
               CALL ARDIST(IEVENT,XDEP,WIDTH,LENGTH,XMAXR)
            ELSE
               CALL ARDIST(IREC,XDEP,WIDTH,LENGTH,XMAXR)
            END IF
C           Assign downwind distance based on most upwind vertex of area source
            XDOM = X
            YDOM = Y
            DISTDOM = XMAXR
C           Calculate volume of dominant plume, passing WIDTH for BHORIZ
            CALL PLUME_VOL(DISTDOM,IDOM,0.0D0,0.0D0,WIDTH,VOLDOM)
         ELSE IF (SRCTYP(IDOM) .EQ. 'OPENPIT') THEN
            CALL SETSRC
C*          Determine the AlongWind Length of the OPENPIT Source  ---   CALL LWIND
            CALL LWIND
   
C*          Calculate the Relative Depth of the OPENPIT Source    ---   CALL PDEPTH
            CALL PDEPTH
   
C*          Calculate the Fractional Size of the
C*          Effective Pit Area (PITFRA)                           ---   CALL PTFRAC
            CALL PTFRAC
   
C*          Determine the Coordinates of the Effective Pit Area
C*          in Wind Direction Coordinate System                   ---   CALL PITEFF
            CALL PITEFF

C*          Calculate the adusted Emission Rate per unit area (QEFF) for the 
C*          Effective Pit Area (PITFRA)                           ---   CALL PITEMI
C*          First assign source emission rate to QPTOT to get adjusted 
C*          emission rate per unit area of effective source
            QPTOT = QS
            CALL PITEMI(QPTOT)
            CALL EMFACT(QEFF)

            IF (EVONLY) THEN
               CALL ARDIST(IEVENT,XDEP,WIDTH,LENGTH,XMAXR)
            ELSE
               CALL ARDIST(IREC,XDEP,WIDTH,LENGTH,XMAXR)
            END IF
C           Assign downwind distance based on most upwind vertex of area source
            XDOM = X
            YDOM = Y
            DISTDOM = XMAXR
C           Calculate volume of dominant plume, passing WIDTH for BHORIZ
            CALL PLUME_VOL(DISTDOM,IDOM,0.0D0,0.0D0,WIDTH,VOLDOM)
         ELSE
C ---       Calculations for POINT or VOLUME sources
            CALL SETSRC
            CALL EMFACT(QS)

            IF (EVONLY) THEN
               CALL XYDIST(IEVENT)
            ELSE
               CALL XYDIST(IREC)
            END IF
            XDOM = X
            YDOM = Y
            DISTDOM = DSQRT(XDOM*XDOM + YDOM*YDOM)
C           Calculate volume of dominant plume
            CALL PLUME_VOL(DISTDOM,IDOM,0.0D0,0.0D0,0.0D0,VOLDOM)
         END IF

         IF (BVERT .GT. 0.0D0 .OR. BHORIZ .GT. 0.0D0) THEN
C           Calculate volume of dominant plus major contributing sources
            CALL PLUME_VOL(DISTDOM,IDOM,BVERT,BVERT3,BHORIZ,VOLSUM)
         ELSE
            VOLSUM = VOLDOM
         END IF

C        Re-Assign wind direction based on dominant source, since values
C        are overwritten in SUB. PLUME_VOL (call to METINI).
         WDSIN = AWDSIN(IDOM)
         WDCOS = AWDCOS(IDOM)

C        Correct plume volume to conditions of standard temperature
C        and pressure (Hanrahan, 1999)
C        0.028966 is the average kg/mole of air

C        First obtain a height to use in the correction
         ZCORR = AZS(IDOM) + HECNTR(IREC,IDOM)

C        Calculate ambient temperature at plume height from pot. temp. profile
         CALL LOCATE(GRIDHT, 1, MXGLVL, ZCORR, NDXBLZ)
         IF (NDXBLZ .GE. 1) THEN
C----       Potential temperature
            CALL GINTRP( GRIDHT(NDXBLZ), GRIDPT(NDXBLZ),
     &                   GRIDHT(NDXBLZ+1), GRIDPT(NDXBLZ+1),
     &                   ZCORR, PTZ )
         ELSE
C           Use GRID value for lowest level
            PTZ = GRIDPT(1)
         END IF
         TAP = PTZ - GOVRCP * ZCORR

         PCORR = DEXP(-G * 0.028966D0 * ZCORR / ( RGAS * TAP))
         TCORR = 273.15D0 / TAP

         VOLDOM = VOLDOM * PCORR * TCORR
         VOLSUM = VOLSUM * PCORR * TCORR

c        get moles of ozone in dominant plume
C        O3 expressed in ug/m3
         domO3moles= VOLdom * O3CONC/(1.0D6 * 48.0D0) ! O3 is ug/m3

c        get moles of ozone in the combined plume
         sumO3moles= VOLsum * O3CONC/(1.0D6 * 48.0D0) ! O3 is ug/m3

c        get moles of NOx in the dominant plume
C        Use molecular weight of NO2 (46) since emission calcs are based on NO2
         IF (SRCTYP(IDOM).EQ.'AREA' .OR.
     &       SRCTYP(IDOM).EQ.'AREAPOLY') THEN
            domNOxmoles = QTK*XINIT*YINIT * DISTDOM/(UDOM*46.0D0)
         ELSE IF (SRCTYP(IDOM).EQ.'AREACIRC') THEN
            domNOxmoles = QTK*PI*RADIUS(IDOM)*RADIUS(IDOM) *
     &                    DISTDOM/(UDOM * 46.0D0)
         ELSE IF (SRCTYP(IDOM).EQ.'OPENPIT') THEN
            domNOxmoles = QTK*XINIT*YINIT * DISTDOM/(UDOM*46.0D0)
         ELSE
            domNOxmoles = QTK * DISTDOM/(UDOM * 46.0D0)
         END IF

c        get moles of NOx in the merged plume
C        Sum emissions for sources within projected width of major
C        contributing sources
C        Assign small delta values for extent of box containing major
C        contributing sources to ensure inclusion of sources on the edge
         CWDELT = MAX( 0.1D0, EPS*(CWMAX - CWMIN) )
         DWDELT = MAX( 0.1D0, EPS*(DWMAX - DWMIN) )
         QSUM = 0.0D0
         SUM_NO2RAT = 0.0D0
         DO ISRC = 1, NUMSRC
         
            IF (.NOT. PSDCREDIT) THEN
C              Full PVMRM run
               CALL MOLES_NOX(CWDELT, DWDELT)

            ELSE IF (PSDCREDIT .AND. TRIM(SRCS2USE) .EQ. 'NAAQSRC') THEN
C              Emission credit run: Increment Consumption+Nonretired Baseline=NAAQS
               IF (PSDSRCTYP(ISRC) .EQ. 'IC' .OR.
     &             PSDSRCTYP(ISRC) .EQ. 'NB') THEN
                  CALL MOLES_NOX(CWDELT, DWDELT)
               ELSE
                  CYCLE
               END IF

            ELSE IF (PSDCREDIT .AND. TRIM(SRCS2USE) .EQ. 'ALLBASE') THEN
C              Emission credit run: Nonretired Baseline and Retired Basline
               IF (PSDSRCTYP(ISRC) .EQ. 'NB' .OR.
     &             PSDSRCTYP(ISRC) .EQ. 'RB') THEN
                  CALL MOLES_NOX(CWDELT, DWDELT)
               ELSE
                  CYCLE
               END IF
            END IF

         END DO

C----    Check for QSUM = 0.0 error (i.e., dominant source not inc. in sum)
C        This condition should never be encountered.
         IF (QSUM .EQ. 0.0D0) THEN
           WRITE(iounit,*)'Error in PVMRM_CALC; QSUM = 0.0. Aborting!!!'
           WRITE(*,*) 'Error in PVMRM_CALC; QSUM = 0.0. Aborting!!!'
           STOP
         END IF

C        Calculate NOx moles for combined plume, and average in-stack ratio
C        Use molecular weight of NO2 (46) since emission calcs are based on NO2
         sumNOxmoles  = QSUM * DISTDOM/(UDOM * 46.0D0)
         AVE_NO2RATIO = SUM_NO2RAT/QSUM

C ---    Calculate NOx conversion ratios for dominant plume and combined plume;
C        excluding in-stack NO2/NOx ratio(s)
         domConverted = domO3moles/domNOxmoles
         sumConverted = sumO3moles/sumNOxmoles

C ---    Find which is more important -- the dominant plume or the combined plume;
C        also include appropriate in-stack NO2/NOx contribution.
C        Select the minimum conversion since the conversion for combined plumes 
C        should not be any larger than conversion for the single dominant plume.
         IF (domConverted .LT. sumConverted) THEN
            PercentNO2 = domConverted + ANO2_RATIO(IDOM)
         ELSE
            PercentNO2 = sumConverted + AVE_NO2RATIO
         END IF

C ---    Limit conversion to equilibrium concentration of NO2 (default set at 90 percent)
         if (PercentNO2 .gt. NO2Equil) PercentNO2 = NO2Equil

C ---    Write data to PVMRM.TXT debugging file
C        Check whether dominant plume or combined plume is controlling
         if (PVMRMDBG .AND. 
     &      ((domconverted .le. sumconverted) .OR.
     &       (DABS((domconverted/sumconverted)-1.0D0).lt.1.0D-10))) then
            write(PVMDBG,9987) 'DOM:', kurdat, irec, srcid(idom), 
     &                     distdom, maxconc, numcont, o3conc, 
     &                     domo3moles, domnoxmoles, bhoriz, bvert, 
     &                     voldom, percentno2
         else if (PVMRMDBG) then
            write(PVMDBG,9987) 'SUM:', kurdat, irec, srcid(idom), 
     &                     distdom, maxconc, numcont, o3conc, 
     &                     sumo3moles, sumnoxmoles, bhoriz, bvert, 
     &                     volsum, percentno2
         end if
9987     format(1x,a4,1x,i8,i5,2x,a8,1x,f11.2,1x,f13.4,1x,i5,1x,f10.3,
     &          2(1x,f12.2),2(1x,f10.2),1x,e12.4,1x,f8.3)

C        Update HRVAL, AVEVAL and ANNVAL Arrays                         ! jop, 9/30/06
         IF (.NOT. PSDCREDIT) THEN
            DO ISRC = 1, NUMSRC
               DO ITYP = 1, NUMTYP
                  HRVAL(ITYP) = CHI(IREC,ISRC,ITYP) * PercentNO2
C                 Sum HRVAL to AVEVAL and ANNVAL Arrays  ---   CALL SUMVAL
               END DO
               IF (EVONLY) THEN
                  CALL EV_SUMVAL
               ELSE
                  CALL SUMVAL
               END IF
               IF (EVAL(ISRC)) THEN
C                 Check ARC centerline values for EVALFILE output ---   CALL EVALCK
                  CALL EVALCK
               END IF
            END DO

         ELSE IF (PSDCREDIT .AND. TRIM(SRCS2USE) .EQ. 'NAAQSRC') THEN
C           IC = increment consuming source
C           NB = nonretired baseline source
C           Use ABVAL to store combined results rather than HRVAL
            DO ISRC = 1, NUMSRC
               DO ITYP = 1, NUMTYP
                  IF( PSDSRCTYP(ISRC) .EQ. 'IC' .OR.
     &                PSDSRCTYP(ISRC) .EQ. 'NB' )THEN
                     ABVAL(IREC,ITYP) = ABVAL(IREC,ITYP) +
     &                                  CHI(IREC,ISRC,ITYP) * PercentNO2
                  END IF
               END DO
            END DO

C           Sum ABVAL to AVEVAL and ANNVAL Arrays  ---   CALL SUMVALPSD
            CALL SUMVALPSD(SRCS2USE)

         ELSE IF (PSDCREDIT .AND. TRIM(SRCS2USE) .EQ. 'ALLBASE') THEN
C           NB = nonretired baseline source
C           RB =    retired baseline source
C           Use BCVAL to store combined results rather than HRVAL
            DO ISRC = 1, NUMSRC
               DO ITYP = 1, NUMTYP
                  IF( PSDSRCTYP(ISRC) .EQ. 'NB' .OR.
     &                PSDSRCTYP(ISRC) .EQ. 'RB' )THEN
                     BCVAL(IREC,ITYP) = BCVAL(IREC,ITYP) +
     &                                  CHI(IREC,ISRC,ITYP) * PercentNO2
                  END IF
               END DO
            END DO

C           Sum ABVAL-BCVAL to AVEVAL and ANNVAL Arrays  ---   CALL SUMVALPSD
            CALL SUMVALPSD(SRCS2USE)

         END IF
         
C        Initialize __VAL arrays (1:NUMTYP)
         HRVAL   = 0.0D0
         AERVAL  = 0.0D0

      END DO RECEPTOR_LOOP
C     End Receptor LOOP

      RETURN
      END

      SUBROUTINE MAJOR_CONT(MAXCONC)
C***********************************************************************
C             MAJOR_CONT Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Identify major contributing souces for PVMRM option,
C                 based on source impacts at the receptor of at least
C                 50% of the dominant source impact.  Also calculates 
C                 the lateral and vertical range of the plumes from the
C                 major contributing sources.
C
C        PROGRAMMER: James Paumier, MACTEC FPI
C
C        DATE:    September 30, 2006
C
C        MODIFIED:   Include calls to SETSRC for all sources types 
C                    to correct initialization problems with PVMRM.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/28/2011
C
C        MODIFICATIONS:
C           Moved from SUBROUTINE PVMRM_CALC to be able to apply
C           emission credit calculations more effectively
C
C        INPUTS:
C
C
C        OUTPUTS:
C
C
C        CALLED FROM:   PVMRM_CALC
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C --- Define threshold for major contributing sources,
C     based on sources which contribute at least 0.5 times
C     the contribution from the dominant source, for each
C     receptor
      DOUBLE PRECISION, PARAMETER :: MAJCONT_THRESH = 0.5D0
      
      DOUBLE PRECISION :: MAXCONC, WIDTH, LENGTH, XMAXR
      DOUBLE PRECISION :: CWDIST, DWDIST
      DOUBLE PRECISION :: XDEP

C     Variable Initializations
      MODNAM = 'MAJOR_CONT'

      IF (CHI(IREC,ISRC,1) .GE. MAJCONT_THRESH*MAXCONC) THEN
C ---    This is a major contributing source
         NUMCONT = NUMCONT + 1
         IF (SRCTYP(ISRC)(1:4) .EQ. 'AREA') THEN
            CALL SETSRC
            IF (EVONLY) THEN
               CALL ARDIST(IEVENT,XDEP,WIDTH,LENGTH,XMAXR)
            ELSE
               CALL ARDIST(IREC,XDEP,WIDTH,LENGTH,XMAXR)
            END IF
C           Store max and min values of crosswind and downwind distances
            CWMAX = MAX( CWMAX, Y + 0.5D0*WIDTH )
            CWMIN = MIN( CWMIN, Y - 0.5D0*WIDTH )
            DWMAX = MAX( DWMAX, X + 0.5D0*LENGTH )
            DWMIN = MIN( DWMIN, X - 0.5D0*LENGTH )
         ELSE IF (SRCTYP(ISRC) .EQ. 'OPENPIT') THEN
            CALL SETSRC
C*          Determine the AlongWind Length of the OPENPIT Source  ---   CALL LWIND
            CALL LWIND
   
C*          Calculate the Relative Depth of the OPENPIT Source    ---   CALL PDEPTH
            CALL PDEPTH
   
C*          Calculate the Fractional Size of the
C*          Effective Pit Area (PITFRA)                           ---   CALL PTFRAC
            CALL PTFRAC
   
C*          Determine the Coordinates of the Effective Pit Area
C*          in Wind Direction Coordinate System                   ---   CALL PITEFF
            CALL PITEFF
   
            IF (EVONLY) THEN
               CALL ARDIST(IEVENT,XDEP,WIDTH,LENGTH,XMAXR)
            ELSE
               CALL ARDIST(IREC,XDEP,WIDTH,LENGTH,XMAXR)
            END IF
C           Store max and min values of crosswind and downwind distances
            CWMAX = MAX( CWMAX, Y + 0.5D0*WIDTH )
            CWMIN = MIN( CWMIN, Y - 0.5D0*WIDTH )
            DWMAX = MAX( DWMAX, X + 0.5D0*LENGTH )
            DWMIN = MIN( DWMIN, X - 0.5D0*LENGTH )
         ELSE
            CALL SETSRC
            IF (EVONLY) THEN
               CALL XYDIST(IEVENT)
            ELSE
               CALL XYDIST(IREC)
            END IF
C           Store max and min values of crosswind and downwind distances
            CWMAX = MAX( CWMAX, Y )
            CWMIN = MIN( CWMIN, Y )
            DWMAX = MAX( DWMAX, X )
            DWMIN = MIN( DWMIN, X )
         END IF
         
C        Assign receptor height above stack base for dominant source
         IF (L_FLATSRC(ISRC)) THEN
            ZRT = ZFLAG
         ELSE
            ZRT = ZELEV - AZS(ISRC) + ZFLAG
         END IF
C        Check plume height ranges for horizontal and terrain responding
         IF (HECNTR(IREC,ISRC) .LT. HMNT) HMNT = HECNTR(IREC,ISRC)
         IF (HECNTR(IREC,ISRC) .GT. HMXT) HMXT = HECNTR(IREC,ISRC)
         IF (HECNTR(IREC,ISRC)-ZRT.LT.HMNH)
     &                                HMNH = HECNTR(IREC,ISRC)-ZRT
         IF (HECNTR(IREC,ISRC)-ZRT.GT.HMXH)
     &                                HMXH = HECNTR(IREC,ISRC)-ZRT
         IF (PPFACT(ISRC) .GT. 0.0D0) THEN
            IF(HECNTR3(IREC,ISRC).LT.HMNT3)
     &                                  HMNT3 = HECNTR3(IREC,ISRC)
            IF(HECNTR3(IREC,ISRC).GT.HMXT3)
     &                                  HMXT3 = HECNTR3(IREC,ISRC)
            IF(HECNTR3(IREC,ISRC)-ZRT.LT.HMNH3)
     &                            HMNH3 = HECNTR3(IREC,ISRC) - ZRT
            IF(HECNTR3(IREC,ISRC)-ZRT.GT.HMXH3)
     &                            HMXH3 = HECNTR3(IREC,ISRC) - ZRT
         END IF
      END IF

      RETURN
      END

      SUBROUTINE MOLES_NOX(CWDELT, DWDELT)
C***********************************************************************
C             MOLES_NOX Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Calculates the moles of NOx in the combined plume (?)
C
C        PROGRAMMER: James Paumier, MACTEC FPI
C
C        DATE:    September 30, 2006
C
C        MODIFIED:   Include calls to SETSRC for all sources types 
C                    to correct initialization problems with PVMRM.
C                    Also include call subroutine EMFACT to apply 
C                    emission factors, if appropriate, in order to 
C                    use the EMISFACT-adjusted emission rates in 
C                    the calculations of the moles of NOx.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/28/2011
C
C        MODIFICATIONS:
C           Moved from SUBROUTINE PVMRM_CALC to be able to apply
C           emission credit calculations more effectively
C
C        INPUTS:
C
C
C        OUTPUTS:
C            QSUM
C            SUM_NO2RAT
C
C
C        CALLED FROM:   PVMRM_CALC
C***********************************************************************
C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      DOUBLE PRECISION :: WIDTH, LENGTH, XMAXR, CWDELT, DWDELT
      DOUBLE PRECISION :: CWDIST, DWDIST
      DOUBLE PRECISION :: QAREA, XDEP
      DOUBLE PRECISION :: QPTOT

C     Variable Initializations
      MODNAM = 'MOLES_NOX'

      IF (SRCTYP(ISRC)(1:4) .EQ. 'AREA') THEN
         CALL SETSRC
         CALL EMFACT(QS)
         IF (EVONLY) THEN
            CALL ARDIST(IEVENT,XDEP,WIDTH,LENGTH,XMAXR)
         ELSE
            CALL ARDIST(IREC,XDEP,WIDTH,LENGTH,XMAXR)
         END IF
C        Assign CWDIST and DWDIST to Y and X based on distance from
C        center of area source to receptor.  If center of area source
C        is within "box" of major contributing sources, then include
C        it's emissions in calculation of NOx moles.
         CWDIST = Y
         DWDIST = X
      ELSE IF (SRCTYP(ISRC) .EQ. 'OPENPIT') THEN
         CALL SETSRC
C*       Determine the AlongWind Length of the OPENPIT Source  ---   CALL LWIND
         CALL LWIND
   
C*       Calculate the Relative Depth of the OPENPIT Source    ---   CALL PDEPTH
         CALL PDEPTH
   
C*       Calculate the Fractional Size of the
C*       Effective Pit Area (PITFRA)                           ---   CALL PTFRAC
         CALL PTFRAC
   
C*       Determine the Coordinates of the Effective Pit Area
C*       in Wind Direction Coordinate System                   ---   CALL PITEFF
         CALL PITEFF
   
C*       Calculate the adusted Emission Rate per unit area (QEFF) for the 
C*       Effective Pit Area (PITFRA)                           ---   CALL PITEMI
C*       First assign source emission rate to QPTOT to get adjusted 
C*       emission rate per unit area of effective source
         QPTOT = QS
         CALL PITEMI(QPTOT)
         CALL EMFACT(QEFF)

         IF (EVONLY) THEN
            CALL ARDIST(IEVENT,XDEP,WIDTH,LENGTH,XMAXR)
         ELSE
            CALL ARDIST(IREC,XDEP,WIDTH,LENGTH,XMAXR)
         END IF
C        Assign CWDIST and DWDIST to Y and X based on distance from
C        center of area source to receptor.  If center of area source
C        is within "box" of major contributing sources, then include
C        it's emissions in calculation of NOx moles.
         CWDIST = Y
         DWDIST = X
      ELSE
         CALL SETSRC
         CALL EMFACT(QS)
         IF (EVONLY) THEN
            CALL XYDIST(IEVENT)
         ELSE
            CALL XYDIST(IREC)
         END IF
         CWDIST = Y
         DWDIST = X
      END IF
      
C     Check for crosswind disance between MIN and MAX of projected width
      IF (CWDIST .GE. CWMIN-CWDELT .AND. CWDIST .LE. CWMAX+CWDELT) THEN
         IF(DWDIST .GE. DWMIN-DWDELT .AND. DWDIST .LE. DWMAX+DWDELT)THEN
            IF (SRCTYP(ISRC).EQ.'AREA' .OR.
     &          SRCTYP(ISRC).EQ.'AREAPOLY' .OR.
     &          SRCTYP(ISRC).EQ.'OPENPIT') THEN
C ---          Note that XINIT and YINIT are "effective" dimensions for 
C              AREAPOLY sources that are used to define the area of the
C              source.
               QAREA = QTK*XINIT*YINIT
               QSUM = QSUM + QAREA
               SUM_NO2RAT = SUM_NO2RAT + ANO2_RATIO(ISRC)*QAREA
            ELSE IF (SRCTYP(ISRC) .EQ. 'AREACIRC') THEN
               QAREA = QTK*PI*RADIUS(ISRC)*RADIUS(ISRC)
               QSUM = QSUM + QAREA
               SUM_NO2RAT = SUM_NO2RAT + ANO2_RATIO(ISRC)*QAREA
            ELSE
               QSUM = QSUM + QTK
               SUM_NO2RAT = SUM_NO2RAT + ANO2_RATIO(ISRC)*QTK
            END IF
         END IF
      END IF

      RETURN
      END

      SUBROUTINE PLUME_VOL(XARG,ISDX,BVARG,BVARG3,BHARG,VOLOUT)
C***********************************************************************
C             PLUME_VOL Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Calculates plume volume for PVMRM option
C
C        PROGRAMMER: Roger W. Brode, PES, Inc.
C
C        DATE:    May 6, 2002
C
C        MODIFICATIONS:
C
C               Modified XTABLE array of distances to use a more 
C               logical progression of distance intervals and to
C               reduce the number of distances used.
C               R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/28/2011
C
C               Added call to PENFCT to calculate plume penetration
C               factor.
C               R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
C
C               Remove calls to VDP and SCAVRAT.
C               R. Brode, MACTEC (f/k/a PES), Inc. - 08/02/05
C
C        INPUTS:
C
C
C        OUTPUTS:
C
C
C        CALLED FROM:   PVMRM_CALC
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      INTEGER, PARAMETER :: NTAB = 59
      DOUBLE PRECISION, PARAMETER :: NSUBZ = 4.00D0, SRMIN = 5.0D0
      DOUBLE PRECISION ::  XARG, BVARG, BVARG3, BHARG, VERT, VOLOUT, 
     &                     SUM, SUM3
      DOUBLE PRECISION ::  XTABLE(NTAB), SR(NTAB), SR3(NTAB), SIGR, 
     &                     SIGR3
      DOUBLE PRECISION ::  XTAB, DELTAX, BVTMP, SIGRZTMP
      INTEGER ::  I, ISDX
      INTEGER :: KITER, NDXZPL
      DOUBLE PRECISION :: HSPRIM, ZPLM, DHFOLD, SVPM, UPM, TGPM, PTPM, 
     &                    PTP
      DOUBLE PRECISION :: VSEQ

C --- Variable Initializations:
C     Distance table for plume volume calculation, XTABLE
      DATA XTABLE/
     &   10.D0,20.D0,30.D0,40.D0,50.D0,60.D0,70.D0,80.D0,90.D0,100.D0,
     &   120.D0,140.D0,160.D0,180.D0,200.D0,
     &   250.D0,300.D0,350.D0,400.D0,450.D0,500.D0,
     &   600.D0,700.D0,800.D0,900.D0,1000.D0,
     &   1200.D0,1400.D0,1600.D0,1800.D0,2000.D0,
     &   2500.D0,3000.D0,3500.D0,4000.D0,4500.D0,5000.D0,
     &   6000.D0,7000.D0,8000.D0,9000.D0,10000.D0,
     &   12000.D0,14000.D0,16000.D0,18000.D0,20000.D0,
     &   25000.D0,30000.D0,35000.D0,40000.D0,45000.D0,50000.D0,
     &   60000.D0,70000.D0,80000.D0,90000.D0,100000.D0,
     &   500000.D0/
     
      DATA SR/NTAB*0.0D0/, SR3/NTAB*0.0D0/

      MODNAM = 'PLUME_VOL'

C     Assign source index to global variable
      ISRC = ISDX
      SUM  = 0.0D0
      SUM3 = 0.0D0

C     Set Mixing Height and Profiles for Urban Option if Needed
      IF (URBSRC(ISRC) .EQ. 'Y') THEN
C        Find Urban Area Index for This Source
         DO I = 1, NUMURB
            IF (IURBGRP(ISRC,I) .EQ. 1) THEN
               IURB = I
               EXIT
            END IF
         END DO
         IF (STABLE .OR. L_MorningTrans(IURB)) THEN
            URBSTAB = .TRUE.
            ZI = MAX( ZIURB(IURB), ZIMECH )
            GRIDSV = GRDSVU(1:MXGLVL,IURB)
            GRIDSW = GRDSWU(1:MXGLVL,IURB)
            GRIDTG = GRDTGU(1:MXGLVL,IURB)
            GRIDPT = GRDPTU(1:MXGLVL,IURB)
            OBULEN = DABS( URBOBULEN(IURB) )
            USTAR  = URBUSTR(IURB)
         ELSE
            URBSTAB = .FALSE.
            ZI = ZIRUR
            GRIDSV = GRDSVR
            GRIDSW = GRDSWR
            GRIDTG = GRDTGR
            GRIDPT = GRDPTR
            OBULEN = RUROBULEN
            USTAR  = RURUSTR
         END IF
      ELSE IF (URBAN .AND. URBSRC(ISRC) .EQ. 'N') THEN
         URBSTAB = .FALSE.
         ZI = ZIRUR
         GRIDSV = GRDSVR
         GRIDSW = GRDSWR
         GRIDTG = GRDTGR
         GRIDPT = GRDPTR
         OBULEN = RUROBULEN
         USTAR  = RURUSTR
      ELSE
         URBSTAB = .FALSE.
      END IF

C     Set the Source Variables for This Source           ---   CALL SETSRC
      CALL SETSRC

C     Calculate the initial meteorological variables     ---   CALL METINI
      CALL METINI

      IF (SRCTYP(ISRC) .EQ. 'VOLUME' .OR.
     &    SRCTYP(ISRC)(1:4) .EQ. 'AREA' .OR.
     &    SRCTYP(ISRC) .EQ. 'OPENPIT') THEN
         FB  = 0.0D0
         FM  = 0.0D0
         PPF = 0.0D0
         HSP = HS
         DHP  = 0.0D0
         DHP1 = 0.0D0
         DHP2 = 0.0D0
         DHP3 = 0.0D0
         DHCRIT = 0.0D0
         XFINAL = 0.0D0
         XMIXED = ZI * UAVG / SWAVG
         IF(XMIXED .LT. XFINAL) XMIXED = XFINAL
         ZMIDMX = 0.5D0 * ZI

      ELSE IF (SRCTYP(ISRC)(1:5) .EQ. 'POINT') THEN
C        Calculate Buoyancy and Momentum Fluxes             ---   CALL FLUXES
         CALL FLUXES(VSEQ)

C        Set Wake and Building Type Switches                ---   CALL WAKFLG
C ---    NOTE:  WAKFLG sets building dimensions based on wind
C        direction at stack top.
C ---    WAKE is set to false for purposes of calculating plume volume,
C        since minimum volume is intended 
         WAKE = .FALSE.

C        Define temporary values of CENTER and SURFAC based on HS
         CENTER = HS
         IF( CENTER .LT. 0.1D0*ZI )THEN
            SURFAC = .TRUE.
         ELSE
            SURFAC = .FALSE.
         END IF

C        Check for stack-tip downwash option and adjust if necessary
         IF( NOSTD )THEN
C           No stack-tip downwash, no adjustments necessary
            HSP = HS
         ELSE IF (SRCTYP(ISRC) .EQ. 'POINTCAP') THEN
C           Apply stack-tip downwash for capped stacks with VS = 0.001m/s
            HSP = HSPRIM ( US, VSEQ, HS, DS )
         ELSE IF (SRCTYP(ISRC) .EQ. 'POINTHOR') THEN
C           Do not apply stack-tip downwash for horizontal releases
            HSP = HS
         ELSE
C           Make adjustments for stack-tip downwash
            HSP = HSPRIM ( US, VS, HS, DS )
         END IF

C        Calculate Distance to Final Rise                   ---   CALL DISTF
         CALL DISTF

C        Calculate the plume penetration factor             ---   CALL PENFCT
         CALL PENFCT

         IF (STABLE.OR.(UNSTAB.AND.(HS.GE.ZI))) THEN
C           Use iterative approach to stable plume rise calculations
            KITER = 0
50          ZPLM = HSP + 0.5D0 * DHFAER
            DHFOLD = DHFAER

C----       Locate index below ZPLM

            CALL LOCATE(GRIDHT, 1, MXGLVL, ZPLM, NDXZPL)

C----       Get Wind speed at ZPLM; replace UP.  Also, replace TGP,
C           vertical potential temperature gradient, if stable.

            CALL GINTRP( GRIDHT(NDXZPL), GRIDSV(NDXZPL),
     &           GRIDHT(NDXZPL+1), GRIDSV(NDXZPL+1), ZPLM, SVPM )
            CALL GINTRP( GRIDHT(NDXZPL), GRIDWS(NDXZPL),
     &           GRIDHT(NDXZPL+1), GRIDWS(NDXZPL+1), ZPLM, UPM )
            SVPM = MAX( SVPM, SVMIN, 0.05D0*UPM )
            UPM  = DSQRT( UPM*UPM + 2.0D0*SVPM*SVPM )
CRWB        Use average of stack top and midpoint wind speeds.
            UP = 0.5D0 * (US + UPM)

            CALL GINTRP( GRIDHT(NDXZPL), GRIDTG(NDXZPL),
     &           GRIDHT(NDXZPL+1), GRIDTG(NDXZPL+1), ZPLM, TGPM )
            CALL GINTRP( GRIDHT(NDXZPL), GRIDPT(NDXZPL),
     &           GRIDHT(NDXZPL+1), GRIDPT(NDXZPL+1), ZPLM, PTPM )
CRWB        Use average of stack top and midpoint temperature gradients.
            TGP = 0.5D0 * (TGS + TGPM)
            PTP = 0.5D0 * (PTS + PTPM)
            BVF = DSQRT( G * TGP / PTP )
            IF(BVF .LT. 1.0D-10) BVF = 1.0D-10
            BVPRIM  = 0.7D0 * BVF

            CALL DISTF

            KITER = KITER + 1

C           Check for convergence            
            IF(DABS((DHFOLD - DHFAER)/DHFAER) .LT. 0.01D0) GO TO 60
            
            IF(KITER .GE. 5) THEN
               DHFAER = 0.5D0 * (DHFAER + DHFOLD)
               GO TO 60
            ELSE
               GO TO 50
            END IF

60          CONTINUE

CRWB        After completing iteration, reset UP and TGP to stack top
CRWB        values for subsequent distance-dependent plume rise calcs.
            UP = US
            TGP = TGS
            PTP = PTS
            BVF = DSQRT( G * TGP / PTP )
            IF(BVF .LT. 1.0D-10) BVF = 1.0D-10
            BVPRIM  = 0.7D0 * BVF
         END IF

C        Initialize PRM_FSTREC Logical Switch for First Receptor of Loop;
         PRM_FSTREC = .TRUE.

         ZMIDMX = 0.5D0 * ZI

CRJP
CRJP     Calculate distance to uniformly mixed plume within the
CRJP     boundary layer (XMIXED) after Turner's Workbook (1970), page 7:
CRJP     distance is approximately (Zi * UAVG)/SWAVG, where UAVG
CRJP     and SWAVG are wind speed and sigma-w averaged over the depth
CRJP     between the ground and Zi (or the plume height, if higher in
CRJP     stable conditions); this height is denoted as 2 * ZMIDMX.
CRJP
CRJP     First, get refined estimate of final rise and distance to final
CRJP     rise if downwash conditions prevail.
CRJP
         XFINAL = XMAX
         DHCRIT = DHFAER
         XMIXED = ZI * UAVG / SWAVG
         IF (UNSTAB .AND. HS.LT.ZI) THEN
C           Check for XMIXED smaller than 1.25*XFINAL
            IF (XMIXED .LT. 1.25D0*XFINAL) THEN
               XFINAL = 0.8D0 * XMIXED
               CALL CBLPRD (XFINAL)
               DHCRIT = DHP1
            END IF
         END IF

      END IF

C     First build table of relative dispersion coefficients for
C     dominant source (source index = ISDX)
      DO I = 1, NTAB

         XTAB = XTABLE(I)
         IF (I .GT. 1) THEN
            DELTAX = XTABLE(I) - XTABLE(I-1)
         ELSE
            DELTAX = XTABLE(1)
         END IF

         IF (XARG .GT. XTAB) THEN

C           Define plume centroid height (CENTER) for use in
C           inhomogeneity calculations
            CALL CENTROID ( XTAB )

            IF (SRCTYP(ISRC)(1:5) .EQ. 'POINT') THEN
C              Calculate the plume rise                  ---   CALL DELTAH
               CALL DELTAH ( XTAB )
            END IF

C           If the atmosphere is unstable and the stack
C           top is below the mixing height, calculate
C           the CBL PDF coefficients                     ---   CALL PDF
            IF( UNSTAB  .AND.  (HS .LT. ZI) ) THEN
               CALL PDF
            END IF

C           Determine Effective Plume Height             ---   CALL HEFF
            CALL HEFF ( XTAB )

C           Compute effective parameters using an
C           average through plume rise layer
            CALL IBLVAL ( XTAB )

C           Call PDF & HEFF again for final CBL plume heights
            IF (UNSTAB .AND. (HS.LT.ZI) ) THEN
               CALL PDF
               CALL HEFF ( XTAB )
            END IF

            IF (SRCTYP(ISRC)(1:5) .EQ. 'POINT') THEN
C              Call BID to get buoyancy-induced dispersion terms
               CALL BID
            ELSE
               SZB  = 0.0D0
               SZBD = 0.0D0
               SZB3 = 0.0D0
            END IF

C           Determine Relative Dispersion Parameters     ---   CALL RELDISP
            CALL RELDISP ( XTAB, SR(I), SR3(I) )

C           Assign virtual sigma value, SIGR, for this distance range
            IF (I .GT. 1) THEN
               SIGR = MAX( SRMIN, 0.5D0 * (SR(I) + SR(I-1)) )
            ELSE
               SIGR = MAX( SRMIN, 0.5D0 * SR(I) )
            END IF

C           Calculate vertical dimension (VERT) taking into account ZI limit
            VERT = BVARG + 2.0D0*NSUBZ*SIGR
            IF (UNSTAB .AND. VERT .GT. ZI) THEN
               VERT  = ZI
               BVTMP = MAX( 0.0D0, ZI-2.0D0*NSUBZ*SIGR )
               IF (2.0D0*NSUBZ*SIGR .GT. ZI) THEN
                  SIGRZTMP = ZI/(2.0D0*NSUBZ)
               ELSE
                  SIGRZTMP = SIGR
               END IF
            ELSE
               BVTMP = BVARG
               SIGRZTMP = SIGR
            END IF


C           Plume volume calculation based on rectangle with rounded corners
C           First component is for major contributing plume only
            SUM = SUM + (PI*(NSUBZ*SIGR)*(NSUBZ*SIGRZTMP) +
     &                   VERT*BHARG +
     &                   2.0D0*NSUBZ*SIGR*BVTMP) * DELTAX

            IF (UNSTAB .AND. PPFACT(ISRC) .GT. 0.0D0) THEN

               IF (I .GT. 1) THEN
                  SIGR3 = MAX( SRMIN, 0.5D0 * (SR3(I) + SR3(I-1)) )
               ELSE
                  SIGR3 = MAX( SRMIN, 0.5D0 * SR3(I) )
               END IF

               VERT = BVARG3 + 2.0D0*NSUBZ*SIGR3

               SUM3 = SUM3 + (PI*(NSUBZ*SIGR3)*(NSUBZ*SIGR3) +
     &                        VERT*BHARG +
     &                        2.0D0*NSUBZ*SIGR3*BVARG3) * DELTAX

            END IF

         ELSE
C ---       XARG is .LE. XTAB

C           Define plume centroid height (CENTER) for use in
C           inhomogeneity calculations
            CALL CENTROID ( XARG )

            IF (SRCTYP(ISRC)(1:5) .EQ. 'POINT') THEN
C              Calculate the plume rise                  ---   CALL DELTAH
               CALL DELTAH ( XARG )
            END IF

C           If the atmosphere is unstable and the stack
C           top is below the mixing height, calculate
C           the CBL PDF coefficients                     ---   CALL PDF
            IF( UNSTAB  .AND.  (HS .LT. ZI) ) THEN
               CALL PDF
            END IF

C           Determine Effective Plume Height             ---   CALL HEFF
            CALL HEFF ( XARG )

C           Compute effective parameters using an
C           average through plume rise layer
            CALL IBLVAL ( XARG )

C           Call PDF & HEFF again for final CBL plume heights
            IF (UNSTAB .AND. (HS.LT.ZI) ) THEN
               CALL PDF
               CALL HEFF ( XARG )
            END IF

            IF (SRCTYP(ISRC)(1:5) .EQ. 'POINT') THEN
C              Call BID to get buoyancy-induced dispersion terms
               CALL BID
            ELSE
               SZB  = 0.0D0
               SZBD = 0.0D0
               SZB3 = 0.0D0
            END IF

C           Determine Relative Dispersion Parameters     ---   CALL RELDISP
            CALL RELDISP ( XARG, SR(I), SR3(I) )

            IF (I .GT. 1) THEN
               DELTAX = XARG - XTABLE(I-1)
            ELSE
               DELTAX = XARG
            END IF

C           Assign virtual sigma value, SIGR, for this distance range
            IF (I .GT. 1) THEN
               SIGR = MAX( SRMIN, 0.5D0 * (SR(I) + SR(I-1)) )
            ELSE
               SIGR = MAX( SRMIN, 0.5D0 * SR(I) )
            END IF

C           Calculate vertical dimension (VERT) taking into account ZI limit
            VERT = BVARG + 2.0D0*NSUBZ*SIGR
            IF (UNSTAB .AND. VERT .GT. ZI) THEN
               VERT  = ZI
               BVTMP = MAX( 0.0D0, ZI-2.0D0*NSUBZ*SIGR )
               IF (2.0D0*NSUBZ*SIGR .GT. ZI) THEN
                  SIGRZTMP = ZI/(2.0D0*NSUBZ)
               ELSE
                  SIGRZTMP = SIGR
               END IF
            ELSE
               BVTMP = BVARG
               SIGRZTMP = SIGR
            END IF

C           Plume volume calculation based on rectangle with rounded corners
C           First component is for major contributing plume only
            SUM = SUM + (PI*(NSUBZ*SIGR)*(NSUBZ*SIGRZTMP) +
     &                   VERT*BHARG +
     &                   2.0D0*NSUBZ*SIGR*BVTMP) * DELTAX

            IF (UNSTAB .AND. PPFACT(ISRC) .GT. 0.0D0) THEN

               IF (I .GT. 1) THEN
                  SIGR3 = MAX( SRMIN, 0.5D0 * (SR3(I) + SR3(I-1)) )
               ELSE
                  SIGR3 = MAX( SRMIN, 0.5D0 * SR3(I) )
               END IF

               VERT = BVARG3 + 2.0D0*NSUBZ*SIGR3

               SUM3 = SUM3 + (PI*(NSUBZ*SIGR3)*(NSUBZ*SIGR3) +
     &                        VERT*BHARG +
     &                        2.0D0*NSUBZ*SIGR3*BVARG3) * DELTAX

            END IF

            EXIT

         END IF

      END DO

C     Combine volume for "direct" plume volume and penetrated plume volume
      VOLOUT = SUM*(1.0D0-PPFACT(ISRC)) + SUM3*PPFACT(ISRC)

      RETURN
      END

      SUBROUTINE RELDISP(XARG,SROUT,SROUT3)
C***********************************************************************
C             RELDISP Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Calculates relative dispersion coefficients for use
C                 in calculating plume volume for the PVMRM option
C
C        PROGRAMMER: Roger W. Brode, PES, Inc.
C
C        DATE:    May 14, 2002
C
C        MODIFICATIONS:
C
C               Modified treatment of virtual source term, VSIGR,
C               to account for cases when SYINIT or SZINIT may be
C               zero, such as AREA sources.
C               R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/28/2011
C
C        INPUTS:
C
C
C        OUTPUTS:
C
C
C        CALLED FROM:   PLUME_VOL
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      DOUBLE PRECISION, PARAMETER :: A1 = 0.57D0, A2 = 0.62D0*A1, 
     &                               AR1 = 0.46D0
      DOUBLE PRECISION :: XARG, SROUT, SROUT3, SRAMB, TLR, VSIGR

C     Variable Initializations
      MODNAM = 'RELDISP'

C --- Use square root of the product of lateral and vertical initial sigmas
C     to define the effective virtual source initial sigma, VSIGR;
C     however, need to check for one or the other being zero, e.g., for 
C     area source types, szinit may be non-zero but syinit is zero.
      IF (DABS(SZINIT-0.0D0) .GE. 1.0D-8) THEN
         IF (DABS(SYINIT-0.0D0) .GE. 1.0D-8) THEN
            VSIGR = DSQRT(SYINIT*SZINIT)
         ELSE
            VSIGR = SZINIT
         END IF
      ELSE IF (DABS(SZINIT-0.0D0) .LE. 1.0D-8) THEN
         IF (DABS(SYINIT-0.0D0) .GE. 1.0D-8) THEN
            VSIGR = SYINIT
         ELSE
            VSIGR = 0.0D0
         END IF
      ELSE
         VSIGR = 0.0D0
      END IF

      IF( STABLE .OR. (UNSTAB .AND. (HS .GE. ZI)) )THEN
C        The atmosphere is stable or the release is above the CBL mixing ht.
         TLR = AR1 * ZI/SWEFF
         SRAMB = (A1 * DSQRT(EPSEFF) * (XARG/UEFF)**1.5D0)/
     &           (1.0D0 + A2 * XARG/(UEFF*TLR))
         SROUT = (SRAMB**3 + SZB**3 + VSIGR**3)**THIRD

         SROUT3 = SROUT

      ELSEIF( UNSTAB )then
C        The atmosphere is unstable and the release is below the CBL mixing ht.
         TLR = AR1 * ZI/SWEFFD
         SRAMB = (A1 * DSQRT(EPSEFFD) * (XARG/UEFFD)**1.5D0)/
     &           (1.0D0 + A2 * XARG/(UEFFD*TLR))
         SROUT = (SRAMB**3 + SZBD**3 + VSIGR**3)**THIRD

C        Calculate relative dispersion for a penetrated plume, SROUT3
         IF( PPFACT(ISRC) .GT. 0.0D0 )THEN
            TLR = AR1 * ZI/SWEFF3
            SRAMB = (A1 * DSQRT(EPSEFF3) * (XARG/UEFF3)**1.5D0)/
     &              (1.0D0 + A2 * XARG/(UEFF3*TLR))
            SROUT3 = (SRAMB**3 + SZB3**3 + VSIGR**3)**THIRD

         ELSE
            SROUT3 = 0.0D0
         END IF

      END IF

      RETURN
      END

      SUBROUTINE AERMAX(SRCS2USE,MAXARG,DOMARG)
C***********************************************************************
C             AERMAX Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: To populate the MAXCHI and DOMIDX arrays
C
C        PROGRAMMER: J Paumier, MACTEC
C
C        DATE:    September 30, 2006
C
C        INPUTS:
C
C
C        OUTPUTS:
C
C
C        CALLED FROM:   PVMRM_CALC
C***********************************************************************

      USE MAIN1
      IMPLICIT NONE
      
      DOUBLE PRECISION :: MAXARG(NUMREC,NUMTYP)
      INTEGER :: DOMARG(NUMREC,NUMTYP)
      INTEGER :: I, J, K
      CHARACTER SRCS2USE*7
      CHARACTER MODNAM*12

      MODNAM = 'AERMAX'
      
C     Initialize MAXARG(NUMREC,NUMTYP) and DOMARG(NUMREC,NUMYTP) arrays      
      MAXARG = 0.0D0
      DOMARG = 0

      IF (TRIM(SRCS2USE) .EQ. 'NAAQSRC') THEN
C        Emission credit run: Increment Consumption+Nonretired Baseline=NAAQS
         DO I = 1, NUMTYP
            DO J = 1, NUMREC
               DO K = 1, NUMSRC
                  IF (PSDSRCTYP(K) .EQ. 'IC' .OR.
     &                PSDSRCTYP(K) .EQ. 'NB') THEN
                     IF(CHI(J,K,I) .GT. MAXARG(J,I) )THEN
                        MAXARG(J,I) = CHI(J,K,I)
                        DOMARG(J,I) = K
                     END IF
                  END IF
               END DO
            END DO
         END DO

      ELSE IF (TRIM(SRCS2USE) .EQ. 'ALLBASE') THEN
C        Emission credit run: Nonretired Baseline and Retired Baseline
         DO I = 1, NUMTYP
            DO J = 1, NUMREC
               DO K = 1, NUMSRC
                  IF (PSDSRCTYP(K) .EQ. 'NB' .OR.
     &                PSDSRCTYP(K) .EQ. 'RB') THEN
                     IF(CHI(J,K,I) .GT. MAXARG(J,I) )THEN
                        MAXARG(J,I) = CHI(J,K,I)
                        DOMARG(J,I) = K
                     END IF
                  END IF
               END DO
            END DO
         END DO

      END IF

      RETURN
      END
