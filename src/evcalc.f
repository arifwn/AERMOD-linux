      SUBROUTINE EVLOOP
C***********************************************************************
C                 EVLOOP Module of ISC2 Short Term EVENT Model - ISCEV2
C
C        PURPOSE: Controls Main Calculation Loop Through Events
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To remove mixed-mode math in calculation of
C                    IENDHR - 4/19/93
C
C        INPUTS:  Source, Receptor and Setup Options
C
C        OUTPUTS: Update Hourly Results
C
C        CALLED FROM:   MAIN
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: IEVYR
      LOGICAL FOPEN, L_FIRSTCALL

C     Variable Initializations
      MODNAM = 'EVLOOP'
      EOF   = .FALSE.
      FOPEN = .FALSE.
      L_FIRSTCALL = .TRUE.

C     Flush HRVAL, AVEVAL, GRPAVE and GRPVAL    ---   CALL FLUSH
      CALL EV_FLUSH

      DO WHILE (FULLDATE.LT.IEDATE .AND. .NOT.EOF)
C        Retrieve Hourly Meteorology Data for Current Day   ---   CALL MEREAD
         CALL MEREAD

C        Check for Hourly Emissions File
         INQUIRE (UNIT=IHREMI,OPENED=FOPEN)
         IF (FOPEN) THEN
C*          Retrieve Hourly Emissions from File for Current Day---   CALL HQREAD
C*          Set ILINE = 1 if L_FIRSTCALL for determining whether VOLUME and AREA 
C*          source inputs include hourly sigmas
            IF (L_FIRSTCALL) ILINE = 1
            CALL EV_HRQREAD(L_FIRSTCALL)
         END IF

         IF (L_BACKGRND) THEN
C-----      Extract BACKGRND concentrations, if available
            CALL BGREAD
         END IF

         IF (PVMRM .OR. OLM) THEN
C-----      Extract Ozone Data; L_FIRSTCALL used to initialize array
C           of O3 values used to apply minimum O3 for stable hours
            CALL O3READ(L_FIRSTCALL)
         END IF

C ---    Set L_FIRSTCALL to .F.
         L_FIRSTCALL = .FALSE.

C        Write Out Update to the Screen for the PC Version
         WRITE(*,909) JDAY, IYR
 909     FORMAT('+','Now Processing Events For Day No. ',I4,' of ',I4)

         IF (IPROC(JDAY).EQ.1 .AND. .NOT.RUNERR) THEN
C           Begin The Event Loop
            DO IEVENT = 1, NUMEVE

C              Calculate year of event for multiple year data files
               IEVYR = INT(EVDATE(IEVENT)/1000000)
               IF (EVJDAY(IEVENT) .EQ. JDAY .AND.
     &                      IEVYR .EQ. IYEAR) THEN

                  IENDHR = EVDATE(IEVENT) -
     &                 INT(EVDATE(IEVENT)/100)*100
                  ISTAHR = IENDHR - EVAPER(IEVENT) + 1
                 
C                 Begin Hourly LOOP
                  DO IHOUR = ISTAHR, IENDHR
                     KHOUR = IHOUR
C                    Retrieve Hourly Data for Current Event ---   CALL METEXT
                     CALL EV_METEXT
C                    Retrieve Hourly Ozone Value
                     IF (PVMRM .OR. OLM) THEN
                        O3CONC = EV_O3CONC(IHOUR)
                     END IF
C*                   Process Hourly Emissions from File, if needed
                     IF (HOURLY) THEN
C*                      Begin Source Loop
                        DO ISRC = 1, NUMSRC
                          IF (QFLAG(ISRC) .EQ. 'HOURLY') THEN
C*                           Retrieve Source Parameters for This Hour  ---   CALL HRQEXT
                             CALL HRQEXT(ISRC)
                          END IF
                        END DO
C*                      End Source Loop
                     END IF
C*----
                     IF (CLMHR .AND. CLMPRO) THEN
C                       Check for Calm Hr & Processing and
C                       Increment Counters
                        EV_NUMHRS = EV_NUMHRS + 1
                        EV_NUMCLM = EV_NUMCLM + 1
                     ELSE IF (MSGHR .AND. MSGPRO) THEN
C                       Check for Missing Hour & Processing and
C                       Increment Counters
                        EV_NUMHRS = EV_NUMHRS + 1
                        EV_NUMMSG = EV_NUMMSG + 1
                     ELSE IF (ZI .LE. 0.0D0) THEN
C                       Write Out The Informational Message &
C                       Increment Counters
                        WRITE(DUMMY,'(I8.8)') KURDAT
                        CALL ERRHDL(PATH,MODNAM,'I','470',DUMMY)
                        EV_NUMHRS = EV_NUMHRS + 1
                     ELSE
C                       Set CALCS Flag, Increment Counters
C                       & Calculate HRVAL
                        CALCS = .TRUE.
                        EV_NUMHRS = EV_NUMHRS + 1
C                       Calculate CONC or DEPOS Values      ---   CALL EVCALC
                        CALL EVCALC
                     END IF

                     IF (PVMRM .AND. .NOT.O3MISS .AND.          ! jop 9/30/06
     &                               .NOT.CLMHR  .AND. 
     &                               .NOT.MSGHR  .AND.
     &                                           .NOT.PSDCREDIT) THEN
C ---                   Process Hourly Values for PVMRM Option
                        CALL PVMRM_CALC('ALLSRCS')
               
                     ELSE IF (PVMRM .AND. .NOT.O3MISS .AND.   ! jop 9/30/06
     &                                    .NOT.CLMHR  .AND. 
     &                                    .NOT.MSGHR  .AND. 
     &                                                PSDCREDIT) THEN
C ---                   Process Hourly Values for PVMRM Option and PSD credits
C ---                   Need to process two separate sets of sources - the
C                       increment consumption sources ('NAAQSRC') and the 
C                       increment expanding sources ('ALLBASE')
                        CALL PVMRM_CALC('NAAQSRC')
                        CALL PVMRM_CALC('ALLBASE')
                     ELSE IF (OLM .AND. .NOT.O3MISS 
     &                            .AND. .NOT.CLMHR  .AND. 
     &                                              .NOT.MSGHR) THEN
C ---                   Process Hourly Values for OLM Option
                        CALL OLM_CALC
                     END IF

                  END DO
C                 End Hourly LOOP

C                 Calculate Applicable Averages             ---   CALL AVEREV
                  CALL AVEREV

C                 Print Out Model Results                   ---   CALL OUTPUT
                  CALL EV_OUTPUT

C                 Flush HRVAL, AVEVAL, GRPAVE and GRPVAL    ---   CALL FLUSH
                  CALL EV_FLUSH

C                 Reset CALCS Flag
                  CALCS = .FALSE.

C                 Reset the Counters
                  EV_NUMHRS = 0
                  EV_NUMCLM = 0
                  EV_NUMMSG = 0

               END IF   ! IF-ENDIF block on events for this JDAY

            END DO
C           End Event LOOP

         END IF
      END DO
C     End Loop Through Meteorology Data

      RETURN
      END

      SUBROUTINE MEREAD
C***********************************************************************
C                MEREAD Module of the AMS/EPA Regulatory Model - AERMOD - EVENT
C
C        PURPOSE: Controls Extraction and Quality Assurance of
C                 One Day of Meteorological Data for EVENT Processing
C
C        PROGRAMMER: ROGER BRODE, JEFF WANG
C
C        DATE:    March 2, 1992
C
C        MODIFIED:  Modified met data arrays to include an additional
C                   array index, since these arrays are also used by 
C                   the OU MAXDCONT post-processing option.
C                   R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/28/2011
C
C        MODIFIED:  Modified code for processing multi-year data
C                   files to determine if header record is present
C                   between years for concatenated files.  Use presence
C                   of colon (':') as only criterion for header record.
C                   Use warning messages if UAIR and SURF IDs don't
C                   match input runstream file for multiple years since
C                   AERMOD allows mismatch for single year files.
C                   Modified check for stable or missing hours in 
C                   calculation of solar irradiance (QSW) for use
C                   in deposition calculations.
C                   Modified to check first hour of met data files
C                   to determine if file starts on hour 01. If not,
C                   cycle through hour loop until loop index matches
C                   hour in data file.
C                   R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
C
C        MODIFIED:  Modified code for reading the header record of the
C                   surface file to use a character variable for the
C                   AERMET version date field, in order to allow for
C                   the future use of screening meteorology that is not
C                   directly linked to a specific version under the
C                   of the AERMET processor under the SCREEN option.
C                   R.W. Brode, U.S. EPA/OAQPS/AQMG, 01/26/2007
C
C        MODIFIED:  To assign non-array logicals STABLE and UNSTAB
C                   for use in subroutine COMPTG.
C                   R.W. Brode, U.S. EPA/OAQPS/AQMG, 01/24/2007
C
C        MODIFIED:  To remove support for unformatted meteorological
C                   data files.
C                   R.W. Brode, PES, Inc., 4/10/2000
C
C        MODIFIED:  To incorporate modifications to date processing
C                   for Y2K compliance, including use of date window
C                   variables (ISTRT_WIND and ISTRT_CENT) and calculation
C                   of 10-digit date variable (FULLDATE) with 4-digit
C                   year for date comparisons.
C                   Also modified calls to METDAT insteaad of EV_METDAT
C                   to allow use of same routine for both normal and
C                   EVENT processing.
C                   R.W. Brode, PES, Inc., 5/12/99
C
C        INPUTS:  Meteorology File Specifications
C
C        OUTPUTS: Arrays of Meteorological Variables for One Day
C
C        CALLED FROM:   EVLOOP
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C---- Constants used in the computation of QSW
      DOUBLE PRECISION, PARAMETER :: C1=5.31D-13, C2=60.0D0, C3=1.12D0,
     &                               STEFB= 5.67D-08
      DOUBLE PRECISION :: RN, Es25
      
      INTEGER :: I, IHR, IJDAY, IDATCHK, JUSI, JSSI,
     &           JFLAG, LEVEL
     
      CHARACTER (LEN=8)   :: CUSI, CSSI, COSI
      CHARACTER (LEN=6)   :: SPEC1, SPEC2, SPEC3
      CHARACTER (LEN=132) :: BUFFER

C     Variable Initializations
      MODNAM = 'MEREAD'
      PATH   = 'MX'

C     READ Meteorology Data Based on Format --
C     When DRY deposition is modeled, U-star, L, and z0 (surface
C     roughness length) are read in addition to the standard RAMMET
C     data.  These must be provided at the end of each hourly record
C     for the FORMATTED ASCII, CARD, and FREE options.
C
C     When WET deposition is modeled, ipcode (precip.
C     code) and prate (precip. rate in mm/hr) must also be added to
C     each hourly record.
C     The format statement allows for all additional data:

C     Calculate the MMDDHH variable to check for end of the year
      IDATCHK = KURDAT - INT(KURDAT/1000000)*1000000
      IF ((IMONTH.EQ.12 .AND. IDAY.EQ.31 .AND. IHOUR.EQ.24) .OR.
     &    IDATCHK .EQ. 123124) THEN
C        End of year has been reached - check for presence of header
C        record at beginning of next year for multi-year data files.
         READ(MFUNIT,'(A132)',ERR=998,END=1000,IOSTAT=IOERRN) BUFFER
         READ(BUFFER,1900,ERR=998,IOSTAT=IOERRN) ALAT, ALON, SPEC1,
     &        CUSI, SPEC2, CSSI, SPEC3, COSI, C_METVER
 1900    FORMAT(2A10,T31,A6,T38,A8,T48,A6,T55,A8,T65,A6,T72,A8,T93,A6)

         IF (INDEX(BUFFER,':') .EQ. 0) THEN
C           Record does not contain colon. Assume it must be regular
C           met data record, so backspace met file before proceeding.
            BACKSPACE MFUNIT
         ELSE
C           Record contains colons. Assume it is a header record,
C           check station IDs before proceeding to flag potential
C           for use of different stations in multi-year data files.
C           Convert UAIR and SURF character IDs to integers
            CALL STONUM(CUSI,8,FNUM,IMIT)
            IF (IMIT .EQ. 1) THEN
               JUSI = NINT(FNUM)
            ELSE
               JUSI = 0
            END IF
            CALL STONUM(CSSI,8,FNUM,IMIT)
            IF (IMIT .EQ. 1) THEN
               JSSI = NINT(FNUM)
            ELSE
               JSSI = 0
            END IF
            IF (JSSI .NE. IDSURF) THEN
C              Write Warning Message:  SURFDATA id mismatch
               CALL ERRHDL(PATH,MODNAM,'W','530','SURFDATA')
            END IF
            IF (JUSI .NE. IDUAIR) THEN
C              Write Warning Message:  UAIRDATA id mismatch
               CALL ERRHDL(PATH,MODNAM,'W','530','UAIRDATA')
            END IF
         END IF

         GO TO 1001

C        Error reading 'header record' - assume that header record is
C        missing.  Backspace met file and continue processing.
 998     BACKSPACE MFUNIT

      END IF

1001  CONTINUE

      HOUR_LOOP: DO IHR = 1, NHR
C
C---- READ surface scaling meteorology data based on format
C
      IF( LDPART .OR. LWPART .OR. LDGAS .OR. LWGAS )THEN
C        Read record from ASCII scalar parameter file using FREE format
C        with deposition variables
C
C ---    First read date variables to check for problems
         READ( MFUNIT, *, END=1000, ERR=99, IOSTAT=IOERRN ) IYEAR,
     &         IMONTH, IDAY, IJDAY, IHOUR
C       
         IF (IHOUR .EQ. IHR) THEN
C ---       Data file hour matches loop hour; backspace and read full record
            BACKSPACE MFUNIT
            READ( MFUNIT, *, END=1000, ERR=99, IOSTAT=IOERRN ) IYEAR,
     &       IMONTH, IDAY, IJDAY, IHOUR, ASFCHF(IHR,1), AUSTAR(IHR,1),
     &       AWSTAR(IHR,1),AVPTGZI(IHR,1),AZICONV(IHR,1),AZIMECH(IHR,1),
     &       AOBULEN(IHR,1), ASFCZ0(IHR,1),ABOWEN(IHR,1),AALBEDO(IHR,1),
     &       AUREF(IHR,1), AWDREF(IHR,1), AUREFHT(IHR,1), ATA(IHR,1),
     &       ATREFHT(IHR,1), IAPCODE(IHR,1), APRATE(IHR,1), ARH(IHR,1),
     &       ASFCP(IHR,1), NACLOUD(IHR,1)
         ELSE IF (IHOUR .GT. IHR) THEN
C ---       Data file starts after hour 01; 
C           Issue warning, backspace file and skip to Profile file
            WRITE(DUMMY,'(2X,3I2)',ERR=99) IMONTH, IDAY, IHR
            CALL ERRHDL(PATH,MODNAM,'W','489',DUMMY)
            BACKSPACE MFUNIT
            GO TO 888
         ELSE
C ---       Data file hour is less than loop hour;
C           could be problem with data file or use of 00-23 hour convention
C           Issue error message:
            WRITE(DUMMY,'(2X,3I2)',ERR=99) IMONTH, IDAY, IHOUR
            CALL ERRHDL(PATH,MODNAM,'E','490',DUMMY)
            EXIT HOUR_LOOP
         END IF
C
C        Calculate solar irradiance, QSW, from Heat Flux, Bowen ratio,
C        albedo and cloud cover, for use in gas deposition algorithm.
         IF (AOBULEN(IHR,1).GT.0.0D0 .OR. AOBULEN(IHR,1).LT.-99990.0D0
     &      .OR. ATA(IHR,1).LT.0.0D0 .OR. 
     &       AALBEDO(IHR,1).EQ.1.0D0 .OR. ABOWEN(IHR,1).EQ.0.0D0) THEN
C           Hour is stable or missing or inappropriate surface chars.
            AQSW(IHR,1) = 0.0D0
         ELSE
            RN = (1.D0 + 1.D0/ABOWEN(IHR,1))*ASFCHF(IHR,1)/0.9D0
            AQSW(IHR,1) = (RN*(1.D0+C3)-C1*ATA(IHR,1)**6+
     &                     STEFB*ATA(IHR,1)**4 -
     &                    C2*0.1D0*DBLE(NACLOUD(IHR,1))) / 
     &                   (1.D0-AALBEDO(IHR,1))
         END IF
C
C        Save precipitation rates for two previous hours
         IF (IHR .EQ. 1) THEN
            Aprec2(IHR,1) = APrate(NHR-1,1)
            Aprec1(IHR,1) = APrate(NHR,1)
         ELSE IF (IHR .EQ. 2) THEN
            Aprec2(IHR,1) = APrate(NHR,1)
            Aprec1(IHR,1) = APrate(IHR-1,1)
         ELSE
            Aprec2(IHR,1) = APrate(IHR-2,1)
            Aprec1(IHR,1) = APrate(IHR-1,1)
         END IF

C        Set variables for dry deposition
         IF (LDPART .OR. LDGAS) THEN
            IF (ATA(IHR,1).LT.0.0D0 .OR. APRATE(IHR,1).LT.0.0D0) THEN
               AWNEW(IHR,1) = AWOLD(IHR,1)
            ELSE
c ...          Compute saturation vapor pressure based on CMAQ formula
               AEsTa(IHR,1) = 0.6112D0*DEXP(19.83D0 - 
     &                        5417.4D0/ATA(IHR,1))
               Es25 = 3.167D0
               AWnew(IHR,1) = Wold+APrec1(IHR,1)-
     &                       0.5D0*f2*AEsTa(IHR,1)/Es25
               Wold = AWnew(IHR,1)
               Af2(IHR,1) = AWnew(IHR,1)/200.D0
               if (Af2(IHR,1).le.0.01D0) Af2(IHR,1) = 0.01D0
               if (Af2(IHR,1).gt.1.0D0) Af2(IHR,1) = 1.0D0
               f2 = Af2(IHR,1)
            END IF
         END IF

      ELSE
C        Read record from ASCII scalar parameter file without deposition
C        parameters, using FREE format
C
C ---    First read date variables to check for problems
         READ( MFUNIT, *, END=1000, ERR=99, IOSTAT=IOERRN ) IYEAR,
     &         IMONTH, IDAY, IJDAY, IHOUR
C
         IF (IHOUR .EQ. IHR) THEN
C ---       Data file hour matches loop hour; backspace and read full record
            BACKSPACE MFUNIT
            READ( MFUNIT, *, END=1000, ERR=99, IOSTAT=IOERRN ) IYEAR,
     &         IMONTH, IDAY, IJDAY, IHOUR, ASFCHF(IHR,1), AUSTAR(IHR,1),
     &         AWSTAR(IHR,1), AVPTGZI(IHR,1), AZICONV(IHR,1), 
     &         AZIMECH(IHR,1), AOBULEN(IHR,1), ASFCZ0(IHR,1), 
     &         ABOWEN(IHR,1), AALBEDO(IHR,1), AUREF(IHR,1), 
     &         AWDREF(IHR,1), AUREFHT(IHR,1), ATA(IHR,1),
     &         ATREFHT(IHR,1)
         ELSE IF (IHOUR .GT. IHR) THEN
C ---       Data file starts after hour 01; 
C           Issue warning, backspace file and skip to Profile file
            WRITE(DUMMY,'(2X,3I2)',ERR=99) IMONTH, IDAY, IHR
            CALL ERRHDL(PATH,MODNAM,'W','489',DUMMY)
            BACKSPACE MFUNIT
            GO TO 888
         ELSE
C ---       Data file hour is less than loop hour;
C           could be problem with data file or use of 00-23 hour convention
C           Issue error message:
            WRITE(DUMMY,'(2X,3I2)',ERR=99) IMONTH, IDAY, IHOUR
            CALL ERRHDL(PATH,MODNAM,'E','490',DUMMY)
            EXIT HOUR_LOOP
         END IF
C
      END IF

C     Set the stability logical variables
      IF( AOBULEN(IHR,1) .GT. 0.0D0 ) THEN
         AUNSTAB(IHR,1) = .FALSE.
         ASTABLE(IHR,1) = .TRUE.
C        Also set non-array variables for use in COMPTG
         UNSTAB = .FALSE.
         STABLE = .TRUE.
      ELSE
         AUNSTAB(IHR,1) = .TRUE.
         ASTABLE(IHR,1) = .FALSE.
C        Also set non-array variables for use in COMPTG
         UNSTAB = .TRUE.
         STABLE = .FALSE.
      END IF

C---- Initialize the profile data to missing;
C     READ profile data based on format
C

C --- Branch here if surface data file starts after hour 01
C
 888  CONTINUE
 
      CALL PFLINI ()
      LEVEL = 1
      JFLAG = 0
C     Read record from ASCII profile file using FREE format; compute
C     sigma_V from sigma_A and wind speed
C --- First read date variables to check for problems
      READ( MPUNIT, *, END=1000, ERR=98, IOSTAT=IOERRN ) KYEAR,
     &         KMONTH, KDAY, KHOUR
C       
      IF (KHOUR .EQ. IHR) THEN
C ---    Data file hour matches loop hour; backspace and read full record
         BACKSPACE MPUNIT

      DO WHILE( JFLAG .EQ. 0 )
         READ( MPUNIT, *, END=1000, ERR=98, IOSTAT=IOERRN ) KYEAR,
     &       KMONTH, KDAY, KHOUR, PFLHT(LEVEL), JFLAG,
     &       PFLWD(LEVEL), PFLWS(LEVEL), PFLTA(LEVEL),
     &       PFLSA(LEVEL), PFLSW(LEVEL)

C        Convert the data to the required units
         CALL PFLCNV (LEVEL)

C        Set the number of profile levels to current index, store
C        the 'top of profile' flag, and increment level if not at top
C        Check that the level does not exceed the maximum allowable
         NPLVLS = LEVEL
         ANPLVLS(IHR,1) = LEVEL
         AIFLAG(IHR,LEVEL,1) = JFLAG
         APFLHT(IHR,LEVEL,1) = PFLHT(LEVEL)
         APFLWD(IHR,LEVEL,1) = PFLWD(LEVEL)
         APFLWS(IHR,LEVEL,1) = PFLWS(LEVEL)
         APFLTA(IHR,LEVEL,1) = PFLTA(LEVEL)
         APFLSA(IHR,LEVEL,1) = PFLSA(LEVEL)
         APFLSV(IHR,LEVEL,1) = PFLSV(LEVEL)
         APFLSW(IHR,LEVEL,1) = PFLSW(LEVEL)
         IF( JFLAG .EQ. 0 )THEN
            LEVEL = LEVEL + 1

            IF( LEVEL .GT. MXPLVL )THEN
               IF( .NOT. PFLERR )THEN
C                 WRITE Error Message: Number of profile levels
C                                      exceeds maximum allowable
                  WRITE(DUMMY,'(I8)') MXPLVL
                  CALL ERRHDL(PATH,MODNAM,'E','465',DUMMY)
                  PFLERR = .TRUE.
                  RUNERR = .TRUE.
               END IF

C              Limit the number of levels to the maximum allowable
               LEVEL = MXPLVL
            END IF

         END IF

      END DO

      ELSE IF (KHOUR .GT. IHR) THEN
C ---    Data file starts after hour 01; 
C        Backspace file and cycle hour loop
         BACKSPACE MPUNIT
         CYCLE HOUR_LOOP

      ELSE
C ---    Data file hour is less than loop hour;
C        could be problem with data file or use of 00-23 hour convention
C        Issue error message:
         WRITE(DUMMY,'(2X,3I2)',ERR=99) IMONTH, IDAY, IHOUR
         CALL ERRHDL(PATH,MODNAM,'E','489',DUMMY)
         EXIT HOUR_LOOP
      END IF

C     Compute the vertical potential temperature gradient profile
      IF( .NOT. RUNERR ) THEN
         NTGLVL = 0
         CALL COMPTG ()
         ANTGLVL(IHR,1) = NTGLVL
         DO I = 1, NTGLVL
           APFLTG(IHR,I,1)  = PFLTG(I)
           APFLTGZ(IHR,I,1) = PFLTGZ(I)
         END DO
      END IF

      END DO HOUR_LOOP

C     Set the date variables
      CALL SET_DATES

      GO TO 999

C     WRITE Error Messages:  Error Reading Met Data File

 98   CALL ERRHDL(PATH,MODNAM,'E','510','PROFFILE')
      RUNERR = .TRUE.
      GO TO 999

 99   CALL ERRHDL(PATH,MODNAM,'E','510','SURFFILE')
      RUNERR = .TRUE.
      GO TO 999

 1000 EOF = .TRUE.
 
C     Set the date variables
      CALL SET_DATES

 999  RETURN
      END

      SUBROUTINE EV_METEXT
C***********************************************************************
C                EV_METEXT Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Controls Extraction and Quality Assurance of
C                 One Hour of Meteorological Data
C
C        PROGRAMMER: ROGER BRODE, JEFF WANG
C        MODIFIED BY D. Strimaitis, SRC (for Wet & Dry DEPOSITION)
C
C        DATE:    November 8, 1993
C
C        MODIFIED:   Modified met data arrays to include an additional
C                    array index, since these arrays are also used by 
C                    the OU MAXDCONT post-processing option.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/28/2011
C
C        MODIFIED:   To remove unused data array (NDAY).
C                    R.W. Brode, PES, Inc., 4/10/2000
C
C        MODIFIED:   To incorporate modifications to date processing
C                    for Y2K compliance, including use of date window
C                    variables (ISTRT_WIND and ISTRT_CENT) and calculation
C                    of 10-digit date variable (FULLDATE) with 4-digit
C                    year for date comparisons.
C                    R.W. Brode, PES, Inc., 5/12/99
C
C        MODIFIED:   To add determination of season index (ISEAS).
C                    R.W. Brode, PES, Inc. - 12/2/98
C
C        MODIFIED BY D. Strimaitis, SRC (for Dry DEPOSITION)
C        (DATE:    February 15, 1993)
C
C        MODIFIED:   To avoid potential math error due to negative
C                    ambient temperatures in calculating the square
C                    root of the stability parameter, RTOFS - 4/19/93
C
C        MODIFIED:
C        7/27/94     J. Paumier, PES, Inc.
C                    The variables for displacement height, ZDM and
C                    AZDM(), were removed from the input to and output
C                    from ISC-COMPDEP.  The following format statements
C                    also were affected: 9009, 9026, 9032, 9033
C
C*       7/27/94     J. Hardikar, PES, Inc.
C*                   Added code to calculate reference wind speed at 10m
C*                   to be used for OPENPIT source algorithms
C
C        INPUTS:  Meteorology File Specifications
C
C        OUTPUTS: Meteorological Variables for One Hour
C
C        CALLED FROM:   EVLOOP
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      INTEGER I

C     Variable Initializations
      MODNAM = 'EV_METEXT'
      PATH   = 'MX'

C     Save Value of Last YR/MN/DY/HR and Previous Hour
      IPDATE = KURDAT
      IPHOUR = IHOUR

C     Set Meteorological Variables for This Hour
      SFCHF  = ASFCHF(IHOUR,1)
      UREF   = AUREF(IHOUR,1)
      UREFHT = AUREFHT(IHOUR,1)
      TA     = ATA(IHOUR,1)
      TREFHT = ATREFHT(IHOUR,1)
      WDREF  = AWDREF(IHOUR,1)
      USTAR  = AUSTAR(IHOUR,1)
      WSTAR  = AWSTAR(IHOUR,1)
      ZICONV = AZICONV(IHOUR,1)
      ZIMECH = AZIMECH(IHOUR,1)
      OBULEN = AOBULEN(IHOUR,1)
      VPTGZI = AVPTGZI(IHOUR,1)
      SFCZ0  = ASFCZ0(IHOUR,1)
      BOWEN  = ABOWEN(IHOUR,1)
      ALBEDO = AALBEDO(IHOUR,1)
      IPCODE = IAPCODE(IHOUR,1)
      PRATE  = APRATE(IHOUR,1)
      RH     = ARH(IHOUR,1)
      SFCP   = ASFCP(IHOUR,1)
      NCLOUD = NACLOUD(IHOUR,1)
      QSW    = AQSW(IHOUR,1)
      Wnew   = AWnew(IHOUR,1)
      f2     = Af2(IHOUR,1)
      EsTa   = AEsTa(IHOUR,1)
      Prec1  = APrec1(IHOUR,1)
      Prec2  = APrec2(IHOUR,1)

      NPLVLS = ANPLVLS(IHOUR,1)

      IFLAG(1:NPLVLS) = AIFLAG(IHOUR,1:NPLVLS,1)
      PFLHT(1:NPLVLS) = APFLHT(IHOUR,1:NPLVLS,1)
      PFLWD(1:NPLVLS) = APFLWD(IHOUR,1:NPLVLS,1)
      PFLWS(1:NPLVLS) = APFLWS(IHOUR,1:NPLVLS,1)
      PFLTA(1:NPLVLS) = APFLTA(IHOUR,1:NPLVLS,1)
      PFLSA(1:NPLVLS) = APFLSA(IHOUR,1:NPLVLS,1)
      PFLSV(1:NPLVLS) = APFLSV(IHOUR,1:NPLVLS,1)
      PFLSW(1:NPLVLS) = APFLSW(IHOUR,1:NPLVLS,1)

      NTGLVL = ANTGLVL(IHOUR,1)

      PFLTG(1:NTGLVL)  = APFLTG(IHOUR,1:NTGLVL,1)
      PFLTGZ(1:NTGLVL) = APFLTGZ(IHOUR,1:NTGLVL,1)

C     Set Meteorological Variables for Current Hour
      CALL SET_METDATA

 999  RETURN
      END

      SUBROUTINE EV_HRQREAD(L_FIRSTCALL)
C***********************************************************************
C*                  EV_HQREAD Module of AERMOD
C* 
C*         PURPOSE: To Read a 24-hour Block of Hourly Emissions Data
C* 
C*         PROGRAMMER:  Jayant Hardikar, Roger Brode
C* 
C*         DATE:    September 15, 1993
C* 
C*         INPUTS:  Variable QFLAG and Current Source Number Being Processed
C* 
C*         OUTPUTS: Source Arrays
C*          
C*         MODIFIED:  REMOVED THE 'POINT' SOURCE CONDITION, SO IT APPLIES 
C*                    TO ALL SOURCE TYPES, EXCEPT SAVING THE TEMP & VEL
C* 
C*         CALLED FROM:  EVLOOP
C************************************************************************
C*
C*    Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: IS, IHR
      INTEGER :: ILSAVE
      LOGICAL :: L_FIRSTCALL
      LOGICAL :: EOF_SAVE

C*    Variable Initializations
      MODNAM = 'EV_HRQREAD'
C*    Save current value of EOF from MEREAD
      EOF_SAVE = EOF
C*    Reinitialize EOF = .F. for HRQREAD
      EOF = .FALSE.
      
      ILSAVE = ILINE

      HOUR_LOOP: DO IHR = 1, NHR
         IQLINE = IQLINE + 1
         SOURCE_LOOP: DO IS = 1, NUMSRC
            IF (QFLAG(IS) .EQ. 'HOURLY') THEN

               IF (L_FIRSTCALL) ILINE = 1
C ---          Assign ILINE = 1 for first call to HRQREAD
               CALL HRQREAD (IS)

               IF (.NOT.EOF .AND. IHR .EQ. NHR) THEN
C*                Check for Date and Time Consistency with Met Data; 
C*                If Failed, Issue Fatal Error
                  IF (FULLDATE .NE. FULLHRQ) THEN
C*                   WRITE Error Message - Date mismatch
                     WRITE(DUMMY,'(I10.10)') FULLDATE
                     CALL ERRHDL(PATH,MODNAM,'E','455',DUMMY)
                     RUNERR = .TRUE.
                     EXIT HOUR_LOOP
                  END IF
               ELSE IF (EOF) THEN
C ---             EOF reached in HRQREAD; reassign EOF based on MEREAD
C                 Exit hour loop to avoid read error in HRQREAD
                  EOF = EOF_SAVE
                  EXIT HOUR_LOOP
               END IF

               EV_HRQS(IS,IHR) = HRQS

               IF (SRCTYP(IS)(1:5) .EQ. 'POINT') THEN
                  EV_HRTS(IS,IHR) = HRTS
                  EV_HRVS(IS,IHR) = HRVS
               ELSE IF (SRCTYP(IS) .EQ. 'VOLUME' .AND. 
     &                                           L_HRLYSIG(IS)) THEN
                  EV_HRHS(IS,IHR) = HRHS
                  EV_HRSY(IS,IHR) = HRSY
                  EV_HRSZ(IS,IHR) = HRSZ
               ELSE IF (SRCTYP(IS)(1:4) .EQ. 'AREA' .AND. 
     &                                           L_HRLYSIG(IS)) THEN
                  EV_HRHS(IS,IHR) = HRHS
                  EV_HRSZ(IS,IHR) = HRSZ
               END IF

            END IF
         END DO SOURCE_LOOP
      END DO HOUR_LOOP

      RETURN
      END

      SUBROUTINE EVCALC
C***********************************************************************
C                 EVCALC Module of ISC2 Short Term EVENT Model - ISCEV2
C
C        PURPOSE: Controls Flow and Processing of CALCulation Modules
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:  Modified to include user-specified background
C                   concentrations through the SO BACKGRND option.
C                   R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/28/2011
C
C        MODIFIED:  To set NUMREC = 1 and use PCALC, VCALC, ACALC, and
C                   OCALC subroutines.  R.W. Brode, PES, Inc. - 12/2/98
C
C        INPUTS:  Arrays of Source Parameters
C                 Arrays of Receptor Locations
C                 Meteorological Variables for One Hour
C
C        OUTPUTS: Array of 1-hr CONC or DEPOS Values for Each Source/Receptor
C
C        CALLED FROM:   EVLOOP
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE

      DOUBLE PRECISION :: BCKGRD

      CHARACTER MODNAM*12
      
C     Variable Initializations
      MODNAM = 'EVCALC'
      PATH   = 'CN'

C     Set NUMREC = 1 to allow use of PCALC, VCALC, ACALC, and OCALC subroutines
      NUMREC = 1

C     Begin Source LOOP
      SOURCE_LOOP: DO ISRC = 1, NUMSRC
         IF (IGROUP(ISRC,IDXEV(IEVENT)) .EQ. 1) THEN
            IF (SRCTYP(ISRC)(1:5) .EQ. 'POINT') THEN
C              Calculate Point Source Values                   ---   CALL PCALC
               CALL PCALC
            ELSE IF (SRCTYP(ISRC) .EQ. 'VOLUME') THEN
C              Calculate Volume Source Values                  ---   CALL VCALC
               CALL VCALC
            ELSE IF (SRCTYP(ISRC)(1:4) .EQ. 'AREA') THEN
C              Calculate AREA/AREAPOLY/AREACIRC Source Values  ---   CALL ACALC
               CALL ACALC
            ELSE IF (SRCTYP(ISRC) .EQ. 'OPENPIT') THEN
C              Calculate OpenPit Source Values                 ---   CALL OCALC
               CALL OCALC
            END IF
         END IF
      END DO SOURCE_LOOP
C     End Source LOOP

      IF (L_BACKGRND) THEN
C ---    User-specified background concentrations are included; 
C        add to modeled concentrations by source group
         BCKGRD = 0.0D0
         IF (GRP_BACK(IDXEV(IEVENT))) THEN
C ---       Include background for this source group
            BCKGRD = EV_BGCONC(IHOUR)
         ELSE
C ---       Do not include background for this source group
            BCKGRD = 0.0D0
         END IF
         GRPVAL(IHOUR) = GRPVAL(IHOUR) + BCKGRD
         GRPAVE  = GRPAVE + BCKGRD
         BACKAVE = BACKAVE + BCKGRD
         BACKHR(IHOUR) = BCKGRD
      END IF
      
      RETURN
      END

      SUBROUTINE EV_SUMVAL
C***********************************************************************
C                 EV_SUMVAL Module of the AMS/EPA Regulatory Model - AERMOD - EVENT
C
C        PURPOSE: Sums HRVAL to AVEVAL and ANNVAL Arrays
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        MODIFIED:  Modified to include GRPAVE variable to account for
C                   user-specified background for the full averaging
C                   period based user-specified background concentrations 
C                   through the SO BACKGRND option.
C                   R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/28/2011
C
C        INPUTS:  HRVAL - Hourly Value for (IHOUR,ISRC) Combination
C                 Averaging Period Options
C                 Source Groupings
C
C        OUTPUTS: Updated Sums of AVEVAL and ANNVAL Arrays
C
C        CALLED FROM:   PCALC
C                       VCALC
C                       ACALC
C                       OCALC
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'EV_SUMVAL'

      HRVALS(IHOUR,ISRC) = HRVAL(1)
      EV_AVEVAL(ISRC)    = EV_AVEVAL(ISRC) + HRVAL(1)
      GRPVAL(IHOUR)      = GRPVAL(IHOUR) + HRVAL(1)
      GRPAVE             = GRPAVE + HRVAL(1)

      RETURN
      END

      SUBROUTINE O3READ(L_FIRSTCALL)
C***********************************************************************
C*                  O3READ Module of AERMOD
C* 
C*         PURPOSE: To Read a 24-hour Block of Ozone Data
C* 
C*         PROGRAMMER:  Roger Brode
C* 
C*         DATE:    October 17, 2005
C* 
C          MODIFIED:  Modified to assign EV_O3CONC(IHR) value based on 
C                     the O3BACK variable from CO OZONEVAL keyword when 
C                     no background hourly ozone file or CO O3VALUES inputs
C                     are available.
C                     R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/29/2012
C
C          MODIFIED:  Modified to use background ozone values input
C                     through the CO O3VALUES option to substitute for 
C                     missing hourly ozonce concentrations.
C                     R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/28/2011
C
C*         INPUTS:
C* 
C*         OUTPUTS:
C*          
C*         CALLED FROM:
C************************************************************************
C*
C*    Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      DOUBLE PRECISION :: O3VALUES(24), O3MIN, O3MAX24
      INTEGER :: IHR, IO3YR, IO3MN, IO3DY, IO3HR, IO3YR2, FULLO3HR
      LOGICAL :: L_FIRSTCALL
C     Initialize O3VALUES array to 40 ppb (78.4 ug/m^3) for first day

C*    Variable Initializations
      MODNAM  = 'O3READ'

      IF (L_FIRSTCALL) THEN
         O3VALUES = 78.4D0
      END IF

      DO IHR = 1, 24

C ---    Assign local IHR index to global IHOUR index
         IHOUR = IHR
         
         IF (O3FILE) THEN
C ---       Hourly O3 file availabe; read the next hour of O3 data
            IF (O3FORM .EQ. 'FREE') THEN
               READ(IO3UNT,*,ERR=99,END=999) IO3YR, IO3MN, IO3DY, IO3HR,
     &                                       EV_O3CONC(IHR)
            ELSE
               READ(IO3UNT,O3FORM,ERR=99,END=999) IO3YR, IO3MN, IO3DY,
     &                                            IO3HR, EV_O3CONC(IHR)
            END IF
        
            IF (IO3YR .LE. 99) THEN
               IO3YR2 = IO3YR
               IF (IO3YR2 .GE. ISTRT_WIND .AND. 
     &                              IO3YR2 .LE. 99) THEN
                  IO3YR  = ISTRT_CENT*100 + IO3YR2
               ELSE IF (IO3YR2 .LT. ISTRT_WIND) THEN
                  IO3YR  = (ISTRT_CENT+1)*100 + IO3YR2
               END IF
            END IF
        
C ---       Calculate full date for this hour of O3 data
            FULLO3HR = IO3YR*1000000 + IO3MN*10000 + IO3DY*100 + IO3HR
            
C ---       Check for temporally-varying ozone concentrations from O3VALUES
C           keyword; used to fill in for missing hourly data.
            IF (L_O3VALUES) THEN
               CALL OZONVALS(O3BACK)
            END IF
               
            IF (EV_O3CONC(IHR) .GE. 900.0D0 .OR. 
     &          EV_O3CONC(IHR) .LT. 0.0D0) THEN
C              Hourly ozone value is missing, check for background value
C              from OZONEVAL card
               IF (O3BACK .GE. 0.0D0) THEN
C                 Write informational message about substitution
                  EV_O3CONC(IHR) = O3BACK
                  O3MISS = .FALSE.
                  IF (.NOT. L_SkipMessages) THEN
                     WRITE(DUMMY,'(I10.10)') FULLO3HR
                     CALL ERRHDL(PATH,MODNAM,'I','458',DUMMY)
                  END IF
C                 Cycle loop since O3BACK units have already been converted to ug/m3
                  CYCLE
               ELSE
C                 Write informational message about missing data and use of full conversion
                  O3MISS = .TRUE.
                  IF (.NOT. L_SkipMessages) THEN
                     WRITE(DUMMY,'(I10.10)') FULLDATE
                     CALL ERRHDL(PATH,MODNAM,'I','459',DUMMY)
                  END IF
                  CYCLE
               END IF
            ELSE
C              Hourly ozone data not missing
               O3MISS = .FALSE.
            END IF
        
            IF (O3FILUNITS .EQ. 'PPB') THEN
               EV_O3CONC(IHR) = EV_O3CONC(IHR) * O3_PPB
            ELSE IF (O3FILUNITS .EQ. 'PPM') then
               EV_O3CONC(IHR) = EV_O3CONC(IHR) * O3_PPM
            END IF
        
            IF (.NOT. O3MISS) THEN
               O3VALUES(IHR) = EV_O3CONC(IHR)
            ELSE
               O3VALUES(IHR) = 0.0D0
            END IF
        
C-----      Apply minimum O3 value for stable hours; O3CONC is in ug/m^3
            IF (ASTABLE(IHR,1)) THEN
C              Use min of 40 ppb (78.4ug/m3) and max from previous 24 hrs
               O3MAX24 = MIN ( 78.40D0, MAXVAL( O3VALUES ) )
C              Adjust minimum O3 value based on OBULEN
               IF (AOBULEN(IHR,1).GT.0.0D0 .AND. 
     &             AOBULEN(IHR,1).LE.50.0D0) THEN
                  O3MIN = O3MAX24
               ELSE IF (AOBULEN(IHR,1) .GT. 250.0D0) THEN
                  O3MIN = 0.0D0
               ELSE
                  O3MIN = O3MAX24 * (250.D0 - AOBULEN(IHR,1)) / 200.D0
               END IF
               EV_O3CONC(IHR) = MAX( EV_O3CONC(IHR), O3MIN )
            END IF
        
         ELSE
C ---       No Hourly O3 file available

C ---       Check for temporally-varying ozone concentrations from O3VALUES
C           keyword; used to fill in for missing hourly data.
            IF (L_O3VALUES) THEN
C ---          Temporally varying O3 values specified on CO O3VALUES keyword
               CALL OZONVALS(EV_O3CONC(IHR))
            ELSE
C ---          No O3 file and no O3VALUES; use O3BACK from CO OZONEVAL keyword
               EV_O3CONC(IHR) = O3BACK
            END IF
            
         END IF
        
      END DO    ! Hour Loop

      IF (O3FILE) THEN
C*       Recalculate full date with last value of IO3HR (should be = 24) for 
C*       comparison to FULLDATE, since FULLDATE is set by MEREAD based on a 
C*       loop through one day of meteorological data.
C*       Check for Date and Time Consistency ; If Failed, Issue Fatal Error
         FULLO3HR = IO3YR*1000000 + IO3MN*10000 + IO3DY*100 + IO3HR
         IF (FULLDATE .NE. FULLO3HR) THEN
C*          WRITE Error Message - Date mismatch
            WRITE(DUMMY,'(I10.10)') FULLO3HR
            CALL ERRHDL(PATH,MODNAM,'E','457',DUMMY)
            RUNERR = .TRUE.
         END IF
      END IF

      GO TO 1000

C*    Write Error Message for Error Reading Hourly Ozone File
 99   CALL ERRHDL(PATH,MODNAM,'E','510','OZONEFIL')
      RUNERR = .TRUE.

 999  CONTINUE
 
C --- End of file reached on O3 file

1000  RETURN
      END

      SUBROUTINE BGREAD
C***********************************************************************
C*                  BGREAD Module of AERMOD
C* 
C*         PURPOSE: To Read a 24-hour Block of Background Data
C* 
C*         PROGRAMMER:  Roger Brode
C* 
C*         DATE:    February 28, 2011
C* 
C*         INPUTS:
C* 
C*         OUTPUTS:
C*          
C*         CALLED FROM:
C************************************************************************
C*
C*    Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: NDAY(12), ISEA_NDX(12)
      INTEGER :: I, IA, IY, IM, ID, NL, NUMSW
      DOUBLE PRECISION :: BGVALUES(24), BGFILL
      INTEGER :: IHR, IBGYR, IBGMN, IBGDY, IBGHR, IBGYR2, FULLBGHR

C     Variable Initializations
      DATA NDAY/31,59,90,120,151,181,212,243,273,304,334,365/
      DATA ISEA_NDX/1,1,2,2,2,3,3,3,4,4,4,1/
      
      MODNAM  = 'BGREAD'
      FULLBGHR = 0

      DO IHR = 1, 24

C ---    Assign local IHR index to global IHOUR index
         IHOUR = IHR
         
         IF (L_HourlyBackgrnd) THEN
C ---       Hourly BACKGRND data file available
         
C ---       Check for temporally-varying background to substitute for missing hours
            IF (IBKGRD .GT. 0) THEN
               CALL BGVAL(BGFILL)
            END IF
            
            IF (.NOT. EOF) THEN
C*             Retrieve hourly background concentrations      ---   CALL BGEXT
               IF (BGFORM .EQ. 'FREE') THEN
                  READ(IBGUNT,*,ERR=99,END=999) IBGYR, IBGMN, IBGDY, 
     &                                          IBGHR, EV_BGCONC(IHR)
               ELSE
                  READ(IBGUNT,BGFORM,ERR=99,END=999) IBGYR, IBGMN,IBGDY,
     &                                              IBGHR,EV_BGCONC(IHR)
               END IF
            END IF

C ---       Check for use of 2-digit year in background file, adjust to 4-digit
C           year for comparison with FULLDATE based on met data file
            IF (IBGYR .LE. 99) THEN
               IBGYR2 = IBGYR
               IF (IBGYR2 .GE. ISTRT_WIND .AND. 
     &                                    IBGYR2 .LE. 99) THEN
                  IBGYR  = ISTRT_CENT*100 + IBGYR2
               ELSE IF (IBGYR2 .LT. ISTRT_WIND) THEN
                  IBGYR  = (ISTRT_CENT+1)*100 + IBGYR2
               END IF
            END IF

C ---       Calculate full date for this hour of BACKGRND data
            FULLBGHR = IBGYR*1000000 + IBGMN*10000 + IBGDY*100 + IBGHR
      
            IF (EV_BGCONC(IHR).GE.900.0D0 .OR. 
     &          EV_BGCONC(IHR).LT.0.0D0) THEN
               IF (IBKGRD .EQ. 0) THEN
C                 Hourly background value is missing and no values
C                 specified for substitution;
C                 Write Error message 
                  WRITE(DUMMY,'(I10.10)') FULLBGHR
                  CALL ERRHDL(PATH,MODNAM,'E','452',DUMMY)
                  RUNERR = .TRUE.
                  GO TO 1000
               ELSE
C                 Hourly background value is missing but values
C                 have been specified for substitution, which 
C                 are processed in subroutine BGVAL;
C                 Write warning message 
                  WRITE(DUMMY,'(I10.10)') FULLBGHR
                  CALL ERRHDL(PATH,MODNAM,'W','453',DUMMY)
C                 Set logical flag for missing hourly background
C                 to trigger substitution in BGVAL
                  L_MissBackgrnd = .TRUE.
               END IF
            ELSE
               L_MissBackgrnd = .FALSE.      
            END IF
                  
C ---       Apply substitution for missing hourly background concentration
            IF (L_MissBackgrnd .AND. IBKGRD .GT. 0) THEN
               EV_BGCONC(IHR) = BGFILL
            END IF
            
         ELSE
C ---       No Hourly BACKGRND file available
         
C ---       Check for temporally-varying background
            IF (IBKGRD .GT. 0) THEN
               CALL BGVAL(EV_BGCONC(IHR))
            END IF
            
         END IF
            
C ---    Adjust background concentration units to UG/M3 if needed;
C        conversion is based on reference temperature (25C) and
C        pressure (1013.25 mb)
         IF (POLLUT .EQ. 'NO2') THEN
            IF (BackUnits .EQ. 'PPB') THEN
               EV_BGCONC(IHR) = EV_BGCONC(IHR) / NO2_PPB
            ELSE IF (BackUnits .EQ. 'PPM') THEN
               EV_BGCONC(IHR) = EV_BGCONC(IHR) / NO2_PPM
            END IF
         ELSE IF (POLLUT .EQ. 'SO2') THEN
            IF (BackUnits .EQ. 'PPB') THEN
               EV_BGCONC(IHR) = EV_BGCONC(IHR) / SO2_PPB
            ELSE IF (BackUnits .EQ. 'PPM') THEN
               EV_BGCONC(IHR) = EV_BGCONC(IHR) / SO2_PPM
            END IF
         ELSE IF (POLLUT .EQ. 'CO') THEN
            IF (BackUnits .EQ. 'PPB') THEN
               EV_BGCONC(IHR) = EV_BGCONC(IHR) * CO_PPB
            ELSE IF (BackUnits .EQ. 'PPM') THEN
               EV_BGCONC(IHR) = EV_BGCONC(IHR) * CO_PPM
            END IF
         END IF

      END DO    ! Hour Loop

      IF (L_HourlyBackgrnd) THEN
C*       Recalculate full date with last value of IBGHR (should be = 24) for 
C*       comparison to FULLDATE, since FULLDATE is set by MEREAD based on a 
C*       loop through one day of meteorological data.
C*       Check for Date and Time Consistency ; If Failed, Issue Fatal Error
         FULLBGHR = IBGYR*1000000 + IBGMN*10000 + IBGDY*100 + IBGHR
         IF (FULLDATE .NE. FULLBGHR) THEN
C*          WRITE Error Message - Date mismatch
            WRITE(DUMMY,'(I10.10)') FULLBGHR
            CALL ERRHDL(PATH,MODNAM,'E','454',DUMMY)
            RUNERR = .TRUE.
         END IF
      END IF

      GO TO 1000

C*    Write Error Message for Error Reading Hourly BACKGRND File
 99   CALL ERRHDL(PATH,MODNAM,'E','510','BACKGRND')
      RUNERR = .TRUE.
      GO TO 1000

 999  CONTINUE
 
1000  RETURN
      END
